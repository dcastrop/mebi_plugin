exception NothingToDo
exception NotImplemented

module type S = sig
  type enc
  type node
  type tree
  type trees

  module Tactic : Proof_solver_tactic.S

  module W :
    Results.S
    with type enc = enc
     and type node = node
     and type tree = tree
     and type trees = trees

  module ProofState :
    Proof_solver_statem.S
    with type enc = enc
     and type node = node
     and type state = W.Model.State.t
     and type label = W.Model.Label.t
     and type annotation = W.Model.Annotation.t
     and type transition = W.Model.Transition.t

  module Step : (_ : Proof_solver_wrapper.Args) ->
    Proof_solver_step.S with type tactic = Tactic.t

  val get_updated_pstate : unit Proofview.tactic -> Declare.Proof.t
  val step : Declare.Proof.t -> Declare.Proof.t
end

module Make (Enc : Encoding.S) :
  S
  with type enc = Enc.t
   and type node = Enc.Tree.Node.t
   and type tree = Enc.Tree.t
   and type trees = Enc.Trees.t = struct
  type enc = Enc.t
  type node = Enc.Tree.Node.t
  type tree = Enc.Tree.t
  type trees = Enc.Trees.t

  (** [module Tactic] is our own wrapper-module for the ['a Proofview.tactic]. The key distinction is that we enable tactics to be attached with an optional message to print {i (each with configurable [module Feedback.level])}, and have a clearer approach to sequencing and chaining tactics together. Tactics built using this must be {b unpacked} in order to by used via function [unpack].
  *)
  module Tactic : Proof_solver_tactic.S = Proof_solver_tactic.Make

  (** [module W] is for running the main part of the algorithm (pre-proof). It is a standard [module Wrapper.S] which itself is wrapped in a [module Results.S] which stores the results and provides some useful functions for using the bisimilarity result.
  *)
  module W = Results.Make (Enc)

  (** [module ProofState] sets up the different internal states of the proof-solver. We require [module Results.S] since some of the internal states {i (e.g., [Exists transition_opt])} store some information from the proof that corresponds to information captured in the initial run of the bisimilarity checking algorithm. {i {b Note:} this 'proof-state-machine' is not actually handled here, it is only the structure.}
    @see [module Proof_solver_step] for how these states are traversed in order to solve the proof.
    *)
  module ProofState = Proof_solver_statem.Make (Enc) (W)

  (** [module TheoryMaker] is a functor that allows us to create a [module Proof_solver_theory.S] for each iteration (step) of the proof-solver. Since a fair amount of it only relies on [module Enc] and the results of the command in [module W], this functor just takes the [module Proof_solver_wrapper.S] created from the [Proofview.Goal.t] of the proof in each proof-step.
  *)
  module TheoryMaker = Proof_solver_theory.Make (Enc) (W)

  (** [module Step] is a functor for returning a [module Proof_solver_step.S] for handling the current [Proofview.Goal.t], which is the only thing that will change for each proof-step.
  *)
  module Step =
    Proof_solver_step.Make (Enc) (Tactic) (W) (ProofState) (TheoryMaker)

  let make (gl : Proofview.Goal.t)
    : (module Proof_solver_step.S with type tactic = Tactic.t)
    =
    (module Step (struct
         let gl = ref gl
       end))
  ;;

  (** [get_updated_pstate x] returns the [pstate] updated by tactic [x]. *)
  let get_updated_pstate (x : unit Proofview.tactic) : Declare.Proof.t =
    Logger.trace __FUNCTION__;
    let new_pstate, is_safe_tactic =
      Declare.Proof.by (Global.env ()) x (ProofState.get_pstate ())
    in
    if Bool.not is_safe_tactic
    then Logger.warning ~__FUNCTION__ "unsafe tactic used";
    new_pstate
  ;;

  let exit_proof () : unit =
    ProofState.update_statem Done;
    raise NothingToDo
  ;;

  (** [step pstate] enters a fresh [module Step] for [pstate] and returns it after being updated by [Step.step ()] (followed by [simpl in *] and [subst]).
  *)
  let step (pstate : Declare.Proof.t) : Declare.Proof.t =
    Logger.trace __FUNCTION__;
    ProofState.update_pstate pstate;
    if Proof.is_done (Declare.Proof.get pstate) then exit_proof ();
    Proofview.Goal.enter (fun gl ->
      let module PStep : Proof_solver_step.S with type tactic = Tactic.t =
        (val make gl)
      in
      let x = PStep.step () in
      let y = PStep.run (PStep.Tacs.simplify_and_subst_all ()) in
      let z = Tactic.seq x y in
      Tactic.unpack z)
    |> get_updated_pstate
  ;;
end

(***********************************************************************)

(* The cache used to also carry the (module Logger.S) this solver was built
   with. Output now goes through Logger against the globally-installed sink, so
   only the solver is left. *)
type t = { solver : (module S) }

let the_cache : t ref option ref = ref None
let reset_the_cache () : unit = the_cache := None

exception NoCachedModules

let get_the_cache () : t ref =
  match !the_cache with None -> raise NoCachedModules | Some x -> x
;;

let get_the_proof_solver () : (module S) ref = ref !(get_the_cache ()).solver

let is_done () : bool =
  let module Ps = (val !(get_the_proof_solver ())) in
  Ps.ProofState.is_done ()
;;

let make (module Enc : Encoding.S) () : t ref =
  Logger.trace __FUNCTION__;
  let module Solver : S with type enc = Enc.t = Make (Enc) in
  the_cache := Some (ref { solver = (module Solver : S) });
  get_the_cache ()
;;

(***********************************************************************)

let stop_msg (x : int) : string =
  Printf.sprintf
    "(Stopped) %s after %i iterations."
    (if is_done () then "Solved" else "Unsolved")
    x
;;

(** What to say when [MeBi Sim Solve N] runs out of steps: which cofix
    strategy was in force (and whether [Auto] picked it), and the two ways on.
    An unsolved stop is not always a mistake -- a proof can be driven by
    several smaller [Solve]s -- so this rides on the [Notice] above rather
    than raising its own warning. *)
let exhausted_msg (bound : int) : string =
  let mutual : bool = !Api.the_mutual_cofix in
  Printf.sprintf
    "(The bound was reached: [MeBi Sim Solve %i] permits %i steps. The solver \
     is using a %s cofix%s. Raise the bound, or force the other strategy with \
     [MeBi Config Solver MutualCofix %s].)"
    bound
    (bound + 1)
    (if mutual then "mutual" else "nested")
    (match !Api.the_solver_strategy with
     | Api.Auto -> ", chosen by Auto"
     | Api.Nested | Api.Mutual -> ", as configured")
    (if mutual then "False" else "True")
;;

(** [step] ... *)
let step (pstate : Declare.Proof.t) : Declare.Proof.t =
  let module Ps : S = (val !(get_the_proof_solver ())) in
  Ps.step pstate
;;

(** [solve ?bound pstate] steps the proof until it closes or [bound] is reached.

    Completion is checked here, after each step, rather than being left to the
    next call to [step] (which raises [NothingToDo] once [Proof.is_done]). Doing
    it there cost a whole iteration to notice a proof that had already closed:
    with [bound] one below the number of productive steps the proof still
    closed and [Qed] succeeded, but the loop exited on the bound instead, so
    [statem] was never set to [Done] and this reported "Unsolved". That is why
    every [MeBi Sim Solve N] in the examples needed [N] to be one more than the
    work actually required. Existing bounds all still hold -- the requirement
    only ever got weaker. *)
let solve ?(bound : int = 10) (pstate : Declare.Proof.t) : Declare.Proof.t =
  Logger.trace __FUNCTION__;
  let module Ps : S = (val !(get_the_proof_solver ())) in
  let finished (p : Declare.Proof.t) : bool =
    Proof.is_done (Declare.Proof.get p)
  in
  (* [f] also says whether it stopped on [bound], as opposed to finishing or
     running out of things to do. *)
  let rec f (n : int) (p : Declare.Proof.t) : int * Declare.Proof.t * bool =
    Logger.thing ~__FUNCTION__ Debug "iter" n (Printf.sprintf "%i");
    if finished p
    then (
      Ps.ProofState.update_statem Done;
      n, p, false)
    else (
      match Int.compare n bound with
      | 1 -> n, p, true
      | _ ->
        (* The recursive call must be a genuine tail call, and nothing may
           capture [p] across it. Previously this read

           (try step p |> f (n + 1) with NothingToDo -> n, p)

           where the handler body mentions [p], so every frame kept its own
           [Declare.Proof.t] reachable for the whole command -- up to [bound]
           intermediate proof terms and evar maps alive at once, none
           collectable. That is why splitting one [Solve N] into several
           smaller [Solve] commands used to be the only way through: each
           command returned, unwound the recursion, and dropped the lot.
           Catching around [step p] alone keeps only the current [p] live. *)
        (match try Some (step p) with NothingToDo -> None with
         | None -> n, p, false
         | Some p' -> f (n + 1) p'))
  in
  let num, pstate, exhausted = f 0 pstate in
  Logger.notice (stop_msg num);
  if exhausted && Bool.not (is_done ()) then Logger.notice (exhausted_msg bound);
  pstate
;;

(** [init ] ... *)
let init
      ?(enc : unit -> (module Encoding.S) = Api.make_enc_int)
      (pstate : Declare.Proof.t)
      (refs : Libnames.qualid list)
      (a : Constrexpr.constr_expr * Libnames.qualid)
      (b : Constrexpr.constr_expr * Libnames.qualid)
  : Declare.Proof.t
  =
  Logger.trace __FUNCTION__;
  let module Enc : Encoding.S = (val enc ()) in
  let c : t ref = make (module Enc) () in
  let module Solver : S = (val !c.solver) in
  (* Which goal is this: [weak_bisimilar] (its product has both systems'
     obligations, [Model.Product.successors_bisim]) or [weak_sim] (where
     only similarity is asked, so states that are not bisimilar may still be
     fine)? *)
  let goal_head_is (name : string) : bool =
    let { Proof.goals; sigma; _ } = Proof.data (Declare.Proof.get pstate) in
    match goals with
    | g :: _ ->
      let concl = Evd.evar_concl (Evd.find_undefined sigma g) in
      let h, _ = EConstr.decompose_app sigma concl in
      EConstr.eq_constr sigma h (Mebi_theories.get name)
    | [] -> false
  in
  let goal_is_bisimilar : bool = goal_head_is "weak_bisimilar" in
  let goal_is_sim : bool = goal_head_is "weak_sim" in
  Solver.W.check_bisimilarity ~fail_if_not_bisim:(Bool.not goal_is_sim) refs a b;
  Solver.W.swapped := false;
  Solver.W.simulators := None;
  (* A [weak_sim] goal asks for similarity, which is coarser than
     bisimilarity: [a.b] is simulated by [a.(b + c)]. When the two states are
     not bisimilar, compute the greatest weak simulation, refuse only if they
     are not even similar, and give the solver each state's simulators to
     fall back on (see [Model.Product.answer]). *)
  (if
     goal_is_sim
     && Bool.not
          (Solver.W.Model.Bisimilarity.Result.are_bisimilar
             (Solver.W.get_the_result ()).result)
   then
     let module Model = Solver.W.Model in
     let fsm_a = Solver.W.get_fsm_a () in
     let fsm_b = Solver.W.get_fsm_b () in
     let sim : Model.Product.Pair.Set.t =
       Model.Product.simulation
         fsm_a
         fsm_b
         (Solver.W.get_fsm_b ~saturated:true ())
     in
     match fsm_a.init, fsm_b.init with
     | Some ra, Some rb ->
       if Model.Product.Pair.Set.mem (ra, rb) sim
       then (
         let table : (Model.State.t, Model.State.Set.t) Hashtbl.t =
           Hashtbl.create 64
         in
         Model.Product.Pair.Set.iter
           (fun ((x, y) : Model.Product.Pair.t) ->
             let ys =
               Stdlib.Option.value
                 (Hashtbl.find_opt table x)
                 ~default:Model.State.Set.empty
             in
             Hashtbl.replace table x (Model.State.Set.add y ys))
           sim;
         Logger.notice
           "(Not bisimilar, but similar: the proof search falls back on the \
            weak simulation preorder.)";
         Solver.W.simulators
         := Some
              (fun x ->
                Stdlib.Option.value
                  (Hashtbl.find_opt table x)
                  ~default:Model.State.Set.empty))
       else if !Api.the_fail_flags.non_bisimilar
       then
         CErrors.user_err
           (Pp.str
              "MeBi: the goal is weak_sim, but the left state is not weakly \
               simulated by the right one (no weak simulation relates them), \
               so there is no proof to find. [MeBi Config FailIf NotBisimilar \
               False] carries on regardless.")
       else
         Logger.warning
           "The left state is not weakly simulated by the right one; the proof \
            search will not close."
     | _ -> ());
  (* The answer policy. [Default] answers move by move with
     [Model.Product.answer], as the solver always has, and builds nothing.
     Anything else is planned here, once: the answer to every move the game
     can reach, which the solver, the mutual block and the estimate below
     all read, so they cannot disagree. *)
  Solver.W.plan := None;
  (* An FSM saturated on demand (notes/13) is too large to walk the whole
     game of up front: planning answers, or [Auto]'s estimate below, would
     saturate state after state, again and again as the cache drops them
     ([Proc/Test4]: still running after 29 minutes). Both are skipped, with a
     notice, and the proof answers move by move on the nested path. *)
  let on_demand : bool =
    Stdlib.Option.is_some (Solver.W.get_fsm_a ~saturated:true ()).fill
    || Stdlib.Option.is_some (Solver.W.get_fsm_b ~saturated:true ()).fill
  in
  if
    on_demand
    && (!Api.the_answer_policy <> Api.Answers_default
        || !Api.the_solver_strategy = Api.Auto)
  then
    Logger.notice
      "(Saturated on demand: answers are chosen move by move (Answers \
       Default), and Auto takes the nested cofix without estimating, as either \
       would walk the whole game up front.)";
  (if !Api.the_answer_policy <> Api.Answers_default && Bool.not on_demand
   then
     let module P = Solver.W.Model.Product in
     let fsm_a = Solver.W.get_fsm_a () in
     let fsm_b = Solver.W.get_fsm_b () in
     let pi = Solver.W.get_bisimilar_partition () in
     match fsm_a.init, fsm_b.init with
     | Some ra, Some rb ->
       let refl = Libnames.qualid_eq (snd a) (snd b) in
       let game =
         if goal_is_bisimilar
         then
           P.Policy.bisim_game
             ~refl
             { a = fsm_a
             ; a_saturated = Solver.W.get_fsm_a ~saturated:true ()
             ; b = fsm_b
             ; b_saturated = Solver.W.get_fsm_b ~saturated:true ()
             }
             pi
         else
           P.Policy.sim_game
             ~silent:fsm_b.edges
             ?sim:!Solver.W.simulators
             ~refl
             fsm_a
             (Solver.W.get_fsm_b ~saturated:true ())
             pi
       in
       let p : P.Policy.plan =
         match !Api.the_answer_policy with
         | Api.Answers_greedy -> P.Policy.plan P.Policy.Greedy game (ra, rb)
         | Api.Answers_minimal -> P.Policy.plan P.Policy.Minimal game (ra, rb)
         | Api.Answers_auto | Api.Answers_default -> P.Policy.best game (ra, rb)
       in
       if p.measure.unanswered > 0
       then
         Logger.warning
           (Printf.sprintf
              "(Answers: the %s plan leaves %i moves unanswered; answering \
               move by move instead.)"
              (P.Policy.name p.policy)
              p.measure.unanswered)
       else (
         if p.policy <> P.Policy.Default then Solver.W.plan := Some p;
         Logger.notice
           (Printf.sprintf
              "(Answers: %s -- %i pairs, %i moves, witness %i; predicted %.0f \
               iterations.)"
              (P.Policy.name p.policy)
              p.measure.pairs
              p.measure.moves
              p.measure.witness
              (P.Policy.predicted p.measure)))
     | _ -> ());
  (* [Auto] decides here, once, before any proof step runs. The product is
     already known at this point, so both strategies can simply be measured:
     a mutual cofix visits each game state once and each move once, while a
     nested cofix walks the tree of simple paths because it can only close a
     repeat that is an ancestor. The nested walk is capped at a small multiple
     of the mutual cost -- the exact figure does not matter, only whether it
     is larger. See [Model.Product.estimate]. *)
  (match !Api.the_solver_strategy with
   | Api.Nested | Api.Mutual -> ()
   | Api.Auto when on_demand -> Api.set_mutual_cofix false
   | Api.Auto ->
     let module S = Solver in
     let fsm_a = S.W.get_fsm_a () in
     let fsm_b = S.W.get_fsm_b ~saturated:true () in
     let pi = S.W.get_bisimilar_partition () in
     (match fsm_a.init, fsm_b.init with
      | Some ra, Some rb ->
        (* The goal is not built yet, so compare the LTS names rather than
           the terms [Concl.is_weak_refl] will see. Two names for one LTS
           only lose the refl short-cut, which over-predicts -- cost, never
           a missing pair. *)
        let refl = Libnames.qualid_eq (snd a) (snd b) in
        let silent = (S.W.get_fsm_b ()).edges in
        let c =
          match !S.W.plan with
          | Some p -> S.W.Model.Product.estimate_plan p
          | None ->
            if goal_is_bisimilar
            then
              S.W.Model.Product.estimate_bisim
                ~refl
                { a = fsm_a
                ; a_saturated = S.W.get_fsm_a ~saturated:true ()
                ; b = S.W.get_fsm_b ()
                ; b_saturated = fsm_b
                }
                pi
                (ra, rb)
            else
              S.W.Model.Product.estimate
                ~silent
                ?sim:!S.W.simulators
                ~refl
                fsm_a
                fsm_b
                pi
                (ra, rb)
        in
        let use_mutual = S.W.Model.Product.prefer_mutual c in
        Api.set_mutual_cofix use_mutual;
        (* Only the mutual path is announced. It is the deviation from what
           the solver has always done, it changes the iteration count a
           checked-in [MeBi Sim Solve] bound was measured against, and on a
           product where it matters it is the difference between finishing and
           not. Staying on the nested path is the status quo and says
           nothing. *)
        if use_mutual
        then
          Logger.notice
            (Printf.sprintf
               "(Auto: mutual cofix -- %i pairs, %i moves; a nested cofix \
                would visit %s goals.)"
               c.pairs
               c.moves
               (match c.nested with
                | Some n -> Printf.sprintf "%i" n
                | None -> Printf.sprintf "over %i" (4 * (c.pairs + c.moves))))
        else
          Logger.debug
            (Printf.sprintf
               "(Auto: nested cofix -- %i pairs, %i moves, nested walk %s.)"
               c.pairs
               c.moves
               (match c.nested with
                | Some n -> Printf.sprintf "%i" n
                | None -> "capped"))
      | _ -> Api.set_mutual_cofix false));
  Solver.ProofState.init pstate (fst a, fst b);
  pstate
;;

(** [guard f] runs a [MeBi Sim] command. An exception from inside the plugin
    that nothing handled would reach Rocq as an {e Anomaly} ("please report
    at rocq-prover.org"), blaming Rocq for a plugin failure -- as
    [BindingInstruction_NotApp] and [CannotGetTransition] did on 2026-10-01.
    Those -- any exception Rocq has no printer for, which it would report
    as "Uncaught exception" -- become a user error that names the exception
    and says whose problem it is. Exceptions with a registered printer -- Rocq's own errors,
    tactic failures, [MEBI_exn] -- pass through unchanged. *)
let guard (f : unit -> 'a) : 'a =
  (* The step logic runs inside a tactic ([Proofview.Goal.enter]), whose
     engine wraps an exception raised there: look through that wrapper, or a
     wrapped plugin exception still escapes as an Anomaly (as
     [CannotGetTransition] did on 2026-10-02). *)
  let rec root : exn -> exn = function
    | Logic_monad.TacticFailure e -> root e
    | e -> e
  in
  (* Internal = Rocq has no printer for it, so it would print as an
     "Uncaught exception" Anomaly. Not a name test: exceptions from the
     plugin's own [lib/] libraries are not [Mebi_plugin.]-prefixed
     ([Rocq_utils_HypIsNot_Atomic], 2026-10-02). *)
  let internal (e : exn) : bool =
    let printed = Pp.string_of_ppcmds (CErrors.print_no_report e) in
    let sub = "Uncaught exception" in
    let n = String.length sub in
    let rec has (i : int) : bool =
      i + n <= String.length printed
      && (String.equal (String.sub printed i n) sub || has (i + 1))
    in
    has 0
  in
  try f () with
  | e when CErrors.noncritical e && internal (root e) ->
    let e = root e in
    CErrors.user_err
      (Pp.str
         (Printf.sprintf
            "MeBi: the proof search stopped on an internal error it does not \
             handle:\n\
            \  %s\n\
             This is a problem in the MeBi plugin, not in Rocq. If the \
             constructors involved have premises that are not over an LTS, see \
             [MeBi Help Premises]."
            (Printexc.to_string e)))
;;
