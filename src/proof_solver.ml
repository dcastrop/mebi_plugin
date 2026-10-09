(* One exception for "nothing to do", whether this module or a step
   finds it: {!solve} stops on either. *)
exception NothingToDo = Proof_solver_step.NothingToDo

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

  (* Our own wrapper for ['a Proofview.tactic]: a tactic can carry a message
     to print (at a configurable [Feedback.level]), and sequencing and
     chaining are explicit. A tactic built with it must be unpacked
     ([Tactic.unpack]) before Rocq can run it. *)
  module Tactic : Proof_solver_tactic.S = Proof_solver_tactic.Make

  (* The main part of the algorithm, run before the proof: a [Wrapper.S]
     inside a [Results.S], which keeps the bisimilarity result and reads it
     for the search. *)
  module W = Results.Make (Enc)

  (* The proof solver's state machine. It needs [W] because some states
     ([Exists t], say) hold what the bisimilarity check found. Only the
     structure is here: [Proof_solver_step] walks it. *)
  module ProofState = Proof_solver_statem.Make (Enc) (W)

  (* A [Proof_solver_theory.S] for each step. Most of it depends only on
     [Enc] and [W], so the functor takes just the step's own
     [Proof_solver_wrapper.S], built from that step's goal. *)
  module TheoryMaker = Proof_solver_theory.Make (Enc) (W)

  (* A [Proof_solver_step.S] for the goal in focus, the only thing that
     changes from one step to the next. *)
  module Step =
    Proof_solver_step.Make (Enc) (Tactic) (W) (ProofState) (TheoryMaker)

  (** [make gl] is a {!Step} for the goal [gl]. Raises nothing. *)
  let make (gl : Proofview.Goal.t)
    : (module Proof_solver_step.S with type tactic = Tactic.t)
    =
    (module Step (struct
         let gl = ref gl
       end))
  ;;

  (* See the [.mli]. *)
  let get_updated_pstate (x : unit Proofview.tactic) : Declare.Proof.t =
    Logger.trace __FUNCTION__;
    let new_pstate, is_safe_tactic =
      Declare.Proof.by (Global.env ()) x (ProofState.get_pstate ())
    in
    if Bool.not is_safe_tactic
    then Logger.warning ~__FUNCTION__ "unsafe tactic used";
    new_pstate
  ;;

  (** [exit_proof ()] sets the state machine to [Done] and stops the step.

      @raise NothingToDo always (raised here). *)
  let exit_proof () : unit =
    ProofState.update_statem Done;
    raise NothingToDo
  ;;

  (* See the [.mli]. A fresh {!Step} is entered for the goal in focus
     ([Proofview.Goal.enter]), so each step reads that goal's [env] and
     [sigma]. The step runs inside that tactic, whose engine wraps an
     exception raised there, so its [NothingToDo] is unwrapped for
     {!solve}. *)
  let step (pstate : Declare.Proof.t) : Declare.Proof.t =
    Logger.trace __FUNCTION__;
    ProofState.update_pstate pstate;
    if Proof.is_done (Declare.Proof.get pstate) then exit_proof ();
    try
      Proofview.Goal.enter (fun gl ->
        let module PStep : Proof_solver_step.S with type tactic = Tactic.t =
          (val make gl)
        in
        let x = PStep.step () in
        let y = PStep.run (PStep.Tacs.simplify_and_subst_all ()) in
        let z = Tactic.seq x y in
        Tactic.unpack z)
      |> get_updated_pstate
    with
    | Logic_monad.TacticFailure NothingToDo -> raise NothingToDo
  ;;
end

(***********************************************************************)

(* The cache used to also carry the (module Logger.S) this solver was built
   with. Output now goes through Logger against the globally-installed sink, so
   only the solver is left. *)
type t = { solver : (module S) }

(** The cached solver, if {!make} has built one. *)
let the_cache : t ref option ref = ref None

exception NoCachedModules

(** [get_the_cache ()] is the cached solver.

    @raise NoCachedModules if none has been built (raised here). *)
let get_the_cache () : t ref =
  match !the_cache with None -> raise NoCachedModules | Some x -> x
;;

(** [get_the_proof_solver ()] is the cached solver's module, in a fresh
    reference.

    @raise NoCachedModules if none has been built (propagated). *)
let get_the_proof_solver () : (module S) ref = ref !(get_the_cache ()).solver

(* See the [.mli]. *)
let is_done () : bool =
  let module Ps = (val !(get_the_proof_solver ())) in
  Ps.ProofState.is_done ()
;;

(* See the [.mli]. *)
let make (module Enc : Encoding.S) () : t ref =
  Logger.trace __FUNCTION__;
  let module Solver : S with type enc = Enc.t = Make (Enc) in
  the_cache := Some (ref { solver = (module Solver : S) });
  get_the_cache ()
;;

(***********************************************************************)

(** [stop_msg n] is the line {!solve} ends with:
    ["(Stopped) Solved after n iterations."], or [Unsolved].

    @raise NoCachedModules if no solver has been built (propagated). *)
let stop_msg (x : int) : string =
  Printf.sprintf
    "(Stopped) %s after %i iterations."
    (if is_done () then "Solved" else "Unsolved")
    x
;;

(** [exhausted_msg bound] is what to say when [MeBi Sim Solve bound] runs
    out of steps: which cofix strategy was in force (and whether [Auto]
    picked it), and the two ways on. An unsolved stop is not always a
    mistake -- a proof can be driven by several smaller [Solve]s -- so this
    rides on the [Notice] above rather than raising its own warning. Raises
    nothing. *)
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

(* See the [.mli]. *)
let step (pstate : Declare.Proof.t) : Declare.Proof.t =
  let module Ps : S = (val !(get_the_proof_solver ())) in
  Ps.step pstate
;;

(* See the [.mli]. Completion is checked here, after each step, rather than
   being left to the next call to [step] (which raises [NothingToDo] once
   [Proof.is_done]). Doing it there cost a whole iteration to notice a proof
   that had already closed: with [bound] one below the number of productive
   steps the proof still closed and [Qed] succeeded, but the loop exited on
   the bound instead, so [statem] was never set to [Done] and this reported
   "Unsolved". That is why every [MeBi Sim Solve N] in the examples needed
   [N] to be one more than the work actually required. Existing bounds all
   still hold -- the requirement only ever got weaker. *)
let solve ?(bound : int = 10) (pstate : Declare.Proof.t) : Declare.Proof.t =
  Logger.trace __FUNCTION__;
  let module Ps : S = (val !(get_the_proof_solver ())) in
  (* [finished p] is whether [p] has no goals left *)
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

(** [goal_head_is pstate name] is whether the goal in focus of [pstate] is
    headed by the theory constant [name] ({!Mebi_theories.get}); [false] if
    there is no goal.

    @raise Failure if [name] is not a theory constant (propagated). *)
let goal_head_is (pstate : Declare.Proof.t) (name : string) : bool =
  let { Proof.goals; sigma; _ } = Proof.data (Declare.Proof.get pstate) in
  match goals with
  | g :: _ ->
    let concl = Evd.evar_concl (Evd.find_undefined sigma g) in
    let h, _ = EConstr.decompose_app sigma concl in
    EConstr.eq_constr sigma h (Mebi_theories.get name)
  | [] -> false
;;

(** [answer_policy_setting ()] is the command that set the answer policy,
    if it is not [Default]. Raises nothing. *)
let answer_policy_setting () : string option =
  match !Api.the_answer_policy with
  | Api.Answers_default -> None
  | Api.Answers_greedy -> Some "MeBi Config Solver Answers Greedy"
  | Api.Answers_minimal -> Some "MeBi Config Solver Answers Minimal"
  | Api.Answers_auto -> Some "MeBi Config Solver Answers Auto"
;;

(** [explicit_whole_game_settings ()] is the settings made explicitly that
    walk the whole game up front -- [MutualCofix True], and an answer
    policy other than [Default] -- as the commands that set them. Raises
    nothing. *)
let explicit_whole_game_settings () : string list =
  (if !Api.the_solver_strategy = Api.Mutual
   then [ "MeBi Config Solver MutualCofix True" ]
   else [])
  @ Stdlib.Option.to_list (answer_policy_setting ())
;;

(** [refuse_whole_game setting why] refuses [setting], which walks the
    whole game up front, on an FSM saturated on demand, saying [why] and
    the ways on.

    @raise CErrors.UserError always (raised here). *)
let refuse_whole_game (setting : string) (why : string) : 'a =
  CErrors.user_err
    (Pp.str
       (Printf.sprintf
          "MeBi: [%s] plans the whole proof up front, walking every pair of \
           states the proof can reach, and an FSM here is saturated on demand \
           (too large to saturate whole; see the warning above), so that walk \
           saturates state after state as it goes. %s Or use [MeBi Config \
           Solver MutualCofix Auto] or [False], and [MeBi Config Solver \
           Answers Default]."
          setting
          why))
;;

(** [refuse_unbounded setting] is {!refuse_whole_game} for [setting] with
    no game bound set.

    @raise CErrors.UserError always (raised here). *)
let refuse_unbounded (setting : string) : 'a =
  refuse_whole_game
    setting
    "Bound it with [MeBi Config Bounds Game <n>] (pairs) to allow it."
;;

(** [refuse_exceeded setting n] is {!refuse_whole_game} for [setting], the
    game having more than [n] pairs, the bound set.

    @raise CErrors.UserError always (raised here). *)
let refuse_exceeded (setting : string) (n : int) : 'a =
  refuse_whole_game
    setting
    (Printf.sprintf
       "The game has more than %i pairs, the bound set with [MeBi Config \
        Bounds Game %i]: raise it to allow it."
       n
       n)
;;

(** [Init (Solver)] is what {!init} decides for the solver [Solver] before
    the proof's first step, one function per decision. *)
module Init (Solver : S) = struct
  module W = Solver.W
  module Product = Solver.W.Model.Product

  (** [add_simulator table (x, y)] records in [table] that [y] simulates
      [x]. Raises nothing. *)
  let add_simulator
        (table : (W.Model.State.t, W.Model.State.Set.t) Hashtbl.t)
        ((x, y) : Product.Pair.t)
    : unit
    =
    let ys =
      Stdlib.Option.value
        (Hashtbl.find_opt table x)
        ~default:W.Model.State.Set.empty
    in
    Hashtbl.replace table x (W.Model.State.Set.add y ys)
  ;;

  (** [simulators_of sim] is the function giving each state its
      simulators in the weak simulation [sim] (the empty set for a state
      [sim] does not relate). Raises nothing. *)
  let simulators_of (sim : Product.Pair.Set.t)
    : W.Model.State.t -> W.Model.State.Set.t
    =
    let table : (W.Model.State.t, W.Model.State.Set.t) Hashtbl.t =
      Hashtbl.create 64
    in
    Product.Pair.Set.iter (add_simulator table) sim;
    fun x ->
      Stdlib.Option.value
        (Hashtbl.find_opt table x)
        ~default:W.Model.State.Set.empty
  ;;

  (** [fall_back_on_simulation ()] is for a [weak_sim] goal whose two
      states are not bisimilar. A [weak_sim] goal asks for similarity, which
      is coarser than bisimilarity: [a.b] is simulated by [a.(b + c)]. So it
      computes the greatest weak simulation ({!Wrapper.S.similarity}: only
      the pairs reachable from the two start states, and on demand only
      within [MeBi Config Bounds Game]) and, if it relates the start states,
      gives the solver each state's simulators to fall back on
      ({!Results.S.simulators}, read by {!Model.Product.answer}). If it does
      not, there is no proof to find.

      @raise CErrors.UserError
        if the left state is not simulated by the right one and
        [FailIf NotBisimilar] is set (raised here; otherwise a warning). *)
  let fall_back_on_simulation () : unit =
    let fsm_a = W.get_fsm_a () in
    let fsm_b = W.get_fsm_b () in
    match W.similarity (W.get_the_result ()), fsm_a.init, fsm_b.init with
    | Some sim, Some ra, Some rb ->
      if Product.Pair.Set.mem (ra, rb) sim
      then (
        let simulators = simulators_of sim in
        Logger.notice
          "(Not bisimilar, but similar: the proof search falls back on the \
           weak simulation preorder.)";
        W.simulators := Some simulators)
      else if !Api.the_fail_flags.non_bisimilar
      then
        CErrors.user_err
          (Pp.str
             "MeBi: the goal is weak_sim, but the left state is not weakly \
              simulated by the right one (no weak simulation relates them), so \
              there is no proof to find. [MeBi Config FailIf NotBisimilar \
              False] carries on regardless.")
      else
        Logger.warning
          "The left state is not weakly simulated by the right one; the proof \
           search will not close."
    | _, _, _ -> ()
  ;;

  (** [on_demand ()] is whether either FSM is saturated on demand
      (notes/13). Raises nothing. *)
  let on_demand () : bool =
    Stdlib.Option.is_some (W.get_fsm_a ~saturated:true ()).fill
    || Stdlib.Option.is_some (W.get_fsm_b ~saturated:true ()).fill
  ;;

  (** [capped bound f] is [Ok (f ())], or [Error n] if [bound] is [Some n]
      and [f] walks past [n] pairs of the game ({!Model.Product.with_cap}).

      Raises whatever [f] raises (propagated), other than the bound being
      reached. *)
  let capped (bound : int option) (f : unit -> 'a) : ('a, int) result =
    match bound with
    | None -> Ok (f ())
    | Some n ->
      (try Ok (Product.with_cap n f) with Product.Game_too_large n -> Error n)
  ;;

  (** [plan_answers ~goal_is_bisimilar ~refl bound] plans the answer to
      every move the game can reach, under the answer policy in force
      ({!Model.Product.Policy}), within [bound]. The plan becomes
      {!Results.S.plan}, which the solver, the mutual block and the estimate
      all read, so they cannot disagree; a [Default] plan is not kept, and a
      plan that leaves moves unanswered is dropped with a warning (the
      solver then answers move by move). [refl] is whether the two systems
      are the same LTS.

      @raise CErrors.UserError
        if the walk goes past [bound] (raised by {!refuse_exceeded}). Also
        raises as {!Model.Product.Policy.plan} (propagated). *)
  let plan_answers ~(goal_is_bisimilar : bool) ~(refl : bool) bound : unit =
    let fsm_a = W.get_fsm_a () in
    let fsm_b = W.get_fsm_b () in
    let pi = W.get_bisimilar_partition () in
    match fsm_a.init, fsm_b.init with
    | Some ra, Some rb ->
      let game =
        if goal_is_bisimilar
        then
          Product.Policy.bisim_game
            ~refl
            { a = fsm_a
            ; a_saturated = W.get_fsm_a ~saturated:true ()
            ; b = fsm_b
            ; b_saturated = W.get_fsm_b ~saturated:true ()
            }
            pi
        else
          Product.Policy.sim_game
            ~silent:fsm_b.edges
            ?sim:!W.simulators
            ~refl
            fsm_a
            (W.get_fsm_b ~saturated:true ())
            pi
      in
      let p : Product.Policy.plan =
        match
          capped bound (fun () ->
            match !Api.the_answer_policy with
            | Api.Answers_greedy ->
              Product.Policy.plan Product.Policy.Greedy game (ra, rb)
            | Api.Answers_minimal ->
              Product.Policy.plan Product.Policy.Minimal game (ra, rb)
            | Api.Answers_auto | Api.Answers_default ->
              Product.Policy.best game (ra, rb))
        with
        | Ok p -> p
        | Error n ->
          refuse_exceeded (Stdlib.Option.get (answer_policy_setting ())) n
      in
      if p.measure.unanswered > 0
      then
        Logger.warning
          (Printf.sprintf
             "(Answers: the %s plan leaves %i moves unanswered; answering move \
              by move instead.)"
             (Product.Policy.name p.policy)
             p.measure.unanswered)
      else (
        if p.policy <> Product.Policy.Default then W.plan := Some p;
        Logger.notice
          (Printf.sprintf
             "(Answers: %s -- %i pairs, %i moves, witness %i; predicted %.0f \
              iterations.)"
             (Product.Policy.name p.policy)
             p.measure.pairs
             p.measure.moves
             p.measure.witness
             (Product.Policy.predicted p.measure)))
    | _ -> ()
  ;;

  (** [announce_auto c use_mutual] says which cofix [Auto] chose, from the
      estimate [c]. Only the mutual path is announced (a [Notice]): it is
      the deviation from what the solver has always done, it changes the
      iteration count a checked-in [MeBi Sim Solve] bound was measured
      against, and on a product where it matters it is the difference
      between finishing and not. Staying on the nested path is the status
      quo, logged at [Debug]. Raises nothing. *)
  let announce_auto (c : Product.cost) (use_mutual : bool) : unit =
    if use_mutual
    then
      Logger.notice
        (Printf.sprintf
           "(Auto: mutual cofix -- %i pairs, %i moves; a nested cofix would \
            visit %s goals.)"
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
  ;;

  (** [choose_cofix_strategy ~on_demand ~goal_is_bisimilar ~refl bound]
      sets the cofix strategy for the proof ({!Api.set_mutual_cofix}).
      [Auto] decides here, once, before any proof step runs. The product is
      already known at this point, so both strategies can simply be
      measured: a mutual cofix visits each game state once and each move
      once, while a nested cofix walks the tree of simple paths because it
      can only close a repeat that is an ancestor. The nested walk is capped
      at a small multiple of the mutual cost -- the exact figure does not
      matter, only whether it is larger. See {!Model.Product.estimate}. On
      an FSM saturated on demand, [Auto] estimates only within [bound], and
      takes the nested cofix without one or past it; a forced [Mutual] is
      checked against [bound] the same way.

      @raise CErrors.UserError
        if a forced [Mutual] walks past [bound] (raised by
        {!refuse_exceeded}). *)
  let choose_cofix_strategy
        ~(on_demand : bool)
        ~(goal_is_bisimilar : bool)
        ~(refl : bool)
        (bound : int option)
    : unit
    =
    match !Api.the_solver_strategy with
    | Api.Nested -> ()
    | Api.Mutual when Bool.not on_demand -> ()
    | Api.Auto when on_demand && Stdlib.Option.is_none bound ->
      Api.set_mutual_cofix false
    | (Api.Auto | Api.Mutual) as strategy ->
      let fsm_a = W.get_fsm_a () in
      let fsm_b = W.get_fsm_b ~saturated:true () in
      let pi = W.get_bisimilar_partition () in
      (match fsm_a.init, fsm_b.init with
       | Some ra, Some rb ->
         let silent = (W.get_fsm_b ()).edges in
         (* [estimate ()] is the cost of both strategies: the plan's, if
            there is one, else the game's from [(ra, rb)] *)
         let estimate () : Product.cost =
           match !W.plan with
           | Some p -> Product.estimate_plan p
           | None ->
             if goal_is_bisimilar
             then
               Product.estimate_bisim
                 ~refl
                 { a = fsm_a
                 ; a_saturated = W.get_fsm_a ~saturated:true ()
                 ; b = W.get_fsm_b ()
                 ; b_saturated = fsm_b
                 }
                 pi
                 (ra, rb)
             else
               Product.estimate
                 ~silent
                 ?sim:!W.simulators
                 ~refl
                 fsm_a
                 fsm_b
                 pi
                 (ra, rb)
         in
         (match capped bound estimate with
          | Error n when strategy = Api.Mutual ->
            refuse_exceeded "MeBi Config Solver MutualCofix True" n
          | Error n ->
            Api.set_mutual_cofix false;
            Logger.notice
              (Printf.sprintf
                 "(Saturated on demand: the game has more than %i pairs, the \
                  bound set with [MeBi Config Bounds Game %i]; Auto takes the \
                  nested cofix.)"
                 n
                 n)
          | Ok _ when strategy = Api.Mutual -> ()
          | Ok c ->
            let use_mutual = Product.prefer_mutual c in
            Api.set_mutual_cofix use_mutual;
            announce_auto c use_mutual)
       | _ -> Api.set_mutual_cofix false)
  ;;
end

(* See the [.mli]. The steps run in the order listed there, each a function
   of {!Init}. *)
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
  let module I = Init (Solver) in
  (* Which goal is this: [weak_bisimilar] (its product has both systems'
     obligations, [Model.Product.successors_bisim]) or [weak_sim] (where
     only similarity is asked, so states that are not bisimilar may still be
     fine)? *)
  let goal_is_bisimilar : bool = goal_head_is pstate "weak_bisimilar" in
  let goal_is_sim : bool = goal_head_is pstate "weak_sim" in
  Solver.W.check_bisimilarity ~fail_if_not_bisim:(Bool.not goal_is_sim) refs a b;
  Solver.W.swapped := false;
  Solver.W.simulators := None;
  if
    goal_is_sim
    && Bool.not
         (Solver.W.Model.Bisimilarity.Result.are_bisimilar
            (Solver.W.get_the_result ()).result)
  then I.fall_back_on_simulation ();
  (* The answer policy. [Default] answers move by move with
     [Model.Product.answer], as the solver always has, and builds nothing.
     Anything else is planned here, once ({!Init.plan_answers}). *)
  Solver.W.plan := None;
  (* An FSM saturated on demand (notes/13) may be too large to walk the
     whole game of up front: planning answers, the mutual cofix's pair set,
     or [Auto]'s estimate, saturate state after state as they walk
     ([Proc/Test4]: still running after 29 minutes). A setting the user made
     explicitly -- [MutualCofix True], an answer policy other than
     [Default] -- needs [MeBi Config Bounds Game <n>] on demand, and is
     refused, naming the bound, if the walk goes past it. [Auto] is the
     tool's own choice: without the bound it takes the nested cofix without
     estimating; with it, it estimates within the bound, and past it takes
     the nested cofix. (2026-10-03; before, the explicit settings were
     refused outright, and before that silently downgraded or stalled.) *)
  let on_demand : bool = I.on_demand () in
  let bound : int option = if on_demand then !Api.the_game_bound else None in
  if on_demand
  then (
    (match explicit_whole_game_settings (), bound with
     | setting :: _, None -> refuse_unbounded setting
     | _ -> ());
    if !Api.the_solver_strategy = Api.Auto && Stdlib.Option.is_none bound
    then
      Logger.notice
        "(Saturated on demand: Auto takes the nested cofix without estimating, \
         as estimating would walk the whole game up front. [MeBi Config Bounds \
         Game <n>] lets it estimate within <n> pairs.)");
  (* The goal is not built yet, so compare the LTS names rather than the
     terms [Concl.is_weak_refl] will see. Two names for one LTS only lose
     the refl short-cut, which over-predicts -- cost, never a missing
     pair. *)
  let refl : bool = Libnames.qualid_eq (snd a) (snd b) in
  if
    !Api.the_answer_policy <> Api.Answers_default
    && ((not on_demand) || Stdlib.Option.is_some bound)
  then I.plan_answers ~goal_is_bisimilar ~refl bound;
  I.choose_cofix_strategy ~on_demand ~goal_is_bisimilar ~refl bound;
  Solver.ProofState.init pstate (fst a, fst b);
  pstate
;;

(* See the [.mli]. *)
type goal_kind =
  | Bisim
  | Sim

(** [goal_statement kind (x, a) (y, b)] is the statement to prove, as an
    expression still to interpret: [weak_bisimilar a b x y] ([Bisim]) or
    [weak_sim a b x y] ([Sim]), [x] stepping by the relation [a] and [y] by
    [b]. Raises nothing. *)
let goal_statement
      (kind : goal_kind)
      ((x, a) : Constrexpr.constr_expr * Libnames.qualid)
      ((y, b) : Constrexpr.constr_expr * Libnames.qualid)
  : Constrexpr.constr_expr
  =
  let head : string =
    match kind with
    | Bisim -> "MEBI.Bisimilarity.weak_bisimilar"
    | Sim -> "MEBI.Bisimilarity.weak_sim"
  in
  Constrexpr_ops.mkAppC
    ( Constrexpr_ops.mkRefC (Libnames.qualid_of_string head)
    , [ Constrexpr_ops.mkRefC a; Constrexpr_ops.mkRefC b; x; y ] )
;;

(* See the [.mli]. The check is the one [MeBi Sim Begin] would run, done
   once: nothing is kept after the command. *)
let start
      ~(kind : goal_kind)
      ~(name : Names.Id.t)
      (refs : Libnames.qualid list)
      (a : Constrexpr.constr_expr * Libnames.qualid)
      (b : Constrexpr.constr_expr * Libnames.qualid)
  : Declare.Proof.t
  =
  Logger.trace __FUNCTION__;
  let env = Global.env () in
  let sigma = Evd.from_env env in
  let typ, uctx =
    Constrintern.interp_type env sigma (goal_statement kind a b)
  in
  let pstate =
    Declare.Proof.start
      ~info:(Declare.Info.make ~kind:Decls.(IsDefinition Example) ())
      ~cinfo:(Declare.CInfo.make ~name ~typ ())
      (Evd.from_ctx uctx)
  in
  init pstate refs a b
;;

(** [contains ~sub s] is whether [sub] occurs in [s]. Raises nothing. *)
let contains ~(sub : string) (s : string) : bool =
  let n = String.length sub in
  (* [from i] is whether [sub] occurs in [s] at [i] or after *)
  let rec from (i : int) : bool =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || from (i + 1))
  in
  from 0
;;

(* See the [.mli]. An exception from inside the plugin that nothing handled
   would reach Rocq as an Anomaly ("please report at rocq-prover.org"),
   blaming Rocq for a plugin failure -- as [BindingInstruction_NotApp] and
   [CannotGetTransition] did on 2026-10-01. Those -- any exception Rocq has
   no printer for, which it would report as "Uncaught exception" -- become a
   user error that names the exception and says whose problem it is.
   Exceptions with a registered printer -- Rocq's own errors, tactic
   failures, [MEBI_exn] -- pass through unchanged. *)
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
    contains
      ~sub:"Uncaught exception"
      (Pp.string_of_ppcmds (CErrors.print_no_report e))
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
