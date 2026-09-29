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
  let rec f (n : int) (p : Declare.Proof.t) : int * Declare.Proof.t =
    Logger.thing ~__FUNCTION__ Debug "iter" n (Printf.sprintf "%i");
    if finished p
    then (
      Ps.ProofState.update_statem Done;
      n, p)
    else (
      match Int.compare n bound with
      | 1 -> n, p
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
         | None -> n, p
         | Some p' -> f (n + 1) p'))
  in
  let num, pstate = f 0 pstate in
  Logger.notice (stop_msg num);
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
  Solver.W.check_bisimilarity refs a b;
  (* [Auto] decides here, once, before any proof step runs. The product is
     already known at this point, so both strategies can simply be measured:
     a mutual cofix visits each game state once and each move once, while a
     nested cofix walks the tree of simple paths because it can only close a
     repeat that is an ancestor. The nested walk is capped at a small multiple
     of the mutual cost -- the exact figure does not matter, only whether it
     is larger. See [Model.Product.estimate]. *)
  (match !Api.the_solver_strategy with
   | Api.Nested | Api.Mutual -> ()
   | Api.Auto ->
     let module S = Solver in
     let fsm_a = S.W.get_fsm_a () in
     let fsm_b = S.W.get_fsm_b ~saturated:true () in
     let pi = S.W.get_bisimilar_partition () in
     (match fsm_a.init, fsm_b.init with
      | Some ra, Some rb ->
        let c = S.W.Model.Product.estimate fsm_a fsm_b pi (ra, rb) in
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
