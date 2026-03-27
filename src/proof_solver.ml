exception NothingToDo
exception NotImplemented

module type S = sig
  type enc
  type node
  type tree
  type trees

  module Tactic : Proof_solver_tactic.S

  module W :
    Results_.S
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

module Make (Log : Logger.S) (Ctx : Rocq_context.S) (Enc : Encoding.S) :
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
  module Tactic : Proof_solver_tactic.S = Proof_solver_tactic.Make (Log)

  (** [module W] is for running the main part of the algorithm (pre-proof). It is a standard [module Wrapper_.S] which itself is wrapped in a [module Results_.S] which stores the results and provides some useful functions for using the bisimilarity result.
  *)
  module W = Results.Make (Log) (Ctx) (Enc)

  (** [module ProofState] sets up the different internal states of the proof-solver. We require [module Results_.S] since some of the internal states {i (e.g., [Exists transition_opt])} store some information from the proof that corresponds to information captured in the initial run of the bisimilarity checking algorithm. {i {b Note:} this 'proof-state-machine' is not actually handled here, it is only the structure.}
    @see [module Proof_solver_step] for how these states are traversed in order to solve the proof.
    *)
  module ProofState = Proof_solver_statem.Make (Log) (Enc) (W)

  (** [module TheoryMaker] is a functor that allows us to create a [module Proof_solver_theory.S] for each iteration (step) of the proof-solver. Since a fair amount of it only relies on [module Enc] and the results of the command in [module W], this functor just takes the [module Proof_solver_wrapper.S] created from the [Proofview.Goal.t] of the proof in each proof-step.
  *)
  module TheoryMaker = Proof_solver_theory.Make (Log) (Enc) (W)

  (** [module Step] is a functor for returning a [module Proof_solver_step.S] for handling the current [Proofview.Goal.t], which is the only thing that will change for each proof-step.
  *)
  module Step =
    Proof_solver_step.Make (Log) (Enc) (Tactic) (W) (ProofState) (TheoryMaker)

  let make (gl : Proofview.Goal.t)
    : (module Proof_solver_step.S with type tactic = Tactic.t)
    =
    (module Step (struct
         let gl = ref gl
       end))
  ;;

  (** [get_updated_pstate x] returns the [pstate] updated by tactic [x]. *)
  let get_updated_pstate (x : unit Proofview.tactic) : Declare.Proof.t =
    Log.trace __FUNCTION__;
    let new_pstate, is_safe_tactic =
      Declare.Proof.by x (ProofState.get_pstate ())
    in
    if Bool.not is_safe_tactic
    then Log.warning ~__FUNCTION__ "unsafe tactic used";
    new_pstate
  ;;

  let exit_proof () : unit =
    ProofState.update_statem Done;
    raise NothingToDo
  ;;

  (** [step pstate] enters a fresh [module Step] for [pstate] and returns it after being updated by [Step.step ()] (followed by [simpl in *] and [subst]).
  *)
  let step (pstate : Declare.Proof.t) : Declare.Proof.t =
    Log.trace __FUNCTION__;
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

type t =
  { logger : (module Logger.S)
  ; solver : (module S)
  }

let the_cache : t ref option ref = ref None
let reset_the_cache () : unit = the_cache := None

exception NoCachedModules

let get_the_cache () : t ref =
  match !the_cache with None -> raise NoCachedModules | Some x -> x
;;

let get_the_logger () : (module Logger.S) ref = ref !(get_the_cache ()).logger
let get_the_proof_solver () : (module S) ref = ref !(get_the_cache ()).solver

let is_done () : bool =
  let module Ps = (val !(get_the_proof_solver ())) in
  Ps.ProofState.is_done ()
;;

let make
      (module Log : Logger.S)
      (module Enc : Encoding.S)
      ?(ctx : (module Rocq_context.S) = (module Rocq_context.Default))
      ()
  : t ref
  =
  Log.trace __FUNCTION__;
  let module Ctx : Rocq_context.S = (val ctx) in
  let module Solver : S with type enc = Enc.t = Make (Log) (Ctx) (Enc) in
  the_cache
  := Some
       (ref { logger = (module Log : Logger.S); solver = (module Solver : S) });
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

(** [solve] ... *)
let solve ?(bound : int = 10) (pstate : Declare.Proof.t) : Declare.Proof.t =
  let module Log : Logger.S = (val !(get_the_logger ())) in
  Log.trace __FUNCTION__;
  let rec f (n : int) (p : Declare.Proof.t) : int * Declare.Proof.t =
    Log.thing ~__FUNCTION__ Debug "iter" n (Printf.sprintf "%i");
    match Int.compare n bound with
    | 1 -> n, p
    | _ -> (try step p |> f (n + 1) with NothingToDo -> n, p)
  in
  let num, pstate = f 0 pstate in
  Log.notice (stop_msg num);
  pstate
;;

(** [init ] ... *)
let init
      ?(log : unit -> (module Logger.S) = Api.make_logger)
      ?(enc : (module Logger.S) -> (module Encoding.S) = Api.make_enc_int)
      ?(ctx : (module Rocq_context.S) = (module Rocq_context.Default))
      (pstate : Declare.Proof.t)
      (refs : Libnames.qualid list)
      (a : Constrexpr.constr_expr * Libnames.qualid)
      (b : Constrexpr.constr_expr * Libnames.qualid)
  : Declare.Proof.t
  =
  let module Log : Logger.S = (val log ()) in
  Log.trace __FUNCTION__;
  let module Enc : Encoding.S = (val enc (module Log)) in
  let c : t ref = make (module Log) (module Enc) ~ctx () in
  let module Solver : S = (val !c.solver) in
  Solver.W.check_bisimilarity refs a b;
  Solver.ProofState.init pstate (fst a, fst b);
  pstate
;;
