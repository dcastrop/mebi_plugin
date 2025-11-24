module State : sig
  type t =
    | NewProof
    | NewWeakSim
    | NewCofix
    | NewTransition of Model.Transition_opt.t
    | GoalTransition of Model.Transition_opt.t
    | ApplyConstructors of applicable_constructors

  and applicable_constructors = {
    annotation : Model.Note.annotation;
    tactics : tactic_to_apply list option;
  }

  and tactic_to_apply = unit -> unit Proofview.tactic

  val to_string : t -> string
end

val default_proof_state : State.t
val the_proof_state : State.t ref
val reset_the_proof_state : unit -> unit
val handle_proof_state : unit -> unit Proofview.tactic
val solve : int -> Declare.Proof.t -> Declare.Proof.t
