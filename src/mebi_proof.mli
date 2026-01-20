module Hyp = Mebi_hypothesis

val econstr_to_string : Proofview.Goal.t -> EConstr.t -> string
val hyp_to_string : Proofview.Goal.t -> Rocq_utils.hyp -> string
val hyps_to_string : Proofview.Goal.t -> string
val concl_to_string : Proofview.Goal.t -> string

type tactic =
  { msg : string option
  ; x : unit Proofview.tactic
  }

module ApplicableConstructors : sig
  type t =
    { current : Model.Tree.node list
    ; annotation : Model.Note.annotation option
    ; destination : Model.State.t
    }

  exception Mebi_proof_CannotGetConstructorInfo_None of unit

  exception
    Mebi_proof_CannotFindConstructorInfo_OfLTS of Model.Enc.t

  exception Mebi_proof_CannotFindConstructorInfo_OfIndex of int

end

module PState : sig
  type t =
    | NewProof
    | NewWeakSim
    | NewCofix
    | DoRefl
    | GoalTransition of transitions
    | ApplyConstructors of ApplicableConstructors.t
    | DetectState

  and transitions =
    { mtrans : Model_transition.t
    ; ntrans : Model_transition_opt.t
    }

  (* and applicable_constructors =
    { annotation : Model_note.annotation option
    ; tactics : tactic list option
    ; goto : Model_state.t
    } *)

  (* and tactic_to_apply = unit -> unit Proofview.tactic *)

  val empty_tactics : tactic list option -> bool
  val to_string : ?short:bool -> t -> string
end

val reset_the_proof_state : unit -> unit
val step : unit -> unit Proofview.tactic
val solve : int -> Declare.Proof.t -> Declare.Proof.t


