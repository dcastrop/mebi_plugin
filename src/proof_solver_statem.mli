module type S = sig
  type enc
  type node
  type state
  type label
  type annotation
  type transition

  module ApplicableConstructors : sig
    module Nodes : sig
      type t = node list

      include Json.S with type k = t
    end

    type t =
      { label : label
      ; destination : state
      ; current : Nodes.t option
      ; remaining : annotation option
      }

    include Json.S with type k = t

    exception TransitionHasNoConstructorsToApply

    val init : transition -> t
  end

  module StateM : sig
    type t =
      | Done
      | NewProof of (Constrexpr.constr_expr * Constrexpr.constr_expr)
      | WeakSim
      | Exists of transition option
      | ApplyConstructors of ApplicableConstructors.t

    include Json.S with type k = t
  end

  type t =
    { p : Declare.Proof.t
    ; x : StateM.t
    }

  val the_state : t ref option ref

  exception NoStateFound

  val get : unit -> t ref
  val set : Declare.Proof.t -> StateM.t -> unit

  val init
    :  Declare.Proof.t
    -> Constrexpr.constr_expr * Constrexpr.constr_expr
    -> unit

  val get_pstate : unit -> Declare.Proof.t
  val get_statem : unit -> StateM.t
  val update_pstate : Declare.Proof.t -> unit
  val update_statem : StateM.t -> unit
  val is_done : unit -> bool
  val log : ?__FUNCTION__:string -> unit -> unit
end

module Make
    (Log : Logger.S)
    (Enc : Encoding.S)
    (W :
       Results.S
       with type enc = Enc.t
        and type node = Enc.Tree.Node.t
        and type tree = Enc.Tree.t
        and type trees = Enc.Trees.t) :
  S
  with type enc = Enc.t
   and type node = Enc.Tree.Node.t
   and type state = W.Model.State.t
   and type label = W.Model.Label.t
   and type annotation = W.Model.Annotation.t
   and type transition = W.Model.Transition.t
