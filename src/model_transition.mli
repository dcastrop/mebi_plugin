module Make : (Log : Logger.S)
    (Enc : Encoding.SEncoding)
    (State : Model_state.Make(Log)(Enc).S)
    (Label_ : module type of Model_label.Make (Log) (Enc))
    (Labels : module type of Label_.Labels (Label_.Label))
    (Tree_ : module type of Enc_tree.Make (Log) (Enc))
    (Annotation : Model_annotation.Make (Log) (Enc) (State) (Label_.Label)
                    (Tree_)
                    (Tree_.Tree).S)
    -> sig
  module type S = sig
    type t =
      { from : State.t
      ; goto : State.t
      ; label : Label_.Label.t
      ; annotation : Annotation.t option
      ; constructor_tree : Tree_.Tree.t option
      }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val is_silent : t -> bool
    val annotation_is_empty : t -> bool
    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module Transition : S

  module Transitions : (Transition : S) -> sig
    include module type of Set.Make (Transition) with type elt = Transition.t

    val labels : t -> Labels.t

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end
end
