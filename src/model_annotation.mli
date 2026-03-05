module Make : (Log : Logger.S)
    (Enc : Encoding.SEncoding)
    (State : Model_state.Make(Log)(Enc).S)
    (Label : Model_label.Make(Log)(Enc).S)
    (Tree_ : module type of Enc_tree.Make (Log) (Enc)) (Tree : Tree_.S)
    -> sig
  module Trees : module type of Tree_.Trees (Tree)

  module type NoteType = sig
    type t =
      { from : State.t
      ; label : Label.t
      ; using : Trees.t
      ; goto : State.t
      }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val is_silent : t -> bool
    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module Note : NoteType

  module type S = sig
    type t =
      { this : Note.t
      ; next : t option
      }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val is_empty : t -> bool
    val length : t -> int
    val shorter : t -> t -> t
    val exists : Note.t -> t -> bool
    val exists_label : Label.t -> t -> bool
    val append : Note.t -> t -> t
    val last : t -> Note.t

    exception CannotDropLastOfSingleton of t

    val drop_last : t -> t
    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module Annotation : S

  module Annotations : (Annotation : S) -> sig
    include module type of Set.Make (Annotation) with type elt = Annotation.t
    val extrapolate : elt -> t
    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end
end
