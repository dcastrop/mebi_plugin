module type S = sig
  type label
  type annotation
  type trees

  type t =
    { label : label
    ; annotation : annotation option
    ; trees : trees
    }

  include Json.S with type k = t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val wk_equal : t -> t -> bool
  val is_silent : t -> bool
  val is_labelled : label -> t -> bool
  val shorter_annotation : t -> t -> t
end

module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (Label : Label.S with type base = Base.t)
    (Annotation : sig
       type t

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val opt_length : ?fail_if_none:bool -> t option -> int
       val json : ?as_elt:bool -> t -> Yojson.t
     end) :
  S
  with type label = Label.t
   and type annotation = Annotation.t
   and type trees = Base.Trees.t
