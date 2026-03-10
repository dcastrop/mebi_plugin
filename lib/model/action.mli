module Make : (Log : Logger.S)
    (Base : Base_term.S)
    (Label : Label.S with type t = Base.t Label.t')
    (Annotation : sig
       type t

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val opt_length : ?fail_if_none:bool -> t option -> int
       val json : ?as_elt:bool -> t -> Yojson.t
     end)
    -> sig
  type t =
    { label : Label.t
    ; annotation : Annotation.t option
    ; constructor_trees : Base.Trees.t
    }

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val wk_equal : t -> t -> bool
  val is_silent : t -> bool
  val is_labelled : Label.t -> t -> bool
  val shorter_annotation : t -> t -> t
end
