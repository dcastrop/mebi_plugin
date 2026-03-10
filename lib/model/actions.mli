module Make : (Log : Logger.S)
    (Base : Base_term.S)
    (Label : Label.S with type t = Base.t Label.t')
    (Labels : sig
       include Set.S with type elt = Label.t
     end)
    (Annotation : sig
       type t

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val opt_length : ?fail_if_none:bool -> t option -> int
       val json : ?as_elt:bool -> t -> Yojson.t
     end)
    (Action : sig
       type t =
         { label : Label.t
         ; annotation : Annotation.t option
         ; constructor_trees : Base.Trees.t
         }

       val json : ?as_elt:bool -> t -> Yojson.t
       val compare : t -> t -> int
     end)
    -> sig
  include Set.S with type elt = Action.t

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val labelled : t -> Label.t -> t
  val labels : t -> Labels.t
end
