module Make : (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type t = Base.t)
    (States : States.S with type elt = State.t)
    (Label : Label.S with type t = Base.t Label.t')
    (Annotation : sig
       type t

       val opt_length : ?fail_if_none:bool -> t option -> int
       val shorter : t -> t -> t
     end)
    (Action : sig
       type t =
         { label : Label.t
         ; annotation : Annotation.t option
         ; constructor_trees : Base.Trees.t
         }

       val json : ?as_elt:bool -> t -> Yojson.t
       val compare : t -> t -> int
       val wk_equal : t -> t -> bool
     end)
    -> sig
  type t = Action.t * States.t

  val compare : t -> t -> int
  val shorter_annotation : t -> t -> t
  val try_update : t -> t list -> t option * t list
  val merge_lists : t list -> t list -> t list
  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
end
