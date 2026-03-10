module Make : (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type t = Base.t)
    (Label : Label.S with type t = Base.t Label.t')
    (Note : sig
       type t =
         { from : State.t
         ; label : Label.t
         ; using : Base.Trees.t
         ; goto : State.t
         }
     end)
    (Annotation : sig
       type t =
         { this : Note.t
         ; next : t option
         }
     end)
    (Action : sig
       type t =
         { label : Label.t
         ; annotation : Annotation.t option
         ; constructor_trees : Base.Trees.t
         }
     end)
    -> sig
  type t =
    { from : State.t
    ; via : Label.t
    ; trees : Base.Trees.t
    }

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val is_silent : t -> bool
  val is_named : t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val create : State.t -> Action.t -> t

  exception IsEmptyList

  val list_to_annotation : State.t -> t list -> Annotation.t
end
