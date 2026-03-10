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
    (WIP : sig
       type t =
         { from : State.t
         ; via : Label.t
         ; trees : Base.Trees.t
         }

       val json : ?as_elt:bool -> t -> Yojson.t
       val is_named : t -> bool
       val equal : t -> t -> bool
       val compare : t -> t -> int
     end)
    -> sig
  type t =
    { this : WIP.t
    ; next : next option
    }

  and next =
    | Next of t
    | Goto of State.t

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val create : WIP.t -> t
  val compare : t -> t -> int
  val compare_next : next -> next -> int

  exception Invalid

  val has_named : ?validate:bool -> t -> bool
  val validate : t -> unit

  exception CouldNotFindGoto

  val get_goto : t -> State.t

  exception CouldNotFindNamed

  val get_named : t -> Label.t
  val get_named_opt : t -> Label.t option

  exception FailAdd_AlreadyNamed
  exception FailAdd_AlreadyHasGoto

  val add : WIP.t -> t -> t

  exception FailSetGoto_AlreadyHasGoto

  val set_goto : State.t -> t -> t

  exception FailSeq_AlreadyNamed
  exception FailSeq_AlreadyHasGoto

  val seq : t -> t -> t
  val seq_opt : t option -> t -> t
  val get : WIP.t -> t -> t
  val upto_named : t -> t

  exception GotoNotSet

  val to_annotation : t -> Annotation.t
end
