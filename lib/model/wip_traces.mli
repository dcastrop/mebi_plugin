module Make : (Log : Logger.S)
    (State : sig
       type t

       (* val json : ?as_elt:bool -> t -> Yojson.t
          val to_string : ?pretty:bool -> t -> string
          val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
          val equal : t -> t -> bool
          val compare : t -> t -> int
          val hash : t -> int *)
     end)
    (WIP : sig
       type t
       (* =
         { from : State.t
         ; via : Label.t
         ; trees : Trees.t
         } *)

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       val equal : t -> t -> bool
       (* val compare : t -> t -> int *)
     end)
    (Trace : sig
       type t =
         { this : WIP.t
         ; next : next option
         }

       and next =
         | Next of t
         | Goto of State.t

       val json : ?as_elt:bool -> t -> Yojson.t

       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val create : WIP.t -> t *)
       val compare : t -> t -> int
       (* val compare_next : next -> next -> int *)

       exception Invalid

       (* val has_named : ?validate:bool -> t -> bool *)
       (* val validate : t -> unit *)

       exception CouldNotFindGoto

       (* val get_goto : t -> State.t *)

       exception CouldNotFindNamed

       (* val get_named : t -> Label.t *)
       (* val get_named_opt : t -> Label.t option *)

       exception FailAdd_AlreadyNamed
       exception FailAdd_AlreadyHasGoto

       (* val add : WIP.t -> t -> t *)

       exception FailSetGoto_AlreadyHasGoto

       (* val set_goto : State.t -> t -> t *)

       exception FailSeq_AlreadyNamed
       exception FailSeq_AlreadyHasGoto

       (* val seq : t -> t -> t *)
       (* val seq_opt : t option -> t -> t *)
       val get : WIP.t -> t -> t
       (* val upto_named : t -> t *)

       exception GotoNotSet

       (* val to_annotation : t -> Annotation.t *)
     end)
    -> sig
  include Set.S with type elt = Trace.t

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val get : WIP.t -> t -> t
end
