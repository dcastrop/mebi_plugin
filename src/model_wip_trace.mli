module Make : (Log : Logger.S)
    (State : sig
       type t

       val json : ?as_elt:bool -> t -> Yojson.t

       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val equal : t -> t -> bool *)
       val compare : t -> t -> int
       (* val hash : t -> int *)
     end)
    (Label : sig
       type t

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val equal : t -> t -> bool *)
       (* val compare : t -> t -> int *)
       (* val hash : t -> int *)
       (* val is_silent : t -> bool *)
     end)
    (Tree : sig
       module Node : sig
         type t

         (* val json : ?as_elt:bool -> t -> Yojson.t *)
         (* val to_string : ?pretty:bool -> t -> string *)
         (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
         (* val compare : t -> t -> int *)
         (* val equal : t -> t -> bool *)
       end

       type 'a tree = N of 'a * 'a tree list
       type t = Node.t tree

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val add : t -> t -> t *)
       (* val add_list : t -> t list -> t list *)
       (* val equal : t -> t -> bool *)
       (* val compare : t -> t -> int *)
       (* val minimize : t -> Node.t list *)

       exception CannotMinimizeEmptyList of unit

       (* val min : t list -> Node.t list *)
     end)
    (Trees : sig
       include Set.S with type elt = Tree.t

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       exception EmptyHasNoMin

       (* val min : t -> Tree.t *)
       (* val min_opt : t -> Tree.t option *)
     end)
    (Note : sig
       type t =
         { from : State.t
         ; label : Label.t
         ; using : Trees.t
         ; goto : State.t
         }
       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)

       (* val equal : t -> t -> bool *)
       (* val compare : t -> t -> int *)
       (* val is_silent : t -> bool *)
       (* val has_label : Label.t -> t -> bool *)
     end)
    (Annotation : sig
       type t =
         { this : Note.t
         ; next : t option
         }

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val equal : t -> t -> bool *)
       (* val compare : t -> t -> int *)
       (* val is_empty : t -> bool *)
       (* val opt_is_empty : ?fail_if_none:bool -> t option -> bool *)
       (* val length : t -> int *)
       (* val opt_length : ?fail_if_none:bool -> t option -> int *)
       (* val shorter : t -> t -> t *)
       (* val exists : Note.t -> t -> bool *)
       (* val exists_label : Label.t -> t -> bool *)
       (* val append : Note.t -> t -> t *)
       (* val last : t -> Note.t *)

       exception CannotDropLastOfSingleton of t

       (* val drop_last : t -> t *)
     end)
    (WIP : sig
       type t =
         { from : State.t
         ; via : Label.t
         ; trees : Trees.t
         }

       val json : ?as_elt:bool -> t -> Yojson.t

       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val is_silent : t -> bool *)
       val is_named : t -> bool
       val equal : t -> t -> bool
       val compare : t -> t -> int
       (* val create : State.t -> Action.t -> t *)

       exception IsEmptyList

       (* val list_to_annotation : State.t -> t list -> Annotation.t *)
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
