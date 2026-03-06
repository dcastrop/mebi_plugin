module Make : (Log : Logger.S)
    (State : sig
       type t

       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
       val equal : t -> t -> bool
       val compare : t -> t -> int
       val hash : t -> int
     end)
    (Label : sig
       type t

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val hash : t -> int
       val is_silent : t -> bool
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (Labels : sig
       include Set.S with type elt = Label.t

       val non_silent : t -> t
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (Tree : sig
       module Node : sig
         type t

         val compare : t -> t -> int
         val equal : t -> t -> bool
         val json : ?as_elt:bool -> t -> Yojson.t
         val to_string : ?pretty:bool -> t -> string
         val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
       end

       type 'a tree = N of 'a * 'a tree list
       type t = Node.t tree

       val add : t -> t -> t
       val add_list : t -> t list -> t list
       val equal : t -> t -> bool
       val compare : t -> t -> int
       val minimize : t -> Node.t list

       exception CannotMinimizeEmptyList of unit

       val min : t list -> Node.t list
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (Note : sig
       type t

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val is_silent : t -> bool
       val has_label : Label.t -> t -> bool
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (Annotation : sig
       type t =
         { this : Note.t
         ; next : t option
         }

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val is_empty : t -> bool
       val opt_is_empty : ?fail_if_none:bool -> t option -> bool
       val length : t -> int
       val opt_length : ?fail_if_none:bool -> t option -> int
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
     end)
    (Transition : sig
       type t =
         { from : State.t
         ; goto : State.t
         ; label : Label.t
         ; tree : Tree.t option
         ; annotation : Annotation.t option
         }

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val is_silent : t -> bool
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    -> sig
  include Set.S with type elt = Transition.t

  val labels : t -> Labels.t
  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
end
