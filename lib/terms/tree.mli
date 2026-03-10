module Make : (Log : Logger.S)
    (Base : sig
       type t

       val json : ?as_elt:bool -> t -> Yojson.t
       val equal : t -> t -> bool
       val compare : t -> t -> int
     end)
    -> sig
    module Node : sig
      type t = Base.t * int

      val json : ?as_elt:bool -> t -> Yojson.t
      val to_string : ?pretty:bool -> t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
      val compare : t -> t -> int
      val equal : t -> t -> bool
    end

    type 'a tree = N of 'a * 'a tree list
    type t = Node.t tree

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    val add : t -> t -> t
    val add_list : t -> t list -> t list
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val minimize : t -> Node.t list

    exception CannotMinimizeEmptyList of unit

    val min : t list -> Node.t list
  end
  with type Node.t = Base.t * int
