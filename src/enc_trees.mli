module Make : (Log : Logger.S)
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
    -> sig
  include Set.S with type elt = Tree.t

  exception EmptyHasNoMin

  val min : t -> Tree.t
  val min_opt : t -> Tree.t option
  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
end
