module Make : (Log : Logger.S)
    (Base : sig
       type t
include Json.S with type k = t
       val equal : t -> t -> bool
       val compare : t -> t -> int
     end)
    -> sig
    module Node : sig
      type t = Base.t * int
include Json.S with type k = t
      val compare : t -> t -> int
      val equal : t -> t -> bool
    end

    type 'a tree = N of 'a * 'a tree list
    type t = Node.t tree
include Json.S with type k = t
    val add : t -> t -> t
    val add_list : t -> t list -> t list
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val minimize : t -> Node.t list

    exception CannotMinimizeEmptyList of unit

    val min : t list -> Node.t list
  end
  with type Node.t = Base.t * int
