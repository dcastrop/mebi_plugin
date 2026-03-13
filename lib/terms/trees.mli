module Make : (Log : Logger.S)
    (Tree : sig
       module Node : sig
         type t
       end

       type 'a tree = N of 'a * 'a tree list
       type t = Node.t tree

       include Json.S with type k = t

       val compare : t -> t -> int
       val minimize : t -> Node.t list
     end)
    -> sig
  include Set.S with type elt = Tree.t
  include Json.S with type k = t

  exception EmptyHasNoMin

  val min : t -> Tree.t
  val min_opt : t -> Tree.t option
end
