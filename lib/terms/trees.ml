module type S = sig
  type tree

  include Set.S with type elt = tree
  include Json.S with type k = t

  exception EmptyHasNoMin

  val min : t -> tree
  val min_opt : t -> tree option
end

module Make (Tree : Tree.S) : S with type tree = Tree.t = struct
  type tree = Tree.t

  module Set_ : Set.S with type elt = Tree.t = Set.Make (Tree)
  include Set_

  include Json.Set.Make (struct
      module Set = Set_

      let name = "Trees"
      let json = Tree.json
    end)

  exception EmptyHasNoMin

  (** [fewer_constructors acc x] is [x] if it has fewer constructors to
      apply than [acc] ({!Tree.size}), else [acc]: so of two the same size,
      the earlier. Raises nothing. *)
  let fewer_constructors (acc : elt) (x : elt) : elt =
    match Int.compare (Tree.size x) (Tree.size acc) with -1 -> x | _ -> acc
  ;;

  (* See the [.mli]. [to_list] is in the set's order. *)
  let min (xs : t) : elt =
    match to_list xs with
    | [] -> raise EmptyHasNoMin
    | h :: tl -> List.fold_left fewer_constructors h tl
  ;;

  let min_opt (xs : t) : elt option =
    try Some (min xs) with EmptyHasNoMin -> None
  ;;
end
