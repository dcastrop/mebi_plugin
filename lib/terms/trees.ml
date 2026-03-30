module type S = sig
  type tree

  include Set.S with type elt = tree
  include Json.S with type k = t

  exception EmptyHasNoMin

  val min : t -> tree
  val min_opt : t -> tree option
end

module Make (Log : Logger.S) (Tree : Tree.S) : S with type tree = Tree.t =
struct
  type tree = Tree.t

  module Set_ : Set.S with type elt = Tree.t = Set.Make (Tree)
  include Set_

  include
    Json.Set.Make
      (Log)
      (struct
        module Set = Set_

        let name = "Trees"
        let json = Tree.json
      end)

  exception EmptyHasNoMin

  (** obtain the tree with the shortest minimized length *)
  let min (xs : t) : elt =
    match to_list xs with
    | [] -> raise EmptyHasNoMin
    | h :: tl ->
      List.fold_left
        (fun (acc : elt) (x : elt) ->
          match
            Int.compare
              (Tree.minimize x |> List.length)
              (Tree.minimize acc |> List.length)
          with
          | -1 -> x
          | _ -> acc)
        h
        tl
  ;;

  let min_opt (xs : t) : elt option =
    try Some (min xs) with EmptyHasNoMin -> None
  ;;
end
