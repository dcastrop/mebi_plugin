module Make
    (Log : Logger.S)
    (Tree : sig
       module Node : sig
         type t
       end

       type 'a tree = N of 'a * 'a tree list
       type t = Node.t tree

       include Json.S with type k = t

       val compare : t -> t -> int
       val minimize : t -> Node.t list
     end) : sig
  include Set.S with type elt = Tree.t
  include Json.S with type k = t

  exception EmptyHasNoMin

  val min : t -> Tree.t
  val min_opt : t -> Tree.t option
end = struct
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
