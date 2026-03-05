module Make (Log : Logger.S) (Enc : Encoding.SEncoding) = struct
  module type NodeType = sig
    type t = Enc.t * int

    val compare : t -> t -> int
    val equal : t -> t -> bool
    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module Node : NodeType with type t = Enc.t * int = struct
    type t = Enc.t * int

    let compare (a : t) (b : t) : int =
      Utils.compare_chain
        [ Enc.compare (fst a) (fst b); Int.compare (snd a) (snd b) ]
    ;;

    let equal (a : t) (b : t) : bool = fst a == fst b && snd b == snd b

    include
      Json.Thing.Make
        (Log)
        (struct
          type k = t

          let name = "TreeNode"

          let json ?as_elt (x : t) : Yojson.t =
            `Assoc
              [ "enc", `String (Enc.to_string (fst x)); "index", `Int (snd x) ]
          ;;
        end)
  end

  module type S = sig
    type 'a tree = N of 'a * 'a tree list
    type t = Node.t tree

    val add : t -> t -> t
    val add_list : t -> t list -> t list
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val minimize : t -> Node.t list

    exception CannotMinimizeEmptyList of unit

    val min : t list -> Node.t list

    type k = t

    val json : ?as_elt:bool -> k -> Yojson.t
    val to_string : ?pretty:bool -> k -> string
    val log : ?__FUNCTION__:string -> ?s:string -> k -> unit
  end

  module Tree : S = struct
    type 'a tree = N of 'a * 'a tree list
    type t = Node.t tree

    (** [add x y] inserts [x] to be a new leaf of [y], mutually recursive with [add_list x ys] (where [ys] is a list of [t]).
    *)
    let rec add (x : t) : t -> t = function
      | N ((enc, index), tl) -> N ((enc, index), add_list x tl)

    and add_list (x : t) : t list -> t list = function
      | [] -> [ x ]
      | h :: tl -> add x h :: add_list x tl
    ;;

    let rec equal (a : t) (b : t) : bool =
      match a, b with
      | N (a, al), N (b, bl) -> Node.equal a b && List.equal equal al bl
    ;;

    let compare (a : t) (b : t) : int =
      match a, b with
      | N (a, al), N (b, bl) ->
        Utils.compare_chain [ Node.compare a b; List.compare compare al bl ]
    ;;

    (** converts a given tree into a flattened list with the minimal number of constructors to apply
    *)
    let rec minimize : t -> Node.t list = function
      | N ((enc, index), []) -> [ enc, index ]
      | N ((enc, index), h :: tl) ->
        (enc, index)
        :: (List.fold_left (fun acc x -> minimize x :: acc) [] tl
            (* NOTE: we only take the shortest one *)
            |> List.fold_left
                 (fun (the_min : Node.t list) x ->
                   match Int.compare (List.length x) (List.length the_min) with
                   | -1 -> x
                   | _ -> the_min)
                 (minimize h))
    ;;

    exception CannotMinimizeEmptyList of unit

    let min : t list -> Node.t list = function
      | [] -> raise (CannotMinimizeEmptyList ())
      | h :: tl ->
        List.map minimize tl
        |> List.fold_left
             (fun the_min x ->
               match Int.compare (List.length x) (List.length the_min) with
               | -1 -> x
               | _ -> the_min)
             (minimize h)
    ;;

    (* *)
    include
      Json.Thing.Make
        (Log)
        (struct
          type k = t

          let name = "Tree"

          let rec json ?as_elt (N (x, xl) : t) : Yojson.t =
            `Assoc
              [ "node", Node.json ~as_elt:true x
              ; "cons", `List (List.map (json ~as_elt:true) xl)
              ]
          ;;
        end)
  end

  module Trees (Tree : S) = struct
    module Set : Set.S with type elt = Tree.t = Set.Make (Tree)
    include Set

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

    (* *)
    include
      Json.Set.Make
        (Log)
        (struct
          module Set = Set

          let name = "Trees"
          let json = Tree.json
        end)
  end
end
