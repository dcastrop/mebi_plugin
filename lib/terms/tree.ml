module type S = sig
  type base

  module Node : sig
    type t = base * int

    include Json.S with type k = t

    val compare : t -> t -> int
    val equal : t -> t -> bool
  end

  type 'a tree = N of 'a * 'a tree list
  type t = Node.t tree

  include Json.S with type k = t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val preorder : t -> Node.t list
  val size : t -> int
end

module Make (Base : Base_.S) : S with type base = Base.t = struct
  type base = Base.t

  module Node = struct
    type t = Base.t * int

    include Json.Thing.Make (struct
        type k = t

        let name = "Node"

        let json ?as_elt (x : t) : Yojson.t =
          `Assoc
            [ "enc", Base.json ~as_elt:true (fst x); "index", `Int (snd x) ]
        ;;
      end)

    let compare (a : t) (b : t) : int =
      Utils.compare_chain
        [ Base.compare (fst a) (fst b); Int.compare (snd a) (snd b) ]
    ;;

    let equal (a : t) (b : t) : bool =
      Base.equal (fst a) (fst b) && Int.equal (snd a) (snd b)
    ;;
  end

  type 'a tree = N of 'a * 'a tree list
  type t = Node.t tree

  include Json.Thing.Make (struct
      type k = t

      let name = "Tree"

      let rec json ?as_elt (N (x, xl) : t) : Yojson.t =
        `Assoc
          [ "node", Node.json ~as_elt:true x
          ; "cons", `List (List.map (json ~as_elt:true) xl)
          ]
      ;;
    end)

  let rec equal (a : t) (b : t) : bool =
    match a, b with
    | N (a, al), N (b, bl) -> Node.equal a b && List.equal equal al bl
  ;;

  let compare (a : t) (b : t) : int =
    match a, b with
    | N (a, al), N (b, bl) ->
      Utils.compare_chain [ Node.compare a b; List.compare compare al bl ]
  ;;

  (* See the [.mli] for these. *)
  let rec preorder : t -> Node.t list = function
    | N (x, cs) -> x :: List.concat_map preorder cs
  ;;

  let rec size : t -> int = function
    | N (_, cs) -> List.fold_left (fun n c -> n + size c) 1 cs
  ;;
end
