module Make
    (Log : Logger.S)
    (Base : sig
       type t

       val json : ?as_elt:bool -> t -> Yojson.t
       val equal : t -> t -> bool
       val compare : t -> t -> int
     end) : sig
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
  with type Node.t = Base.t * int = struct
  module Node = struct
    type t = Base.t * int

    include
      Json.Thing.Make
        (Log)
        (struct
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

  (** [add x y] inserts [x] to be a new leaf of [y], mutually recursive with [add_list x ys] (where [ys] is a list of [t]).
  *)
  let rec add (x : t) : t -> t = function N (h, tl) -> N (h, add_list x tl)

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
    | N (x, []) -> [ x ]
    | N (x, h :: tl) ->
      x
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
end
