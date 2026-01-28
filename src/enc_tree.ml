(* module type S = sig
   module Enc : Encoding.SEncoding

   module type STreeNode = sig
   type t = Enc.t * int

   val to_string : t -> string
   end

   module TreeNode : STreeNode with type t = Enc.t * int

   type 'a tree = Node of 'a * 'a tree list
   type t = TreeNode.t tree

   val add : t -> t -> t
   val add_list : t -> t list -> t list
   val equal : t -> t -> bool
   val compare : t -> t -> int
   val minimize : t -> TreeNode.t list

   exception CannotMinimizeEmptyList of unit

   val min : t list -> TreeNode.t list
   val to_string : t -> string
   val list_to_string : ?args:Utils.Strfy.style_args -> t list -> string
   end *)

module Make
    (Enc : Encoding.SEncoding)
     (* : *)
     (* S with module Enc = E and type Enc.t = E.t and type TreeNode.t = E.t * int *) =
struct
  (* module Enc : Encoding.SEncoding with type t = Enc.t = Enc *)

  module type STreeNode = sig
    type t = Enc.t * int

    val to_string : t -> string
  end

  module TreeNode : STreeNode with type t = Enc.t * int = struct
    type t = Enc.t * int

    let to_string ((enc, index) : t) : string =
      Utils.Strfy.record
        [ "enc", Enc.to_string enc; "index", Utils.Strfy.int index ]
    ;;
  end

  (* module TreeNode : STreeNode with type t = Enc.t * int = MakeTreeNode (Enc) *)

  (* module type STreeNode = sig
     type t = Enc.t * int

     val to_string : t -> string
     end

     module TreeNode : STreeNode with type t = Enc.t * int = struct
     type t = Enc.t * int

     let to_string ((enc, index) : t) : string =
     Utils.Strfy.record
     [ "enc", Enc.to_string enc; "index", Utils.Strfy.int index ]
     ;;
     end *)

  (* module type STree = sig
     type 'a tree = Node of 'a * 'a tree list
     type t = TreeNode.t tree

     val add : t -> t -> t
     val add_list : t -> t list -> t list
     val equal : t -> t -> bool
     val compare : t -> t -> int
     val minimize : t -> TreeNode.t list

     exception CannotMinimizeEmptyList of unit

     val min : t list -> TreeNode.t list
     val to_string : t -> string
     val list_to_string : ?args:Utils.Strfy.style_args -> t list -> string
     end

     module Tree : STree = struct *)
  type 'a tree = Node of 'a * 'a tree list
  type t = TreeNode.t tree

  (** [add x y] inserts [x] to be a new leaf of [y], mutually recursive with [add_list x ys] (where [ys] is a list of [t]).
  *)
  let rec add (x : t) : t -> t = function
    | Node ((enc, index), tl) -> Node ((enc, index), add_list x tl)

  and add_list (x : t) : t list -> t list = function
    | [] -> [ x ]
    | h :: tl -> add x h :: add_list x tl
  ;;

  let rec equal (t1 : t) (t2 : t) : bool =
    match t1, t2 with
    | Node (a1, b1), Node (a2, b2) ->
      fst a1 == fst a2 && snd a1 == snd a2 && List.equal equal b1 b2
  ;;

  let compare (t1 : t) (t2 : t) : int =
    match t1, t2 with
    | Node (i1, l1), Node (i2, l2) ->
      Utils.compare_chain
        [ Enc.compare (fst i1) (fst i2)
        ; Int.compare (snd i1) (snd i2)
        ; List.compare compare l1 l2
        ]
  ;;

  (** converts a given tree into a flattened list with the minimal number of constructors to apply
  *)
  let rec minimize : t -> TreeNode.t list = function
    | Node ((enc, index), []) -> [ enc, index ]
    | Node ((enc, index), h :: tl) ->
      (enc, index)
      :: (List.fold_left (fun acc x -> minimize x :: acc) [] tl
          |> List.fold_left
               (fun the_min x ->
                 match Int.compare (List.length x) (List.length the_min) with
                 | -1 -> x
                 | _ -> the_min)
               (minimize h))
  ;;

  exception CannotMinimizeEmptyList of unit

  let min : t list -> TreeNode.t list = function
    | [] -> raise (CannotMinimizeEmptyList ())
    | h :: tl ->
      List.map minimize tl
      |> List.fold_left
           (fun the_min y ->
             match Int.compare (List.length y) (List.length the_min) with
             | -1 -> y
             | _ -> the_min)
           (minimize h)
  ;;

  let rec to_string (Node (node, nodes) : t) : string =
    Utils.Strfy.tuple
      (Of TreeNode.to_string)
      (Args (Utils.Strfy.list (Of to_string)))
      (node, nodes)
  ;;

  let list_to_string
        ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
    : t list -> string
    =
    Utils.Strfy.list
      ~args:{ args with name = Some "Constructor Trees" }
      (Of to_string)
  ;;
  (* end *)
end
