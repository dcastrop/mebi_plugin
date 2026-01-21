open Mebi_setup

module Tree = struct
  module Node = struct
    type t = Mebi_setup.Enc.t * int

    let to_string
          ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
          ((enc, index) : t)
      : string
      =
      Utils.Strfy.record
        ~args
        [ "enc", Mebi_setup.Enc.to_string enc; "index", Utils.Strfy.int index ]
    ;;
  end

  type 'a tree = Node of 'a * 'a tree list
  type t = Node.t tree

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
  let rec minimize : t -> Node.t list = function
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

  exception Mebi_constr_Tree_EmptyList of unit

  let min : t list -> Node.t list = function
    | [] -> raise (Mebi_constr_Tree_EmptyList ())
    | h :: tl ->
      List.map minimize tl
      |> List.fold_left
           (fun the_min y ->
             match Int.compare (List.length y) (List.length the_min) with
             | -1 -> y
             | _ -> the_min)
           (minimize h)
  ;;

  let rec to_string
            ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
            (Node (node, nodes) : t)
    : string
    =
    Utils.Strfy.tuple
      ~args
      (Args Node.to_string)
      (Args (Utils.Strfy.list (Args to_string)))
      (node, nodes)
  ;;

  let list_to_string
        ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
    : t list -> string
    =
    Utils.Strfy.list
      ~args:{ args with name = Some "Constructor Trees" }
      (Args to_string)
  ;;
end

(** A triple denoting a constructor of an rocq LTS definition.
    - [EConstr.t] action
    - [EConstr.t] destination
    - [Mebi_constr.Tree.t] coq-constructor index *)
type t = EConstr.t * EConstr.t * Tree.t

let to_string
      env
      sigma
      ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
      ((action, destination, tree) : t)
  : string
  =
  let open Rocq_utils.Strfy in
  let action : string = econstr env sigma action in
  let destination : string = econstr env sigma destination in
  let tree : string = Tree.to_string tree in
  Utils.Strfy.record
    [ "action", action; "destination", destination; "tree", tree ]
;;
