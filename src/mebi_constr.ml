open Mebi_setup

module Tree = struct
  type 'a tree = Node of 'a * 'a tree list
  type node = Mebi_setup.Enc.t * int
  type t = node tree

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
  let rec minimize : t -> node list = function
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

  let to_string ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
    : t -> string
    =
    let rec to_string (x : t) : string =
      match x with
      | Node (lhs_int, rhs_int_tree_list) ->
        Printf.sprintf
          "(%s:%i) [%s]"
          (Enc.to_string (fst lhs_int))
          (snd lhs_int)
          (match List.length rhs_int_tree_list with
           | 0 -> ""
           | 1 -> to_string (List.hd rhs_int_tree_list)
           | _ ->
             List.fold_left
               (fun (acc : string) (rhs_int_tree : t) ->
                 Printf.sprintf "%s, %s" acc (to_string rhs_int_tree))
               (to_string (List.hd rhs_int_tree_list))
               (List.tl rhs_int_tree_list))
    in
    to_string
  ;;

  let list_to_string
        ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
        (x : t list)
    : string
    =
    let open Utils.Strfy in
    list ~args:{ args with name = Some "Constructor Trees" } to_string x
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
  let open Utils.Strfy in
  let open Rocq_utils.Strfy in
  let action : string = econstr env sigma ~args:(nest args) action in
  let destination : string = econstr env sigma destination in
  let tree : string = Tree.to_string tree in
  Utils.Strfy.record
    [ "action", action; "destination", destination; "tree", tree ]
;;
