type 'a tree = Node of 'a * 'a tree list
type t = (int * int) tree

let eq (t1 : t) (t2 : t) : bool =
  let rec tree_eq (t1 : t) (t2 : t) : bool =
    match t1, t2 with
    | Node (a1, b1), Node (a2, b2) ->
      fst a1 == fst a2 && snd a1 == snd a2 && tree_list_eq b1 b2
  and tree_list_eq (l1 : t list) (l2 : t list) : bool =
    match l1, l2 with
    | [], [] -> true
    | h1 :: t1, h2 :: t2 -> tree_eq h1 h2 && tree_list_eq t1 t2
    | [], _ :: _ -> false
    | _ :: _, [] -> false
  in
  tree_eq t1 t2
;;

let compare (t1 : t) (t2 : t) : int =
  let rec tree_compare (t1 : t) (t2 : t) : int =
    match t1, t2 with
    | Node (i1, l1), Node (i2, l2) ->
      (match Int.compare (fst i1) (fst i2) with
       | 0 ->
         (match Int.compare (snd i1) (snd i2) with
          | 0 -> tree_list_compare l1 l2
          | n -> n)
       | n -> n)
  and tree_list_compare (l1 : t list) (l2 : t list) : int =
    match l1, l2 with
    | [], [] -> 0
    | h1 :: t1, h2 :: t2 ->
      (match tree_compare h1 h2 with
       | 0 -> tree_list_compare t1 t2 (* these should always be empty *)
       | n -> n (* prioritise the main node when comparing *))
    | [], _ :: _ -> -1
    | _ :: _, [] -> 1
  in
  tree_compare t1 t2
;;

let rec pstr (t1 : t) : string =
  match t1 with
  | Node (lhs_int, rhs_int_tree_list) ->
    Printf.sprintf
      "(%i:%i) [%s]"
      (fst lhs_int)
      (snd lhs_int)
      (match List.length rhs_int_tree_list with
       | 0 -> ""
       | 1 -> pstr (List.hd rhs_int_tree_list)
       | _ ->
         List.fold_left
           (fun (acc : string) (rhs_int_tree : t) ->
             Printf.sprintf "%s, %s" acc (pstr rhs_int_tree))
           (pstr (List.hd rhs_int_tree_list))
           (List.tl rhs_int_tree_list))
;;
