type t =
  { label : Model_label.t
  ; annotations : Model_note.annotations
  ; constructor_trees : Mebi_constr.Tree.t list
  }

let hash (x : t) : int = Model_label.hash x.label

(** [eq] is equal but allows for finer control over what is compared with *)
let check_equal
      ?(annotations : bool = true)
      ?(constructor_trees : bool = true)
      (a : t)
      (b : t)
  : bool
  =
  let x : bool =
    if annotations
    then Model_note.annotations_equal a.annotations b.annotations
    else true
  in
  let y : bool =
    if constructor_trees
    then
      List.equal Mebi_constr.Tree.equal a.constructor_trees b.constructor_trees
    else true
  in
  Model_label.equal a.label b.label && x && y
;;

let equal (a : t) (b : t) : bool =
  check_equal ~annotations:true ~constructor_trees:true a b
;;

(** [compare] *)
let check_compare
      ?(annotations : bool = true)
      ?(constructor_trees : bool = true)
      (a : t)
      (b : t)
  : int
  =
  let x = Model_label.compare a.label b.label in
  let y =
    if annotations
    then Some (Model_note.annotations_compare a.annotations b.annotations)
    else None
  in
  let z =
    if constructor_trees
    then
      Some
        (List.compare
           Mebi_constr.Tree.compare
           a.constructor_trees
           b.constructor_trees)
    else None
  in
  match x, y, z with
  | x, None, None -> x
  | x, Some y, None -> Int.compare x y
  | x, None, Some z -> Int.compare x z
  | x, Some y, Some z -> Int.compare x (Int.compare y z)
;;

let compare (a : t) (b : t) : int =
  check_compare ~annotations:true ~constructor_trees:true a b
;;

open Utils.Strfy

(** [to_string] *)
let to_string ?(args : style_args = record_args ()) (x : t) : string =
  let label : string = Model_label.to_string ~args x.label in
  let annotations : string =
    Model_note.annotations_to_string ~args x.annotations
  in
  let constructor_trees : string =
    Mebi_constr.Tree.list_to_string x.constructor_trees
  in
  record
    ~args
    [ "label", label
    ; "annotations", annotations
    ; "constructor_trees", constructor_trees
    ]
;;
