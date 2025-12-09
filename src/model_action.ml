type t =
  { label : Model_label.t
  ; annotation : Model_note.annotation
  ; constructor_trees : Mebi_constr.Tree.t list
  }

(* let get_annotation (from : Model_state.t) (x : t) : Model_note.annotation =
  { this = Model_note.create from x.label; next = x.annotation }
;; *)

let hash (x : t) : int = Model_label.hash x.label

(** [eq] is equal but allows for finer control over what is compared with *)
let check_equal
      ?(annotation : bool = true)
      ?(constructor_trees : bool = true)
      (a : t)
      (b : t)
  : bool
  =
  let x : bool =
    if annotation
    then Model_note.annotation_equal a.annotation b.annotation
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
  check_equal ~annotation:true ~constructor_trees:true a b
;;

let wk_equal (a : t) (b : t) : bool =
  check_equal ~annotation:false ~constructor_trees:false a b
;;

(** [compare] *)
let check_compare
      ?(annotation : bool = true)
      ?(constructor_trees : bool = true)
      (a : t)
      (b : t)
  : int
  =
  let f = Model_label.compare in
  let g = Model_note.annotation_compare in
  let h = List.compare Mebi_constr.Tree.compare in
  Utils.compare_chain
    (List.flatten
       [ [ f a.label b.label ]
       ; (if annotation then [ g a.annotation b.annotation ] else [])
       ; (if constructor_trees
          then [ h a.constructor_trees b.constructor_trees ]
          else [])
       ])
;;

let compare (a : t) (b : t) : int =
  check_compare ~annotation:true ~constructor_trees:true a b
;;

open Utils.Strfy

(** [to_string] *)
let to_string ?(args : style_args = record_args ()) (x : t) : string =
  let label : string = Model_label.to_string ~args x.label in
  let annotation : string =
    Model_note.annotation_to_string ~args x.annotation
  in
  let constructor_trees : string =
    Mebi_constr.Tree.list_to_string x.constructor_trees
  in
  record
    ~args
    [ "label", label
    ; "annotations", annotation
    ; "constructor_trees", constructor_trees
    ]
;;
