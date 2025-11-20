type t =
  { label : Model_label.t
  ; annotations_list : annotations list
  ; constructor_trees : Mebi_constr.Tree.t list
  }

and annotations = annotation list

and annotation =
  { from : Model_state.t
  ; via : Model_label.t
  }

let hash (x : t) : int = Model_label.hash x.label

(** [eq] is equal but allows for finer control over what is compared with *)
let rec check_equal
          ?(annotations : bool = true)
          ?(constructor_trees : bool = true)
          (a : t)
          (b : t)
  : bool
  =
  Model_label.equal a.label b.label
  && (if annotations
      then eq_annotations_list a.annotations_list b.annotations_list
      else true)
  &&
  if constructor_trees
  then eq_constructor_tree_list a.constructor_trees b.constructor_trees
  else true

and eq_annotations_list (a : annotations list) (b : annotations list) : bool =
  List.equal eq_annotations a b

and eq_annotations (a : annotations) (b : annotations) : bool =
  List.equal eq_annotation a b

and eq_annotation (a : annotation) (b : annotation) : bool =
  Mebi_setup.Enc.equal a.from.enc b.from.enc
  && Mebi_setup.Enc.equal a.via.enc b.via.enc

and eq_constructor_tree_list
      (a : Mebi_constr.Tree.t list)
      (b : Mebi_constr.Tree.t list)
  : bool
  =
  List.equal Mebi_constr.Tree.equal a b
;;

let equal (a : t) (b : t) : bool =
  check_equal ~annotations:true ~constructor_trees:true a b
;;

(** [compare] *)
let rec check_compare
          ?(annotations : bool = true)
          ?(constructor_trees : bool = true)
          (a : t)
          (b : t)
  : int
  =
  let x = Model_label.compare a.label b.label in
  let y =
    if annotations
    then Some (annotations_list_compare a.annotations_list b.annotations_list)
    else None
  in
  let z =
    if constructor_trees
    then
      Some
        (constructor_tree_list_compare a.constructor_trees b.constructor_trees)
    else None
  in
  match x, y, z with
  | x, None, None -> x
  | x, Some y, None -> Int.compare x y
  | x, None, Some z -> Int.compare x z
  | x, Some y, Some z -> Int.compare x (Int.compare y z)

and annotations_list_compare (a : annotations list) (b : annotations list) : int
  =
  List.compare annotations_compare a b

and annotations_compare (a : annotations) (b : annotations) : int =
  List.compare annotation_compare a b

and annotation_compare (a : annotation) (b : annotation) : int =
  Int.compare
    (Model_state.compare a.from b.from)
    (Model_label.compare a.via b.via)

and constructor_tree_list_compare
      (a : Mebi_constr.Tree.t list)
      (b : Mebi_constr.Tree.t list)
  : int
  =
  List.compare Mebi_constr.Tree.compare a b
;;

let compare (a : t) (b : t) : int =
  check_compare ~annotations:true ~constructor_trees:true a b
;;

(** [to_string] *)
let rec to_string
          ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
          (x : t)
  : string
  =
  let label : string = Model_label.to_string ~args x.label in
  let open Utils.Strfy in
  let annotations : string =
    annotations_list_to_string ~args x.annotations_list
  in
  let constructor_trees : string =
    constructor_tree_list_to_string ~args x.constructor_trees
  in
  record
    ~args
    [ "label", label
    ; "annotations", annotations
    ; "constructor_trees", constructor_trees
    ]

and annotations_list_to_string
      ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
      (x : annotations list)
  : string
  =
  let open Utils.Strfy in
  list
    ~args:{ args with name = Some "Annotations List" }
    annotations_to_string
    x

and annotations_to_string
      ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
      (x : annotations)
  : string
  =
  let open Utils.Strfy in
  list ~args:{ args with name = Some "Annotations" } annotation_to_string x

and annotation_to_string
      ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
      (x : annotation)
  : string
  =
  let goto : string = Mebi_setup.Enc.to_string x.from.enc in
  let via : string = Mebi_setup.Enc.to_string x.via.enc in
  Printf.sprintf "<State (%s) Via (%s)>" goto via

and constructor_tree_list_to_string
      ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
      (x : Mebi_constr.Tree.t list)
  : string
  =
  let open Utils.Strfy in
  list
    ~args:{ args with name = Some "Constructor Trees" }
    Mebi_constr.Tree.to_string
    x
;;
