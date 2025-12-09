type t =
  { from : Model_state.t
  ; label : Model_label.t
  ; goto : Model_state.t option
  ; annotation : Model_note.annotation
  ; constructor_trees : Mebi_constr.Tree.t list option
  }

let create
      (from : Model_state.t)
      (label : Model_label.t)
      (goto : Model_state.t option)
      (annotation : Model_note.annotation)
      (constructor_trees : Mebi_constr.Tree.t list)
  : t
  =
  { from
  ; label
  ; goto
  ; annotation
  ; constructor_trees =
      (if List.is_empty constructor_trees then None else Some constructor_trees)
  }
;;

let equal (a : t) (b : t) : bool =
  Model_state.equal a.from b.from
  && Model_label.equal a.label b.label
  && Option.equal Model_state.equal a.goto b.goto
  && Option.equal
       (List.equal Mebi_constr.Tree.equal)
       a.constructor_trees
       b.constructor_trees
;;

let compare (a : t) (b : t) : int =
  Utils.compare_chain
    [ Model_state.compare a.from b.from
    ; Model_label.compare a.label b.label
    ; Option.compare Model_state.compare a.goto b.goto
    ; Option.compare
        (List.compare Mebi_constr.Tree.compare)
        a.constructor_trees
        b.constructor_trees
    ]
;;

let annotation_is_empty (x : t) : bool =
  Model_note.annotation_is_empty x.annotation
;;

open Utils.Strfy

let to_string ?(args : style_args = record_args ()) (x : t) : string =
  let from : string = Model_state.to_string ~args:(nest args) x.from in
  let label : string = Model_label.to_string ~args:(nest args) x.label in
  let goto : string =
    Option.cata
      (fun x -> Model_state.to_string ~args:(nest args) x)
      "None"
      x.goto
  in
  let constructor_trees : string =
    Option.cata
      (fun y -> Mebi_constr.Tree.list_to_string ~args:(nest args) y)
      "None"
      x.constructor_trees
  in
  let annotations : string =
    Model_note.annotation_to_string ~args:(nest args) x.annotation
  in
  record
    ~args
    [ "from", from
    ; "label", label
    ; "goto", goto
    ; "constructor_trees", constructor_trees
    ; "annotations", annotations
    ]
;;
