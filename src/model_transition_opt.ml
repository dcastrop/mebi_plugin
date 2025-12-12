type t =
  { from : Model_state.t
  ; label : Model_label.t
  ; goto : Model_state.t option
  ; annotation : Model_note.annotation option
  ; constructor_trees : Mebi_constr.Tree.t list
  }

let create
      (from : Model_state.t)
      (label : Model_label.t)
      (goto : Model_state.t option)
      ?(annotation : Model_note.annotation option = None)
      ?(constructor_trees : Mebi_constr.Tree.t list option)
      ()
  : t
  =
  let constructor_trees : Mebi_constr.Tree.t list =
    Option.cata (fun x -> x) [] constructor_trees
  in
  { from; label; goto; annotation; constructor_trees }
;;

let equal (a : t) (b : t) : bool =
  Model_state.equal a.from b.from
  && Model_label.equal a.label b.label
  && Option.equal Model_state.equal a.goto b.goto
  && Option.equal Model_note.annotation_equal a.annotation b.annotation
  && (List.equal Mebi_constr.Tree.equal) a.constructor_trees b.constructor_trees
;;

let compare (a : t) (b : t) : int =
  Utils.compare_chain
    [ Model_state.compare a.from b.from
    ; Model_label.compare a.label b.label
    ; Option.compare Model_state.compare a.goto b.goto
    ; Option.compare Model_note.annotation_compare a.annotation b.annotation
    ; (List.compare Mebi_constr.Tree.compare)
        a.constructor_trees
        b.constructor_trees
    ]
;;

let annotation_is_empty : t -> bool = function
  | { annotation = None; _ } -> true
  | { annotation = Some annotation; _ } ->
    Model_note.annotation_is_empty annotation
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
    Mebi_constr.Tree.list_to_string ~args:(nest args) x.constructor_trees
  in
  let annotation : string =
    Option.cata
      (fun y -> Model_note.annotation_to_string ~args:(nest args) y)
      "None"
      x.annotation
  in
  record
    ~args
    [ "from", from
    ; "label", label
    ; "goto", goto
    ; "constructor_trees", constructor_trees
    ; "annotation", annotation
    ]
;;
