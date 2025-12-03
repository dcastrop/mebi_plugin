type t =
  { from : Model_state.t
  ; label : Model_label.t
  ; goto : Model_state.t
  ; annotations : Model_note.annotations option
  ; constructor_trees : Mebi_constr.Tree.t list option
  }

let equal (a : t) (b : t) : bool =
  Model_state.equal a.from b.from
  && Model_label.equal a.label b.label
  && Model_state.equal a.goto b.goto
  && Option.equal
       (List.equal Mebi_constr.Tree.equal)
       a.constructor_trees
       b.constructor_trees
;;

let compare (a : t) (b : t) : int =
  Utils.compare_chain
    [ Model_state.compare a.from b.from
    ; Model_label.compare a.label b.label
    ; Model_state.compare a.goto b.goto
    ; Option.compare
        (List.compare Mebi_constr.Tree.compare)
        a.constructor_trees
        b.constructor_trees
    ]
;;

let annotations_is_empty : t -> bool = function
  | { annotations = None; _ } -> true
  | { annotations = Some annotations; _ } ->
    Model_note.annotations_is_empty annotations
;;

open Utils.Strfy

let to_string ?(args : style_args = style_args ()) (x : t) : string =
  let args : style_args =
    { args with
      name = None
    ; style = Some { (record_style ()) with inline = true }
    ; newline = false
    ; nested = false
    }
  in
  let from : string = Model_state.to_string ~args:(nest args) x.from in
  let label : string = Model_label.to_string ~args:(nest args) x.label in
  let goto : string = Model_state.to_string ~args:(nest args) x.goto in
  let constructor_trees : string =
    Option.cata
      (fun y -> Mebi_constr.Tree.list_to_string ~args:(nest args) y)
      "None"
      x.constructor_trees
  in
  let annotations : string =
    Option.cata
      (fun y -> Model_note.annotations_to_string ~args:(nest args) y)
      "None"
      x.annotations
  in
  record
    ~args:{ args with newline = true }
    [ "from", from
    ; "label", label
    ; "goto", goto
    ; "constructor_trees", constructor_trees
    ; "annotations", annotations
    ]
;;
