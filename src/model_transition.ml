type t =
  { from : Model_state.t
  ; label : Model_label.t
  ; goto : Model_state.t
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
  Int.compare
    (Model_state.compare a.from b.from)
    (Int.compare
       (Model_label.compare a.label b.label)
       (Int.compare
          (Model_state.compare a.goto b.goto)
          (Option.compare
             (List.compare Mebi_constr.Tree.compare)
             a.constructor_trees
             b.constructor_trees)))
;;

let to_string (a : t) : string = "TODO: model_transition.to_string"
