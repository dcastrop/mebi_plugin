(** [action] is a 2-tuple with a unique [id] and (non-unique) [label]).
    - [id] is an integer for identifying the action.
    - [label] is a (pretty-printed) string describing the action. *)
type action =
  { id : int
  ; label : string
  ; is_tau : bool
  }

let eq (a1 : action) (a2 : action) : bool =
  match a1, a2 with
  | { id = i1; label = l1; is_tau = t1 }, { id = i2; label = l2; is_tau = t2 }
    -> i1 == i2 && l1 == l2 && t1 == t2
;;
