(** [action] is a 2-tuple with a unique [id] and (non-unique) [label]).
    - [id] is an integer for identifying the action.
    - [label] is a (pretty-printed) string describing the action. *)
type action =
  { label : string
  ; is_tau : bool
  }

let eq (a1 : action) (a2 : action) : bool =
  match a1, a2 with
  | { label = l1; is_tau = t1 }, { label = l2; is_tau = t2 } ->
    String.equal l1 l2 && t1 == t2
;;
