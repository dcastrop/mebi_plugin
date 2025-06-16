(** [action] is a 2-tuple with a unique [id] and (non-unique) [label]).
    - [id] is an integer for identifying the action.
    - [label] is a (pretty-printed) string describing the action. *)
type t =
  { label : Model_label.t
  ; is_silent : bool option
  ; info : string option
  ; mutable annos : annotations
  }

and annotation = (Model_state.t * t) list
and annotations = annotation list

let rec anno_eq (a1 : annotation) (a2 : annotation) : bool =
  match a1, a2 with
  | [], [] -> true
  | [], h :: t -> false
  | h :: t, [] -> false
  | (e1, s1) :: t1, (e2, s2) :: t2 ->
    Model_state.eq e1 e2 && eq s1 s2 && anno_eq t1 t2

and annos_eq (a1 : annotations) (a2 : annotations) : bool =
  match a1, a2 with
  | [], [] -> true
  | [], h :: t -> false
  | h :: t, [] -> false
  | h1 :: t1, h2 :: t2 -> anno_eq h1 h2 && annos_eq t1 t2

and eq (a1 : t) (a2 : t) : bool =
  match a1, a2 with
  | ( { label = v1; is_silent = t1; info = i1; annos = a1 }
    , { label = v2; is_silent = t2; info = i2; annos = a2 } ) ->
    Model_label.eq v1 v2
    && (match t1, t2 with
        | None, None -> true
        | Some t1, Some t2 -> Bool.equal t1 t2
        | _, _ -> false)
    && (match i1, i2 with
        | None, None -> true
        | Some s1, Some s2 -> String.equal s1 s2
        | _, _ -> false)
    && annos_eq a1 a2
;;

let rec anno_compare (a1 : annotation) (a2 : annotation) : int =
  match a1, a2 with
  | [], [] -> 0
  | [], h :: t -> -1
  | h :: t, [] -> 1
  | (e1, s1) :: t1, (e2, s2) :: t2 ->
    Int.compare (Model_state.compare e1 e2) (compare s1 s2)

and annos_compare (a1 : annotations) (a2 : annotations) : int =
  match a1, a2 with
  | [], [] -> 0
  | [], h :: t -> -1
  | h :: t, [] -> 1
  | h1 :: t1, h2 :: t2 -> Int.compare (anno_compare h1 h2) (annos_compare t1 t2)

and compare (a1 : t) (a2 : t) : int =
  match a1, a2 with
  | ( { label = v1; is_silent = t1; info = i1; annos = a1 }
    , { label = v2; is_silent = t2; info = i2; annos = a2 } ) ->
    Int.compare
      (Model_label.compare v1 v2)
      (Int.compare
         (match t1, t2 with
          | None, None -> 0
          | None, Some _ -> -1
          | Some _, None -> 1
          | Some t1, Some t2 -> Bool.compare t1 t2)
         (Int.compare
            (annos_compare a1 a2)
            (match i1, i2 with
             | None, None -> 0
             | None, Some _ -> -1
             | Some _, None -> 1
             | Some s1, Some s2 -> String.compare s1 s2)))
;;

let hash (a : t) : int = match a with { label; _ } -> Model_label.hash label
