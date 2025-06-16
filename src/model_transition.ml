type t = Model_state.t * Model_label.t * Model_state.t * Model_label.meta option

let eq (t1 : t) (t2 : t) =
  match t1, t2 with
  | (from1, a1, dest1, meta1), (from2, a2, dest2, meta2) ->
    Model_state.eq from1 from2
    && Model_label.eq a1 a2
    && Model_state.eq dest1 dest2
    &&
      (match meta1, meta2 with
      | None, None -> true
      | None, Some _ -> false
      | Some _, None -> false
      | Some m1, Some m2 ->
        (match m1.is_silent, m2.is_silent with
         | None, None -> true
         | None, Some _ -> false
         | Some _, None -> false
         | Some s1, Some s2 -> Bool.equal s1 s2))
;;

let compare (t1 : t) (t2 : t) =
  match t1, t2 with
  | (from1, a1, dest1, meta1), (from2, a2, dest2, meta2) ->
    Int.compare
      (Int.compare
         (Model_state.compare from1 from2)
         (Model_state.compare dest1 dest2))
      (Int.compare
         (Model_label.compare a1 a2)
         (match meta1, meta2 with
          | None, None -> 0
          | None, Some _ -> -1
          | Some _, None -> 1
          | Some m1, Some m2 ->
            (match m1.is_silent, m2.is_silent with
             | None, None -> 0
             | None, Some _ -> -1
             | Some _, None -> 1
             | Some s1, Some s2 -> Bool.compare s1 s2)))
;;
