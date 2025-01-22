(** [Examples] contains examples to be used by either [KS90] or [PT87]. *)
open Fsm

(** [example] is a type for denoting pairs of fsms that we check are bisimilar. *)
type example =
  { name : string
  ; s : fsm
  ; t : fsm
  ; are_bisimilar : bool
  }

let exa (name : string) (s : fsm) (t : fsm) (are_bisimilar : bool) : example =
  { name; s; t; are_bisimilar }
;;

(** [exa_1] is `Example 3.2.5` on page 106. *)
let exa_1 : example =
  (* s *)
  let (s : fsm) =
    let init = { id = 0; pp = "s0" } in
    let states =
      States.of_list
        [ { id = 0; pp = "s0" }; { id = 1; pp = "s1" }; { id = 2; pp = "s2" } ]
    in
    let alphabet =
      Alphabet.of_list [ { id = 1; label = "a" }; { id = 2; label = "b" } ]
    in
    let edges = Edges.create 4 in
    (* s0 *)
    Edges.add
      edges
      (get_state_by_id states 0)
      (Actions.of_seq
         (List.to_seq
            [ ( get_action_by_label alphabet "a"
              , States.of_list
                  [ get_state_by_id states 1; get_state_by_id states 2 ] )
            ]));
    (* s1 *)
    Edges.add
      edges
      (get_state_by_id states 1)
      (Actions.of_seq
         (List.to_seq
            [ ( get_action_by_label alphabet "b"
              , States.of_list [ get_state_by_id states 2 ] )
            ]));
    (* s2 *)
    Edges.add
      edges
      (get_state_by_id states 2)
      (Actions.of_seq
         (List.to_seq
            [ ( get_action_by_label alphabet "b"
              , States.of_list [ get_state_by_id states 2 ] )
            ]));
    { init; states; alphabet; edges }
  in
  (* t *)
  let (t : fsm) =
    let init = { id = 0; pp = "t0" } in
    let states =
      States.of_list [ { id = 0; pp = "t0" }; { id = 1; pp = "t1" } ]
    in
    let alphabet =
      Alphabet.of_list [ { id = 1; label = "a" }; { id = 2; label = "b" } ]
    in
    let edges = Edges.create 4 in
    (* t0 *)
    Edges.add
      edges
      (get_state_by_id states 0)
      (Actions.of_seq
         (List.to_seq
            [ ( get_action_by_label alphabet "a"
              , States.of_list [ get_state_by_id states 1 ] )
            ]));
    (* t1 *)
    Edges.add
      edges
      (get_state_by_id states 1)
      (Actions.of_seq
         (List.to_seq
            [ ( get_action_by_label alphabet "b"
              , States.of_list [ get_state_by_id states 1 ] )
            ]));
    { init; states; alphabet; edges }
  in
  exa "exa1" s t true
;;

(** [exa_2] is `Example 3.2.6` on page 107. *)
let exa_2 : example =
  (* s *)
  let (s : fsm) =
    let init = { id = 0; pp = "s0" } in
    let states =
      States.of_list
        [ { id = 0; pp = "s0" }
        ; { id = 1; pp = "s1" }
        ; { id = 2; pp = "s2" }
        ; { id = 3; pp = "s3" }
        ; { id = 4; pp = "s4" }
        ]
    in
    let alphabet =
      Alphabet.of_list [ { id = 1; label = "a" }; { id = 2; label = "b" } ]
    in
    let edges = Edges.create 4 in
    (* s0 *)
    Edges.add
      edges
      (get_state_by_id states 0)
      (Actions.of_seq
         (List.to_seq
            [ ( get_action_by_label alphabet "a"
              , States.of_list
                  [ get_state_by_id states 1; get_state_by_id states 2 ] )
            ]));
    (* s1 *)
    Edges.add
      edges
      (get_state_by_id states 1)
      (Actions.of_seq
         (List.to_seq
            [ ( get_action_by_label alphabet "a"
              , States.of_list [ get_state_by_id states 3 ] )
            ; ( get_action_by_label alphabet "b"
              , States.of_list [ get_state_by_id states 4 ] )
            ]));
    (* s2 *)
    Edges.add
      edges
      (get_state_by_id states 2)
      (Actions.of_seq
         (List.to_seq
            [ ( get_action_by_label alphabet "a"
              , States.of_list [ get_state_by_id states 4 ] )
            ]));
    (* s3 *)
    Edges.add
      edges
      (get_state_by_id states 3)
      (Actions.of_seq
         (List.to_seq
            [ ( get_action_by_label alphabet "a"
              , States.of_list [ get_state_by_id states 0 ] )
            ]));
    (* s4 *)
    Edges.add
      edges
      (get_state_by_id states 4)
      (Actions.of_seq
         (List.to_seq
            [ ( get_action_by_label alphabet "a"
              , States.of_list [ get_state_by_id states 0 ] )
            ]));
    { init; states; alphabet; edges }
  in
  (* t *)
  let (t : fsm) =
    let init = { id = 0; pp = "t0" } in
    let states =
      States.of_list
        [ { id = 0; pp = "t0" }
        ; { id = 1; pp = "t1" }
        ; { id = 2; pp = "t2" }
        ; { id = 3; pp = "t3" }
        ; { id = 4; pp = "t4" }
        ; { id = 5; pp = "t5" }
        ]
    in
    let alphabet =
      Alphabet.of_list [ { id = 1; label = "a" }; { id = 2; label = "b" } ]
    in
    let edges = Edges.create 4 in
    (* t0 *)
    Edges.add
      edges
      (get_state_by_id states 0)
      (Actions.of_seq
         (List.to_seq
            [ ( get_action_by_label alphabet "a"
              , States.of_list
                  [ get_state_by_id states 1; get_state_by_id states 3 ] )
            ]));
    (* t1 *)
    Edges.add
      edges
      (get_state_by_id states 1)
      (Actions.of_seq
         (List.to_seq
            [ ( get_action_by_label alphabet "b"
              , States.of_list [ get_state_by_id states 2 ] )
            ; ( get_action_by_label alphabet "a"
              , States.of_list [ get_state_by_id states 5 ] )
            ; ( get_action_by_label alphabet "b"
              , States.of_list [ get_state_by_id states 5 ] )
            ]));
    (* t2 *)
    Edges.add
      edges
      (get_state_by_id states 2)
      (Actions.of_seq
         (List.to_seq
            [ ( get_action_by_label alphabet "a"
              , States.of_list [ get_state_by_id states 0 ] )
            ]));
    (* t3 *)
    Edges.add
      edges
      (get_state_by_id states 3)
      (Actions.of_seq
         (List.to_seq
            [ ( get_action_by_label alphabet "a"
              , States.of_list [ get_state_by_id states 4 ] )
            ]));
    (* t4 *)
    Edges.add
      edges
      (get_state_by_id states 4)
      (Actions.of_seq
         (List.to_seq
            [ ( get_action_by_label alphabet "a"
              , States.of_list [ get_state_by_id states 0 ] )
            ]));
    (* t5 *)
    Edges.add
      edges
      (get_state_by_id states 5)
      (Actions.of_seq
         (List.to_seq
            [ ( get_action_by_label alphabet "a"
              , States.of_list
                  [ get_state_by_id states 0; get_state_by_id states 4 ] )
            ]));
    { init; states; alphabet; edges }
  in
  exa "exa2" s t false
;;

(** [exa_mc] ...
    I was wondering about how to encode mixed-states using either mixed-choice or silent transitions -- Jonah *)
let exa_mc : example =
  (* [s] has a mixed-choice *)
  let s : fsm =
    let init = { id = 0; pp = "s0" } in
    let states =
      States.of_list
        [ { id = 0; pp = "s0" }
        ; { id = 1; pp = "s1" }
        ; { id = 2; pp = "s2" }
        ; { id = 3; pp = "sEnd" }
        ]
    in
    let alphabet =
      Alphabet.of_list
        [ { id = 0; label = "silent" }
        ; { id = 1; label = "send" }
        ; { id = 2; label = "recv" }
        ]
    in
    let edges = Edges.create 4 in
    (* s0 *)
    Edges.add
      edges
      (get_state_by_id states 0)
      (Actions.of_seq
         (List.to_seq
            [ ( get_action_by_label alphabet "send"
              , States.of_list [ get_state_by_id states 1 ] )
            ; ( get_action_by_label alphabet "recv"
              , States.of_list [ get_state_by_id states 2 ] )
            ; ( get_action_by_label alphabet "silent"
              , States.of_list [ get_state_by_id states 0 ] )
            ]));
    (* s1 *)
    Edges.add
      edges
      (get_state_by_id states 1)
      (Actions.of_seq
         (List.to_seq
            [ ( get_action_by_label alphabet "recv"
              , States.of_list [ get_state_by_id states 3 ] )
            ]));
    (* s2 *)
    Edges.add
      edges
      (get_state_by_id states 2)
      (Actions.of_seq
         (List.to_seq
            [ ( get_action_by_label alphabet "send"
              , States.of_list [ get_state_by_id states 3 ] )
            ]));
    make_fsm init alphabet states edges
  (* [t] has silent transitions *)
  and t : fsm =
    let init = { id = 0; pp = "t0" } in
    let states =
      States.of_list
        [ { id = 0; pp = "t0" }
        ; { id = 1; pp = "t1" }
        ; { id = 2; pp = "t2" }
        ; { id = 3; pp = "t3" }
        ; { id = 4; pp = "tEnd" }
        ]
    in
    let alphabet =
      Alphabet.of_list
        [ { id = 0; label = "silent" }
        ; { id = 1; label = "send" }
        ; { id = 2; label = "recv" }
        ]
    in
    let edges = Edges.create 4 in
    (* t0 *)
    Edges.add
      edges
      (get_state_by_id states 0)
      (Actions.of_seq
         (List.to_seq
            [ ( get_action_by_label alphabet "send"
              , States.of_list [ get_state_by_id states 1 ] )
            ; ( get_action_by_label alphabet "silent"
              , States.of_list [ get_state_by_id states 2 ] )
            ]));
    (* t1 *)
    Edges.add
      edges
      (get_state_by_id states 1)
      (Actions.of_seq
         (List.to_seq
            [ ( get_action_by_label alphabet "recv"
              , States.of_list [ get_state_by_id states 4 ] )
            ]));
    (* t2 *)
    Edges.add
      edges
      (get_state_by_id states 2)
      (Actions.of_seq
         (List.to_seq
            [ ( get_action_by_label alphabet "recv"
              , States.of_list [ get_state_by_id states 3 ] )
            ; ( get_action_by_label alphabet "silent"
              , States.of_list [ get_state_by_id states 0 ] )
            ]));
    (* t3 *)
    Edges.add
      edges
      (get_state_by_id states 3)
      (Actions.of_seq
         (List.to_seq
            [ ( get_action_by_label alphabet "send"
              , States.of_list [ get_state_by_id states 4 ] )
            ]));
    make_fsm init alphabet states edges
  in
  exa "exa_mc" s t false
;;
