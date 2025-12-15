Require Import MEBI.Examples.CADP.

Import Protocol.

Definition do_acquire_inner (c:tm) (s:state) (r:resource)
  : composition
  :=
  let e : env := (s, r) in
  (* REC_DEF 2 *)
  let t : tm := unfold (AcquireInnerDef, AcquireInnerBody) AcquireInnerBody in
  (* ACT READ_LOCKED *)
  match read_locked e with
  | None => (PRC ERR
                (add_error (Build_error 40
                  "do_acquire_inner, read_locked e failed" (Build_error_info None (None, None))) s),
                r)
  | Some e =>
    (* IF (VAR LOCKED) *)
    match (get_local_var_value LOCKED (get_state e)) with
    | BOOL true => (* REC_CALL 2 *) (PRC (SEQ t c) (get_state e), get_resource e)
    | BOOL false => (PRC c (get_state e), get_resource e)
    | _ => (PRC ERR
                (add_error (Build_error 41
                  "do_acquire_inner, var locked not bool" (Build_error_info None (None, None))) (get_state e)),
                get_resource e)
    end
  end
  .


Definition do_acquire (c:tm) (s:state) (r:resource)
  : composition
  :=
  let e : env := (s, r) in
  (* ACT (WRITE_NEXT THE_PID NIL) *)
  match write_next THE_PID NIL e with
  | None => (PRC ERR
                (add_error (Build_error 50
                  "do_acquire, write_next THE_PID NIL failed" (Build_error_info None (None, None))) s),
                r)
  | Some e =>
    (* ACT FETCH_AND_STORE *)
    match fetch_and_store e with
    | None => (PRC ERR
                  (add_error (Build_error 51
                    "do_acquire, fetch_and_store failed" (Build_error_info None (None, None))) (get_state e)),
                  (get_resource e))
    | Some e =>
      (* IF (EQ (VAR PREDECESSOR) (VAL NIL)) *)
      match (get_local_var_value PREDECESSOR (get_state e)) with
      | NIL => (* OK *) (PRC c (get_state e), get_resource e)
      | _ =>
        (* ACT (WRITE_LOCKED THE_PID (BOOL true)) *)
        match write_locked THE_PID (BOOL true) e with
        | None => (PRC ERR
                      (add_error (Build_error 52
                        "do_acquire, write_locked THE_PID (BOOL true) failed" (Build_error_info None (None, None))) (get_state e)),
                  (get_resource e))
        | Some e =>
          (* ACT (WRITE_NEXT PREDECESSOR (GET THE_PID)) *)
          match write_next PREDECESSOR (GET THE_PID) e with
          | None => (PRC ERR
                        (add_error (Build_error 53
                          "do_acquire, write_next PREDECESSOR (GET THE_PID) failed" (Build_error_info None (None, None))) (get_state e)),
                    (get_resource e))
          | Some e =>
            (PRC (SEQ (AcquireInnerLoop) c) (get_state e)
            , get_resource e
            )
          end
        end
      end
    end
  end
  .

Definition do_release_inner (c:tm) (s:state) (r:resource)
  : composition
  :=
  let e : env := (s, r) in
  (* REC_DEF 1 *)
  let t : tm := unfold (ReleaseInnerDef, ReleaseInnerBody) ReleaseInnerBody in
  (* ACT READ_NEXT *)
  match read_next e with
  | None => (PRC ERR
                (add_error (Build_error 45
                  "do_release_inner, read_next e failed" (Build_error_info None (None, None))) s),
                r)
  | Some e =>
    (* IF (EQ (VAL NIL) (VAR NEXT)) *)
    match (get_local_var_value NEXT (get_state e)) with
    | NIL => (* REC_CALL 1 *) (PRC (SEQ t c) (get_state e), get_resource e)
    | _ =>
      (* ACT (WRITE_LOCKED NEXT (BOOL false)) *)
      match write_locked NEXT (BOOL false) e with
      | None => (PRC ERR
                    (add_error (Build_error 46
                      "do_release_inner, write_locked NEXT (BOOL false) failed" (Build_error_info None (None, None))) (get_state e)),
                    (get_resource e))
      | Some e =>
        (PRC c (get_state e), get_resource e)
      end
    end
  end
  .

Definition do_release (c:tm) (s:state) (r:resource)
  : composition
  :=
  let e : env := (s, r) in
  (* ACT READ_NEXT *)
  match read_next e with
  | None => (PRC ERR
                (add_error (Build_error 60
                  "do_release, read_next e failed" (Build_error_info None (None, None))) s),
                r)
  | Some e =>
    (* IF (EQ (VAL NIL) (VAR NEXT)) *)
    match (get_local_var_value NEXT (get_state e)) with
    | NIL =>
      (* ACT COMPARE_AND_SWAP *)
      match compare_and_swap e with
      | None => (PRC ERR
                    (add_error (Build_error 61
                      "do_acquire, compare_and_swap failed" (Build_error_info None (None, None))) (get_state e)),
                    (get_resource e))
      | Some e =>
        (* IF (EQ FLS (VAR SWAP)) *)
        match (get_local_var_value SWAP (get_state e)) with
        | BOOL false =>
          (PRC (SEQ (ReleaseInnerLoop) c) (get_state e)
          , get_resource e)
        | _ => (* OK *) (PRC c (get_state e), get_resource e)
        end
      end

    | _ =>
      (* ACT (WRITE_LOCKED NEXT (BOOL false)) *)
      match write_locked NEXT (BOOL false) e with
      | None => (PRC ERR
                    (add_error (Build_error 61
                      "do_release, write_locked NEXT (BOOL false) failed" (Build_error_info None (None, None))) (get_state e)),
                    (get_resource e))
      | Some e =>
        (PRC c (get_state e), get_resource e)
      end
    end
  end
  .


Definition do_main_loop (b:tm) (s:state) (r:resource)
  : composition
  :=
  (* REC_DEF 0 *)
  (PRC (unfold (PMainLoopDef, b) b) s, r)
  .

Inductive bigstep : composition -> option label -> composition -> Prop :=
(*  0 *)
| DO_MAIN_LOOP : forall b s r,
  bigstep (PRC (REC_DEF PMainLoopDef b) s, r) 
          None 
          (do_main_loop b s r)

(*  1 *)
| DO_ACQUIRE : forall c s r,
  bigstep (PRC (SEQ (Acquire) (c)) s, r)
          None
          (do_acquire c s r)

(*  2 *)
| DO_RELEASE : forall c s r,
  bigstep (PRC (SEQ (Release) (c)) s, r)
          None
          (do_release c s r)

(*  3 *)
| DO_ACQUIRE_INNER : forall c s r,
  bigstep (PRC (SEQ (AcquireInnerLoop) c) s, r)
          None
          (do_acquire_inner c s r)

(*  4 *)
| DO_RELEASE_INNER : forall c s r,
  bigstep (PRC (SEQ (ReleaseInnerLoop) c) s, r)
          None
          (do_release_inner c s r)

(*  5 *)
| DO_SEQ_ACT_ENTER : forall y s r,
  bigstep (PRC (SEQ (ACT ENTER) y) s, r) 
          (Some (ENTER, (get_pid s))) 
          (PRC y s, r)

(*  6 *)
| DO_SEQ_ACT_LEAVE : forall y s r,
  bigstep (PRC (SEQ (ACT LEAVE) y) s, r) 
          (Some (LEAVE, (get_pid s))) 
          (PRC y s, r)

(*  7 *)
| DO_PAR_L : forall a l1 l2 r gr1 gr2,
  bigstep (l1, gr1) a (l2, gr2) ->
  bigstep (PAR l1 r, gr1) a (PAR l2 r, gr2)

(*  8 *)
| DO_PAR_R : forall a l r1 r2 gr1 gr2,
  bigstep (r1, gr1) a (r2, gr2) ->
  bigstep (PAR l r1, gr1) a (PAR l r2, gr2)

.
