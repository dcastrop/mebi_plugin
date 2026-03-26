Require Import MEBI.Examples.CADP.

Import Protocol.

Definition return_error' (n:nat) (m:string) (s:state) (r:resource) : composition := (PRC (ERR, (add_error (Build_error n m (Build_error_info None (None, None))) s)), r).


Definition do_acquire_inner (c:tm) (s:state) (r:resource) : composition :=
(* REC_DEF 2 *)
(* let t : tm := unfold (AcquireInnerDef, AcquireInnerBody) AcquireInnerBody in *)
let t : tm := AcquireInnerLoop in
(* ACT READ_LOCKED *)
match read_locked (s, r) with
| None => return_error' 40 "do_acquire_inner, read_locked e failed" s r
| Some (s, r) =>
  (* IF (VAR LOCKED) *)
  match value_to_bool_opt (get_local_var_value LOCKED s) with
  | None => return_error' 41 "do_acquire_inner, var locked not bool" s r
  | Some true => (* REC_CALL 2 *) (PRC (SEQ t c, s), r)
  | Some false => (PRC (c, s), r)
  end
end.

(* Compute (unfold (AcquireInnerDef, AcquireInnerBody) AcquireInnerBody). *)


Definition do_acquire (c:tm) (s:state) (r:resource) : composition :=
(* ACT (WRITE_NEXT THE_PID NIL) *)
match write_next THE_PID NIL (s, r) with
| None => return_error' 42 "do_acquire, write_next THE_PID NIL failed" s r
| Some (s, r) =>
  (* ACT FETCH_AND_STORE *)
  let e_:env := fetch_and_store (s, r) in
  let s:state := fst e_ in
  let r:resource := snd e_ in
  (* IF (EQ (VAR PREDECESSOR) (VAL NIL)) *)
  match value_to_index_opt (get_local_var_value PREDECESSOR s) with
  | None => return_error' 43 "do_acquire, PREDECESSOR not INDEX" s r
  | Some None => (* OK *) (PRC (c, s), r)
  | Some (Some _i) =>
    (* ACT (WRITE_LOCKED THE_PID (BOOL true)) *)
    match write_locked THE_PID (BOOL true) (s, r) with
    | None => return_error' 44 "do_acquire, write_locked THE_PID (BOOL true) failed" s r
    | Some (s, r) =>
      (* ACT (WRITE_NEXT PREDECESSOR (GET THE_PID)) *)
      match write_next PREDECESSOR (GET THE_PID) (s, r) with
      | None =>  return_error' 45 "do_acquire, write_next PREDECESSOR (GET THE_PID) failed" s r 
      | Some (s, r) => (PRC (SEQ (AcquireInnerLoop) c, s), r)
      end
    end
  end
end.

Definition do_release_inner (c:tm) (s:state) (r:resource) : composition :=
(* REC_DEF 1 *)
(* let t : tm := unfold (ReleaseInnerDef, ReleaseInnerBody) ReleaseInnerBody in *)
let t : tm := ReleaseInnerLoop in
(* ACT READ_NEXT *)
match read_next (s, r) with
| None => return_error' 50 "do_release_inner, read_next e failed" s r
| Some (s, r) =>
  (* IF (EQ (VAL NIL) (VAR NEXT)) *)
  match value_to_index_opt (get_local_var_value NEXT s) with
  | None => return_error' 51 "do_release_inner, NEXT not INDEX" s r
  | Some None => (* REC_CALL 1 *) (PRC ((SEQ t c), s), r)
  | Some (Some _i) =>
    (* ACT (WRITE_LOCKED NEXT (BOOL false)) *)
    match write_locked NEXT (BOOL false) (s, r) with
    | None => return_error' 52 "do_release_inner, write_locked NEXT (BOOL false) failed" s r
    | Some (s, r) => (PRC (c, s), r)
    end
  end
end.

Definition do_release (c:tm) (s:state) (r:resource) : composition :=
(* ACT READ_NEXT *)
match read_next (s, r) with
| None => return_error' 53 "do_release, read_next e failed" s r
| Some (s, r) =>
  (* IF (EQ (VAL NIL) (VAR NEXT)) *)
  match value_to_index_opt (get_local_var_value NEXT s) with
  | None => return_error' 54 "do_release, NEXT not INDEX" s r
  | Some None => 
    (* ACT COMPARE_AND_SWAP *)
    let e_:env := compare_and_swap (s, r) in
    let s:state := fst e_ in
    let r:resource := snd e_ in
    (* IF (EQ FLS (VAR SWAP)) *)
    match value_to_bool_opt (get_local_var_value SWAP s) with
    | None => return_error' 55 "do_release, SWAP not BOOL" s r
    | Some false => (PRC (SEQ (ReleaseInnerLoop) c, s), r)
    | Some true => (* OK *) (PRC (c, s), r)
    end
  | Some (Some _i) =>
    (* ACT (WRITE_LOCKED NEXT (BOOL false)) *)
    match write_locked NEXT (BOOL false) (s, r) with
    | None => return_error' 56 "do_release, write_locked NEXT (BOOL false) failed" s r
    | Some (s, r) => (PRC (c, s), r)
    end
  end
end.


Definition do_main_loop (b:tm) (s:state) (r:resource) : composition :=
  (* REC_DEF 0 *)
  (PRC ((unfold (PMainLoopDef, b) b), s), r).

Inductive bigstep : composition -> option label -> composition -> Prop :=
| DO_MAIN_LOOP : forall b s r,
  bigstep (PRC (REC_DEF PMainLoopDef b, s), r) None (do_main_loop b s r)
| DO_ACQUIRE : forall c s r,
  bigstep (PRC (SEQ Acquire c, s), r) None (do_acquire c s r)
| DO_RELEASE : forall c s r,
  bigstep (PRC (SEQ Release c, s), r) None (do_release c s r)
| DO_ACQUIRE_INNER : forall c s r,
  bigstep (PRC (SEQ AcquireInnerLoop c, s), r) None (do_acquire_inner c s r)
| DO_RELEASE_INNER : forall c s r,
  bigstep (PRC (SEQ ReleaseInnerLoop c, s), r) None (do_release_inner c s r)
| DO_SEQ_ACT_ENTER : forall y s r,
  bigstep (PRC (SEQ (ACT ENTER) y, s), r) (Some (ENTER, (get_pid s))) (PRC (y, s), r)
| DO_SEQ_ACT_LEAVE : forall y s r,
  bigstep (PRC (SEQ (ACT LEAVE) y, s), r) (Some (LEAVE, (get_pid s))) (PRC (y, s), r)
| DO_PAR_L : forall a l1 l2 r gr1 gr2,
  bigstep (l1, gr1) a (l2, gr2) ->
  bigstep (PAR l1 r, gr1) a (PAR l2 r, gr2)
| DO_PAR_R : forall a l r1 r2 gr1 gr2,
  bigstep (r1, gr1) a (r2, gr2) ->
  bigstep (PAR l r1, gr1) a (PAR l r2, gr2).