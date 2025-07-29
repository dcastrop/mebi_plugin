Require Import MEBI.loader.

Require Export String.
Require Import PeanoNat.
Require Import Notations.
Require Export Bool.
Require Import List.
Import ListNotations.

Require Import MEBI.Examples.CADP.

(** [mutual_exclusion]
  [ true*.
    { ENTER ?i:nat }.
    (not { LEAVE !i })*.
    { ENTER ?j:nat where j<>i }
  ] false
*)

Definition pid_of_action (a:action) : option pid :=
  match a with 
  | SILENT => None
  | LABEL (_, p) => Some p
  end.

Definition log : Type := list label.

Definition update_log (l:log) (a:action) : log :=
  match a with 
  | SILENT => l
  | LABEL (ENTER, p) => (ENTER, p) :: l
  | LABEL (LEAVE, p) => (LEAVE, p) :: l
  | LABEL _ => l
  end.

(** [verify_log l p] verifies whether the contents of log [l] adheres to mutual exclusion, i.e., if a process [p] performs an [ENTER] action, then the most recent action of all other processes must be either nothing or [LEAVE]. *)
Fixpoint verify_log (l:log) (p:option pid) (q:option pid) : Prop :=
  match l with 
  | [] => match q with 
          | None => True
          | Some _ => False
          end
  | h::t => 
    match h, p, q with 
    (* case (A): last action [h] was [ENTER] *)
    | (ENTER, pid1), None,      None => verify_log t (Some pid1) None

    (* (A) cannot have [ENTER] without [LEAVE] *)
    | (ENTER, pid1), Some pid2, None => False

    (* (A) [LEAVE] must have corresponding [ENTER] in [t] *)
    | (LEAVE, pid1), Some pid2, None => verify_log t (Some pid2) (Some pid1)

    (* (A) [ENTER] must correspond to previous [LEAVE] *)
    | (ENTER, pid1), Some pid2, Some pid3 => pid1 =  pid3 -> 
                                             verify_log t (Some pid2) None

    | (LEAVE, pid1), Some pid2, Some pid3 => verify_log t (Some pid2) (Some pid1)

    (* case (B): last action [h] was [LEAVE] *)
    | (LEAVE, pid1), None,      None => verify_log t None (Some pid1)

    (* (B) two [LEAVE] must be separated by corresponding [ENTER] *)
    | (LEAVE, pid1), None,      Some pid3 => False

    (* (B) [ENTER] must correspond to previous [LEAVE] *)
    | (ENTER, pid1), None,      Some pid3 => pid1 =  pid3 -> 
                                             verify_log t None None
    (* else *)
    | (NCS,              _), _, _ => False
    | (READ_NEXT,        _), _, _ => False
    | (READ_LOCKED,      _), _, _ => False
    | (WRITE_NEXT _ _,   _), _, _ => False
    | (WRITE_LOCKED _ _, _), _, _ => False
    | (FETCH_AND_STORE,  _), _, _ => False
    | (COMPARE_AND_SWAP, _), _, _ => False
    end
  end.


(** [MutualExclusion.lts] is defined so long as when a process [p] does an [ENTER] action, there are no other processes whose last action was [ENTER], rather their last action should either be LEAVE or nothing. *)


Inductive valid_lts : (sys * resource) * log -> action -> (sys * resource) * log -> Prop :=
| VALID_LTS : forall p1 p2 r1 r2 a l,
    verify_log l None None /\ verify_log (update_log l a) None None -> 
    lts (p1, r1) a (p2, r2) -> 
    valid_lts ((p1, r1), l) a ((p2, r2), update_log l a) 
.

Inductive valid_lts_transitive_closure : (sys * resource) * log -> Prop :=
| trans_valid_lts : forall t a t' e' e l l',
    lts (t, e) a (t', e') ->
    valid_lts_transitive_closure ((t', e'), l') ->
    valid_lts_transitive_closure ((t, e), l)

| no_valid_lts : forall t e, valid_lts_transitive_closure (t, e)
.

(* MeBi Dump "g1_weak_mutual_ex_base_FSM" 
     FSM Bounded 1024 Of (g1, []) With valid_lts Weak SILENT Of action Using valid_lts lts step. *)

(* Goal valid_lts_transitive_closure (g1, []).
  unfold g1.
  eapply trans_valid_lts.
  constructor.
Abort. *)


Inductive valid_big : (sys * resource) * log -> action -> (sys * resource) * log -> Prop :=
| VALID_BIG : forall p1 p2 r1 r2 a l,
    verify_log l None None /\ verify_log (update_log l a) None None -> 
    bigstep (p1, r1) a (p2, r2) -> 
    valid_big ((p1, r1), l) a ((p2, r2), update_log l a) 
.

Inductive valid_big_transitive_closure : (sys * resource) * log -> Prop :=
| trans_valid_big : forall t a t' e' e l l',
    bigstep (t, e) a (t', e') ->
    valid_big_transitive_closure ((t', e'), l') ->
    valid_big_transitive_closure ((t, e), l)

| no_valid_big : forall t e, valid_big_transitive_closure (t, e)
.

(* MeBi Dump "g1_weak_mutual_ex_glue_FSM" 
     FSM Bounded 1024 Of (g1, []) With valid_big Weak SILENT Of action Using valid_big bigstep. *)


(* MeBi 
  Dump "g1_weak_mutual_ex_base" 
     Bisim LTS Bounded 50 Of g1         With lts    Weak SILENT Of action
       And LTS Bounded 500 Of (g1, [])  With valid  Weak SILENT Of action
       Using valid lts step.

MeBi 
  Dump "g1_weak_mutual_ex_glue" 
     Bisim LTS Bounded 50 Of g1         With bigstep  Weak SILENT Of action
       And LTS Bounded 500 Of (g1, [])  With valid    Weak SILENT Of action
       Using valid bigstep lts step. *)





(* Inductive lts : sys * resource * log -> action -> sys * resource * log -> Prop :=

| LTS_PRC : forall t1 t2 s1 s2 r1 r2 a l,
  CADP.lts (PRC t1 s1, r1) a (PRC t2 s2, r2) ->
  lts (PRC t1 s1, r1, l) a (PRC t2 s2, r2, update_log l a s1)

| LTS_PAR : forall pl1 pl2 pr1 pr2 r1 r2 a l1 l2,
  CADP.lts (PAR pl1 pr1, r1, l1) a (PAR pl2 pr2, r2, l2) ->
  lts (PAR pl1 pr1, r1, l1) a (PAR pl2 pr2, r2, l2)

(* | LTS_PAR_L : forall pl1 pl2 pr r1 r2 a l1 l2,
  CADP.lts (PAR pl1 pr, r1, l1) a (PAR pl2 pr, r2, l2) ->
  lts (PAR pl1 pr, r1, l1) a (PAR pl2 pr, r2, l2) *)

(* | LTS_PAR_R : forall pl pr1 pr2 r1 r2 a l1 l2,
  CADP.lts (PAR pl pr1, r1, l1) a (PAR pl pr2, r2, l2)
  lts (PAR pl pr1, r1, l1) a (PAR pl pr2, r2, l2) *)

. *)

Module MutualExclusionOld.

  (** [can_enter p a s] checks that the last act of [p] is LEAVE or nothing. *)
  Definition can_enter (p:pid) (s:sys_trace) : option bool :=
    match can_do_act p ENTER s with
    | None => None
    | Some false => Some false
    | Some true => Some (negb (exist_last_act ENTER (get_other_traces p s)))
    end.

  (** [do_enter p s] returns [Some s] updated with [p] last act ENTER, if [can_enter] is true. *)
  Definition do_enter (p:pid) (s:sys_trace) : option sys_trace :=
    match can_enter p s with
    | Some true => append_act_hd p ENTER s
    | _ => None
    end.


  (** [can_leave p a s] checks that the last act of [p] is ENTER. *)
  Definition can_leave (p:pid) (s:sys_trace) : option bool :=
    can_do_act p LEAVE s.

  (** [do_leave p s] returns [Some s] updated with [p] last act LEAVE, if [can_leave] is true. *)
  Definition do_leave (p:pid) (s:sys_trace) : option sys_trace :=
    match can_leave p s with
    | Some true => append_act_hd p LEAVE s
    | _ => None
    end.

  (** [MutualExclusion.lts] is defined so long as when a process [p] does an [ENTER] action, there are no other processes whose last action was [ENTER], rather their last action should either be LEAVE or nothing. *)
  Inductive lts : sys_trace -> action -> sys_trace -> Prop :=

    | ENTER : forall t1 t2 p,
      do_enter p t1 = Some t2 -> lts t1 (LABEL (ENTER, p)) t2

    | LEAVE : forall t1 t2 p,
      do_leave p t1 = Some t2 -> lts t1 (LABEL (LEAVE, p)) t2

    | SILENT : forall t, lts t SILENT t
    .

End MutualExclusionOld.
