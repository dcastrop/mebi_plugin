From Stdlib Require Export String.
From Stdlib Require Import PeanoNat.
Require Import Notations.
From Stdlib Require Import Bool.
From Stdlib Require Import Lists.List.
Import ListNotations.

Fixpoint app {X:Type} (l1 l2 : list X) : list X :=
match l1 with
| nil    => l2
| h :: t => h :: (app t l2)
end.

(* pid is a non-nil index *)
Definition pid   : Type  := nat.
Definition index : Type  := option pid.
Definition Nil   : index := None.
Definition index_initial : index := Nil.
Definition index_of_pid (x:pid) : index  := Some x.

Definition index_eqb (a:index) (b:index) : bool :=
match a, b with 
| None, None => true
| Some a, Some b => Nat.eqb a b
| _, _ => false
end.

Definition idef : Type := nat.

Record local_vars := { var_predecessor : index (* acquire *)
                     ; var_locked      : bool  (* acquire *)
                     ; var_next        : index (* release *)
                     ; var_swap        : bool  (* release *) }.

Definition local_vars_initial : local_vars := Build_local_vars Nil false Nil false.

Definition local_vars_set_predecessor (i:index) (v:local_vars) : local_vars := Build_local_vars i (var_locked v) (var_next v) (var_swap v).
Definition local_vars_set_locked (b:bool) (v:local_vars) : local_vars := Build_local_vars (var_predecessor v) b (var_next v) (var_swap v).
Definition local_vars_set_next (i:index) (v:local_vars) : local_vars := Build_local_vars (var_predecessor v) (var_locked v) i (var_swap v).
Definition local_vars_set_swap (b:bool) (v:local_vars) : local_vars := Build_local_vars (var_predecessor v) (var_locked v) (var_next v) b.

Inductive local_var : Type :=
| PREDECESSOR
| LOCKED
| NEXT
| SWAP
| THE_PID.

Definition state : Type := pid * local_vars.
Definition state_create (p:pid) : state := (p, local_vars_initial).
Definition state_set_vars (v:local_vars) (s:state) : state := (fst s, v).

(* FIXME: David mentioned to use [Set] instead of [Type] here,
but it returned [Universal inconsistency, cannot enforce pid.u0<=Set] *)
Inductive value : Type (*Set*) :=
| NAT : nat -> value
| PID : pid -> value
| INDEX : index -> value
| BOOL : bool -> value
| NIL : value
| GET : local_var -> value.

Inductive value_eq : (value * value) -> bool -> Prop :=
| VAL_EQ_NIL : value_eq (NIL, NIL) true
| VAL_EQ_NAT : forall x y, value_eq (NAT x, NAT y) (Nat.eqb x y)
| VAL_EQ_PID : forall x y, value_eq (PID x, PID y) (Nat.eqb x y)
| VAL_EQ_BOOL : forall x y, value_eq (BOOL x, BOOL y) (eqb x y)
| VAL_EQ_INDEX : forall x y, value_eq (INDEX x, INDEX y) (index_eqb x y).

Definition value_of_index (i:index) : value :=
match i with
| None => NIL
| _ => INDEX i
end.

Inductive var_value : (state * local_var) -> value -> Prop :=
| VARVAL_PREDECESSOR : forall p v, 
  var_value ((p, v), PREDECESSOR) (value_of_index (var_predecessor v))
| VARVAL_NEXT : forall p v, 
  var_value ((p, v), NEXT) (value_of_index (var_next v))
| VARVAL_LOCKED : forall p v, var_value ((p, v), LOCKED) (BOOL (var_locked v))
| VARVAL_SWAP : forall p v, var_value ((p, v), SWAP) (BOOL (var_swap v))
| VARVAL_THE_PID : forall p v, var_value ((p, v), THE_PID) (PID p).

Inductive get_pid : (state * local_var) -> pid -> Prop :=
| GET_PID_INDEX : forall s x y, 
  var_value (s, x) (INDEX (Some y)) -> get_pid (s, x) y
| GET_PID_PID : forall s x y, var_value (s, x) (PID y) -> get_pid (s, x) y.

Inductive value_to_index : (state * value) -> index -> Prop :=
| VAL_TO_INDEX_NIL : forall s, value_to_index (s, NIL) Nil
| VAL_TO_INDEX_NAT : forall s x, value_to_index (s, NAT x) (Some x)
| VAL_TO_INDEX_PID : forall s x, value_to_index (s, PID x) (Some x)
| VAL_TO_INDEX_INDEX : forall s x, value_to_index (s, INDEX x) x
| VAL_TO_INDEX_GET : forall s x y z, 
  var_value (s, x) y -> value_to_index (s, y) z -> value_to_index (s, GET x) z.

Inductive value_to_bool : (state * value) -> bool -> Prop :=
| VAL_TO_BOOL : forall s x, value_to_bool (s, BOOL x) x
| VAL_TO_BOOL_GET : forall s x y z, 
  var_value (s, x) y -> value_to_bool (s, y) z -> value_to_bool (s, GET x) z.

Inductive get_value : (state * value) -> value -> Prop :=
| GET_VAL : forall s l k, var_value (s, l) k -> get_value (s, GET l) k
| GET_NIL : forall s, get_value (s, NIL) NIL
| GET_NAT : forall s x, get_value (s, (NAT x)) (NAT x)
| GET_PID : forall s x, get_value (s, (PID x)) (PID x)
| GET_INDEX : forall s x, get_value (s, (INDEX x)) (INDEX x)
| GET_BOOL : forall s x, get_value (s, (BOOL x)) (BOOL x).

Inductive expr : Type :=
| TRU : expr
| FLS : expr
| NOT : expr -> expr
| EQ : expr -> expr -> expr
| VAR : local_var -> expr
| VAL : value -> expr.

Inductive eval : (state * expr) -> value -> Prop :=
| EVAL_TRU : forall s, eval (s, TRU) (BOOL true)
| EVAL_FLS : forall s, eval (s, FLS) (BOOL false)
| EVAL_VAR : forall s v k, var_value (s, v) k -> eval (s, VAR v) k
| EVAL_VAL : forall s v k, get_value (s, v) k -> eval (s, VAL v) k
| EVAL_NOT : forall s x y, 
  eval (s, x) (BOOL y) -> eval (s, (NOT x)) (BOOL (negb y))
| EVAL_EQ : forall s a b x y z, 
  eval (s, a) b ->
  eval (s, x) y -> 
  value_eq (b, y) z -> eval (s, EQ a x) (BOOL z).
  
Inductive action : Type :=
| NCS : action
| ENTER : action
| LEAVE : action
| FETCH_AND_STORE : action
| COMPARE_AND_SWAP : action
| READ_NEXT : action
| READ_LOCKED : action
| WRITE_NEXT : local_var -> value -> action
| WRITE_LOCKED : local_var -> value -> action.

Inductive tm : Type :=
| OK  : tm (* no-op *)
| IF  : expr -> tm -> tm -> tm
| ACT : action -> tm
| SEQ : tm -> tm -> tm
| REC_DEF : idef -> tm -> tm
| REC_CALL : idef -> tm
.

Definition rec_def : Type := idef * tm.

Fixpoint unfold (new:rec_def) (old:tm) : tm :=
match old with
| OK  => OK
| ACT a => ACT a
| IF c t1 t2 => IF c (unfold new t1) (unfold new t2)
| SEQ l r => SEQ l (unfold new r)
| REC_DEF other_idef b =>
  match new with
  | (new_idef, b') =>
    if Nat.eqb new_idef other_idef
      then unfold (new_idef, b) b (* overwrite and continue *)
      else REC_DEF new_idef (unfold new b)
  end
| REC_CALL i =>
  match new with
  | (new_idef, b) =>
    if Nat.eqb new_idef i
      then (* unfold *) REC_DEF new_idef b
      else REC_CALL i  (* ignore, for some other def *)
  end
end.

Definition lock : Type := index.
Definition lock_initial : lock := index_initial.

Record qnode := { next   : index
                ; locked : bool }.

Definition qnode_initial : qnode := Build_qnode Nil false.
Definition qnode_set_next (i:index) (q:qnode) : qnode := Build_qnode i (locked q).
Definition qnode_set_locked (b:bool) (q:qnode) : qnode := Build_qnode (next q) b.

Fixpoint qnode_create (n:nat) : list qnode :=
match n with
| 0    => []
| S m => app (qnode_create m) [qnode_initial]
end.


Record mem := { mem_next : index
              ; mem_locked : bool
              ; mem_qnodes : list qnode }.

Definition mem_create (n:nat) : mem := Build_mem Nil false (qnode_create n).
Definition mem_set_next (i:index) (m:mem) : mem := Build_mem i (mem_locked m) (mem_qnodes m).
Definition mem_set_locked (b:bool) (m:mem) : mem := Build_mem (mem_next m) b (mem_qnodes m).
Definition mem_set_qnodes (q:list qnode) (m:mem) : mem := Build_mem (mem_next m) (mem_locked m) q.

Definition resource : Type := mem * lock.
Definition resource_create (n:nat) : resource := (mem_create n, lock_initial).
Definition resource_set_mem (m:mem) (r:resource) : resource := (m, snd r).
Definition resource_set_lock (l:lock) (r:resource) : resource := (fst r, l).

Definition env : Type := state * resource.
Definition env_create (n:nat) : env := (state_create n, resource_create n).

Inductive get_nth_qnode : (list qnode * nat) -> qnode -> Prop :=
| GET_THIS_QNODE : forall h tl, get_nth_qnode (h::tl, 0) h
| GET_NEXT_QNODE : forall h tl n q, 
  get_nth_qnode (tl, n) q -> get_nth_qnode (h::tl, S n) q.

Inductive set_nth_qnode : (list qnode * nat * qnode) -> list qnode -> Prop :=
| SET_THIS_QNODE : forall h tl q, set_nth_qnode (h::tl, 0, q) (q::tl)
| SET_NEXT_QNODE : forall h tl n qs q, 
  set_nth_qnode (tl, n, q) qs -> set_nth_qnode (h::tl, S n, q) qs.

Definition read_next (p:pid) (v:local_vars) (m:mem) (l:lock) (q:qnode) : env := 
((p, local_vars_set_next (next q) v), (mem_set_next (next q) m, l)).

Definition read_locked (p:pid) (v:local_vars) (m:mem) (l:lock) (q:qnode) : env := 
((p, local_vars_set_locked (locked q) v), (mem_set_locked (locked q) m, l)).

Definition fetch_and_store (p:pid) (v:local_vars) (m:mem) (l:lock) : env := 
((p, local_vars_set_predecessor l v), (m, index_of_pid p)).

Definition compare_and_swap (p:pid) (v:local_vars) (m:mem) (l:lock) : env :=
if index_eqb l (index_of_pid p) 
then ((p, local_vars_set_swap true v), (m, Nil))
else ((p, local_vars_set_swap false v), (m, l)).

Inductive do_action : (action * env) -> (tm * env) -> Prop :=
| DO_ENTER : forall x, do_action (ENTER, x) (OK, x)
| DO_LEAVE : forall x, do_action (LEAVE, x) (OK, x)
| DO_NCS : forall x, do_action (NCS, x) (OK, x)
| DO_READ_NEXT : forall p v m l q, get_nth_qnode (mem_qnodes m, p) q -> 
  do_action (READ_NEXT, ((p, v), (m, l))) (OK, read_next p v m l q) 
| DO_READ_LOCKED : forall p v m l q, get_nth_qnode (mem_qnodes m, p) q -> 
  do_action (READ_LOCKED, ((p, v), (m, l))) (OK, read_locked p v m l q) 
| DO_FETCH_AND_STORE : forall p v m l,
  do_action (FETCH_AND_STORE, ((p, v), (m, l))) (OK, fetch_and_store p v m l)
| DO_COMPARE_AND_SWAP : forall p v m l,
  do_action (COMPARE_AND_SWAP, ((p, v), (m, l))) (OK, compare_and_swap p v m l)
| DO_WRITE_NEXT : forall p v m l w x p' q k i q',
  get_pid ((p, v), w) p' ->
  get_nth_qnode (mem_qnodes m, p') q -> 
  get_value ((p, v), x) k ->
  value_to_index ((p, v), k) i ->
  set_nth_qnode (mem_qnodes m, p', qnode_set_next i q) q' -> 
  do_action (WRITE_NEXT w x, ((p, v), (m, l)))
    (OK, ((p, v), (mem_set_qnodes q' m, l)))
| DO_WRITE_LOCKED : forall p v m l w x p' q k b q',
  get_pid ((p, v), w) p' ->
  get_nth_qnode (mem_qnodes m, p') q -> 
  get_value ((p, v), x) k ->
  value_to_bool ((p, v), k) b ->
  set_nth_qnode (mem_qnodes m, p', qnode_set_locked b q) q' -> 
  do_action (WRITE_NEXT w x, ((p, v), (m, l)))
    (OK, ((p, v), (mem_set_qnodes q' m, l))).

Definition label : Type := action * pid.

Inductive label_opt : action * env -> option label -> Prop :=
| LABEL_ENTER : forall p v r, label_opt (ENTER, ((p, v), r)) (Some (ENTER, p))
| LABEL_LEAVE : forall p v r, label_opt (LEAVE, ((p, v), r)) (Some (LEAVE, p))
| LABEL_NCS : forall x, label_opt (NCS, x) None
| LABEL_READ_NEXT : forall x, label_opt (READ_NEXT, x) None
| LABEL_READ_LOCKED : forall x, label_opt (READ_LOCKED, x) None
| LABEL_WRITE_NEXT : forall x v k, label_opt (WRITE_NEXT v k, x) None
| LABEL_WRITE_LOCKED : forall x v k, label_opt (WRITE_LOCKED v k, x) None
| LABEL_FETCH_AND_STORE : forall x, label_opt (FETCH_AND_STORE, x) None
| LABEL_COMPARE_AND_SWAP : forall x, label_opt (COMPARE_AND_SWAP, x) None.

Definition process : Type := tm * env.
Definition process_create (n:nat) (x:tm) : process := (x, (env_create n)).

Inductive step : process -> option label -> process -> Prop :=
| STEP_ACT : forall a e l t, 
  label_opt (a, e) l -> do_action (a, e) t -> step (ACT a, e) l t
| STEP_SEQ_END : forall r e, step (SEQ OK r, e) None (r, e)
| STEP_SEQ : forall a l1 l2 r e1 e2,
  step (l1, e1) a (l2, e2) -> step (SEQ l1 r, e1) a (SEQ l2 r, e2)
| STEP_IF : forall c x y s r b,
  eval (s, c) (BOOL b) -> 
  step (IF c x y, (s, r)) None (if b then x else y, (s, r))
| STEP_REC_DEF : forall i b e, step (REC_DEF i b, e) None (unfold (i, b) b, e).

Definition worker : Type := tm * state.
Definition worker_create (n:nat) (x:tm) : worker := (x, (state_create n)).

Inductive sys : Type :=
| PRC : worker -> sys
| PAR : sys -> sys -> sys.

Fixpoint sys_create (n:nat) (x:tm) : sys :=
match n with
| 0 => PRC (worker_create n x)
| S m => PAR (PRC (worker_create n x)) (sys_create m x)
end.

Definition composition : Type := sys * resource.
Definition composition_create (n:nat) (x:tm) : composition := (sys_create n x, resource_create n).

Inductive lts : composition -> option label -> composition -> Prop :=
| LTS_PRC : forall a t1 t2 s1 s2 r1 r2,
  step (t1, (s1, r1)) a (t2, (s2, r2)) -> lts (PRC (t1, s1), r1) a (PRC (t2, s2), r2)
| LTS_PAR_L : forall a l1 l2 r gr1 gr2,
  lts (l1, gr1) a (l2, gr2) -> lts (PAR l1 r, gr1) a (PAR l2 r, gr2)
| LTS_PAR_R : forall a l r1 r2 gr1 gr2,
  lts (r1, gr1) a (r2, gr2) -> lts (PAR l r1, gr1) a (PAR l r2, gr2).


(* Require Import MEBI.Examples.CADP. *)

Module Protocol.


Example AcquireInnerDef : idef := 2.
Example AcquireInnerBody : tm :=
  SEQ 
    (ACT READ_LOCKED) 
    (IF (VAR LOCKED) 
      (REC_CALL (AcquireInnerDef)) 
      (OK)).

Example AcquireInnerLoop : tm := 
  REC_DEF (AcquireInnerDef) (AcquireInnerBody).

Example Acquire : tm :=
  SEQ 
    (ACT (WRITE_NEXT THE_PID NIL)) 
    (SEQ 
      (ACT FETCH_AND_STORE) 
      (IF (EQ (VAR PREDECESSOR) (VAL NIL)) 
        (OK) 
        (SEQ 
          (ACT (WRITE_LOCKED THE_PID (BOOL true))) 
          (SEQ 
            (ACT (WRITE_NEXT PREDECESSOR (GET THE_PID))) 
            (AcquireInnerLoop))))).

Example ReleaseInnerDef : idef := 1.
Example ReleaseInnerBody : tm :=
  SEQ 
    (ACT READ_NEXT) 
    (IF (EQ (VAL NIL) (VAR NEXT))
      (REC_CALL (ReleaseInnerDef))
      (ACT (WRITE_LOCKED NEXT (BOOL false)))).

Example ReleaseInnerLoop : tm := 
  REC_DEF (ReleaseInnerDef) (ReleaseInnerBody).

Example Release : tm :=
  SEQ 
    (ACT READ_NEXT) 
    (IF (EQ (VAL NIL) (VAR NEXT)) 
      (SEQ 
        (ACT COMPARE_AND_SWAP) 
        (IF (EQ FLS (VAR SWAP)) 
          (ReleaseInnerLoop) 
          (OK))) 
      (ACT (WRITE_LOCKED NEXT (BOOL false)))).

Example PMainLoopDef : idef := 0.
Example P : tm :=
  REC_DEF (PMainLoopDef) (
    (* SEQ (ACT NCS) ( *)
    SEQ (Acquire) (
    SEQ (ACT ENTER) (
    SEQ (ACT LEAVE) (
    SEQ (Release) (
    REC_CALL (PMainLoopDef)))))(* ) *)).


End Protocol.

Example c1 : composition := composition_create 1 Protocol.P.
Example c2 : composition := composition_create 2 Protocol.P.

Require Import MEBI.Bisimilarity.
Require Import MEBI.loader.


MeBi Config Output "Debug" True.
MeBi Config Output "Info" True.
MeBi Config Output "Notice" True.
MeBi Config Output "Warning" True.
MeBi Config Output "Error" True.
MeBi Config Output "Trace" True.
MeBi Config Output "Result" True.
MeBi Config Output "Show" True.
MeBi Config Output "DecodeResults" True.
MeBi Config Output "DumpResults" True.

MeBi Run FSM c1 Using lts step.
(* MeBi Run FSM c2 Using lts step. *)
