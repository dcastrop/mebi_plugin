
From Coq Require Export String.

(* following Figure 1: https://doi.org/10.1007/s10009-012-0244-z *)

Definition index : Type := nat.

Definition pid : Type := option index.

Definition Nil : option index := None.

Definition iloop : Type := index.

Definition ibool : Type := index.
Fixpoint index_eq (t1 t2:index) : ibool :=
  match t1, t2 with
  | 0, 0 => 1
  | S n1, S n2 => index_eq n1 n2
  | _, _ => 0
  end.

Definition Index (p:pid) : index :=
  match p with
  | None => 0
  | Some n => n
  end.

Definition Pid (i:index) : pid :=
  match i with
  | 0 => None
  | n => Some n
  end.

Definition Bool (i:index) : option bool :=
  match i with
  | 0 => Some false
  | 1 => Some true
  | _ => None
  end.

(* used in steps, not syntax *)
Inductive action : Type :=
  (* channel CS_Access *)
  | NCS   : pid -> action
  | ENTER : pid -> action
  | LEAVE : pid -> action

  (* channel Memory_Access *)
  | READ_NEXT    : pid -> action (* M, binds ?next:index *)
  | READ_LOCKED  : pid -> action (* M, binds ?locked:bool *)
  | WRITE_NEXT   : pid -> index -> action
  | WRITE_LOCKED : pid -> ibool -> action

  (* channel Lock_Access *)
  | FETCH_AND_STORE  : index -> action (* L, binds predecessor -> i *)
  | COMPARE_AND_SWAP : index -> action (* L, binds: swap -> i==j *)

  (* silent action *)
  | TAU   : action
  .

Definition silent : action := TAU.


Inductive tm : Type :=
  | TERM  : tm                       (* termination *)
  | OK    : tm                      (* no-op *)

  | IF    : tm -> tm -> tm -> tm    (* condition -> if true -> if false -> ... *)

  (* CS_access *)
  | ACT_NCS   : tm -> tm
  | ACT_ENTER : tm -> tm
  | ACT_LEAVE : tm -> tm

  (* Lock_access *)
  | ACT_FETCH_AND_STORE  : tm -> tm
  | ACT_COMPARE_AND_SWAP : tm -> tm

  (* Memory_access *)
  | ACT_READ_NEXT    : tm -> tm
  | ACT_READ_LOCKED  : tm -> tm
  | ACT_WRITE_NEXT   : tm -> tm
  | ACT_WRITE_LOCKED : tm -> tm

  | LOOP  : option iloop -> tm -> tm -> tm  (* optional loop id -> body -> outer continuation -> ... *)
  | BREAK : iloop -> tm                     (* loop id -> ... *)

  | TRU   : tm        (* true *)
  | FLS   : tm        (* false *)

  (* | ZRO : tm
  | SCC : tm -> tm *)
  .

(* following: https://softwarefoundations.cis.upenn.edu/plf-current/Types.html *)

Declare Custom Entry tm.
Declare Scope tm_scope.
Notation "<{ e }>" := e (e custom tm at level 99): tm_scope.
Notation "( x )" := x (in custom tm, x at level 99): tm_scope.
Notation "x" := x (in custom tm at level 0, x constr at level 0): tm_scope.
(* Notation "'0'" := (ZRO) (in custom tm at level 0): tm_scope.
Notation "'0'" := 0 (at level 1): tm_scope. *)
(* Notation "'succ' x" := (SCC x) (in custom tm at level 90, x custom tm at level 80): tm_scope. *)
Notation "'if' c 'then' t 'else' e" := (IF c t e)
                 (in custom tm at level 90, c custom tm at level 80,
                  t custom tm at level 80, e custom tm at level 80): tm_scope.
Local Open Scope tm_scope.


Notation "x :: l" := (cons x l) (at level 60, right associativity).
Notation "[ ]" := nil.
Notation "[ x ; .. ; y ]" := (cons x .. (cons y nil) ..).

(* Inductive bvalue : tm -> Prop :=
  | bv_true : bvalue <{ TRU }>
  | bv_false : bvalue <{ FLS }>.

Inductive nvalue : tm -> Prop :=
  | nv_0 : nvalue <{ 0 }>
  | nv_succ : forall t, nvalue t -> nvalue <{ succ t }>.

Definition value (t : tm) := bvalue t \/ nvalue t.

Hint Constructors bvalue nvalue : core.
Hint Unfold value : core. *)

Record qnode := { next   : index
                ; locked : ibool }.

Definition mem : Type := list qnode.

Definition lock : Type := index * index * index. (* i, new_i, j *)

Record vars := { acquire_predecessor : option index
               ; acquire_locked      : option ibool
               ; release_next        : option index
               ; release_swap        : option ibool }.

Definition state : Type := pid * vars * mem * lock.

Definition get_pid  (s:state) : pid  := match s with | (pid', _vars, _mem, _lock) => pid'  end.
Definition get_vars (s:state) : vars := match s with | (_pid, vars', _mem, _lock) => vars' end.
Definition get_mem  (s:state) : mem  := match s with | (_pid, _vars, mem', _lock) => mem'  end.
Definition get_lock (s:state) : lock := match s with | (_pid, _vars, _mem, lock') => lock' end.

(* mem get/set *)
Fixpoint get_index_of (i:nat) (m:mem) : option qnode :=
  match m with
  | [] => None
  | h :: t => match i with
              | 0 => Some h
              | S n => get_index_of n t
              end end.

Definition get_mem_qnode_next (i:index) (s:state) : index :=
  match get_index_of i (get_mem s) with
  | None => 0
  | Some n => (next n)
  end.


(* lock get/set *)
Definition get_lock_i     (s:state) : index := match get_lock s with | (i, _new_i, _j) => i     end.
Definition get_lock_new_i (s:state) : index := match get_lock s with | (_i, new_i, _j) => new_i end.
Definition get_lock_j     (s:state) : index := match get_lock s with | (_i, _new_i, j) => j     end.

Definition set_lock_i (i:index) (s:state) : state :=
  match s with
  | (_pid, _vars, _mem, lock') => match lock' with | (_i, new_i, j) => (_pid, _vars, _mem, (i, new_i, j))
  end end.

Definition set_lock_new_i (new_i:index) (s:state) : state :=
  match s with
  | (_pid, _vars, _mem, lock') => match lock' with | (i, _new_i, j) => (_pid, _vars, _mem, (i, new_i, j))
  end end.

Definition set_lock_j (j:index) (s:state) : state :=
  match s with
  | (_pid, _vars, _mem, lock') => match lock' with | (i, new_i, _j) => (_pid, _vars, _mem, (i, new_i, j))
  end end.

(* bind vars *)
Definition get_acquire_predecessor (s:state) : option index := acquire_predecessor (get_vars s).
Definition get_acquire_locked      (s:state) : option ibool := acquire_locked      (get_vars s).
Definition get_release_next        (s:state) : option index := release_next        (get_vars s).
Definition get_release_swap        (s:state) : option ibool := release_swap        (get_vars s).

Definition bind_predecessor (p:index) (s:state) : state :=
  match s with
  | (_pid, vars', _mem, _lock) => (_pid, Build_vars (Some p) (acquire_locked vars') (release_next vars') (release_swap vars'), _mem, _lock)
  end.

Definition bind_locked (p:index) (s:state) : state :=
  match s with
  | (_pid, vars', _mem, _lock) => (_pid, Build_vars (acquire_predecessor vars') (Some p) (release_next vars') (release_swap vars'), _mem, _lock)
  end.

Definition bind_next (p:index) (s:state) : state :=
  match s with
  | (_pid, vars', _mem, _lock) => (_pid, Build_vars (acquire_predecessor vars') (acquire_locked vars') (Some p) (release_swap vars'), _mem, _lock)
  end.

Definition bind_swap (p:index) (s:state) : state :=
  match s with
  | (_pid, vars', _mem, _lock) => (_pid, Build_vars (acquire_predecessor vars') (acquire_locked vars') (release_next vars') (Some p), _mem, _lock)
  end.

(* initial *)
Definition Initial_mem   : mem   := [].
Definition Initial_lock  : lock  := (0, 0, 0).
Definition Initial_vars  : vars  := Build_vars None None None None.
Definition Initial_state : state := (Nil, Initial_vars, Initial_mem, Initial_lock).

Definition res_fetch_and_store  (s:state) : state := set_lock_i (Index (get_pid s)) (bind_predecessor (get_lock_i s) s).
Definition res_compare_and_swap (s:state) : state := bind_swap (index_eq (get_lock_i s) (get_lock_j s)) s.

(* TODO: check if [get_mem_qnode_next] should return 0 if pid of s not in mem yet. *)
Definition res_read_next              (s:state) : state := bind_next (get_mem_qnode_next (Index (get_pid s)) s) s.
Definition res_read_locked            (s:state) : state := s. (* TODO: *)
Definition res_write_next             (s:state) : state := s. (* TODO: *)
Definition res_write_locked (l:ibool) (s:state) : state := s. (* TODO: *)

Definition act_fetch_and_store        (s:state) : action := FETCH_AND_STORE (Index (get_pid s)).
Definition act_compare_and_swap       (s:state) : action := COMPARE_AND_SWAP (Index (get_pid s)).
Definition act_read_next              (s:state) : action := READ_NEXT (get_pid s).
Definition act_read_locked            (s:state) : action := READ_LOCKED (get_pid s).
Definition act_write_next             (s:state) : action := WRITE_NEXT (get_acquire_predecessor s) (Index (get_pid s)).
Definition act_write_locked (l:ibool) (s:state) : action := WRITE_LOCKED (get_pid s) l.

Reserved Notation "t '--<{' a '}>-->' t'" (at level 40).

Inductive step : (tm * state) -> action -> (tm * state) -> Prop :=
  | ST_IF_TT : forall t1 t2 s, (<{ if TRU then t1 else t2 }>, s) --<{silent}>--> (t1, s)
  | ST_IF_FF : forall t1 t2 s, (<{ if FLS then t1 else t2 }>, s) --<{silent}>--> (t2, s)

  | ST_IF    : forall c1 c2 t1 t2 s1 s2,
                (c1, s1) --<{silent}>--> (c2, s2) ->
                  (<{ if c1 then t1 else t2 }>, s1) --<{silent}>--> (<{ if c2 then t1 else t2 }>, s2)

  | ST_NCS   : forall t s1 s2, (ACT_NCS   t, s1) --<{NCS   (get_pid s1)}>--> (t, s2)
  | ST_ENTER : forall t s1 s2, (ACT_ENTER t, s1) --<{ENTER (get_pid s1)}>--> (t, s2)
  | ST_LEAVE : forall t s1 s2, (ACT_LEAVE t, s1) --<{LEAVE (get_pid s1)}>--> (t, s2)

  | ST_FETCH_AND_STORE : forall t s,
    (OK, s) --<{act_fetch_and_store s}>--> (OK, res_fetch_and_store s) ->
      (ACT_FETCH_AND_STORE t, s) --<{silent}>--> (t, res_fetch_and_store s)

  | ST_COMPARE_AND_SWAP : forall t s,
    (OK, s) --<{act_compare_and_swap s}>--> (OK, res_compare_and_swap s) ->
      (ACT_COMPARE_AND_SWAP t, s) --<{silent}>--> (t, res_compare_and_swap s)

  | ST_READ_NEXT : forall t s,
    (OK, s) --<{act_read_next s}>--> (OK, res_read_next s) ->
      (ACT_READ_NEXT t, s) --<{silent}>--> (t, res_read_next s)

  | ST_READ_LOCKED : forall t s,
    (OK, s) --<{act_read_locked s}>--> (OK, res_read_locked s) ->
      (ACT_READ_LOCKED t, s) --<{silent}>--> (t, res_read_locked s)

  | ST_WRITE_NEXT : forall t s,
    (OK, s) --<{act_write_next s}>--> (OK, res_write_next s) ->
      (ACT_WRITE_NEXT t, s) --<{silent}>--> (t, res_write_next s)

  | ST_WRITE_LOCKED : forall l t s,
    (OK, s) --<{act_write_locked l s}>--> (OK, res_write_locked l s) ->
      (ACT_WRITE_LOCKED t, s) --<{silent}>--> (t, res_write_locked l s)

  where "t '--<{' a '}>-->' t'" := (step t a t').

Print step.
