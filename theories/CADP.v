Require Import MEBI.loader.

Require Export String.
Require Import PeanoNat.
Require Import Notations.
Require Export Bool.



(* following Figure 1: https://doi.org/10.1007/s10009-012-0244-z *)

Definition index : Type := nat.
Definition pid   : Type := option index.
Definition ibool : Type := index.
Definition iloop : Type := index.

Definition Nil   : option index := None.

Definition Initial_index : index := 0.
Definition Initial_pid   : pid   := Nil.
Definition Initial_ibool : ibool := 0.

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

Definition IBool (b:bool) : ibool := if b then 1 else 0.

Inductive var : Type :=
  | PREDECESSOR
  | LOCKED
  | NEXT
  | SWAP
  | PID
  | NIL
  .

Inductive val : Type :=
  | VAR : var -> val
  | TRU : val
  | FLS : val
  .

(* used in steps, not syntax *)
Inductive act : Type :=
  | NCS   : act
  | ENTER : act
  | LEAVE : act

  | READ_NEXT   : act
  | READ_LOCKED : act

  | WRITE_NEXT   : var -> val -> act
  | WRITE_LOCKED : var -> val -> act

  | FETCH_AND_STORE  : act
  | COMPARE_AND_SWAP : act
  .


Inductive tm : Type :=
  | TYPE_ERR : tm
  | OK    : tm                      (* no-op *)

  | ACT : act -> tm -> tm

  | SEQ : tm -> tm -> tm

  | IF    : tm -> tm -> tm -> tm    (* condition -> if true -> if false -> ... *)

  | VAL    : val  -> tm
  | NOT    : tm   -> tm
  | EQ_N   : nat  -> nat -> tm
  | EQ_B   : bool -> bool -> tm
  | IS_TRU : tm   -> tm
  | IS_NIL : tm   -> tm

  | TT   : tm        (* true *)
  | FF   : tm        (* false *)

  | LOOP      : tm -> tm (* inner loop body -> *)
  | LOOP_END  : tm

  | LOOP_OVER : iloop -> tm -> tm -> tm (* loop id -> body -> outer continuation -> ... *)
  | BREAK     : iloop -> tm             (* loop id -> ... *)
  .

(* following: https://softwarefoundations.cis.upenn.edu/plf-current/Types.html *)

Declare Custom Entry tm.
Declare Scope tm_scope.
Notation "<{ e }>" := e (e custom tm at level 99): tm_scope.
Notation "( x )" := x (in custom tm, x at level 99): tm_scope.
Notation "x" := x (in custom tm at level 0, x constr at level 0): tm_scope.
Notation "'if' c 'then' t 'else' e" := (IF c t e)
                 (in custom tm at level 90, c custom tm at level 80,
                  t custom tm at level 80, e custom tm at level 80): tm_scope.
Local Open Scope tm_scope.



Record qnode := { next   : index
                ; locked : ibool }.

Definition mem : Type := list qnode.

Definition InitialQnode : qnode := Build_qnode 0 0.

(* cant seem to import the following on my pc (vscoq) *)
Notation "x :: l" := (cons x l) (at level 60, right associativity).
Notation "[ ]" := nil.
Notation "[ x ; .. ; y ]" := (cons x .. (cons y nil) ..).

Fixpoint mem_app (l1 l2 : mem) : mem :=
  match l1 with
  | nil    => l2
  | h :: t => h :: (mem_app t l2)
  end.

Notation "x ++ y" := (app x y)
                     (right associativity, at level 60).

Fixpoint build_mem (n:nat) : mem :=
  match n with
  | 0 => []
  | S n' => mem_app (build_mem n') [InitialQnode]
  end.

(* we dont need new_i, only i and j *)
Definition lock : Type := index * index.

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
Fixpoint length_of_mem (m:mem) : nat :=
  match m with
  | [] => 0
  | h :: t => S (length_of_mem t)
  end.

Fixpoint repeat_qnode (n:nat) (m:mem) :=
  match n with
    | O  => m
    | S n' => InitialQnode::(repeat_qnode n' m)
  end.

(* pads mem so that index i exists *)
Definition mem_check (i:nat) (m:mem) : mem :=
  if (Nat.ltb (length_of_mem m) i)
  then (repeat_qnode i m)
  else m.

Fixpoint get_index_of (i:nat) (m:mem) : option qnode :=
  match m with
  | [] => None
  | h :: t => match i, (length_of_mem m) with
              | _, 0 => None
              | _, _ => if (Nat.eqb i (length_of_mem m)) then Some h else get_index_of i t
              end end.

Definition get_mem_qnode_next (i:index) (s:state) : index :=
  match get_index_of i (get_mem s) with
  | None => 0
  | Some n => (next n)
  end.

Definition get_mem_qnode_locked (i:index) (s:state) : ibool :=
  match get_index_of i (get_mem s) with
  | None => 0
  | Some n => (locked n)
  end.

Fixpoint set_index_qnode_next_of (qnode_index:index) (new_next:index) (m:mem) : mem :=
  match m with
  | [] => m (* shouldn't happen*)
  | h :: t =>
      if (Nat.eqb qnode_index (length_of_mem m))
      then Build_qnode new_next (locked h) :: t
      else h :: set_index_qnode_next_of qnode_index new_next t
  end.

Definition set_mem_qnode_next (index_of_qnode:option index) (new_next:index) (s:state) : state :=
  match index_of_qnode with
  | None => s (* will never happen, guarded by [predecessor!=nil] *)
  | Some qnode_index =>
    match s with
    | (_pid, _vars, mem', _lock) =>
      (_pid, _vars, set_index_qnode_next_of qnode_index new_next (mem_check qnode_index mem'), _lock)
    end
  end.


Fixpoint set_index_qnode_locked_of (qnode_index:index) (new_locked:ibool) (m:mem) : mem :=
  match m with
  | [] => m (* shouldn't happen*)
  | h :: t =>
    if (Nat.eqb qnode_index (length_of_mem m))
    then Build_qnode (next h) new_locked :: t
    else (h :: set_index_qnode_locked_of qnode_index new_locked t)
    end.

Definition set_mem_qnode_locked (index_of_qnode:option index) (new_locked:ibool) (s:state) : state :=
  match index_of_qnode with
  | None => s (* should never happen *)
  | Some qnode_index =>
    match s with
    | (_pid, _vars, mem', _lock) =>
        (_pid, _vars, set_index_qnode_locked_of qnode_index new_locked (mem_check qnode_index mem'), _lock)
    end
  end.




(* lock get/set *)
Definition get_lock_i     (s:state) : index := match get_lock s with | (i,  _j) => i     end.
Definition get_lock_j     (s:state) : index := match get_lock s with | (_i,  j) => j     end.

Definition set_lock_i (i:index) (s:state) : state :=
  match s with
  | (_pid, _vars, _mem, lock') => match lock' with | (_i, j) => (_pid, _vars, _mem, (i, j))
  end end.

Definition set_lock_j (j:index) (s:state) : state :=
  match s with
  | (_pid, _vars, _mem, lock') => match lock' with | (i, _j) => (_pid, _vars, _mem, (i, j))
  end end.


(* bind vars *)
Definition get_var (v:var) (s:state) : option index :=
  match v with
  | PREDECESSOR => acquire_predecessor (get_vars s)
  | LOCKED      => acquire_locked      (get_vars s)
  | NEXT        => release_next        (get_vars s)
  | SWAP        => release_swap        (get_vars s)
  | PID         => Some (Index (get_pid s))
  | NIL         => None
  end.

Definition get_val (v:val) (s:state) : index :=
  match v with
  | VAR r => Index (get_var r s)
  | TRU   => 1
  | FLS   => 0
  end.



Definition tmBool (b:bool) : tm := if true then TT else FF.
Definition tmIBool (b:ibool) : tm := match b with | 0 => FF | 1 => TT | _ => TYPE_ERR end.

Definition IsBool (v:var) : bool :=
  match v with
  | LOCKED => true
  | SWAP => true
  | _ => false
  end.

Definition IsIndex (v:var) : bool :=
  match v with
  | PREDECESSOR => true
  | NEXT => true
  | _ => false
  end.

Definition IsSome (o:option nat) : bool :=
  match o with
  | None => false
  | Some _ => true
  end.



Definition tmIsNil (o:option nat) : tm := match o with | None => VAL TRU | _ => VAL FLS end.

Definition tmIsTru (o:option nat) : tm := match o with | None => TYPE_ERR | Some b => tmIBool b end.

Definition bind_predecessor (p:index) (s:state) : state :=
  match s with
  | (_pid, vars', _mem, _lock) =>
    let vars'' := Build_vars (Some p) (acquire_locked vars') (release_next vars') (release_swap vars') in
    (_pid, vars'', _mem, _lock)
  end.

Definition bind_locked (p:index) (s:state) : state :=
  match s with
  | (_pid, vars', _mem, _lock) =>
    let vars'' := Build_vars (acquire_predecessor vars') (Some p) (release_next vars') (release_swap vars') in
    (_pid, vars'', _mem, _lock)
  end.

Definition bind_next (p:index) (s:state) : state :=
  match s with
  | (_pid, vars', _mem, _lock) =>
    let vars'' := Build_vars (acquire_predecessor vars') (acquire_locked vars') (Some p) (release_swap vars') in
    (_pid, vars'', _mem, _lock)
  end.

Definition bind_swap (p:index) (s:state) : state :=
  match s with
  | (_pid, vars', _mem, _lock) =>
    let vars'' := Build_vars (acquire_predecessor vars') (acquire_locked vars') (release_next vars') (Some p) in
    (_pid, vars'', _mem, _lock)
  end.


(* initial *)
Definition Initial_mem   (n:nat) : mem   := build_mem n.
Definition Initial_lock          : lock  := (0, 0).
Definition Initial_vars          : vars  := Build_vars None None None None.
Definition Initial_state (n:nat) : state := (Nil, Initial_vars, Initial_mem n, Initial_lock).



(* resulting term *)
Definition tm_is_nil (v:val) (s:state) : tm :=
  match v with
  | VAR v' => if IsIndex v'
              then tmIsNil (get_var v' s)
              else TYPE_ERR
  | TRU => FF
  | FLS => FF
  end.

Definition tm_is_tru (v:val) (s:state) : tm :=
  match v with
  | VAR v' => if IsBool v'
              then tmIsTru (get_var v' s)
              else TYPE_ERR
  | TRU => TT
  | FLS => FF
  end.



(* substitution *)
Fixpoint subst (new old:tm) (loops:bool) : tm :=
  match old with
  | TYPE_ERR => TYPE_ERR
  | OK   => OK

  | IF c t e => IF c (subst new t loops) (subst new e loops)


  | VAL t => VAL t
  | NOT t      => NOT t
  | EQ_N a b   => EQ_N a b
  | EQ_B a b   => EQ_B a b
  | IS_TRU a   => IS_TRU a
  | IS_NIL t   => IS_NIL t
  (* | VAR v      => VAR v *)

  | TT => TT
  | FF => FF

  | ACT a t => ACT a (subst new t loops)

  | SEQ l r => SEQ l (subst new r loops) (* do not substitute in other body *)

  | LOOP b   => if loops then LOOP (subst new b loops) else LOOP b
  | LOOP_END => new

  | LOOP_OVER l b c => LOOP_OVER l (subst new b loops) (subst new c loops)
  | BREAK l         => BREAK l
  end.

(* handle resulting terms *)


Inductive action : Type := | SILENT : action | LABEL : act -> pid -> action.

Definition step_state (a:act) (s:state) : state :=
  match a with
  (* CS Access *)
  | NCS   => s
  | ENTER => s
  | LEAVE => s

  (* Memory Access *)
  | READ_NEXT    => bind_next   (get_mem_qnode_next   (Index (get_pid s)) s) s
  | READ_LOCKED  => bind_locked (get_mem_qnode_locked (Index (get_pid s)) s) s

  | WRITE_NEXT   pid_to_write next_pid  =>
                    set_mem_qnode_next   (get_var pid_to_write s) (get_val next_pid  s) s

  | WRITE_LOCKED pid_to_write is_locked =>
                    set_mem_qnode_locked (get_var pid_to_write s) (get_val is_locked s) s

  (* Lock Access *)
  | FETCH_AND_STORE  => set_lock_i (Index (get_pid s)) (bind_predecessor (get_lock_i s) s)
  | COMPARE_AND_SWAP => let s1 := set_lock_j (Index (get_pid s)) s in
                        let s2 := bind_swap (index_eq (get_lock_i s1) (get_lock_j s1)) s1 in
                        let s3 := set_lock_i 0 s2 in s3
  end.


Reserved Notation "t '--<{' a '}>-->' t'" (at level 40).

Inductive step : (tm * state) -> action -> (tm * state) -> Prop :=

  | ST_VAL_TRU : forall s, (VAL TRU, s) --<{SILENT}>--> (TT, s)
  | ST_VAL_FLS : forall s, (VAL FLS, s) --<{SILENT}>--> (FF, s)

  | ST_NOT_TT : forall s, (NOT TT, s) --<{SILENT}>--> (FF, s)
  | ST_NOT_FF : forall s, (NOT FF, s) --<{SILENT}>--> (TT, s)

  | ST_EQ_N : forall a b s, (EQ_N a b, s) --<{SILENT}>--> (tmBool (Nat.eqb a b), s)
  | ST_EQ_B : forall a b s, (EQ_B a b, s) --<{SILENT}>--> (tmBool (eqb a b), s)

  | ST_IS_NIL : forall v s,
    (IS_NIL (VAL v), s) --<{SILENT}>--> (tm_is_nil v s, s)

  | ST_IS_TRU : forall v s,
    (IS_TRU (VAL v), s) --<{SILENT}>--> (tm_is_tru v s, s)

  | ST_ACT : forall a t s, (ACT a t, s) --<{LABEL a (get_pid s)}>--> (t, step_state a s)

  | ST_IF_TT : forall t1 t2 s, (<{ if TT then t1 else t2 }>, s) --<{SILENT}>--> (t1, s)
  | ST_IF_FF : forall t1 t2 s, (<{ if FF then t1 else t2 }>, s) --<{SILENT}>--> (t2, s)

  | ST_IF    : forall a c1 c2 t1 t2 s,
    (c1, s) --<{a}>--> (c2, s) ->
      (<{ if c1 then t1 else t2 }>, s) --<{SILENT}>--> (<{ if c2 then t1 else t2 }>, s)

  | ST_SEQ_END : forall r s, (SEQ OK r, s) --<{SILENT}>--> (r, s)
  | ST_SEQ     : forall a l1 l2 r s1 s2,
    (l1, s1) --<{a}>--> (l2, s2) ->
      (SEQ l1 r, s1) --<{a}>--> (SEQ l2 r, s2)

  | ST_LOOP  : forall t s, (LOOP t, s) --<{SILENT}>--> (subst t LOOP_END false, s)
  | ST_BREAK : forall l c s, (LOOP_OVER l (BREAK l) c, s) --<{SILENT}>--> (c, s)

  | ST_LOOP_OVER : forall a l t1 t2 c s1 s2,
    (LOOP t1, s1) --<{a}>--> (LOOP t2, s2) ->
      (LOOP_OVER l t1 c, s1) --<{a}>--> (LOOP_OVER l t2 c, s2)

  where "t '--<{' a '}>-->' t'" := (step t a t').


Print step.

Reserved Notation "t '==<{' a '}>==>' t'" (at level 40).

Inductive sys : Type :=
  | PRC : tm -> sys
  | PAR : sys -> sys -> sys
  .

(* all tm must share the state, pass upwards to step. *)
Inductive lts : sys * state -> action -> sys * state -> Prop :=

  | LTS_PRC : forall a t1 t2 s1 s2,
    (t1, s1) --<{a}>--> (t2, s2) ->
      (PRC t1, s1) ==<{a}>==> (PRC t2, s2)

  | LTS_TERM_L : forall a r s, (PAR (PRC OK) r, s) ==<{a}>==> (r, s)
  | LTS_TERM_R : forall a l s, (PAR l (PRC OK), s) ==<{a}>==> (l, s)

  | LTS_PAR_L : forall a l1 l2 r s1 s2,
    (l1, s1) ==<{a}>==> (l2, s2) ->
      (PAR l1 r, s1) ==<{a}>==> (PAR l2 r, s2)

  | LTS_PAR_R : forall a l r1 r2 s1 s2,
    (r1, s1) ==<{a}>==> (r2, s2) ->
      (PAR l r1, s1) ==<{a}>==> (PAR l r2, s2)


  where "t '==<{' a '}>==>' t'" := (lts t a t').

Print lts.


(*******************)
(**** Example ******)
(*******************)

Example Acquire : tm :=
  ACT (WRITE_NEXT PID (VAR NIL)) (
    ACT FETCH_AND_STORE (
      IF (NOT (IS_NIL (VAL (VAR PREDECESSOR)))) (
        ACT (WRITE_LOCKED PID TRU) (
          ACT (WRITE_NEXT PREDECESSOR (VAR PID)) (
            LOOP_OVER 0 (*L*) (
              ACT READ_LOCKED (
                IF (NOT (VAL (VAR LOCKED))) (
                  BREAK 0 (*L*)
                ) (OK)
              )
            ) (OK)
          )
        )
      ) (OK)
    )
  ).

Example Release : tm :=
  ACT READ_NEXT (
    IF (IS_NIL (VAL (VAR NEXT))) (
      ACT COMPARE_AND_SWAP (
        IF (NOT (IS_TRU (VAL (VAR SWAP)))) (
          LOOP_OVER 0 (*L*) (
            ACT READ_NEXT (
              IF (NOT (IS_NIL (VAL (VAR NEXT)))) (
                BREAK 0 (*L*)
              ) (OK)
            )
          ) (
            ACT (WRITE_LOCKED NEXT TRU) (OK)
          )
        ) (OK)
      )
    ) (
      ACT (WRITE_LOCKED NEXT FLS) (OK)
    )
  ).

Example P : tm :=
  LOOP (
    ACT NCS (
      SEQ Acquire (
        ACT ENTER (
          ACT LEAVE (
            SEQ Release (OK)
          )
        )
      )
    )
  ).

Print P.

Example config : tm * state := (P, Initial_state 1).
Print config.

Example sys_config : sys * state := (PRC P, Initial_state 1).
Print sys_config.

MeBi LTS step config.
MeBi LTS lts sys_config.
