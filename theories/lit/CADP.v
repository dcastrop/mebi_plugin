
From Coq Require Export String.

(* following Figure 1: https://doi.org/10.1007/s10009-012-0244-z *)

(* interval type *)
Definition index : Type := nat.

Definition Nil : index := 0.

(* predicate type *)
(* Definition pid : Type := {n:index | ~(Nil=n)}. *)
Definition pid := {n:index | ~(Nil=n)}.
(* Inductive pid' (i:index) : index -> Prop := | pid : {n:index | ~(Nil=n)}.  *)

Record qnode := {
  q_next : index;
  q_locked : bool
}.

Definition memory := list qnode.

(* enumerated type *)
Inductive operation :=
  | READ_NEXT | READ_LOCKED
  | WRITE_NEXT | WRITE_LOCKED
  | FETCH_AND_STORE
  | COMPARE_AND_SWAP
  .

Inductive cs_access :=
  | CHANNEL (p:pid)
  .

Inductive memory_access :=
  (* | ma_next : operation*pid*index*pid -> memory_access *)
  (* | ma_next : (op:operation) (p:pid) (next_index:index) (q:pid) *)
  (* | ma_locked : (op:operation) (p:pid) (is_locked:bool) (q:pid) *)
  | MEMACC_NEXT (op:operation) (p:pid) (next_index:index) (q:pid)
  | MEMACC_LOCKED (op:operation) (p:pid) (is_locked:bool) (q:pid)
  .

Print memory_access.

Inductive lock_access :=
  | LOCKACC_NEXT (op:operation) (p:pid) (next_index:index) (q:pid)
  | LOCKACC_LOCKED (op:operation) (p:pid) (is_locked:bool) (q:pid)
  .

Inductive access_operation :=
  | MEMORY : memory_access -> access_operation
  | LOCK : lock_access -> access_operation
  .

Inductive iterable :=
  | ITER_LOCK_ACCESS : lock_access -> iterable
  .

Inductive typable :=
  | TYPE_LOCK_ACCESS : lock_access -> typable
  .

Inductive var_def :=
  | VAR (name:string) (type:typable)
  .

Inductive term :=
  (*  *)
  | TERM
  (*  *)
  | OP (op:access_operation)
  (*  *)
  | PAR (lhs:term) (rhs:term)
  | PARS (bodies:list term)
  (*  *)
  | DEF (def:var_def)
  | DEFs (defs:list var_def)
  (*  *)
  | LOOP (body:term)
  | LOOP_OVER (over:iterable) (body:term)
  (*  *)
  | IF_THEN (condition:Prop) (do_then:term)
  | IF_THEN_ELSE (condition:Prop) (do_then:term) (do_else:term)
  (*  *)
  (* | SELECT () *)
  .

Inductive sequence :=
  | SEQ (terms:list term)
  .

Inductive process_params :=
  | PARAMS (p_options:option cs_access) (l:lock_access) (m:memory_access)
  .

Inductive body :=
  | BODY (body:sequence)
  .

Inductive process :=
  | PROCESS (name:string) (pid:pid) (body:body)
  .

(* Example acquire := PROCESS "acquire" (2) (BODY (SEQ (TERM :: nil))). *)















(*
(* Inductive Pid (n:index) : pid -> Prop :=
  | n. *)

Print index.
Print pid.

Check (forall (n:index), ~(Nil=n) -> pid).
Check (forall (n:index), ~(Nil=n) -> pid n).



(* Check Pid 0. *)
Check Pid 1.
Check Pid 2.


(* Definition Pid : Type := forall (pid:Index), ~(Nil = pid). *)


(* Inductive Pid
  (pid:nat:=forall (p:nat), _ -> ~(Nil = p)) : Type := pid. *)

Definition Pid : Index := forall pid:Index, ~(Nil = pid) -> pid.

(* Inductive Pid : Type :=
  | one : Pid
  | suc :  ->
  . *)


(* Definition Pid' (pid:Index) := forall (pid:Index), ~(Nil = pid) . *)

(* Inductive Pid' := | forall (pid:Index), ~(Nil = pid) -> pid. *)

(* Check Pid 0. *)

(* Check Pid' 0.
Check Pid' 1. *)

Print Pid.
Print Pid'.

Check Pid' 1. *)