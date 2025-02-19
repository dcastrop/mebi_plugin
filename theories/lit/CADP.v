
From Coq Require Export String.

(* following Figure 1: https://doi.org/10.1007/s10009-012-0244-z *)

(* interval type *)
Definition index : Type := nat.

Definition Nil : index := 0.

(* predicate type *)
Definition pid : Type := index.
(* Definition pid : Type := {n:index | ~(Nil=n)}. *)
(* Definition pid : Type := {n:nat | ~(Nil=n)}. *)
(* Definition pid : Type := {n:index | ~(Nil=n)}. *)
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

(* TODO: change this to just be kinds, and then type-check later? (or during semantics) *)
(* Inductive typable :=
  | TYPE_LOCK_ACCESS : lock_access -> typable
  | TYPE_INDEX : index -> typable
  | TYPE_BOOL : bool -> typable
  .

Inductive var_def :=
  | VAR (name:string) (type:typable)
  . *)

Inductive ref_kinds :=
  | KIND_CS | KIND_LOCK | KIND_MEM | KIND_INDEX | KIND_BOOL | KIND_PID.

Inductive var_def :=
  | VAR (kind:ref_kinds) (name:string)
  .

(* Inductive var_ref :=
  | VAR_REF (kind:ref_kinds) (ref:string). *)

Inductive val :=
  | LIT (kind:ref_kinds) (val:string) | REF (kind:ref_kinds) (ref:string).


(* the parameters declared in the scope of processes/functions *)
Inductive params := | PARAMS (params:list var_def).
(* Inductive params := | PARAMS (params:list (ref_kinds * string)). *)

(* the arguments passed into processes/functions *)
Inductive args := | ARGS (args:list val).



(* Inductive cs_access_param :=
  | PARAM_CS (name:string)
  .

Inductive lock_access_param :=
  | PARAM_LOCK (name:string)
  .

Inductive mem_access_param :=
  | PARAM_MEM (name:string)
  . *)


(* Inductive params :=
  | PRO_PARAMS (ncs:cs_access_param) (enter:cs_access_param) (leave:cs_access_param) (l:lock_access_param) (m:mem_access_param)
  | PRC_PARAMS (ncs:cs_access_param) (enter:cs_access_param) (leave:cs_access_param) (l:lock_access_param) (m:mem_access_param) (pid:pid)
  | PARAMS (l:lock_access_param) (m:string) (pid:pid)
  . *)

(* the arguments passed into processes/functions *)
(* Inductive args :=
  | PRO_ARGS (ncs:string) (enter:string) (leave:string) (l:string) (m:string)
  | PRC_ARGS (ncs:string) (enter:string) (leave:string) (l:string) (m:string) (pid:string)
  | ARGS (l:string) (m:string) (pid:string)
  . *)

Inductive access_operation :=
  | MEMORY : memory_access -> access_operation
  | LOCK : lock_access -> access_operation
  .

Inductive iterable :=
  | ITER_LOCK_ACCESS : lock_access -> iterable
  .

Inductive term :=
  (*  *)
  | TERM
  | SEQ (terms:list term)
  (*  *)
  | SPAWN (name:string) (args:args)
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

Notation "x :: l" := (cons x l) (at level 60, right associativity).
Notation "[ ]" := nil.
Notation "[ x ; .. ; y ]" := (cons x .. (cons y nil) ..).

Declare Custom Entry term.
Declare Scope term_scope.
Notation "'if' c 'then' t" := (IF_THEN_ELSE c t)
                  (in custom term at level 90, c custom term at level 80, t custom term at level 80): term_scope.
Notation "'if' c 'then' t 'else' e" := (IF_THEN_ELSE c t e)
                  (in custom term at level 90, c custom term at level 80, t custom term at level 80, e custom term at level 80): term_scope.


Inductive process :=
  | PROCESS (name:string) (params:params) (body:term)
  .

Inductive composition :=
  | COMP (processes:list process)
  .

Print index.
Print pid.

Check 2:index.
Check 2:pid.
(* Check (2:index):pid. *)

(* TODO: make sure to add checks that pid is not nil *)

Example MCS : composition := COMP [
  (PROCESS "Protocol"
    (PARAMS [ VAR KIND_CS "NCS"
            ; VAR KIND_CS "ENTER"
            ; VAR KIND_CS "LEAVE"
            ; VAR KIND_LOCK "L"
            ; VAR KIND_MEM "M" ])
    (SEQ [
      (* PAR ( *)
        PARS [ SPAWN "P" (ARGS [ REF KIND_CS   "NCS"
                               ; REF KIND_CS   "ENTER"
                               ; REF KIND_CS   "LEAVE"
                               ; REF KIND_LOCK "L"
                               ; REF KIND_MEM  "M"
                               ; LIT KIND_PID  "1" ])
             ; SPAWN "P" (ARGS [ REF KIND_CS   "NCS"
                               ; REF KIND_CS   "ENTER"
                               ; REF KIND_CS   "LEAVE"
                               ; REF KIND_LOCK "L"
                               ; REF KIND_MEM  "M"
                               ; LIT KIND_PID  "2" ])
             ; SPAWN "P" (ARGS [ REF KIND_CS   "NCS"
                               ; REF KIND_CS   "ENTER"
                               ; REF KIND_CS   "LEAVE"
                               ; REF KIND_LOCK "L"
                               ; REF KIND_MEM  "M"
                               ; LIT KIND_PID  "3" ])
             ; SPAWN "P" (ARGS [ REF KIND_CS   "NCS"
                               ; REF KIND_CS   "ENTER"
                               ; REF KIND_CS   "LEAVE"
                               ; REF KIND_LOCK "L"
                               ; REF KIND_MEM  "M"
                               ; LIT KIND_PID  "4" ])
             ; SPAWN "P" (ARGS [ REF KIND_CS   "NCS"
                               ; REF KIND_CS   "ENTER"
                               ; REF KIND_CS   "LEAVE"
                               ; REF KIND_LOCK "L"
                               ; REF KIND_MEM  "M"
                               ; LIT KIND_PID  "5" ])
        ]
      (* TODO: don't actually spawn these, will handle them in semantics *)
      (* ) (
        PAR
          (SPAWN "P" (PRC_ARGS "NCS", "ENTER", "LEAVE", "L", "M" 5))
          (SPAWN "P" (PRC_ARGS "NCS", "ENTER", "LEAVE", "L", "M" 5))
      ) *)
    ])
  (* );
  (PROCESS "P"
    (PRC_PARAMS "NCS" "ENTER" "LEAVE" "L" "M" ) *)
  )
  (* (PROCESS "P" (PARGS  1)
    (SEQ [
      (LOOP (SEQ [

      ])) ;
      TERM
    ])
  );
  (PROCESS "acquire" (2)
    (SEQ [
      TERM
    ])
  );
  (PROCESS "release" (3)
    (SEQ [
      TERM
    ])
  ) *)
].

Print MCS.















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