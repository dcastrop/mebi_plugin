
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

Inductive of_kind :=
  | OF (kind:ref_kinds) (ref:string).

Inductive val :=
  (* | LIT (kind:ref_kinds) (val:string) | REF (of_kind:of_kind). *)
  | BIND (to:string)
  | LIT (of_kind:of_kind)
  | REF (of_kind:of_kind)
  | AS (to:ref_kinds) (from:of_kind)
  | OP (op:operation)
  .


(* the parameters declared in the scope of processes/functions *)
Inductive params :=
  | PARAM (param:var_def)
  | PARAMS (params:list var_def)
  .
(* Inductive params := | PARAMS (params:list (ref_kinds * string)). *)

(* the arguments passed into processes/functions *)
Inductive args :=
  | ARG (arg:val)
  | ARGS (args:list val)
  .



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

(* Inductive access_operation :=
  | MEMORY : memory_access -> access_operation
  | LOCK : lock_access -> access_operation
  . *)

(* Inductive iterable :=
  | ITER_LOCK_ACCESS : lock_access -> iterable
  . *)

  (* SEQ ((ACT a) :: rest) , state ->^{\tau} SEQ rest (do a state) -- if a is not visible *)
  (* SEQ ((ACT a) :: rest) , state ->^{a} SEQ rest (do a state)  -- if a is visible *)
Inductive term : Type :=
  (*  *)
  | TERM
  | OK
  | SEQ (terms:list term)
  (*  *)
  | ACT (of_kind:of_kind) (args:args)
  (*  *)
  | SPAWN (name:string) (args:args)
  (*  *)
  (* | OP (op:access_operation) *)
  (*  *)
  | PAR (lhs:term) (rhs:term)
  | PARS (bodies:list term)
  (*  *)
  | DEF (def:var_def)
  | DEFS (defs:list var_def)
  (*  *)
  | LOOP      (body:term)
  | LOOP_OVER (over:of_kind) (body:term)
  | BREAK (loop:of_kind)
  (*  *)
  | IFF (condition:Prop) (do_then:term)
  | IF  (condition:Prop) (do_then:term) (do_else:term)
  (*  *)
  (* | SELECT () *)
  .

Notation "x :: l" := (cons x l) (at level 60, right associativity).
Notation "[ ]" := nil.
Notation "[ x ; .. ; y ]" := (cons x .. (cons y nil) ..).

Declare Custom Entry term.
Declare Scope term_scope.
Notation "'if' c 'then' t" := (IF c t)
                  (in custom term at level 90, c custom term at level 80, t custom term at level 80): term_scope.
Notation "'if' c 'then' t 'else' e" := (IF c t e)
                  (in custom term at level 90, c custom term at level 80, t custom term at level 80, e custom term at level 80): term_scope.


Inductive process :=
  | PROCESS (name:string) (params:params) (body:term)
  .

Inductive composition :=
  | COMP (processes:list process)
  .

(* Definition state := nat * mem * ... -- i **)
(* Definition Nil := None.  *)
(* Initial_state : state = (Nil, ... ) *)
(* Inductive LTS_proc : process * state -> action -> process * state -> Prop :=  *)
(* Inductive LTS : list process * state -> action -> list process * state -> Prop := *)

Print index.
Print pid.

Check 2:index.
Check 2:pid.
(* Check (2:index):pid. *)

(* TODO: make sure to add checks that pid is not nil *)

Example MCS : composition := COMP
[ (PROCESS "Protocol"
    (PARAMS [ VAR KIND_CS "NCS"
            ; VAR KIND_CS "ENTER"
            ; VAR KIND_CS "LEAVE"
            ; VAR KIND_LOCK "L"
            ; VAR KIND_MEM "M" ])
    (SEQ [ PARS [ SPAWN "P" (ARGS [ REF (OF KIND_CS   "NCS")
                                  ; REF (OF KIND_CS   "ENTER")
                                  ; REF (OF KIND_CS   "LEAVE")
                                  ; REF (OF KIND_LOCK "L")
                                  ; REF (OF KIND_MEM  "M")
                                  ; LIT (OF KIND_PID  "1") ])
                ; SPAWN "P" (ARGS [ REF (OF KIND_CS   "NCS")
                                  ; REF (OF KIND_CS   "ENTER")
                                  ; REF (OF KIND_CS   "LEAVE")
                                  ; REF (OF KIND_LOCK "L")
                                  ; REF (OF KIND_MEM  "M")
                                  ; LIT (OF KIND_PID  "2") ])
                ; SPAWN "P" (ARGS [ REF (OF KIND_CS   "NCS")
                                  ; REF (OF KIND_CS   "ENTER")
                                  ; REF (OF KIND_CS   "LEAVE")
                                  ; REF (OF KIND_LOCK "L")
                                  ; REF (OF KIND_MEM  "M")
                                  ; LIT (OF KIND_PID  "3") ])
                ; SPAWN "P" (ARGS [ REF (OF KIND_CS   "NCS")
                                  ; REF (OF KIND_CS   "ENTER")
                                  ; REF (OF KIND_CS   "LEAVE")
                                  ; REF (OF KIND_LOCK "L")
                                  ; REF (OF KIND_MEM  "M")
                                  ; LIT (OF KIND_PID  "4") ])
                ; SPAWN "P" (ARGS [ REF (OF KIND_CS   "NCS")
                                  ; REF (OF KIND_CS   "ENTER")
                                  ; REF (OF KIND_CS   "LEAVE")
                                  ; REF (OF KIND_LOCK "L")
                                  ; REF (OF KIND_MEM  "M")
                                  ; LIT (OF KIND_PID  "5") ]) ] ]) )
  ; (PROCESS "P"
      (PARAMS [ VAR KIND_CS   "NCS"
              ; VAR KIND_CS   "ENTER"
              ; VAR KIND_CS   "LEAVE"
              ; VAR KIND_LOCK "L"
              ; VAR KIND_MEM  "M"
              ; VAR KIND_PID  "PID" ])
      (SEQ [ LOOP (SEQ [ ACT (OF KIND_CS "NCS")
                           (ARG (REF (OF KIND_PID "PID")))

                       ; SPAWN "acquire"
                           (ARGS [ REF (OF KIND_LOCK "L")
                                 ; REF (OF KIND_MEM  "M")
                                 ; REF (OF KIND_PID  "PID") ])
                       ; ACT (OF KIND_CS "ENTER")
                           (ARG (REF (OF KIND_PID "PID")))

                       ; ACT (OF KIND_CS "LEAVE")
                           (ARG (REF (OF KIND_PID "PID")))

                       ; SPAWN "release"
                           (ARGS [ REF (OF KIND_LOCK "L")
                                 ; REF (OF KIND_MEM  "M")
                                 ; REF (OF KIND_PID  "PID") ]) ]) ]) )
  ; (PROCESS "acquire"
      (PARAMS [ VAR KIND_LOCK "L"
              ; VAR KIND_MEM  "M"
              ; VAR KIND_PID  "PID" ])
      (SEQ [ DEFS [ VAR KIND_INDEX "predecessor"
                  ; VAR KIND_BOOL  "locked" ]

           ; ACT (OF KIND_MEM "M")
              (ARGS [ OP  WRITE_NEXT
                    ; REF (OF KIND_PID   "PID")
                    ; LIT (OF KIND_INDEX "Nil")
                    ; REF (OF KIND_PID   "PID") ])

           ; ACT (OF KIND_MEM "L")
              (ARGS [ OP   FETCH_AND_STORE
                    ; BIND "predecessor"
                    ; AS   KIND_INDEX (OF KIND_PID "PID")
                    ; REF             (OF KIND_PID "PID") ])

           ; IFF
              (REF (OF KIND_INDEX "predecessor") = LIT (OF KIND_INDEX "Nil"))
              (SEQ [ ACT (OF KIND_MEM "M")
                      (ARGS [ OP  WRITE_LOCKED
                            ; REF (OF KIND_PID  "PID")
                            ; LIT (OF KIND_BOOL "true")
                            ; REF (OF KIND_PID  "PID") ])

                   ; ACT (OF KIND_MEM "M")
                      (ARGS [ OP  WRITE_NEXT
                            ; AS  KIND_PID   (OF KIND_INDEX "predecessor")
                            ; AS  KIND_INDEX (OF KIND_PID "PID")
                            ; REF            (OF KIND_PID  "PID") ])

                    ; LOOP_OVER (OF KIND_LOCK "L")
                      (SEQ [ ACT (OF KIND_MEM "M")
                              (ARGS [ OP   READ_LOCKED
                                    ; REF  (OF KIND_PID "PID")
                                    ; BIND "locked"
                                    ; REF  (OF KIND_PID "PID") ])

                           ; IFF (REF (OF KIND_BOOL "locked") = LIT (OF KIND_BOOL "false"))
                              (BREAK (OF KIND_LOCK "L")) ]) ]) ]) )
  ; (PROCESS "release"
      (PARAMS [ VAR KIND_LOCK "L"
              ; VAR KIND_MEM  "M"
              ; VAR KIND_PID  "PID" ])
      (SEQ [ DEFS [ VAR KIND_INDEX "next"
                  ; VAR KIND_BOOL  "swap" ]

           ; ACT (OF KIND_MEM "M")
              (ARGS [ OP   READ_NEXT
                    ; REF  (OF KIND_PID   "PID")
                    ; BIND "next"
                    ; REF  (OF KIND_PID   "PID") ])
           ; IF (REF (OF KIND_INDEX "next") = LIT (OF KIND_INDEX "Nil"))
              (SEQ [ ACT (OF KIND_LOCK "L")
                      (ARGS [ OP   COMPARE_AND_SWAP
                            ; AS   KIND_INDEX (OF KIND_PID "PID")
                            ; LIT  (OF KIND_INDEX "Nil")
                            ; BIND "swap"
                            ; REF  (OF KIND_PID "PID") ])

                   ; IFF
                      (REF (OF KIND_BOOL "swap") = LIT (OF KIND_BOOL "false"))
                      (LOOP_OVER (OF KIND_LOCK "L")
                        (SEQ [ ACT (OF KIND_MEM "M")
                                (ARGS [ OP   READ_NEXT
                                      ; REF (OF KIND_PID "PID")
                                      ; BIND "next"
                                      ; REF (OF KIND_PID "PID") ])

                             ; IFF (~(REF (OF KIND_INDEX "next") = LIT (OF KIND_INDEX "Nil")))
                                (BREAK (OF KIND_LOCK "L")) ])) ])

              (ACT (OF KIND_MEM "M")
                (ARGS [ OP  WRITE_LOCKED
                      ; AS  KIND_PID (OF KIND_INDEX "next")
                      ; LIT (OF KIND_BOOL "false")
                      ; REF (OF KIND_PID "PID") ]))
 ]) )
].

Print term.

Print MCS.



(* following: https://softwarefoundations.cis.upenn.edu/plf-current/Types.html *)

Inductive step : term -> term -> Prop :=
  (*  *)
  | ST_PAR_L : forall t1 t2 t3,  step (PAR t1 t2) (PAR t3 t2)
  | ST_PAR_R : forall t1 t2 t3,  step (PAR t1 t2) (PAR t1 t3)
  (*  *)
  (* | ST_SEQ : forall h1 h2 t,
    step (cons h1 (cons h2 t)) (cons h1 (cons h2 t)) -> step (cons h1 (cons h2 t)) (cons h2 t) *)
  (*  *)
  (* | ST_IFF_TRUE : forall t1,    step (IFF () t1) t1 *)
  (*  *)
  | ST_IF_TRUE : forall t1 t2,  step (IF true t1 t2) t1
  | ST_IF_FALSE : forall t1 t2, step (IF false t1 t2) t2
  .




(* Hint Constructors step : core. *)










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
