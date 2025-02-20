
From Coq Require Export String.

(* following Figure 1: https://doi.org/10.1007/s10009-012-0244-z *)

Definition index : Type := nat.

Definition pid : Type := option index.

Definition Nil : option index := None.

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

Inductive action_detail : Type :=
  | READ_NEXT
  | READ_LOCKED
  | WRITE_NEXT
  | WRITE_LOCKED
  | FETCH_AND_STORE
  | COMPARE_AND_SWAP
  .

Inductive action_kind : Type :=
  | NCS | ENTER | LEAVE | M | L | TAU.

Definition action : Type := (action_kind * option action_detail).
Definition silent : action := (TAU, None).

Inductive tm : Type :=
  | TERM  : tm                            (* termination *)
  | OK    : tm                            (* no-op *)

  | ACT   : action -> tm -> tm            (* action -> continuation -> ... *)
  | IF    : tm -> tm -> tm -> tm          (* condition -> if true -> if false -> ... *)

  | LOOP  : option nat -> tm -> tm -> tm  (* optional loop id -> body -> outer continuation -> ... *)
  | BREAK : nat -> tm                     (* loop id -> ... *)

  | TRU   : tm                            (* true *)
  | FLS   : tm                            (* false *)

  | ZRO : tm
  | SCC : tm -> tm
  .

(* following: https://softwarefoundations.cis.upenn.edu/plf-current/Types.html *)

Declare Custom Entry tm.
Declare Scope tm_scope.
Notation "<{ e }>" := e (e custom tm at level 99): tm_scope.
Notation "( x )" := x (in custom tm, x at level 99): tm_scope.
Notation "x" := x (in custom tm at level 0, x constr at level 0): tm_scope.
Notation "'0'" := (ZRO) (in custom tm at level 0): tm_scope.
Notation "'0'" := 0 (at level 1): tm_scope.
Notation "'succ' x" := (SCC x) (in custom tm at level 90, x custom tm at level 80): tm_scope.
Notation "'if' c 'then' t 'else' e" := (IF c t e)
                 (in custom tm at level 90, c custom tm at level 80,
                  t custom tm at level 80, e custom tm at level 80): tm_scope.
Local Open Scope tm_scope.

Inductive bvalue : tm -> Prop :=
  | bv_true : bvalue <{ TRU }>
  | bv_false : bvalue <{ FLS }>.

Inductive nvalue : tm -> Prop :=
  | nv_0 : nvalue <{ 0 }>
  | nv_succ : forall t, nvalue t -> nvalue <{ succ t }>.

Definition value (t : tm) := bvalue t \/ nvalue t.

Hint Constructors bvalue nvalue : core.
Hint Unfold value : core.

Inductive mem_item : Type :=
  | PID (name:string) (val:pid)
  .

Definition mem : Type := list mem_item.

Definition Empty : mem := nil.

Definition state : Type := pid * mem.
Definition Initial_state : state := (Nil, Empty).

Reserved Notation "t '-->' t'" (at level 40).

Inductive step : (tm * state) -> action -> (tm * state) -> Prop :=
  | ST_IF_TT : forall t1 t2 s, (<{ if TRU then t1 else t2 }>, s) --> silent --> (t1, s)
  | ST_IF_FF : forall t1 t2 s, (<{ if FLS then t1 else t2 }>, s) --> (t2, s)
  | ST_IF : forall c1 c2 t1 t2 s1 s2, (c1, s1) --> (c2, s2) -> (<{ if c1 then t1 else t2 }>, s1) --> (<{ if c2 then t1 else t2 }>, s2)

  where "t '-->' t'" := (step t t').

(* Print step. *)
