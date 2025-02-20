
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

Inductive action : Type :=
  | READ_NEXT
  | READ_LOCKED
  | WRITE_NEXT
  | WRITE_LOCKED
  | FETCH_AND_STORE
  | COMPARE_AND_SWAP
  .

Inductive tm : Type :=
  | OK
  | SEQ (terms:list tm)
  | ACT (label:action)
  | IF (condition:Prop) (pass:tm) (fail:tm)
  | LOOP (name:option nat)
  | BREAK (name:nat)
  .

Inductive mem_item : Type :=
  | PID (name:string) (val:pid)
  .

Definition mem : Type := list mem_item.

Definition Empty : mem := nil.

Definition state : Type := pid * mem.
Definition Initial_state : state := (Nil, Empty).

(* Inductive step : process * state -> action -> process * state -> Prop :=
  | ST_IF_TT : forall t1 t2, <{ if tru then t1 else t2 }> --> t1
  | ST_IF_FF : forall t1 t2, <{ if fls then t1 else t2 }> --> t2
  | ST_IF : forall c1 c2 t1 t2, c1 --> c2 -> <{ if c1 then t1 else t2 }> --> <{ if c2 then t1 else t2 }>

  where "t '-->' t'" := (step t t'). *)

Print step.
