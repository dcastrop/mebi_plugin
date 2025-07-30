Require Import MEBI.Bisimilarity.

Section stream0.
  Variable A : Type.

  CoInductive stream' : Type :=
  | Cons : A -> stream' -> stream'.
End stream0.

Set Primitive Projections.
Inductive streamF (Stream : Set -> Set) (A : Set) :=
| nilF : streamF Stream A
| consF : A -> Stream A -> streamF Stream A.
Arguments nilF & {_}{_}.
Arguments consF & {_}{_}.

CoInductive stream (A : Set) : Set := 
  In_stream { out_stream : streamF stream A }.
Arguments out_stream {A%_type_scope} s.
Arguments In_stream {A%_type_scope}.

Print stream'.
Print stream.

Definition stream_hd_opt {A : Set} (s : stream A) : option A :=
  match out_stream s with
  | consF n _ns => Some n
  | nilF => None
  end.

Definition stream_hd {A : Set} (d : A) (s : stream A) : A :=
  match stream_hd_opt s with
  | Some n => n
  | None => d
  end.

Definition stream_tl {A : Set} (s : stream A) : stream A :=
  match out_stream s with
  | consF _n ns => ns
  | nilF => In_stream nilF
  end.

CoFixpoint fmap {A B : Set} (f : A -> B) (s : stream A) : stream B :=
  match out_stream s with
  | nilF => In_stream nilF
  | consF h t => In_stream (consF (f h) (fmap f t))
  end.

Inductive parity : Type :=
| EVN : nat -> parity
| ODD : nat -> parity
| NIL : parity.

Definition inv_parity (a b : parity) : Prop :=
match a, b with
| EVN _, ODD _ => True
| ODD _, EVN _ => True
| _, _ => False
end.

Definition get_parity (n : nat) : parity := 
  if Nat.even n then EVN n else ODD n.

Definition stream_parity (s : stream nat) : parity :=
  match out_stream s with
  | nilF => NIL
  | consF n _ => get_parity n
  end.

Inductive plus_1 : stream nat -> option parity -> stream nat -> Prop :=
| PLUS_ONE : forall n ns,
  plus_1  (In_stream (consF n ns)) 
          (Some (get_parity (S n)))
          (In_stream (consF (S n) (In_stream (consF n ns))))
.

Lemma nats_plus_1 : forall s n ns a c, 
  plus_1 (In_stream (consF n ns)) a c -> 
  out_stream c = consF (S n) s.
Proof. Admitted.

Inductive plus_2 : stream nat -> option parity -> stream nat -> Prop :=
| PLUS_TWO : forall s0 s1 a0 a1 n ns,
  plus_1 s0 a0 s1 ->
  plus_1 s1 a1 (In_stream (consF n ns)) -> 
  plus_2 s0 (Some (get_parity n)) (In_stream (consF n ns))
.

Inductive plus_3 : stream nat -> option parity -> stream nat -> Prop :=
| PLUS_THREE : forall s0 s1 a0 a1 n ns,
  plus_1 s0 a0 s1 ->
  plus_2 s1 a1 (In_stream (consF n ns)) -> 
  plus_3 s0 (Some (get_parity n)) (In_stream (consF n ns))
.

CoFixpoint zeros : stream nat := In_stream (consF 0 zeros).
CoFixpoint nats_from (n : nat) : stream nat := 
  In_stream (consF n (nats_from (S n))).

Lemma stream_plus_1 : forall n a s,
  plus_1 (nats_from n) a s -> s = nats_from (S n).
Proof. Admitted.

Definition geq0 : stream nat := nats_from 0.
Definition geq2 : stream nat := nats_from 2.

Example bisim_odd_parity : weak_bisim plus_1 plus_3 geq0 geq2.
Proof. Admitted.


Module StreamTest.
  (* http://adam.chlipala.net/cpdt/html/Coinductive.html *)

  Section stream.
    Variable A : Type.

    CoInductive stream : Type :=
    | Cons : A -> stream -> stream.
  End stream.

  CoFixpoint zeroes : stream nat := Cons _ 0 zeroes.

  CoFixpoint trues_falses : stream bool := Cons _ true falses_trues
  with falses_trues : stream bool := Cons _ false trues_falses.

  Fixpoint approx A (s : stream A) (n : nat) : list A :=
    match n with
    | 0 => nil
    | S n' => 
      match s with 
      | Cons _ h t => h :: approx _ t n'
      end
    end.
  
  Eval simpl in approx nat zeroes 10.

  Eval simpl in approx bool trues_falses 10.

  Section map.
    Variables A B : Type.
    Variable f : A -> B.

    CoFixpoint map (s : stream A) : stream B :=
      match s with 
      | Cons _ h t => Cons _ (f h) (map t)
    end.
  End map.

End StreamTest.