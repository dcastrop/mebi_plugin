(* Require Import MEBI.Bisimilarity. *)

Section stream.
  Variable A : Type.

  CoInductive stream : Type :=
  | Cons : A -> stream -> stream.

  Fixpoint approx (s : stream) (n : nat) : list A :=
    match n with
    | 0 => nil
    | S n' => 
      match s with 
      | Cons h t => h :: approx t n'
      end
    end.  
End stream.

Section nats.
  Inductive parity : Type :=
  | EVN : nat -> parity
  | ODD : nat -> parity.

  Definition get_parity (n : nat) : parity := 
    if Nat.even n then EVN n else ODD n.

  CoInductive plus_1 : stream nat -> parity -> stream nat -> Prop :=
  | PLUS_ONE : forall h t,
    plus_1 (Cons _ h t) (get_parity (S h)) (Cons _ (S h) (Cons _ h t))
  .
  
  CoInductive plus_2 : stream nat -> parity -> stream nat -> Prop :=
  | PLUS_TWO : forall h0 t0 p0 h1 t1 p1 h2 t2,
    plus_1 (Cons _ h0 t0) p0 (Cons _ h1 t1) ->
    plus_1 (Cons _ h1 t1) p1 (Cons _ h2 t2) ->
    plus_2 (Cons _ h0 t0) (get_parity h2) (Cons _ h2 t2)
  .
  
  CoInductive plus_3 : stream nat -> parity -> stream nat -> Prop :=
  | PLUS_THREE : forall h0 t0 p0 h1 t1 p1 h2 t2,
    plus_1 (Cons _ h0 t0) p0 (Cons _ h1 t1) ->
    plus_2 (Cons _ h1 t1) p1 (Cons _ h2 t2) ->
    plus_3 (Cons _ h0 t0) (get_parity h2) (Cons _ h2 t2)
  .
End nats.

CoFixpoint 

(* CoFixpoint even_odd (n : nat) : stream nat := Cons _ n (odd_even n) *)
(* with odd_even (n : nat) : stream nat := Cons _ (S n) (even_odd (S S n)). *)
(* Eval simpl in approx nat (even_odd 2) 10. *)












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