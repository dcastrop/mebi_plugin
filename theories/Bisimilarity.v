
Section Definitions.
  Context
    {M : Set}
    {A : Set}
    (LTS1 : M -> A -> M -> Prop)
    {N : Set}
    (LTS2 : N -> A -> N -> Prop).

  Record bisim_concl (Bisim : M -> N -> Prop) (t t' : N) (a : A) (s' : M) : Prop :=
    { next_tr : LTS2 t a t';
      next_bi : Bisim s' t'
    }.

  Record bisim_f (Bisim : M -> N -> Prop) (s : M) (t : N) : Prop :=
    { bisim_l :
        forall s' a,
          LTS1 s a s' ->
          exists t',
            bisim_concl Bisim t t' a s';
      bisim_r :
        forall t' a,
          LTS2 t a t' ->
          exists s',
            LTS1 s a s' /\ Bisim s' t';
    }.

  Set Primitive Projections.
  CoInductive bisim (s : M) (t : N) : Prop :=
    In_bisim { out_bisim :  bisim_f bisim s t }.
  Unset Primitive Projections.
End Definitions.

Section StreamExample.
   (* WARNING! We cannot do this with our approach because not an FSM *)
Inductive streamF_ (Stream : Set -> Set) (A : Set) :=
  | nilF : streamF_ Stream A
  | consF : A -> Stream A -> streamF_ Stream A.
Arguments nilF & {_}{_}.
Arguments consF & {_}{_}.

Set Primitive Projections.
CoInductive stream (A : Set) : Set := In_stream { out_stream : streamF_ stream A }.
Unset Primitive Projections.
Arguments out_stream {A%_type_scope} s.
Arguments In_stream {A%_type_scope}.

CoFixpoint fmap {A B : Set} (f : A -> B) (s : stream A) : stream B :=
  match out_stream s with
  | nilF => In_stream nilF
  | consF x xs => In_stream (consF (f x) (fmap f xs))
  end.

CoFixpoint zeros : stream nat := In_stream (consF 0 zeros).
CoFixpoint ones : stream nat := In_stream (consF 1 ones).

Definition zplus1 : stream nat := fmap S zeros.

(* Infinite case *)
Inductive StreamLTS A : stream A -> A -> stream A -> Prop :=
| stream_step : forall s x s', out_stream s = consF x s' -> StreamLTS A s x s'.

Goal bisim (StreamLTS nat) (StreamLTS nat) zeros ones.
  cofix CH.
  constructor.
  constructor.
  - intros s' a H. inversion_clear H; subst.
    inversion H0; subst.
    exists ones, 1.
    split.
    apply stream_step; reflexivity.
    apply CH.
    Guarded.
  - intros s' a H. inversion_clear H; subst.
    inversion H0; subst.
    exists zeros, 0.
    split.
    apply stream_step; reflexivity.
    apply CH.
    Guarded.
  Qed.
End StreamExample.
