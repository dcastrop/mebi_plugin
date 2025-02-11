
Section Definitions.
  Context
    {M : Set}
    {A : Set}
    (LTS1 : M -> A -> M -> Prop)
    {N : Set}
    (LTS2 : N -> A -> N -> Prop).

  Definition simF (Sim : M -> N -> Prop) (s : M) (t : N) : Prop :=
    forall s' a,
      LTS1 s a s' ->
      exists t',
        LTS2 t a t' /\ Sim s' t'.

  Set Primitive Projections.
  CoInductive sim (s : M) (t : N) : Prop :=
    In_sim { out_sim :  simF sim s t }.
  Unset Primitive Projections.

End Definitions.

Lemma sim_refl (M A : Set) (LTS : M -> A -> M -> Prop) (m : M)
  : sim LTS LTS m m.
Proof.
  revert m.
  cofix CH.
  intros m.
  constructor.
  intros m' a tr.
  exists m'.
  split.
  - exact tr.
  - apply CH. Guarded.
Qed.

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

Example stream_sim : sim (StreamLTS nat) (StreamLTS nat) zplus1 ones.
  cofix CH.
  constructor.
  intros s' a H. inversion_clear H; subst.
  inversion H0; subst.
  exists ones.
  split.
  apply stream_step; reflexivity.
  apply CH. Guarded.
Qed.

End StreamExample.

Module BisimTest1.
  Inductive action : Set := | TheAction1 | TheAction2.
  Inductive term : Set :=
  | trec : term
  | tend : term
  | tfix : term -> term
  | tact : action -> term -> term
  | tpar : action -> action -> term -> term
  .

  Fixpoint subst (t1 : term) (t2 : term) :=
    match t2 with
    | trec => t1
    | tend => tend
    | tfix t => tfix t
    | tact a t => tact a (subst t1 t)
    | tpar a b t => tpar a b (subst t1 t)
    end.

  Inductive termLTS : term -> action -> term -> Prop :=
  | do_act : forall a t, termLTS (tact a t) a t

  | do_par1 : forall a b t, termLTS (tpar a b t) a (tact b t)

  | do_par2 : forall a b t, termLTS (tpar a b t) b (tact a t)

  | do_fix : forall a t t',
      termLTS (subst (tfix t) t) a t' ->
      termLTS (tfix t) a t'.

  Ltac example_tactic CH :=
   let r := fresh "r" in
   let a := fresh "a" in
   let tr := fresh "tr" in
   constructor; intros r a tr;

   (*  *)
   repeat
     match goal with
     | [ H : termLTS ?l ?a ?r |- _ ] =>
         inversion_clear H; simpl in *
     end;
   (*  *)

   eexists;
   split; [ repeat constructor | idtac ];
   try apply sim_refl; try apply CH.

  Ltac example_bisim :=
    let CH := fresh "CH" in
    cofix CH; repeat (example_tactic CH).

  Goal sim termLTS termLTS
    (tfix (tact TheAction1 trec))
    (tact TheAction1 (tfix (tact TheAction1 trec))).
  Proof.
    example_bisim.
  Qed.

  Goal sim termLTS termLTS
    (tact TheAction1 (tfix (tact TheAction1 trec)))
    (tfix (tact TheAction1 trec)).
  Proof.
    example_bisim.
  Qed.

  Goal sim termLTS termLTS
    (tfix (tact TheAction1 (tact TheAction2 trec)))
    (tact TheAction1 (tfix (tact TheAction2 (tact TheAction1 trec)))).
  Proof.
    example_bisim.
  Qed.

  Goal sim
    termLTS termLTS
      (tfix (tpar TheAction1 TheAction2 trec))
      (tfix (tpar TheAction1 TheAction2 trec)).
    cofix CH.

   let r := fresh "r" in
   let a := fresh "a" in
   let tr := fresh "tr" in
   constructor; intros r a tr.
   repeat
     match goal with
     | [ H : termLTS ?l ?a ?r |- _ ] =>
         inversion_clear H; simpl in *
     end.
   eexists; split; [ repeat constructor | idtac ].
   apply sim_refl.
   simpl.

   eexists.
   split.
   repeat constructor.
   apply sim_refl.
   Qed.

  Section NonDetExamples.
    Inductive Action := A | B.

    Inductive TM1 := S | S1 | S2.
    Inductive TM2 := T | T1.

    Inductive LTS1 : TM1 -> Action -> TM1 -> Prop :=
    | SS1A : LTS1 S A S1
    | SS2A : LTS1 S A S2
    | S1S2B : LTS1 S1 B S2
    | S2S2B : LTS1 S2 B S2.

    Inductive LTS2 : TM2 -> Action -> TM2 -> Prop :=
    | TT1A : LTS2 T A T1
    | T1T1A : LTS2 T1 A T1
    | T1T1B : LTS2 T1 B T1.

    Goal sim LTS2 LTS1 T S.
      cofix CH.
      constructor.
      intros r a tr.
      inversion_clear tr.
      eexists.
      split.
      constructor.

      cofix CH1.
      constructor.
      clear r a.
      intros r a tr.
      inversion_clear tr.
      eexists.
      Abort.
    (*   split. *)
    (*   constructor. *)

    (*   cofix CH2. *)
    (*   constructor. *)
    (*   clear r a. *)
    (*   intros r a tr. *)
    (*   inversion_clear tr. *)
    (*   eexists. *)
    (*   split. *)
    (*   constructor. *)
    (*   apply CH2. *)
    (* Qed. *)

  End NonDetExamples.
End BisimTest1.
