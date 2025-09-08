(* https://rocq-prover.org/doc/v8.9/stdlib/Coq.Relations.Relation_Operators.html *)
Require Import Relation_Definitions.
Require Import Relation_Operators.

Set Primitive Projections.

Section Definitions.
  Context (M : Type) (A : Type).
  Definition LTS : Type := M -> option A -> M -> Prop.
  (* tau-labelled transition *)
  Definition tau (R : LTS) : relation M := fun x y => R x None y.
End Definitions.
Arguments tau {M A} R.

Hint Constructors clos_refl_trans_1n clos_trans_1n : rel_db.
Lemma clos_t_clos_rt {A : Type} (R : relation A) :
      forall x y, clos_trans_1n A R x y -> clos_refl_trans_1n A R x y.
Proof. intros x y H; induction H; eauto with rel_db. Qed.

Lemma clos_rt_clos_t {A : Type} {R : relation A} :
      forall {x y z}, R x y -> clos_refl_trans_1n A R y z ->
                    clos_trans_1n A R x z.
Proof. intros x y z Rxy H. revert x Rxy. induction H; eauto with rel_db. Qed.

Hint Resolve clos_t_clos_rt clos_rt_clos_t : rel_db.

Section WeakTrans.
  Context {M : Type} {A : Type} (lts : LTS M A).

  (* trace of tau-labelled transitions *)
  Definition silent : relation M := clos_refl_trans_1n M (tau lts).
  Definition silent1 : relation M := clos_trans_1n M (tau lts).

  (*  x ==> pre_str ->^a post_str ==> y *)
  Record weak_tr (x z : M) a (t y : M) : Prop :=
    Pack_weak { pre : silent x z; str : lts z a t; post : silent t y }.
  Definition weak (x : M) a (y : M) : Prop :=
    exists z t, weak_tr x z a t y.
End WeakTrans.
Hint Constructors weak_tr : rel_db.
Hint Resolve pre post : rel_db.
Hint Unfold silent silent1 weak : rel_db.


Section WeakSim.
  Context {M : Type} {N : Type} {A : Type} (ltsM : LTS M A) (ltsN : LTS N A).

  Record simF G m1 n1 :=
    Pack_sim
      { sim_weak : forall m2 a,
          weak ltsM m1 (Some a) m2 ->
          exists n2, weak ltsN n1 (Some a) n2 /\ G m2 n2;
        sim_tau : forall m2,
          (*  weak ltsM m1 None m2 -> *)
          silent1 ltsM m1 m2 ->
          exists n2, silent ltsN n1 n2 /\ G m2 n2;
      }.

  CoInductive weak_sim (s : M) (t : N) : Prop
    := In_sim { out_sim :  simF weak_sim s t }.
End WeakSim.
Arguments Pack_sim {M N A}%_type_scope & {ltsM ltsN G}%_function_scope
  {m1 n1} (sim_weak sim_tau)%_function_scope.
Arguments sim_weak {M N A}%_type_scope & {ltsM ltsN G%_function_scope m1 n1} s
  {m2 a} _.
Arguments sim_tau {M N A}%_type_scope & {ltsM ltsN G%_function_scope m1 n1} s
  {m2} _.
Hint Constructors simF : rel_db.
Hint Constructors weak_sim : rel_db.
Hint Resolve sim_weak sim_tau : rel_db.

Ltac needs_weak_simpl ltsR x a :=
  match goal with
  | [ H : @weak _ _ ltsR x a _ |- _ ] => fail 1
  | |- _ => idtac
  end.

Ltac needs_silent_simpl ltsM ltsN x y :=
  unfold silent, silent1, tau in *;
  match goal with
  | [ H : @clos_trans_1n _ (fun a b => ltsM a None b) x ?y
    , W : @weak_sim _ _ _ ltsM ltsN ?y y
    |- _ ]
      => fail 1
  | [ H : @clos_trans_1n _ (fun a b => ltsN a None b) y _ |- _ ]
      => fail 1
  | |- _ => idtac
  end.

Ltac bisim_simpl :=
  unfold silent, silent1, tau in *;
  repeat match goal with
  | [ H : _ /\ _ |- _] => destruct H
  | [ H : exists _, _ |- _ ] => destruct H
  | [ H : @simF _ _ _ ?ltsM ?ltsR _ ?m ?n, W : @weak _ _ ?ltsM ?m ?a _ |- _] =>
    needs_weak_simpl ltsR n a;
    destruct (@sim_weak _ _ _ _ _ _ _ _ H _ _ W) as [?[??]]
  | [ H : @simF _ _ _ ?ltsM ?ltsN _ ?m ?n
    , T : @clos_trans_1n _ (fun a b => ?ltsM a None b) ?m _
    |- _ ] =>
      needs_silent_simpl ltsM ltsN m n;
      let Th := fresh "Th" in
      let Tt := fresh "Tt" in
      destruct (@sim_tau _ _ _ _ _ _ _ _ H _ T) as [?[[|?? Th Tt]?]];
      [|pose proof (@clos_rt_clos_t _ _ _ _ _ Th Tt)]
  end.

Hint Extern 0 => progress bisim_simpl : rel_db.

Ltac bisim_cofix :=
  let CH := fresh "CH" in
  cofix CH; intros; constructor;
  (repeat match goal with
         | [ H : @weak_sim _ _ _ _ _ _ _ |- _ ] => apply out_sim in H
         end).
Tactic Notation "solve_bisim" integer(t) := bisim_cofix; eauto t with rel_db.

Lemma weak_sim_refl {M A} (lts : LTS M A) : forall x, weak_sim lts lts x x.
Proof. solve_bisim 8. Qed.
Hint Resolve weak_sim_refl : rel_db.

Lemma weak_sim_trans {M N R A}
  (ltsM : LTS M A) (ltsN : LTS N A) (ltsR : LTS R A)
  : forall x y r, weak_sim ltsM ltsN x y -> weak_sim ltsN ltsR y r ->
                  weak_sim ltsM ltsR x r.
Proof. solve_bisim 19. Qed.
Hint Immediate weak_sim_trans : rel_db.

Section WeakBisim.
  Context {M : Type} {N : Type} {A : Type} (ltsM : LTS M A) (ltsN : LTS N A).

  Definition weak_bisim (s : M) (t : N) : Prop
    := weak_sim ltsM ltsN s t /\ weak_sim ltsN ltsM t s.
End WeakBisim.
Hint Unfold weak_bisim : rel_db.

Lemma wk_bisim_refl {M A} (lts : LTS M A) : forall x, weak_bisim lts lts x x.
Proof. eauto with rel_db. Qed.

Lemma wk_bisim_trans {M A} (lts : LTS M A) : forall x y z,
    weak_bisim lts lts x y -> weak_bisim lts lts y z -> weak_bisim lts lts x z.
Proof. eauto with rel_db. Qed.

Lemma wk_bisim_sym {M A} (lts : LTS M A) : forall x y,
    weak_bisim lts lts x y -> weak_bisim lts lts y x.
Proof. eauto with rel_db. Qed.
