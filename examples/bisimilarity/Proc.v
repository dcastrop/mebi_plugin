(* Require Import MEBI.loader. *)
Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.

Require Coq.Program.Tactics.

(* Set Primitive Projections. *)

Import NoBranching.

Inductive bLTS : term -> option bool -> term -> Prop :=
| BISIM_WRAP : forall t1 a t2, termLTS t1 a t2 -> bLTS t1 (Some a) t2.

Lemma blts_some_label : forall t1 l t2, bLTS t1 l t2 -> l <> None.
Proof. intros t1 l t2 Ht HlNone. rewrite HlNone in Ht. inversion Ht. Qed.

Lemma blts_silent1_refl : forall x1 x2, silent1 bLTS x1 x2 -> x1 = x2.
Proof. intros x1 x2 Hs1. inversion Hs1; subst. inversion H. inversion H. Qed.

(* Lemma blts_silent1_wsim (ltsX ltsY:term -> option bool -> term -> Prop) 
: forall x1 x2 y1, weak_sim ltsX ltsY x1 y1 -> silent1 bLTS x1 x2 -> 
  exists y2, silent bLTS y1 y2 /\ weak_sim ltsX ltsY x2 y2.
Proof.
  intros x1 x2 y1 Hxy Htx.
  exists y1. split.
  -  *)


Module Test1.
  Example x : term := tfix (tact ASend trec).  
  Example y : term := tfix (tact ASend (tact ASend trec)). 

  Theorem r : forall x0 y0, x0 = x -> y0 = y -> weak_sim bLTS bLTS x y.
  Proof. unfold x, y. intros x0 y0 Hx0 Hy0. cofix CH1. rewrite <- Hx0, <- Hy0. 
    apply In_sim, Pack_sim; 
      [ | intros x1 Htx; apply blts_silent1_refl in Htx; 
          rewrite <- Htx; exists y0; 
          split; [ constructor | rewrite Hx0, Hy0; apply CH1 ] ].

    intros x1 ax01 Htx. 
    (* TODO: Htx weak -> no silent actions, make new lemma *)
    (* unfold weak in Htx. inversion Htx as [_x0 Htx']. inversion Htx' as [_x1 Htx'']. *)
    remember (tsubst y0 (tact ASend (tact ASend trec))) as y1; exists y1.
    split.
    {
      unfold weak. exists y0, y1. apply Pack_weak.
      - constructor. 
      - apply BISIM_WRAP. rewrite Heqy1, !Hy0. 
        (* TODO: determine that ax01 is false *)
        apply do_fix. constructor. constructor.
      - constructor. 
    }
    { cofix CH2. apply In_sim, Pack_sim.
      { admit. }
      { admit. } (* silent -- generalize lines 2-5 of proof here. *)
    }
End Test1.