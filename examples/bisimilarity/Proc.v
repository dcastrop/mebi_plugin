(* Require Import MEBI.loader. *)
Require Coq.Program.Tactics.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.

(****************************************************************************)
(* NOTE: makes goals easier to read, e.g., [H : tend = t] -> [H : t = tend] *)
Ltac ltr_goals :=
  repeat match goal with
  | [ t : term, H : tend       = ?t |- _ ] => symmetry in H
  | [ t : term, H : trec       = ?t |- _ ] => symmetry in H
  | [ t : term, H : tfix _     = ?t |- _ ] => symmetry in H
  | [ t : term, H : tact _ _   = ?t |- _ ] => symmetry in H
  | [ t : term, H : tseq _ _   = ?t |- _ ] => symmetry in H
  | [ t : term, H : tpar _ _   = ?t |- _ ] => symmetry in H
  | [ t : term, H : tsubst _ _ = ?t |- _ ] => symmetry in H
  | |- _  => idtac
  end.

(* NOTE: computes certain fun, e.g., [tsubst _ _] for recursive unfoldings *)
Ltac compute_goals :=
  repeat match goal with
  | [ t : term, H : ?t = tsubst _ _ |- _ ] => compute in H
  | |- _ => idtac
  end.

(* NOTE: reformats all goals in a consistent way, resolves certain fun calls *)
Ltac fmt_goals := ltr_goals; compute_goals.

(* NOTE: puts terms into the hypothesis (useful when they get too long). *)
Ltac move_terms_to_hypothesis :=
  ltr_goals;
  match goal with
  (* do nothing if both are vars with defined goals *)
  | [ Hx0 : ?xv0 = ?xt0, Hy0 : ?yv0 = ?yt0 
    |- @weak_sim _ _ _ _ _ ?xv0 ?yv0 ] => idtac
  (* otherwise, repackage the terms as variables in the goal *)
  | |- _ =>
    repeat match goal with
    (* if [x] term is defined and unpacked then repack *)
    | [ Hx0 : ?xv0 = ?xt0, Hx1 : ?xv1 = ?xt1
      , Htx : @weak _ _ _ ?xv0 (Some _) ?xv1
      |- @weak_sim _ _ _ _ _ ?xt1 _ ] => rewrite <- Hx1
    (* if [y] term is defined and unpacked then repack *)
    | [ Hx1 : ?xv1 = ?xt1, Hy1 : ?yv1 = ?yt1
      |- @weak_sim _ _ _ _ _ ?xv1 ?yt1 ] => rewrite <- Hy1
    (* if current hypothesis has unpackaged y term: *)
    | [ Hx0 : ?xv0 = ?xt0
      (* , Hx1 : ?xv1 = ?xt1 *)
      , Htx : @weak _ _ _ ?xv0 (Some _) ?xv1
      |- @weak_sim _ _ _ _ _ ?xv1 _ ] => 
      let y1 := fresh "y0" in let Hy1 := fresh "Hy0" in 
      match goal with 
      | |- @weak_sim _ _ _ _ _ ?xv1 tend                 => remember tend               as y1 eqn:Hy1
      | |- @weak_sim _ _ _ _ _ ?xv1 trec                 => remember trec               as y1 eqn:Hy1
      | |- @weak_sim _ _ _ _ _ ?xv1 (tfix ?yt1)          => remember (tfix yt1)         as y1 eqn:Hy1
      | |- @weak_sim _ _ _ _ _ ?xv1 (tact ?yt1a ?yt1b)   => remember (tact yt1a yt1b)   as y1 eqn:Hy1
      | |- @weak_sim _ _ _ _ _ ?xv1 (tseq ?yt1a ?yt1b)   => remember (tseq yt1a yt1b)   as y1 eqn:Hy1
      | |- @weak_sim _ _ _ _ _ ?xv1 (tseq ?yt1a ?yt1b)   => remember (tseq yt1a yt1b)   as y1 eqn:Hy1
      | |- @weak_sim _ _ _ _ _ ?xv1 (tpar ?yt1a ?yt1b)   => remember (tpar yt1a yt1b)   as y1 eqn:Hy1
      | |- @weak_sim _ _ _ _ _ ?xv1 (tsubst ?yt1a ?yt1b) => remember (tsubst yt1a yt1b) as y1 eqn:Hy1
      end
    end
  end.

(****************************************************************************)
Require Import Notations.
Require Import List.
Require Import Lists.ListSet.
Import ListNotations.
  
Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

(****************************************************************************)
(* NOTE: useful for using [clos_rt_t] *)
Ltac silent_to_clos_rt :=
  repeat match goal with
  | [ H : @silent _ _ _ ?x ?y |- _ ] => apply Operators_Properties.clos_rt1n_rt in H
  | [ |- @silent _ _ _ ?x ?y ] => apply Operators_Properties.clos_rt_rt1n
  | |- _ => idtac
  end.

Section Definitions.
  (* NOTE: this is only *syntactic* *)
  Fixpoint silent_upto_strong (x : term) : list term :=
    match x with
    | trec => [x]
    | tend => [x]
    | tact m y => [x]
    | tfix b => silent_upto_strong b
    | tpar l r => 
      match l, r with
      | tact _ _, tact _ _ => [x]
      | _, tact _ _ => [x]
      | tact _ _, _ => [x]
      | _, _ => set_union term_eq (silent_upto_strong l) (silent_upto_strong r)
      end
    | tseq l r =>
      match silent_upto_strong l with
      | [] => (silent_upto_strong r)
      | ts => ts
      end
    end.

  Definition label_eqb (a b : label): bool := 
    match a, b with
    | A, A => true
    | B, B => true
    | C, C => true
    | _, _ => false
    end.

  Definition action_label (a : action) : label :=
    match a with
    | send l => l
    | recv l => l
    end.

  Fixpoint get_next_strong_label (x : term) : list label :=
    match x with
    | trec => []
    | tend => []
    | tact m y => [action_label m]
    | tfix b => get_next_strong_label b
    | tpar l r => set_union label_eq (get_next_strong_label l) (get_next_strong_label r)
    | tseq l r =>
      match get_next_strong_label l with
      | [] => (get_next_strong_label r)
      | ts => ts
      end
    end.

  (* NOTE: [a] is not necessarily the *only* next strong *)
  Definition is_next_strong (x : term) (a : label) : bool :=
    set_mem label_eq a (get_next_strong_label x).

  Lemma seq_label :
        forall tx ty tz a, termLTS (tseq tx tz) (Some a) (tseq ty tz) ->
          termLTS tx (Some a) ty.
  Proof. intros. inversion H; apply H3. Qed.

  Inductive pre_strong_hole (x : term) : term -> Prop :=
  | is_hole : pre_strong_hole x x
  | go_hole : 
    forall a b, pre_strong_hole x a ->
                pre_strong_hole x (tseq a b)
  .


  Inductive next_strong : term -> label -> Prop :=
  | check_next : 
    forall x y a, termLTS x None y -> next_strong y a -> next_strong x a 

  | found_strong : 
    forall x y a, termLTS x (Some a) y -> next_strong x a 
  .
  
  Lemma is_next_strong1 :
        forall x y a, termLTS x (Some a) y -> next_strong x a.
  Proof. intros. inversion H; subst. 
    eapply found_strong, H.
    eapply check_next; [ eapply ].


  Lemma weak_next_strong :
        forall x y a, weak termLTS x (Some a) y -> next_strong x a.
  Proof.
    intros.
    inversion H as [? [? [Hpre Hstr Hpost]]].



  Lemma strong_action :
        forall x y a, termLTS x (Some a) y -> 




  Lemma t : 
        forall x y z a, silent termLTS x y /\ termLTS y (Some a) z ->
                        is_next_strong x a = true.
  Proof. intros x y z a [Hxy Hyz].
    unfold is_next_strong.
    generalize dependent x.

    induction a.
    {
      intros. inversion Hxy; subst.
      {
        inversion Hyz; subst. 
        { compute. reflexivity. }
        { 
          
        }
      }
    }

    inversion Hyz; subst. 
      intros.
      inversion Hxy; subst.
        induction a; compute; reflexivity.
        
    
    induction a. intros. compute. reflexivity.

    induction Hxy; subst.
      inversion Hyz; [ induction a; compute; reflexivity | ].

      .

      induction a. inversion H.
    

    destruct (get_next_strong_label x).
    unfold get_next_strong_label.

    admit.


    (* match x with
    | trec => false
    | tend => false
    | tact m y => label_eqb (action_label m) a
    | tfix b => is_next_strong b a
    | tpar l r => is_next_strong l a || is_next_strong r a
    | tseq l r => is_next_strong l a 
    end. *)

  (* Fixpoint is_upto_strong (x y : term) : bool := *)


  Lemma t : forall x y z a, silent termLTS x y /\ termLTS y (Some a) z ->



  Lemma t : forall x y a, termLTS x (Some a) y -> 
  set_mem term_eq x (silent_upto_strong x) = true.
  Proof. intros.
    (* unfold silent_upto_strong.  *)
    inversion H. 
      subst x; subst y. 
      destruct silent_upto_strong. simpl.



    inversion H as [? [? [Hpre Hstr Hpost]]].
    inversion Hstr.

    cbv.


    unfold silent_upto_strong.

  Fixpoint extract_labels_from_actions (xs : list term) : list label :=
    match xs with
    | [] => []
    | h :: t => 
      match h with
      | tact (send l) _ => set_add label_eq l (extract_labels_from_actions t)
      | tact (recv l) _ => set_add label_eq l (extract_labels_from_actions t)
      | _ => extract_labels_from_actions t
      end 
    end.

  Definition next_strong_labels (x : term) : list label :=
    extract_labels_from_actions (silent_upto_strong x).

  (****************************************************************************)
  Lemma tseq_label_trans : 
        forall xa ya b l, termLTS xa l ya -> termLTS (tseq xa b) l (tseq ya b).
  Proof. intros. apply do_seq, H. Qed.

  (* Lemma t : forall x y a, termLTS x (Some a) y -> (next_strong_labels x) = [a].
  Proof. intros x y a Hxy. 
    (* unfold next_strong_labels. *)
    (* unfold extract_labels_from_actions. *)
    (* unfold silent_upto_strong. *)

    induction a; (inversion Hxy; [ compute; reflexivity | ]); subst x; subst y. 

    eapply tseq_label_trans in Hxy.

    inversion Hxy.
    
  Admitted.


    compute. 
    .
    
    ; [compute; exact I |]; subst x; subst y.
    inversion Hxy


  Lemma t :
        forall x y a, termLTS x (Some a) y -> 
                      Bool.Is_true (set_mem label_eq a (next_strong_labels x)).
  Proof. intros x y a Hxy; unfold Bool.Is_true.

    (* generalize dependent x.

    induction a.
    intros x Hxy. 
    inversion Hxy; [compute; exact I |]; subst x; subst y.

    inversion t. *)

    unfold next_strong_labels.
    unfold extract_labels_from_actions.
    unfold silent_upto_strong.

    destruct a; (inversion Hxy; [ compute; exact I | ]); subst x; subst y.
    {

    }

    (* destruct a. inversion Hxy.  compute; exact I. *)
    
    
    inversion H.
    


    admit.
    admit.



    generalize dependent Hxy.
    generalize dependent H.
    compute.

    cbv.

    simpl.


  Admitted.


  Lemma t :
        forall x y a, weak termLTS x (Some a) y -> 
                      Bool.Is_true (set_mem label_eq a (next_strong_labels x)).
  Proof. intros x y a Hxy; unfold Bool.Is_true.
    inversion Hxy as [? [? [Hpre Hstr Hpost]]].

    destruct Hpre.

    admit.


    - inversion Hstr.
      + subst x1; subst x; subst a0. 
        induction a; compute; exact I.
      + subst x; subst x1; subst a0.
        induction a. 
        induction H. *)
  
  Inductive red_ctx : term -> Prop :=
  | rc_trec : red_ctx trec
  | rc_tend : red_ctx tend
  | rc_tfix : forall y, red_ctx y -> red_ctx (tfix y)
  | rc_tact : forall a y, red_ctx y -> red_ctx (tact a y)
  | rc_tpar : forall l r, red_ctx l -> red_ctx r -> red_ctx (tpar l r)
  | rc_tseq : forall l r, red_ctx l -> red_ctx r -> red_ctx (tseq l r)
  .

  Inductive silent_red_ctx : term -> Prop :=
  | tau_rc_trec : silent_red_ctx trec
  | tau_rc_tend : silent_red_ctx tend
  | tau_rc_tfix : forall y, silent_red_ctx y -> silent_red_ctx (tfix y)
  | tau_rc_tact : forall m y, silent_red_ctx (tact m y)
  | tau_rc_tpar : forall l r, silent_red_ctx l -> silent_red_ctx r -> silent_red_ctx (tpar l r)
  | tau_rc_tseq : forall l r, silent_red_ctx l -> silent_red_ctx r -> silent_red_ctx (tseq l r)
  .

  (* Lemma t : forall x y, silent x y -> *)

  (* Lemma t : forall x, silent_red_ctx x -> red_ctx x. *)
  Lemma t : forall x, red_ctx x -> silent_red_ctx x.
  Proof. intros. 
    (* induction x. *)
    
    induction H. 
    constructor. 
    constructor. 
    constructor. 
      (* apply IHsilent_red_ctx. *)
      apply IHred_ctx.
    
    - subst y.
    (* admit. *)

    constructor. apply IHsilent_red_ctx1. apply IHsilent_red_ctx2.
    constructor. apply IHsilent_red_ctx1. apply IHsilent_red_ctx2.
End Definitions.

(****************************************************************************)
Section Lemmas.
  Context {M L : Type} (lts : LTS M L).

  (* NOTE: Reduction Context -- list of terms up-to next strong action. *)
  Definition rctx_upto_strong (x : M) (f : M -> list M) : list M := f x.

  (* NOTE: Reduction Context -- list of labels of next strong action. *)
  Definition rctx_next_strong (x : M) (f : M -> list L) : list L := f x.

  (* Inductive silent_reduction_ctx (x : M) (f : M -> list M) : Set :=
  | rc_nil : silent_reduction_ctx x f
  | rc_cons : 
  . *)

  (****************************************************************************)
  (* Lemma t (Heq : forall (a b : L), {a = b} + {a <> b}) (f : M -> list L) :
        forall x y a, weak lts x (Some a) y -> 
                      Bool.Is_true (set_mem Heq a (rctx_next_strong x f)).
  Proof. intros x y a Hxy; unfold Bool.Is_true.
    inversion Hxy as [? [? [Hpre Hstr Hpost]]].



  Admitted. *)


  (****************************************************************************)
  (* Lemma t :
        forall x y a, weak lts x (Some a) y -> set_mem label_eq a (next_weak_labels x).

  Lemma silent_reduction_context :
        forall x y, silent lts x y -> silent_red_ctx x = y. *)

  
  (****************************************************************************)
  (****************************************************************************)
  (****************************************************************************)

  (****************************************************************************)
  Lemma tau_is_silent1 : forall x y, lts x None y -> silent1 lts x y.
  Proof. intros x y Hxy. info_auto with rel_db. Qed.

  Lemma silent1_is_silent : forall x y, silent1 lts x y -> silent lts x y.
  Proof. intros x y Hxy. info_auto with rel_db. Qed.

  Lemma tau_silent_is_silent1 : 
        forall x y z, tau lts x y -> silent lts y z -> silent1 lts x z.
  Proof. intros x y z Hxy Hyz. info_eauto with rel_db. Qed. 
  
  Lemma strong_is_weak : 
        forall x y a, lts x (Some a) y -> weak lts x (Some a) y.
  Proof. intros x y a Hxy. exists x, y. info_auto with rel_db. Qed.
  
  (****************************************************************************)
  Lemma expand_silent1 : 
        forall x y z, silent1 lts x y -> silent lts y z -> 
                      silent1 lts x z.
  Proof. intros x y z Hxy Hyz. induction Hxy; eapply clos_rt_clos_t.
    { exact H. } { exact Hyz. } { exact H. } 
    { apply clos_t_clos_rt, IHHxy; exact Hyz. }
  Qed.

  Lemma do_expand_silent1 : 
        forall x z, silent1 lts x z -> 
          exists y, silent1 lts x y -> silent lts y z.
  Proof. intros x z Hxz.
    induction Hxz; exists y; intros; 
      [apply rt1n_refl | apply clos_t_clos_rt in Hxz; apply Hxz ].
  Qed.

  (****************************************************************************)
  Lemma silent1_pre_weak : 
        forall x y z a, silent1 lts x y -> lts y (Some a) z -> 
                        weak lts x (Some a) z.
  Proof. intros x y z a Hxy Hyz. unfold weak. exists y, z.
    apply Pack_weak; [ apply silent1_is_silent, Hxy | apply Hyz | apply rt1n_refl ].
  Qed.
  
  Lemma silent1_post_weak : 
        forall x y z a, lts x (Some a) y -> silent1 lts y z ->
                        weak lts x (Some a) z.
  Proof. intros x y z a Hxy Hyz. unfold weak. exists x, y.
    apply Pack_weak; [ apply rt1n_refl | apply Hxy | apply silent1_is_silent, Hyz ].
  Qed.
  
  Lemma silent1_both_weak : 
        forall w x y z a, silent1 lts w x -> lts x (Some a) y -> 
                          silent1 lts y z -> weak lts w (Some a) z.
  Proof. intros w x y z a Hwx Hxy Hyz. unfold weak. exists x, y.
    apply Pack_weak; 
      [ apply silent1_is_silent, Hwx | apply Hxy | apply silent1_is_silent, Hyz ].
  Qed.

  (****************************************************************************)
  Lemma silent_weak_expands_weak : 
        forall x y z a, silent lts x y -> weak lts y (Some a) z -> 
                        weak lts x (Some a) z.
  Proof. intros x y z a Hxy Hyz. 
    inversion Hyz as [ ?y [ ?z [ Hpre Hstr Hpost ]]]; exists y0, z0. 
    apply Pack_weak; [ | apply Hstr | apply Hpost ].
    induction Hxy; [ apply Hpre | ].
    eapply rt1n_trans; [ apply H | ].
    apply IHHxy; [ apply Hyz | apply Hpre ]. 
  Qed. 

  Lemma weak_silent_expands_weak : 
        forall x y z a, weak lts x (Some a) y -> silent lts y z -> 
                        weak lts x (Some a) z.
  Proof. intros x y z a Hxy Hyz.
    inversion Hxy as [ ?x [ ?y [ Hpre Hstr Hpost ]]]; exists x0, y0.
    apply Pack_weak; [ apply Hpre | apply Hstr | ].
    silent_to_clos_rt; apply (@rt_trans _ _ y0 y z); [ apply Hpost | apply Hyz ].
  Qed.  

  (****************************************************************************)
  Lemma do_expand_weak_pre_silent1 : 
        forall x y z a, weak lts x (Some a) z -> silent1 lts x y -> 
                        weak lts y (Some a) z ->
            exists w v, silent lts y w -> lts w (Some a) v -> silent lts v z.
  Proof. intros x y z a Hxz Hxy Hyz. info_eauto with rel_db. Qed.

  (****************************************************************************)
  (****************************************************************************)
  (****************************************************************************)

  Lemma weak_to_strong :
        forall x z a, weak lts x (Some a) z -> 
          exists y w, silent lts x y /\ lts y (Some a) w /\ silent lts w z.
  Proof. intros x z a Hxz. inversion Hxz as [y [w [Hpre Hstr Hpost]]]. 
    exists y, w. split; [ apply Hpre | split; [ apply Hstr | apply Hpost ]].
  Qed.

  (* Lemma ssss :
        forall x z a, weak lts x (Some a) z -> *)


  (* Lemma weak_to_strong2 :
        forall x z a, weak lts x (Some a) z -> 
            exists w, silent lts w z /\ 
            forall y, lts y (Some a) w -> silent lts x y.
  Proof. intros x z a Hxz. inversion Hxz as [y [w [Hpre Hstr Hpost]]]. 
    exists w. split; [ apply Hpost | ].
    intros. 
    eauto with rel_db.
    split; [ apply Hpre | split; [ apply Hstr | apply Hpost ]].
  Qed. *)

End Lemmas.

(* 
Ltac determine_term_of_strong :=
  match goal with
  | [ Hx0 : ?x0 = ?xt0
    , Htx0_pre : @silent ?M ?A ?lts ?x0 ?x0b 
    , Htx0_str : ?lts ?x0b (Some ?a) ?x1a 
    , Htx0_post : @silent ?M ?A ?lts ?x1a ?x1 
    |- _ ] => idtac
  | |- _ => fail 1
  end.
*)


  Example z : term := tseq (tpar (tact (recv A) tend) 
                                 (tact (send A) tend)) 
                           (tfix (tseq (tpar (tact (send A) tend) 
                                             (tact (recv A) tend)) trec)).



  Compute (rctx_upto_strong z silent_upto_strong).
  Compute (rctx_next_strong z next_weak_labels).


(****************************************************************************)
Ltac wsim_case_weak_action_filter_from_silent Htx :=
  let xa := fresh "xa0" in let xb := fresh "xb0" in 
  let xc := fresh "xc0" in let xd := fresh "xd0" in 
  let Hpre_tau := fresh "Hpre_tau0" in 
  let Hpre_cft := fresh "Hpre_cft0" in 
  let Hstr := fresh "Hstr0" in 
  let Hpost := fresh "Hpost0" in 
  inversion Htx as [xa [xd [[| xb xc Hpre_tau Hpre_cft] Hstr Hpost]]]; 
  [ destruct Hpost; inversion Hstr | ].

(****************************************************************************)
Ltac wsim_case_weak_action := 
  let nx := fresh "x0" in intros nx;
  let a := fresh "a0" in intros a;
  let Htx := fresh "Htx0" in intros Htx;
  match goal with
  | [ Hx : ?cx = ?vx, Hy : ?cy = ?vy
    , Htx : @weak _ _ _ ?cx (Some a) nx 
    |- exists ny, @weak _ _ _ ?cy (Some a) ny /\ @weak_sim _ _ _ _ _ nx ny ] => 
      rewrite Hy
  end.

Ltac wsim_case_silent_action := 
  let nx := fresh "x0" in intros nx;
  let Htx := fresh "Htx0" in intros Htx;
  match goal with
  | [ Hx : ?cx = ?vx, Hy : ?cy = ?vy
    , Htx : @silent1 _ _ _ ?cx nx 
    |- exists ny, @silent _ _ _ ?cy ny /\ @weak_sim _ _ _ _ _ nx ny ] => 
      rewrite Hy
  end.

(****************************************************************************)
(* NOTE: called at the beginning of each new proof that terms are wsim. *)
Ltac wsim_new_cofix :=
  let CH := fresh "CH0" in cofix CH; 
  apply In_sim, Pack_sim; [ wsim_case_weak_action | wsim_case_silent_action ].
Tactic Notation "wsim_next" := wsim_new_cofix.

(* NOTE: entrypoint, handles first cofix before [wsim_new_cofix]. *)
Ltac wsim_pre_cofix_unfold :=
  let x := fresh "x0" in intros x;
  let y := fresh "y0" in intros y;
  let Hx := fresh "Hx0" in intros Hx;
  let Hy := fresh "Hy0" in intros Hy;
  match goal with 
  | [ Hx : x = ?vx, Hy : y = ?vy 
    |- @weak_sim _ _ _ _ _ x y ] => unfold vx, vy in Hx, Hy; wsim_new_cofix
  end.
Tactic Notation "solve_wsim" := wsim_pre_cofix_unfold.

(****************************************************************************)
Section Test2.
  Example p : term := tfix (tseq (tpar (tact (send A) tend) 
                                       (tact (recv A) tend)) trec).

  Example q : term := tfix (tseq (tpar (tact (recv A) tend) 
                                       (tact (send A) tend)) 
                                 (tseq (tpar (tact (send A) tend) 
                                             (tact (recv A) tend)) trec)).
  
  Example r : term := tseq (tpar (tact (recv A) tend) 
                                 (tact (send A) tend)) 
                           (tfix (tseq (tpar (tact (send A) tend) 
                                             (tact (recv A) tend)) trec)).

  (****************************************************************************)
  (* NOTE: quick checks to see if current ltac breaks any of the other cases *)
  Example wsim_test_pq : forall x y, x = p -> y = q -> weak_sim termLTS termLTS x y. Proof. solve_wsim. Admitted.
  Example wsim_test_pr : forall x y, x = p -> y = r -> weak_sim termLTS termLTS x y. Proof. solve_wsim. Admitted.
  Example wsim_test_qp : forall x y, x = q -> y = q -> weak_sim termLTS termLTS x y. Proof. solve_wsim. Admitted.
  Example wsim_test_qr : forall x y, x = q -> y = r -> weak_sim termLTS termLTS x y. Proof. solve_wsim. Admitted.
  Example wsim_test_rp : forall x y, x = r -> y = p -> weak_sim termLTS termLTS x y. Proof. solve_wsim. Admitted.
  Example wsim_test_rq : forall x y, x = r -> y = q -> weak_sim termLTS termLTS x y. Proof. solve_wsim. Admitted.

  (****************************************************************************)
  (* NOTE: below is the "hands-on" proof for testing *)
  Example wsim_testing : forall x y,
    x = p -> y = q ->
    (* x = p -> y = r -> *)
    (* x = q -> y = p -> *)
    (* x = q -> y = r -> *)
    (* x = r -> y = p -> *)
    (* x = r -> y = q -> *)
    weak_sim termLTS termLTS x y.
  Proof.
    solve_wsim.
    { 
      eapply weak_to_strong in Htx0 as [x0b [x1a [Htx_pre [Htx_str Htx_post]]]].
      
      generalize dependent Htx_pre. 
      cbv delta.
      cbv fix.
      cbv beta.
      cbv match.

      (* generalize dependent Htx_pre.  *)
      (* generalize dependent (termLTS x0 (Some a0) x1a).  *)

      match goal with
      | [ Hx0 : ?x0 = ?xt0
        , Htx0_pre : @silent ?M ?A ?lts ?x0 ?x0b 
        , Htx0_str : ?lts ?x0b (Some ?a) ?x1a 
        , Htx0_post : @silent ?M ?A ?lts ?x1a ?x1 
        |- _ ] => 
          match goal with 
          | [
            |- _ ] => idtac
          | |- _ => idtac
          end
      | |- _ => fail 1
      end.
    
      eexists. 
      (* NOTE: first need to determine [a0] -- by expanding [Htx0] *)
      (* NOTE: [einduction] keeps [Htx0] -- used by [pack_hyp_weak] *)
      einduction Htx0 as [x0b [x1a [Hpre Hstr Hpost]]].

      (* NOTE: it cannot be that [x0 = x0b] *)
      inversion Hpre as [ Heqx0b | x0c x0d Htx0c Htx0cb Heqx0db ];
        [ rewrite <- Heqx0b, Hx0 in Hstr; inversion Hstr | ]; subst x0d.

      rewrite Hx0 in Htx0c; inversion Htx0c. subst t; subst t'; rename H0 into Hx0c.

      inversion Htx0cb; subst x0b.
      { rewrite Hx0c in Hstr; inversion Hstr. subst t; subst a; subst s; subst x1a.
        (* NOTE: determine that [a0 = A] *)
        inversion H3. subst a; subst l; subst r0; subst a0; subst t';
          clear H2; rename H3 into Htx0_inner.

        (* NOTE: we can now resolve [y1] *)
        split.
        { (* NOTE: expand [y] as far as we can *)
          eapply silent1_pre_weak; [ | eapply do_seq, do_handshake ]. 
          eapply expand_silent1; [ | eapply silent1_is_silent ].
            { eapply tau_is_silent1, do_fix; compute; apply eq_refl. }
            { eapply t1n_step; unfold tau. eapply do_seq, do_comm. } }
        { (* NOTE: package [y1] *)
          move_terms_to_hypothesis.

          (* NOTE: need determine [x1] *)
          (* wsim_next. *)
          admit. 
        } }
      { (* NOTE: handle the case where [do_comm] for ever *)
        rewrite Hx0c in H; inversion H; fmt_goals.
          subst t; subst s; subst a; subst y.
        admit. 
      } }
    { exists y0. split; [ rewrite <- Hy0; eapply rt1n_refl |]. 

      (* NOTE: determine [x1] *)
      inversion Htx0; [ subst y | subst z; rename y into x0b ].
      { (* NOTE: case [termLTS x0 None x1] *)
        rewrite Hx0 in H; inversion_clear H; rename H0 into Hx1.
        
        wsim_next.
        admit. admit. 
      }
      { (* NOTE: case [termLTS x0 None x0b /\ silent termLTS x0b x1]*)
        rewrite Hx0 in H; inversion_clear H; 
          rename H0 into Htxb1; rename H1 into Hx0b.

        (* NOTE: need to determine [x1] *)
        (* wsim_next. *)
        admit. }
    }
  Admitted.
                            
(****************************************************************************)

  Theorem wsim_xy : forall x y, x = p -> y = q -> weak_sim termLTS termLTS x y.
  Proof. 
  Admitted.

  Theorem wsim_yx : forall x y, x = q -> y = p -> weak_sim termLTS termLTS x y.
  Admitted.

  Theorem wbisim_xy 
  : forall x y, x = p -> y = q -> weak_bisim termLTS termLTS x y.
  Proof.
    intros x0 y0 Hx0 Hy0; unfold q, p in Hx0, Hy0. 
    rewrite Hx0, Hy0; unfold weak_bisim. 
    split; [ apply wsim_xy | apply wsim_yx ]; trivial.
  Qed.
End Test2.

(* TODO *)
Module Test3.
  Example p : term := tpar (tfix (tact (send A) (tact (send B) trec))) 
                           (tfix (tact (recv A) (tact (recv B) trec))). 
  Example q : term := tpar (tact (send A) (tfix (tact (send B) (tact (send A) trec)))) 
                           (tact (recv A) (tfix (tact (recv B) (tact (recv A) trec)))). 
  
  Theorem ws_xy : forall x0 y0, x0 = p -> y0 = q -> weak_sim termLTS termLTS x0 y0.
  Proof. 
    intros x0 y0 Hx0 Hy0; unfold p, q in Hx0, Hy0. 
  Admitted.

  Theorem ws_yx : forall x0 y0, x0 = q -> y0 = p -> weak_sim termLTS termLTS x0 y0.
  Proof. 
    intros x0 y0 Hx0 Hy0; unfold p, q in Hx0, Hy0.
  Admitted.

  Theorem bs_xy : forall x0 y0, x0 = p -> y0 = q -> weak_bisim termLTS termLTS x0 y0.
  Proof. 
    intros x0 y0 Hx0 Hy0; unfold p, q in Hx0, Hy0. 
    rewrite Hx0, Hy0; unfold weak_bisim. 
    split; [ apply ws_xy | apply ws_yx ]; trivial.
  Qed.
End Test3.

