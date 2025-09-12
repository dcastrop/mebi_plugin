(* Require Import MEBI.loader. *)
Require Coq.Program.Tactics.

Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.
Import Flat.
Import Flat.Complex.

Require Import MEBI.Examples.bisimilarity.Proc.Test2.Terms.

Ltac wsim_try_clear_H3 :=
  match goal with 
  | [ H : ?a = ?a |- _ ] => clear H
  | |- _ => idtac (* "no H3" *)
  end.

Ltac wsim_try_inversion_H4 :=
  match goal with 
  | [ CH : @weak_sim ?M ?N ?A ?ltsM ?ltsN ?m ?n
    , H  : ?ltsM (_ ?ml ?mr) ?a (_ ?ml' ?mr')
    , H4 : ?ltsM ?ml ?a ?ml'
    |- exists n' : ?N, 
        @weak ?N ?A ?ltsN ?n n' ?a /\ 
        @weak_sim ?M ?N ?A ?ltsM ?ltsN (_ ?ml' ?mr') n'
    ] => inversion H4; subst; clear H4
  | [ CH : @weak_sim ?M ?N ?A ?ltsM ?ltsN ?m ?n
    , H  : ?ltsM (_ ?ml ?mr) ?a (_ ?ml' ?mr')
    , H4 : ?ltsM ?mr ?a ?mr'
    |- exists n' : ?N, 
        @weak ?N ?A ?ltsN ?n n' ?a /\ 
        @weak_sim ?M ?N ?A ?ltsM ?ltsN (_ ?ml' ?mr') n'
    ] => inversion H4; subst; clear H4
  | |- _ => idtac (* "no H4" *)
  end.

Ltac wsim_inversion_H :=
  match goal with 
  | [ CH : @weak_sim ?M ?N ?A ?ltsM ?ltsN ?m ?n
    , H  : ?ltsM ?m ?a ?m'
    |- exists n' : ?N, 
        @weak ?N ?A ?ltsN ?n n' ?a /\ 
        @weak_sim ?M ?N ?A ?ltsM ?ltsN ?m' n' ] 
      => 
      inversion H; subst; wsim_try_inversion_H4; clear H; wsim_try_clear_H3
  end.

Ltac wsim_handle_tsubst :=
  match goal with 
  | |- ?n = tsubst _ _ => constructor; idtac (* "?n tsubst" *)
  | |- _ => idtac (* "no tsubst" *)
  end
  ; unfold tsubst in *.

Ltac wsim_handle_wk_trans :=
  match goal with
  | |- ?ltsN ?n (Some ?a) ?n' => eauto with rel_db; do 2 constructor
                                (* ; idtac "wk some1" *)
  | |- ?ltsN ?n  None     ?n' => eauto with rel_db; do 2 constructor
                                (* ; idtac "wk none" *)
  | |- @clos_refl_trans_1n ?N ?tauN ?n ?n' => 
      match n with
      | tpar (tact (send ?a) _) (tact (recv ?b) _) => 
          tryif progress eauto with rel_db; do 3 constructor
          then eauto with rel_db (* ; idtac "wk some2" *)
          else idtac (* "wk tpar fail" *)
      | _ => 
          eapply rt1n_trans; [do 3 constructor|]
          ; unfold tsubst in * (* ; idtac "wk cont" *)
          ; wsim_handle_wk_trans
      end
  end.

Ltac wsim_handle_weak := 
  match goal with
  | |- @weak ?N ?A ?ltsN ?n ?n' (Some ?a) => eapply wk_some (* ; idtac "wk some" *)
  | |- @weak ?N ?A ?ltsN ?n ?n'  None     => eapply wk_none (* ; idtac "wk none" *)
  end
  ; unfold silent
  ; wsim_handle_wk_trans.

Ltac wsim_cofix := 
  let CH := fresh "CH0" in cofix CH
  ; apply In_sim, Pack_sim
  ; intros
  ; wsim_inversion_H
  ; wsim_handle_tsubst
  ; (eexists; split; [wsim_handle_weak|]).

Ltac wsim_next_case :=
  tryif solve [eauto with rel_db] 
  then idtac (* "solved case" *) else wsim_cofix.

Ltac wsim_begin := 
  intros; subst;
  match goal with 
  | |- @weak_sim _ _ _ _ _ ?x ?y => unfold x, y; wsim_cofix
  end.

(****************************************************************************)
Example wsim_pq : weak_sim termLTS termLTS p q. 
Proof. wsim_begin; do 7 wsim_next_case. Qed. 

Example wsim_qp : weak_sim termLTS termLTS q p. 
Proof. wsim_begin; do 9 wsim_next_case. Qed. 
  
Theorem wbisim_pq : weak_bisim termLTS termLTS p q.
Proof. unfold weak_bisim; split; [apply wsim_pq | apply wsim_qp]. Qed.

(****************************************************************************)
Example wsim_qr : weak_sim termLTS termLTS q r. 
Proof. wsim_begin; do 9 wsim_next_case. Qed. 
  
Example wsim_rq : weak_sim termLTS termLTS r q. 
Proof. wsim_begin; do 8 wsim_next_case. Qed. 
  
Theorem wbisim_qr : weak_bisim termLTS termLTS q r.
Proof. unfold weak_bisim; split; [apply wsim_qr | apply wsim_rq]. Qed.

(****************************************************************************)
Example wsim_pr : weak_sim termLTS termLTS p r. 
Proof. wsim_begin; do 10 wsim_next_case. Qed. 

Example wsim_rp : weak_sim termLTS termLTS r p. 
Proof. wsim_begin; do 9 wsim_next_case. Qed. 
  
Theorem wbisim_pr : weak_bisim termLTS termLTS p r.
Proof. unfold weak_bisim; split; [apply wsim_pr | apply wsim_rp]. Qed.