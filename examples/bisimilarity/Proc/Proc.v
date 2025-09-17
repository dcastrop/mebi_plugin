Require Import MEBI.loader.
Require Coq.Program.Tactics.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.Proc.

Require Import Relation_Definitions.
Require Import Relation_Operators.
Require Operators_Properties.

(****************************************************************************)
Module FlatTests.
Import Flat.

(****************************************************************************)
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
