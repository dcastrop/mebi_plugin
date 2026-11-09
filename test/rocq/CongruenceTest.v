(** Structural congruence written as silent rules (doc/upto.md): the
    reorderings found in an LTS's rules (commutation, associativity, a
    swap), or given by hand ([MeBi Congruence]). Detection only: no proof
    uses them yet (CLAUDE.md, hard rule 1: certificates up to congruence
    are to come from the engine, as proof terms). *)
Require Import MEBI.loader.

(** Commutation and associativity, found in the rules. *)
Module Found.
  Inductive act := a | b | c.
  Inductive pr := nil | pre (x : option act) (k : pr) | par (p q : pr).
  Inductive step : pr -> option act -> pr -> Prop :=
  | s_pre x k : step (pre x k) x k
  | s_parl p q l p' : step p l p' -> step (par p q) l (par p' q)
  | s_parr p q l q' : step q l q' -> step (par p q) l (par p q')
  | s_comm p q : step (par p q) None (par q p)
  | s_assocl p q r : step (par p (par q r)) None (par (par p q) r)
  | s_assocr p q r : step (par (par p q) r) None (par p (par q r))
  | s_end : step (par nil nil) None nil.
  (* expected: s_comm, s_assocl, s_assocr; not s_end (it drops a value) *)
  Print MeBi Skeleton (par (pre (Some a) nil) (pre (Some b) nil)) Using step.
End Found.

(** A swap inside a structure, its own inverse: found. *)
Module Swap.
  Inductive act := a | b.
  Inductive pr := nil | pre (x : option act) (k : pr) | tri (p q r : pr).
  Inductive step : pr -> option act -> pr -> Prop :=
  | s_pre x k : step (pre x k) x k
  | s_1 p q r l p' : step p l p' -> step (tri p q r) l (tri p' q r)
  | s_2 p q r l q' : step q l q' -> step (tri p q r) l (tri p q' r)
  | s_3 p q r l r' : step r l r' -> step (tri p q r) l (tri p q r')
  | s_swap p q r : step (tri p q r) None (tri q p r).
  (* expected: s_swap *)
  Print MeBi Skeleton (tri (pre (Some a) nil) (pre (Some b) nil) nil) Using step.
End Swap.

(** A rotation without its inverse: not found (not reversible), so given
    by hand. *)
Module ByHand.
  Inductive act := a | b | c.
  Inductive pr := nil | pre (x : option act) (k : pr) | tri (p q r : pr).
  Inductive step : pr -> option act -> pr -> Prop :=
  | s_pre x k : step (pre x k) x k
  | s_1 p q r l p' : step p l p' -> step (tri p q r) l (tri p' q r)
  | s_2 p q r l q' : step q l q' -> step (tri p q r) l (tri p q' r)
  | s_3 p q r l r' : step r l r' -> step (tri p q r) l (tri p q r')
  | s_rot p q r : step (tri p q r) None (tri q r p).
  (* expected: none found *)
  Print MeBi Skeleton (tri (pre (Some a) nil) (pre (Some b) nil) (pre (Some c) nil)) Using step.
  MeBi Congruence step s_rot.
  (* expected: s_rot, given by hand *)
  Print MeBi Skeleton (tri (pre (Some a) nil) (pre (Some b) nil) (pre (Some c) nil)) Using step.
  (* a name that is not a rule of the LTS is refused *)
  Fail MeBi Congruence step s_nope.
End ByHand.
