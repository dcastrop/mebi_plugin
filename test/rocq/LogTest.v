(** Proofs from Sylvan's certificate (doc/certificate.md). Each tactic
    decides its goal, Sylvan logs the decision's certificate, and the
    tactic closes the goal with the proof built from it; [Qed] has the
    kernel check that proof, which must be closed under the global
    context. With [Set MeBi Log Mode "keep"] the log stays in
    [.LogTest.v.mebi/<lemma>.log], next to this file, and
    [MeBi Check Log] turns its records into facts, each chunk a constant
    the kernel checks. [MeBi Prove id : goal] proves a goal given as a
    statement and declares the lemma [id], each piece of its proof a
    constant checked when declared (less memory on large goals). *)
Require Import MEBI.loader.

Module Logged.
  Inductive act := a | b.
  Inductive pr := nil | pre (x : option act) (k : pr) | par (p q : pr).
  Inductive step : pr -> option act -> pr -> Prop :=
  | s_pre x k : step (pre x k) x k
  | s_parl p q l p' : step p l p' -> step (par p q) l (par p' q)
  | s_parr p q l q' : step q l q' -> step (par p q) l (par p q').
  Definition A := pre (Some a) (pre None (pre (Some a) nil)).
  Definition B := pre (Some b) nil.

  (* the four relations, each proved: expected "Closed under the global
     context" *)
  Example s1 : strong_sim step step (par A B) (par B A).
  Proof. sim. Qed.
  Print Assumptions s1.
  Example w1 : weak_sim step step (par A B) (par B A).
  Proof. weak_sim. Qed.
  Print Assumptions w1.
  Example b1 : strong_bisimilar step step (par A B) (par B A).
  Proof. bisim. Qed.
  Print Assumptions b1.

  (* "keep": the log stays, and its facts can be checked on their own *)
  Set MeBi Log Mode "keep".
  Example logged : weak_bisimilar step step (par A B) (par B A).
  Proof. weak_bisim. Qed.
  Print Assumptions logged.
  Set MeBi Log Mode "off".
  MeBi Check Log ".LogTest.v.mebi/logged.log".
  (* its one chunk: expected "Closed under the global context" *)
  Print Assumptions mebi_log1_c1.

  (* "off" (the default): the log is deleted once the proof is built *)
  Example passing : weak_bisimilar step step (par A B) (par B A).
  Proof. weak_bisim. Qed.
  Fail MeBi Check Log ".LogTest.v.mebi/passing.log".

  (* a mode that does not exist is refused *)
  Fail Set MeBi Log Mode "temporary".

  (* a negative verdict is an error *)
  Example refused : weak_bisimilar step step (par A B) (par A A).
  Proof. Fail weak_bisim. Abort.

  (* the command: the lemma declared, each piece of its proof a constant
     checked when declared; expected "Closed under the global context" *)
  MeBi Prove proved : weak_bisimilar step step (par A B) (par B A).
  Print Assumptions proved.
  Fail MeBi Prove refused2 : weak_bisimilar step step (par A B) (par A A).
End Logged.
