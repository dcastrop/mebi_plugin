Require Import MEBI.loader.

MeBi Divider "Theories.Test.BasicCommands".
Module BasicCommands.

  Example z := 0.
  MeBi Config Weak As z Of nat. 
  MeBi Config Reset Weak.
  

  Inductive s_label : Set :=
  | TAU : s_label 
  | LABEL1 : nat -> s_label
  .
  MeBi Config Weak As TAU Of s_label. 
  MeBi Config Reset Weak.
  

  Inductive t_label : Type :=
  | SILENT : t_label 
  | LABEL2 : bool -> t_label
  .
  MeBi Config Weak As SILENT Of t_label. 
  MeBi Config Reset Weak.
  
End BasicCommands.

MeBi Config Output "Debug" False.
MeBi Config Output "Info" False.
MeBi Config Output "Notice" False.
MeBi Config Output "Warning" True.
MeBi Config Output "Error" True.
MeBi Config Output "Trace" False.
MeBi Config Output "Result" False.
MeBi Config Output "Show" False.
MeBi Config Output "DecodeResults" False.
MeBi Config Output "DumpResults" False.

MeBi Divider "Theories.Test.GeneralTests".
Inductive i := C0 (i : nat) | C1 (b : bool) (j : nat) | C2 (x : nat).

Fail MeBi Run LTS 0 Using i.
Fail MeBi Run LTS 0 Using j.

Definition k := 0.
Fail MeBi Run LTS 0 Using k.
Fail MeBi Run LTS 0 Using nat.

Definition nnat := nat.
Fail MeBi Run LTS 0 Using nnat.
Fail MeBi Run LTS 0 Using False.

CoInductive co_nat := CoZ | CoS : co_nat -> co_nat.

Inductive test_lts (A:Type) : co_nat -> nat -> nat -> Prop :=
| less_lt (x : A) (i : co_nat) (j : nat) : test_lts A (CoS i) 1 j.

Fail MeBi Run LTS 0 Using test_lts.

(* TODO: [Auto Template Polymorphism] automatically enabled here... *)
Inductive test_mut (A:Type) : Prop := Mk1 (x : A) (y : test_mut2 A)
with test_mut2 (A:Type) : Prop := Mk2 (y : test_mut A).

Fail MeBi Run LTS 0 Using test_mut2.


Inductive testLTS : nat -> bool -> nat -> Prop :=
  | test1 n : testLTS (S n) true n
  | test2 : testLTS (S 0) false (S 0).

Definition one := 1.

Fail MeBi Run LTS false Using testLTS.

MeBi Run LTS 0 Using testLTS.
MeBi Run LTS (S 0) Using testLTS.
MeBi Run LTS (S (S 0)) Using testLTS.
MeBi Run LTS (S (S (S 0))) Using testLTS.

MeBi Run LTS one Using testLTS.
MeBi Run LTS (S one) Using testLTS.

MeBi Run LTS (S one) Using testLTS.


Inductive nonTerminatingTestLTS : nat -> bool -> nat -> Prop :=
  | test1' n : nonTerminatingTestLTS n true (S n)
  | test2' n : nonTerminatingTestLTS (S n) false n
  .

(* below cannot be finitely represented  *)
(* MeBi Config Fail If Incomplete True. *)
Fail MeBi Run LTS 0 Using nonTerminatingTestLTS.
Fail MeBi Run LTS (S 0) Using nonTerminatingTestLTS.
Fail MeBi Run LTS (S (S 0)) Using nonTerminatingTestLTS.
Fail MeBi Run LTS (S (S (S 0))) Using nonTerminatingTestLTS.

MeBi Divider "Theories.Test.Test1".
Module Test1.
  Inductive action : Set := | TheAction1 | TheAction2.
  Inductive term : Set :=
  | trec : term
  | tend : term
  | tfix : term -> term
  | tact : action -> term -> term
  .

  Fixpoint subst (t1 : term) (t2 : term) :=
    match t2 with
    | trec => t1
    | tend => tend
    | tfix t => tfix t
    | tact a t => tact a (subst t1 t)
    end.

  Inductive termLTS : term -> action -> term -> Prop :=
  | do_act : forall a t, termLTS (tact a t) a t

  | do_fix : forall a t t',
      termLTS (subst (tfix t) t) a t' ->
      termLTS (tfix t) a t'.

  MeBi Run LTS (tfix (tact TheAction1 tend)) Using termLTS.
  MeBi Run LTS (tfix (tact TheAction1 (tact TheAction2 trec))) Using termLTS.
End Test1.

MeBi Divider "Theories.Test.Test2".
Module Test2.
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

  MeBi Run LTS (tfix (tact TheAction1 tend)) Using termLTS.
  MeBi Run LTS (tfix (tact TheAction1 (tact TheAction2 trec))) Using termLTS.
  MeBi Run LTS (tfix (tpar TheAction1 TheAction2 trec)) Using termLTS.
  MeBi Run LTS (tfix (tpar TheAction1 TheAction2 trec)) Using termLTS.
End Test2.

MeBi Divider "Theories.Test.BisimTest1".
(* MeBi Config Fail If Incomplete True. *)
MeBi Config Reset Weak.
Module BisimTest1.
  Inductive action : Set := | TheAction1 | TheAction2.

  Inductive term : Set :=
  | trec : term
  | tend : term
  | tfix : term -> term
  | tact : action -> term -> term
  | tpar : action -> action -> term -> term.

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

  (* true *)
  MeBi Run Bisim (tact TheAction1 tend) With termLTS
         And (tact TheAction1 tend) With termLTS
         Using termLTS.

  (* true *)
  MeBi Run Bisim (tact TheAction2 tend) With termLTS
         And (tact TheAction2 tend) With termLTS
         Using termLTS.

  (* false *)
  Fail 
  MeBi Run Bisim (tact TheAction1 tend) With termLTS
         And (tact TheAction2 tend) With termLTS
         Using termLTS.

  (* false *)
  Fail 
  MeBi Run Bisim (tact TheAction2 tend) With termLTS
         And (tact TheAction1 tend) With termLTS
         Using termLTS.

  (* true *)
  MeBi Run Bisim (tact TheAction1 (tact TheAction2 tend)) With termLTS
         And (tact TheAction1 (tact TheAction2 tend)) With termLTS
         Using termLTS.

  (* true *)
  MeBi Run Bisim (tact TheAction2 (tact TheAction1 tend)) With termLTS
         And (tact TheAction2 (tact TheAction1 tend)) With termLTS
         Using termLTS.

  (* false *) 
  Fail 
  MeBi Run Bisim (tact TheAction1 (tact TheAction2 tend)) With termLTS
         And (tact TheAction2 (tact TheAction1 tend)) With termLTS
         Using termLTS.

  (* true *)
  MeBi Run Bisim (tpar TheAction1 TheAction2 tend) With termLTS
         And (tpar TheAction1 TheAction2 tend) With termLTS
         Using termLTS.

  (* true *)
  MeBi Run Bisim (tpar TheAction1 TheAction2 tend) With termLTS
         And (tpar TheAction2 TheAction1 tend) With termLTS
         Using termLTS.

  (* true *)
  MeBi Run Bisim (tpar TheAction2 TheAction1 tend) With termLTS
         And (tpar TheAction1 TheAction2 tend) With termLTS
         Using termLTS.

  (* false *)
  Fail 
  MeBi Run Bisim (tpar TheAction1 TheAction1 tend) With termLTS
         And (tact TheAction1 tend) With termLTS
         Using termLTS.

  (* false *)
  Fail 
  MeBi Run Bisim (tpar TheAction1 TheAction2 tend) With termLTS
         And (tact TheAction1 tend) With termLTS
         Using termLTS.

  (* true *)
  MeBi Run Bisim (tfix (tact TheAction1 trec)) With termLTS
         And (tfix (tact TheAction1 trec)) With termLTS
         Using termLTS.

  (* true *)
  MeBi Run Bisim (tfix (tact TheAction1 trec)) With termLTS
         And (tact TheAction1 (tfix (tact TheAction1 trec))) With termLTS
         Using termLTS.

  (* true *)
  MeBi Run Bisim (tfix (tact TheAction1 (tact TheAction2 trec))) With termLTS
         And (tact TheAction1 (tact TheAction2 (tfix (tact TheAction1 (tact TheAction2 trec))))) With termLTS
         Using termLTS.

  (* saturate *)
  MeBi Run Saturate 
    (tact TheAction1 (tact TheAction2 (tfix (tact TheAction1 (tact TheAction2 trec))))) 
    Using termLTS.

  (* minimize *)
  MeBi Run Minimize 
    (tact TheAction1 (tact TheAction2 (tfix (tact TheAction1 (tact TheAction2 trec))))) 
    Using termLTS.
End BisimTest1.

MeBi Divider "Theories.Test.BisimTest2".
(* MeBi Config Fail If Incomplete True. *)
Module BisimTest2.
  Inductive action : Set := | TAU | TheAction1 | TheAction2.

  Inductive term : Set :=
  | trec : term
  | tend : term
  | tfix : term -> term
  | tact : action -> term -> term
  | tpar : action -> action -> term -> term.

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

  | do_fix : forall t, termLTS (tfix t) TAU (subst (tfix t) t).

  (* MeBi Config WeakMode Disable. *)
  MeBi Config Reset Weak.

  Example exa1 := (tact TheAction1 (tact TheAction2 (tfix (tact TheAction1 (tact TheAction2 trec))))).
  MeBi Run FSM exa1 Using termLTS.
  MeBi Run Bisim exa1 With termLTS And exa1 With termLTS Using termLTS.

  (* MeBi Config WeakMode Enable. *)
  MeBi Config Weak As TAU Of action.

  MeBi Run FSM exa1 Using termLTS.
  MeBi Run Saturate exa1 Using termLTS.
  MeBi Run Minimize exa1 Using termLTS.

  MeBi Run Bisim exa1 With termLTS And exa1 With termLTS Using termLTS.

  Example exa2 := (tfix (tact TheAction1 (tact TheAction2 trec))).
  MeBi Run FSM exa2 Using termLTS.
  MeBi Run Saturate exa2 Using termLTS.
  MeBi Run Minimize exa2 Using termLTS.
  
  Example exa3 := (tact TheAction1 (tfix (tact TheAction2 (tact TheAction1 trec)))).
  MeBi Run FSM exa3 Using termLTS.
  MeBi Run Saturate exa3 Using termLTS.
  MeBi Run Minimize exa3 Using termLTS.
  
  MeBi Run Bisim exa1 With termLTS And exa2 With termLTS Using termLTS.
  MeBi Run Bisim exa1 With termLTS And exa3 With termLTS Using termLTS.

  MeBi Run Bisim exa2 With termLTS And exa1 With termLTS Using termLTS.
  MeBi Run Bisim exa2 With termLTS And exa3 With termLTS Using termLTS.

  MeBi Run Bisim exa3 With termLTS And exa1 With termLTS Using termLTS.
  MeBi Run Bisim exa3 With termLTS And exa2 With termLTS Using termLTS.
End BisimTest2.

MeBi Divider "Theories.Test.BisimTest3".
(* MeBi Config Fail If Incomplete True.  *)
Module BisimTest3.
  Inductive action : Set := | TheAction1 | TheAction2.

  Inductive term : Set :=
  | trec : term
  | tend : term
  | tfix : term -> term
  | tact : action -> term -> term
  | tpar : action -> action -> term -> term.

  Fixpoint subst (t1 : term) (t2 : term) :=
    match t2 with
    | trec => t1
    | tend => tend
    | tfix t => tfix t
    | tact a t => tact a (subst t1 t)
    | tpar a b t => tpar a b (subst t1 t)
    end.

  Inductive termLTS : term -> option action -> term -> Prop :=
  | do_act : forall a t, termLTS (tact a t) (Some a) t

  | do_par1 : forall a b t, termLTS (tpar a b t) (Some a) (tact b t)

  | do_par2 : forall a b t, termLTS (tpar a b t) (Some b) (tact a t)

  | do_fix : forall t, termLTS (tfix t) None (subst (tfix t) t).

  MeBi Config Reset Weak.
  (* MeBi Config WeakMode Disable. *)

  Example exa1 := (tact TheAction1 (tact TheAction2 (tfix (tact TheAction1 (tact TheAction2 trec))))).
  MeBi Run FSM exa1 Using termLTS.
  MeBi Run Bisim exa1 With termLTS And exa1 With termLTS Using termLTS.

  (* MeBi Config WeakMode Enable. *)
  MeBi Config Reset Weak.
  MeBi Config Weak As Option action.

  MeBi Run FSM exa1 Using termLTS.
  MeBi Run Saturate exa1 Using termLTS.
  MeBi Run Minimize exa1 Using termLTS.

  MeBi Run Bisim exa1 With termLTS And exa1 With termLTS Using termLTS.

  Example exa2 := (tfix (tact TheAction1 (tact TheAction2 trec))).
  MeBi Run FSM exa2 Using termLTS.
  MeBi Run Saturate exa2 Using termLTS.
  MeBi Run Minimize exa2 Using termLTS.
  
  Example exa3 := (tact TheAction1 (tfix (tact TheAction2 (tact TheAction1 trec)))).
  MeBi Run FSM exa3 Using termLTS.
  MeBi Run Saturate exa3 Using termLTS.
  MeBi Run Minimize exa3 Using termLTS.
  
  MeBi Run Bisim exa1 With termLTS And exa2 With termLTS Using termLTS.
  MeBi Run Bisim exa1 With termLTS And exa3 With termLTS Using termLTS.

  MeBi Run Bisim exa2 With termLTS And exa1 With termLTS Using termLTS.
  MeBi Run Bisim exa2 With termLTS And exa3 With termLTS Using termLTS.

  MeBi Run Bisim exa3 With termLTS And exa1 With termLTS Using termLTS.
  MeBi Run Bisim exa3 With termLTS And exa2 With termLTS Using termLTS.
End BisimTest3.

(* MeBi Divider "Theories.Test.ProofTest".
Module ProofTest.

  MeBi Config Reset.
  MeBi Set ShowDebug True.
  MeBi Set ShowDetails True.

(* Example bool_assoc : forall a b c, (a -> b) -> (b -> c) -> (a -> c).
Proof.
  (* intros.  *)
  (* MeBi_intro. *)
  
  (* intros a. *)
  (* MeBi_intro a. *)
  (* MeBi_intro "a". *)
  (* MeBi_intros a. *)
  MeBi_intros_only x y z.


Admitted. *)


End ProofTest. *)


(* Section BisimDef.
  Context (Term1 Term2 : Set)  (Action1 Action2 : Set)
    (LTS1 : Term1 -> Action1 -> Term1 -> Prop)
    (LTS2 : Term2 -> Action2 -> Term2 -> Prop).

  CoInductive sim (s : Term1) (t : Term2) : Prop :=
  | Bisim :
         (forall s' a1, LTS1 s a1 s'
           -> exists t', exists a2, (LTS2 t a2 t') /\ (bisim s' t'))
      -> (forall t' a2, LTS2 t a2 t'
         -> exists s', exists a1, (LTS1 s a1 s') /\ (bisim s' t'))
     -> bisim s t.
End BisimDef. *)


(* Cannot capture things like below due to cases like [tfix t --> tfix (tfix
 t')] leading to infinite states *)
(* Module Test3.
  Inductive action : Set := | TheAction1 | TheAction2 | Collapse.
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
      termLTS (tfix t) a (tfix t')
  | do_collapse : forall t, termLTS (tfix (tfix t)) Collapse (tfix t).

  MeBi Run LTS termLTS (tfix (tact TheAction1 tend)).
  MeBi Run LTS termLTS (tfix (tact TheAction1 (tact TheAction2 trec))).

  MeBi Run LTS termLTS (tfix (tpar TheAction1 TheAction2 trec)).
End Test3. *)

(* FIXME: The case below is hard to implement. *)
(* Solution 1:
   - Formalise CIC inductive types to state machines (bounded). Throw error if LTS definition does
not match input shape (e.g. we can simply prevent these "shape restrictions" as below, and throw
an error stating we do not support them).
   - Do a "heuristic" approach. Any parameter, e.g. "forall a t t'", instantiate with metavariables.
Any term that *depends* on parameters ("a", "t", "t'"), try to find all terms that inhabit it, and
try to find all possible ways that this constructor can be instantiated. If we can't figure out if it
is inhabited, or we have no way to check if we exhaustively cover all possible cases, fail.
 *)
(* Module Test4.
  Inductive action : Set := | TheAction1 | TheAction2 | Collapse.
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

  Definition not_fix t :=
    match t with
    | tfix _ => False
    | _ => True
    end.

  Inductive termLTS : term -> action -> term -> Prop :=
  | do_act : forall a t, termLTS (tact a t) a t

  | do_par1 : forall a b t, termLTS (tpar a b t) a (tact b t)

  | do_par2 : forall a b t, termLTS (tpar a b t) b (tact a t)

  | do_fix : forall a t t',
      not_fix t ->
      termLTS (subst (tfix t) t) a t' ->
      termLTS (tfix t) a (tfix t')
  | do_collapse : forall t, termLTS (tfix (tfix t)) Collapse (tfix t).

  MeBi Run LTS termLTS (tfix (tact TheAction1 tend)).
  MeBi Run LTS termLTS (tfix (tact TheAction1 (tact TheAction2 trec))).

  MeBi Run LTS termLTS (tfix (tpar TheAction1 TheAction2 trec)).
End Test4. *)




(* (*** Printing user inputs ***) *)

(* Definition definition := 5. *)
(* What's definition. *)
(* What kind of term is definition. *)
(* What kind of identifier is definition. *)

(* What is 1 2 3 a list of. *)
(* What is a list of. (* no arguments = empty list *) *)

(* Is 1 2 3 nonempty. *)
(* (* Is nonempty *) (* does not parse *) *)

(* And is 1 provided. *)
(* And is provided. *)

(* (*** Interning terms ***) *)

(* Intern 3. *)
(* Intern definition. *)
(* Intern (fun (x : Prop) => x). *)
(* Intern (fun (x : Type) => x). *)
(* Intern (forall (T : Type), T). *)
(* Intern (fun (T : Type) (t : T) => t). *)
(* Intern _. *)
(* Intern (Type : Type). *)

(* (*** Defining terms ***) *)

(* MyDefine n := 1. *)
(* Print n. *)

(* MyDefine f := (fun (x : Type) => x). *)
(* Print f. *)

(* (*** Printing terms ***) *)

(* MyPrint f. *)
(* MyPrint n. *)
(* Fail MyPrint nat. *)

(* DefineLookup n' := 1. *)
(* DefineLookup f' := (fun (x : Type) => x). *)

(* (*** Checking terms ***) *)

(* Check1 3. *)
(* Check1 definition. *)
(* Check1 (fun (x : Prop) => x). *)
(* Check1 (fun (x : Type) => x). *)
(* Check1 (forall (T : Type), T). *)
(* Check1 (fun (T : Type) (t : T) => t). *)
(* Check1 _. *)
(* Check1 (Type : Type). *)

(* Check2 3. *)
(* Check2 definition. *)
(* Check2 (fun (x : Prop) => x). *)
(* Check2 (fun (x : Type) => x). *)
(* Check2 (forall (T : Type), T). *)
(* Check2 (fun (T : Type) (t : T) => t). *)
(* Check2 _. *)
(* Check2 (Type : Type). *)

(* (*** Convertibility ***) *)

(* Convertible 1 1. *)
(* Convertible (fun (x : Type) => x) (fun (x : Type) => x). *)
(* Convertible Type Type. *)
(* Convertible 1 ((fun (x : nat) => x) 1). *)

(* Convertible 1 2. *)
(* Convertible (fun (x : Type) => x) (fun (x : Prop) => x). *)
(* Convertible Type Prop. *)
(* Convertible 1 ((fun (x : nat) => x) 2). *)

(* (*** Introducing variables ***) *)

(* Theorem foo: *)
(*   forall (T : Set) (t : T), T. *)
(* Proof. *)
(*   my_intro T. my_intro t. apply t. *)
(* Qed. *)

(* (*** Exploring proof state ***) *)

(* Fail ExploreProof. (* not in a proof *) *)

(* Theorem bar: *)
(*   forall (T : Set) (t : T), T. *)
(* Proof. *)
(*   ExploreProof. my_intro T. ExploreProof. my_intro t. ExploreProof. apply t. *)
(* Qed. *)

(* Regression test for the "multiple actionpairs" branch of
   [Proof_solver_step.ReModel.transition] (backlog item A2, fix 6124eeb).

   [do_par1] and [do_par2] coincide when [a = b], so the step
   [tpar A A t -A-> tact A t] has two derivations. The unsaturated FSM keeps
   one [Action.t] per derivation tree, so resolving that hypothesis finds two
   candidates. Before 6124eeb this raised and the proof below could not be
   built; it now picks the shorter annotation. Verified both ways on
   2026-10-01: with the old raising behaviour patched back in, this proof
   fails at that branch. *)
MeBi Divider "Theories.Test.MultipleDerivations".
Require Import MEBI.Bisimilarity.
Module MultipleDerivations.
  Inductive action : Set := | A | B.

  Inductive term : Set :=
  | trec : term
  | tend : term
  | tfix : term -> term
  | tact : action -> term -> term
  | tpar : action -> action -> term -> term.

  Fixpoint subst (t1 : term) (t2 : term) :=
    match t2 with
    | trec => t1
    | tend => tend
    | tfix t => tfix t
    | tact a t => tact a (subst t1 t)
    | tpar a b t => tpar a b (subst t1 t)
    end.

  Inductive termLTS : term -> option action -> term -> Prop :=
  | do_act : forall a t, termLTS (tact a t) (Some a) t
  | do_par1 : forall a b t, termLTS (tpar a b t) (Some a) (tact b t)
  | do_par2 : forall a b t, termLTS (tpar a b t) (Some b) (tact a t)
  | do_fix : forall t, termLTS (tfix t) None (subst (tfix t) t).

  MeBi Config Reset Weak.
  MeBi Config Weak As Option action.

  Example p : term := tfix (tpar A A trec).
  Example q : term := tfix (tact A (tact A trec)).

  Example wsim_pq : weak_sim termLTS termLTS p q.
  Proof. MeBi Sim Begin termLTS p And termLTS q Using termLTS.
    MeBi Sim Solve 1000. Qed.
End MultipleDerivations.

(* Regression tests for the size check before saturation (backlog item H2)
   and for the premise extraction does not check (I2). A plugin [Warning]
   cannot be asserted from a .v file, only an error, so these pin down the
   refusals and that each way out works. *)
MeBi Divider "Theories.Test.SaturationGuard".
Module SaturationGuard.
  Import MultipleDerivations.
  MeBi Config Reset Weak.
  MeBi Config Weak As Option action.

  (* [p] saturates to a handful of weak actions: over a bound of 1.
     [Run Saturate] and [Run Minimize] need the whole saturated FSM, so they
     are refused. *)
  MeBi Config Bounds Saturation 1.
  Fail MeBi Run Saturate p Using termLTS.
  Fail MeBi Run Minimize p Using termLTS.

  (* The bisimilarity check saturates on demand instead (since 2026-10-03,
     notes/13; it warns): each state when asked about, the partition on the
     silent-SCC quotient. The proof is the same proof, step for step: the
     count is pinned below the bound, saturated whole, and at bound 1. *)
  MeBi Run Bisim p With termLTS And q With termLTS Using termLTS.
  Example wsim_on_demand : weak_sim termLTS termLTS p q.
  Proof. MeBi Sim Begin termLTS p And termLTS q Using termLTS. MeBi Sim Solve 1000. Qed.
  MeBi Config Reset Bounds.
  MeBi Config Saturation OnDemand True.
  Example wsim_forced : weak_sim termLTS termLTS p q.
  Proof. MeBi Sim Begin termLTS p And termLTS q Using termLTS. MeBi Sim Solve 1000. Qed.
  MeBi Config Saturation OnDemand False.
  Example wsim_whole : weak_sim termLTS termLTS p q.
  Proof. MeBi Sim Begin termLTS p And termLTS q Using termLTS. MeBi Sim Solve 1000. Qed.

  (* On demand, settings that plan the whole proof up front are refused
     (they would walk every reachable pair, saturating as they go); [Auto]
     takes the nested cofix instead, with a notice. *)
  MeBi Config Saturation OnDemand True.
  MeBi Config Solver MutualCofix True.
  Example wsim_mutual : weak_sim termLTS termLTS p q.
  Proof. Fail MeBi Sim Begin termLTS p And termLTS q Using termLTS. Abort.
  MeBi Config Solver MutualCofix Auto.
  MeBi Config Solver Answers Greedy.
  Example wsim_greedy : weak_sim termLTS termLTS p q.
  Proof. Fail MeBi Sim Begin termLTS p And termLTS q Using termLTS. Abort.
  MeBi Config Solver Answers Default.

  (* With [Bounds Game <n>] they are allowed, the walk run within <n> pairs;
     past it they are refused, and Auto takes the nested cofix. *)
  MeBi Config Bounds Game 1000.
  MeBi Config Solver MutualCofix True.
  Example wsim_mutual_bounded : weak_sim termLTS termLTS p q.
  Proof. MeBi Sim Begin termLTS p And termLTS q Using termLTS. MeBi Sim Solve 1000. Qed.
  MeBi Config Solver MutualCofix Auto.
  MeBi Config Solver Answers Greedy.
  Example wsim_greedy_bounded : weak_sim termLTS termLTS p q.
  Proof. MeBi Sim Begin termLTS p And termLTS q Using termLTS. MeBi Sim Solve 1000. Qed.
  MeBi Config Solver Answers Default.
  MeBi Config Bounds Game 1.
  MeBi Config Solver MutualCofix True.
  Example wsim_mutual_over : weak_sim termLTS termLTS p q.
  Proof. Fail MeBi Sim Begin termLTS p And termLTS q Using termLTS. Abort.
  MeBi Config Solver MutualCofix Auto.
  MeBi Config Solver Answers Greedy.
  Example wsim_greedy_over : weak_sim termLTS termLTS p q.
  Proof. Fail MeBi Sim Begin termLTS p And termLTS q Using termLTS. Abort.
  MeBi Config Solver Answers Default.
  Example wsim_auto_over : weak_sim termLTS termLTS p q.
  Proof. MeBi Sim Begin termLTS p And termLTS q Using termLTS. MeBi Sim Solve 1000. Qed.
  MeBi Config Reset Bounds.
  MeBi Config Saturation OnDemand False.

  (* [Saturation OnDemand False]: refused above the bound, as before. *)
  MeBi Config Bounds Saturation 1.
  Fail MeBi Run Bisim p With termLTS And q With termLTS Using termLTS.
  Example wsim_refused : weak_sim termLTS termLTS p q.
  Proof. Fail MeBi Sim Begin termLTS p And termLTS q Using termLTS. Abort.
  MeBi Config Saturation OnDemand Auto.

  (* [FailIf Oversaturated False]: warn and carry on. *)
  MeBi Config FailIf Oversaturated False.
  MeBi Run Saturate p Using termLTS.
  MeBi Config FailIf Oversaturated True.
  Fail MeBi Run Saturate p Using termLTS.

  (* [Reset Bounds] restores the default bound (1,000,000). *)
  MeBi Config Reset Bounds.
  MeBi Run Saturate p Using termLTS.
  MeBi Config Reset Weak.
End SaturationGuard.

MeBi Divider "Theories.Test.DecidedPremise".
Module DecidedPremise.
  (* An equation premise over closed terms is decided during extraction
     (backlog item I2): [go] fires from 0 ([0 = 0] holds) and not from 1
     ([1 = 0] does not), so the LTS from 0 is the single transition
     [0 -true-> 1]: 2 states, 1 transition. (Until 2026-10-01 the premise
     was skipped and this was [0 -> 1 -> 2 -> ...], unbounded.) *)
  Inductive st : nat -> bool -> nat -> Prop :=
  | go (n : nat) : n = 0 -> st n true (S n).
  MeBi Config Bounds As Num States 2.
  MeBi Run LTS 0 Using st.
  MeBi Config Bounds As Num States 1.
  Fail MeBi Run LTS 0 Using st.
  MeBi Config Bounds As Num Transitions 1.
  MeBi Run LTS 0 Using st.
  MeBi Config Reset Bounds.
End DecidedPremise.

MeBi Divider "Theories.Test.UndecidedPremise".
Module UndecidedPremise.
  (* KNOWN WRONG (backlog item I2): an equation MeBi cannot decide -- here
     over an opaque [f], which does not reduce -- is skipped (with a
     warning), so [go] is applied from every state and the LTS hits the
     bound. If such premises become decidable (e.g. by proof search), this
     [Fail] starts failing: replace it with a positive test. *)
  Parameter f : nat -> nat.
  Inductive st : nat -> bool -> nat -> Prop :=
  | go (n : nat) : f n = 0 -> st n true (S n).
  MeBi Config Bounds As Num States 20.
  Fail MeBi Run LTS 0 Using st.
  MeBi Config Reset Bounds.
End UndecidedPremise.

(* Pinned sizes (backlog item E(b)). No command reports an LTS's size, but
   the bounds can be used as assertions: "succeeds at n, fails at n - 1" pins
   the least bound at which extraction completes, a regression pin.
   [Bounds Saturation] pins the weak-action count exactly. The state and
   transition bounds are checked {e before} each state is explored, so the
   final LTS can exceed the least bound by the out-degree of the last state
   explored (corrected 2026-10-02: [OutputPremises.via_c] completes at 2
   transitions with 3). Below, the hand-counted [p1] figures coincide with
   the true counts because its last-explored states are terminal.

   [procLTS] recurses on itself ([p_parl]/[p_parr]); [sysLTS] has a premise
   over a {e different} LTS, the layered shape of [Proc] and [CADP] -- the
   path that drops each state's matching evars, and the one I2's premise
   check sits on. The [p1] figures are counted by hand:
   [ppar (A.B.0) (B.0)] has 8 states and 10 transitions (2 silent [p_tidy]),
   and saturates to 11 weak actions; [run p1] adds [done] and [s_stop]. *)
MeBi Divider "Theories.Test.ExtractionSizes".
Module ExtractionSizes.
  Inductive act : Set := A | B.
  Inductive proc : Set := pnil | pact (a : act) (p : proc) | ppar (p q : proc).

  Inductive procLTS : proc -> option act -> proc -> Prop :=
  | p_act a p : procLTS (pact a p) (Some a) p
  | p_parl p p' q a : procLTS p a p' -> procLTS (ppar p q) a (ppar p' q)
  | p_parr p q q' a : procLTS q a q' -> procLTS (ppar p q) a (ppar p q')
  | p_tidy q : procLTS (ppar pnil q) None q.

  Inductive sys : Set := run (p : proc) | done.

  Inductive sysLTS : sys -> option act -> sys -> Prop :=
  | s_step p a p' : procLTS p a p' -> sysLTS (run p) a (run p')
  | s_stop : sysLTS (run pnil) None done.

  Definition p1 : proc := ppar (pact A (pact B pnil)) (pact B pnil).
  Definition p2 : proc :=
    ppar (ppar (pact A pnil) (pact B pnil)) (pact A (pact A pnil)).

  MeBi Config Reset Weak.

  (* p1: 8 states, 10 transitions. *)
  MeBi Config Bounds As Num States 8.
  MeBi Run LTS p1 Using procLTS.
  MeBi Config Bounds As Num States 7.
  Fail MeBi Run LTS p1 Using procLTS.
  MeBi Config Bounds As Num Transitions 10.
  MeBi Run LTS p1 Using procLTS.
  MeBi Config Bounds As Num Transitions 9.
  Fail MeBi Run LTS p1 Using procLTS.

  (* p2: 21 states, 38 transitions. *)
  MeBi Config Bounds As Num States 21.
  MeBi Run LTS p2 Using procLTS.
  MeBi Config Bounds As Num States 20.
  Fail MeBi Run LTS p2 Using procLTS.
  MeBi Config Bounds As Num Transitions 38.
  MeBi Run LTS p2 Using procLTS.
  MeBi Config Bounds As Num Transitions 37.
  Fail MeBi Run LTS p2 Using procLTS.

  (* Layered: run p1 is p1's LTS plus [done]: 9 states, 11 transitions. *)
  MeBi Config Bounds As Num States 9.
  MeBi Run LTS (run p1) Using sysLTS procLTS.
  MeBi Config Bounds As Num States 8.
  Fail MeBi Run LTS (run p1) Using sysLTS procLTS.
  MeBi Config Bounds As Num Transitions 11.
  MeBi Run LTS (run p1) Using sysLTS procLTS.
  MeBi Config Bounds As Num Transitions 10.
  Fail MeBi Run LTS (run p1) Using sysLTS procLTS.

  (* Layered: run p2, 22 states, 39 transitions. *)
  MeBi Config Bounds As Num States 22.
  MeBi Run LTS (run p2) Using sysLTS procLTS.
  MeBi Config Bounds As Num States 21.
  Fail MeBi Run LTS (run p2) Using sysLTS procLTS.
  MeBi Config Bounds As Num Transitions 39.
  MeBi Run LTS (run p2) Using sysLTS procLTS.
  MeBi Config Bounds As Num Transitions 38.
  Fail MeBi Run LTS (run p2) Using sysLTS procLTS.
  MeBi Config Reset Bounds.

  (* Saturation sizes, with [None] silent: p1 11 weak actions, run p2 61. *)
  MeBi Config Weak As Option act.
  MeBi Config Bounds Saturation 11.
  MeBi Run Saturate p1 Using procLTS.
  MeBi Config Bounds Saturation 10.
  Fail MeBi Run Saturate p1 Using procLTS.
  MeBi Config Bounds Saturation 61.
  MeBi Run Saturate (run p2) Using sysLTS procLTS.
  MeBi Config Bounds Saturation 60.
  Fail MeBi Run Saturate (run p2) Using sysLTS procLTS.
  MeBi Config Reset Bounds.
  MeBi Config Reset Weak.
End ExtractionSizes.

MeBi Divider "Theories.Test.TwoPremises".
Module TwoPremises.
  (* Constructors with more than one LTS premise (backlog item A6, fixed
     2026-10-01). No example in the repository has one. A derivation's
     children are its premises, all required; the solver used to keep only
     the shortest child ([Tree.minimize]), so it proved the first premise
     and was left with the second. It now replays the whole tree in
     pre-order. The shapes below are the ones that told the two apart in
     the spike (notes/8): premises over different LTSs at different
     constructor indices fail if replayed in the wrong order, so they also
     pin the order. *)
  Import ExtractionSizes.
  Inductive leftLTS : proc -> option act -> proc -> Prop :=
  | l_act a p : leftLTS (pact a p) (Some a) p.
  Inductive rightLTS : proc -> option act -> proc -> Prop :=
  | r_unused : rightLTS pnil None pnil
  | r_act a p : rightLTS (pact a p) (Some a) p.
  Inductive thirdLTS : proc -> option act -> proc -> Prop :=
  | t_unused1 : thirdLTS pnil None pnil
  | t_unused2 : thirdLTS (ppar pnil pnil) None pnil
  | t_act a p : thirdLTS (pact a p) (Some a) p.

  MeBi Config Reset Weak.
  MeBi Config Weak As Option act.

  (* Both premises over one LTS: order cannot matter. *)
  Inductive sameLTS : proc -> option act -> proc -> Prop :=
  | same p p' q q' a : leftLTS p (Some a) p' -> leftLTS q (Some a) q' ->
                       sameLTS (ppar p q) (Some a) (ppar p' q').
  Inductive sameLTS' : proc -> option act -> proc -> Prop :=
  | same' p p' q q' a : leftLTS p (Some a) p' -> leftLTS q (Some a) q' ->
                        sameLTS' (ppar p q) (Some a) (ppar p' q').
  Definition x2 : proc := ppar (pact A (pact B pnil)) (pact A (pact B pnil)).
  Example wsim_same : weak_sim sameLTS sameLTS' x2 x2.
  Proof. MeBi Sim Begin sameLTS x2 And sameLTS' x2 Using leftLTS.
    MeBi Sim Solve 100. Qed.

  (* Two different LTSs. *)
  Inductive syncLTS : proc -> option act -> proc -> Prop :=
  | sync p p' q q' a : leftLTS p (Some a) p' -> rightLTS q (Some a) q' ->
                       syncLTS (ppar p q) (Some a) (ppar p' q').
  Inductive syncLTS' : proc -> option act -> proc -> Prop :=
  | sync' p p' q q' a : leftLTS p (Some a) p' -> rightLTS q (Some a) q' ->
                        syncLTS' (ppar p q) (Some a) (ppar p' q').
  Example wsim_sync : weak_sim syncLTS syncLTS' x2 x2.
  Proof. MeBi Sim Begin syncLTS x2 And syncLTS' x2 Using leftLTS rightLTS.
    MeBi Sim Solve 100. Qed.

  (* Three LTSs, constructor indices 0/1/2: catches a rotated order. *)
  Inductive tripleLTS : proc -> option act -> proc -> Prop :=
  | triple p p' q q' r r' a :
      leftLTS p (Some a) p' -> rightLTS q (Some a) q' -> thirdLTS r (Some a) r' ->
      tripleLTS (ppar p (ppar q r)) (Some a) (ppar p' (ppar q' r')).
  Inductive tripleLTS' : proc -> option act -> proc -> Prop :=
  | triple' p p' q q' r r' a :
      leftLTS p (Some a) p' -> rightLTS q (Some a) q' -> thirdLTS r (Some a) r' ->
      tripleLTS' (ppar p (ppar q r)) (Some a) (ppar p' (ppar q' r')).
  Definition x3 : proc := ppar (pact A pnil) (ppar (pact A pnil) (pact A pnil)).
  Example wsim_triple : weak_sim tripleLTS tripleLTS' x3 x3.
  Proof. MeBi Sim Begin tripleLTS x3 And tripleLTS' x3
           Using leftLTS rightLTS thirdLTS.
    MeBi Sim Solve 100. Qed.

  (* Nested: a two-premise LTS as a premise of a two-premise LTS. *)
  Inductive outerLTS : proc -> option act -> proc -> Prop :=
  | outer p p' q q' a : syncLTS p (Some a) p' -> thirdLTS q (Some a) q' ->
                        outerLTS (ppar p q) (Some a) (ppar p' q').
  Inductive outerLTS' : proc -> option act -> proc -> Prop :=
  | outer' p p' q q' a : syncLTS p (Some a) p' -> thirdLTS q (Some a) q' ->
                         outerLTS' (ppar p q) (Some a) (ppar p' q').
  Definition x4 : proc := ppar (ppar (pact A pnil) (pact A pnil)) (pact A pnil).
  Example wsim_nested : weak_sim outerLTS outerLTS' x4 x4.
  Proof. MeBi Sim Begin outerLTS x4 And outerLTS' x4
           Using syncLTS leftLTS rightLTS thirdLTS.
    MeBi Sim Solve 100. Qed.

  (* Equation premises, after or before the LTS one (backlog item I2,
     2026-10-01): extraction decides them, the solver closes them by
     [reflexivity] and no longer tries to invert them. Until then the first
     stalled (an [a = a] hypothesis was inverted forever) and the second was
     an Anomaly. *)
  Inductive eqLTS : proc -> option act -> proc -> Prop :=
  | with_eq p p' q a : leftLTS p (Some a) p' -> a = a ->
                       eqLTS (ppar p q) (Some a) (ppar p' q).
  Inductive eqLTS' : proc -> option act -> proc -> Prop :=
  | with_eq' p p' q a : leftLTS p (Some a) p' -> a = a ->
                        eqLTS' (ppar p q) (Some a) (ppar p' q).
  Definition x5 : proc := ppar (pact A (pact B pnil)) pnil.
  Example wsim_eq : weak_sim eqLTS eqLTS' x5 x5.
  Proof. MeBi Sim Begin eqLTS x5 And eqLTS' x5 Using leftLTS.
    MeBi Sim Solve 100. Qed.
  Inductive eqfirstLTS : proc -> option act -> proc -> Prop :=
  | eq_first p p' q a : a = a -> leftLTS p (Some a) p' ->
                        eqfirstLTS (ppar p q) (Some a) (ppar p' q).
  Inductive eqfirstLTS' : proc -> option act -> proc -> Prop :=
  | eq_first' p p' q a : a = a -> leftLTS p (Some a) p' ->
                         eqfirstLTS' (ppar p q) (Some a) (ppar p' q).
  Example wsim_eqfirst : weak_sim eqfirstLTS eqfirstLTS' x5 x5.
  Proof. MeBi Sim Begin eqfirstLTS x5 And eqfirstLTS' x5 Using leftLTS.
    MeBi Sim Solve 100. Qed.

  (* A false equation premise blocks a step inside a proof: [guard] lets
     [A] through and not [B], so from [x5] only the [A]-step exists. *)
  Inductive guardLTS : proc -> option act -> proc -> Prop :=
  | guard p p' a : a = A -> leftLTS p (Some a) p' -> guardLTS p (Some a) p'.
  Inductive guardLTS' : proc -> option act -> proc -> Prop :=
  | guard' p p' a : a = A -> leftLTS p (Some a) p' -> guardLTS' p (Some a) p'.
  Definition x6 : proc := pact A (pact B pnil).
  MeBi Config Bounds As Num States 2.
  MeBi Run LTS x6 Using guardLTS leftLTS.
  MeBi Config Reset Bounds.
  Example wsim_guard : weak_sim guardLTS guardLTS' x6 x6.
  Proof. MeBi Sim Begin guardLTS x6 And guardLTS' x6 Using leftLTS.
    MeBi Sim Solve 100. Qed.
  MeBi Config Reset Weak.
End TwoPremises.

MeBi Divider "Theories.Test.GeneralPremises".
Module GeneralPremises.
  (* Premises that are neither over an LTS nor equations are decided by a
     bounded proof search over their inductive's constructors (backlog item
     I2, stage 1; notes/9). Each counter steps from 0 while its guard holds,
     so the LTS size pins the guard's verdict for every n; the proofs check
     the solver closes true premises (by the proof found) and refutes false
     ones in hypotheses. Counts hand-checked: [n <= 2] holds for 0..2, so
     states 0..3. *)
  From Stdlib Require Import List PeanoNat. Import ListNotations.
  MeBi Config Reset Weak.

  Inductive le_c : nat -> option bool -> nat -> Prop :=
  | le_go n : n <= 2 -> le_c n (Some true) (S n).
  Inductive le_c' : nat -> option bool -> nat -> Prop :=
  | le_go' n : n <= 2 -> le_c' n (Some true) (S n).
  (* [lt] is a definition over [le]: unfolded before the search *)
  Inductive lt_c : nat -> option bool -> nat -> Prop :=
  | lt_go n : n < 2 -> lt_c n (Some true) (S n).
  (* [In] is a fixpoint: reduces to [or]/[eq]/[False] once the list is known *)
  Inductive in_c : nat -> option bool -> nat -> Prop :=
  | in_go n : In n [0; 1] -> in_c n (Some true) (S n).
  Inductive in_c' : nat -> option bool -> nat -> Prop :=
  | in_go' n : In n [0; 1] -> in_c' n (Some true) (S n).
  (* [Forall]'s predicate is a parameter: refutable although it is a lambda *)
  Inductive fa_c : nat -> option bool -> nat -> Prop :=
  | fa_go n : Forall (fun k => k <= 1) [n; n] -> fa_c n (Some true) (S n).
  Inductive fa_c' : nat -> option bool -> nat -> Prop :=
  | fa_go' n : Forall (fun k => k <= 1) [n; n] -> fa_c' n (Some true) (S n).
  Inductive and_c : nat -> option bool -> nat -> Prop :=
  | and_go n : n <= 3 /\ 1 <= S n -> and_c n (Some true) (S n).
  Inductive or_c : nat -> option bool -> nat -> Prop :=
  | or_go n : n = 0 \/ n = 1 -> or_c n (Some true) (S n).
  Inductive or_c' : nat -> option bool -> nat -> Prop :=
  | or_go' n : n = 0 \/ n = 1 -> or_c' n (Some true) (S n).

  MeBi Config Bounds As Num States 4.
  MeBi Run LTS 0 Using le_c.
  MeBi Config Bounds As Num States 3.
  Fail MeBi Run LTS 0 Using le_c.
  MeBi Run LTS 0 Using lt_c.
  MeBi Run LTS 0 Using in_c.
  MeBi Run LTS 0 Using fa_c.
  MeBi Run LTS 0 Using or_c.
  MeBi Config Bounds As Num States 2.
  Fail MeBi Run LTS 0 Using lt_c.
  Fail MeBi Run LTS 0 Using in_c.
  Fail MeBi Run LTS 0 Using fa_c.
  Fail MeBi Run LTS 0 Using or_c.
  MeBi Config Bounds As Num States 5.
  MeBi Run LTS 0 Using and_c.
  MeBi Config Bounds As Num States 4.
  Fail MeBi Run LTS 0 Using and_c.

  (* The depth bound: [le 0 2] needs 3 constructor applications. *)
  MeBi Config Premise Depth 2.
  MeBi Config Bounds As Num States 20.
  Fail MeBi Run LTS 0 Using le_c.
  MeBi Config Reset Bounds.

  MeBi Config Weak As Option bool.
  Example w_le : weak_sim le_c le_c' 0 0.
  Proof. MeBi Sim Begin le_c 0 And le_c' 0 Using le_c. MeBi Sim Solve 100. Qed.
  Example w_in : weak_sim in_c in_c' 0 0.
  Proof. MeBi Sim Begin in_c 0 And in_c' 0 Using in_c. MeBi Sim Solve 100. Qed.
  Example w_fa : weak_sim fa_c fa_c' 0 0.
  Proof. MeBi Sim Begin fa_c 0 And fa_c' 0 Using fa_c. MeBi Sim Solve 100. Qed.
  Example w_or : weak_sim or_c or_c' 0 0.
  Proof. MeBi Sim Begin or_c 0 And or_c' 0 Using or_c. MeBi Sim Solve 100. Qed.
  MeBi Config Reset Weak.

  (* A negation [~ P] holds iff [P] is refuted by a complete search; the
     solver proves it by [intro] and refuting [P]. [~ (3 <= n)]: 0..2,
     so states 0..3. *)
  Inductive not_c : nat -> option bool -> nat -> Prop :=
  | not_go n : ~ (3 <= n) -> not_c n (Some true) (S n).
  Inductive not_c' : nat -> option bool -> nat -> Prop :=
  | not_go' n : ~ (3 <= n) -> not_c' n (Some true) (S n).
  MeBi Config Bounds As Num States 4.
  MeBi Run LTS 0 Using not_c.
  MeBi Config Bounds As Num States 3.
  Fail MeBi Run LTS 0 Using not_c.
  MeBi Config Reset Bounds.
  MeBi Config Weak As Option bool.
  Example w_not : weak_sim not_c not_c' 0 0.
  Proof. MeBi Sim Begin not_c 0 And not_c' 0 Using not_c. MeBi Sim Solve 100. Qed.
  MeBi Config Reset Weak.

  (* KNOWN WRONG: an opaque function cannot be decided, so the LTS
     over-approximates (with a warning). When supported, this Fail starts
     failing: make it positive. *)
  Parameter f : nat -> nat.
  Inductive op_c : nat -> option bool -> nat -> Prop :=
  | op_go n : f n <= 2 -> op_c n (Some true) (S n).
  MeBi Config Bounds As Num States 20.
  Fail MeBi Run LTS 0 Using op_c.
  MeBi Config Reset Bounds.

  (* ... unless the user supplies a tactic for what the search cannot
     decide ([MeBi Config Premise Tactic]): it proves the premise or its
     negation, at extraction and again in the proof. *)
  From Stdlib Require Import Lia.
  Axiom f_def : forall n, f n = n.
  Inductive op_c' : nat -> option bool -> nat -> Prop :=
  | op_go' n : f n <= 2 -> op_c' n (Some true) (S n).
  MeBi Config Premise Tactic (rewrite f_def; lia).
  MeBi Config Bounds As Num States 4.
  MeBi Run LTS 0 Using op_c.
  MeBi Config Bounds As Num States 3.
  Fail MeBi Run LTS 0 Using op_c.
  MeBi Config Reset Bounds.
  MeBi Config Weak As Option bool.
  Example w_op : weak_sim op_c op_c' 0 0.
  Proof. MeBi Sim Begin op_c 0 And op_c' 0 Using op_c. MeBi Sim Solve 100. Qed.
  MeBi Config Reset Weak.
  MeBi Config Reset Premise.
End GeneralPremises.

MeBi Divider "Theories.Test.OutputPremises".
Module OutputPremises.
  (* Premises that compute what the transition needs (backlog item I2,
     stage 2; notes/9). An open premise is enumerated once the LTS premises
     are unified -- every solution its own transition -- and one that fixes an
     LTS premise's source is resolved before that premise is explored. In
     proofs the step's target is bound into the constructor, and premise
     goals are moved behind the LTS ones. Until 2026-10-02 each of these
     LTSs had one state and no transitions (or, for [via], one of three). *)
  From Stdlib Require Import List PeanoNat. Import ListNotations.
  MeBi Config Reset Weak.
  Inductive succ_rel : nat -> nat -> Prop := sr n : succ_rel n (S n).
  Inductive two : nat -> nat -> Prop :=
  | t1 n : two n (S n) | t2 n : two n (S (S n)).
  Inductive base : nat -> option bool -> nat -> Prop :=
  | b0 : base 0 (Some true) 1 | b1 : base 1 (Some true) 2.

  (* target by an equation: 0..3, 3 transitions *)
  Inductive eq_c : nat -> option bool -> nat -> Prop :=
  | eq_go n m : n <= 2 -> m = S n -> eq_c n (Some true) m.
  (* target by a relation: 0..3 *)
  Inductive rel_c : nat -> option bool -> nat -> Prop :=
  | rel_go n m : n <= 2 -> succ_rel n m -> rel_c n (Some true) m.
  Inductive rel_c' : nat -> option bool -> nat -> Prop :=
  | rel_go' n m : n <= 2 -> succ_rel n m -> rel_c' n (Some true) m.
  (* two solutions, two transitions: 0..3, edges 0-1 0-2 1-2 1-3 *)
  Inductive two_c : nat -> option bool -> nat -> Prop :=
  | two_go n m : n <= 1 -> two n m -> two_c n (Some true) m.
  Inductive two_c' : nat -> option bool -> nat -> Prop :=
  | two_go' n m : n <= 1 -> two n m -> two_c' n (Some true) m.
  (* target enumerated from a list: 0, 5, 7 *)
  Inductive in_c : nat -> option bool -> nat -> Prop :=
  | in_go q n : n <= 0 -> In q [n + 5; n + 7] -> in_c n (Some true) q.
  (* [In] fixes the source of the [base] premise: 0, 1, 2 and 3 transitions
     (0-1, 0-2, 1-2) *)
  Inductive via_c : nat -> option bool -> nat -> Prop :=
  | via q n a q' : In q [n; S n] -> base q a q' -> via_c n a q'.
  Inductive via_c' : nat -> option bool -> nat -> Prop :=
  | via' q n a q' : In q [n; S n] -> base q a q' -> via_c' n a q'.

  MeBi Config Bounds As Num States 4.
  MeBi Run LTS 0 Using eq_c.
  MeBi Run LTS 0 Using rel_c.
  MeBi Run LTS 0 Using two_c.
  MeBi Config Bounds As Num States 3.
  Fail MeBi Run LTS 0 Using eq_c.
  Fail MeBi Run LTS 0 Using rel_c.
  Fail MeBi Run LTS 0 Using two_c.
  MeBi Run LTS 0 Using in_c.
  MeBi Run LTS 0 Using via_c base.
  MeBi Config Bounds As Num Transitions 3.
  MeBi Run LTS 0 Using eq_c.
  MeBi Config Bounds As Num Transitions 2.
  Fail MeBi Run LTS 0 Using eq_c.
  (* least transition bound 2 for 3 transitions: see the note on bounds in
     [ExtractionSizes] *)
  MeBi Run LTS 0 Using via_c base.
  MeBi Config Bounds As Num Transitions 1.
  Fail MeBi Run LTS 0 Using via_c base.
  MeBi Config Bounds As Num Transitions 4.
  MeBi Run LTS 0 Using two_c.
  MeBi Config Bounds As Num Transitions 3.
  Fail MeBi Run LTS 0 Using two_c.
  MeBi Config Bounds As Num States 2.
  Fail MeBi Run LTS 0 Using in_c.
  Fail MeBi Run LTS 0 Using via_c base.
  MeBi Config Reset Bounds.

  MeBi Config Weak As Option bool.
  Example w_rel : weak_sim rel_c rel_c' 0 0.
  Proof. MeBi Sim Begin rel_c 0 And rel_c' 0 Using rel_c. MeBi Sim Solve 100. Qed.
  Example w_two : weak_sim two_c two_c' 0 0.
  Proof. MeBi Sim Begin two_c 0 And two_c' 0 Using two_c. MeBi Sim Solve 100. Qed.
  Example w_via : weak_sim via_c via_c' 0 0.
  Proof. MeBi Sim Begin via_c 0 And via_c' 0 Using base. MeBi Sim Solve 100. Qed.
  MeBi Config Reset Weak.

  (* An LTS premise whose source nothing determines: [q] below. Its sources
     are enumerated by the premise search and each is explored as usual.
     Until 2026-10-03 it was explored from the open source, where the first
     constructor that matched fixed [q] for all the others, so only [b0] was
     found (2 states; [b0] and [b1] swapped gave a different 2), and a
     recursive constructor was cut off the same way (note 9, option B). *)
  Inductive open_c : nat -> option bool -> nat -> Prop :=
  | open_go q n a q' : base q a q' -> n <= 5 -> open_c n a q'.
  Inductive open_c' : nat -> option bool -> nat -> Prop :=
  | open_go' q n a q' : base q a q' -> n <= 5 -> open_c' n a q'.
  (* [base] with its constructors swapped: the same LTS *)
  Inductive base_sw : nat -> option bool -> nat -> Prop :=
  | bs1 : base_sw 1 (Some true) 2 | bs0 : base_sw 0 (Some true) 1.
  Inductive open_sw : nat -> option bool -> nat -> Prop :=
  | open_sw_go q n a q' : base_sw q a q' -> n <= 5 -> open_sw n a q'.
  (* recursive, guarded: sources 0..3, so 0 goes to 1..4 *)
  Inductive rb : nat -> option bool -> nat -> Prop :=
  | rb0 : rb 0 (Some true) 1
  | rbs q a q' : q <= 2 -> rb q a q' -> rb (S q) a (S q').
  Inductive open_rec : nat -> option bool -> nat -> Prop :=
  | open_rec_go q a q' : rb q a q' -> open_rec 0 a q'.
  Inductive open_rec' : nat -> option bool -> nat -> Prop :=
  | open_rec_go' q a q' : rb q a q' -> open_rec' 0 a q'.

  MeBi Config Bounds As Num States 3.
  MeBi Run LTS 0 Using open_c base.
  MeBi Run LTS 0 Using open_sw base_sw.
  MeBi Config Bounds As Num States 2.
  Fail MeBi Run LTS 0 Using open_c base.
  Fail MeBi Run LTS 0 Using open_sw base_sw.
  MeBi Config Bounds As Num States 5.
  MeBi Run LTS 0 Using open_rec rb.
  MeBi Config Bounds As Num States 4.
  Fail MeBi Run LTS 0 Using open_rec rb.
  MeBi Config Reset Bounds.

  MeBi Config Weak As Option bool.
  Example w_open : weak_sim open_c open_c' 0 0.
  Proof. MeBi Sim Begin open_c 0 And open_c' 0 Using base. MeBi Sim Solve 100. Qed.
  (* Until 2026-10-03 the solver stopped here on an internal [Not_found]:
     reading the hypotheses for the transition to answer, it took [rb 2 a
     3], a premise's step left by inverting [open_rec 0 a 3], for a step of
     [open_rec] (its states are numbers too), and state 2 has no edges. It
     now reads only steps of the relation being simulated. *)
  Example w_open_rec : weak_sim open_rec open_rec' 0 0.
  Proof. MeBi Sim Begin open_rec 0 And open_rec' 0 Using rb. MeBi Sim Solve 100. Qed.

  (* [base] as a plain premise (not in [Using]): applying [open_go] leaves
     [q] an evar that no LTS premise's replay fixes, in [base ?q a 1]. The
     witness is now chosen by enumerating the premise goals that mention it,
     together: in [two_p], [q = 0] satisfies [In q [0; 1]] but not [base q a
     2]. Until 2026-10-03 both stopped on "cannot prove the constructor
     premise". *)
  Inductive two_p : nat -> option bool -> nat -> Prop :=
  | tp q n a q' : In q [0; 1] -> base q a q' -> n <= 0 -> two_p n a q'.
  Inductive two_p' : nat -> option bool -> nat -> Prop :=
  | tp' q n a q' : In q [0; 1] -> base q a q' -> n <= 0 -> two_p' n a q'.
  Example w_open_plain : weak_sim open_c open_c' 0 0.
  Proof. MeBi Sim Begin open_c 0 And open_c' 0. MeBi Sim Solve 100. Qed.
  Example w_two_plain : weak_sim two_p two_p' 0 0.
  Proof. MeBi Sim Begin two_p 0 And two_p' 0. MeBi Sim Solve 100. Qed.
  MeBi Config Reset Weak.

  (* KNOWN LIMIT, warned: transitions MeBi cannot determine. [u n] is a
     step from [S n] for every [n], with nothing to bound [n]: [open_u 0]
     has infinitely many successors. Such a transition is dropped with a
     warning ("cannot determine"), so these LTSs have one state. Until
     2026-10-03 the first two were dropped silently, and the third kept
     [S (S ?n)] as if it were a state. Such an LTS is incomplete: an error
     ([LTS_Incomplete], "only an approximation") unless [FailIf Incomplete
     False]. *)
  Inductive ung : nat -> option bool -> nat -> Prop := u n : ung (S n) None n.
  Inductive open_u : nat -> option bool -> nat -> Prop :=
  | open_u_go q a q' : ung q a q' -> open_u 0 a q'.
  Inductive ung2 : nat -> option bool -> nat -> Prop :=
  | u2 n : ung2 (S n) None (S (S n)).
  Inductive open_u2 : nat -> option bool -> nat -> Prop :=
  | open_u2_go q a q' : ung2 q a q' -> open_u2 0 a q'.
  MeBi Config Bounds As Num States 1.
  Fail MeBi Run LTS 0 Using open_u ung.      (* [ung] explored as an LTS *)
  Fail MeBi Run LTS 0 Using open_u.          (* [ung] as a plain premise *)
  Fail MeBi Run LTS 0 Using open_u2 ung2.
  MeBi Config FailIf Incomplete False.
  MeBi Run LTS 0 Using open_u ung.
  MeBi Run LTS 0 Using open_u.
  MeBi Run LTS 0 Using open_u2 ung2.
  MeBi Config Reset FailIf.
  MeBi Config Reset Bounds.

  (* KNOWN LIMIT, warned: recursive and unguarded, so the sources are
     infinitely many; the search stops at [MeBi Config Premise Depth] and
     says its solutions may be incomplete. Exploring from the open source,
     as before 2026-10-03, would recurse without bound once the first
     match no longer cut it off. Incomplete, as above. *)
  Inductive ur : nat -> option bool -> nat -> Prop :=
  | ur0 : ur 0 (Some true) 1
  | urs q a q' : ur q a q' -> ur (S q) a (S q').
  Inductive open_ur : nat -> option bool -> nat -> Prop :=
  | open_ur_go q a q' : ur q a q' -> open_ur 0 a q'.
  MeBi Config Bounds As Num States 100.
  Fail MeBi Run LTS 0 Using open_ur ur.
  MeBi Config FailIf Incomplete False.
  MeBi Run LTS 0 Using open_ur ur.
  MeBi Config Reset FailIf.
  MeBi Config Reset Bounds.
End OutputPremises.

MeBi Divider "Theories.Test.SilentResponse".
Module SilentResponse.
  (* Found 2026-10-02 while porting rocq-sims' [examples/SimExample.v], and
     fixed the same day: [q] is [p] renamed, both [tau.a + b], so [p <= q]
     holds. Answering [p -tau-> p1] needs [q] to move silently to [q1], since
     staying at [q] is not bisimilar to [p1]. Saturation records only weak
     moves with a visible action, so [Product.respond] used to find no silent
     reply and the solver stopped on [CouldNotGetGoalTransition]; it now
     answers from the unsaturated FSM's silent steps. *)
  Inductive st : Set := p | p1 | q | q1 | z.
  Inductive lab : Set := a | b.
  Inductive step : st -> option lab -> st -> Prop :=
  | p_tau : step p None p1 | p_b : step p (Some b) z | p1_a : step p1 (Some a) z
  | q_tau : step q None q1 | q_b : step q (Some b) z | q1_a : step q1 (Some a) z.
  MeBi Config Weak As Option lab.
  Example sim_p_q : weak_sim step step p q.
  Proof. MeBi Sim Begin step p And step q Using step. MeBi Sim Solve 100. Qed.

  (* The LTS of rocq-sims' [examples/SimExample.v] (Nicolas Chappe,
     https://github.com/rocq-sims/rocq-sims, LGPL-3.0-or-later), re-encoded
     with [None] for tau. There it is proved by hand that [u0] simulates [t0]
     in a {e divergence-sensitive} sense; [weak_sim] ignores divergence, so
     this is a weaker statement, proved here in both directions. *)
  Inductive sst : Set := t0 | t1 | t2 | t3 | t4 | u0 | u1 | u2 | u3.
  Inductive sobs : Set := sa | sb.
  Inductive strans : sst -> option sobs -> sst -> Prop :=
  | t0t0 : strans t0 None t0 | t0t1 : strans t0 None t1
  | t0t3 : strans t0 (Some sb) t3 | t1t2 : strans t1 (Some sa) t2
  | t3t4 : strans t3 None t4 | u0u1 : strans u0 None u1
  | u0u3 : strans u0 (Some sb) u3 | u1u1 : strans u1 None u1
  | u1u2 : strans u1 (Some sa) u2.
  MeBi Config Weak As Option sobs.
  Example sims_t0_u0 : weak_sim strans strans t0 u0.
  Proof. MeBi Sim Begin strans t0 And strans u0 Using strans. MeBi Sim Solve 100. Qed.
  Example sims_u0_t0 : weak_sim strans strans u0 t0.
  Proof. MeBi Sim Begin strans u0 And strans t0 Using strans. MeBi Sim Solve 100. Qed.

  (* Divergence is invisible to [weak_sim]: a tau-loop and a stuck state
     simulate each other, and are weakly bisimilar. A divergence-sensitive
     relation (rocq-sims' mudiv-simulation, for one) separates them. *)
  Inductive dst : Set := loop | stop.
  Inductive dstep : dst -> option sobs -> dst -> Prop :=
  | dloop : dstep loop None loop.
  MeBi Run Bisim loop With dstep And stop With dstep.
  Example div_loop_stop : weak_sim dstep dstep loop stop.
  Proof. MeBi Sim Begin dstep loop And dstep stop Using dstep. MeBi Sim Solve 100. Qed.
  MeBi Config Reset Weak.
End SilentResponse.

MeBi Divider "Theories.Test.CheckerVerdicts".
Module CheckerVerdicts.
  (* Neither pair below is bisimilar, and until 2026-10-02 [MeBi Run Bisim]
     answered "bisimilar" for both. With [FailIf] on a negative result (the
     default), a correct verdict makes the command fail. *)

  (* 1. Rooted. [x0 = a.b.x0] and [y0 = b.a.y0] differ in their first step,
     so they are not even strongly bisimilar. The verdict used to ask only
     whether every block of the partition holds states of both systems
     ({x0, y1} and {x1, y0} do), never whether [x0] and [y0] share one. *)
  Inductive rst : Set := x0 | x1 | y0 | y1.
  Inductive rlab : Set := ra | rb.
  Inductive rstep : rst -> rlab -> rst -> Prop :=
  | xa : rstep x0 ra x1 | xb : rstep x1 rb x0
  | yb : rstep y0 rb y1 | ya : rstep y1 ra y0.
  Fail MeBi Run Bisim x0 With rstep And y0 With rstep.

  (* 2. Silent closure. [p = tau.p1 + b.z + c.p1], [p1 = a.z] against
     [r = a.z + b.z + c.r1], [r1 = a.z]. After [p -tau-> p1], [r] cannot move
     silently and is not equivalent to [p1] ([r] can do [b]), so p and r are
     not weakly bisimilar (Milner's [tau.a + b] vs [a + b], plus a [c]-branch
     giving every state a partner). Partition refinement splits on visible
     weak moves only, which do not tell them apart; it now also splits by
     [=ε=>], the blocks each state reaches by zero or more silent steps.
     [a.z] against [τ.a.z] checks the other way: they are weakly bisimilar,
     and stay so only because [=ε=>] includes zero steps. *)
  Inductive wst : Set := p | p1 | r | r1 | z.
  Inductive wlab : Set := a | b | c.
  Inductive wstep : wst -> option wlab -> wst -> Prop :=
  | p_tau : wstep p None p1 | p_b : wstep p (Some b) z
  | p_c : wstep p (Some c) p1 | p1_a : wstep p1 (Some a) z
  | r_a : wstep r (Some a) z | r_b : wstep r (Some b) z
  | r_c : wstep r (Some c) r1 | r1_a : wstep r1 (Some a) z.
  MeBi Config Weak As Option wlab.
  Fail MeBi Run Bisim p With wstep And r With wstep.
  MeBi Run Bisim p1 With wstep And r1 With wstep.
  Inductive tst : Set := u | v | v1 | w.
  Inductive tstep : tst -> option wlab -> tst -> Prop :=
  | u_a : tstep u (Some a) w | v_tau : tstep v None v1 | v1_a : tstep v1 (Some a) w.
  MeBi Run Bisim u With tstep And v With tstep.
  MeBi Config Reset Weak.
End CheckerVerdicts.

(* [MeBi Help]: every topic parses and prints (backlog item F). *)
MeBi Divider "Theories.Test.Help".
MeBi Help.
MeBi Help Run.
MeBi Help Sim.
MeBi Help Benchmark.
MeBi Help Premises.
MeBi Help Config.
MeBi Help Config Bounds.
MeBi Help Config Bounds Saturation.
MeBi Help Config Weak.
MeBi Help Config FailIf.
MeBi Help Config Solver.
MeBi Help Config Output.

(* [weak_bisimilar] is strictly finer than [weak_bisim] (mutual similarity),
   proved by hand on the textbook pair [a.b + a] and [a.b]: each simulates
   the other, but after [P -a-> Z] (the stuck branch) [Q] can only reach
   [Q1], which can still do [b]. No plugin command involved. *)
Module WeakBisimilarVsMutualSim.
  Inductive st : Set := P | P1 | Q | Q1 | Z.
  Inductive lab : Set := a | b.
  Inductive step : st -> option lab -> st -> Prop :=
  | p_a1 : step P (Some a) P1 | p_a2 : step P (Some a) Z
  | p1_b : step P1 (Some b) Z
  | q_a : step Q (Some a) Q1 | q1_b : step Q1 (Some b) Z.

  Lemma no_silent : forall x y, silent step x y -> x = y.
  Proof.
    intros x y H; destruct H as [|y' z' T _]; [reflexivity|].
    unfold tau in T; inversion T.
  Qed.

  Lemma z_sim : forall t, weak_sim step step Z t.
  Proof. intros t; constructor; constructor; intros m2 a' T; inversion T. Qed.

  Lemma p1_q1 : weak_sim step step P1 Q1.
  Proof.
    constructor; constructor; intros m2 a' T; inversion T; subst.
    exists Z; split; [exact (inject_weak _ _ _ q1_b) | apply z_sim].
  Qed.

  Lemma q1_p1 : weak_sim step step Q1 P1.
  Proof.
    constructor; constructor; intros m2 a' T; inversion T; subst.
    exists Z; split; [exact (inject_weak _ _ _ p1_b) | apply z_sim].
  Qed.

  Example mutually_similar : weak_bisim step step P Q.
  Proof.
    split; constructor; constructor; intros m2 a' T; inversion T; subst.
    - exists Q1; split; [exact (inject_weak _ _ _ q_a) | exact p1_q1].
    - exists Q1; split; [exact (inject_weak _ _ _ q_a) | apply z_sim].
    - exists P1; split; [exact (inject_weak _ _ _ p_a1) | exact q1_p1].
  Qed.

  Example mutually_similar' : mutual_sim step step P Q.
  Proof. exact mutually_similar. Qed.

  Example not_bisimilar : ~ weak_bisimilar step step P Q.
  Proof.
    intros H.
    destruct (bisim_l (out_bisim H) p_a2) as [n2 [W B]].
    inversion W as [a' z t S1 T S2|]; subst.
    apply no_silent in S1; apply no_silent in S2; subst.
    inversion T; subst.
    destruct (bisim_r (out_bisim B) q1_b) as [m2 [W' _]].
    inversion W' as [a'' z' t' S1' T' _|]; subst.
    apply no_silent in S1'; subst.
    inversion T'.
  Qed.
End WeakBisimilarVsMutualSim.

(* [MeBi Sim] proves [weak_bisimilar] goals: both obligations ([bisim_l],
   [bisim_r]) in one proof, the second by answering with the left system. *)
MeBi Divider "Theories.Test.WeakBisimilarProofs".
Module WeakBisimilarProofs.
  Import SilentResponse.
  MeBi Config Weak As Option lab.
  (* [tau.a + b] against a renamed copy: a silent answer on each side. *)
  Example bis_p_q : weak_bisimilar step step p q.
  Proof. MeBi Sim Begin step p And step q Using step. MeBi Sim Solve 100. Qed.
  Example bis_p_p : weak_bisimilar step step p p.
  Proof. MeBi Sim Begin step p And step p Using step. MeBi Sim Solve 100. Qed.
  MeBi Config Reset Weak.

  MeBi Config Weak As Option sobs.
  (* rocq-sims' [SimExample] pair (see [SilentResponse]). *)
  Example bis_t0_u0 : weak_bisimilar strans strans t0 u0.
  Proof. MeBi Sim Begin strans t0 And strans u0 Using strans. MeBi Sim Solve 200. Qed.
  MeBi Config Reset Weak.

  (* [a.b + a] and [a.b] are mutually similar but not bisimilar, so [Begin]
     refuses: the check it runs first decides weak bisimilarity. *)
  Import WeakBisimilarVsMutualSim.
  MeBi Config Weak As Option lab.
  Example bis_P_Q : weak_bisimilar step step P Q.
  Proof. Fail MeBi Sim Begin step P And step Q Using step. Abort.
  MeBi Config Reset Weak.
End WeakBisimilarProofs.

(* [weak_sim] asks for similarity, not bisimilarity: [MeBi Sim Begin] accepts
   states that are similar but not bisimilar, and the search falls back on the
   weak simulation preorder for answers (it used to refuse, Not_Bisimilar). *)
MeBi Divider "Theories.Test.SimilarNotBisimilar".
Module SimilarNotBisimilar.
  Inductive st : Set := p | p1 | q | q1 | z | m | m1 | r.
  Inductive lab : Set := a | b | c.
  Inductive step : st -> option lab -> st -> Prop :=
  | p_a : step p (Some a) p1 | p1_b : step p1 (Some b) z
  | q_a : step q (Some a) q1 | q1_b : step q1 (Some b) z
  | q1_c : step q1 (Some c) z
  | m_tau : step m None m1 | m_b : step m (Some b) z | m1_a : step m1 (Some a) z
  | r_a : step r (Some a) z | r_b : step r (Some b) z.
  MeBi Config Weak As Option lab.

  (* [a.b <= a.(b + c)]: after [a], [b] is simulated by [b + c]. *)
  Example sim_p_q : weak_sim step step p q.
  Proof. MeBi Sim Begin step p And step q Using step. MeBi Sim Solve 100. Qed.

  (* The converse is false, and [Begin] says so before searching. *)
  Example sim_q_p : weak_sim step step q p.
  Proof. Fail MeBi Sim Begin step q And step p Using step. Abort.

  (* Milner's [tau.a + b] and [a + b]: mutually similar, not bisimilar. Each
     direction proves, [mutual_sim] is their conjunction, and
     [weak_bisimilar] is refused. *)
  Example sim_m_r : weak_sim step step m r.
  Proof. MeBi Sim Begin step m And step r Using step. MeBi Sim Solve 100. Qed.
  Example sim_r_m : weak_sim step step r m.
  Proof. MeBi Sim Begin step r And step m Using step. MeBi Sim Solve 100. Qed.
  Example msim_m_r : mutual_sim step step m r.
  Proof. split; [exact sim_m_r | exact sim_r_m]. Qed.
  Example bis_m_r : weak_bisimilar step step m r.
  Proof. Fail MeBi Sim Begin step m And step r Using step. Abort.
  MeBi Config Reset Weak.
End SimilarNotBisimilar.

(* [MeBi Config Solver Answers]: every policy parses, and each proves the
   same goals ([Qed] checks the answers it chose). *)
MeBi Divider "Theories.Test.AnswerPolicies".
Module AnswerPolicies.
  Import SilentResponse.
  MeBi Config Weak As Option lab.
  MeBi Config Solver Answers Greedy.
  Example g_p_q : weak_bisimilar step step p q.
  Proof. MeBi Sim Begin step p And step q Using step. MeBi Sim Solve 200. Qed.
  MeBi Config Solver Answers Minimal.
  Example m_p_q : weak_bisimilar step step p q.
  Proof. MeBi Sim Begin step p And step q Using step. MeBi Sim Solve 200. Qed.
  Example m_sim_p_q : weak_sim step step p q.
  Proof. MeBi Sim Begin step p And step q Using step. MeBi Sim Solve 200. Qed.
  MeBi Config Solver Answers Auto.
  Example a_p_q : weak_bisimilar step step p q.
  Proof. MeBi Sim Begin step p And step q Using step. MeBi Sim Solve 200. Qed.
  MeBi Config Solver Answers Default.
  MeBi Config Reset Weak.
End AnswerPolicies.

(* [MeBi Run Bisim ... As <name>] states [weak_bisimilar], opens its proof
   and begins the proof search, as [Example ... Proof. MeBi Sim Begin ...]
   would: the same count (43, measured 2026-10-04). It refuses, opening
   nothing, if the two are not bisimilar. *)
MeBi Divider "Theories.Test.RunBisimAs".
Module RunBisimAs.
  Inductive st : nat -> option bool -> nat -> Prop :=
  | p0 : st 0 (Some true) 1 | p1 : st 1 (Some false) 0
  | q0 : st 10 (Some true) 11 | q1 : st 11 (Some false) 12
  | q2 : st 12 (Some true) 11
  | r0 : st 20 (Some true) 21.
  MeBi Config Weak As Option bool.
  MeBi Run Bisim 0 With st And 10 With st As pq_bis Using st.
  MeBi Sim Solve 100. Qed.
  Check pq_bis : weak_bisimilar st st 0 10.
  MeBi Run Bisim 0 With st And 10 With st As pq_bis2.
  MeBi Sim Solve 100. Qed.
  (* [20] is simulated by [0] but not bisimilar to it (Not_Bisimilar) *)
  Fail MeBi Run Bisim 20 With st And 0 With st As rp_bis Using st.
  Fail Check rp_bis.
  MeBi Config Reset Weak.
End RunBisimAs.

(* [MeBi Run Sim x With a And y With b]: is [x] weakly simulated by [y]?
   Bisimilar states are similar outright; otherwise the greatest weak
   simulation among the pairs reachable from the two start states decides
   (until 2026-10-04 computed over all pairs). Not similar is an error under
   [FailIf NotBisimilar] (default), else a warning. [... As <name>] states
   [weak_sim] and opens its proof, as [Sim Begin] would (same count, 9).
   When an FSM is saturated on demand, the walk needs [Bounds Game] and
   stays within it. *)
MeBi Divider "Theories.Test.RunSim".
Module RunSim.
  Inductive st : nat -> option bool -> nat -> Prop :=
  | p0 : st 0 (Some true) 1 | p1 : st 1 (Some false) 0
  | q0 : st 10 (Some true) 11 | q1 : st 11 (Some false) 12
  | q2 : st 12 (Some true) 11
  | r0 : st 20 (Some true) 21
  | t0 : st 30 None 31 | t1 : st 31 (Some true) 32.
  MeBi Config Weak As Option bool.
  MeBi Run Sim 20 With st And 0 With st Using st.
  MeBi Run Sim 0 With st And 10 With st.
  (* not similar: [0] can do [false] after [true], [20] cannot *)
  Fail MeBi Run Sim 0 With st And 20 With st Using st.
  MeBi Config FailIf NotBisimilar False.
  MeBi Run Sim 0 With st And 20 With st Using st.
  MeBi Config FailIf NotBisimilar True.
  MeBi Run Sim 20 With st And 0 With st As rp_sim Using st.
  MeBi Sim Solve 100. Qed.
  Check rp_sim : weak_sim st st 20 0.
  Fail MeBi Run Sim 0 With st And 20 With st As pr_sim.
  Fail Check pr_sim.
  (* on demand ([30] has a silent step): refused without [Bounds Game],
     allowed within it, refused past it *)
  MeBi Config Saturation OnDemand True.
  Fail MeBi Run Sim 30 With st And 0 With st Using st.
  MeBi Config Bounds Game 1000.
  MeBi Run Sim 30 With st And 0 With st Using st.
  Example tp_od : weak_sim st st 30 0.
  Proof. MeBi Sim Begin st 30 And st 0 Using st. MeBi Sim Solve 100. Qed.
  MeBi Config Bounds Game 1.
  Fail MeBi Run Sim 30 With st And 0 With st Using st.
  MeBi Config Reset Bounds.
  MeBi Config Reset Weak.
End RunSim.

(* Premises that are not an application: an implication or a [forall]. Until
   2026-10-02 extraction took them for a variable's type and dropped them
   without a warning, so the LTS silently gained transitions. *)
MeBi Divider "Theories.Test.ProductPremises".
Module ProductPremises.
  (* [n = 3 -> False] is [~ (n = 3)] unfolded, and now decided like it: the
     LTS is 0 -> 1 -> 2 -> 3, four states (it used to be six, 0..5, and fail
     the bound of 4 below). *)
  Inductive impl : nat -> bool -> nat -> Prop :=
  | i_go n : (n = 3 -> False) -> n < 5 -> impl n true (S n).
  MeBi Config Bounds As Num States 4.
  MeBi Run LTS 0 Using impl.
  MeBi Config Bounds As Num States 3.
  Fail MeBi Run LTS 0 Using impl.

  (* A bounded universal, [forall k, k < n -> P k] with [n] a numeral once
     the source is known, is decided value by value (2026-10-04, notes/14
     item 1; until then undecided, applied from every state, 0..5, and
     pinned KNOWN WRONG here). [k <> 2] for all [k < n]: n = 0..2, so
     states 0..3; [n = 0] holds vacuously. [<=] and [>] the same way. *)
  Inductive univ : nat -> bool -> nat -> Prop :=
  | u_go n : (forall k, k < n -> k <> 2) -> n < 5 -> univ n true (S n).
  Inductive univ_le : nat -> bool -> nat -> Prop :=
  | ul_go n : (forall k, k <= n -> k <> 3) -> n < 5 -> univ_le n true (S n).
  Inductive univ_gt : nat -> bool -> nat -> Prop :=
  | ug_go n : (forall k, n > k -> k <> 2) -> n < 5 -> univ_gt n true (S n).
  (* negated: holds from 3 on, so from 3, states 3..5 *)
  Inductive univ_neg : nat -> bool -> nat -> Prop :=
  | un_go n : ~ (forall k, k < n -> k <> 2) -> n < 5 -> univ_neg n true (S n).
  MeBi Config Bounds As Num States 4.
  MeBi Run LTS 0 Using univ.
  MeBi Run LTS 0 Using univ_le.
  MeBi Run LTS 0 Using univ_gt.
  MeBi Config Bounds As Num States 3.
  Fail MeBi Run LTS 0 Using univ.
  Fail MeBi Run LTS 0 Using univ_le.
  Fail MeBi Run LTS 0 Using univ_gt.
  MeBi Run LTS 3 Using univ_neg.
  MeBi Config Bounds As Num States 2.
  Fail MeBi Run LTS 3 Using univ_neg.
  MeBi Config Reset Bounds.
  (* [MeBi Config Premise Range]: [univ] at state 3 ranges over 3 values
     ([k < 3]). Within a range of 3 it is decided (4 states, as above); with
     2 it is left undecided, with a warning naming the range, so the LTS is
     incomplete and refused. *)
  MeBi Config Premise Range 3.
  MeBi Config Bounds As Num States 4.
  MeBi Run LTS 0 Using univ.
  MeBi Config Premise Range 2.
  Fail MeBi Run LTS 0 Using univ.
  MeBi Config Reset Premise.
  MeBi Config Reset Bounds.

  (* In proofs: a true universal premise is proved instance by instance
     ([MEBI.Premises]' lemmas), and a false one in a hypothesis (state 3 of
     [univ_s]) refuted at its false instance; the negated form both ways. *)
  Inductive univ_s : nat -> option bool -> nat -> Prop :=
  | us_go n : (forall k, k < n -> k <> 2) -> n < 5 -> univ_s n (Some true) (S n).
  Inductive univ_s' : nat -> option bool -> nat -> Prop :=
  | us_go' n : (forall k, k < n -> k <> 2) -> n < 5 -> univ_s' n (Some true) (S n).
  Inductive univ_n : nat -> option bool -> nat -> Prop :=
  | unn_go n : ~ (forall k, k < n -> k <> 2) -> n < 5 -> univ_n n (Some true) (S n).
  Inductive univ_n' : nat -> option bool -> nat -> Prop :=
  | unn_go' n : ~ (forall k, k < n -> k <> 2) -> n < 5 -> univ_n' n (Some true) (S n).
  MeBi Config Weak As Option bool.
  Example w_univ : weak_sim univ_s univ_s' 0 0.
  Proof. MeBi Sim Begin univ_s 0 And univ_s' 0 Using univ_s. MeBi Sim Solve 100. Qed.
  Example w_univ_neg : weak_sim univ_n univ_n' 3 3.
  Proof. MeBi Sim Begin univ_n 3 And univ_n' 3 Using univ_n. MeBi Sim Solve 100. Qed.
  MeBi Config Reset Weak.
End ProductPremises.

(* A premise over a constructor whose index is computed ([e k : ev k (dbl k)]).
   Until 2026-10-02 the bounded search could fail to match [ev 2 4] against
   [ev ?k (dbl ?k)] and counted that as a refutation: the true premise was
   "refuted" and the transitions silently dropped (one state, not three). A
   failed match now refutes only when the constructor's indices are patterns,
   and the match is retried argument by argument, so [ev 2 4] is proved. *)
MeBi Divider "Theories.Test.ComputedIndex".
Module ComputedIndex.
  Fixpoint dbl (n : nat) : nat := match n with 0 => 0 | S k => S (S (dbl k)) end.
  Inductive ev : nat -> nat -> Prop := e k : ev k (dbl k).
  Inductive st : nat -> bool -> nat -> Prop :=
  | go n : ev 2 4 -> n < 2 -> st n true (S n).
  MeBi Config Bounds As Num States 3.
  MeBi Run LTS 0 Using st.
  MeBi Config Bounds As Num States 2.
  Fail MeBi Run LTS 0 Using st.
  MeBi Config Reset Bounds.
End ComputedIndex.


(* Inversion shapes, for whoever tries backlog option C ("clear an inner
   hypothesis once inverted, keep the top-level transition"; note 7). Each
   proves today; the counts after each are the reference, measured after
   2026-10-02's hypothesis-order fix. C must keep every one proving, and
   should lower [Deep] (the shape it is for) without raising the others.

   - [Sync] and [Source] are the counterexample shapes: one inversion fixes
     a variable another hypothesis needs ([Sync]: two LTS premises share the
     label; [Source]: an [In] premise fixes the LTS premise's source). A rule
     that cleared or skipped a hypothesis inverted while still open would
     lose a case split there.
   - [Deep] is the shape C is for: one relation at every layer, so inverting
     the top hypothesis yields inner hypotheses of the same relation, most
     branches impossible (the CCS/ABP pattern, in miniature).
   - [Computed] is a known-wrong pin, found while writing these.
   The earlier "remember inverted hypotheses" attempt also broke
   [Proc/Test1] and [Proc/Test3] (note 7): run those too. *)
MeBi Divider "Theories.Test.InversionShapes".
Module InversionShapes.
  Import Stdlib.Lists.List.
  (* sync_sim: 26 iterations; sync_bis: 81 (29 and 105 before dead LTS
     steps were refuted instead of inverted, 2026-10-02, note 11 option D'). *)
  Module Sync.
    Inductive lab : Set := A | B.
    Inductive st : Set := s0 | s1 | s2 | t0 | t1.
    Inductive comp : st -> option lab -> st -> Prop :=
    | c_s0 : comp s0 (Some A) s1 | c_s1 : comp s1 (Some B) s2
    | c_s0b : comp s0 (Some B) s2
    | c_t0 : comp t0 (Some A) t1 | c_t1 : comp t1 (Some B) t0.
    Inductive sys : st * st -> option lab -> st * st -> Prop :=
    | sync p q a p' q' : comp p a p' -> comp q a q' -> sys (p, q) a (p', q').
    Inductive sys' : st * st -> option lab -> st * st -> Prop :=
    | sync' p q a p' q' : comp p a p' -> comp q a q' -> sys' (p, q) a (p', q').
    MeBi Config Weak As Option lab.
    Example sync_sim : weak_sim sys sys' (s0, t0) (s0, t0).
    Proof. MeBi Sim Begin sys (s0, t0) And sys' (s0, t0) Using sys sys' comp.
      MeBi Sim Solve 500. Qed.
    (* A pin: [Solve 80] permits 81 steps, the least that closes it. The
       pair [(s2, t0)] has no move ([s2] has none), so each obligation from
       it starts from a step that cannot happen, [sys (s2, t0) a m2];
       refuting that at once, rather than inverting it down to [comp], is
       what brings 105 down to 81, so without it this runs out of steps. *)
    Example sync_bis : weak_bisimilar sys sys' (s0, t0) (s0, t0).
    Proof. MeBi Sim Begin sys (s0, t0) And sys' (s0, t0) Using sys sys' comp.
      MeBi Sim Solve 80. Qed.
    MeBi Config Reset Weak.
  End Sync.

  (* source_sim: 71 iterations. *)
  Module Source.
    Inductive base : nat -> option bool -> nat -> Prop :=
    | b0 : base 0 (Some true) 1 | b1 : base 1 (Some false) 0.
    Inductive pick : list nat -> option bool -> list nat -> Prop :=
    | p_go q l a q' : In q l -> base q a q' -> pick l a (q' :: nil).
    Inductive pick' : list nat -> option bool -> list nat -> Prop :=
    | p_go' q l a q' : In q l -> base q a q' -> pick' l a (q' :: nil).
    MeBi Config Weak As Option bool.
    Example source_sim : weak_sim pick pick' (0 :: 1 :: nil) (0 :: 1 :: nil).
    Proof. MeBi Sim Begin pick (0 :: 1 :: nil) And pick' (0 :: 1 :: nil) Using pick pick' base.
      MeBi Sim Solve 500. Qed.
    MeBi Config Reset Weak.
  End Source.

  (* deep_bis: 120 iterations, unchanged by refuting dead steps: a
     constructor-headed source ([pre], [sum], [par]) lets [inversion] itself
     discard the impossible rules. It is a source that needs computing --
     CCS's [var k], unfolding to [def k] -- that inversion cannot see
     through, and there dead steps are common (61% of all inversions on the
     Alternating Bit Protocol). *)
  Module Deep.
    Inductive name : Set := x | y.
    Inductive pr : Set :=
    | nil0 | pre (n : option name) (p : pr) | sum (p q : pr) | par (p q : pr).
    Inductive step : pr -> option name -> pr -> Prop :=
    | s_pre n p : step (pre n p) n p
    | s_suml p q n p' : step p n p' -> step (sum p q) n p'
    | s_sumr p q n q' : step q n q' -> step (sum p q) n q'
    | s_parl p q n p' : step p n p' -> step (par p q) n (par p' q)
    | s_parr p q n q' : step q n q' -> step (par p q) n (par p q').
    Definition l1 := par (sum (pre (Some x) nil0) (pre None nil0)) (pre (Some y) nil0).
    Definition r1 := par (pre (Some y) nil0) (sum (pre (Some x) nil0) (pre None nil0)).
    MeBi Config Weak As Option name.
    Example deep_bis : weak_bisimilar step step l1 r1.
    Proof. MeBi Sim Begin step l1 And step r1 Using step.
      MeBi Sim Solve 2000. Qed.
    MeBi Config Reset Weak.
  End Deep.

  Module Computed.
    Inductive succ_rel : nat -> nat -> Prop := sr n : succ_rel n (S n).
    Inductive st : nat -> option bool -> nat -> Prop :=
    | go n m : n < 3 -> succ_rel n m -> st n (Some true) m
    | back : st 3 (Some false) 0.
    Inductive st' : nat -> option bool -> nat -> Prop :=
    | go' n m : n < 3 -> succ_rel n m -> st' n (Some true) m
    | back' : st' 3 (Some false) 0.
    MeBi Config Weak As Option bool.
    (* computed_sim: 42 iterations; computed_bis: 83. A guard headed by a
       definition ([n < 3] is [lt]) before a target-computing premise. Until
       2026-10-02 the solver did not recognise [0 < 3] as a premise (it
       judged it by its head, [lt], a constant) and applied [rt1n_refl] to
       it ("Unable to unify clos_refl_trans_1n ... with 0 < 3"), as
       [weak_sim] and [weak_bisimilar], in every cofix mode. *)
    Example computed_sim : weak_sim st st' 0 0.
    Proof. MeBi Sim Begin st 0 And st' 0 Using st st'.
      MeBi Sim Solve 500. Qed.
    Example computed_bis : weak_bisimilar st st' 0 0.
    Proof. MeBi Sim Begin st 0 And st' 0 Using st st'.
      MeBi Sim Solve 500. Qed.
    MeBi Config Reset Weak.
  End Computed.
End InversionShapes.

(* Proofs up to silent steps (notes/13, option 4): the plugin proves one pair,
   and [Bisimilarity.v]'s transfer lemmas give the other members of a silent
   cycle. [cyc]: 0 and 1 reach each other silently, and 1 does [a] to 2;
   [lin] does [a] from 0. Added 2026-10-03. *)
MeBi Divider "Theories.Test.SilentTransfer".
Module SilentTransfer.
  Inductive cyc : nat -> option bool -> nat -> Prop :=
  | c01 : cyc 0 None 1 | c10 : cyc 1 None 0 | c12 : cyc 1 (Some true) 2.
  (* [lin]'s states are numbered apart from [cyc]'s on purpose: when the
     two systems' state terms coincide, the bisimilarity check merges them
     as one state (found 2026-10-03, open: notes/13). *)
  Inductive lin : nat -> option bool -> nat -> Prop :=
  | l01 : lin 10 (Some true) 11.
  MeBi Config Weak As Option bool.
  Example sim_0 : weak_sim cyc lin 0 10.
  Proof. MeBi Sim Begin cyc 0 And lin 10 Using cyc. MeBi Sim Solve 100. Qed.
  Example bis_0 : weak_bisimilar cyc lin 0 10.
  Proof. MeBi Sim Begin cyc 0 And lin 10 Using cyc. MeBi Sim Solve 100. Qed.
  Example sim_rev_0 : weak_sim lin cyc 10 0.
  Proof. MeBi Sim Begin lin 10 And cyc 0 Using lin. MeBi Sim Solve 100. Qed.
  MeBi Config Reset Weak.

  Lemma s01 : silent cyc 0 1.
  Proof. eapply Relation_Operators.rt1n_trans; [constructor | constructor]. Qed.
  Lemma s10 : silent cyc 1 0.
  Proof. eapply Relation_Operators.rt1n_trans; [constructor | constructor]. Qed.

  (* 1 from 0, on the left *)
  Example sim_1 : weak_sim cyc lin 1 10.
  Proof. exact (weak_sim_silent_l 0 1 10 s01 sim_0). Qed.
  Example bis_1 : weak_bisimilar cyc lin 1 10.
  Proof. exact (weak_bisimilar_silent_l 0 1 10 s01 s10 bis_0). Qed.
  (* and on the right *)
  Example sim_rev_1 : weak_sim lin cyc 10 1.
  Proof. exact (weak_sim_silent_r 10 1 0 s10 sim_rev_0). Qed.
  Example bis_rev_1 : weak_bisimilar lin cyc 10 1.
  Proof.
    exact (weak_bisimilar_silent_r 10 1 0 s10 s01
             (weak_bisimilar_sym _ _ bis_0)).
  Qed.

  Print Assumptions weak_sim_silent_l.
  Print Assumptions weak_sim_silent_r.
  Print Assumptions weak_bisimilar_silent_l.
  Print Assumptions weak_bisimilar_silent_r.
End SilentTransfer.

(* Two systems whose state terms coincide (two relations over [nat], both
   from [0]). Until 2026-10-03 the bisimilarity check merged them into one
   state: [lin] (does [a]) and [other] (does [b]) were reported bisimilar,
   and the [weak_sim] proof below stopped on an internal error. When a shared
   state moves differently on each side, the second system's copies are now
   renamed apart (with a notice), and the check is right. *)
MeBi Divider "Theories.Test.OverlappingStates".
Module OverlappingStates.
  Inductive cyc : nat -> option bool -> nat -> Prop :=
  | c01 : cyc 0 None 1 | c10 : cyc 1 None 0 | c12 : cyc 1 (Some true) 2.
  Inductive cyc' : nat -> option bool -> nat -> Prop :=
  | c01' : cyc' 0 None 1 | c10' : cyc' 1 None 0 | c12' : cyc' 1 (Some true) 2.
  Inductive lin : nat -> option bool -> nat -> Prop := l01 : lin 0 (Some true) 1.
  Inductive other : nat -> option bool -> nat -> Prop :=
  | o01 : other 0 (Some false) 1.
  Inductive lin' : nat -> option bool -> nat -> Prop := l01' : lin' 0 (Some true) 1.
  MeBi Config Weak As Option bool.
  (* not bisimilar, and now said so (Not_Bisimilar) *)
  Fail MeBi Run Bisim 0 With lin And 0 With other Using lin other.
  (* bisimilar, with states 0 and 1 shared but moving differently *)
  MeBi Run Bisim 0 With cyc And 0 With lin Using cyc lin.
  Example sim : weak_sim cyc lin 0 0.
  Proof. MeBi Sim Begin cyc 0 And lin 0 Using cyc. MeBi Sim Solve 100. Qed.
  Example bis : weak_bisimilar cyc lin 0 0.
  Proof. MeBi Sim Begin cyc 0 And lin 0 Using cyc. MeBi Sim Solve 100. Qed.
  Example sim_rev : weak_sim lin cyc 0 0.
  Proof. MeBi Sim Begin lin 0 And cyc 0 Using lin. MeBi Sim Solve 100. Qed.
  (* the same moves on both sides: one state, nothing renamed *)
  MeBi Run Bisim 0 With lin And 0 With lin' Using lin lin'.
  Example sim_copy : weak_sim cyc cyc' 0 0.
  Proof. MeBi Sim Begin cyc 0 And cyc' 0 Using cyc. MeBi Sim Solve 100. Qed.
  MeBi Config Reset Weak.
End OverlappingStates.

(* Why proofs cannot be shortened to one state per silent SCC (notes/13,
   stage 3, abandoned 2026-10-03). [r] and [m] reach each other silently,
   [m] can do [b], [n] can do nothing: [n] does not simulate [r]. Inside a
   coinduction, answering [r]'s silent move to [m] by transfer from the pair
   being proved ([weak_sim_silent_l] applied to the cofix hypothesis) would
   "prove" it: circular, so unsound -- and Rocq's guard check rejects it.
   The transfer lemmas are sound only outside a coinduction
   ([SilentTransfer]). *)
MeBi Divider "Theories.Test.CircularTransfer".
Module CircularTransfer.
  Inductive cx : nat -> option bool -> nat -> Prop :=
  | x_rm : cx 0 None 1 | x_mr : cx 1 None 0 | x_mb : cx 1 (Some false) 2.
  Inductive nx : nat -> option bool -> nat -> Prop := .
  MeBi Config Weak As Option bool.
  (* the plugin refuses the goal: not similar *)
  Example refused : weak_sim cx nx 0 10.
  Proof. Fail MeBi Sim Begin cx 0 And nx 10 Using cx. Abort.
  MeBi Config Reset Weak.

  Lemma s_rm : silent cx 0 1.
  Proof. eapply Relation_Operators.rt1n_trans; [constructor | constructor]. Qed.

  (* the circular "proof" is not accepted *)
  Example circular : weak_sim cx nx 0 10.
  Proof.
    cofix CH.
    constructor; constructor; intros m2 a T.
    inversion T; subst.
    exists 10; split.
    - constructor. constructor.
    - exact (weak_sim_silent_l 0 1 10 s_rm CH).
    Fail Guarded.
  Abort.

  (* and the goal is false *)
  Example not_similar : ~ weak_sim cx nx 0 10.
  Proof.
    intros H.
    destruct (weak_sim_silent_clos H s_rm) as [n2 [S W]].
    destruct (sim_weak (out_sim W) x_mb) as [n3 [Wk _]].
    inversion Wk as [b z t PRE ACT POST | ]; subst.
    (* [nx] has no transitions: the silent path from 10 is empty, and no
       step can follow it *)
    inversion S; subst; [ | match goal with H : tau nx _ _ |- _ => inversion H end ].
    inversion PRE; subst; [ inversion ACT | match goal with H : tau nx _ _ |- _ => inversion H end ].
  Qed.
End CircularTransfer.

(* One label term silent on one side and visible on the other: only
   [Weak2] is set, so the first system has no silent label (its labels do
   not know whether they are silent) and the second treats [None] as
   silent. Until 2026-10-10 [Label.compare] treated "unknown" as equal to
   both [Some true] and [Some false], so the merged alphabet kept one copy
   of [None]'s label, whichever the set kept: here the second system's
   silent one, and the first system's visible [None] step went unchecked.
   Strong [p] was then "bisimilar" to weak [p] but not to weak [q], though
   weak [p] and weak [q] are equivalent. *)
MeBi Divider "Theories.Test.MixedSilence".
Module MixedSilence.
  Inductive act := A.
  Inductive term := done | step (l : option act) (t : term).
  Inductive lts : term -> option act -> term -> Prop :=
  | do_step : forall l t, lts (step l t) l t.

  Example p := step None (step (Some A) done).
  Example q := step (Some A) done.

  MeBi Config Reset Weak.
  MeBi Config Weak2 As Option act.
  (* strong [p] has a visible [None] step that weak [p] and [q] cannot match *)
  Fail MeBi Run Bisim p With lts And p With lts Using lts.
  Fail MeBi Run Bisim p With lts And q With lts Using lts.
  (* strong [q] is weak [p] without its silent step *)
  MeBi Run Bisim q With lts And p With lts Using lts.
  MeBi Config Reset Weak.
End MixedSilence.
