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

  (* [p] saturates to a handful of weak actions: over a bound of 1. *)
  MeBi Config Bounds Saturation 1.
  Fail MeBi Run Saturate p Using termLTS.
  Fail MeBi Run Minimize p Using termLTS.
  Fail MeBi Run Bisim p With termLTS And q With termLTS Using termLTS.
  Example wsim_refused : weak_sim termLTS termLTS p q.
  Proof. Fail MeBi Sim Begin termLTS p And termLTS q Using termLTS. Abort.

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
   the bounds do: with [FailIf Incomplete] (the default), [Bounds As Num
   States n] succeeds iff the LTS has at most [n] states, so "succeeds at n,
   fails at n - 1" pins the count exactly. Likewise transitions, and
   [Bounds Saturation] pins the number of weak actions saturation produces.

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

  (* KNOWN WRONG (stage 1): undecidable premises still over-approximate --
     an opaque function, and a negation (deciding ~P needs a complete search
     of P and a proof of the negation, not built). When supported, these
     Fails start failing: make them positive. *)
  Parameter f : nat -> nat.
  Inductive op_c : nat -> option bool -> nat -> Prop :=
  | op_go n : f n <= 2 -> op_c n (Some true) (S n).
  Inductive not_c : nat -> option bool -> nat -> Prop :=
  | not_go n : ~ (3 <= n) -> not_c n (Some true) (S n).
  MeBi Config Bounds As Num States 20.
  Fail MeBi Run LTS 0 Using op_c.
  Fail MeBi Run LTS 0 Using not_c.
  MeBi Config Reset Bounds.
End GeneralPremises.

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
