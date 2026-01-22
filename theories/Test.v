Require Import MEBI.loader.

(** testing getters & setters *)
MeBi Divider "Theories.Test.BasicCommands".
Module BasicCommands.
  MeBi Config Bound 1000. 
  MeBi Config Reset Bound.

  MeBi Config Fail If Incomplete True. 
  MeBi Config Fail If Incomplete False. 

  MeBi Config Fail If NotBisim True. 
  MeBi Config Fail If NotBisim False. 

  MeBi Config Output Enable.  
  MeBi Config Output Disable. 

  MeBi Config Output Debug Enable.  
  MeBi Config Output Debug Disable. 
  
  MeBi Config Output Info Enable.  
  MeBi Config Output Info Disable. 

  MeBi Config Output Notice Enable.  
  MeBi Config Output Notice Disable. 
  
  MeBi Config Output Warning Enable. 
  MeBi Config Output Warning Disable. 

  MeBi Config Output Error Enable. 
  MeBi Config Output Error Disable. 

  MeBi Config Output Trace Enable.  
  MeBi Config Output Trace Disable. 

  MeBi Config Output Results Enable.  
  MeBi Config Output Results Disable. 

  MeBi Config Output Show Enable.  
  MeBi Config Output Show Disable. 

  MeBi Config WeakMode Enable.  
  MeBi Config WeakMode Disable. 
  
  MeBi Config Weak As Option nat. 
  

  Example z := 0.
  MeBi Config Weak As z Of nat. 
  

  Inductive s_label : Set :=
  | TAU : s_label 
  | LABEL1 : nat -> s_label
  .
  MeBi Config Weak As TAU Of s_label. 
  

  Inductive t_label : Type :=
  | SILENT : t_label 
  | LABEL2 : bool -> t_label
  .
  MeBi Config Weak As SILENT Of t_label. 
  
End BasicCommands.

MeBi Config Reset.
MeBi Config Output Enable.
MeBi Config Output Notice Enable.
MeBi Config Output Debug Enable.
MeBi Config Output Info Enable.
MeBi Config Output Results Enable.
MeBi Config Output Warning Enable.
(* MeBi Config Bound 100. *)
(* MeBi Config WeakMode Enable. *)
(* MeBi Config Weak As Option _. *)
(* MeBi See All.  *)
MeBi Config Output Enable.

MeBi Divider "Theories.Test.GeneralTests".
Inductive i := C0 (i : nat) | C1 (b : bool) (j : nat) | C2 (x : nat).

Fail MeBi LTS 0 Using i.
Fail MeBi LTS 0 Using j.

Definition k := 0.
Fail MeBi LTS 0 Using k.
Fail MeBi LTS 0 Using nat.

Definition nnat := nat.
Fail MeBi LTS 0 Using nnat.
Fail MeBi LTS 0 Using False.

CoInductive co_nat := CoZ | CoS : co_nat -> co_nat.

Inductive test_lts A : co_nat -> nat -> nat -> Prop :=
| less_lt (x : A) (i : co_nat) (j : nat) : test_lts A (CoS i) 1 j.

Fail MeBi LTS 0 Using test_lts.

Inductive test_mut A : Prop := Mk1 (x : A) (y : test_mut2 A)
with test_mut2 A : Prop := Mk2 (y : test_mut A).

Fail MeBi LTS 0 Using test_mut2.


Inductive testLTS : nat -> bool -> nat -> Prop :=
  | test1 n : testLTS (S n) true n
  | test2 : testLTS (S 0) false (S 0).

Definition one := 1.

Fail MeBi LTS false Using testLTS.

MeBi LTS 0 Using testLTS.
MeBi LTS (S 0) Using testLTS.
MeBi LTS (S (S 0)) Using testLTS.
MeBi LTS (S (S (S 0))) Using testLTS.

MeBi LTS one Using testLTS.
MeBi LTS (S one) Using testLTS.

MeBi LTS (S one) Using testLTS.


Inductive nonTerminatingTestLTS : nat -> bool -> nat -> Prop :=
  | test1' n : nonTerminatingTestLTS n true (S n)
  | test2' n : nonTerminatingTestLTS (S n) false n
  .

(* below cannot be finitely represented  *)
MeBi Config Fail If Incomplete True.
Fail MeBi LTS 0 Using nonTerminatingTestLTS.
Fail MeBi LTS (S 0) Using nonTerminatingTestLTS.
Fail MeBi LTS (S (S 0)) Using nonTerminatingTestLTS.
Fail MeBi LTS (S (S (S 0))) Using nonTerminatingTestLTS.

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

  MeBi LTS (tfix (tact TheAction1 tend)) Using termLTS.
  MeBi LTS (tfix (tact TheAction1 (tact TheAction2 trec))) Using termLTS.
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

  MeBi LTS (tfix (tact TheAction1 tend)) Using termLTS.
  MeBi LTS (tfix (tact TheAction1 (tact TheAction2 trec))) Using termLTS.
  MeBi LTS (tfix (tpar TheAction1 TheAction2 trec)) Using termLTS.
  MeBi LTS (tfix (tpar TheAction1 TheAction2 trec)) Using termLTS.
End Test2.

MeBi Divider "Theories.Test.BisimTest1".
MeBi Config Fail If Incomplete True.
MeBi Config WeakMode Disable.
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
  MeBi Bisim (tact TheAction1 tend) With termLTS
         And (tact TheAction1 tend) With termLTS
         Using termLTS.

  (* true *)
  MeBi Bisim (tact TheAction2 tend) With termLTS
         And (tact TheAction2 tend) With termLTS
         Using termLTS.

  (* false *)
  Fail 
  MeBi Bisim (tact TheAction1 tend) With termLTS
         And (tact TheAction2 tend) With termLTS
         Using termLTS.

  (* false *)
  Fail 
  MeBi Bisim (tact TheAction2 tend) With termLTS
         And (tact TheAction1 tend) With termLTS
         Using termLTS.

  (* true *)
  MeBi Bisim (tact TheAction1 (tact TheAction2 tend)) With termLTS
         And (tact TheAction1 (tact TheAction2 tend)) With termLTS
         Using termLTS.

  (* true *)
  MeBi Bisim (tact TheAction2 (tact TheAction1 tend)) With termLTS
         And (tact TheAction2 (tact TheAction1 tend)) With termLTS
         Using termLTS.

  (* false *) 
  Fail 
  MeBi Bisim (tact TheAction1 (tact TheAction2 tend)) With termLTS
         And (tact TheAction2 (tact TheAction1 tend)) With termLTS
         Using termLTS.

  (* true *)
  MeBi Bisim (tpar TheAction1 TheAction2 tend) With termLTS
         And (tpar TheAction1 TheAction2 tend) With termLTS
         Using termLTS.

  (* true *)
  MeBi Bisim (tpar TheAction1 TheAction2 tend) With termLTS
         And (tpar TheAction2 TheAction1 tend) With termLTS
         Using termLTS.

  (* true *)
  MeBi Bisim (tpar TheAction2 TheAction1 tend) With termLTS
         And (tpar TheAction1 TheAction2 tend) With termLTS
         Using termLTS.

  (* false *)
  Fail 
  MeBi Bisim (tpar TheAction1 TheAction1 tend) With termLTS
         And (tact TheAction1 tend) With termLTS
         Using termLTS.

  (* false *)
  Fail 
  MeBi Bisim (tpar TheAction1 TheAction2 tend) With termLTS
         And (tact TheAction1 tend) With termLTS
         Using termLTS.

  (* true *)
  MeBi Bisim (tfix (tact TheAction1 trec)) With termLTS
         And (tfix (tact TheAction1 trec)) With termLTS
         Using termLTS.

  (* true *)
  MeBi Bisim (tfix (tact TheAction1 trec)) With termLTS
         And (tact TheAction1 (tfix (tact TheAction1 trec))) With termLTS
         Using termLTS.

  (* true *)
  MeBi Bisim (tfix (tact TheAction1 (tact TheAction2 trec))) With termLTS
         And (tact TheAction1 (tact TheAction2 (tfix (tact TheAction1 (tact TheAction2 trec))))) With termLTS
         Using termLTS.

  (* saturate *)
  (* MeBi Saturate 
    (tact TheAction1 (tact TheAction2 (tfix (tact TheAction1 (tact TheAction2 trec))))) 
    Using termLTS. *)

  (* minimize *)
  MeBi Minimize 
    (tact TheAction1 (tact TheAction2 (tfix (tact TheAction1 (tact TheAction2 trec))))) 
    Using termLTS.
End BisimTest1.

MeBi Divider "Theories.Test.BisimTest2".
MeBi Config Fail If Incomplete True.
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

  MeBi Config WeakMode Disable.

  Example exa1 := (tact TheAction1 (tact TheAction2 (tfix (tact TheAction1 (tact TheAction2 trec))))).
  MeBi FSM exa1 Using termLTS.
  MeBi Bisim exa1 With termLTS And exa1 With termLTS Using termLTS.

  MeBi Config WeakMode Enable.
  MeBi Config Weak As TAU Of action.

  MeBi FSM exa1 Using termLTS.
  MeBi Saturate exa1 Using termLTS.
  MeBi Minimize exa1 Using termLTS.

  MeBi Bisim exa1 With termLTS And exa1 With termLTS Using termLTS.

  Example exa2 := (tfix (tact TheAction1 (tact TheAction2 trec))).
  MeBi FSM exa2 Using termLTS.
  MeBi Saturate exa2 Using termLTS.
  MeBi Minimize exa2 Using termLTS.
  
  Example exa3 := (tact TheAction1 (tfix (tact TheAction2 (tact TheAction1 trec)))).
  MeBi FSM exa3 Using termLTS.
  MeBi Saturate exa3 Using termLTS.
  MeBi Minimize exa3 Using termLTS.
  
  MeBi Bisim exa1 With termLTS And exa2 With termLTS Using termLTS.
  MeBi Bisim exa1 With termLTS And exa3 With termLTS Using termLTS.

  MeBi Bisim exa2 With termLTS And exa1 With termLTS Using termLTS.
  MeBi Bisim exa2 With termLTS And exa3 With termLTS Using termLTS.

  MeBi Bisim exa3 With termLTS And exa1 With termLTS Using termLTS.
  MeBi Bisim exa3 With termLTS And exa2 With termLTS Using termLTS.
End BisimTest2.

MeBi Divider "Theories.Test.BisimTest3".
MeBi Config Fail If Incomplete True. 
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

  MeBi Config WeakMode Disable.

  Example exa1 := (tact TheAction1 (tact TheAction2 (tfix (tact TheAction1 (tact TheAction2 trec))))).
  MeBi FSM exa1 Using termLTS.
  MeBi Bisim exa1 With termLTS And exa1 With termLTS Using termLTS.

  MeBi Config WeakMode Enable.
  MeBi Config Weak As Option action.

  MeBi FSM exa1 Using termLTS.
  MeBi Saturate exa1 Using termLTS.
  MeBi Minimize exa1 Using termLTS.

  MeBi Bisim exa1 With termLTS And exa1 With termLTS Using termLTS.

  Example exa2 := (tfix (tact TheAction1 (tact TheAction2 trec))).
  MeBi FSM exa2 Using termLTS.
  MeBi Saturate exa2 Using termLTS.
  MeBi Minimize exa2 Using termLTS.
  
  Example exa3 := (tact TheAction1 (tfix (tact TheAction2 (tact TheAction1 trec)))).
  MeBi FSM exa3 Using termLTS.
  MeBi Saturate exa3 Using termLTS.
  MeBi Minimize exa3 Using termLTS.
  
  MeBi Bisim exa1 With termLTS And exa2 With termLTS Using termLTS.
  MeBi Bisim exa1 With termLTS And exa3 With termLTS Using termLTS.

  MeBi Bisim exa2 With termLTS And exa1 With termLTS Using termLTS.
  MeBi Bisim exa2 With termLTS And exa3 With termLTS Using termLTS.

  MeBi Bisim exa3 With termLTS And exa1 With termLTS Using termLTS.
  MeBi Bisim exa3 With termLTS And exa2 With termLTS Using termLTS.
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

  MeBi LTS termLTS (tfix (tact TheAction1 tend)).
  MeBi LTS termLTS (tfix (tact TheAction1 (tact TheAction2 trec))).

  MeBi LTS termLTS (tfix (tpar TheAction1 TheAction2 trec)).
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

  MeBi LTS termLTS (tfix (tact TheAction1 tend)).
  MeBi LTS termLTS (tfix (tact TheAction1 (tact TheAction2 trec))).

  MeBi LTS termLTS (tfix (tpar TheAction1 TheAction2 trec)).
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
