Require Import MEBI.loader.

Inductive i := C0 (i : nat) | C1 (b : bool) (j : nat) | C2 (x : nat).

Fail MeBi LTS i 0.

Fail MeBi LTS j 0.

Definition k := 0.
Fail MeBi LTS k 0.

Fail MeBi LTS nat 0.
Definition nnat := nat.
Fail MeBi LTS nnat 0.

Fail MeBi LTS False 0.

CoInductive co_nat := CoZ | CoS : co_nat -> co_nat.

Inductive test_lts A : co_nat -> nat -> nat -> Prop :=
| less_lt (x : A) (i : co_nat) (j : nat) : test_lts A (CoS i) 1 j.

Fail MeBi LTS test_lts 0.

Inductive test_mut A : Prop := Mk1 (x : A) (y : test_mut2 A)
with test_mut2 A : Prop := Mk2 (y : test_mut A).

Fail MeBi LTS test_mut2 0.


Inductive testLTS : nat -> bool -> nat -> Prop :=
  | test1 n : testLTS (S n) true n
  | test2 : testLTS (S 0) false (S 0).

Definition one := 1.

Fail MeBi LTS testLTS false.

MeBi LTS testLTS 0.
MeBi LTS testLTS (S 0).
MeBi LTS testLTS (S (S 0)).
MeBi LTS testLTS (S (S (S 0))).

MeBi LTS testLTS one.
MeBi LTS testLTS (S one).


Inductive nonTerminatingTestLTS : nat -> bool -> nat -> Prop :=
  | test1' n : nonTerminatingTestLTS n true (S n)
  | test2' n : nonTerminatingTestLTS (S n) false n.

MeBi LTS nonTerminatingTestLTS 0.
MeBi LTS nonTerminatingTestLTS (S 0).
MeBi LTS nonTerminatingTestLTS (S (S 0)).
MeBi LTS nonTerminatingTestLTS (S (S (S 0))).

(* Definition boundedLTS (p:Prop) (n:nat) : Type := Prop * nat. *)

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

  MeBi LTS termLTS (tfix (tact TheAction1 tend)).
  MeBi LTS termLTS (tfix (tact TheAction1 (tact TheAction2 trec))).
End Test1.

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

  MeBi LTS termLTS (tfix (tact TheAction1 tend)).
  MeBi LTS termLTS (tfix (tact TheAction1 (tact TheAction2 trec))).
  MeBi LTS termLTS (tfix (tpar TheAction1 TheAction2 trec)).
End Test2.




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
