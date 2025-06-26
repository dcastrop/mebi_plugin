Require Import MEBI.loader.

Inductive i := C0 (i : nat) | C1 (b : bool) (j : nat) | C2 (x : nat).

Fail MeBi Check LTS Of 0 Using i.
Fail MeBi Check LTS Of 0 Using j.

Definition k := 0.
Fail MeBi Check LTS Of 0 Using k.
Fail MeBi Check LTS Of 0 Using nat.

Definition nnat := nat.
Fail MeBi Check LTS Of 0 Using nnat.
Fail MeBi Check LTS Of 0 Using False.

CoInductive co_nat := CoZ | CoS : co_nat -> co_nat.

Inductive test_lts A : co_nat -> nat -> nat -> Prop :=
| less_lt (x : A) (i : co_nat) (j : nat) : test_lts A (CoS i) 1 j.

Fail MeBi Check LTS Of 0 Using test_lts.

Inductive test_mut A : Prop := Mk1 (x : A) (y : test_mut2 A)
with test_mut2 A : Prop := Mk2 (y : test_mut A).

Fail MeBi Check LTS Of 0 Using test_mut2.


Inductive testLTS : nat -> bool -> nat -> Prop :=
  | test1 n : testLTS (S n) true n
  | test2 : testLTS (S 0) false (S 0).

Definition one := 1.

Fail MeBi Check LTS Of false Using testLTS.

MeBi Check LTS Of 0 Using testLTS.
MeBi Check LTS Of (S 0) Using testLTS.
MeBi Check LTS Of (S (S 0)) Using testLTS.
MeBi Check LTS Of (S (S (S 0))) Using testLTS.

MeBi Check LTS Of one Using testLTS.
MeBi Check LTS Of (S one) Using testLTS.

MeBi Check LTS Bounded 30 Of (S one) Using testLTS.


Inductive nonTerminatingTestLTS : nat -> bool -> nat -> Prop :=
  | test1' n : nonTerminatingTestLTS n true (S n)
  | test2' n : nonTerminatingTestLTS (S n) false n
  .

(* below cannot be finitely represented  *)
MeBi Check LTS Bounded 50 Of 0 Using nonTerminatingTestLTS.
MeBi Check LTS Of (S 0) Using nonTerminatingTestLTS.
MeBi Check LTS Of (S (S 0)) Using nonTerminatingTestLTS.
MeBi Check LTS Of (S (S (S 0))) Using nonTerminatingTestLTS.

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

  MeBi Check LTS Is Bounded 2 Of (tfix (tact TheAction1 tend)) Using termLTS.

  MeBi Check LTS Is Bounded 2 
    Of (tfix (tact TheAction1 (tact TheAction2 trec)))
    Using termLTS.

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

  MeBi Check LTS Is Bounded 2 
    Of (tfix (tact TheAction1 tend))
    Using termLTS.

  MeBi Check LTS Is Bounded 2 
    Of (tfix (tact TheAction1 (tact TheAction2 trec)))
    Using termLTS.

  MeBi Check LTS Is Bounded 2 
    Of (tfix (tpar TheAction1 TheAction2 trec))
    Using termLTS.

  MeBi Check LTS Is Bounded 2 
    Of (tfix (tpar TheAction1 TheAction2 trec))
    Using termLTS.

End Test2.



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
  MeBi 
    (* Dump "test_01" *)
    Check 
      Bisim LTS Of (tact TheAction1 tend) With termLTS
        And LTS Of (tact TheAction1 tend) With termLTS
        Using termLTS.

  (* true *)
  MeBi 
    (* Dump "test_02" *)
    Check 
      Bisim LTS Of (tact TheAction2 tend) With termLTS
        And LTS Of (tact TheAction2 tend) With termLTS
        Using termLTS.

  (* false *)
  MeBi 
    (* Dump "test_03" *)
    Check 
    Bisim LTS Of (tact TheAction1 tend) With termLTS
      And LTS Of (tact TheAction2 tend) With termLTS
      Using termLTS.

  (* true *)
  MeBi 
    (* Dump "test_04" *)
    Check 
      Bisim LTS Of (tact TheAction1 (tact TheAction2 tend)) With termLTS
        And LTS Of (tact TheAction1 (tact TheAction2 tend)) With termLTS
        Using termLTS.

  (* true *)
  MeBi 
    (* Dump "test_05" *)
    Check 
      Bisim LTS Of (tact TheAction2 (tact TheAction1 tend)) With termLTS
        And LTS Of (tact TheAction2 (tact TheAction1 tend)) With termLTS
        Using termLTS.

  (* false *) 
  MeBi 
    (* Dump "test_06" *)
    Check 
      Bisim LTS Of (tact TheAction2 (tact TheAction1 tend)) With termLTS
        And LTS Of (tact TheAction1 (tact TheAction2 tend)) With termLTS
        Using termLTS.

  (* true *)
  MeBi
    (* Dump "test_07" *)
    Check 
    Bisim LTS Of (tpar TheAction1 TheAction2 tend) With termLTS
      And LTS Of (tpar TheAction1 TheAction2 tend) With termLTS
      Using termLTS.

  (* true *)
  MeBi 
    (* Dump "test_08" *)
    Check 
      Bisim LTS Of (tpar TheAction2 TheAction1 tend) With termLTS
        And LTS Of (tpar TheAction2 TheAction1 tend) With termLTS
        Using termLTS.

  (* true *)
  MeBi 
    (* Dump "test_09" *)
    Check 
      Bisim LTS Of (tpar TheAction1 TheAction2 tend) With termLTS
        And LTS Of (tpar TheAction2 TheAction1 tend) With termLTS
        Using termLTS.

  (* true *)
  MeBi 
    (* Dump "test_10" *)
    Check 
      Bisim LTS Of (tpar TheAction2 TheAction1 tend) With termLTS
        And LTS Of (tpar TheAction1 TheAction2 tend) With termLTS
        Using termLTS.

  (* false *)
  MeBi 
    (* Dump "test_11" *)
    Check 
      Bisim LTS Of (tpar TheAction1 TheAction1 tend) With termLTS
        And LTS Of (tact TheAction1 tend) With termLTS
        Using termLTS.

  (* false *)
  MeBi 
    (* Dump "test_12" *)
    Check 
      Bisim LTS Of (tpar TheAction1 TheAction2 tend) With termLTS
        And LTS Of (tact TheAction1 (tact TheAction2 tend)) With termLTS
        Using termLTS.

  (* true *)
  MeBi 
    (* Dump "test_13" *)
    Check 
      Bisim LTS Of (tfix (tact TheAction1 trec)) With termLTS
        And LTS Of (tfix (tact TheAction1 trec)) With termLTS
        Using termLTS.

  (* true *)
  MeBi 
    (* Dump "test_14" *)
    Check 
      Bisim LTS Of (tfix (tact TheAction1 trec)) With termLTS
        And LTS Of (tfix (tact TheAction1 (tfix (tact TheAction1 trec)))) With termLTS
        Using termLTS.

  (* true *)
  MeBi 
    (* Dump "test_15" *)
    Check 
      Bisim LTS Of (tfix (tact TheAction1 (tact TheAction2 trec))) With termLTS
        And LTS Of (tact TheAction1 (tact TheAction2 (tfix (tact TheAction1 (tact TheAction2 trec))))) With termLTS
        Using termLTS.

  (* true *)
  MeBi 
    (* Dump "test_16" *)
    Check 
      Bisim LTS Of (tfix (tact TheAction1 (tact TheAction2 trec))) With termLTS
        And LTS Of (tact TheAction1 (tact TheAction2 (tfix (tact TheAction1 (tact TheAction2 trec))))) With termLTS
        Using termLTS. 

  (* minimize *)
  (* MeBi Show Minim
    Of (tact TheAction1 (tact TheAction2 (tfix (tact TheAction1 (tact TheAction2 trec)))))
    Using termLTS. *)

End BisimTest1.

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
