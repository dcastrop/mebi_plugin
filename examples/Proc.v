Require Import MEBI.loader.

Inductive action : Set := | ASend | ARecv | BSend | BRecv.

Inductive term : Set :=
| trec : term
| tend : term
| tfix : term -> term
| tact : action -> term -> term
| tpar : term -> term -> term (* parallel *)
| topt : term -> term -> term (* choice between options *)
.

Fixpoint tsubst (t1 : term) (t2 : term) :=
  match t2 with
  | trec => t1
  | tend => tend
  | tfix t => tfix t
  | tact a t => tact a (tsubst t1 t)
  | tpar tl tr => tpar (tsubst t1 tl) (tsubst t1 tr)
  | topt tl tr => topt (tsubst t1 tl) (tsubst t1 tr)
  end.

(* true: "comm", false: "silent" *)
Inductive termLTS : term -> bool -> term -> Prop :=
(* syncrhonous communication, i.e., send/recv in parallel *)
| do_senda : forall tl tr, termLTS (tpar (tact ASend tl) (tact ARecv tr))
                             true  (tpar tl tr)

| do_sendb : forall tl tr, termLTS (tpar (tact BSend tl) (tact BRecv tr))
                             true  (tpar tl tr)

(* non-deterministic choice between tl and tr *)
| do_optl : forall a tl tr tl', termLTS tl a tl' ->
                                termLTS (topt tl tr) a tl'

| do_optr : forall a tl tr tr', termLTS tr a tr' ->
                                termLTS (topt tl tr) a tr'

(* non-deterministic parallel step *)
| do_parl : forall a tl tl' tr, termLTS tl a tl' ->
                                termLTS (tpar tl tr) a (tpar tl' tr)

| do_parr : forall a tl tr tr', termLTS tr a tr' ->
                                termLTS (tpar tl tr) a (tpar tl tr')

(* These below capture "structural congruence": using "silent" transitions *)
| do_fix : forall t,
    termLTS (tfix t) false (tsubst (tfix t) t)

| do_comm : forall tl tr,
    termLTS (tpar tl tr) false (tpar tr tl)

| do_assocl : forall t1 t2 t3,
    termLTS (tpar t1 (tpar t2 t3)) false (tpar (tpar t1 t2) t3)

| do_assocr : forall t1 t2 t3,
    termLTS (tpar (tpar t1 t2) t3) false (tpar t1 (tpar t2 t3))
.

(*************************************)
(** Basic: No recursion or branches **)
(*************************************)

Example proc0_send0 := tact ASend tend. (* empty lts *)
MeBi Show LTS Bounded 150 Of proc0_send0 Using termLTS.

Example proc0_send1 := tpar (tact ASend tend)
                            (tact ARecv tend).
MeBi Debug LTS Bounded 150 Of proc0_send1 Using termLTS.
(* MeBi Dump "proc0_send1" LTS Bounded 150 Of proc0_send1 Using termLTS. *)

Example proc0_send2 := tpar (tact ASend (tact ASend tend))
                            (tact ARecv (tact ARecv tend)).
(* MeBi Show LTS Bounded 150 Of proc0_send2 Using termLTS. *)

Example proc0_send3 := tpar (tact ASend (tact BSend tend))
                            (tact ARecv (tact BRecv tend)).
(* MeBi Show LTS Bounded 150 Of proc0_send3 Using termLTS. *)

Example proc0_send4 := tpar (tact BSend (tact ASend tend))
                            (tact BRecv (tact ARecv tend)).
(* MeBi Show LTS Bounded 150 Of proc0_send4 Using termLTS. *)


(*

(*************************************)
(** Simple Recursion, no Branching ***)
(*************************************)

Example proc1_rec1 := tfix trec.
(* MeBi Show LTS Bounded 150 Of proc1_rec1 Using termLTS. *)

Example proc1_rec2 := tpar (tfix (tact ASend trec))
                           (tfix (tact ARecv trec)).
(* MeBi Show LTS Bounded 150 Of proc1_rec2 Using termLTS. *)

Example proc1_rec3 := tpar (tfix (tact ASend (tact BSend trec)))
                           (tfix (tact ARecv (tact BRecv trec))).
(* MeBi Show LTS Bounded 150 Of proc1_rec3 Using termLTS. *)

Example proc1_rec4 := tpar (tact ASend (tfix (tact BSend trec)))
                           (tact ARecv (tfix (tact BRecv trec))).
(* MeBi Show LTS Bounded 150 Of proc1_rec4 Using termLTS. *)


(*************************************)
(** Branching, no Recursion **********)
(*************************************)


(*************************************)
(** Simple Branching and Recursion ***)
(*************************************)


(*************************************)
(** Nested Branching and Recursion ***)
(*************************************)




Example proc1 := tpar (tfix (tact ASend trec)) (tfix (tact ARecv trec)).
MeBi Show LTS Bounded 150 Of proc1 Using termLTS.
(* MeBi Dump "proc1" LTS Bounded 1000 Of proc1 Using termLTS. *)

Example proc2 := tpar proc1 proc1.
(* MeBi Show LTS Bounded 150 Of proc2 Using termLTS. *)
MeBi Dump "proc2" LTS Bounded 20000 Of proc2 Using termLTS.


(* TODO: would be cool to do first a "FSM minimisation". I believe there are
 many bisimilar states here, where the order (or assoc) of the processes is
 completely irrelevant *)
(* MeBi Show LTS Bounded 150 Of proc1 Using termLTS. *)
(* MeBi Dump "proc0" FSM Bounded 150 Of proc1 Using termLTS. *)


Inductive comp : Set :=
| cterm : term -> comp
| cpar : comp -> comp -> comp
.

(* second layer *)
Inductive compLTS : comp -> bool -> comp -> Prop :=
| do_t : forall a t t', termLTS t a t' -> compLTS (cterm t) a (cterm t')

| do_l : forall a l l' r, compLTS l a l' -> compLTS (cpar l r) a (cpar l' r)
| do_r : forall a l r r', compLTS r a r' -> compLTS (cpar l r) a (cpar l r')

| do_l_end : forall a r, compLTS (cpar (cterm tend) r) a r
| do_r_end : forall a l, compLTS (cpar l (cterm tend)) a l
.

Example comp0 := cpar (cterm proc0) (cterm proc0).
(* MeBi Show LTS Bounded 150 Of comp0 Using termLTS compLTS. *)
(* MeBi Dump "comp0" LTS Bounded 350 Of comp0 Using termLTS compLTS. *)

Example comp1a := cpar (cterm proc1) (cterm tend).
(* MeBi Show LTS Bounded 150 Of comp1a Using termLTS compLTS. *)
(* MeBi Dump "comp1a" LTS Bounded 350 Of comp1a Using termLTS compLTS. *)

Example comp1b := cpar (cterm proc1) (cterm proc1).
(* MeBi Show LTS Bounded 150 Of comp1b Using termLTS compLTS. *)
(* MeBi Dump "comp1b" LTS Bounded 5000 Of comp1b Using termLTS compLTS. *)


*)