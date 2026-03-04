Require Import MEBI.loader.
Require Stdlib.Program.Tactics.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Examples.CADP.
Import Protocol. 

Require Import MEBI.Examples.CADP_Glued.

MeBi Divider "Examples.Bisimilarity.CADP.Size1.Terms".

Example p1 : tm * env := (P, Env.initial 1).

Example c1 : composition := compose (create 1 P).
