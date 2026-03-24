Require Import MEBI.loader.
Require Stdlib.Program.Tactics.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Examples.CADP.
Import Protocol. 

Require Import MEBI.Examples.CADP_Glued.

MeBi Divider "Examples.Bisimilarity.CADP.Size2.Terms".

(* Example p1 : process := process_create 0 Protocol.P. *)

Example c2 : composition := composition_create 1 Protocol.P.