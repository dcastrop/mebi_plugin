Require Import MEBI.loader.
Require Stdlib.Program.Tactics.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Examples.CADP.

MeBi Divider "Examples.Bisimilarity.CADP.Size3.Terms".

(* Three processes, pids 2, 1 and 0. *)
Example c3 : composition := composition_create 2 Protocol.P.
