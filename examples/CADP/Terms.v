Require Import MEBI.loader.
Require Stdlib.Program.Tactics.

From Corelib Require Import Relations.Relation_Definitions.
From Stdlib Require Import Relations.Relation_Operators.
From Stdlib Require Operators_Properties.

Require Import MEBI.Examples.CADP.Model.

MeBi Divider "Examples.CADP.Terms".

(* One process, pid 0. *)
Example p1 : process := process_create 0 Protocol.P.
Example c1 : composition := composition_create 0 Protocol.P.
Example c1b : composition := (PAR (PRC (worker_create 0 OK)) (PRC (worker_create 1 Protocol.P)), resource_create 1).

(* Two processes, pids 1 and 0. *)
Example c2 : composition := composition_create 1 Protocol.P.

(* Three processes, pids 2, 1 and 0. *)
Example c3 : composition := composition_create 2 Protocol.P.
