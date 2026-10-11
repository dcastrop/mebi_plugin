(* A FIFO lock server (2026-10-11): the property LTS for no starvation, as
   designed in [examples/README.md] ("No starvation").

   A state holds the idle pids (sorted), the queue, and whether the queue's
   head is in its critical section:
   - [REQ i] (the label [(FETCH_AND_STORE, i)], the MCS tail swap) moves an
     idle [i] to the back of the queue;
   - [ENTER i] is allowed only for the head, and only once;
   - [LEAVE i] takes the head out of the queue and makes it idle again.
   It is mutually exclusive (only the head enters) and FIFO (the queue is
   served in order) by construction. [REQ] picks its pid by recursing on
   the idle list ([FIFO_REQ_SKIP]), as [spec_lts] does, so the plugin can
   explore it. *)

From Stdlib Require Import List.
Import ListNotations.
Require Import MEBI.Examples.CADP.

(* [ins i l] inserts [i] into the sorted list [l]. *)
Fixpoint ins (i : pid) (l : list pid) : list pid :=
  match l with
  | [] => [i]
  | j :: l' => if Nat.leb i j then i :: l else j :: ins i l'
  end.

Inductive fifo_state : Type :=
| FS : list pid -> list pid -> bool -> fifo_state.

Inductive spec_fifo : fifo_state -> option label -> fifo_state -> Prop :=
| FIFO_REQ : forall i idle q b,
  spec_fifo (FS (i :: idle) q b) (Some (FETCH_AND_STORE, i)) (FS idle (q ++ [i]) b)
| FIFO_REQ_SKIP : forall j i idle idle' q q' b b',
  spec_fifo (FS idle q b) (Some (FETCH_AND_STORE, i)) (FS idle' q' b') ->
  spec_fifo (FS (j :: idle) q b) (Some (FETCH_AND_STORE, i)) (FS (j :: idle') q' b')
| FIFO_ENTER : forall i idle q,
  spec_fifo (FS idle (i :: q) false) (Some (ENTER, i)) (FS idle (i :: q) true)
| FIFO_LEAVE : forall i idle q,
  spec_fifo (FS idle (i :: q) true) (Some (LEAVE, i)) (FS (ins i idle) q false).

(* All of pids [0 .. n] idle, as for [composition_create n]. *)
Definition make_fifo (n : nat) : fifo_state := FS (seq 0 (S n)) [] false.
