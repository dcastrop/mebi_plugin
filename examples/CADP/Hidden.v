(* Hiding joining the queue (2026-10-11), with [MEBI.Hiding].

   [Obs.bigstep_obs] is [Glued.bigstep] with one silent step made
   visible: [DO_ACQUIRE], the tail swap, labelled [(FETCH_AND_STORE, pid)].
   [bigstep_hides] proves the converse: [bigstep] is [bigstep_obs] with that
   label hidden ([hide_join]). Likewise [spec_fifo_hidden] is the FIFO lock
   server ([SpecFifo.spec_fifo]) with joining hidden ([spec_fifo_hides]).

   Hiding preserves weak bisimilarity ([MEBI.Hiding.relabel_bisimilar]), so
   FIFO order proved for the observing semantics gives, for plain
   [bigstep], [weak_bisimilar bigstep spec_fifo_hidden] ([wbis_hidden]).
   [Fifo.v] goes on to mutual exclusion from there:
   [spec_fifo_hidden] only ever lets one process in, which the plugin proves
   against [spec_lts]. *)

From Stdlib Require Import List.
Import ListNotations.
Require Import MEBI.Bisimilarity.
Require Import MEBI.Hiding.
Require Import MEBI.Examples.CADP.Model.
Require Import MEBI.Examples.CADP.Glued.
Require Import MEBI.Examples.CADP.Obs.
Require Import MEBI.Examples.CADP.SpecFifo.

Import Protocol.

(* [hide_join a] is [a] with joining the queue, [(FETCH_AND_STORE, _)], made
   silent. *)
Definition hide_join (a : option label) : option label :=
  match a with
  | Some (FETCH_AND_STORE, _) => None
  | _ => a
  end.

Lemma bigstep_hides : relabelled hide_join bigstep_obs bigstep.
Proof.
  intros c a d; split.
  - intros T; induction T.
    + exists None; split; [constructor | reflexivity].
    + exists (Some (FETCH_AND_STORE, get_pid s)); split; [constructor | reflexivity].
    + exists None; split; [constructor | reflexivity].
    + exists None; split; [constructor | reflexivity].
    + exists None; split; [constructor | reflexivity].
    + eexists; split; [constructor | reflexivity].
    + eexists; split; [constructor | reflexivity].
    + destruct IHT as [b [Tb ->]]; exists b; split; [apply OBS_PAR_L; exact Tb | reflexivity].
    + destruct IHT as [b [Tb ->]]; exists b; split; [apply OBS_PAR_R; exact Tb | reflexivity].
  - intros [b [T ->]]; induction T; simpl; try constructor; assumption.
Qed.

(* The FIFO lock server with joining hidden: [spec_fifo] rule for rule,
   [REQ] silent. *)
Inductive spec_fifo_hidden : fifo_state -> option label -> fifo_state -> Prop :=
| FIFOH_REQ : forall i idle q b,
  spec_fifo_hidden (FS (i :: idle) q b) None (FS idle (q ++ [i]) b)
| FIFOH_REQ_SKIP : forall j idle idle' q q' b b',
  spec_fifo_hidden (FS idle q b) None (FS idle' q' b') ->
  spec_fifo_hidden (FS (j :: idle) q b) None (FS (j :: idle') q' b')
| FIFOH_ENTER : forall i idle q,
  spec_fifo_hidden (FS idle (i :: q) false) (Some (ENTER, i)) (FS idle (i :: q) true)
| FIFOH_LEAVE : forall i idle q,
  spec_fifo_hidden (FS idle (i :: q) true) (Some (LEAVE, i)) (FS (ins i idle) q false).

Lemma spec_fifo_hides : relabelled hide_join spec_fifo spec_fifo_hidden.
Proof.
  intros c a d; split.
  - intros T; induction T.
    + exists (Some (FETCH_AND_STORE, i)); split; [constructor | reflexivity].
    + destruct IHT as [[[act p]|] [Tb Eb]].
      * destruct act; try discriminate.
        exists (Some (FETCH_AND_STORE, p)); split; [apply FIFO_REQ_SKIP; exact Tb | reflexivity].
      * inversion Tb.
    + exists (Some (ENTER, i)); split; [constructor | reflexivity].
    + exists (Some (LEAVE, i)); split; [constructor | reflexivity].
  - intros [b [T ->]]; induction T; simpl; try constructor; assumption.
Qed.

(* FIFO order for the observing semantics gives it for plain [bigstep],
   joining hidden on both sides. *)
Theorem wbis_hidden : forall c f,
  weak_bisimilar bigstep_obs spec_fifo c f -> weak_bisimilar bigstep spec_fifo_hidden c f.
Proof.
  exact (relabel_bisimilar hide_join bigstep_obs bigstep spec_fifo spec_fifo_hidden
           eq_refl bigstep_hides spec_fifo_hides).
Qed.
