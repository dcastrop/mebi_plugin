Require Import MEBI.loader.

Require Export String.
Require Import PeanoNat.
Require Import Notations.
Require Export Bool.
Require Import List.
Import ListNotations.

Require Import MEBI.Examples.CADP.

(** [mutual_exclusion]
  [ true*.
    { ENTER ?i:nat }.
    (not { LEAVE !i })*.
    { ENTER ?j:nat where j<>i }
  ] false
*)
Module MutualExclusion.

  (** [can_enter p a s] checks that the last act of [p] is LEAVE or nothing. *)
  Definition can_enter (p:pid) (s:sys_trace) : option bool :=
    match can_do_act p ENTER s with
    | None => None
    | Some false => Some false
    | Some true => Some (negb (exist_last_act ENTER (get_other_traces p s)))
    end.

  (** [do_enter p s] returns [Some s] updated with [p] last act ENTER, if [can_enter] is true. *)
  Definition do_enter (p:pid) (s:sys_trace) : option sys_trace :=
    match can_enter p s with
    | Some true => append_act_hd p ENTER s
    | _ => None
    end.


  (** [can_leave p a s] checks that the last act of [p] is ENTER. *)
  Definition can_leave (p:pid) (s:sys_trace) : option bool :=
    can_do_act p LEAVE s.

  (** [do_leave p s] returns [Some s] updated with [p] last act LEAVE, if [can_leave] is true. *)
  Definition do_leave (p:pid) (s:sys_trace) : option sys_trace :=
    match can_leave p s with
    | Some true => append_act_hd p LEAVE s
    | _ => None
    end.

  (** [MutualExclusion.lts] is defined so long as when a process [p] does an [ENTER] action, there are no other processes whose last action was [ENTER], rather their last action should either be LEAVE or nothing. *)
  Inductive lts : sys_trace -> action -> sys_trace -> Prop :=

    | ENTER : forall t1 t2 p,
      do_enter p t1 = Some t2 -> lts t1 (LABEL (ENTER, p)) t2

    | LEAVE : forall t1 t2 p,
      do_leave p t1 = Some t2 -> lts t1 (LABEL (LEAVE, p)) t2

    | SILENT : forall t, lts t SILENT t
    .

End MutualExclusion.
