Require Import MEBI.loader.

From Stdlib Require Export String.
From Stdlib Require Import PeanoNat.
Require Import Notations.
From Stdlib Require Import Bool.
From Stdlib Require Import Lists.List.
Import ListNotations.

Require Import MEBI.Bisimilarity.
Require Import MEBI.Examples.CADP.
Require Import MEBI.Examples.CADP_Glued.

Fixpoint app {X:Type} (l1 l2 : list X) : list X :=
  match l1 with
  | nil    => l2
  | h :: t => h :: (app t l2)
  end.

(** [no_starvation]
  [ true* ] forall i:nat among {1..N}.
  [ for j:nat from 1 to N do
      (not { ENTER ...!i })*.
      { ?G:string ...!j where (j=i) -> (G<>"ENTER") }
    end for
  ]-|
*)
Module NoStarvation.

  Definition prc_trace : Type := list act.

  Definition sys_trace : Type := list (pid * prc_trace).

  (** [prc_history] is a tuple of: (1) [list pid] to track the processes that have acted since a process last acted, (2) [option bool] denoting if the process starves (i.e., None if not at risk of starving, Some false if it has starved reecently, and Some true if it routinely starves) *)
  Definition prc_history : Type := list pid * option bool.

  Definition reset_prc_history (y:prc_history) : prc_history :=
    match y with
    | (_, None) => ([], None) (* nothing to change *)
    | (_, Some false) => ([], None) (* deescelate if only recently starved *)
    | (_, Some true) => ([], Some true) (* cannot be descelated *)
    end.

  Definition num_prc_acted (y:prc_history) : nat :=
    match y with
    | (l, _) => List.length l
    end.

  Fixpoint update_prc_history_pids (p:pid) (y:list pid) : list pid :=
    match y with
    | [] => [p] (* [p] is not being tracked yet, add *)
    | q :: t =>
      if Nat.eqb p q then y (* [p] already in [y] *)
      else q :: update_prc_history_pids p t
    end.

  (** [sys_history] is a list of tuples, where [pid] is the last to act, [list act] is the list of actions performed, and [prc_history] is ... . *)
  Definition sys_history : Type := list (pid * prc_trace * prc_history).

  Definition num_of_prc (z:sys_history) : nat := List.length z.

  (** [get_sys_trace z] strips the [prc_history] from [z]. *)
  Fixpoint get_sys_trace (z:sys_history) : sys_trace :=
    match z with
    | [] => []
    | (p, l, h) :: t => (p, l) :: get_sys_trace t
    end.

  (** [update_sys_history p a z] updates the trace or history for each process in [z], where if a process [q] is performing action [a] then it is added to their trace and their history is reset. Otherwise, [p] is added to their history of processes that have acted since they last did. *)
  Fixpoint update_sys_history (p:pid) (a:act) (z:sys_history) :
    sys_history :=
      match z with
      | [] => []
      | (q, l, h) :: t =>
        (* if [q] is acting, then reset history *)
        (if Nat.eqb p q then (p, a :: l, reset_prc_history h) else
        (* otherwise, make sure that history of [q] has [p] acting *)
        match h with
        | (y, b) => (q, l, (update_prc_history_pids p y, b))
        end) :: update_sys_history p a t
      end.

  (** [no_starvation z] returns [(true, z')] if no processes in [z] are starving, where [z'] is updated to track how close each process is to starving. Otherwise, [(false, _)] if any process has been starving. *)
  Fixpoint no_starvation (z:sys_history) : bool * sys_history :=
    match z with
    | [] => (true, [])
    | (p, l, h) :: t =>
      match no_starvation t with (* first continue exploring t *)
      | (b, z') =>
        if Nat.leb (S (num_prc_acted h)) (num_of_prc z)
        then (* at least one other process could act before [p] starves *)
          (b, (p, l, h) :: z')
        else (* [p] is starving, but check if this has already happened *)
        match h with
        (* first time starving -- also reset their history *)
        | (_, None) => (b, (p, l, ([], Some false)) :: z')

        (* has starved some time in the past *)
        | (_, Some false) => (b, (p, l, ([], Some true)) :: z')

        (* [p] is starving *)
        | (i, Some true) => (false, (p, l, (i, Some true)) :: z')
        end
      end
    end.

    Definition act_eq (a:act) (b:act) : bool :=
    match a, b with
    | ENTER, ENTER => true
    | LEAVE, LEAVE => true
    | NCS, NCS => true
    | READ_NEXT, READ_NEXT => true
    | READ_LOCKED, READ_LOCKED => true
    | WRITE_NEXT _ _, WRITE_NEXT _ _ => true
    | WRITE_LOCKED _ _, WRITE_LOCKED _ _ => true
    | _, _ => false
    end.

  (** Note: dual only applies to ENTER and LEAVE. *)
  Definition act_get_dual (a:act) : option act :=
    match a with
    | ENTER => Some LEAVE
    | LEAVE => Some ENTER
    | _ => None
    end.

  (** [are_dual a b] returns [Some bool] denoting if [a] and [b] are dual, and returns [None] if either [a] or [b] have no dual. *)
  Definition act_are_dual (a:act) (b:act) : option bool :=
    match act_get_dual a, act_get_dual b with
    | None, _ => None
    | _, None => None
    | Some a, Some _ => Some (act_eq a b)
    end.
    
  Fixpoint get_prc_trace (p:pid) (s:sys_trace) : option prc_trace :=
    match s with
    | [] => None
    | (q, l) :: t => if Nat.eqb p q then Some l else get_prc_trace p t
    end.


  (** [last_act_of p s] returns the last act of [p] (if [p] not in [s], then [None]). *)
  Definition last_act_of (p:pid) (s:sys_trace) : bool * option act :=
    match get_prc_trace p s with
    | None => (false, None)
    | Some [] => (true, None)
    | Some (h :: _) => (true, Some h)
    end.

  Definition can_do_act (p:pid) (a:act) (s:sys_trace) : option bool :=
    match last_act_of p s with
    | (false, _) => None (* [p] not in [s] *)

    | (true, None) => (* [p] in [s], but empty trace*)
      match a with
      | ENTER => Some true
      | LEAVE => Some false
      | _ => None
      end

    | (true, Some b) => act_are_dual a b (* [p] last act [b] in [s]*)
    end.

  (** [do_action a z] checks if the process [p] can do act [a] in [z] and in doing so also updates all of the history/logs of the processes that are/are not acting. If [p] can do [a], it then checks to see if any process is now starving. It returns None if any of the above is not met. *)
  Definition do_action (a:option label) (z:sys_history) : option sys_history :=
    match a with
    | None => Some z (* skip *)
    | Some (a, p) =>
      match can_do_act p a (get_sys_trace z) with
      | None => None
      | Some false => None
      | Some true =>
        match no_starvation z with
        | (true, z) => Some z
        | (false, _) => None
        end
      end
    end.

  (** [NoStarvation.lts] is defined so long as after each action, [no_starvation s] holds. I.e.,  *)
  Inductive lts : sys_history -> option label -> sys_history -> Prop :=
    | ACT : forall t1 t2 a, do_action a t1 = Some t2 -> lts t1 a t2
    .

End NoStarvation.

