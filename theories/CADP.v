Require Import MEBI.loader.

Require Export String.
Require Import PeanoNat.
Require Import Notations.
Require Export Bool.
Require Import List.
Import ListNotations.

Fixpoint app {X:Type} (l1 l2 : list X) : list X :=
match l1 with
| nil    => l2
| h :: t => h :: (app t l2)
end.


(*************************************************************************)
(**** Basic defs *********************************************************)
(*************************************************************************)


(* pid is a non-nil index *)
Definition pid   : Type  := nat.
Definition index : Type  := option pid.
Definition Nil   : index := None.

(* loop identifier *)
Definition iloop : Type := nat.

(* we dont need new_i, only i and j *)
Definition lock : Type := index * index.

Module Lock.
  Definition initial : lock := (None, None).
End Lock.


Module Index.

  Definition initial : index := Nil.

  Definition of_pid (i : pid) : index := Some i.

  Definition eqb (i:index) (j:index) : bool :=
    match i, j with
    | None, None => true
    | Some n, Some m => Nat.eqb n m
    | _, _ => false
    end.

End Index.




(*************************************************************************)
(**** Memory *************************************************************)
(*************************************************************************)


(**********************************)
(**** Qnode ***********************)
(**********************************)

Record qnode :=
  { next   : index
  ; locked : bool }.

Module Qnode.
  Definition initial : qnode := Build_qnode Nil false.

  Definition get_next   (q:qnode) : index := next q.
  Definition get_locked (q:qnode) : bool  := locked q.

  Definition set_next   (i:index) (q:qnode) : qnode := Build_qnode
                                                        i (Qnode.get_locked q).
  Definition set_locked (b:bool)  (q:qnode) : qnode := Build_qnode
                                                        (Qnode.get_next q) b.
End Qnode.


(**********************************)
(**** mem (Memory) Type ***********)
(**********************************)

Definition mem : Type := list qnode.

Fixpoint mem_create (n:nat) : mem :=
  match n with
  | 0    => []
  | S n' => app (mem_create n') [Qnode.initial]
  end.

Module Memory.
  Definition initial  (n:nat) : mem := mem_create n.

  Definition nth_error (i:nat) (m:mem) : option qnode := nth_error m i.

  Definition nth_default (d:qnode) (i:nat) (m:mem) : qnode := nth_default d m i.

  Fixpoint nth_replace (r:qnode) (i:nat) (m:mem) : option mem :=
    match i, m with
    | 0, [] => None
    | 0, h :: t => Some (r :: t)
    | S n, [] => None
    | S n, h :: t =>
      match nth_replace r n m with
      | None => None
      | Some t' => Some (h :: t')
      end
    end.

End Memory.



(*************************************************************************)
(**** Vars (Local to process ) *******************************************)
(*************************************************************************)

Record local_vars :=
  { var_predecessor : index (* acquire *)
  ; var_locked      : bool  (* acquire *)
  ; var_next        : index (* release *)
  ; var_swap        : bool  (* release *) }.

Module Vars.
  Definition initial : local_vars := Build_local_vars None false None false.
End Vars.

Inductive local_var : Type :=
  | PREDECESSOR
  | LOCKED
  | NEXT
  | SWAP
  | THE_PID
  .

Definition set_predecessor (i:index) (v:local_vars) : local_vars :=
  Build_local_vars i (var_locked v) (var_next v) (var_swap v).

Definition set_locked (b:bool) (v:local_vars) : local_vars :=
  Build_local_vars (var_predecessor v) b (var_next v) (var_swap v).

Definition set_next (i:index) (v:local_vars) : local_vars :=
  Build_local_vars (var_predecessor v) (var_locked v) i (var_swap v).

Definition set_swap (b:bool) (v:local_vars) : local_vars :=
  Build_local_vars (var_predecessor v) (var_locked v) (var_next v) b.




(*************************************************************************)
(**** Value **************************************************************)
(*************************************************************************)

(* FIXME: David mentioned to use [Set] instead of [Type] here,
but it returned [Universal inconsistency, cannot enforce pid.u0<=Set] *)
Inductive value : Type (*Set*) :=
  | NAT : nat -> value
  | PID : pid -> value
  | INDEX : index -> value
  | BOOL : bool -> value
  | NIL : value
  | GET : local_var -> value
  .


Definition valid_cast_to_nat (v:value) : bool * option nat :=
  match v with
  | NAT n => (true, Some n)
  | PID p => (true, Some p)
  | INDEX i => (true, i)
  | _ => (false, None) (* i.e., BOOL, NIL *)
  end.

(*************************************************************************)
(**** (Global) Resource **************************************************)
(*************************************************************************)
Definition resource : Type := mem * lock.

Module Resource.
  Definition initial (n:nat) : resource := (Memory.initial n, Lock.initial).

  Definition get_mem (r:resource) : mem :=
    match r with | (m, _) => m end.

  Definition get_lock_i (r:resource) : index :=
    match r with | (_, (i, _)) => i end.

  Definition get_lock_j (r:resource) : index :=
    match r with | (_, (_, j)) => j end.

  Definition set_mem (new_m:mem) (r:resource) : resource :=
    match r with | (_, l) => (new_m, l) end.

  Definition set_lock_i (new_i:index) (r:resource) : resource :=
    match r with | (m, (_, j)) => (m, (new_i, j)) end.

  Definition set_lock_j (new_j:index) (r:resource) : resource :=
    match r with | (m, (i, _)) => (m, (i, new_j)) end.
End Resource.


(*************************************************************************)
(**** (Local) State ******************************************************)
(*************************************************************************)
Definition state : Type := pid * local_vars.

Module State.
  Definition create (p:pid) : state := (p, Vars.initial).
  Definition initial : state := State.create 0.

  Definition get_pid (s:state) : pid :=
    match s with | (p, _) => p end.

  Definition get_vars (s:state) : local_vars :=
    match s with | (_, v) => v end.

  Definition get_predecessor (s:state) : index := var_predecessor (get_vars s).
  Definition get_locked      (s:state) : bool  := var_locked (get_vars s).
  Definition get_next        (s:state) : index := var_next (get_vars s).
  Definition get_swap        (s:state) : bool  := var_swap (get_vars s).

  Definition get_local_var_value (v:local_var) (s:state) : value :=
    match v with
    | PREDECESSOR => INDEX (get_predecessor s)
    | LOCKED      => BOOL  (get_locked s)
    | NEXT        => INDEX (get_next s)
    | SWAP        => BOOL  (get_swap s)
    | THE_PID     => PID   (get_pid s)
    end.

  Definition set_vars (v:local_vars) (s:state) : state :=
    match s with | (p, _) => (p, v) end.

End State.



(*************************************************************************)
(**** (Conditional) Expressions ******************************************)
(*************************************************************************)

Inductive expr : Type :=
  | NOT : expr -> expr
  | EQ : expr -> expr -> expr
  | VAR : local_var -> expr
  | VAL : value -> expr
  | TRU : expr
  | FLS : expr
  .

Module Expr.
  Definition of_bool (b:bool) : expr := if b then TRU else FLS.
End Expr.


(*************************************************************************)
(**** Actions ************************************************************)
(*************************************************************************)

Inductive act : Type :=
  | NCS   : act
  | ENTER : act
  | LEAVE : act

  | READ_NEXT   : act
  | READ_LOCKED : act

  (* below: [expr] as may be value or var *)
  | WRITE_NEXT   : local_var -> value -> act
  | WRITE_LOCKED : local_var -> value -> act

  | FETCH_AND_STORE  : act
  | COMPARE_AND_SWAP : act
  .


(*************************************************************************)
(**** Process ************************************************************)
(*************************************************************************)

Inductive tm : Type :=
  | ERR : tm (* error *)
  | OK  : tm (* no-op *)

  | IF  : expr -> tm -> tm -> tm

  | ACT : act -> tm

  | SEQ : tm -> tm -> tm

  (* loop id -> body -> outer continuation -> ... *)
  | LOOP_OVER : iloop -> tm -> tm -> tm
  | BREAK : iloop -> tm
  .

Declare Custom Entry tm.
Declare Scope tm_scope.
Notation "<{ e }>" := e (e custom tm at level 99): tm_scope.
Notation "( x )" := x (in custom tm, x at level 99): tm_scope.
Notation "x" := x (in custom tm at level 0, x constr at level 0): tm_scope.
Notation "'if' c 'then' t 'else' e" := (IF c t e)
                (in custom tm at level 90, c custom tm at level 80,
                  t custom tm at level 80, e custom tm at level 80): tm_scope.
Local Open Scope tm_scope.


(*************************************************************************)
(**** Evaluate Expressions  **********************************************)
(*************************************************************************)

Fixpoint eval (e:expr) (s:state) : option value :=
  match e with

  | VAL v => Some v

  | TRU => Some (BOOL true)
  | FLS => Some (BOOL false)

  | VAR v => Some (State.get_local_var_value v s)

  | NOT b =>
    match eval b s with
    | Some (BOOL b) => Some (BOOL (negb b))
    | _ => None
    end

  | EQ a b =>
    match eval a s, eval b s with
    | Some (NAT a),
      Some (NAT b)   => Some (BOOL (Nat.eqb a b))

    | Some (INDEX a),
      Some (INDEX b) => Some (BOOL (Index.eqb a b))

    | Some (PID a),
      Some (PID b)   => Some (BOOL (Nat.eqb a b))

    | Some (BOOL a),
      Some (BOOL b)  => Some (BOOL (eqb a b))

    | _, _ => None
    end

  end.

Definition handle_value (v:value) (s:state) : option value :=
  match v with
  | GET n => eval (VAR n) s
  | _ => Some v
  end.

(*************************************************************************)
(**** Recursive Variables ************************************************)
(*************************************************************************)
Record rec_def := { loop_id : iloop
                  ; body    : tm
                  ; cont    : tm }.

Definition rec_defs : Type := list rec_def.

Fixpoint get_rec_def (i:iloop) (d:rec_defs) : option rec_def :=
  match d with
  | [] => None
  | h :: t => if Nat.eqb i (loop_id h) then Some h else get_rec_def i t
  end.

Definition rec_def_to_tm (d:rec_def) : tm :=
  LOOP_OVER (loop_id d) (body d) (cont d).


(*************************************************************************)
(**** Unfolding **********************************************************)
(*************************************************************************)

Fixpoint unfold (new:rec_def) (old:tm) : tm :=
  match old with
  | ERR => ERR
  | IF c t1 t2 => IF c (unfold new t1) (unfold new t2)
  | ACT a => ACT a

  | SEQ l r => SEQ l (unfold new r)

  | LOOP_OVER l b c =>
    if Nat.eqb l (loop_id new)
    then ERR
    else LOOP_OVER l (unfold new b) (unfold new c)

  | BREAK l =>
    if Nat.eqb l (loop_id new)
    then (cont new)
    else BREAK l

  | OK  => OK
  end.


(*************************************************************************)
(**** Environment (local) ************************************************)
(*************************************************************************)

Definition env : Type := state * resource * rec_defs.

Module Env.
  Definition initial (n:nat) : env := (State.initial, Resource.initial n, []).

  Definition get_state (e:env) : state := match e with | (s, _, _) => s end.

  Definition get_resource (e:env) : resource :=
    match e with | (_, r, _) => r end.

  Definition get_rec_defs (e:env) : rec_defs :=
    match e with | (_, _, d) => d end.

  Definition set_state (s:state) (e:env) : env :=
    match e with | (_, r, d) => (s, r, d) end.

  Definition set_resource (r:resource) (e:env) : env :=
    match e with | (s, _, d) => (s, r, d) end.

  Definition set_rec_defs (d:rec_defs) (e:env) : env :=
    match e with | (s, r, _) => (s, r, d) end.

End Env.


Definition do_unfold (l:iloop) (b:tm) (c:tm) (e:env) : option (tm * env) :=
  let d:rec_defs := Env.get_rec_defs e in
  match get_rec_def l d with
  | None => (* [l] is not yet defined (good) *)
    let new:rec_def := Build_rec_def l b c in
    let e:env := Env.set_rec_defs (new :: d) e in
    Some (unfold new (rec_def_to_tm new), e)

  | Some _ => None (* [l] is already defined *)
  end.

Definition do_break (l:iloop) (e:env) : option tm :=
  let d:rec_defs := Env.get_rec_defs e in
  match get_rec_def l d with
  | None => None (* [l] is not defined *)
  | Some r => Some (cont r) (* return cont *)
  end.


(*************************************************************************)
(**** (Local) Semantics of Processes *************************************)
(*************************************************************************)

(** [read_next e] will set var [next] to be the value
    currently of the [qnode.next] corresponding to the
    [pid] of the process in the state. *)
Definition read_next (e:env) : option env :=
  (* get pid *)
  let s:state := Env.get_state e in
  let p:pid := State.get_pid s in
  (* get qnode *)
  let r:resource := Env.get_resource e in
  let m:mem := Resource.get_mem r in
  match Memory.nth_error p m with
  | None => None (* tried to access out of bounds *)
  | Some q => (* get next of qnode *)
    let qnode_next:index := Qnode.get_next q in
    (* get local vars *)
    let v:local_vars := State.get_vars s in
    (* update env *)
    Some (State.set_vars (set_next qnode_next v) s, r, Env.get_rec_defs e)
  end.

(** [read_locked e] will set var [locked] to be
    the value of the [qnode.locked] corresponding
    to the [pid] of the process in the state.
    ! ! ! it requires that [qnode.locked] is not None. *)
Definition read_locked (e:env) : option env :=
  (* get pid *)
  let s:state := Env.get_state e in
  let p:pid := State.get_pid s in
  (* get qnode *)
  let r:resource := Env.get_resource e in
  let m:mem := Resource.get_mem r in
  match Memory.nth_error p m with
  | None => None (* tried to access out of bounds *)
  | Some q => (* get locked of qnode *)
    let qnode_locked:bool := Qnode.get_locked q in
    (* get local vars *)
    let v:local_vars := State.get_vars s in
    (* update env *)
    Some (State.set_vars (set_locked qnode_locked v) s, r, Env.get_rec_defs e)
  end.

(*DC: slight simplification: instead of taking an expr, it needs to take a val*)
(** [write_next to_write next e] will set the
    field [qnode.next] of the [qnode] of the
    index [to_write] to the value of [next]. *)
Definition write_next (to_write:local_var) (next:value) (e:env) : option env :=
  let s:state := Env.get_state e in
  (* get value of [next] *)
  match handle_value next s with
  | None => None (* [next=GET v] and [v] could not be resolved *)
  | Some next =>
    (* get pid from [to_write] *)
    match State.get_local_var_value to_write s with
    | PID p => (* should be pid *)
      (* get qnode *)
      let r:resource := Env.get_resource e in
      let m:mem := Resource.get_mem r in
      match Memory.nth_error p m with
      | None => None (* [qnode] must exist *)
      | Some q =>
        (* check [next] is some kind of nat *)
        match valid_cast_to_nat next with
        | (false, _) => None (* i.e., BOOL, NIL *)
        | (true, None) => None (* i.e., INDEX NONE *)
        | (true, Some n) =>
          match Memory.nth_replace (Qnode.set_next (Some n) q) p m with
          | None => None
          | Some m => Some (s, Resource.set_mem m r, Env.get_rec_defs e)
          end
        end
      end
    | _ => None (* [to_write] must be a [pid] *)
    end
  end.

(** [write_locked to_write locked e] will set the
    field [qnode.locked] of the [qnode] of the
    index [to_write] to the value of [locked]. *)
Definition write_locked (to_write:local_var) (locked:value) (e:env) : option env :=
  (* get pid from [to_write] *)
  let s:state := Env.get_state e in
  match State.get_local_var_value to_write s with
  | PID p => (* should be pid *)
    (* get qnode *)
    let r:resource := Env.get_resource e in
    let m:mem := Resource.get_mem r in
    match Memory.nth_error p m with
    | None => None (* [qnode] must exist *)
    | Some q =>
      (* make sure [locked] is a [BOOL] *)
      match locked with
      | BOOL b =>
        match Memory.nth_replace (Qnode.set_locked b q) p m with
        | None => None
        | Some m => Some (s, (Resource.set_mem m r), Env.get_rec_defs e)
        end
      | _ => None (* [locked] must be a [BOOL] *)
      end
    end
  | _ => None (* [to_write] must be a [pid] *)
  end.

Definition fetch_and_store (e:env) : option env :=
  (* get [i] *)
  let r:resource := Env.get_resource e in
  let i:index := Resource.get_lock_i r in
  (* store [i] in [predecessor] *)
  let s:state := Env.get_state e in
  let v:local_vars := State.get_vars s in
  let v:local_vars := set_predecessor i v in
  let s:state := State.set_vars v s in
  (* set [i] to [pid] *)
  let p:pid := State.get_pid s in
  let r:resource := Resource.set_lock_i (Index.of_pid p) r in
  (* update env *)
  Some (s, r, Env.get_rec_defs e).


Definition compare_and_swap (e:env) : option env :=
  (* set [j] to [pid] *)
  let s:state := Env.get_state e in
  let p:pid := State.get_pid s in
  let r:resource := Env.get_resource e in
  let r:resource := Resource.set_lock_j (Index.of_pid p) r in
  (* get [i] and [j] *)
  let i:index := Resource.get_lock_i r in
  let j:index := Resource.get_lock_j r in
  (* if the same, then set [swap] to true and set [i] to [Nil], else false *)
  let v:local_vars := State.get_vars s in
  match Index.eqb i j with
  | true =>
    (* set [swap] true *)
    let v:local_vars := set_swap true v in
    let s:state := State.set_vars v s in
    (* set [i] to [Nil] *)
    let r:resource := Resource.set_lock_i Nil r in
    (* update env *)
    Some (s, r, Env.get_rec_defs e)
  | false =>
    (* set [swap] false *)
    let v:local_vars := set_swap false v in
    let s:state := State.set_vars v s in
    (* update env *)
    Some (s, r, Env.get_rec_defs e)
  end.


Definition do_act (a:act) (e:env) : (tm * env) :=
  match a with
  | NCS   => (OK, e)
  | ENTER => (OK, e)
  | LEAVE => (OK, e)

  | _ =>
    let res : option env :=
      match a with
      | READ_NEXT   => read_next e
      | READ_LOCKED => read_locked e

      | WRITE_NEXT to_write next   => write_next to_write next e
      | WRITE_LOCKED to_write next => write_locked to_write next e

      | FETCH_AND_STORE => fetch_and_store e
      | COMPARE_AND_SWAP => compare_and_swap e

      | _ => None (* shouldn't happen, outer match catches these*)
      end
    in
    match res with
    | None => (ERR, e)
    | Some e => (OK, e)
    end
  end.

Definition named_action : Type := act * pid.

Inductive action : Type :=
  | SILENT : action
  | LABEL  : named_action -> action.

Definition visible_action (a:act) (e:env) : action :=
  LABEL (a, (State.get_pid (Env.get_state e))).

Definition get_action (a:act) (e:env) : action :=
  match a with
  (* | NCS   => visible_action a e *)
  | NCS   => SILENT
  | ENTER => visible_action a e
  | LEAVE => visible_action a e
  | _ => SILENT
  end.

Reserved Notation "t '--<{' a '}>-->' t'" (at level 40).

Inductive step : (tm * env) -> action -> (tm * env) -> Prop :=

  | STEP_ACT : forall a e, (ACT a, e) --<{get_action a e}>--> (do_act a e)

  | STEP_SEQ_END : forall r e, (SEQ OK r, e) --<{SILENT}>--> (r, e)

  | STEP_SEQ : forall a l1 l2 r e1 e2,
    (l1, e1) --<{a}>--> (l2, e2) ->
    (SEQ l1 r, e1) --<{a}>--> (SEQ l2 r, e2)

  | STEP_IF_TRU : forall c t1 t2 e,
    eval c (Env.get_state e) = Some (BOOL true) ->
    (<{ if c then t1 else t2 }>, e) --<{SILENT}>--> (t1, e)

  | STEP_IF_FLS : forall c t1 t2 e,
    eval c (Env.get_state e) = Some (BOOL false) ->
    (<{ if c then t1 else t2 }>, e) --<{SILENT}>--> (t2, e)

  | STEP_LOOP_OVER : forall l b c t e1 e2,
    do_unfold l b c e1 = Some (t, e2) -> (* unfolded t, stores def in [e2] *)
    (LOOP_OVER l b c, e1) --<{SILENT}>--> (t, e2)

  (* UNUSED: would cause [BREAK l] to act like [REC_CALL l] *)
  (* | STEP_BREAK : forall l e t,
    do_break l e = Some t ->
    (BREAK l, e) --<{SILENT}>--> (t, e) *)

  where "t '--<{' a '}>-->' t'" := (step t a t').





Inductive sys : Type :=
  | PRC : tm -> state -> rec_defs -> sys
  | PAR : sys -> sys -> sys
  | TERM : sys
  .

Reserved Notation "t '==<{' a '}>==>' t'" (at level 40).

Inductive lts : sys * resource -> action -> sys * resource -> Prop :=

  | LTS_PRC : forall a t1 t2 s1 s2 d1 d2 r1 r2,
    (t1, (s1, r1, d1)) --<{a}>--> (t2, (s2, r2, d2)) ->
    (PRC t1 s1 d1, r1) ==<{a}>==> (PRC t2 s2 d2, r2)

  | LTS_PAR_L : forall a l1 l2 r gr1 gr2,
    (l1, gr1) ==<{a}>==> (l2, gr2) ->
    (PAR l1 r, gr1) ==<{a}>==> (PAR l2 r, gr2)

  | LTS_PAR_R : forall a l r1 r2 gr1 gr2,
    (r1, gr1) ==<{a}>==> (r2, gr2) ->
    (PAR l r1, gr1) ==<{a}>==> (PAR l r2, gr2)

  | LTS_OK_L : forall s d r g, (PAR (PRC OK s d) r, g) ==<{SILENT}>==> (r, g)

  | LTS_OK_R : forall l s d g, (PAR l (PRC OK s d), g) ==<{SILENT}>==> (l, g)

  where "t '==<{' a '}>==>' t'" := (lts t a t')
  and "t '--<{' a '}>-->' t'" := (step t a t').



(*************************************************************************)
(**** Example ************************************************************)
(*************************************************************************)

Import Expr.

Example Acquire : tm :=
  SEQ (ACT (WRITE_NEXT THE_PID NIL)) (
    SEQ (ACT FETCH_AND_STORE) (
      IF (NOT (EQ (VAR PREDECESSOR) (VAL NIL))) (
        SEQ (ACT (WRITE_LOCKED PREDECESSOR (BOOL true))) (
          SEQ (ACT (WRITE_NEXT PREDECESSOR (GET THE_PID))) (
            LOOP_OVER 2 (*L*) (
              SEQ (ACT READ_LOCKED) (
                IF (NOT (VAR LOCKED)) (BREAK 2 (*L*)) (OK)
              )
            ) (OK)
          )
        )
      ) (OK)
    )
  ).


Example Release : tm :=
  SEQ (ACT READ_NEXT) (
    IF (EQ (VAL NIL) (VAR NEXT)) (
      SEQ (ACT COMPARE_AND_SWAP) (
        IF (EQ FLS (VAR SWAP)) (
          LOOP_OVER 1 (*L*) (
            SEQ (ACT READ_NEXT) (
              IF (NOT (EQ (VAL NIL) (VAR NEXT))) (BREAK 1 (*L*)) (OK)
            )
          ) (ACT (WRITE_LOCKED NEXT (BOOL true)))
        ) (OK)
      )
    ) (ACT (WRITE_LOCKED NEXT (BOOL false)))
  ).



Example P : tm :=
  LOOP_OVER 0 (
    SEQ (ACT NCS) (
      SEQ Acquire (
        SEQ (ACT ENTER) (
          SEQ (ACT LEAVE) (
            SEQ Release OK
          )
        )
      )
    )
  ) (OK).


(**********************************)
(**** Single process **************)
(**********************************)

(* Example p0 : tm * env := (P, Env.initial 0). *)
Example p0 : tm * env := (P, (State.initial, Resource.initial 0, [])).
(* MeBi LTS step p0. *) (* FIXME: calling mebi appears to cause exception *)



(**********************************)
(**** System **********************)
(**********************************)

Definition process : Type := tm * state * rec_defs.

Definition spawn (p:pid) (b:tm) : process := (b, State.create p, []).

Fixpoint populate (n:nat) (b:tm) : list process :=
  match n with
  | 0 => []
  | S m => app (populate m b) [spawn n b]
  end.

Definition system : Type := list process * resource.

Definition create (n:nat) (b:tm) : system := (populate n b, Resource.initial n).

Fixpoint load (ps:list process) : sys :=
  match ps with
  | [] => TERM
  | h :: t =>
    match h with
    | (p, s, d) => PAR (PRC p s d) (load t)
    end
  end.

Definition composition : Type := sys * resource.
Definition compose (s:system) : composition :=
  match s with
  | (ps, r) => (load ps, r)
  end.

Example ncs : composition := compose (create 5 P).
MeBi LTS lts ncs.


(*************************************************************************)
(**** TODO: LTS equiv temp. logic prop. **********************************)
(*************************************************************************)


(* Module NamedAction.
  Definition to_action (v:named_action) : action :=
    match v with
    | (a, p) => LABEL (a, p)
    end.
End NamedAction. *)


(* Definition trace : Type := list named_action. *)

(* Module Trace.
  (** [prune t] removes all [SILENT] actions from [t]. *)
  Fixpoint prune (t:list action) : trace :=
    match t with
    | [] => []
    | SILENT :: t => (prune t)
    | LABEL h :: t => h :: (prune t)
    end.

  Definition of_action_list (l:list action) : trace := prune l.
End Trace.

Example trace_1 : trace := [(ENTER, 0); (LEAVE, 0)]. *)


(* ! remember: just encode the LTL examples into an LTS.
  - that means two separate LTS, one of each.
  - each action in the trace is used to navigate the LTS.
    i.e., if a transition is not possible, then the property does not hold *)

Definition trace : Type := list action.

Definition prc_trace : Type := list named_action.

Definition sys_trace : Type := list (pid * prc_trace).

Definition named_action_of (a:action) : option named_action :=
  match a with
  | SILENT => None
  | LABEL n => Some n
  end.

(* Definition named_action_eq (a:named_action) (b:named_action) : bool :=
  match a, b with
  | (ENTER, _), (ENTER, _) => true
  | (LEAVE, _), (LEAVE, _) => true
  | _, _ => false
  end. *)

Definition is_named_action_enter (a:named_action) : bool :=
  match a with
  | (ENTER, _) => true
  | _ => false
  end.

Definition is_named_action_leave (a:named_action) : bool :=
  match a with
  | (LEAVE, _) => true
  | _ => false
  end.

Fixpoint get_prc_trace (p:pid) (s:sys_trace) : option prc_trace :=
  match s with
  | [] => None
  | (q, l) :: t => if Nat.eqb p q then Some l else get_prc_trace p t
  end.

Fixpoint get_other_traces (p:pid) (s:sys_trace) : sys_trace :=
  match s with
  | [] => []
  | (q, l) :: t =>
    (if Nat.eqb p q then [] else [(q, l)]) ++ get_other_traces p t
  end.



(* Definition automata : Type := sys_trace. *)
(* Inductive  *)

Reserved Notation "t '-{' a '}->' t'" (at level 40).

(** [mutual_exclusion]
    [ true*.
      { ENTER ?i:nat }.
      (not { LEAVE !i })*.
      { ENTER ?j:nat where j<>i }
    ] false *)

Module MutualExclusion.
  Inductive lts : sys_trace -> action -> sys_trace -> Prop :=
    (* | SILENT : forall  *)

    | ENTER : forall s a p h t,
      (* sanity check: [h] not already ENTER *)
      get_prc_trace p s = Some (h :: t)   /\ negb (is_named_action_enter a) /\
      named_action_of a = Some (ENTER, p) /\
      get_other_traces p s                ->
      s -{LABEL (ENTER, p)}-> s


    where "t '-{' a '}->' t'" := (lts t a t').
End MutualExclusion.



(** [no_starvation]
    [ true* ] forall i:nat among {1..N}.
    [ for j:nat from 1 to N do
        (not { ENTER ...!i })*.
        { ?G:string ...!j where (j=i) -> (G<>"ENTER") }
      end for
    ]-| *)

Module NoStarvation.
  Inductive lts : automata -> action -> automata -> Prop :=
    | SILENT :
    .
End NoStarvation.


(*
Module LTL.

  Inductive prop_const  : Type :=
    | VAL : string -> prop_const (* !i *)
    | VAR : string -> Type -> prop_const (* ?j:nat *)

    | WHERE : prop_const -> prop_const
    .

  Definition prop_consts : Type := list prop_const.

  Inductive formula : Type :=

    (* logical constants *)
    | TRUE | FALSE

    (* propositional constants *)
    (* | PROP  : prop_const -> formula *)

    | ENTER : prop_const -> formula
    | LEAVE : prop_const -> formula
    | NCS   : prop_const -> formula
    | LABEL : prop_const -> prop_const -> formula

    | FORALL : prop_const -> nat -> formula
    | EXISTS : prop_const -> nat -> formula

    | FOR : prop_const -> nat -> nat -> formula

    (* operators *)
    | NOT      : formula -> formula

    | CONCAT   : formula -> formula -> formula

    | AND      : formula -> formula -> formula
    | OR       : formula -> formula -> formula
    | IMPLIES  : formula -> formula -> formula
    | BIMPLIES : formula -> formula -> formula

    | EQ       : formula -> formula -> formula

    (* symbols *)
    | NEXTTIME   : formula -> formula -> formula (* (F) G *)
    | HENCEFORTH : formula -> formula -> formula (* [F] G *)
    | EVENTUALLY : formula -> formula -> formula (* <F> G *)

    (* saturation operator, forbids infinite repetition *)
    | SATURATION : fomrula -> formula (* [F]-| *)
    .

Definition mutual_exclusion : formula :=
  HENCEFORTH (
    CONCAT (ENTER (VAR "i" nat)) (
      CONCAT (NOT (LEAVE (VAL "i"))) (
        ENTER (
          WHERE (VAR "j" nat)
          (NOT (EQ "j" "j"))
        )
      )
    )
  ) FALSE.


(** [no_starvation]
    [ true* ] forall i:nat among {1..N}.
    [ for j:nat from 1 to N do
        (not { ENTER ...!i })*.
        { ?G:string ...!j where (j=i) -> (G<>"ENTER") }
      end for
    ]-| *)
Definition no_starvation (n:nat) : formula :=
  HENCEFORTH ( TRUE ) (
    CONCAT (FORALL (VAR "i" nat) n) (
      SATURATION (FOR (VAR "j" nat) 1 n (
        CONCAT (NOT (ENTER (VAL "i"))) (
          LABEL (VAR "g" string) (VAL "j")
        )
      ))
    )
  ).


  (* Fixpoint eval (f:formula) : option bool :=
    match f with
    | TRUE => Some true
    | FALSE => Some false

    | NOT g =>
      match eval g with
      | None => None
      | Some g => Some (negb g)
      end

    | AND a b =>
      match eval a, eval b with
      | Some a, Some b => Some (andb a b)
      | _, _ => None
      end

    | OR a b =>
      match eval a, eval b with
      | Some a, Some b => Some (orb a b)
      | _, _ => None
      end

    | IMPLIES a b =>
      match eval a, eval b with
      | Some false, Some false => Some true
      | Some false, Some true  => Some true
      | Some true,  Some false => Some false
      | Some true,  Some true  => Some true
      | _, _ => None
      end

    | BIMPLIES a b =>
      match eval a, eval b with
      | Some true,  Some true  => Some true
      | Some false, Some false => Some true
      | Some _,     Some _     => Some false
      | _, _ => None
      end

    end. *)



End LTL.
 *)
