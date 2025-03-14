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
  | VAR_PREDECESSOR
  | VAR_LOCKED
  | VAR_NEXT
  | VAR_SWAP
  | VAR_PID
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
    | VAR_PREDECESSOR => INDEX (get_predecessor s)
    | VAR_LOCKED      => BOOL  (get_locked s)
    | VAR_NEXT        => INDEX (get_next s)
    | VAR_SWAP        => BOOL  (get_swap s)
    | VAR_PID         => PID   (get_pid s)
    end.

  Definition set_vars (v:local_vars) (s:state) : state :=
    match s with | (p, _) => (p, v) end.

End State.


(*************************************************************************)
(**** Environment (local) ************************************************)
(*************************************************************************)
Definition env : Type := state * resource.

Module Env.

  Definition get_state (e:env) : state := match e with | (s, _) => s end.

  Definition get_resource (e:env) : resource := match e with | (_, r) => r end.

  Definition set_state (s:state) (e:env) : env :=
    match e with | (_, r) => (s, r) end.

  Definition set_resource (r:resource) (e:env) : env :=
    match e with | (s, _) => (s, r) end.

End Env.



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
  | WRITE_NEXT   : local_var -> expr -> act
  | WRITE_LOCKED : local_var -> expr -> act

  | FETCH_AND_STORE  : act
  | COMPARE_AND_SWAP : act
  .


(*************************************************************************)
(**** Process ************************************************************)
(*************************************************************************)

Inductive tm : Type :=
  | ERR : tm (* error *)
  | OK  : tm (* no-op *)
  | END : tm (* termination *)

  | IF  : expr -> tm -> tm -> tm

  | ACT : act -> tm

  | SEQ : tm -> tm -> tm

  | LOOP      : tm -> tm (* inner loop body -> *)
  | LOOP_END  : tm

  (* loop id -> body -> outer continuation -> ... *)
  | LOOP_OVER : iloop -> tm -> tm -> tm
  | BREAK     : iloop -> tm
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

Fixpoint subst (new old:tm) (loops:bool) : tm :=
  match old with
  | ERR => ERR
  | OK   => OK
  | END   => END

  | IF c t e => IF c (subst new t loops) (subst new e loops)

  | ACT a => ACT a

  (* do not substitute in other body *)
  | SEQ l r => SEQ l (subst new r loops)

  | LOOP b   => if loops then LOOP (subst new b loops) else LOOP b
  | LOOP_END => new

  | LOOP_OVER l b c => LOOP_OVER l (subst new b loops) (subst new c loops)
  | BREAK l         => BREAK l
  end.


(*************************************************************************)
(**** Evaluate Expressions  **********************************************)
(*************************************************************************)

Fixpoint eval (e:expr) (s:state) : option expr :=
  match e with

  | NOT b =>
    match eval b s with
    | Some TRU => Some FLS
    | Some FLS => Some TRU
    | _ => None
    end

  | EQ a b =>
    match eval a s, eval b s with
    | Some (VAL (NAT a)),
      Some (VAL (NAT b))   => Some (Expr.of_bool (Nat.eqb a b))

    | Some (VAL (INDEX a)),
      Some (VAL (INDEX b)) => Some (Expr.of_bool (Index.eqb a b))

    | Some (VAL (PID a)),
      Some (VAL (PID b))   => Some (Expr.of_bool (Nat.eqb a b))

    | Some (VAL (BOOL a)),
      Some (VAL (BOOL b))  => Some (Expr.of_bool (eqb a b))

    | _, _ => None
    end

  | VAR v => Some (VAL (State.get_local_var_value v s))

  | VAL v => Some (VAL v)

  | TRU => Some TRU
  | FLS => Some FLS
  end.

Definition eval_or_err (c:expr) (t1 t2:tm) (e:env) : tm * env :=
  match eval c (Env.get_state e) with
  | None => (ERR, e)
  | Some t => (<{ if t then t1 else t2 }>, e)
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
    let e:env := Env.set_state (State.set_vars (set_next qnode_next v) s) e in
    Some e
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
    Some (Env.set_state (State.set_vars (set_locked qnode_locked v) s) e)
  end.

(** [write_next to_write next e] will set the
    field [qnode.next] of the [qnode] of the
    index [to_write] to the value of [next]. *)
Definition write_next (to_write:local_var) (next:expr) (e:env) : option env :=
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
      (* make sure [next] is a [VAL] *)
      match eval next s with
      | Some (VAL v) =>
        (* check that this is some kind of nat *)
        match valid_cast_to_nat v with
        | (false, _) => None (* i.e., BOOL, NIL *)
        | (true, None) => None (* i.e., INDEX NONE *)
        | (true, Some n) =>
          match Memory.nth_replace (Qnode.set_next (Some n) q) p m with
          | None => None
          | Some m => Some (Env.set_resource (Resource.set_mem m r) e)
          end
        end
      | _ => None (* [next] must be an [VAL] *)
      end
    end
  | _ => None (* [to_write] must be a [pid] *)
  end.

(** [write_locked to_write locked e] will set the
    field [qnode.locked] of the [qnode] of the
    index [to_write] to the value of [locked]. *)
Definition write_locked (to_write:local_var) (locked:expr) (e:env) : option env :=
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
      (* make sure [locked] is a [VAL] of [BOOL] *)
      match eval locked s with
      | Some (VAL (BOOL b)) =>
        match Memory.nth_replace (Qnode.set_locked b q) p m with
        | None => None
        | Some m => Some (Env.set_resource (Resource.set_mem m r) e)
        end
      | _ => None (* [locked] must be an [VAL] of [BOOL] *)
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
  Some (s, r).


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
    Some (s, r)
  | false =>
    (* set [swap] false *)
    let v:local_vars := set_swap false v in
    let s:state := State.set_vars v s in
    (* update env *)
    Some (s, r)
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


Inductive action : Type :=
  | SILENT : action
  | LABEL  : act -> pid -> action.

Reserved Notation "t '--<{' a '}>-->' t'" (at level 40).

Inductive step : (tm * env) -> action -> (tm * env) -> Prop :=

  | STEP_ACT : forall a e,
    (ACT a, e) --<{LABEL a (State.get_pid (Env.get_state e))}>--> (do_act a e)

  | STEP_SEQ : forall a l1 l2 r e1 e2,
    (l1, e1) --<{a}>--> (l2, e2) ->
    (SEQ l1 r, e1) --<{a}>--> (SEQ l2 r, e2)

  | STEP_SEQ_END : forall r e,
    (SEQ OK r, e) --<{SILENT}>--> (r, e)

  | STEP_LOOP  : forall t e,
    (LOOP t, e) --<{SILENT}>--> (subst t LOOP_END false, e)

  (* TODO: this currently only breaks the immediate outer loop if id's match *)
  (* FIXME: some kind of context hold needed? I.e.: A.B.C =~ A.[[C]] *)
  (* E.g.: LOOP_OVER l [[ BREAK l ]] *)
  | STEP_BREAK : forall l c e,
    (LOOP_OVER l (BREAK l) c, e) --<{SILENT}>--> (c, e)

  (* Or, maybe we can just pass the BREAK outwards to find the correct LOOP: *)
  | STEP_BREAK_OTHER : forall l1 l2 c e,
    (LOOP_OVER l1 (BREAK l2) c, e) --<{SILENT}>--> (BREAK l2, e)

  | STEP_LOOP_OVER : forall a l t1 t2 c e1 e2,
    (LOOP t1, e1) --<{a}>--> (LOOP t2, e2) ->
    (LOOP_OVER l t1 c, e1) --<{a}>--> (LOOP_OVER l t2 c, e2)

  | STEP_IF_TT : forall t1 t2 e,
    (<{ if TRU then t1 else t2 }>, e) --<{SILENT}>--> (t1, e)

  | STEP_IF_FF : forall t1 t2 e,
    (<{ if FLS then t1 else t2 }>, e) --<{SILENT}>--> (t2, e)

  | STEP_IF : forall c t1 t2 e,
    (<{ if c then t1 else t2 }>, e) --<{SILENT}>--> (eval_or_err c t1 t2 e)

  where "t '--<{' a '}>-->' t'" := (step t a t').





Inductive sys : Type :=
  | PRC : tm -> state -> sys
  | PAR : sys -> sys -> sys
  | TERM : sys
  .

Reserved Notation "t '==<{' a '}>==>' t'" (at level 40).

Inductive lts : sys * resource -> action -> sys * resource -> Prop :=

  | LTS_PRC : forall a t1 t2 s1 s2 r1 r2,
    (t1, (s1, r1)) --<{a}>--> (t2, (s2, r2)) ->
    (PRC t1 s1, r1) ==<{a}>==> (PRC t2 s2, r2)

  | LTS_PAR_L : forall a l1 l2 r gr1 gr2,
    (l1, gr1) ==<{a}>==> (l2, gr2) ->
    (PAR l1 r, gr1) ==<{a}>==> (PAR l2 r, gr2)

  | LTS_PAR_R : forall a l r1 r2 gr1 gr2,
    (r1, gr1) ==<{a}>==> (r2, gr2) ->
    (PAR l r1, gr1) ==<{a}>==> (PAR l r2, gr2)

  | LTS_END_L : forall a ls r gr,
    (PAR (PRC END ls) r, gr) ==<{a}>==> (r, gr)

  | LTS_END_R : forall a l rs gr,
    (PAR l (PRC END rs), gr) ==<{a}>==> (l, gr)

  where "t '==<{' a '}>==>' t'" := (lts t a t')
  and "t '--<{' a '}>-->' t'" := (step t a t').



(*************************************************************************)
(**** Example ************************************************************)
(*************************************************************************)

Import Expr.

Example Acquire : tm :=
  SEQ (ACT (WRITE_NEXT VAR_PID (VAL NIL))) (
    SEQ (ACT FETCH_AND_STORE) (
      IF (NOT (EQ (VAR VAR_PREDECESSOR) (VAL NIL))) (
        SEQ (ACT (WRITE_LOCKED VAR_PREDECESSOR (VAR VAR_PID))) (
          LOOP_OVER 0 (
            SEQ (ACT READ_LOCKED) (
              IF (NOT (VAR VAR_LOCKED)) (BREAK 0) (OK)
            )
          ) (END)
        )
      ) (END)
    )
  ).


Example Release : tm :=
  SEQ (ACT READ_NEXT) (
    IF (EQ (VAL NIL) (VAR VAR_NEXT)) (
      SEQ (ACT COMPARE_AND_SWAP) (
        IF (EQ FLS (VAR VAR_SWAP)) (
          LOOP_OVER 0 (*L*) (
            SEQ (ACT READ_NEXT) (
              IF (NOT (EQ (VAL NIL) (VAR VAR_NEXT))) (BREAK 0 (*L*)) (OK)
            )
          ) (ACT (WRITE_LOCKED VAR_NEXT TRU))
        ) (END)
      )
    ) (ACT (WRITE_LOCKED VAR_NEXT FLS))
  ).


Example P : tm :=
  LOOP (
    SEQ (ACT NCS) (
      SEQ Acquire (
        SEQ (ACT ENTER) (
          SEQ (ACT LEAVE) (
            SEQ Release END
          )
        )
      )
    )
  ).



(**********************************)
(**** Single process **************)
(**********************************)

Example p0 : tm * env := (P, (State.initial, Resource.initial 0)).
MeBi LTS step p0.



(**********************************)
(**** System **********************)
(**********************************)

Definition process : Type := tm * state.

Definition spawn (p:pid) (b:tm) : process := (b, State.create p).

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
    | (p, s) => PAR (PRC p s) (load t)
    end
  end.

Definition composition : Type := sys * resource.
Definition compose (s:system) : composition :=
  match s with
  | (ps, r) => (load ps, r)
  end.

Example ncs : composition := compose (create 5 P).
MeBi LTS lts ncs.

