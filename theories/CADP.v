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

(* recursive definition identifier *)
Definition idef : Type := nat.

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

  Definition set_next   (i:index) (q:qnode) : qnode :=
    Build_qnode i (Qnode.get_locked q).

  Definition set_locked (b:bool)  (q:qnode) : qnode :=
    Build_qnode (Qnode.get_next q) b.

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


Definition valid_cast_to_index (v:value) : bool * index :=
  match v with
  | NAT n => (true, Some n)
  | PID p => (true, Index.of_pid p)
  | INDEX i => (true, i)
  | NIL => (true, None)
  | _ => (false, None) (* i.e., BOOL *)
  end.

(*************************************************************************)
(**** (Global) Resource **************************************************)
(*************************************************************************)
Definition resource : Type := mem * lock.

Module Resource.
  Definition initial (n:nat) : resource := (Memory.initial n, Lock.initial).
End Resource.

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


(*************************************************************************)
(**** (Local) State ******************************************************)
(*************************************************************************)
Definition state : Type := pid * local_vars.

Module State.
  Definition create (p:pid) : state := (p, Vars.initial).
  Definition initial : state := State.create 0.
End State.

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

  (* def id -> def body -> *)
  | REC_DEF : idef -> tm -> tm
  | REC_CALL : idef -> tm
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

  | VAR v => Some (get_local_var_value v s)

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
(**** Environment (local) ************************************************)
(*************************************************************************)

Definition env : Type := state * resource.

Module Env.
  Definition initial (n:nat) : env := (State.initial, Resource.initial n).
End Env.

Definition get_state (e:env) : state := match e with | (s, _) => s end.

Definition get_resource (e:env) : resource :=
  match e with | (_, r) => r end.

(* not used, but useful in goals *)
Definition set_state (s:state) (e:env) : env :=
  match e with | (_, r) => (s, r) end.

(* not used, but useful in goals *)
Definition set_resource (r:resource) (e:env) : env :=
  match e with | (s, _) => (s, r) end.


(*************************************************************************)
(**** (Local) Semantics of Processes *************************************)
(*************************************************************************)

(** [read_next e] will set var [next] to be the value
    currently of the [qnode.next] corresponding to the
    [pid] of the process in the state. *)
Definition read_next (e:env) : option env :=
  (* get pid *)
  let s:state := get_state e in
  let p:pid := get_pid s in
  (* get qnode *)
  let r:resource := get_resource e in
  let m:mem := get_mem r in
  match Memory.nth_error p m with
  | None => None (* tried to access out of bounds *)
  | Some q => (* get next of qnode *)
    let qnode_next:index := Qnode.get_next q in
    (* get local vars *)
    let v:local_vars := get_vars s in
    (* update env *)
    Some (set_vars (set_next qnode_next v) s, r)
  end.

(** [read_locked e] will set var [locked] to be
    the value of the [qnode.locked] corresponding
    to the [pid] of the process in the state. *)
Definition read_locked (e:env) : option env :=
  (* get pid *)
  let s:state := get_state e in
  let p:pid := get_pid s in
  (* get qnode *)
  let r:resource := get_resource e in
  let m:mem := get_mem r in
  match Memory.nth_error p m with
  | None => None (* tried to access out of bounds *)
  | Some q => (* get locked of qnode *)
    let qnode_locked:bool := Qnode.get_locked q in
    (* get local vars *)
    let v:local_vars := get_vars s in
    (* update env *)
    Some (set_vars (set_locked qnode_locked v) s, r)
  end.

(*DC: slight simplification: instead of taking an expr, it needs to take a val*)
(** [write_next to_write next e] will set the
    field [qnode.next] of the [qnode] of the
    index [to_write] to the value of [next]. *)
Definition write_next (to_write:local_var) (next:value) (e:env) : option env :=
  let s:state := get_state e in
  (* get value of [next] *)
  match handle_value next s with
  | None => None (* [next=GET v] and [v] could not be resolved *)
  | Some next =>
    (* get pid from [to_write] *)
    match get_local_var_value to_write s with
    | PID p => (* should be pid *)
      (* get qnode *)
      let r:resource := get_resource e in
      let m:mem := get_mem r in
      match Memory.nth_error p m with
      | None => None (* tried to access out of bounds *)
      | Some q => (* check [next] of [qnode] is some kind of nat *)
        match valid_cast_to_index next with
        | (false, _) => None (* i.e., BOOL *)
        | (true, n) => (* i.e., PID n, INDEX n, NIL, NAT n *)
          match Memory.nth_replace (Qnode.set_next n q) p m with
          | None => None
          | Some m => Some (s, set_mem m r)
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
  let s:state := get_state e in
  match get_local_var_value to_write s with
  | PID p => (* should be pid *)
    (* get qnode *)
    let r:resource := get_resource e in
    let m:mem := get_mem r in
    match Memory.nth_error p m with
    | None => None (* [qnode] must exist *)
    | Some q => (* check [locked] of [qnode] is a [BOOL] *)
      match locked with
      | BOOL b =>
        match Memory.nth_replace (Qnode.set_locked b q) p m with
        | None => None
        | Some m => Some (s, (set_mem m r))
        end
      | _ => None (* [locked] must be a [BOOL] *)
      end
    end
  | _ => None (* [to_write] must be a [pid] *)
  end.

Definition fetch_and_store (e:env) : option env :=
  (* get [i] *)
  let r:resource := get_resource e in
  let i:index := get_lock_i r in
  (* store [i] in [predecessor] *)
  let s:state := get_state e in
  let v:local_vars := get_vars s in
  let v:local_vars := set_predecessor i v in
  let s:state := set_vars v s in
  (* set [i] to [pid] *)
  let p:pid := get_pid s in
  let r:resource := set_lock_i (Index.of_pid p) r in
  (* update env *)
  Some (s, r).


Definition compare_and_swap (e:env) : option env :=
  (* set [j] to [pid] *)
  let s:state := get_state e in
  let p:pid := get_pid s in
  let r:resource := get_resource e in
  let r:resource := set_lock_j (Index.of_pid p) r in
  (* get [i] and [j] *)
  let i:index := get_lock_i r in
  let j:index := get_lock_j r in
  (* if the same, then set [swap] to true and set [i] to [Nil], else false *)
  let v:local_vars := get_vars s in
  match Index.eqb i j with
  | true =>
    (* set [swap] true *)
    let v:local_vars := set_swap true v in
    let s:state := set_vars v s in
    (* set [i] to [Nil] *)
    let r:resource := set_lock_i Nil r in
    (* update env *)
    Some (s, r)
  | false =>
    (* set [swap] false *)
    let v:local_vars := set_swap false v in
    let s:state := set_vars v s in
    (* update env *)
    Some (s, r)
  end.

(*************************************************************************)
(**** Actions ************************************************************)
(*************************************************************************)

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

      | _ => None (* shouldn't happen, outer match catches these *)
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
  LABEL (a, (get_pid (get_state e))).

Definition get_action (a:act) (e:env) : action :=
  match a with
  (* | NCS   => visible_action a e *)
  | NCS   => SILENT
  | ENTER => visible_action a e
  | LEAVE => visible_action a e
  | _ => SILENT
  end.

(*************************************************************************)
(**** Unfolding **********************************************************)
(*************************************************************************)

Definition rec_def : Type := idef * tm.

Fixpoint unfold (new:rec_def) (old:tm) : tm :=
  match old with
  | ERR => ERR
  | OK  => OK

  | ACT a => ACT a

  | IF c t1 t2 => IF c (unfold new t1) (unfold new t2)

  | SEQ l r => SEQ l (unfold new r)

  | REC_DEF i b =>
    match new with
    | (j, b') => if Nat.eqb i j
                 then unfold (i, b) b
                 else REC_DEF i (unfold new b)
    end

  | REC_CALL i =>
    match new with
    | (j, b) => if Nat.eqb i j
                then REC_DEF j b
                else REC_CALL i
    end

  end.

(*************************************************************************)
(**** Semantics (Local step) *********************************************)
(*************************************************************************)

Reserved Notation "t '--<{' a '}>-->' t'" (at level 40).

Inductive step : (tm * env) -> action -> (tm * env) -> Prop :=

  | STEP_ACT : forall a e, (ACT a, e) --<{get_action a e}>--> (do_act a e)

  | STEP_SEQ_END : forall r e, (SEQ OK r, e) --<{SILENT}>--> (r, e)

  | STEP_SEQ : forall a l1 l2 r e1 e2,
    (l1, e1) --<{a}>--> (l2, e2) ->
    (SEQ l1 r, e1) --<{a}>--> (SEQ l2 r, e2)

  | STEP_IF_TRU : forall c t1 t2 e,
    eval c (get_state e) = Some (BOOL true) ->
    (<{ if c then t1 else t2 }>, e) --<{SILENT}>--> (t1, e)

  | STEP_IF_FLS : forall c t1 t2 e,
    eval c (get_state e) = Some (BOOL false) ->
    (<{ if c then t1 else t2 }>, e) --<{SILENT}>--> (t2, e)

  | STEP_REC_DEF : forall i b e,
    (REC_DEF i b, e) --<{SILENT}>--> (unfold (i, b) b, e)

  where "t '--<{' a '}>-->' t'" := (step t a t').

(*************************************************************************)
(**** Semantics (System lts) *********************************************)
(*************************************************************************)

Inductive sys : Type :=
  | PRC : tm -> state -> sys
  | PAR : sys -> sys -> sys
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

  | LTS_OK_L : forall s r g, (PAR (PRC OK s) r, g) ==<{SILENT}>==> (r, g)

  | LTS_OK_R : forall l s g, (PAR l (PRC OK s), g) ==<{SILENT}>==> (l, g)

  where "t '==<{' a '}>==>' t'" := (lts t a t')
  and "y '--<{' a '}>-->' y'" := (step y a y').

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
            REC_DEF 2 (
              SEQ (ACT READ_LOCKED) (
                IF (VAR LOCKED) (REC_CALL 2) (OK)
              )
            )
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
          REC_DEF 1 (
            SEQ (ACT READ_NEXT) (
              IF (EQ (VAL NIL) (VAR NEXT))
                (REC_CALL 1)
                (ACT (WRITE_LOCKED NEXT (BOOL true)))
            )
          )
        ) (OK)
      )
    ) (ACT (WRITE_LOCKED NEXT (BOOL false)))
  ).

Example P : tm :=
  REC_DEF 0 (
    SEQ (ACT NCS) (
      SEQ Acquire (
        SEQ (ACT ENTER) (
          SEQ (ACT LEAVE) (
            SEQ Release (REC_CALL 0)
          )
        )
      )
    )
  ).

Compute (write_next THE_PID NIL (Env.initial 1)).

(** Thank you paulo for the tip! -- (see below, reworded by Jonah)
      A lemma/property can be used to help tell
      Coq how to unify existential variables. *)
Lemma STEP_ACT_helper:
  forall a e x y z,
  x = (ACT a, e) ->
  y = get_action a e ->
  z = do_act a e ->
  x --<{y}>--> z.
Proof.
  intros; subst.
  constructor.
Qed.

Goal exists a t e, (Acquire, Env.initial 1) --<{ a }>--> (t, e).
  do 3 eexists.
  eapply STEP_SEQ.
  eapply STEP_ACT_helper; reflexivity.
Qed.

Goal exists a t e, (Release, Env.initial 1) --<{ a }>--> (t, e).
  do 3 eexists.
  eapply STEP_SEQ.
  eapply STEP_ACT_helper; reflexivity.
Qed.

(*************************************************************************)
(**** Single process *****************************************************)
(*************************************************************************)

Example p0 : tm * env := (P, Env.initial 0).
(* MeBi LTS show_debug step p0. *)
MeBi LTS step p0.

(*************************************************************************)
(**** System *************************************************************)
(*************************************************************************)

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
  | [] => PRC OK State.initial (* shouldn't happen *)
  | h :: t =>
    match h with
    | (p, s) =>
      match t with (* to avoid having a spare empty state at the end *)
      | [] => PRC p s
      | _  => PAR (PRC p s) (load t)
      end
    end
  end.

Definition composition : Type := sys * resource.
Definition compose (s:system) : composition :=
  match s with
  | (ps, r) => (load ps, r)
  end.

Example ncs1 : composition := compose (create 1 P).
Compute ncs1.

(* original: *) MeBi LTS  show_debug lts ncs1.
(* updated:  *) MeBi LTS2 show_debug lts ncs1 step.

Example ncs2 : composition := compose (create 2 P).
Compute ncs2.
MeBi LTS show_debug lts ncs2.
MeBi LTS2 show_debug lts ncs2 step.

Example ncs5 : composition := compose (create 5 P).
Compute ncs5.
MeBi LTS show_debug lts ncs5.
MeBi LTS2 show_debug lts ncs5 step.



Goal exists a t s r, (ncs1) ==<{a}>==> (PRC t s, r).
Proof.
  do 4 eexists.
  unfold ncs1.
  simpl.
  unfold P.
  eapply LTS_PRC.
  constructor.
Qed.


(** Following the same stratefy as [STEP_ACT_helper] (above) *)
(** Note: below ends up not being necessary. *)
(* Lemma LTS_PRC_helper :
  forall a t1 t2 s1 s2 r1 r2 x1 x2,
  x1 = (PRC t1 s1, r1) ->
  (t1, (s1, r1)) --<{ a }>--> (t2, (s2, r2)) ->
  x2 = (PRC t2 s2, r2) ->
  x1 ==<{ a }>==> x2.
Proof.
  intros; subst.
  constructor.
  apply H0.
Qed. *)

(** this time testing the ACT *)
Example ncs1b : composition := compose (create 1 Acquire).
Compute ncs1b.

Goal exists a t s r, (ncs1b) ==<{a}>==> (PRC t s, r).
Proof.
  do 4 eexists.
  unfold ncs1b.
  simpl.
  unfold Acquire.
  constructor.
  eapply STEP_SEQ.
  eapply STEP_ACT_helper; reflexivity.
Qed.



(*************************************************************************)
(**** TODO: LTS equiv temp. logic prop. **********************************)
(*************************************************************************)

Definition prc_trace : Type := list act.

Definition sys_trace : Type := list (pid * prc_trace).

Module Act.
  Definition eq (a:act) (b:act) : bool :=
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
  Definition get_dual (a:act) : option act :=
    match a with
    | ENTER => Some LEAVE
    | LEAVE => Some ENTER
    | _ => None
    end.

  (** [are_dual a b] returns [Some bool] denoting if [a] and [b] are dual, and returns [None] if either [a] or [b] have no dual. *)
  Definition are_dual (a:act) (b:act) : option bool :=
    match Act.get_dual a, Act.get_dual b with
    | None, _ => None
    | _, None => None
    | Some a, Some _ => Some (Act.eq a b)
    end.
End Act.

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

(** [last_act_of p s] returns the last act of [p] (if [p] not in [s], then [None]). *)
Definition last_act_of (p:pid) (s:sys_trace) : bool * option act :=
  match get_prc_trace p s with
  | None => (false, None)
  | Some [] => (true, None)
  | Some (h :: _) => (true, Some h)
  end.


(** [exist_last_act a s] returns [true] if the head of any of the [prc_trace] in [s] matches [a]. *)
Fixpoint exist_last_act (a:act) (s:sys_trace) : bool :=
  match s with
  | [] => false
  | (_, l) :: t =>
    match l with
    | [] => exist_last_act a t
    | b :: _ => if Act.eq a b then true else exist_last_act a t
    end
  end.

Fixpoint all_last_act (a:act) (s:sys_trace) : bool :=
  match s with
  | [] => true
  | (_, l) :: t =>
    match l with
    | [] => false
    | b :: _ => if Act.eq a b then all_last_act a t else false
    end
  end.

Fixpoint append_act_hd (p:pid) (a:act) (s:sys_trace) : option sys_trace :=
  match s with
  | [] => None
  | (q, l) :: t =>
    if Nat.eqb p q then Some ((p, a :: l) :: t) else
    match append_act_hd p a t with
    | None => None
    | Some c => Some ((q,l) :: c)
    end
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

  | (true, Some b) => Act.are_dual a b (* [p] last act [b] in [s]*)
  end.


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



(** [no_starvation]
  [ true* ] forall i:nat among {1..N}.
  [ for j:nat from 1 to N do
      (not { ENTER ...!i })*.
      { ?G:string ...!j where (j=i) -> (G<>"ENTER") }
    end for
  ]-|
*)
Module NoStarvation.

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
    | (l, _) => length l
    end.

  Fixpoint update_prc_history_pids (p:pid) (y:list pid) : list pid :=
    match y with
    | [] => [p] (* [p] is not being tracked yet, add *)
    | q :: t =>
      if Nat.eqb p q then y (* [p] already in [y] *)
      else q :: update_prc_history_pids p t
    end.

  (** [sys_history] is the same as [sys_trace], but with [prc_history]. *)
  Definition sys_history : Type := list (pid * prc_trace * prc_history).

  Definition num_of_prc (z:sys_history) : nat := length z.

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

  (** [do_action a z] checks if the process [p] can do act [a] in [z] and in doing so also updates all of the history/logs of the processes that are/are not acting. If [p] can do [a], it then checks to see if any process is now starving. It returns None if any of the above is not met. *)
  Definition do_action (a:action) (z:sys_history) : option sys_history :=
    match a with
    | SILENT => Some z (* skip *)
    | LABEL (a, p) =>
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
  Inductive lts : sys_history -> action -> sys_history -> Prop :=
    | ACT : forall t1 t2 a, do_action a t1 = Some t2 -> lts t1 a t2
    .

End NoStarvation.

