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
  Definition initial : local_vars := Build_local_vars Nil false Nil false.
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
(**** Debugging **********************************************************)
(*************************************************************************)

Record error := { error_code : nat
                ; error_msg  : string
                ; info       : option (expr * option value)
                }.

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
Definition state : Type := pid * local_vars * (option (list error)).

Module State.
  Definition create (p:pid) : state := (p, Vars.initial, None).
  Definition initial : state := State.create 0.
End State.

Definition get_pid (s:state) : pid :=
  match s with | (p, _, _) => p end.

Definition get_vars (s:state) : local_vars :=
  match s with | (_, v, _) => v end.

Definition get_errors (s:state) : option (list error) :=
  match s with | (_, _, e) => e end.

Definition get_predecessor (s:state) : index := var_predecessor (get_vars s).
Definition get_locked      (s:state) : bool  := var_locked (get_vars s).
Definition get_next        (s:state) : index := var_next (get_vars s).
Definition get_swap        (s:state) : bool  := var_swap (get_vars s).

Definition handle_index_value (i:index) : value :=
  match i with
  | None => NIL
  | _ => INDEX i
  end.

Definition get_local_var_value (v:local_var) (s:state) : value :=
  match v with
  | LOCKED      => BOOL  (get_locked s)
  | SWAP        => BOOL  (get_swap s)
  | PREDECESSOR => handle_index_value (get_predecessor s)
  | NEXT        => handle_index_value (get_next s)
  | THE_PID     => PID   (get_pid s)
  end.

Definition set_vars (v:local_vars) (s:state) : state :=
  match s with | (p, _, e) => (p, v, e) end.

Definition has_error_occurred (s:state) : bool :=
  match get_errors s with
  | None => false
  | _ => true
  end.

Definition add_error (e:error) (s:state) : state :=
  match s with
  | (p, v, es) =>
    match es with
    | None => (p, v, Some [e])
    | Some es => (p, v, Some (es ++ [e]))
    end
  end.


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
(* Notation "'if' c 'then' t 'else' e" := (IF c t e)
                (in custom tm at level 90, c custom tm at level 80,
                  t custom tm at level 80, e custom tm at level 80): tm_scope. *)
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

    | Some NIL, Some NIL => Some (BOOL true)

    (* always propagate a None (i.e., an error) *)
    | Some NIL, None => None
    | None, Some NIL => None

    (* NOT NIL if other side isn't NIL and isn't None *)
    | Some NIL, _ => Some (BOOL true)
    | _, Some NIL => Some (BOOL true)


    (* error has occurred *)
    | _, _ => None
    end

  end.

(* Compute (eval (VAL (BOOL true)) State.initial). *)
(* Compute (eval (VAL (BOOL false)) State.initial). *)

(* Compute (eval (VAL NIL) State.initial). *)
(* Compute (eval (VAR PREDECESSOR) State.initial). *)


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
    let p:option pid := (* should be index of pid, not None *)
    match get_local_var_value to_write s with
    | INDEX i => i
    | PID p => Some p
    | _ => None (* [to_write] must be a [pid] *)
    end
    in
    match p with
    | None => None
    | Some p => (* get qnode *)
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
    end
  end.

(** [write_locked to_write locked e] will set the
    field [qnode.locked] of the [qnode] of the
    index [to_write] to the value of [locked]. *)
Definition write_locked (to_write:local_var) (locked:value) (e:env) : option env :=
  (* get pid from [to_write] *)
  let s:state := get_state e in
  let p:option pid := (* should be index of pid, not None *)
    match get_local_var_value to_write s with
    | INDEX i => i
    | PID p => Some p
    | _ => None (* [to_write] must be a [pid] *)
    end
  in
  match p with
  | None =>
      (* Some (State.create 99, get_resource e) *)
      match get_local_var_value to_write s with
      | INDEX None => Some (add_error (Build_error 91 "write_locked, to_write yielded INDEX None" None) s, get_resource e)
      | INDEX _ => Some (add_error (Build_error 92 "write_locked, to_write yielded INDEX _" None) s, get_resource e)
      | NIL => Some (add_error (Build_error 93 "write_locked, to_write yielded NIL" None) s, get_resource e)
      | PID _ => Some (add_error (Build_error 94 "write_locked, to_write yielded PID _" None) s, get_resource e)
      | BOOL _ => Some (add_error (Build_error 95 "write_locked, to_write yielded BOOL _" None) s, get_resource e)
      | _ => None (* [to_write] must be a [pid] *)
      end
  | Some p => (* get qnode *)
    let r:resource := get_resource e in
    let m:mem := get_mem r in
    match Memory.nth_error p m with
    | None => Some (add_error (Build_error 96 "write_locked, qnode does not exist" None) s, get_resource e) (* [qnode] must exist *)
    | Some q => (* check [locked] of [qnode] is a [BOOL] *)
      match locked with
      | BOOL b =>
        match Memory.nth_replace (Qnode.set_locked b q) p m with
        | None => Some (add_error (Build_error 97 "write_locked, nth_replace failed" None) s, get_resource e)
        | Some m => Some (s, (set_mem m r))
        end
      | _ => Some (add_error (Build_error 98 "write_locked, locked value not bool" None) s, get_resource e) (* [locked] must be a [BOOL] *)
      end
    end
  end.

(** stores the current contents of [i] of the lock in the global resource [e] within the local var [predecessor], and sets [i] to contain the [pid]. *)
Definition fetch_and_store (e:env) : option env :=
  (* get [i] from lock in global resource [e] *)
  let r:resource := get_resource e in
  let i:index := get_lock_i r in
  (* store [i] in local var [predecessor] *)
  let s:state := get_state e in
  let v:local_vars := get_vars s in
  let v:local_vars := set_predecessor i v in
  let s:state := set_vars v s in
  (* set [i] of lock in global resource to [pid] *)
  let p:pid := get_pid s in
  let r:resource := set_lock_i (Index.of_pid p) r in
  (* update env *)
  Some (s, r).

(** if [i] and [j] of the lock of the global resource [e] are the same then sets local var [swap] to [true] and sets [i] to [NIL]. otherwise, sets local var [swap] to [false] and [i] and [j] are unchanged. *)
Definition compare_and_swap (e:env) : option env :=
  (* set [j] of lock (in global resource [e]) to [pid] *)
  let s:state := get_state e in
  let p:pid := get_pid s in
  let r:resource := get_resource e in
  let r:resource := set_lock_j (Index.of_pid p) r in
  (* get [i] and [j] of lock in global resource [e] *)
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
    | Some e => (if has_error_occurred (get_state e) then ERR else OK, e)
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
  | NCS   => SILENT (* visible_action a e *)
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
(**** Conditional branching **********************************************)
(*************************************************************************)

Definition take_branch (c:expr) (t1:tm) (t2:tm) (e:env) : tm * env :=
  match eval c (get_state e) with
  | Some (BOOL true) => (t1, e)
  | Some (BOOL false) => (t2, e)
  | err_val => (ERR, ((add_error (Build_error 1 "conditional statement did not evaluate correctly" (Some (c, err_val))) (get_state e)), get_resource e))
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

| STEP_IF : forall c t1 t2 e,
  (IF c t1 t2, e) --<{SILENT}>--> (take_branch c t1 t2 e)

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
(**** System *************************************************************)
(*************************************************************************)

Definition process : Type := tm * state.

Definition spawn (p:pid) (b:tm) : process := (b, State.create p).

Fixpoint populate (n:nat) (b:tm) : list process :=
  match n with
  | 0 => []
  | S m => app (populate m b) [spawn m b]
  end.

Definition system : Type := list process * resource.

Definition create (n:nat) (b:tm) : system := (populate n b, Resource.initial n).

Fixpoint load (ps:list process) : sys :=
  match ps with
  | [] => PRC ERR State.initial (* shouldn't happen *)
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

(*************************************************************************)
(**** Proof Utils ********************************************************)
(*************************************************************************)

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

Inductive step_transitive_closure : tm * env -> Prop :=
| trans_step : forall t a t' e' e,
    (t, e) --<{ a }>--> (t', e') ->
    step_transitive_closure (t', e') ->
    step_transitive_closure (t, e)

| no_step : forall t e, step_transitive_closure (t, e)
.

Inductive lts_transitive_closure : sys * resource -> Prop :=
| trans_lts : forall t a t' e' e,
    (t, e) ==<{ a }>==> (t', e') ->
    lts_transitive_closure (t', e') ->
    lts_transitive_closure (t, e)

| no_lts : forall t e, lts_transitive_closure (t, e)
.

(*************************************************************************)
(**** Example: Full CADP *************************************************)
(*************************************************************************)

Import Expr.

Example AcquireInner : tm :=
  SEQ (ACT READ_LOCKED) (
    IF (VAR LOCKED) (REC_CALL 2) (OK)
  ).

Example Acquire : tm :=
  SEQ (ACT (WRITE_NEXT THE_PID NIL)) (
    SEQ (ACT FETCH_AND_STORE) (
      IF (EQ (VAR PREDECESSOR) (VAL NIL)) (OK) (
        SEQ (ACT (WRITE_LOCKED PREDECESSOR (BOOL true))) (
          SEQ (ACT (WRITE_NEXT PREDECESSOR (GET THE_PID))) (
            REC_DEF 2 (AcquireInner)
          )
        )
      )
    )
  ).

Example ReleaseInner : tm :=
  SEQ (ACT READ_NEXT) (
    IF (EQ (VAL NIL) (VAR NEXT))
      (REC_CALL 1)
      (ACT (WRITE_LOCKED NEXT (BOOL true)))
  ).

Example Release : tm :=
  SEQ (ACT READ_NEXT) (
    IF (EQ (VAL NIL) (VAR NEXT)) (
      SEQ (ACT COMPARE_AND_SWAP) (
        IF (EQ FLS (VAR SWAP)) (
            REC_DEF 1 (ReleaseInner)
        ) (OK)
      )
    ) (ACT (WRITE_LOCKED NEXT (BOOL false)))
  ).

Example P : tm :=
  REC_DEF 0 (
    (* SEQ (ACT NCS) ( *)
      SEQ Acquire (
        SEQ (ACT ENTER) (
          SEQ (ACT LEAVE) (
            SEQ Release (REC_CALL 0)
            (* SEQ Release (OK) *)
            (* SEQ Release (
              SEQ Acquire (
                SEQ (ACT ENTER) (
                  SEQ (ACT LEAVE) (
                    SEQ Release (
                      SEQ Acquire (
                        SEQ (ACT ENTER) (
                          SEQ (ACT LEAVE) (
                            (* SEQ Release (OK) *)
                            SEQ Release (
                              SEQ Acquire (
                                SEQ (ACT ENTER) (
                                  SEQ (ACT LEAVE) (
                                    SEQ Release (OK)
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            ) *)
          )
        )
      )
    (* ) *)
  )
  .

(*************************************************************************)
(**** Instances of P *****************************************************)
(*************************************************************************)

(***************************)
(**** Single process *******)
(***************************)
Example p0 : tm * env := (P, Env.initial 1).
(* MeBi Show LTS Bounded 37 Of p0 Using step. *)
MeBi Dump "p0" LTS Bounded 37 Of p0 Using step.

(* MeBi Dump "p0" FSM Bounded 150 Of p0 Using step. *)
(* MeBi Dump "p0" Minim Bounded 37 Of p0 Using step. *)


(***************************)
(**** System size: 1 *******)
(***************************)
Example g1 : sys * resource := compose (create 1 P).
(* MeBi Show LTS Bounded 37 Of g1 Using lts step. *)
(* MeBi Dump "g1" LTS Bounded 37 Of g1 Using lts step. *)

(***************************)
(**** System size: 2 *******)
(***************************)
Example g2 : sys * resource := compose (create 2 P).
(* Compute g2. *)
(* MeBi Dump "g2" LTS Bounded 5000 Of g2 Using lts step. *)


(*************************************************************************)
(**** Example: Glued CADP ************************************************)
(*************************************************************************)

Definition do_acquire_inner (i:idef) (c:tm) (s:state) (r:resource)
  : sys * resource
  :=
  let e : env := (s, r) in
  (* REC_DEF 2 *)
  let t : tm := unfold (i, AcquireInner) AcquireInner in
  (* ACT READ_LOCKED *)
  match read_locked e with
  | None => (PRC ERR
                (add_error (Build_error 40
                  "do_acquire_inner, read_locked e failed" None) s),
                r)
  | Some e =>
    (* IF (VAR LOCKED) *)
    match (get_local_var_value LOCKED (get_state e)) with
    | BOOL true => (* REC_CALL 2 *) (PRC (SEQ t c) (get_state e), get_resource e)
    | BOOL false => (PRC c (get_state e), get_resource e)
    | _ => (PRC ERR
                (add_error (Build_error 41
                  "do_acquire_inner, var locked not bool" None) (get_state e)),
                get_resource e)
    end
  end
  .


Definition do_acquire (c:tm) (s:state) (r:resource)
  : sys * resource
  :=
  let e : env := (s, r) in
  (* ACT (WRITE_NEXT THE_PID NIL) *)
  match write_next THE_PID NIL e with
  | None => (PRC ERR
                (add_error (Build_error 50
                  "do_acquire, write_next THE_PID NIL failed" None) s),
                r)
  | Some e =>
    (* ACT FETCH_AND_STORE *)
    match fetch_and_store e with
    | None => (PRC ERR
                  (add_error (Build_error 51
                    "do_acquire, fetch_and_store failed" None) (get_state e)),
                  (get_resource e))
    | Some e =>
      (* IF (EQ (VAR PREDECESSOR) (VAL NIL)) *)
      match (get_local_var_value PREDECESSOR (get_state e)) with
      | NIL => (* OK *) (PRC c (get_state e), get_resource e)
      | _ =>
        (* ACT (WRITE_LOCKED PREDECESSOR (BOOL true)) *)
        match write_locked PREDECESSOR (BOOL true) e with
        | None => (PRC ERR
                      (add_error (Build_error 52
                        "do_acquire, write_locked PREDECESSOR (BOOL true) failed" None) (get_state e)),
                  (get_resource e))
        | Some e =>
          (* ACT (WRITE_NEXT PREDECESSOR (GET THE_PID)) *)
          match write_next PREDECESSOR (GET THE_PID) e with
          | None => (PRC ERR
                        (add_error (Build_error 53
                          "do_acquire, write_next PREDECESSOR (GET THE_PID) failed" None) (get_state e)),
                    (get_resource e))
          | Some e =>
            (PRC (SEQ (REC_DEF 2 (AcquireInner)) c) (get_state e)
            , get_resource e
            )
          end
        end
      end
    end
  end
  .

Definition do_release_inner (i:idef) (c:tm) (s:state) (r:resource)
  : sys * resource
  :=
  let e : env := (s, r) in
  (* REC_DEF 1 *)
  let t : tm := unfold (i, ReleaseInner) ReleaseInner in
  (* ACT READ_NEXT *)
  match read_next e with
  | None => (PRC ERR
                (add_error (Build_error 45
                  "do_release_inner, read_next e failed" None) s),
                r)
  | Some e =>
    (* IF (EQ (VAL NIL) (VAR NEXT)) *)
    match (get_local_var_value NEXT (get_state e)) with
    | NIL => (* REC_CALL 1 *) (PRC (SEQ t c) (get_state e), get_resource e)
    | _ =>
      (* ACT (WRITE_LOCKED NEXT (BOOL false)) *)
      match write_locked PREDECESSOR (BOOL true) e with
      | None => (PRC ERR
                    (add_error (Build_error 46
                      "do_release_inner, write_locked PREDECESSOR (BOOL false) failed" None) (get_state e)),
                    (get_resource e))
      | Some e =>
        (PRC c (get_state e), get_resource e)
      end
    end
  end
  .

Definition do_release (c:tm) (s:state) (r:resource)
  : sys * resource
  :=
  let e : env := (s, r) in
  (* ACT READ_NEXT *)
  match read_next e with
  | None => (PRC ERR
                (add_error (Build_error 60
                  "do_release, read_next e failed" None) s),
                r)
  | Some e =>
    (* IF (EQ (VAL NIL) (VAR NEXT)) *)
    match (get_local_var_value NEXT (get_state e)) with
    | NIL =>
      (* ACT COMPARE_AND_SWAP *)
      match compare_and_swap e with
      | None => (PRC ERR
                    (add_error (Build_error 61
                      "do_acquire, compare_and_swap failed" None) (get_state e)),
                    (get_resource e))
      | Some e =>
        (* IF (EQ FLS (VAR SWAP)) *)
        match (get_local_var_value SWAP (get_state e)) with
        | BOOL false =>
          (PRC (SEQ (REC_DEF 1 (ReleaseInner)) c) (get_state e)
          , get_resource e)
        | _ => (* OK *) (PRC c (get_state e), get_resource e)
        end
      end

    | _ =>
      (* ACT (WRITE_LOCKED NEXT (BOOL false)) *)
      match write_locked PREDECESSOR (BOOL true) e with
      | None => (PRC ERR
                    (add_error (Build_error 61
                      "do_release, write_locked PREDECESSOR (BOOL true) failed" None) (get_state e)),
                    (get_resource e))
      | Some e =>
        (PRC c (get_state e), get_resource e)
      end
    end
  end
  .

Inductive bigstep : sys * resource -> action -> sys * resource -> Prop :=
(*  0 *)
| DO_ACQUIRE : forall c s r,
  bigstep (PRC (SEQ (Acquire) (c)) s, r)
          SILENT
          (do_acquire c s r)

(*  1 *)
| DO_RELEASE : forall c s r,
  bigstep (PRC (SEQ (Release) (c)) s, r)
          SILENT
          (do_release c s r)

(*  2 *)
| DO_ACQUIRE_INNER : forall i c s r,
  bigstep (PRC (SEQ (REC_DEF i AcquireInner) c) s, r)
          SILENT
          (do_acquire_inner i c s r)

(*  3 *)
| DO_RELEASE_INNER : forall i c s r,
  bigstep (PRC (SEQ (REC_DEF i ReleaseInner) c) s, r)
          SILENT
          (do_release_inner i c s r)

(*  4 *)
| DO_SEQ_ACT_ENTER : forall y s r,
  bigstep (PRC (SEQ (ACT ENTER) y) s, r) (LABEL (ENTER, (get_pid s))) (PRC y s, r)

(*  5 *)
| DO_SEQ_ACT_LEAVE : forall y s r,
  bigstep (PRC (SEQ (ACT LEAVE) y) s, r) (LABEL (LEAVE, (get_pid s))) (PRC y s, r)

(*  6 *)
| DO_SEQ_OK : forall y a s r,
  lts (PRC (SEQ OK y) s, r) a (PRC y s, r) ->
  bigstep (PRC (SEQ OK y) s, r) a (PRC y s, r)

(*  7 *)
| DO_REC_DEF : forall x b a u s1 r1 s2 r2,
  lts (PRC (REC_DEF x b) s1, r1) a (PRC u s2, r2) ->
  bigstep (PRC (REC_DEF x b) s1, r1) a (PRC u s2, r2)

(*  8 *)
| DO_PAR_L : forall a l1 l2 r gr1 gr2,
  bigstep (l1, gr1) a (l2, gr2) ->
  bigstep (PAR l1 r, gr1) a (PAR l2 r, gr2)

(*  9 *)
| DO_PAR_R : forall a l r1 r2 gr1 gr2,
  bigstep (r1, gr1) a (r2, gr2) ->
  bigstep (PAR l r1, gr1) a (PAR l r2, gr2)

(* 10 *)
| DO_OK_L : forall s r g, bigstep (PAR (PRC OK s) r, g) SILENT (r, g)

(* 11 *)
| DO_OK_R : forall s l g, bigstep (PAR l (PRC OK s), g) SILENT (l, g)

(* 12 *)

.

MeBi Dump "g1" LTS Bounded 16384 Of g1 Using bigstep lts step.

Example gTest1 : sys * resource :=
  (
    (PRC P (State.create 0))
  , Resource.initial 1
  ).
MeBi Dump "gTest1" LTS Bounded 16384 Of gTest1 Using bigstep lts step.

Example gTest2 : sys * resource :=
  (
    PAR (PRC P (State.create 0))
        (PRC OK (State.create 1))
  , Resource.initial 2
  ).
MeBi Dump "gTest2" LTS Bounded 16384 Of gTest2 Using bigstep lts step.

Example gTest2b : sys * resource :=
  (
    PAR (PRC P (State.create 0))
        (PRC (SEQ OK OK) (State.create 1))
  , Resource.initial 2
  ).
MeBi Dump "gTest2b" LTS Bounded 16384 Of gTest2b Using bigstep lts step.

Example gTest3 : sys * resource :=
  (
    PAR (PRC P (State.create 0))
        (PRC P (State.create 1))
  , Resource.initial 2
  ).
(* MeBi Dump "gTest3" LTS Bounded 16384 Of gTest3 Using bigstep lts step. *)


(* MeBi Dump "g2" LTS Bounded 16384 Of g2 Using bigstep lts step. *)
(* MeBi Show LTS Bounded 16384 Of g2 Using bigstep lts step. *)

(*
Example g3 : sys * resource := compose (create 3 P).
MeBi Dump "g3" LTS Bounded 16384 Of g3 Using bigstep lts step.

Example g4 : sys * resource := compose (create 4 P).
MeBi Dump "g4" LTS Bounded 16384 Of g4 Using bigstep lts step.

Example g5 : sys * resource := compose (create 5 P).
MeBi Dump "g5" LTS Bounded 16384 Of g5 Using bigstep lts step.

Example g6 : sys * resource := compose (create 6 P).
MeBi Dump "g6" LTS Bounded 16384 Of g6 Using bigstep lts step. *)

(*************************************************************************)
(**** Simpler Example ****************************************************)
(*************************************************************************)

(* Example LiteAcquire : tm :=
  REC_DEF 1 (
    SEQ (ACT READ_LOCKED) (
      IF (VAR LOCKED) (REC_CALL 1) (OK)
    )
  ).

Example LiteRelease : tm :=
  REC_DEF 2 (
    SEQ (ACT READ_LOCKED) (
      IF (VAR LOCKED) (REC_CALL 2) (OK)
    )
  ).

Example ListP : tm :=
  REC_DEF 0 (
    SEQ LiteAcquire (
      SEQ (ACT ENTER) (
        SEQ (ACT LEAVE) (
          SEQ LiteRelease (REC_CALL 0)
        )
      )
    )
  ). *)


(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(**** Below are unfinished tests/sketches. *******************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)
(*************************************************************************)



(*************************************************************************)
(**** TESTING: Coq-type for mebi input. **********************************)
(*************************************************************************)

(* Record mebi_input {A:Type} {B:Type} :=
  { input_lts  : A -> B -> A -> Prop
  ; input_term : A }. *)

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


