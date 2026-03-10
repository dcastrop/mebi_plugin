From Stdlib Require Export String.
From Stdlib Require Import PeanoNat.
Require Import Notations.
From Stdlib Require Import Bool.
From Stdlib Require Import Lists.List.
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

Definition idef : Type := nat.

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

(* we dont need new_i or j, only i*)
Definition lock : Type := index.

Module Lock.
  Definition initial : lock := Index.initial.
End Lock.

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

  Fixpoint create (n:nat) : list qnode :=
    match n with
    | 0    => []
    | S m => app (create m) [Qnode.initial]
    end.

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

Record mem :=
  { mem_next : index
  ; mem_locked : bool
  ; qnodes : list qnode
  }.

Module Memory.
  Definition create (n:nat) : mem := Build_mem Nil false (Qnode.create n).

  Definition get_mem_next   (m:mem) : index := mem_next m.
  Definition get_mem_locked (m:mem) : bool  := mem_locked m.
  Definition get_mem_qnodes (m:mem) : list qnode  := qnodes m.

  Definition set_mem_next   (i:index) (m:mem) : mem :=
    Build_mem i (get_mem_locked m) (get_mem_qnodes m).

  Definition set_mem_locked (b:bool)  (m:mem) : mem :=
    Build_mem (get_mem_next m) b (get_mem_qnodes m).

  Definition nth_error (i:nat) (m:mem)
  : option qnode := nth_error (qnodes m) i.

  Definition nth_default (d:qnode) (i:nat) (m:mem)
  : qnode := nth_default d (qnodes m) i.

  Fixpoint qnodes_nth_replace (r:qnode) (i:nat) (qs:list qnode)
  : option (list qnode) :=
    match i, qs with
    | _, [] => None
    | 0, h :: t => Some (r :: t)
    | S n, h :: t =>
      match qnodes_nth_replace r n t with
      | None => None
      | Some t' => Some (h :: t')
      end
    end.

  Definition nth_replace (r:qnode) (i:nat) (m:mem) : option mem :=
    match (qnodes_nth_replace r i (qnodes m)) with
    | None => None
    | Some qs => Some (Build_mem (mem_next m) (mem_locked m) qs)
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

Record error_info := { expr_info : option (expr * option (list value))
                     ; local_var_info : (option local_var * option value) }.

Record error := { error_code : nat
                ; error_msg  : string
                ; info       : error_info
                }.

(*************************************************************************)
(**** (Global) Resource **************************************************)
(*************************************************************************)

Definition resource : Type := mem * lock.

Module Resource.
  Definition initial (n:nat) : resource := (Memory.create n, Lock.initial).
End Resource.

Definition get_mem (r:resource) : mem :=
  match r with | (m, _) => m end.

Definition get_lock_i (r:resource) : index :=
  match r with | (_, i) => i end.

Definition set_mem (new_m:mem) (r:resource) : resource :=
  match r with | (_, l) => (new_m, l) end.

Definition set_lock_i (new_i:index) (r:resource) : resource :=
  match r with | (m, _) => (m, new_i) end.


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

Fixpoint eval (e:expr) (s:state) : (option value) * (option (list value)) :=
  match e with

  | VAL (GET THE_PID) => (Some (PID (get_pid s)), None)
  | VAL v => (Some v, None)

  | TRU => (Some (BOOL true), None)
  | FLS => (Some (BOOL false), None)

  | VAR v => (Some (get_local_var_value v s), None)

  | NOT b =>
    match eval b s with
    | (Some (BOOL b), errs) => (Some (BOOL (negb b)), errs)
    | (_, errs) => (None, errs)
    end

  | EQ a b =>
    match eval a s, eval b s with
    | (Some (NAT a), None),
      (Some (NAT b), None)   => (Some (BOOL (Nat.eqb a b)), None)

    | (Some (INDEX a), None),
      (Some (INDEX b), None) => (Some (BOOL (Index.eqb a b)), None)

    | (Some (PID a), None),
      (Some (PID b), None)   => (Some (BOOL (Nat.eqb a b)), None)

    | (Some (BOOL a), None),
      (Some (BOOL b), None)  => (Some (BOOL (eqb a b)), None)

    | (Some NIL, None), (Some NIL, None) => (Some (BOOL true), None)

    (* always propagate a None (i.e., an error) *)
    | (Some NIL, None), (None, None) => (None, None)
    | (None, None), (Some NIL, None) => (None, None)

    (* NOT NIL if other side isn't NIL and isn't None *)
    | (Some NIL, None), (_, None) => (Some (BOOL false), None)
    | (_, None), (Some NIL, None) => (Some (BOOL false), None)

    (* if PID vs INDEX *)
    | (Some (PID a), None),
      (Some (INDEX b), None) =>
                              (Some (BOOL (Index.eqb (Index.of_pid a) b)), None)

    | (Some (INDEX a), None),
      (Some (PID b), None) =>
                              (Some (BOOL (Index.eqb a (Index.of_pid b))), None)

    (* error has occurred *)
    | a, b =>
      let errs1:list value :=
        match a with
        | (Some a, Some errs) => a :: errs
        | (Some a, None) => [a]
        | (None, Some errs) => errs
        | (None, None) => []
        end in
      let errs2:list value :=
        match b with
        | (Some b, Some errs) => b :: errs
        | (Some b, None) => [b]
        | (None, Some errs) => errs
        | (None, None) => []
        end in
      (None, Some (app errs1 errs2))
    end

  end.

Definition handle_value (v:value) (s:state) : option value * option (list value) :=
  match v with
  | GET n => eval (VAR n) s
  | _ => (Some v, None)
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

(** [read_next e] will set [mem_next] and local var [next] to be the value
  * currently of the [qnode.next] corresponding to the [pid] of the process
  * in the state *)
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
    (* update local vars *)
    let v:local_vars := set_next qnode_next (get_vars s) in
    (* update mem_next *)
    let m:mem := Memory.set_mem_next qnode_next m in
    (* update env *)
    Some (set_vars v s, set_mem m r)
  end.

(** [read_locked e] will set [mem_locked] and local var [locked] to be the value
  * currently of the [qnode.locked] corresponding to the [pid] of the process
  *  in the state *)
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
    (* update local vars *)
    let v:local_vars := set_locked qnode_locked (get_vars s) in
    (* update mem_locked *)
    let m:mem := Memory.set_mem_locked qnode_locked m in
    (* update env *)
    Some (set_vars v s, set_mem m r)
  end.

(** [write_next to_write next e] will set the field [qnode.next] of the [qnode] 
  * of the index [to_write] to the value of [next]. *)
Definition write_next (to_write:local_var) (next:value) (e:env) : option env :=
  let s:state := get_state e in
  (* get value of [next] *)
  match handle_value next s with
  | (_, Some errs) => Some (add_error (Build_error 89 "write_next, value of NEXT failed" (Build_error_info None (None, Some next))) s, get_resource e)
  | (None, None) => None (* [next=GET v] and [v] could not be resolved *)
  | (Some next, None) =>
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

(** [write_locked to_write locked e] will set the  field [qnode.locked] of the 
  * [qnode] of the index [to_write] to the value of [locked]. *)
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
      | INDEX None => Some (add_error (Build_error 91 "write_locked, to_write yielded INDEX None" (Build_error_info None (Some to_write, Some locked))) s, get_resource e)
      | INDEX _ => Some (add_error (Build_error 92 "write_locked, to_write yielded INDEX _" (Build_error_info None (Some to_write, Some locked))) s, get_resource e)
      | NIL => Some (add_error (Build_error 93 "write_locked, to_write yielded NIL" (Build_error_info None (Some to_write, Some locked))) s, get_resource e)
      | PID _ => Some (add_error (Build_error 94 "write_locked, to_write yielded PID _" (Build_error_info None (Some to_write, Some locked))) s, get_resource e)
      | BOOL _ => Some (add_error (Build_error 95 "write_locked, to_write yielded BOOL _" (Build_error_info None (Some to_write, Some locked))) s, get_resource e)
      | _ => None (* [to_write] must be a [pid] *)
      end
  | Some p => (* get qnode *)
    let r:resource := get_resource e in
    let m:mem := get_mem r in
    match Memory.nth_error p m with
    | None => Some (add_error (Build_error 96 "write_locked, qnode does not exist" (Build_error_info None (Some to_write, Some locked))) s, get_resource e) (* [qnode] must exist *)
    | Some q => (* check [locked] of [qnode] is a [BOOL] *)
      match locked with
      | BOOL b =>
        match Memory.nth_replace (Qnode.set_locked b q) p m with
        | None => Some (add_error (Build_error 97 "write_locked, nth_replace failed" (Build_error_info None (Some to_write, Some locked))) s, get_resource e)
        | Some m => Some (s, (set_mem m r))
        end
      | _ => Some (add_error (Build_error 98 "write_locked, locked value not bool" (Build_error_info None (Some to_write, Some locked))) s, get_resource e) (* [locked] must be a [BOOL] *)
      end
    end
  end.

(** stores the current contents of [i] of the lock in the global resource [e] 
  * within the local var [predecessor], and sets [i] to contain the [pid]. *)
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

(** if [i] of the global resource lock is the same as [pid], then set [i] to 
  * [NIL] and set [swap] to [true]. Otherwise, set [swap] to [false] and leave 
  * [i] unchanged. *)
Definition compare_and_swap (e:env) : option env :=
  (* set [j] of lock (in global resource [e]) to [pid] *)
  let s:state := get_state e in
  let p:pid := get_pid s in
  (* get [i] of lock in global resource [e] *)
  let r:resource := get_resource e in
  let i:index := get_lock_i r in
  (* if the same, then set [swap] to true and set [i] to [Nil], else false *)
  let v:local_vars := get_vars s in
  match Index.eqb i (Index.of_pid p) with
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

Definition label : Type := act * pid.

Definition get_action (a:act) (e:env) : option label :=
  match a with
  | ENTER => Some (a, (get_pid (get_state e)))
  | LEAVE => Some (a, (get_pid (get_state e)))
  | NCS   => None
  | _     => None
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

  | REC_DEF other_idef b =>
    match new with
    | (new_idef, b') =>
      if Nat.eqb new_idef other_idef
        then unfold (new_idef, b) b (* overwrite and continue *)
        else REC_DEF new_idef (unfold new b)
    end

  | REC_CALL i =>
    match new with
    | (new_idef, b) =>
      if Nat.eqb new_idef i
        then (* unfold *) REC_DEF new_idef b
        else REC_CALL i  (* ignore, for some other def *)
    end

  end.

(*************************************************************************)
(**** Conditional branching **********************************************)
(*************************************************************************)

Definition take_branch (c:expr) (t1:tm) (t2:tm) (e:env) : tm * env :=
  match eval c (get_state e) with
  | (Some (BOOL true), None) => (t1, e)
  | (Some (BOOL false), None) => (t2, e)
  (** NOTE: the below are for detecting errors *)
  | (Some other, None) => (ERR, ((add_error (Build_error 1 "conditional statement evaluated to non-bool type" (Build_error_info (Some (c, Some [other])) (None, None))) (get_state e)), get_resource e))

  | (Some other, Some errs) => (ERR, ((add_error (Build_error 2 "conditional statement evaluated to non-bool type with caused error" (Build_error_info (Some (c, Some (other :: errs))) (None, None))) (get_state e)), get_resource e))

  | (None, None) => (ERR, ((add_error (Build_error 3 "conditional statement could not be evaluated" (Build_error_info (Some (c, None)) (None, None))) (get_state e)), get_resource e))

  | (None, Some errs) => (ERR, ((add_error (Build_error 4 "conditional statement could not be evaluated with error" (Build_error_info (Some (c, Some (errs))) (None, None))) (get_state e)), get_resource e))
  end.



(*************************************************************************)
(**** Semantics (Local step) *********************************************)
(*************************************************************************)

Reserved Notation "t '--<{' a '}>-->' t'" (at level 40).

Inductive step : (tm * env) -> option label -> (tm * env) -> Prop :=
| STEP_ACT : forall a e, (ACT a, e) --<{get_action a e}>--> (do_act a e)

| STEP_SEQ_END : forall r e, (SEQ OK r, e) --<{None}>--> (r, e)

| STEP_SEQ : forall a l1 l2 r e1 e2,
  (l1, e1) --<{a}>--> (l2, e2) ->
  (SEQ l1 r, e1) --<{a}>--> (SEQ l2 r, e2)

| STEP_IF : forall c t1 t2 e,
  (IF c t1 t2, e) --<{None}>--> (take_branch c t1 t2 e)

| STEP_REC_DEF : forall i b e,
  (REC_DEF i b, e) --<{None}>--> (unfold (i, b) b, e)

where "t '--<{' a '}>-->' t'" := (step t a t').

(* Compute (get_action (WRITE_NEXT THE_PID NIL) (0, {| var_predecessor := None; var_locked := false; var_next := None; var_swap := false |}, None, ({| mem_next := None; mem_locked := false; qnodes := {| next := None; locked := false |} :: nil |}, None))). *)

(* Compute (do_act (WRITE_NEXT THE_PID NIL) (0, {| var_predecessor := None; var_locked := false; var_next := None; var_swap := false |}, None, ({| mem_next := None; mem_locked := false; qnodes := {| next := None; locked := false |} :: nil |}, None))). *)

(*************************************************************************)
(**** Semantics (System lts) *********************************************)
(*************************************************************************)

Inductive sys : Type :=
  | PRC : tm -> state -> sys
  | PAR : sys -> sys -> sys
  .

Definition composition : Type := sys * resource.

Reserved Notation "t '==<{' a '}>==>' t'" (at level 40).

Inductive lts : composition -> option label -> composition -> Prop :=
| LTS_PRC : forall a t1 t2 s1 s2 r1 r2,
  (t1, (s1, r1)) --<{a}>--> (t2, (s2, r2)) ->
  (PRC t1 s1, r1) ==<{a}>==> (PRC t2 s2, r2)

| LTS_PAR_L : forall a l1 l2 r gr1 gr2,
  (l1, gr1) ==<{a}>==> (l2, gr2) ->
  (PAR l1 r, gr1) ==<{a}>==> (PAR l2 r, gr2)

| LTS_PAR_R : forall a l r1 r2 gr1 gr2,
  (r1, gr1) ==<{a}>==> (r2, gr2) ->
  (PAR l r1, gr1) ==<{a}>==> (PAR l r2, gr2)
(** NOTE: the transitions below stop the term from growing in size, 
  * but instead cause the state-space to grow far bigger. *)
(* | LTS_OK_L : forall s r g, (PAR (PRC OK s) r, g) ==<{SILENT}>==> (r, g) *)

(* | LTS_OK_R : forall l s g, (PAR l (PRC OK s), g) ==<{SILENT}>==> (l, g) *)

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

Definition compose (s:system) : composition :=
  match s with
  | (ps, r) => (load ps, r)
  end.


Module Protocol.

  Import CADP.Expr.

Example AcquireInnerDef : idef := 2.
Example AcquireInnerBody : tm :=
  SEQ 
    (ACT READ_LOCKED) 
    (IF (VAR LOCKED) 
      (REC_CALL (AcquireInnerDef)) 
      (OK)).

Example AcquireInnerLoop : tm := 
  REC_DEF (AcquireInnerDef) (AcquireInnerBody).

Example Acquire : tm :=
  SEQ 
    (ACT (WRITE_NEXT THE_PID NIL)) 
    (SEQ 
      (ACT FETCH_AND_STORE) 
      (IF (EQ (VAR PREDECESSOR) (VAL NIL)) 
        (OK) 
        (SEQ 
          (ACT (WRITE_LOCKED THE_PID (BOOL true))) 
          (SEQ 
            (ACT (WRITE_NEXT PREDECESSOR (GET THE_PID))) 
            (AcquireInnerLoop))))).

Example ReleaseInnerDef : idef := 1.
Example ReleaseInnerBody : tm :=
  SEQ 
    (ACT READ_NEXT) 
    (IF (EQ (VAL NIL) (VAR NEXT))
      (REC_CALL (ReleaseInnerDef))
      (ACT (WRITE_LOCKED NEXT (BOOL false)))).

Example ReleaseInnerLoop : tm := 
  REC_DEF (ReleaseInnerDef) (ReleaseInnerBody).

Example Release : tm :=
  SEQ 
    (ACT READ_NEXT) 
    (IF (EQ (VAL NIL) (VAR NEXT)) 
      (SEQ 
        (ACT COMPARE_AND_SWAP) 
        (IF (EQ FLS (VAR SWAP)) 
          (ReleaseInnerLoop) 
          (OK))) 
      (ACT (WRITE_LOCKED NEXT (BOOL false)))).

Example PMainLoopDef : idef := 0.
Example P : tm :=
  REC_DEF (PMainLoopDef) (
    (* SEQ (ACT NCS) ( *)
    SEQ (Acquire) (
    SEQ (ACT ENTER) (
    SEQ (ACT LEAVE) (
    SEQ (Release) (
    REC_CALL (PMainLoopDef)))))(* ) *)).


End Protocol.

(*************************************************************************)
(**** Tests: Lock ********************************************************)
(*************************************************************************)

(*************************)
(**** FETCH_AND_STORE ****)
(*************************)

Example Lock_FaS_1 : composition :=
  compose (create 2 (
      REC_DEF 0 (
        SEQ (ACT FETCH_AND_STORE) (
          (* end when value in fetch-and-store is our own PID *)
          IF (EQ (VAR PREDECESSOR) (VAL (GET THE_PID))) (OK) (REC_CALL 0)
        )
      )
  )).
(* MeBi Dump "Lock_FaS_1" LTS Bounded 16384 Of Lock_FaS_1 Using lts step. *)

Example Lock_FaS_2 : composition :=
  compose (create 2 (
      REC_DEF 0 (
        SEQ (ACT FETCH_AND_STORE) (
          (* only fetch-and-store once PID has stopped being your own PID *)
          IF (EQ (VAR PREDECESSOR) (VAL (GET THE_PID))) (
            (* busy wait for fetch-and-store to not be your own PID*)
            REC_DEF 1 (
              SEQ (ACT FETCH_AND_STORE) (
                IF (EQ (VAR PREDECESSOR) (VAL (GET THE_PID)))
                  (REC_CALL 1)
                  (REC_CALL 0)
              )
            )
          ) (REC_CALL 0)
        )
      )
  )).
(* MeBi Dump "Lock_FaS_2" LTS Bounded 16384 Of Lock_FaS_2 Using lts step. *)

(*******************************************)
(**** FETCH_AND_STORE & COMPARE_AND_SWAP ***)
(*******************************************)

Example Lock_CaS_1 : composition :=
  compose (create 2 (
      REC_DEF 0 (
        SEQ (ACT COMPARE_AND_SWAP) (
          (* end if value already in fetch-and-store is our own PID *)
          IF (EQ (VAR PREDECESSOR) (VAL (GET THE_PID))) (OK) (
            SEQ (ACT FETCH_AND_STORE) (
              (* if swap then lock [i] contained our own PID, end *)
              IF (VAR SWAP) (OK) (REC_CALL 0)
            )
          )
        )
      )
  )).
(* MeBi Dump "Lock_CaS_1" LTS Bounded 16384 Of Lock_CaS_1 Using lts step. *)


