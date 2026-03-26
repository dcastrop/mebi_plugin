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
Definition lock_initial : lock := Index.initial.

(*************************************************************************)
(**** Memory *************************************************************)
(*************************************************************************)

(**********************************)
(**** Qnode ***********************)
(**********************************)

Record qnode :=
  { next   : index
  ; locked : bool }.

Definition qnode_initial : qnode := Build_qnode Nil false.
Definition qnode_create : qnode := qnode_initial.
Definition qnode_set_next (i:index) (q:qnode) : qnode := Build_qnode i (locked q).
Definition qnode_set_locked (b:bool) (q:qnode) : qnode := Build_qnode (next q) b.

Definition qnodes : Type := list qnode.
Fixpoint qnodes_create (n:nat) : qnodes := 
match n with
| 0    => [qnode_create]
| S m => qnode_create :: (qnodes_create m)
end.

(**********************************)
(**** mem (Memory) Type ***********)
(**********************************)

Record mem :=
  { mem_next : index
  ; mem_locked : bool
  ; mem_qnodes : qnodes
  }.

Definition mem_create (n:nat) : mem := Build_mem Nil false (qnodes_create n).
Definition mem_set_next (i:index) (m:mem) : mem := Build_mem i (mem_locked m) (mem_qnodes m).
Definition mem_set_locked (b:bool) (m:mem) : mem := Build_mem (mem_next m) b (mem_qnodes m).
Definition mem_set_qnodes (q:qnodes) (m:mem) : mem := Build_mem (mem_next m) (mem_locked m) q.

Module Memory.
  Definition nth_error (i:nat) (m:mem) : option qnode := nth_error (mem_qnodes m) i.

  Fixpoint qnodes_nth_replace (r:qnode) (i:nat) (qs:list qnode) : option (list qnode) :=
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
  match (qnodes_nth_replace r i (mem_qnodes m)) with
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
| THE_PID.

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
| NIL : value
| NAT : nat -> value
| PID : pid -> value
| INDEX : index -> value
| BOOL : bool -> value
| GET : local_var -> value.
  
(*************************************************************************)
(**** (Conditional) Expressions ******************************************)
(*************************************************************************)

Inductive expr : Type :=
| TRU : expr
| FLS : expr
| NOT : expr -> expr
| EQ : expr -> expr -> expr
| VAR : local_var -> expr
| VAL : value -> expr.

Definition expr_of_bool (b:bool) : expr := if b then TRU else FLS.

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
Definition resource_create (n:nat) : resource := (mem_create n, lock_initial).
Definition resource_set_mem (m:mem) (r:resource) : resource := (m, snd r).
Definition resource_set_lock (l:lock) (r:resource) : resource := (fst r, l).
Definition resource_get_mem (r:resource) : mem := fst r.
Definition resource_get_lock (r:resource) : index := snd r.

(*************************************************************************)
(**** (Local) State ******************************************************)
(*************************************************************************)

Definition state : Type := pid * local_vars * (option (list error)).
Definition state_create (p:pid) : state := (p, Vars.initial, None).

Definition get_pid (s:state) : pid := match s with | (p, _, _) => p end.
Definition get_vars (s:state) : local_vars := match s with | (_, v, _) => v end.
Definition get_errors (s:state) : option (list error) := match s with | (_, _, e) => e end.

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

Definition value_to_index_opt (v:value) : option index :=
  match v with
  | NAT n => Some (Some n)
  | PID p => Some (Index.of_pid p)
  | INDEX i => Some i
  | NIL => Some Nil
  | _ => None (* i.e., BOOL *)
  end.

Definition local_var_to_index_opt (x:local_var) (s:state) : option index := 
  value_to_index_opt (get_local_var_value x s).

Definition local_var_to_pid_opt (x:local_var) (s:state) : option pid := 
  match local_var_to_index_opt x s with
  | None => None
  | Some None => None
  | Some (Some n) => Some n
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
| FETCH_AND_STORE  : act
| COMPARE_AND_SWAP : act
| WRITE_NEXT   : local_var -> value -> act
| WRITE_LOCKED : local_var -> value -> act.


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
Definition env_create (n:nat) : env := (state_create n, resource_create n).

Definition get_state (e:env) : state := fst e.
Definition get_resource (e:env) : resource := snd e.



(*************************************************************************)
(**** (Local) Semantics of Processes *************************************)
(*************************************************************************)

(** stores the current contents of [i] of the lock in the global resource [e] 
  * within the local var [predecessor], and sets [i] to contain the [pid]. *)
Definition fetch_and_store (e:env) : env :=
  (* get [i] from lock in global resource [e] *)
  let r:resource := get_resource e in
  let i:index := resource_get_lock r in
  (* store [i] in local var [predecessor] *)
  let s:state := get_state e in
  let v:local_vars := get_vars s in
  let v:local_vars := set_predecessor i v in
  let s:state := set_vars v s in
  (* set [i] of lock in global resource to [pid] *)
  let p:pid := get_pid s in
  let r:resource := resource_set_lock (Index.of_pid p) r in
  (* update env *)
  (s, r).

(** if [i] of the global resource lock is the same as [pid], then set [i] to 
  * [NIL] and set [swap] to [true]. Otherwise, set [swap] to [false] and leave 
  * [i] unchanged. *)
Definition compare_and_swap (e:env) : env :=
  (* set [j] of lock (in global resource [e]) to [pid] *)
  let s:state := get_state e in
  let p:pid := get_pid s in
  (* get [i] of lock in global resource [e] *)
  let r:resource := get_resource e in
  let i:index := resource_get_lock r in
  (* if the same, then set [swap] to true and set [i] to [Nil], else false *)
  let v:local_vars := get_vars s in
  match Index.eqb i (Index.of_pid p) with
  | true =>
    (* set [swap] true *)
    let v:local_vars := set_swap true v in
    let s:state := set_vars v s in
    (* set [i] to [Nil] *)
    let r:resource := resource_set_lock Nil r in
    (* update env *)
    (s, r)
  | false =>
    (* set [swap] false *)
    let v:local_vars := set_swap false v in
    let s:state := set_vars v s in
    (* update env *)
    (s, r)
  end.

Definition return_error (n:nat) (s:string) (e:env) (a:option local_var) (b:option value) : option env :=
Some (add_error (Build_error n s (Build_error_info None (a, b))) (get_state e), get_resource e).

(** [read_next e] will set [mem_next] and local var [next] to be the value
  * currently of the [qnode.next] corresponding to the [pid] of the process
  * in the state *)
Definition read_next (e:env) : option env :=
(* get qnode *)
match Memory.nth_error (get_pid (get_state e)) (resource_get_mem (get_resource e)) with
| None => return_error 10 "read_next, memory out of bounds" e None None
| Some q => (* get next of qnode *)
  let qnode_next:index := next q in
  (* update local vars *)
  let v:local_vars := set_next qnode_next (get_vars (get_state e)) in
  (* update mem_next *)
  let m:mem := mem_set_next qnode_next (resource_get_mem (get_resource e)) in
  (* update env *)
  Some (set_vars v (get_state e), resource_set_mem m (get_resource e))
end.

(** [read_locked e] will set [mem_locked] and local var [locked] to be the value
  * currently of the [qnode.locked] corresponding to the [pid] of the process
  *  in the state *)
Definition read_locked (e:env) : option env :=
(* get qnode *)
match Memory.nth_error (get_pid (get_state e)) (resource_get_mem (get_resource e)) with
| None => return_error 10 "read_locked, memory out of bounds" e None None
| Some q => (* get locked of qnode *)
  let qnode_locked:bool := locked q in
  (* update local vars *)
  let v:local_vars := set_locked qnode_locked (get_vars (get_state e)) in
  (* update mem_locked *)
  let m:mem := mem_set_locked qnode_locked (resource_get_mem (get_resource e)) in
  (* update env *)
  Some (set_vars v (get_state e), resource_set_mem m (get_resource e))
end.


(** [write_next to_write next e] will set the field [qnode.next] of the [qnode] 
  * of the index [to_write] to the value of [next]. *)
Definition write_next (to_write:local_var) (next:value) (e:env) : option env :=
(* get value of [next] *)
match handle_value next (get_state e) with
| (_, Some errs) => return_error 89 "write_next, value of NEXT failed" e (Some to_write) (Some next)
| (None, None) => return_error 88 "write_next, value of NEXT could not resolve GET" e (Some to_write) (Some next)
| (Some next, None) => (* get pid [to_write] *)
  match local_var_to_pid_opt to_write (get_state e) with
  | None => return_error 87 "write_next, value of TO_WRITE is not pid" e (Some to_write) (Some next)
  | Some p => (* get qnode *)
    match Memory.nth_error p (resource_get_mem (get_resource e)) with
    | None => return_error 10 "write_next, memory out of bounds" e (Some to_write) (Some next)
    | Some q => (* check [next] of [qnode] is some kind of nat *)
      match value_to_index_opt next with
      | None => return_error 86 "write_next, value of NEXT is not pid" e (Some to_write) (Some next)
      | Some i => (* replace next of q with i *)
        match Memory.nth_replace (qnode_set_next i q) p (resource_get_mem (get_resource e)) with
        | None => return_error 10 "write_next, replace memory out of bounds" e (Some to_write) (Some next)
        | Some m => Some (get_state e, resource_set_mem m (get_resource e))
        end
      end
    end
  end
end.

Definition value_to_bool_opt (x:value) : option bool :=
match x with
| BOOL b => Some b
| _ => None
end.

(** [write_locked to_write locked e] will set the  field [qnode.locked] of the 
  * [qnode] of the index [to_write] to the value of [locked]. *)
Definition write_locked (to_write:local_var) (locked:value) (e:env) : option env :=
(* get value of [locked] *)
match handle_value locked (get_state e) with
| (_, Some errs) => return_error 99 "write_locked, value of NEXT failed" e (Some to_write) (Some locked)
| (None, None) => return_error 98 "write_locked, value of NEXT could not resolve GET" e (Some to_write) (Some locked)
| (Some locked, None) => (* get pid from [to_write] *)
  match local_var_to_pid_opt to_write (get_state e) with
  | None => return_error 97 "write_locked, value of TO_WRITE is not pid" e (Some to_write) (Some locked)
  | Some p => (* get qnode *)
    match Memory.nth_error p (resource_get_mem (get_resource e)) with
    | None => return_error 10 "write_locked, memory out of bounds" e (Some to_write) (Some locked)
    | Some q => (* check [locked] of [qnode] is a [BOOL] *)
      match value_to_bool_opt locked with
      | None => return_error 98 "write_locked, locked value not bool" e (Some to_write) (Some locked) (* [locked] must be a [BOOL] *)
      | Some b =>
        match Memory.nth_replace (qnode_set_locked b q) p (resource_get_mem (get_resource e)) with
        | None => return_error 10 "write_locked, replace memory out of bounds" e (Some to_write) (Some locked)
        | Some m => Some (get_state e, resource_set_mem m (get_resource e))
        end
      end
    end
  end
end.

(*************************************************************************)
(**** Actions ************************************************************)
(*************************************************************************)

Definition handle_env_opt (e:env) (r:option env) : (tm * env) :=
match r with 
| None => (ERR, e)
| Some e => (if has_error_occurred (get_state e) then ERR else OK, e)
end.

Definition do_act (a:act) (e:env) : (tm * env) :=
  match a with
  | NCS   => (OK, e)
  | ENTER => (OK, e)
  | LEAVE => (OK, e)
  | FETCH_AND_STORE => (OK, fetch_and_store e)
  | COMPARE_AND_SWAP => (OK, compare_and_swap e)

  | READ_NEXT   => handle_env_opt e (read_next e)
  | READ_LOCKED => handle_env_opt e (read_locked e)

  | WRITE_NEXT to_write next   => handle_env_opt e (write_next to_write next e)
  | WRITE_LOCKED to_write next => handle_env_opt e (write_locked to_write next e)
  end.

Definition label : Type := act * pid.

Definition get_action (a:act) (e:env) : option label :=
  match a with
  | ENTER => Some (a, (get_pid (get_state e)))
  | LEAVE => Some (a, (get_pid (get_state e)))
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
  | (Some v, _errs_opt) => 
    match value_to_bool_opt v with
    | None => (ERR, ((add_error (Build_error 1 "conditional statement evaluated to non-bool type" (Build_error_info (Some (c, _errs_opt)) (None, Some v))) (get_state e)), get_resource e))
    | Some true => (t1, e)
    | Some false => (t2, e)
    end
  | (None, errs) => (ERR, ((add_error (Build_error 3 "conditional statement could not be evaluated" (Build_error_info (Some (c, errs)) (None, None))) (get_state e)), get_resource e))
  end.



(*************************************************************************)
(**** Semantics (Local step) *********************************************)
(*************************************************************************)

Definition process : Type := tm * env.
Definition process_create (n:nat) (x:tm) : process := (x, (env_create n)).

Inductive step : process -> option label -> process -> Prop :=
| STEP_ACT : forall a e, step (ACT a, e) (get_action a e) (do_act a e)
| STEP_IF : forall c t1 t2 e, step (IF c t1 t2, e) None (take_branch c t1 t2 e)
| STEP_REC_DEF : forall i b e, step (REC_DEF i b, e) None (unfold (i, b) b, e)
| STEP_SEQ_END : forall r e, step (SEQ OK r, e) None (r, e)
| STEP_SEQ : forall a l1 l2 r e1 e2, 
  step (l1, e1) a (l2, e2) ->
  step (SEQ l1 r, e1) a (SEQ l2 r, e2).

(*************************************************************************)
(**** Semantics (System lts) *********************************************)
(*************************************************************************)

Definition worker : Type := tm * state.
Definition worker_create (n:nat) (x:tm) : worker := (x, (state_create n)).

Inductive sys : Type :=
| PRC : worker -> sys
| PAR : sys -> sys -> sys.

Fixpoint sys_create (n:nat) (x:tm) : sys :=
match n with
| 0 => PRC (worker_create n x)
| S m => PAR (PRC (worker_create n x)) (sys_create m x)
end.

Definition composition : Type := sys * resource.
Definition composition_create (n:nat) (x:tm) : composition := (sys_create n x, resource_create n).

Inductive lts : composition -> option label -> composition -> Prop :=
| LTS_PRC : forall a t1 t2 s1 s2 r1 r2,
  step (t1, (s1, r1)) a (t2, (s2, r2)) ->
  lts (PRC (t1, s1), r1) a (PRC (t2, s2), r2)
| LTS_PAR_L : forall a l1 l2 r gr1 gr2,
  lts (l1, gr1) a (l2, gr2) ->
  lts (PAR l1 r, gr1) a (PAR l2 r, gr2)
| LTS_PAR_R : forall a l r1 r2 gr1 gr2,
  lts (r1, gr1) a (r2, gr2) ->
  lts (PAR l r1, gr1) a (PAR l r2, gr2).

Module Protocol.

Example AcquireInnerDef : idef := 2.
Example AcquireInnerBody : tm :=
  SEQ (ACT READ_LOCKED) 
      (IF (VAR LOCKED) 
          (REC_CALL (AcquireInnerDef)) 
          (OK)).

Example AcquireInnerLoop : tm := REC_DEF (AcquireInnerDef) (AcquireInnerBody).

Example Acquire : tm :=
  SEQ (ACT (WRITE_NEXT THE_PID NIL)) 
      (SEQ (ACT FETCH_AND_STORE) 
           (IF (EQ (VAR PREDECESSOR) (VAL NIL)) 
               (OK) 
               (SEQ (ACT (WRITE_LOCKED THE_PID (BOOL true))) 
                    (SEQ (ACT (WRITE_NEXT PREDECESSOR (GET THE_PID))) 
                         (AcquireInnerLoop))))).

Example ReleaseInnerDef : idef := 1.
Example ReleaseInnerBody : tm :=
  SEQ (ACT READ_NEXT) 
      (IF (EQ (VAL NIL) (VAR NEXT))
          (REC_CALL (ReleaseInnerDef))
          (ACT (WRITE_LOCKED NEXT (BOOL false)))).

Example ReleaseInnerLoop : tm := REC_DEF (ReleaseInnerDef) (ReleaseInnerBody).

Example Release : tm :=
  SEQ (ACT READ_NEXT) 
      (IF (EQ (VAL NIL) (VAR NEXT)) 
          (SEQ (ACT COMPARE_AND_SWAP) 
               (IF (EQ FLS (VAR SWAP)) 
                   (ReleaseInnerLoop) 
                   (OK))) 
          (ACT (WRITE_LOCKED NEXT (BOOL false)))).

Example PMainLoopDef : idef := 0.
Example P : tm :=
  REC_DEF (PMainLoopDef) 
          (SEQ (Acquire) 
               (SEQ (ACT ENTER) 
                    (SEQ (ACT LEAVE) 
                         (SEQ (Release) 
                              (REC_CALL (PMainLoopDef)))))).


End Protocol.

(*************************************************************************)
(**** Tests: Lock ********************************************************)
(*************************************************************************)

(*************************)
(**** FETCH_AND_STORE ****)
(*************************)

Example Lock_FaS_1 : composition :=
  composition_create 1 (
      REC_DEF 0 (
        SEQ (ACT FETCH_AND_STORE) (
          (* end when value in fetch-and-store is our own PID *)
          IF (EQ (VAR PREDECESSOR) (VAL (GET THE_PID))) (OK) (REC_CALL 0)
        )
      )
  ).
(* MeBi Dump "Lock_FaS_1" LTS Bounded 16384 Of Lock_FaS_1 Using lts step. *)

Example Lock_FaS_2 : composition :=
  composition_create 1 (
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
  ).
(* MeBi Dump "Lock_FaS_2" LTS Bounded 16384 Of Lock_FaS_2 Using lts step. *)

(*******************************************)
(**** FETCH_AND_STORE & COMPARE_AND_SWAP ***)
(*******************************************)

Example Lock_CaS_1 : composition :=
  composition_create 1 (
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
  ).
(* MeBi Dump "Lock_CaS_1" LTS Bounded 16384 Of Lock_CaS_1 Using lts step. *)


