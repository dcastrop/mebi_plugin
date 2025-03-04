Require Import MEBI.loader.

Require Export String.
Require Import PeanoNat.
Require Import Notations.
Require Export Bool.


Definition index : Type := option nat.
Definition pid   : Type := nat.

Definition ibool : Type := nat.
Definition iloop : Type := nat.

Definition Nil   : index := None.


(* we dont need new_i, only i and j *)
Definition lock : Type := index * index.

Module Index.

  Definition Initial : index := Nil.

  Definition of_pid (i : pid) : index := Some i.


End Index.



Module IBool.

  Definition of_index (i:index) : option bool :=
  match i with
  | None => None
  | Some n =>
    match n with
    | 0 => Some false
    | 1 => Some true
    | _ => None
    end
  end.

  Definition of_bool (b:bool) : ibool := if b then 1 else 0.

  Definition is_some (b:option ibool) : bool :=
    match b with
    | None => false
    | _    => true
    end.

End IBool.




Module Memory.


  (**********************************)
  (**** Qnode ***********************)
  (**********************************)

  Record qnode :=
    { next   : index
    ; locked : ibool }.

  Module Qnode.

    Definition initial : qnode := Build_qnode None 0.

    (* redundant ? *)
    Module _Get.

      Definition next   (q:qnode) : index := (next q).
      Definition locked (q:qnode) : ibool := (locked q).

    End _Get.


    Module _Set.

      Definition next   (i:index) (q:qnode) : qnode := Build_qnode i (_Get.locked q).
      Definition locked (b:ibool) (q:qnode) : qnode := Build_qnode (_Get.next q) b.

    End _Set.


  End Qnode.


  (**********************************)
  (**** mem Type, notation & app ****)
  (**********************************)

  Definition mem : Type := list qnode.

  Notation "x :: l" := (cons x l) (at level 60, right associativity).
  Notation "[ ]" := nil.
  Notation "[ x ; .. ; y ]" := (cons x .. (cons y nil) ..).

  Fixpoint app (l1 l2 : mem) : mem :=
    match l1 with
    | nil    => l2
    | h :: t => h :: (app t l2)
    end.

  (**********************************)
  (**** Creating & Initializing *****)
  (**********************************)

  Fixpoint create (n:nat) : mem :=
    match n with
    | 0    => []
    | S n' => app (create n') [Qnode.initial]
    end.

  Definition initial (n:nat) : mem := create n.

  (**********************************)
  (**** helpful funs/definitions ****)
  (**********************************)

  Fixpoint length_of (m:mem) : nat :=
    match m with
    | [] => 0
    | h :: t => S (length_of t)
    end.

  Fixpoint add_n_qnodes (n:nat) (m:mem) :=
    match n with
      | O  => m
      | S n' => Qnode.initial::(add_n_qnodes n' m)
    end.

  (* pads mem so that index i exists *)
  Definition pad (i:nat) (m:mem) : mem :=
    let length_of_m:nat := length_of m in
    if (Nat.ltb length_of_m i)
    then (add_n_qnodes (i - length_of_m) m)
    else m.


  Module _Get.

    Fixpoint index_of (i:nat) (m:mem) : option qnode :=
      match m with
      | []     => None
      | h :: t =>
        let length_of_m:nat := length_of m in
        match i, length_of_m with
        | _, 0 => None
        | _, _ => if (Nat.eqb i length_of_m)
                  then Some h
                  else index_of i t
        end
      end.

    Definition locked_of (i:index) (m:mem) : option ibool :=
      match i with
      | None => None
      | Some n =>
        match (index_of n m) with
        | None => None
        | Some b => Some (locked b)
        end
      end.

    Definition next_of (i:index) (m:mem) : index :=
      match i with
        | None => None
        | Some n =>
        match (index_of n m) with
        | None => None
        | Some b => (next b)
        end
      end.

  End _Get.


  Module _Set.

    Fixpoint index_of (i:nat) (q:qnode) (m:mem) : option mem :=
      match m with
      | []     => None
      | h :: t =>
        let length_of_m:nat := length_of m in
        match i, length_of_m with
        | _, 0 => None
        | _, _ =>
          if (Nat.eqb i length_of_m) then Some (q :: t)
          else match (index_of i q t) with
               | None   => None
               | Some l => Some (h :: l)
               end
        end
      end.

    Definition locked_of (i:index) (l:ibool) (m:mem) : option mem :=
      match i with
        | None => None
        | Some n =>
        match _Get.index_of n m with
        | None       => None
        | Some qnode => index_of n (Qnode._Set.locked l qnode) m
        end
      end.

    Definition next_of (i:index) (n:index) (m:mem) : option mem :=
      match i with
      | None => None
      | Some j =>
        match _Get.index_of j m with
        | None       => None
        | Some qnode => index_of j (Qnode._Set.next n qnode) m
        end
      end.

  End _Set.


End Memory.



Module Vars.

  Record vars :=
  { var_predecessor : index        (* acquire *)
  ; var_locked      : option ibool (* acquire *)
  ; var_next        : index        (* release *)
  ; var_swap        : option ibool (* release *) }.

  Inductive var : Type :=
    | PREDECESSOR
    | LOCKED
    | NEXT
    | SWAP
    (* | PID *)
    (* | NIL *)
    .

  Inductive val : Type :=
  | INDEX : index        -> val (* not yet used *)
  | IBOOL : option ibool -> val (* not yet used *)
  | VAR : var -> val
  | TRU : val
  | FLS : val
  | NIL : val
  .


  Module _Get.

    Definition predecessor (v:vars) : index := var_predecessor v.

    Definition locked (v:vars) : option ibool := var_locked v.

    Definition next (v:vars) : index := var_next v.

    Definition swap (v:vars) : option ibool := var_swap v.

  End _Get.


  Module _Set.

    Definition predecessor (i:index) (v:vars) : vars :=
      Build_vars i (_Get.locked v) (_Get.next v) (_Get.swap v).

    Definition locked (b:ibool) (v:vars) : vars :=
      Build_vars (_Get.predecessor v) (Some b) (_Get.next v) (_Get.swap v).

    Definition next (i:index) (v:vars) : vars :=
      Build_vars (_Get.predecessor v) (_Get.locked v) i (_Get.swap v).

    Definition swap (b:ibool) (v:vars) : vars :=
      Build_vars (_Get.predecessor v) (_Get.locked v) (_Get.next v) (Some b).

  End _Set.

End Vars.



Module State.

  Definition local : Type := pid * Vars.vars.

  Definition resource : Type := Memory.mem * lock.

  Definition global : Type := (list local) * resource.


  Module _Local.

    Module _Get.

      Definition pid (s:local) : pid :=
        match s with
        | (pid, vars) => pid
        end.

      Definition vars (s:local) : Vars.vars :=
        match s with
        | (pid, vars) => vars
        end.


      Module Var.

        Definition predecessor (s:local) : index := Vars._Get.predecessor (vars s).

        Definition locked (s:local) : option ibool := Vars._Get.locked (vars s).

        Definition next (s:local) : index := Vars._Get.next (vars s).

        Definition swap (s:local) : option ibool := Vars._Get.swap (vars s).

      End Var.

      Definition var (v:Vars.var) (s:local) : option nat :=
        match v with
        | Vars.PREDECESSOR => Var.predecessor s
        | Vars.LOCKED      => Var.locked s
        | Vars.NEXT        => Var.next s
        | Vars.SWAP        => Var.swap s
        (* | PID         => Some (pid s) *)
        (* | NIL         => None *)
        end.

      Definition val (v:Vars.val) (s:local) : option nat :=
        match v with
        | Vars.INDEX i => i
        | Vars.IBOOL b => b
        | Vars.VAR v   => var v s
        | Vars.TRU     => Some 1
        | Vars.FLS     => Some 0
        | Vars.NIL     => Nil
        end.

    End _Get.


    Module _Set.


      Module Var.

        Definition predecessor (i:index) (s:local) : local :=
          match s with
          | (pid, vars) => (pid, Vars._Set.predecessor i vars)
          end.

        Definition locked (b:ibool) (s:local) : local :=
        match s with
        | (pid, vars) => (pid, Vars._Set.locked b vars)
        end.

        Definition next (i:index) (s:local) : local :=
          match s with
          | (pid, vars) => (pid, Vars._Set.next i vars)
          end.

        Definition swap (b:ibool) (s:local) : local :=
        match s with
        | (pid, vars) => (pid, Vars._Set.swap b vars)
        end.

      End Var.

    End _Set.

  End _Local.


  Module _Resource.


    Module _Get.

      Definition mem (s:resource) : Memory.mem :=
        match s with
        | (mem, lock) => mem
        end.

      Definition lock (s:resource) : lock :=
        match s with
        | (mem, lock) => lock
        end.


      Module Lock.

        Definition i (s:resource) : index :=
          match (lock s) with
          | (i, j) => i
          end.

        Definition j (s:resource) : index :=
          match (lock s) with
          | (i, j) => j
          end.

      End Lock.

    End _Get.


    Module _Set.


      Module Lock.

        Definition i (new_i:index) (s:resource) : resource :=
          match (_Get.lock s) with
          | (i, j) => (_Get.mem s, (new_i, j))
          end.

        Definition j (new_j:index) (s:resource) : resource :=
          match (_Get.lock s) with
          | (i, j) => (_Get.mem s, (i, new_j))
          end.

      End Lock.

    End _Set.

  End _Resource.


  Module _Global.


    Module _Get.

      Definition mem (s:global) : Memory.mem :=
        match s with
        | (pids, resource) =>
          match resource with
          | (mem, lock) => mem
          end
        end.

      Definition lock (s:global) : lock :=
        match s with
        | (pids, resource) =>
          match resource with
          | (mem, lock) => lock
          end
        end.

      End _Get.

  End _Global.

  Inductive state : Type :=
  | LOCAL  : local  -> state
  | GLOBAL : global -> state
  .

End State.


Module Process.

  Definition env : Type := State.local * State.resource.

  Module Env.

    Module _Get.

      Definition local (e:env) : State.local :=
        match e with
        | (l, r) => l
        end.

      Definition pid (e:env) : pid := State._Local._Get.pid (local e).
      Definition vars (e:env) : Vars.vars := State._Local._Get.vars (local e).

      Definition resource (e:env) : State.resource :=
        match e with
        | (l, r) => r
        end.

      Definition mem (e:env) : Memory.mem := State._Resource._Get.mem (resource e).
      Definition lock (e:env) : lock := State._Resource._Get.lock (resource e).

    End _Get.


    Module _Set.

      Module Var.

        Definition predecessor (i:index) (e:env) : env :=
          match e with
          | (l, r) => ((State._Local._Set.Var.predecessor i l), r)
          end.

        Definition locked (b:ibool) (e:env) : env :=
          match e with
          | (l, r) => ((State._Local._Set.Var.locked b l), r)
          end.

        Definition next (i:index) (e:env) : env :=
          match e with
          | (l, r) => ((State._Local._Set.Var.next i l), r)
          end.

        Definition swap (b:ibool) (e:env) : env :=
          match e with
          | (l, r) => ((State._Local._Set.Var.swap b l), r)
          end.

      End Var.

    End _Set.

  End Env.


  Module Expr.

    Inductive expr : Type :=
      | NOT : expr -> expr
      | VAR : Vars.var -> expr
      | NAT : nat -> expr
      | IBOOL : nat -> expr
      | EQ  : nat -> nat -> expr
      | TT  : expr
      | FF  : expr
      .

    Inductive ibool_guard : option ibool -> Prop :=
      | SOME_IBOOL : forall (b:ibool), ibool_guard (Some b)
      .

    Inductive eval : (expr * env) -> (expr * env) -> Prop :=

      | IBOOL_TT : forall e, eval (IBOOL 1, e) (TT, e)
      | IBOOL_FF : forall e, eval (IBOOL 0, e) (FF, e)

      | NOT_TT : forall e, eval (NOT TT, e) (FF, e)
      | NOT_FF : forall e, eval (NOT FF, e) (TT, e)

      | EQ_NAT : forall n e,
        eval (EQ n n) (TT)

      .

  End Expr.


  Module Operation.

    (* used in steps, not syntax *)
    Inductive act : Type :=
      | NCS   : act
      | ENTER : act
      | LEAVE : act

      | READ_NEXT   : act
      | READ_LOCKED : act

      | WRITE_NEXT   : Vars.var -> Vars.val -> act
      | WRITE_LOCKED : Vars.var -> Vars.val -> act

      | FETCH_AND_STORE  : act
      | COMPARE_AND_SWAP : act
      .

      Module Handle.

        (** [read_next e] will set var [next] to be the value
            currently of the [qnode.next] corresponding to the
            [pid] of the process in the state. *)
        Definition read_next (e:env) : env :=
          let m : Memory.mem := Env._Get.mem e in
          let p : pid        := Env._Get.pid e in
          let new_next:index := Memory._Get.next_of (Index.of_pid p) m in
          Env._Set.Var.next new_next e.

        (** [read_locked e] will set var [locked] to be
            the value of the [qnode.locked] corresponding
            to the [pid] of the process in the state.
            ! ! ! it requires that [qnode.locked] is not None. *)
        Definition read_locked (e:env) : env :=
        let m : Memory.mem := Env._Get.mem e in
        let p : pid        := Env._Get.pid e in
        match (Memory._Get.locked_of (Index.of_pid p) m) with
        | None => e (* should not happen, semantics guard this from happening *)
        | Some new_locked =>
          Env._Set.Var.locked new_locked e
        end.


      End Handle.

      Inductive guard : (act * env) -> Prop :=

        | G_READ_NEXT : forall (e:env),
          guard (READ_NEXT, e)

        | G_READ_LOCKED : forall (e:env) (p:pid) (m:Memory.mem) (l:ibool),
          Expr.ibool_guard (Memory._Get.next_of (Index.of_pid p) m) ->
          guard (READ_LOCKED, e)

        | G_WRITE_NEXT : forall vr vl (e:env),
          guard (WRITE_NEXT vr vl, e)

        | G_WRITE_LOCKED : forall vr vl (e:env) (p:pid) (m:Memory.mem) (l:ibool),
          guard (WRITE_LOCKED vr vl, e)

        .

      Definition update_env (a:act) (e:env) : env :=
        match a with
        (* CS Access *)
        | NCS   => e
        | ENTER => e
        | LEAVE => e

        (* Memory Access *)
        | READ_NEXT    => Handle.read_next e
        | READ_LOCKED  => Handle.read_locked e
        (* bind_locked (get_mem_qnode_locked (Index (get_pid s)) s) s *)

        | WRITE_NEXT   pid_to_write next_pid  => e
                          (* set_mem_qnode_next   (get_var pid_to_write s) (get_val next_pid  s) s *)

        | WRITE_LOCKED pid_to_write is_locked => e
                          (* set_mem_qnode_locked (get_var pid_to_write s) (get_val is_locked s) s *)

        (* Lock Access *)
        | FETCH_AND_STORE  => e
        (* set_lock_i (Index (get_pid s)) (bind_predecessor (get_lock_i s) s) *)
        | COMPARE_AND_SWAP => e
        (* let s1 := set_lock_j (Index (get_pid s)) s in
                              let s2 := bind_swap (index_eq (get_lock_i s1) (get_lock_j s1)) s1 in
                              let s3 := set_lock_i 0 s2 in s3 *)
        end.

  End Operation.

  Inductive tm : Type :=
    | TYPE_ERR : tm
    | OK    : tm                      (* no-op *)

    | ACT : Operation.act -> tm

    | SEQ : tm -> tm -> tm

    | IF    : Expr.expr -> tm -> tm -> tm    (* condition -> if true -> if false -> ... *)

    (* | VAL    : Vars.val  -> tm
    | NOT    : tm   -> tm
    | EQ_N   : nat  -> nat -> tm
    | EQ_B   : bool -> bool -> tm
    | IS_TRU : tm   -> tm
    | IS_NIL : tm   -> tm

    | TT   : tm        (* true *)
    | FF   : tm        (* false *)
 *)
    | LOOP      : tm -> tm (* inner loop body -> *)
    | LOOP_END  : tm

    | LOOP_OVER : iloop -> tm -> tm -> tm (* loop id -> body -> outer continuation -> ... *)
    | BREAK     : iloop -> tm             (* loop id -> ... *)
    .

  (* following: https://softwarefoundations.cis.upenn.edu/plf-current/Types.html *)

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
    | TYPE_ERR => TYPE_ERR
    | OK   => OK

    | IF c t e => IF c (subst new t loops) (subst new e loops)


    | VAL t => VAL t
    | NOT t      => NOT t
    | EQ_N a b   => EQ_N a b
    | EQ_B a b   => EQ_B a b
    | IS_TRU a   => IS_TRU a
    | IS_NIL t   => IS_NIL t
    (* | VAR v      => VAR v *)

    | TT => TT
    | FF => FF

    | ACT a => ACT a

    | SEQ l r => SEQ l (subst new r loops) (* do not substitute in other body *)

    | LOOP b   => if loops then LOOP (subst new b loops) else LOOP b
    | LOOP_END => new

    | LOOP_OVER l b c => LOOP_OVER l (subst new b loops) (subst new c loops)
    | BREAK l         => BREAK l
    end.

    Module Semantics.

      Inductive action : Type :=
      | SILENT : action
      | LABEL  : Operation.act -> pid -> action.

      Reserved Notation "t '--<{' a '}>-->' t'" (at level 40).

      Inductive step : (tm * env) -> action -> (tm * env) -> Prop :=

        | ACT : forall a (e:env),
          (Operation.guard (a, e)) ->
          (ACT a, e) --<{LABEL a (Env._Get.pid e)}>--> (OK, Operation.update_env a e)

        | SEQ : forall a l1 l2 r e1 e2,
          (l1, e1) --<{a}>--> (l2, e2) ->
          (SEQ l1 r, e1) --<{a}>--> (SEQ l2 r, e2)

        | SEQ_END : forall r e,
          (SEQ OK r, e) --<{SILENT}>--> (r, e)

        | LOOP  : forall t e,
          (LOOP t, e) --<{SILENT}>--> (subst t LOOP_END false, e)

        | BREAK : forall l c e,
          (LOOP_OVER l (BREAK l) c, e) --<{SILENT}>--> (c, e)

        | LOOP_OVER : forall a l t1 t2 c e1 e2,
          (LOOP t1, e1) --<{a}>--> (LOOP t2, e2) ->
            (LOOP_OVER l t1 c, e1) --<{a}>--> (LOOP_OVER l t2 c, e2)

        | IF_TT : forall t1 t2 e,
          (<{ if TT then t1 else t2 }>, e) --<{SILENT}>--> (t1, e)

        | IF_FF : forall t1 t2 e,
          (<{ if FF then t1 else t2 }>, e) --<{SILENT}>--> (t2, e)

        | IF : forall c1 c2 t1 t2 e1,
          (Expr.eval (c1, e1) (c2, e2)) ->
          (<{ if c1 then t1 else t2 }>, e1) --<{SILENT}>--> (<{ if c2 then t1 else t2 }>, e2)

      where "t '--<{' a '}>-->' t'" := (step t a t').



    End Semantics.

End Process.





Definition tmBool (b:bool) : tm := if true then TT else FF.
Definition tmIBool (b:ibool) : tm := match b with | 0 => FF | 1 => TT | _ => TYPE_ERR end.

Definition IsBool (v:var) : bool :=
  match v with
  | LOCKED => true
  | SWAP => true
  | _ => false
  end.

Definition IsIndex (v:var) : bool :=
  match v with
  | PREDECESSOR => true
  | NEXT => true
  | _ => false
  end.

Definition IsSome (o:option nat) : bool :=
  match o with
  | None => false
  | Some _ => true
  end.



Definition tmIsNil (o:option nat) : tm := match o with | None => VAL TRU | _ => VAL FLS end.

Definition tmIsTru (o:option nat) : tm := match o with | None => TYPE_ERR | Some b => tmIBool b end.

Definition bind_predecessor (p:index) (s:state) : state :=
  match s with
  | (_pid, vars', _mem, _lock) =>
    let vars'' := Build_vars (Some p) (acquire_locked vars') (release_next vars') (release_swap vars') in
    (_pid, vars'', _mem, _lock)
  end.

Definition bind_locked (p:index) (s:state) : state :=
  match s with
  | (_pid, vars', _mem, _lock) =>
    let vars'' := Build_vars (acquire_predecessor vars') (Some p) (release_next vars') (release_swap vars') in
    (_pid, vars'', _mem, _lock)
  end.

Definition bind_next (p:index) (s:state) : state :=
  match s with
  | (_pid, vars', _mem, _lock) =>
    let vars'' := Build_vars (acquire_predecessor vars') (acquire_locked vars') (Some p) (release_swap vars') in
    (_pid, vars'', _mem, _lock)
  end.

Definition bind_swap (p:index) (s:state) : state :=
  match s with
  | (_pid, vars', _mem, _lock) =>
    let vars'' := Build_vars (acquire_predecessor vars') (acquire_locked vars') (release_next vars') (Some p) in
    (_pid, vars'', _mem, _lock)
  end.


(* initial *)
Definition Initial_mem   (n:nat) : mem   := build_mem n.
Definition Initial_lock          : lock  := (0, 0).
Definition Initial_vars          : vars  := Build_vars None None None None.
Definition Initial_state (n:nat) : state := (Nil, Initial_vars, Initial_mem n, Initial_lock).



(* resulting term *)
Definition tm_is_nil (v:val) (s:state) : tm :=
  match v with
  | VAR v' => if IsIndex v'
              then tmIsNil (get_var v' s)
              else TYPE_ERR
  | TRU => FF
  | FLS => FF
  end.

Definition tm_is_tru (v:val) (s:state) : tm :=
  match v with
  | VAR v' => if IsBool v'
              then tmIsTru (get_var v' s)
              else TYPE_ERR
  | TRU => TT
  | FLS => FF
  end.



(* substitution *)
(* handle resulting terms *)


Inductive action : Type := | SILENT : action | LABEL : act -> pid -> action.

Definition step_state (a:act) (s:state) : state :=
  match a with
  (* CS Access *)
  | NCS   => s
  | ENTER => s
  | LEAVE => s

  (* Memory Access *)
  | READ_NEXT    => bind_next   (get_mem_qnode_next   (Index (get_pid s)) s) s
  | READ_LOCKED  => bind_locked (get_mem_qnode_locked (Index (get_pid s)) s) s

  | WRITE_NEXT   pid_to_write next_pid  =>
                    set_mem_qnode_next   (get_var pid_to_write s) (get_val next_pid  s) s

  | WRITE_LOCKED pid_to_write is_locked =>
                    set_mem_qnode_locked (get_var pid_to_write s) (get_val is_locked s) s

  (* Lock Access *)
  | FETCH_AND_STORE  => set_lock_i (Index (get_pid s)) (bind_predecessor (get_lock_i s) s)
  | COMPARE_AND_SWAP => let s1 := set_lock_j (Index (get_pid s)) s in
                        let s2 := bind_swap (index_eq (get_lock_i s1) (get_lock_j s1)) s1 in
                        let s3 := set_lock_i 0 s2 in s3
  end.


Reserved Notation "t '--<{' a '}>-->' t'" (at level 40).

Inductive step : (tm * state) -> action -> (tm * state) -> Prop :=

  | ST_VAL_TRU : forall s, (VAL TRU, s) --<{SILENT}>--> (TT, s)
  | ST_VAL_FLS : forall s, (VAL FLS, s) --<{SILENT}>--> (FF, s)

  | ST_NOT_TT : forall s, (NOT TT, s) --<{SILENT}>--> (FF, s)
  | ST_NOT_FF : forall s, (NOT FF, s) --<{SILENT}>--> (TT, s)

  | ST_EQ_N : forall a b s, (EQ_N a b, s) --<{SILENT}>--> (tmBool (Nat.eqb a b), s)
  | ST_EQ_B : forall a b s, (EQ_B a b, s) --<{SILENT}>--> (tmBool (eqb a b), s)

  | ST_IS_NIL : forall v s,
    (IS_NIL (VAL v), s) --<{SILENT}>--> (tm_is_nil v s, s)

  | ST_IS_TRU : forall v s,
    (IS_TRU (VAL v), s) --<{SILENT}>--> (tm_is_tru v s, s)

  | ST_ACT : forall a t s, (ACT a t, s) --<{LABEL a (get_pid s)}>--> (t, step_state a s)

  | ST_IF_TT : forall t1 t2 s, (<{ if TT then t1 else t2 }>, s) --<{SILENT}>--> (t1, s)
  | ST_IF_FF : forall t1 t2 s, (<{ if FF then t1 else t2 }>, s) --<{SILENT}>--> (t2, s)

  | ST_IF    : forall a c1 c2 t1 t2 s,
    (c1, s) --<{a}>--> (c2, s) ->
      (<{ if c1 then t1 else t2 }>, s) --<{SILENT}>--> (<{ if c2 then t1 else t2 }>, s)

  | ST_SEQ_END : forall r s, (SEQ OK r, s) --<{SILENT}>--> (r, s)
  | ST_SEQ     : forall a l1 l2 r s1 s2,
    (l1, s1) --<{a}>--> (l2, s2) ->
      (SEQ l1 r, s1) --<{a}>--> (SEQ l2 r, s2)

  | ST_LOOP  : forall t s, (LOOP t, s) --<{SILENT}>--> (subst t LOOP_END false, s)
  | ST_BREAK : forall l c s, (LOOP_OVER l (BREAK l) c, s) --<{SILENT}>--> (c, s)

  | ST_LOOP_OVER : forall a l t1 t2 c s1 s2,
    (LOOP t1, s1) --<{a}>--> (LOOP t2, s2) ->
      (LOOP_OVER l t1 c, s1) --<{a}>--> (LOOP_OVER l t2 c, s2)

  where "t '--<{' a '}>-->' t'" := (step t a t').


Print step.

Reserved Notation "t '==<{' a '}>==>' t'" (at level 40).

Inductive sys : Type :=
  | PRC : tm -> sys
  | PAR : sys -> sys -> sys
  .

(* all tm must share the state, pass upwards to step. *)
Inductive lts : sys * state -> action -> sys * state -> Prop :=

  | LTS_PRC : forall a t1 t2 s1 s2,
    (t1, s1) --<{a}>--> (t2, s2) ->
      (PRC t1, s1) ==<{a}>==> (PRC t2, s2)

  | LTS_TERM_L : forall a r s, (PAR (PRC OK) r, s) ==<{a}>==> (r, s)
  | LTS_TERM_R : forall a l s, (PAR l (PRC OK), s) ==<{a}>==> (l, s)

  | LTS_PAR_L : forall a l1 l2 r s1 s2,
    (l1, s1) ==<{a}>==> (l2, s2) ->
      (PAR l1 r, s1) ==<{a}>==> (PAR l2 r, s2)

  | LTS_PAR_R : forall a l r1 r2 s1 s2,
    (r1, s1) ==<{a}>==> (r2, s2) ->
      (PAR l r1, s1) ==<{a}>==> (PAR l r2, s2)


  where "t '==<{' a '}>==>' t'" := (lts t a t').

Print lts.


(*******************)
(**** Example ******)
(*******************)

Example Acquire : tm :=
  ACT (WRITE_NEXT PID (VAR NIL)) (
    ACT FETCH_AND_STORE (
      IF (NOT (IS_NIL (VAL (VAR PREDECESSOR)))) (
        ACT (WRITE_LOCKED PID TRU) (
          ACT (WRITE_NEXT PREDECESSOR (VAR PID)) (
            LOOP_OVER 0 (*L*) (
              ACT READ_LOCKED (
                IF (NOT (VAL (VAR LOCKED))) (
                  BREAK 0 (*L*)
                ) (OK)
              )
            ) (OK)
          )
        )
      ) (OK)
    )
  ).

Example Release : tm :=
  ACT READ_NEXT (
    IF (IS_NIL (VAL (VAR NEXT))) (
      ACT COMPARE_AND_SWAP (
        IF (NOT (IS_TRU (VAL (VAR SWAP)))) (
          LOOP_OVER 0 (*L*) (
            ACT READ_NEXT (
              IF (NOT (IS_NIL (VAL (VAR NEXT)))) (
                BREAK 0 (*L*)
              ) (OK)
            )
          ) (
            ACT (WRITE_LOCKED NEXT TRU) (OK)
          )
        ) (OK)
      )
    ) (
      ACT (WRITE_LOCKED NEXT FLS) (OK)
    )
  ).

Example P : tm :=
  LOOP (
    ACT NCS (
      SEQ Acquire (
        ACT ENTER (
          ACT LEAVE (
            SEQ Release (OK)
          )
        )
      )
    )
  ).

Print P.

Example config : tm * state := (P, Initial_state 1).
Print config.

Example sys_config : sys * state := (PRC P, Initial_state 1).
Print sys_config.

MeBi LTS step config.
MeBi LTS lts sys_config.
