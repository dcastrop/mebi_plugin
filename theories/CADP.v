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

Definition index : Type := option nat.
Definition pid   : Type := nat.

Definition ibool : Type := nat.
Definition iloop : Type := nat.

Definition Nil   : index := None.


(* we dont need new_i, only i and j *)
Definition lock : Type := index * index.

Module Lock.
  Definition initial : lock := (None, None).
End Lock.


Module Index.

  Definition initial : index := Nil.

  Definition of_pid (i : pid) : index := Some i.

  Definition to_nat (i : index) : nat :=
    match i with
    | None => 0
    | Some n => n
    end.

  Definition is_nil (i : index) : bool :=
    match i with
    | None => false
    | _ => true
    end.

  Definition eqb (i:index) (j:index) : bool :=
    match i, j with
    | None, None => true
    | Some n, Some m => Nat.eqb n m
    | _, _ => false
    end.

  Definition is_index (n:option nat) : bool := true.

End Index.


Module PID.

  Definition is_pid (n : nat) : bool := if Nat.eqb n 0 then false else true.

  Definition to_nat (p:pid) : nat := p.

End PID.



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

  Definition to_nat (b:ibool) : nat := b.

  Definition is_ibool (n:nat) : bool :=
    match n with
    | 0 => true
    | 1 => true
    | _ => false
    end.

  Definition is_some (b:option ibool) : bool :=
    match b with
    | None => false
    | _    => true
    end.

End IBool.




(*************************************************************************)
(**** Memory *************************************************************)
(*************************************************************************)
Module Memory.

  (**********************************)
  (**** Qnode ***********************)
  (**********************************)

  Record qnode :=
    { next   : index
    ; locked : ibool }.

  Module Qnode.
    Definition initial : qnode := Build_qnode None 0.

    Module _Get.
      Definition next   (q:qnode) : index := (next q).
      Definition locked (q:qnode) : ibool := (locked q).
    End _Get.


    Module _Set.
      Definition next   (i:index) (q:qnode) : qnode :=
        Build_qnode i (Qnode._Get.locked q).
      Definition locked (b:ibool) (q:qnode) : qnode :=
        Build_qnode (Qnode._Get.next q) b.
    End _Set.

  End Qnode.


  (**********************************)
  (**** mem Type & app **************)
  (**********************************)

  Definition mem : Type := list qnode.

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


(*************************************************************************)
(**** Vars ***************************************************************)
(*************************************************************************)
Module Vars.

  Record vars :=
  { var_predecessor : index        (* acquire *)
  ; var_locked      : option ibool (* acquire *)
  ; var_next        : index        (* release *)
  ; var_swap        : option ibool (* release *) }.

  Definition initial : vars := Build_vars None None None None.

  Inductive var : Type :=
    | PREDECESSOR
    | LOCKED
    | NEXT
    | SWAP
    | PID
    (* | NIL *)
    .


  Module Kind.
    Inductive kind : Type := | INDEX | IBOOL | PID.
  End Kind.


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


(*************************************************************************)
(**** Value **************************************************************)
(*************************************************************************)
Module Val.
  Inductive val : Type :=
  | INDEX : index        -> val
  | IBOOL : option ibool -> val
  | PID : val
  | VAR : Vars.var -> val
  | TRU : val
  | FLS : val
  | NIL : val
  .
End Val.


(*************************************************************************)
(**** (Global) Resource **************************************************)
(*************************************************************************)
Module Resource.
  Definition resource : Type := Memory.mem * lock.
  Definition initial (n:nat) : resource := (Memory.initial n, Lock.initial).

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
End Resource.


(*************************************************************************)
(**** (Local) State ******************************************************)
(*************************************************************************)
Module State.

  Definition local : Type := pid * Vars.vars.

  Definition create (p:pid) : local := (p, Vars.initial).

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
      Definition predecessor (s:local) : index :=
        Vars._Get.predecessor (vars s).
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
      | Vars.PID         => Some (pid s)
      end.

    Definition val (v:Val.val) (s:local) : option nat :=
      match v with
      | Val.INDEX i => i
      | Val.IBOOL b => b
      | Val.PID     => Some (pid s)
      | Val.VAR v   => var v s
      | Val.TRU     => Some 1
      | Val.FLS     => Some 0
      | Val.NIL     => Nil
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
End State.


(*************************************************************************)
(**** Environment (local) ************************************************)
(*************************************************************************)
Definition env : Type := State.local * Resource.resource.

Module Env.
  Module _Get.

    Definition local (e:env) : State.local :=
      match e with | (l, r) => l end.

    Definition pid (e:env) : pid := State._Get.pid (local e).
    Definition vars (e:env) : Vars.vars := State._Get.vars (local e).

    Module Var.
      Definition predecessor (e:env) : index :=
        Vars._Get.predecessor (vars e).
      Definition locked (e:env) : option ibool :=
        Vars._Get.locked (vars e).
      Definition next (e:env) : index :=
        Vars._Get.next (vars e).
      Definition swap (e:env) : option ibool :=
        Vars._Get.swap (vars e).

    End Var.

    Definition var (v:Vars.var) (e:env) : option nat :=
      State._Get.var v (local e).

    Definition val (v:Val.val) (e:env) : option nat :=
      State._Get.val v (local e).

    Definition resource (e:env) : Resource.resource :=
      match e with | (l, r) => r end.

    Definition mem (e:env) : Memory.mem :=
      Resource._Get.mem (resource e).

    Module Mem.
      Definition locked_of (i:index) (e:env) : option ibool :=
        Memory._Get.locked_of i (Env._Get.mem e).

      Definition next_of (i:index) (e:env) : index :=
        Memory._Get.next_of i (Env._Get.mem e).

    End Mem.


    Definition lock (e:env) : lock :=
      Resource._Get.lock (resource e).

    Module Lock.
      Definition i (e:env) : index :=
        Resource._Get.Lock.i (Env._Get.resource e).
      Definition j (e:env) : index :=
        Resource._Get.Lock.j (Env._Get.resource e).

    End Lock.
  End _Get.


  Module _Set.
    Module Var.

      Definition predecessor (i:index) (e:env) : env :=
        match e with
        | (l, r) => ((State._Set.Var.predecessor i l), r)
        end.

      Definition locked (b:ibool) (e:env) : env :=
        match e with
        | (l, r) => ((State._Set.Var.locked b l), r)
        end.

      Definition next (i:index) (e:env) : env :=
        match e with
        | (l, r) => ((State._Set.Var.next i l), r)
        end.

      Definition swap (b:ibool) (e:env) : env :=
        match e with
        | (l, r) => ((State._Set.Var.swap b l), r)
        end.

    End Var.

    Definition resource (r:Resource.resource) (e:env) : env :=
      (Env._Get.local e, r).

    Definition mem (m:Memory.mem) (e:env) : env :=
      (Env._Get.local e, (m, Env._Get.lock e)).

    Module Mem.
      Definition locked_of (i:index) (b:ibool) (e:env) : option env :=
        match (Memory._Set.locked_of i b (Env._Get.mem e)) with
        | None => None
        | Some m => Some (mem m e)
        end.

      Definition next_of (i:index) (n:index) (e:env) : option env :=
        match (Memory._Set.next_of i n (Env._Get.mem e)) with
        | None => None
        | Some m => Some (mem m e)
        end.

    End Mem.

    Definition lock (l:lock) (e:env) : env :=
      (Env._Get.local e, (Env._Get.mem e, l)).

    Module Lock.
      Definition i (new_i:index) (e:env) : env :=
        resource (Resource._Set.Lock.i new_i (Env._Get.resource e)) e.

      Definition j (new_j:index) (e:env) : env :=
        resource (Resource._Set.Lock.j new_j (Env._Get.resource e)) e.

    End Lock.

  End _Set.


  Module Handle.

    (** [read_next e] will set var [next] to be the value
        currently of the [qnode.next] corresponding to the
        [pid] of the process in the state. *)
    Definition read_next (e:env) : env :=
      let p : pid        := Env._Get.pid e in
      let new_next:index := Env._Get.Mem.next_of (Index.of_pid p) e in
      Env._Set.Var.next new_next e.

    (** [read_locked e] will set var [locked] to be
        the value of the [qnode.locked] corresponding
        to the [pid] of the process in the state.
        ! ! ! it requires that [qnode.locked] is not None. *)
    Definition read_locked (e:env) : env :=
    let p : pid        := Env._Get.pid e in
    match (Env._Get.Mem.locked_of (Index.of_pid p) e) with
    | None => e (* shouldn't happen, semantics guard from this. *)
    | Some new_locked => Env._Set.Var.locked new_locked e
    end.

    (*  *)
    Definition write_next (to_write:Vars.var) (next:Val.val) (e:env) : env :=
      match (Env._Get.val next e) with
      | None => e (* shouldn't happen, semantics guard from this. *)
      | Some next' =>
        let next' : index := Index.of_pid next' in
        match (Env._Get.var to_write e) with
        | None => e (* shouldn't happen, semantics guard from this. *)
        | Some to_write' =>
          match (Env._Set.Mem.next_of (Index.of_pid to_write') next' e) with
          | None => e (* shouldn't happen, semantics guard from this. *)
          | Some e' => e'
          end
        end
      end.

    (*  *)
    Definition write_locked (to_write:Vars.var) (locked:Val.val) (e:env) : env :=
      match (Env._Get.val locked e) with
        | None => e (* shouldn't happen, semantics guard from this. *)
        | Some locked' =>
        match (IBool.is_ibool locked') with
        | false => e (* shouldn't happen, semantics guard from this. *)
        | true =>
          match (Env._Get.var to_write e) with
          | None => e (* shouldn't happen, semantics guard from this. *)
          | Some to_write' =>
            let to_write' : index := Index.of_pid to_write' in
            match (Env._Set.Mem.locked_of to_write' locked' e) with
            | None => e (* shouldn't happen, semantics guard from this. *)
            | Some e' => e'
            end
          end
        end
      end.

    (*  *)
    Definition fetch_and_store (e:env) : env :=
      (* store [Lock.i] in [Vars.predecessor]. *)
      let i : index := Env._Get.Lock.i e in
      let e' : env := Env._Set.Var.predecessor i e in
      (* set [Lock.i] to [pid]. *)
      let p : pid := Env._Get.pid e' in
      Env._Set.Lock.i (Index.of_pid p) e'.

    (*  *)
    Definition compare_and_swap (e:env) : env :=
      let i : index := Env._Get.Lock.i e in
      let j : index := Env._Get.Lock.j e in
      (* set [Lock.j] to [pid]. *)
      let p : pid := Env._Get.pid e in
      let e' : env := Env._Set.Lock.j (Index.of_pid p) e in
      let are_eq : bool := Index.eqb i j in
      Env._Set.Var.swap
        (IBool.of_bool are_eq)
        (* also, if eq then reset [i] to Nil. *)
        (if are_eq then Env._Set.Lock.i Nil e' else e').

    End Handle.

End Env.



(*************************************************************************)
(**** Process ************************************************************)
(*************************************************************************)
Module Process.



  Module Expr.

    Inductive expr : Type :=
      | NOT : expr -> expr
      | VAR : Vars.var -> expr
      | NAT : nat -> expr
      | IBOOL : ibool -> expr
      | INDEX : index -> expr
      | CAST : expr -> Vars.Kind.kind -> expr
      | PID : pid -> expr
      | EQ  : expr -> expr -> expr
      | TT  : expr
      | FF  : expr
      | NIL : expr
      .

    Inductive eval : (expr * env) -> (expr * env) -> Prop :=

      | IBOOL_TT : forall e, eval (IBOOL 1, e) (TT, e)
      | IBOOL_FF : forall e, eval (IBOOL 0, e) (FF, e)

      | NOT_TT : forall e, eval (NOT TT, e) (FF, e)
      | NOT_FF : forall e, eval (NOT FF, e) (TT, e)

      | VAR_PREDECESSOR : forall v e,
        (Env._Get.Var.predecessor e)=v ->
        eval (VAR Vars.PREDECESSOR, e) (INDEX v, e)

      | VAR_LOCKED : forall v b e,
        (Env._Get.Var.locked e)=v /\ v=(Some b) ->
        eval (VAR Vars.LOCKED, e) (IBOOL b, e)

      | VAR_NEXT : forall v e,
        (Env._Get.Var.next e)=v ->
        eval (VAR Vars.NEXT, e) (INDEX v, e)

      | VAR_SWAP : forall v b e,
        (Env._Get.Var.swap e)=v /\ v=(Some b) ->
        eval (VAR Vars.SWAP, e) (IBOOL b, e)

      | CAST_PID_TO_INDEX : forall p e,
        eval (CAST (PID p) Vars.Kind.INDEX, e) (INDEX (Index.of_pid p), e)

      | CAST_INDEX_TO_PID : forall i n e,
        (i=Some(S n)) ->
        eval (CAST (INDEX i) Vars.Kind.PID, e) (PID n, e)

      | EQ_TT_NAT : forall n m e,
        (Nat.eqb n m)=true ->
        eval (EQ (NAT n) (NAT m), e) (TT, e)

      | EQ_FF_NAT : forall n m e,
        (Nat.eqb n m)=false ->
        eval (EQ (NAT n) (NAT m), e) (FF, e)

      | EQ_TT_IBOOL : forall n m e,
        ((IBool.is_ibool n)=true /\ (IBool.is_ibool m)=true)
        /\ (Nat.eqb (IBool.to_nat n) (IBool.to_nat m))=true ->
        eval (EQ (IBOOL n) (IBOOL m), e) (TT, e)

      | EQ_FF_IBOOL : forall n m e,
        ((IBool.is_ibool n)=false \/ (IBool.is_ibool m)=false)
        \/ (Nat.eqb (IBool.to_nat n) (IBool.to_nat m))=false ->
        eval (EQ (IBOOL n) (IBOOL m), e) (FF, e)

      | EQ_TT_INDEX : forall n m e,
        (Nat.eqb (Index.to_nat n) (Index.to_nat m))=true ->
        eval (EQ (INDEX n) (INDEX m), e) (TT, e)

      | EQ_FF_INDEX : forall n m e,
        (Nat.eqb (Index.to_nat n) (Index.to_nat m))=false ->
        eval (EQ (INDEX n) (INDEX m), e) (TT, e)

      | EQ_TT_PID : forall n m e,
        ((PID.is_pid n)=true /\ (PID.is_pid m)=true)
        /\ (Nat.eqb (PID.to_nat n) (PID.to_nat m))=true ->
        eval (EQ (PID n) (PID m), e) (TT, e)

      | EQ_FF_PID : forall n m e,
        ((PID.is_pid n)=false \/ (PID.is_pid m)=false)
        \/ (Nat.eqb (PID.to_nat n) (PID.to_nat m))=false ->
        eval (EQ (PID n) (PID m), e) (TT, e)

      | EQ_TT_NIL_NIL : forall e,
        eval (EQ NIL NIL, e) (TT, e)

      | EQ_TT_NIL_INDEX_L : forall n e,
        (Index.is_nil n)=true ->
        eval (EQ (INDEX n) NIL, e) (TT, e)

      | EQ_FF_NIL_INDEX_L : forall n e,
        (Index.is_nil n)=false ->
        eval (EQ (INDEX n) NIL, e) (FF, e)

      | EQ_TT_NIL_INDEX_R : forall n e,
        (Index.is_nil n)=true ->
        eval (EQ NIL (INDEX n), e) (TT, e)

      | EQ_FF_NIL_INDEX_R : forall n e,
        (Index.is_nil n)=false ->
        eval (EQ NIL (INDEX n), e) (FF, e)

      | EQ_FF_NIL_PID_L : forall n e,
        (PID.is_pid n)=true ->
        eval (EQ (PID n) NIL, e) (FF, e)

      | EQ_FF_NIL_PID_R : forall n e,
        (PID.is_pid n)=true ->
        eval (EQ NIL (PID n), e) (FF, e)
      .

    Print eval.

  End Expr.


  Module Operation.

    (* used in steps, not syntax *)
    Inductive act : Type :=
      | NCS   : act
      | ENTER : act
      | LEAVE : act

      | READ_NEXT   : act
      | READ_LOCKED : act

      | WRITE_NEXT   : Vars.var -> Val.val -> act
      | WRITE_LOCKED : Vars.var -> Val.val -> act

      | FETCH_AND_STORE  : act
      | COMPARE_AND_SWAP : act
      .

    Inductive guard : (act * env) -> Prop :=

      (* no guard. *)
      | G_READ_NEXT : forall e,
        guard (READ_NEXT, e)

      (* check that [qnode.next] of [pid] is not None. *)
      | G_READ_LOCKED : forall v b e,
        (Memory._Get.next_of
          (Index.of_pid (Env._Get.pid e))
          (Env._Get.mem e))=v /\ v=(Some b) ->
        guard (READ_LOCKED, e)

      (* check that [vqi] is not None. *)
      | G_WRITE_NEXT : forall vqi qi vni ni e,
        (Env._Get.var vqi e)=(Some qi) /\ (PID.is_pid qi)=true /\
        (Env._Get.val vni e)=(Some ni) /\ (PID.is_pid ni)=true /\
        vni=(Val.INDEX (Some ni)) ->
        guard (WRITE_NEXT vqi vni, e)

      (* check [vqi] and [vnl] are not None, and that [vnl] is Some ibool. *)
      | G_WRITE_LOCKED : forall vqi qi vnl nl e,
        (Env._Get.var vqi e)=(Some qi) /\ (PID.is_pid qi)=true     /\
        (Env._Get.val vnl e)=(Some nl) /\ (IBool.is_ibool nl)=true /\
        vnl=(Val.IBOOL (Some nl)) ->
        guard (WRITE_LOCKED vqi vnl, e)

      (* no guard. *)
      | G_FETCH_AND_STORE : forall e,
        guard (FETCH_AND_STORE, e)

      (* no guard. *)
      | G_COMPARE_AND_SWAP : forall e,
        guard (COMPARE_AND_SWAP, e)
      .

    Print guard.

    Definition update_env (a:act) (e:env) : env :=
      match a with
      (* CS Access *)
      | NCS   => e
      | ENTER => e
      | LEAVE => e

      (* Memory Access *)
      | READ_NEXT    => Env.Handle.read_next e
      | READ_LOCKED  => Env.Handle.read_locked e

      | WRITE_NEXT to_write next =>
          Env.Handle.write_next to_write next e

      | WRITE_LOCKED to_write locked =>
          Env.Handle.write_locked to_write locked e

      (* Lock Access *)
      | FETCH_AND_STORE  => Env.Handle.fetch_and_store e
      | COMPARE_AND_SWAP => Env.Handle.compare_and_swap e

      end.

  End Operation.

  Inductive tm : Type :=
    | ERR : tm (* error *)
    | OK  : tm (* no-op *)
    | END : tm (* termination *)

    | IF  : Expr.expr -> tm -> tm -> tm

    | ACT : Operation.act -> tm

    | SEQ : tm -> tm -> tm

    | LOOP      : tm -> tm (* inner loop body -> *)
    | LOOP_END  : tm

    (* loop id -> body -> outer continuation -> ... *)
    | LOOP_OVER : iloop -> tm -> tm -> tm
    | BREAK     : iloop -> tm
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
        (<{ if Expr.TT then t1 else t2 }>, e) --<{SILENT}>--> (t1, e)

      | IF_FF : forall t1 t2 e,
        (<{ if Expr.FF then t1 else t2 }>, e) --<{SILENT}>--> (t2, e)

      | IF : forall c1 c2 t1 t2 e,
        (Expr.eval (c1, e) (c2, e)) ->
        (<{ if c1 then t1 else t2 }>, e) --<{SILENT}>--> (<{ if c2 then t1 else t2 }>, e)

    where "t '--<{' a '}>-->' t'" := (step t a t').

  End Semantics.

End Process.


(*************************************************************************)
(**** System (of processes) **********************************************)
(*************************************************************************)
Module System.

  Inductive sys : Type :=
    | PRC : Process.tm -> State.local -> sys
    | PAR : sys -> sys -> sys
    | TERM : sys
    .

  Module LTS.
    Reserved Notation "t '--<{' a '}>-->' t'" (at level 40).
    Reserved Notation "t '==<{' a '}>==>' t'" (at level 40).

    Inductive lts : sys * Resource.resource -> Process.Semantics.action -> sys * Resource.resource -> Prop :=

    (* wrapper for sharing global resource with local process. *)
      | PRC : forall a t1 t2 s1 s2 r1 r2,
        (t1, (s1, r1)) --<{a}>--> (t2, (s2, r2)) ->
        (PRC t1 s1, r1) ==<{a}>==> (PRC t2 s2, r2)

      | PAR_L : forall a l1 l2 r gr1 gr2,
        (l1, gr1) ==<{a}>==> (l2, gr2) ->
        (PAR l1 r, gr1) ==<{a}>==> (PAR l2 r, gr2)

      | PAR_R : forall a l r1 r2 gr1 gr2,
        (r1, gr1) ==<{a}>==> (r2, gr2) ->
        (PAR l r1, gr1) ==<{a}>==> (PAR l r2, gr2)

      | END_L : forall a ls r gr,
        (PAR (PRC Process.END ls) r, gr) ==<{a}>==> (r, gr)

      | END_R : forall a l rs gr,
        (PAR l (PRC Process.END rs), gr) ==<{a}>==> (l, gr)

      where "t '==<{' a '}>==>' t'" := (lts t a t')
      and "t '--<{' a '}>-->' t'" := (Process.Semantics.step t a t').

    Print lts.
  End LTS.

End System.


(*************************************************************************)
(**** Example ************************************************************)
(*************************************************************************)

Import Process.
Import Operation.
Import Expr.

Example Acquire : tm :=
  SEQ (ACT (WRITE_NEXT Vars.PID Val.NIL)) (
    SEQ (ACT FETCH_AND_STORE) (
      IF (NOT (EQ (VAR Vars.PREDECESSOR) NIL)) (
        SEQ (ACT (WRITE_LOCKED Vars.PREDECESSOR (Val.PID))) (
          LOOP_OVER 0 (
            SEQ (ACT READ_LOCKED) (
              IF (NOT (VAR Vars.LOCKED)) (BREAK 0) (OK)
            )
          ) (END)
        )
      ) (END)
    )
  ).


Example Release : tm :=
  SEQ (ACT READ_NEXT) (
    IF (EQ NIL (VAR Vars.NEXT)) (
      SEQ (ACT COMPARE_AND_SWAP) (
        IF (EQ FF (VAR Vars.SWAP)) (
          LOOP_OVER 0 (*L*) (
            SEQ (ACT READ_NEXT) (
              IF (NOT (EQ NIL (VAR Vars.NEXT))) (BREAK 0 (*L*)) (OK)
            )
          ) (ACT (WRITE_LOCKED Vars.NEXT Val.TRU))
        ) (END)
      )
    ) (ACT (WRITE_LOCKED Vars.NEXT Val.FLS))
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


Example p0 : tm * env := (P, (State.create 0, Resource.initial 0)).
MeBi LTS Semantics.step p0.


(*******************************)
(**** As part of a system ******)
(*******************************)

Definition process : Type := tm * State.local.
Definition spawn (p:pid) (b:tm) : process := (b, State.create p).

Fixpoint populate (n:nat) (b:tm) : list process :=
  match n with
  | 0 => []
  | S m => app (populate m b) [spawn n b]
  end.

Definition system : Type := list process * Resource.resource.
Definition create (n:nat) (b:tm) : system := (populate n b, Resource.initial n).

Fixpoint load (ps:list process) : System.sys :=
  match ps with
  | [] => System.TERM
  | h :: t =>
    match h with
    | (p, s) => System.PAR (System.PRC p s) (load t)
    end
  end.

Definition composition : Type := System.sys * Resource.resource.
Definition compose (s:system) : composition :=
  match s with
  | (ps, r) => (load ps, r)
  end.

Example ncs : composition := compose (create 5 P).
MeBi LTS System.LTS.lts ncs.
