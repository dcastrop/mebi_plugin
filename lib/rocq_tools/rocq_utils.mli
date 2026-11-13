(** Small Rocq helpers shared by the symbolic route and the tactics. *)

(** [lts_shaped env ind] is whether the inductive [ind] has the type of an
    LTS, [M -> option A -> M -> Prop] ([theories/Bisimilarity.v]'s [LTS]),
    without parameters. Raises nothing. *)
val lts_shaped : Environ.env -> Names.inductive -> bool

(** [is_prop env sigma t] is whether the type [t] is a proposition (its
    sort is [Prop]). Raises nothing. *)
val is_prop : Environ.env -> Evd.evar_map -> EConstr.t -> bool

(** [push_var d env]: [env] with the local definition or assumption [d] as
    a named variable (Rocq 9.3 also asks for its kind: a proof's
    variable). Raises nothing. *)
val push_var : Constr.named_declaration -> Environ.env -> Environ.env

(** [collapse_sorts sigma]: [sigma] with every sort variable collapsed
    (Rocq 9.3's [~only_above_prop:false]; Rocq 9.2 has no such flag).
    Raises nothing. *)
val collapse_sorts : Evd.evar_map -> Evd.evar_map
