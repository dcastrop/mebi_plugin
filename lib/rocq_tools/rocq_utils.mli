(** Small Rocq helpers shared by the symbolic route and the tactics. *)

(** [lts_shaped env ind] is whether the inductive [ind] has the type of an
    LTS, [M -> option A -> M -> Prop] ([theories/Bisimilarity.v]'s [LTS]),
    without parameters. Raises nothing. *)
val lts_shaped : Environ.env -> Names.inductive -> bool

(** [is_prop env sigma t] is whether the type [t] is a proposition (its
    sort is [Prop]). Raises nothing. *)
val is_prop : Environ.env -> Evd.evar_map -> EConstr.t -> bool
