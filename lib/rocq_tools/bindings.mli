(** Where an LTS constructor's binders sit in its source, label and target
    terms, as paths into those terms; {!Constructor_bindings} follows the
    paths in a concrete step to bind them. *)
module type S = Bindings_intf.S

module Make (M : Rocq_monad_utils.S) : S with type 'a mm = 'a M.mm
