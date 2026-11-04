module type S = Wrapper_intf.S

module Make (Enc : Encoding.S) :
  S
  with type enc = Enc.t
   and type node = Enc.Tree.Node.t
   and type tree = Enc.Tree.t
   and type trees = Enc.Trees.t

(** [make ?enc ()] is a new wrapper over the encoding [enc ()] (by default
    {!Api.make_enc_int}). Raises nothing. *)
val make : ?enc:(unit -> (module Encoding.S)) -> unit -> (module S)

(** The shared instance the [MeBi ...] vernaculars run against, created on first
    use. Commands no longer build their own: per-command state lifetime is
    carried by the [~reset_encoding:true] they already pass, not by rebuilding
    the module tree. *)
val get : unit -> (module S)

(** Drops the shared instance so the next {!get} builds a fresh one. *)
val reset : unit -> unit
