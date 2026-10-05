module type S = sig
  (** One tactic of a chain, with the message to show when it runs. *)
  module Tac : sig
    type t =
      { get : unit Proofview.tactic
      ; msg : msg option
      }

    and msg = Output.Kind.t * string

    (** [to_string_opt t] is [t]'s message, if it has one and its kind is
        enabled ({!Logger.is_enabled}). Raises nothing. *)
    val to_string_opt : t -> string option
  end

  (** A non-empty chain of tactics, run in order. *)
  type t =
    { this : Tac.t
    ; next : t option
    }

  (** [create ?kind ?msg tac] is the chain of the one tactic [tac], showing
      [msg] at [kind] (default [Info]) when it runs. Raises nothing. *)
  val create : ?kind:Output.Kind.t -> ?msg:string -> unit Proofview.tactic -> t

  (** [empty ()] is the chain of [tclUNIT], doing nothing. Raises nothing. *)
  val empty : unit -> t

  (** [do_nothing ()] is {!empty}, shown as "(skip)" at [Debug]. Raises
      nothing. *)
  val do_nothing : unit -> t

  (** [seq a b] is the chain [a] then [b]. Raises nothing. *)
  val seq : t -> t -> t

  (** Raised by {!chain} on an empty list when asked to. *)
  exception EmptyTacticChain

  (** [chain ?nonempty ts] is the tactics [ts] chained in order ({!seq}),
      or {!empty} if there are none.

      @raise EmptyTacticChain
        if [ts] is empty and [nonempty] is set (raised
        here). *)
  val chain : ?nonempty:bool -> t list -> t

  (** [unpack c] is the tactic that runs [c]'s tactics in order
      ([tclTHEN]), after showing their messages at [Notice]. Raises nothing;
      fails (as a tactic) when one of them does. *)
  val unpack : t -> unit Proofview.tactic
end

module Make : S
