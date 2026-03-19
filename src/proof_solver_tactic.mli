module type S = sig
  module Tac : sig
    type t =
      { get : unit Proofview.tactic
      ; msg : msg option
      }

    and msg = Output.Kind.t * string

    val to_string_opt : t -> string option
  end

  type t =
    { this : Tac.t
    ; next : t option
    }

  val create : ?kind:Output.Kind.t -> ?msg:string -> unit Proofview.tactic -> t
  val empty : unit -> t
  val do_nothing : unit -> t
  val seq : t -> t -> t

  exception EmptyTacticChain

  val chain : ?nonempty:bool -> t list -> t
  val unpack : t -> unit Proofview.tactic
end

module Make (Log : Logger.S) : S
