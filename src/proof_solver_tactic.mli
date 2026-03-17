module type S = sig
  type t =
    { this : tactic
    ; next : t option
    }

  and tactic =
    { get : unit Proofview.tactic
    ; msg : (Output.Kind.t * string) option
    }

  val create : ?level:Output.Kind.t -> ?msg:string -> unit Proofview.tactic -> t
  val empty : unit -> t
  val do_nothing : unit -> t
  val seq : t -> t -> t

  exception EmptyTacticChain

  val chain : ?nonempty:bool -> t list -> t
  val unpack : t -> unit Proofview.tactic
end

module Make (Log : Logger.S) : S
