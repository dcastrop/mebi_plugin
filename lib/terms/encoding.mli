module type S = sig
  include Base_term.S

  val init : t
  val next : t -> t
  val reset : unit -> unit
  val incr : unit -> t
end

module Make : (Log : Logger.S)
    (Base : Base_term.S)
    (X : sig
       val init : Base.t
       val next : Base.t -> Base.t
     end)
    -> S with type t = Base.t

module ToBase : (Enc : S) -> Base_term.S
module Int : (Log : Logger.S) -> S with type t = int
