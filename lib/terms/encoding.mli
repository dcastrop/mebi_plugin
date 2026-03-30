module type S = sig
  include Base_term.S

  include Json.S with type k = t (** @closed *)

  val init : t
  val next : t -> t
  val reset : unit -> unit
  val incr : unit -> t
end

module type Args = sig
  type t

  val init : t
  val next : t -> t
end

module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (X : Args with type t = Base.t) : S with type t = Base.t

module Packed : sig
  module type PackedS = sig
    type t

    module BaseArgs : Base_term.Args with type t = t
    module EncodingArgs : Args with type t = t
  end

  module Int : PackedS with type t = Int.t
  module Unpack (Log : Logger.S) (Args : PackedS) : S with type t = Args.t
end
