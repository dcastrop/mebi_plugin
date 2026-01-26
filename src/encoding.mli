  module type SEncoding = sig
    type t

    val init : t
    val next : t -> t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val to_string : t -> string
    val counter : t ref
    val reset : unit -> unit
    val incr : unit -> t
  end

  module type S = sig
    type t

    val init : t
    val next : t -> t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val to_string : t -> string
  end

  module Make : (_ : S) -> SEncoding
  module Int : SEncoding