module type S = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val to_string : t -> string
end

module MakeFromEnc : (Enc : Encoding.SEncoding) -> S with type t = Enc.t

module MakeFromEConstr : (_ : Rocq_context.SRocq_context) ->
  S with type t = EConstr.t
