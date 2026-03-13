module type S = sig
  type enc

  type t =
    | Option of enc
    | Custom of enc * enc

  type k = t

  val json : ?as_elt:bool -> k -> Yojson.t
  val to_string : ?pretty:bool -> k -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit
  val eq : t -> t -> bool
end

module Make
    (Log : Logger.S)
    (Enc : Encoding.S)
    (M : Rocq_monad_utils.S with type enc = Enc.t) : S with type enc = Enc.t