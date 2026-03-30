module type S = sig
  type enc

  type t =
    | Option of enc
    | Custom of enc * enc

  include Json.S with type k = t (** @closed *)

  val eq : t -> t -> bool
end

module Make
    (Log : Logger.S)
    (Enc : Encoding.S)
    (M : Rocq_monad_utils.S with type enc = Enc.t) : S with type enc = Enc.t
