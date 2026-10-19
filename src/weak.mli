module type S = sig
  type enc

  (** A silent label: [Option l], where the label type is an [option] and
      [None] is silent ([l] is the label type); or [Custom (tau, ty)], where
      [tau] is the silent label of type [ty]. *)
  type t =
    | Option of enc
    | Custom of enc * enc

  include Json.S with type k = t (** @closed *)

  (** [eq a b] is whether [a] and [b] are the same kind of silent label with
      equal encodings. Raises nothing. *)
  val eq : t -> t -> bool
end

module Make (Enc : Encoding.S) (M : Rocq_monad_utils.S with type enc = Enc.t) :
  S with type enc = Enc.t
