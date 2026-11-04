(** The encodings of theory terms: which encoded term of the current
    encoding table is a given theory term ([None], [Some], ...). *)
module type S = sig
  type 'a mm
  type enc

  include Theories.S (** @closed *)

  (** Raised by the [_if_eq] lookups when the term is not the theory term
      asked about. *)
  exception NotEqTheory

  (** [get_None_enc_if_eq x] is {!get_theory_enc_if_eq} for [None], and
      raises as it does. *)
  val get_None_enc_if_eq : Evd.econstr -> enc mm

  (** [get_Some_enc_if_eq x] is {!get_theory_enc_if_eq} for [Some], and
      raises as it does. *)
  val get_Some_enc_if_eq : Evd.econstr -> enc mm
end

module Make
    (Enc : Encoding.S)
    (M : Rocq_monad_utils.S with type enc = Enc.t and type tree = Enc.Tree.t)
    (I : Rocq_monad_utils.S with type enc = Enc.t and type tree = Enc.Tree.t)
    (Theories : Theories.S with type 'a im = 'a I.mm) :
  S with type 'a mm = 'a M.mm and type 'a im = 'a I.mm and type enc = Enc.t
