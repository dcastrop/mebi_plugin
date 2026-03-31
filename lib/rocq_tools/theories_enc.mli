module type S = sig
  type 'a mm
  type enc

  include Theories.S (** @closed *)

  val get_theory_enc : (Evd.econstr -> bool im) -> enc mm

  exception NoEncodingFoundFor_TheoriesNone

  val get_None_enc : unit -> enc mm

  exception NoEncodingFoundFor_TheoriesSome

  val get_Some_enc : unit -> enc mm

  exception NotEqTheory

  val get_theory_enc_if_eq : Evd.econstr -> (Evd.econstr -> bool im) -> enc mm
  val get_None_enc_if_eq : Evd.econstr -> enc mm
  val get_Some_enc_if_eq : Evd.econstr -> enc mm
end

module Make
    (Log : Logger.S)
    (Enc : Encoding.S)
    (M : Rocq_monad_utils.S with type enc = Enc.t and type tree = Enc.Tree.t)
    (I : Rocq_monad_utils.S with type enc = Enc.t and type tree = Enc.Tree.t)
    (Theories : Theories.S with type 'a im = 'a I.mm) :
  S with type 'a mm = 'a M.mm and type 'a im = 'a I.mm and type enc = Enc.t
