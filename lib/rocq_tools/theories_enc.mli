(** The encodings of theory terms: which encoded term of the current
    encoding table is a given theory term ([None], [Some], ...). *)
module type S = sig
  type 'a mm
  type enc

  include Theories.S (** @closed *)

  (** [get_theory_enc f] is the encoding of the first term in the encoding
      table that [f] recognises.

      @raise Not_found when run, if no encoded term satisfies [f] (raised
                       here). *)
  val get_theory_enc : (Evd.econstr -> bool im) -> enc mm

  (** Meant to be raised by {!get_None_enc}; see there. *)
  exception NoEncodingFoundFor_TheoriesNone

  (** [get_None_enc ()] is the encoding of [None].

      @raise Not_found
        when run, if [None] has no encoding (propagated from
        {!get_theory_enc}). The handler meant to turn it into
        {!NoEncodingFoundFor_TheoriesNone} surrounds only the construction
        of the value, so it does not see it ([TODO.md], "try around a
        monadic value"). Unused. *)
  val get_None_enc : unit -> enc mm

  (** Meant to be raised by {!get_Some_enc}; see there. *)
  exception NoEncodingFoundFor_TheoriesSome

  (** [get_Some_enc ()] is the encoding of [Some]; as {!get_None_enc}.

      @raise Not_found
        when run, if [Some] has no encoding (propagated; not
        converted, as for {!get_None_enc}). Unused. *)
  val get_Some_enc : unit -> enc mm

  (** Raised by the [_if_eq] lookups when the term is not the theory term
      asked about. *)
  exception NotEqTheory

  (** [get_theory_enc_if_eq x f] is the encoding of the theory term [f]
      recognises, provided [f] recognises [x] itself.

      @raise NotEqTheory
        if [f] does not recognise [x] (raised here, while
        building the value, so a handler around the call does see it).
      @raise Not_found
        when run, if [f] recognises [x] but no encoded term
        satisfies [f] (propagated from {!get_theory_enc}; not converted). *)
  val get_theory_enc_if_eq : Evd.econstr -> (Evd.econstr -> bool im) -> enc mm

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
