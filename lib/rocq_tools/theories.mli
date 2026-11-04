(** Recognising the plugin's theory terms ([weak_sim], [tau], [Some], ...;
    {!Mebi_theories}) in goals and hypotheses. Each [is_*] check is a
    monadic value, run in the current environment. *)
module type S = sig
  type 'a im

  (** [is_any_theory x] is whether [x] equals any of the terms
      {!Mebi_theories} loads. Raises nothing. *)
  val is_any_theory : Evd.econstr -> bool

  (** [is_exists x] is whether [x]'s head is [ex] ([exists]). Each of the
      [is_*] checks below is {!is_theory} against one theory term, and
      like it raises nothing. *)
  val is_exists : Evd.econstr -> bool im

  (** [is_weak_sim x] is whether [x]'s head is [weak_sim]. *)
  val is_weak_sim : Evd.econstr -> bool im

  (** [is_weak_bisimilar x] is whether [x]'s head is [weak_bisimilar]. *)
  val is_weak_bisimilar : Evd.econstr -> bool im

  (** [is_weak x] is whether [x]'s head is [weak] (a weak transition). *)
  val is_weak : Evd.econstr -> bool im

  (** [is_tau x] is whether [x]'s head is [tau]. *)
  val is_tau : Evd.econstr -> bool im

  (** [is_silent x] is whether [x]'s head is [silent]. *)
  val is_silent : Evd.econstr -> bool im

  (** [is_None x] is whether [x]'s head is [None]. *)
  val is_None : Evd.econstr -> bool im

  (** [is_Some x] is whether [x]'s head is [Some]. *)
  val is_Some : Evd.econstr -> bool im

  (** [is_list x] is whether [x]'s head is [list]. *)
  val is_list : Evd.econstr -> bool im

  (** [ensure x f] checks that [f x] holds.

      @raise EnsureFail
        when run, if [f x] is [false] (raised here); and
        whatever [f x] raises (propagated). *)
  val ensure : Evd.econstr -> (Evd.econstr -> bool im) -> unit im
end

module Make
    (Enc : Encoding.S)
    (M : Rocq_monad_utils.S with type enc = Enc.t and type tree = Enc.Tree.t) :
  S with type 'a im = 'a M.mm
