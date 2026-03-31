module type S = sig
  type 'a im

  val is_theory : Evd.econstr -> Evd.econstr -> bool im
  val is_any_theory : Evd.econstr -> bool
  val is_exists : Evd.econstr -> bool im
  val is_weak_sim : Evd.econstr -> bool im
  val is_weak : Evd.econstr -> bool im
  val is_tau : Evd.econstr -> bool im
  val is_silent : Evd.econstr -> bool im
  val is_silent1 : Evd.econstr -> bool im
  val is_LTS : Evd.econstr -> bool im
  val is_None : Evd.econstr -> bool im
  val is_Some : Evd.econstr -> bool im
  val is_list : Evd.econstr -> bool im
  val is_cons : Evd.econstr -> bool im
  val is_nil : Evd.econstr -> bool im
  val ensure : Evd.econstr -> (Evd.econstr -> bool im) -> unit im
end

module Make
    (Log : Logger.S)
    (Enc : Encoding.S)
    (M : Rocq_monad_utils.S with type enc = Enc.t and type tree = Enc.Tree.t) :
  S with type 'a im = 'a M.mm
