module type S = sig
  type 'a im
  type 'a mm
  type enc
  type fsm

  val is_any_theory : Evd.econstr -> bool
  val is_theory : Evd.econstr -> Evd.econstr -> bool im
  val is_exists : Evd.econstr -> bool im
  val is_weak_sim : Evd.econstr -> bool im
  val is_weak : Evd.econstr -> bool im
  val is_tau : Evd.econstr -> bool im
  val is_silent : Evd.econstr -> bool im
  val is_silent1 : Evd.econstr -> bool im
  val is_LTS : Evd.econstr -> bool im
  val is_None : Evd.econstr -> bool im
  val is_Some : Evd.econstr -> bool im

  exception EnsureFail

  val ensure : Evd.econstr -> (Evd.econstr -> bool im) -> unit im
  val get_theory_enc : (Evd.econstr -> bool im) -> enc mm

  exception NoEncodingFoundFor_TheoriesNone

  val get_None_enc : unit -> enc mm

  exception NoEncodingFoundFor_TheoriesSome

  val get_Some_enc : unit -> enc mm

  exception NotEqTheory

  val get_theory_enc_if_eq : Evd.econstr -> (Evd.econstr -> bool im) -> enc mm
  val get_None_enc_if_eq : Evd.econstr -> enc mm
  val get_Some_enc_if_eq : Evd.econstr -> enc mm

  exception FSM_HasNoSilentLabel of fsm

  val is_fsm_silent_label : Evd.econstr -> fsm -> bool

  exception FSM_HasNoVisibleLabel of fsm

  val is_fsm_visible_label : Evd.econstr -> fsm -> bool

  exception FSM_HasNoWeakLabels of fsm

  val is_fsm_weak_labels : Evd.econstr -> fsm -> bool

  exception FSM_HasNoConstructors of fsm

  val is_fsm_constructor : Evd.econstr -> fsm -> bool
end

(** [module Make] ...
    (* @param M
      is the [module Rocq_monad_utils.S] of the initial run of the algorithm *)
    @param I
      is the [module Rocq_monad_utils.S] for the current {i iteration} of the proof-solver.
*)
module Make
    (_ : Logger.S)
    (Enc : Encoding.S)
    (W :
       Wrapper.S
       with type enc = Enc.t
        and type node = Enc.Tree.Node.t
        and type tree = Enc.Tree.t
        and type trees = Enc.Trees.t)
    (I : Rocq_monad_utils.S with type enc = Enc.t and type tree = Enc.Tree.t) :
  S
  with type 'a mm = 'a W.M.mm
   and type 'a im = 'a I.mm
   and type enc = Enc.t
   and type fsm = W.Model.FSM.t
