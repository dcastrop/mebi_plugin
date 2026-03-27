module type S = sig
  include Theories_enc.S

  type fsm

  exception FSM_HasNoSilentLabel of fsm

  val is_fsm_silent_label : EConstr.t -> fsm -> bool

  exception FSM_HasNoVisibleLabel of fsm

  val is_fsm_visible_label : EConstr.t -> fsm -> bool

  exception FSM_HasNoWeakLabels of fsm

  val is_fsm_weak_labels : EConstr.t -> fsm -> bool

  exception FSM_HasNoConstructors of fsm

  val is_fsm_constructor : EConstr.t -> fsm -> bool
end

(** [module Make] ...
    @param I
      is the [module Rocq_monad_utils.S] for the current {i iteration} of the proof-solver.
*)
module Make
    (Log : Logger.S)
    (Enc : Encoding.S)
    (W :
       Wrapper_.S
       with type enc = Enc.t
        and type node = Enc.Tree.Node.t
        and type tree = Enc.Tree.t
        and type trees = Enc.Trees.t)
    (I : Proof_solver_wrapper.S with type enc = Enc.t and type tree = Enc.Tree.t) :
  S
  with type 'a mm = 'a W.M.mm
   and type 'a im = 'a I.mm
   and type enc = Enc.t
   and type fsm = W.Model.FSM.t = struct
  module M = W.M
  module Model = W.Model
  module Decode = W.Decode

  module ThEnc :
    Theories_enc.S
    with type 'a im = 'a I.mm
     and type 'a mm = 'a M.mm
     and type enc = Enc.t =
    Theories_enc.Make (Log) (Enc) (M) (I) (Theories.Make (Log) (Enc) (I))

  include ThEnc

  type fsm = W.Model.FSM.t

  exception FSM_HasNoSilentLabel of Model.FSM.t

  let is_fsm_silent_label (x : EConstr.t) (m : Model.FSM.t) : bool =
    Log.trace __FUNCTION__;
    match
      Model.Labels.filter Model.Label.is_silent m.info.weak_labels
      |> Model.Labels.to_list
    with
    | [] -> raise (FSM_HasNoSilentLabel m)
    | ys -> M.exists_eq x ys Decode.label |> M.run
  ;;

  exception FSM_HasNoVisibleLabel of Model.FSM.t

  (** i.e., not silent label *)
  let is_fsm_visible_label (x : EConstr.t) (m : Model.FSM.t) : bool =
    Log.trace __FUNCTION__;
    match
      Model.Labels.filter
        (fun y -> Model.Label.is_silent y |> Bool.not)
        m.info.weak_labels
      |> Model.Labels.to_list
    with
    | [] -> raise (FSM_HasNoVisibleLabel m)
    | ys -> M.exists_eq x ys Decode.label |> M.run
  ;;

  exception FSM_HasNoWeakLabels of Model.FSM.t

  let is_fsm_weak_labels (x : EConstr.t) (m : Model.FSM.t) : bool =
    Log.trace __FUNCTION__;
    let is_silent = is_fsm_silent_label x m in
    if is_silent then true else is_fsm_visible_label x m
  ;;

  exception FSM_HasNoConstructors of Model.FSM.t

  let is_fsm_constructor (x : EConstr.t) (m : Model.FSM.t) : bool =
    Log.trace __FUNCTION__;
    match m with
    | { info = { meta = None; _ }; _ } -> raise (FSM_HasNoConstructors m)
    | { info = { meta = Some { lts; _ }; _ }; _ } ->
      if is_any_theory x
      then false
      else M.exists_eq x lts Decode.lts_constructor |> M.run
  ;;
end
