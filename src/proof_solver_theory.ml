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
   and type fsm = W.Model.FSM.t = struct
  type 'a im = 'a I.mm
  type 'a mm = 'a W.M.mm
  type enc = Enc.t
  type fsm = W.Model.FSM.t

  module M = W.M
  module Model = W.Model
  module Decode = W.Decode
  open I

  (** [is_any_theory x] is [true] if term [x] is equal to any of the terms presented in [Theories].
  *)
  let is_any_theory (x : EConstr.t) : bool =
    Theories.collect_bisimilarity_theories ()
    |> List.exists (fun (y : EConstr.t) -> econstr_eq x y |> run)
  ;;

  (** [is_theory x y] checks if term [x] is equal to theory term [y], catching the exception thrown when [EConstr.kind_of_type x] is not [AtomicType (ty, tys)].
  *)
  let is_theory (x : EConstr.t) (y : EConstr.t) : bool mm =
    try
      let open Syntax in
      let* ty, tys = to_atomic x in
      econstr_eq y ty
    with
    | Rocq_utils.Rocq_utils_EConstrIsNotA_Type _ -> return false
  ;;

  (** exists *)
  let is_exists (x : EConstr.t) : bool mm = is_theory x (Theories.c_ex ())

  (** weak simulation*)
  let is_weak_sim (x : EConstr.t) : bool mm =
    is_theory x (Theories.c_weak_sim ())
  ;;

  (** weak transition *)
  let is_weak (x : EConstr.t) : bool mm = is_theory x (Theories.c_weak ())

  let is_tau (x : EConstr.t) : bool mm = is_theory x (Theories.c_tau ())
  let is_silent (x : EConstr.t) : bool mm = is_theory x (Theories.c_silent ())
  let is_silent1 (x : EConstr.t) : bool mm = is_theory x (Theories.c_silent1 ())
  let is_LTS (x : EConstr.t) : bool mm = is_theory x (Theories.c_LTS ())
  let is_None (x : EConstr.t) : bool mm = is_theory x (Theories.c_None ())
  let is_Some (x : EConstr.t) : bool mm = is_theory x (Theories.c_Some ())

  exception EnsureFail

  (** assert *)
  let ensure (x : EConstr.t) (f : EConstr.t -> bool mm) : unit mm =
    let open Syntax in
    let* b = f x in
    if b then return () else raise EnsureFail
  ;;

  (** *)
  let get_theory_enc (f : EConstr.t -> bool mm) : Enc.t M.mm =
    let open M.Syntax in
    let* fm = M.get_fwdmap in
    let rec find_theory : (EConstr.t * Enc.t) list -> Enc.t M.mm = function
      | [] -> raise Not_found
      | (x, y) :: tl ->
        let is_match : bool = run (f x) in
        if is_match then M.return y else find_theory tl
    in
    M.F.to_seq fm |> List.of_seq |> find_theory
  ;;

  exception NoEncodingFoundFor_TheoriesNone

  let get_None_enc () : Enc.t M.mm =
    try get_theory_enc is_None with
    | Not_found -> raise NoEncodingFoundFor_TheoriesNone
  ;;

  exception NoEncodingFoundFor_TheoriesSome

  let get_Some_enc () : Enc.t M.mm =
    try get_theory_enc is_Some with
    | Not_found -> raise NoEncodingFoundFor_TheoriesSome
  ;;

  exception NotEqTheory

  (** *)
  let get_theory_enc_if_eq (x : EConstr.t) (f : EConstr.t -> bool mm)
    : Enc.t M.mm
    =
    let is_eq : bool = run (f x) in
    try if is_eq then get_theory_enc f else raise Not_found with
    | Not_found -> raise NotEqTheory
  ;;

  let get_None_enc_if_eq (x : EConstr.t) : Enc.t M.mm =
    get_theory_enc_if_eq x is_None
  ;;

  let get_Some_enc_if_eq (x : EConstr.t) : Enc.t M.mm =
    get_theory_enc_if_eq x is_Some
  ;;

  exception FSM_HasNoSilentLabel of Model.FSM.t

  let is_fsm_silent_label (x : EConstr.t) (m : Model.FSM.t) : bool =
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
    let is_silent = is_fsm_silent_label x m in
    if is_silent then true else is_fsm_visible_label x m
  ;;

  exception FSM_HasNoConstructors of Model.FSM.t

  let is_fsm_constructor (x : EConstr.t) (m : Model.FSM.t) : bool =
    match m with
    | { info = { meta = None; _ }; _ } -> raise (FSM_HasNoConstructors m)
    | { info = { meta = Some { lts; _ }; _ }; _ } ->
      if is_any_theory x
      then false
      else M.exists_eq x lts Decode.lts_constructor |> M.run
  ;;
end
