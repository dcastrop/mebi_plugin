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
  val ensure : Evd.econstr -> (Evd.econstr -> bool im) -> unit im
end

module Make
    (Log : Logger.S)
    (Enc : Encoding.S)
    (M : Rocq_monad_utils.S with type enc = Enc.t and type tree = Enc.Tree.t) :
  S with type 'a im = 'a M.mm = struct
  type 'a im = 'a M.mm

  open M
  module Th = Mebi_theories

  (** [is_theory x y] checks if term [x] is equal to theory term [y], catching the exception thrown when [EConstr.kind_of_type x] is not [AtomicType (ty, tys)].
  *)
  let is_theory (x : EConstr.t) (y : EConstr.t) : bool mm =
    try
      let open Syntax in
      let* xty, _tys = to_atomic x in
      econstr_eq xty y
    with
    | Rocq_utils.Rocq_utils_EConstrIsNotA_Type _ -> return false
  ;;

  (** [is_any_theory x] is [true] if term [x] is equal to any of the terms presented in [Mebi_theories].
  *)
  let is_any_theory (x : EConstr.t) : bool =
    Log.trace __FUNCTION__;
    Th.collect_bisimilarity_theories ()
    |> List.exists (fun (y : EConstr.t) -> econstr_eq x y |> run)
  ;;

  (** rocq exists *)
  let is_exists (x : EConstr.t) : bool mm = is_theory x (Th.c_ex ())

  (** weak simulation*)
  let is_weak_sim (x : EConstr.t) : bool mm = is_theory x (Th.c_weak_sim ())

  (** weak transition *)
  let is_weak (x : EConstr.t) : bool mm = is_theory x (Th.c_weak ())

  (** tau transition *)
  let is_tau (x : EConstr.t) : bool mm = is_theory x (Th.c_tau ())

  let is_silent (x : EConstr.t) : bool mm = is_theory x (Th.c_silent ())
  let is_silent1 (x : EConstr.t) : bool mm = is_theory x (Th.c_silent1 ())
  let is_LTS (x : EConstr.t) : bool mm = is_theory x (Th.c_LTS ())
  let is_None (x : EConstr.t) : bool mm = is_theory x (Th.c_None ())
  let is_Some (x : EConstr.t) : bool mm = is_theory x (Th.c_Some ())

  exception EnsureFail

  (** [ensure x f] is a custom assertion for [f x] being [true]. @raise EnsureFail if [f x] is [false]. *)
  let ensure (x : EConstr.t) (f : EConstr.t -> bool mm) : unit mm =
    Log.trace __FUNCTION__;
    let open Syntax in
    let* b = f x in
    if b then return () else raise EnsureFail
  ;;
end
