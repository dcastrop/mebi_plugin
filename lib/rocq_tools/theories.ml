module type S = sig
  type 'a im

  val is_theory : Evd.econstr -> Evd.econstr -> bool im
  val is_any_theory : Evd.econstr -> bool
  val is_exists : Evd.econstr -> bool im
  val is_weak_sim : Evd.econstr -> bool im
  val is_weak_bisimilar : Evd.econstr -> bool im
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
    (Enc : Encoding.S)
    (M : Rocq_monad_utils.S with type enc = Enc.t and type tree = Enc.Tree.t) :
  S with type 'a im = 'a M.mm = struct
  type 'a im = 'a M.mm

  open M
  module Th = Mebi_theories

  (* See the [.mli]. [x] is split here, in the continuation, so the
     handler runs with the check rather than around building it. *)
  let is_theory (x : EConstr.t) (y : EConstr.t) : bool mm =
    let open Syntax in
    let* sigma = get_sigma in
    match Rocq_utils.econstr_to_atomic sigma x with
    | xty, _tys -> econstr_eq xty y
    | exception
        ( Rocq_utils.Rocq_utils_EConstrIsNotA_Type _
        | Rocq_utils.Rocq_utils_EConstrIsNot_Atomic _ ) ->
      return false
  ;;

  (* See the [.mli]. Compares [x] with every loaded constant, running each
     comparison. *)
  let is_any_theory (x : EConstr.t) : bool =
    Logger.trace __FUNCTION__;
    Th.get_constants ()
    |> Hashtbl.to_seq_values
    |> List.of_seq
    |> List.exists (fun (y : EConstr.t) -> econstr_eq x y |> run)
  ;;

  (* See the [.mli] for every [is_*] below. *)
  let is_exists (x : EConstr.t) : bool mm = is_theory x (Th.get "ex")
  let is_weak_sim (x : EConstr.t) : bool mm = is_theory x (Th.get "weak_sim")

  let is_weak_bisimilar (x : EConstr.t) : bool mm =
    is_theory x (Th.get "weak_bisimilar")
  ;;

  let is_weak (x : EConstr.t) : bool mm = is_theory x (Th.get "weak")
  let is_tau (x : EConstr.t) : bool mm = is_theory x (Th.get "tau")
  let is_silent (x : EConstr.t) : bool mm = is_theory x (Th.get "silent")
  let is_silent1 (x : EConstr.t) : bool mm = is_theory x (Th.get "silent1")
  let is_LTS (x : EConstr.t) : bool mm = is_theory x (Th.get "LTS")
  let is_None (x : EConstr.t) : bool mm = is_theory x (Th.get "None")
  let is_Some (x : EConstr.t) : bool mm = is_theory x (Th.get "Some")
  let is_list (x : EConstr.t) : bool mm = is_theory x (Th.get "list")
  let is_cons (x : EConstr.t) : bool mm = is_theory x (Th.get "cons")
  let is_nil (x : EConstr.t) : bool mm = is_theory x (Th.get "nil")

  (** Raised by {!ensure}. *)
  exception EnsureFail

  (* See the [.mli]. *)
  let ensure (x : EConstr.t) (f : EConstr.t -> bool mm) : unit mm =
    Logger.trace __FUNCTION__;
    let open Syntax in
    let* b = f x in
    if b then return () else raise EnsureFail
  ;;
end
