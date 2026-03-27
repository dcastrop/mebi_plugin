module type S = sig
  type 'a mm
  type enc

  include Theories.S

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
  S with type 'a mm = 'a M.mm and type 'a im = 'a I.mm and type enc = Enc.t =
struct
  type 'a mm = 'a M.mm
  type enc = Enc.t

  include Theories

  let get_theory_enc (f : EConstr.t -> bool I.mm) : Enc.t M.mm =
    Log.trace __FUNCTION__;
    let open M.Syntax in
    let* fm = M.get_fwdmap in
    let rec find_theory : (EConstr.t * Enc.t) list -> Enc.t M.mm = function
      | [] -> raise Not_found
      | (x, y) :: tl ->
        let is_match : bool = I.run (f x) in
        if is_match then M.return y else find_theory tl
    in
    M.F.to_seq fm |> List.of_seq |> find_theory
  ;;

  exception NoEncodingFoundFor_TheoriesNone

  let get_None_enc () : Enc.t M.mm =
    Log.trace __FUNCTION__;
    try get_theory_enc is_None with
    | Not_found -> raise NoEncodingFoundFor_TheoriesNone
  ;;

  exception NoEncodingFoundFor_TheoriesSome

  let get_Some_enc () : Enc.t M.mm =
    Log.trace __FUNCTION__;
    try get_theory_enc is_Some with
    | Not_found -> raise NoEncodingFoundFor_TheoriesSome
  ;;

  exception NotEqTheory

  (** *)
  let get_theory_enc_if_eq (x : EConstr.t) (f : EConstr.t -> bool I.mm)
    : Enc.t M.mm
    =
    Log.trace __FUNCTION__;
    let is_eq : bool = I.run (f x) in
    try if is_eq then get_theory_enc f else raise Not_found with
    | Not_found -> raise NotEqTheory
  ;;

  let get_None_enc_if_eq (x : EConstr.t) : Enc.t M.mm =
    Log.trace __FUNCTION__;
    get_theory_enc_if_eq x is_None
  ;;

  let get_Some_enc_if_eq (x : EConstr.t) : Enc.t M.mm =
    Log.trace __FUNCTION__;
    get_theory_enc_if_eq x is_Some
  ;;
end
