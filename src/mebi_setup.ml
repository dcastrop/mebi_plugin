open Logging

let enable_logging : bool ref = ref true

(********************************************)
(****** COQ ENVIRONMENT/CONTEXT *************)
(********************************************)

type coq_context =
  { coq_env : Environ.env
  ; coq_ctx : Evd.evar_map
  }

(** *)
let the_coq_env_opt : Environ.env ref option ref = ref None

let new_coq_env () : Environ.env ref =
  if !enable_logging
  then Log.debug "mebi_setup.new_coq_env: Created new coq env.";
  let env : Environ.env ref = ref (Global.env ()) in
  the_coq_env_opt := Some env;
  env
;;

let the_coq_env ?(fresh : bool = false) () : Environ.env ref =
  match !the_coq_env_opt with
  | None -> new_coq_env ()
  | Some env -> if fresh then new_coq_env () else env
;;

(** *)
let the_coq_ctx_opt : Evd.evar_map ref option ref = ref None

let new_coq_ctx ?(fresh : bool = false) () : Evd.evar_map ref =
  if !enable_logging
  then Log.debug "mebi_setup.new_coq_ctx: Created new coq ctx.";
  let ctx = ref (Evd.from_env !(the_coq_env ~fresh ())) in
  the_coq_ctx_opt := Some ctx;
  ctx
;;

let the_coq_ctx ?(fresh : bool = false) () : Evd.evar_map ref =
  match !the_coq_ctx_opt with
  | None -> new_coq_ctx ~fresh ()
  | Some ctx -> if fresh then new_coq_ctx ~fresh () else ctx
;;

(********************************************)
(****** FORWARD ENCODING MAP ****************)
(********************************************)

module FwdMap : Hashtbl.S with type key = EConstr.t = Hashtbl.Make (struct
    type t = EConstr.t

    let equal t1 t2 = EConstr.eq_constr !(the_coq_ctx ()) t1 t2

    let hash t =
      Constr.hash
        (EConstr.to_constr
           ?abort_on_undefined_evars:(Some false)
           !(the_coq_ctx ())
           t)
    ;;
  end)

module F = FwdMap

(********************************************)
(****** ENCODINGS ***************************)
(********************************************)

module Enc = Mebi_enc.IntEncoding (FwdMap)
module B = Enc.Tbl

(********************************************)
(****** Equalities **************************)
(********************************************)

module EqF =
functor
  (Enc : Mebi_enc.ENCODING_TYPE)
  (* (FwdMap : Hashtbl.S with type key = EConstr.t) *)
  ->
  struct
    let enc = Enc.eq
    let econstr sigma = EConstr.eq_constr sigma
    let constr = Constr.equal
  end

module Eq = EqF (Enc)

(********************************************)
(****** Conversions *************************)
(********************************************)

module Convert = struct
  let constrexpr_to_econstr env sigma
    : Constrexpr.constr_expr -> Evd.evar_map * EConstr.t
    =
    Constrintern.interp_constr_evars env sigma
  ;;

  let econstr_to_constr ?(abort_on_undefined_evars : bool = false) sigma
    : EConstr.t -> Constr.t
    =
    EConstr.to_constr ~abort_on_undefined_evars sigma
  ;;

  let econstr_to_constr_opt sigma : EConstr.t -> Constr.t option =
    EConstr.to_constr_opt sigma
  ;;

  let globref_to_econstr env : Names.GlobRef.t -> EConstr.t =
    fun x -> EConstr.of_constr (UnivGen.constr_of_monomorphic_global env x)
  ;;
end
