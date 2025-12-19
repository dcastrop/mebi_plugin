(***********************************************************************)
module Log : Logger.LOGGER_TYPE = Logger.MkDefault ()

let () = Log.Config.configure_output Debug false
let () = Log.Config.configure_output Trace false
(***********************************************************************)

type unif_problem =
  { termL : EConstr.t
  ; termR : EConstr.t
  }
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
  Log.trace __FUNCTION__;
  Log.trace ~__FUNCTION__ "Created new coq env.";
  let env : Environ.env ref = ref (Global.env ()) in
  the_coq_env_opt := Some env;
  env
;;

let the_coq_env ?(fresh : bool = false) () : Environ.env ref =
  Log.trace __FUNCTION__;
  match !the_coq_env_opt with
  | None -> new_coq_env ()
  | Some env -> if fresh then new_coq_env () else env
;;

(** *)
let the_coq_ctx_opt : Evd.evar_map ref option ref = ref None

let new_coq_ctx ?(fresh : bool = false) () : Evd.evar_map ref =
  Log.trace __FUNCTION__;
  Log.trace ~__FUNCTION__ "Created new coq ctx.";
  let ctx : Evd.evar_map ref = ref (Evd.from_env !(the_coq_env ~fresh ())) in
  the_coq_ctx_opt := Some ctx;
  ctx
;;

let the_coq_ctx ?(fresh : bool = false) () : Evd.evar_map ref =
  Log.trace __FUNCTION__;
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

(********************************************)
(****** ENCODINGS ***************************)
(********************************************)

module IntEncoding : Mebi_enc.S = struct
  include Int
  module F = FwdMap

  type t = int

  let init : t = 0
  let next : t -> t = fun x -> x + 1
  let to_string : t -> string = Printf.sprintf "%i"
end

module Enc = Mebi_enc.Make (IntEncoding)

(********************************************)
(****** Equalities **************************)
(********************************************)

module Eq = struct
  let enc = Enc.equal
  let econstr sigma = EConstr.eq_constr sigma
  let constr = Constr.equal
end
