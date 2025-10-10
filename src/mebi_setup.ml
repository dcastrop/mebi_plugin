open Logging

let enable_logging : bool ref = ref true

(********************************************)
(****** COQ ENVIRONMENT/CONTEXT *************)
(********************************************)

(* TODO: move proof stuff to own monad/wrapper *)
type proof_context =
  { mutable proof : Declare.Proof.t option
  ; mutable names : Names.Id.Set.t option
  }

type coq_context =
  { coq_env : Environ.env
  ; coq_ctx : Evd.evar_map
  ; proofv : proof_context
  }

(* *)
let the_proofv_opt : proof_context ref option ref = ref None

let new_proofv (proof : Declare.Proof.t option) (names : Names.Id.Set.t option)
  : proof_context ref
  =
  Log.trace "mebi_setup.new_proofv: Created new proofv.";
  let the_proofv : proof_context ref = ref { proof; names } in
  the_proofv_opt := Some the_proofv;
  the_proofv
;;

let the_coq_proofv
      ?(new_proof : bool = false)
      ?(proof : Declare.Proof.t option = None)
      ?(names : Names.Id.Set.t option = None)
      ()
  : proof_context ref
  =
  Log.trace "mebi_setup.the_coq_proofv";
  match !the_proofv_opt with
  | None ->
    Log.debug "mebi_setup.the_coq_proofv: proofv is None, using args";
    new_proofv proof names
  | Some proofv ->
    if new_proof
    then (
      Log.debug "mebi_setup.the_coq_proofv: new proof";
      new_proofv proof names)
    else (
      let the_proof =
        match !proofv.proof with
        | None ->
          Log.debug
            "mebi_setup.the_coq_proofv: proofv.proof is None, using proof arg";
          proof
        | Some _ ->
          (match proof with
           | None ->
             Log.debug
               "mebi_setup.the_coq_proofv: proofv.proof is Some and proof arg \
                is None, preserving proofv.proof";
             !proofv.proof
           | Some q ->
             Log.debug
               "mebi_setup.the_coq_proofv: proofv.proof and proof arg are \
                Some, overriding, using new proof arg";
             proof)
      in
      let the_names =
        match !proofv.names with
        | None ->
          Log.debug
            "mebi_setup.the_coq_proofv: proofv.names is None, using names arg";
          names
        | Some _ ->
          (match names with
           | None ->
             Log.debug
               "mebi_setup.the_coq_proofv: proofv.names is Some and names arg \
                is None, preserving proofv.names";
             !proofv.names
           | Some q ->
             Log.debug
               "mebi_setup.the_coq_proofv: proofv.names and names arg are \
                Some, overriding, using new names arg";
             names)
      in
      new_proofv the_proof the_names)
;;

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

module E = Mebi_enc.IntEncoding (FwdMap)
module B = E.Tbl
