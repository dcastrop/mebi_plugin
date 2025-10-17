open Logging
open Mebi_setup
module F = Mebi_setup.F
module Enc = Mebi_setup.Enc
module B = Mebi_setup.B

(********************************************)
(****** WRAPPER & CONTEXT *******************)
(********************************************)

let the_enc_maps_cache : (Enc.t F.t * EConstr.t B.t) ref option ref = ref None

let get_the_enc_maps ?(keep_encoding : bool = false) ()
  : (Enc.t F.t * EConstr.t B.t) ref
  =
  match !the_enc_maps_cache with
  | None ->
    let the_fwd_map : Enc.t F.t = F.create 0 in
    let the_bck_map : EConstr.t B.t = B.create 0 in
    let the_enc_maps = ref (the_fwd_map, the_bck_map) in
    the_enc_maps_cache := Some the_enc_maps;
    the_enc_maps
  | Some the_enc_maps ->
    if keep_encoding
    then the_enc_maps
    else (
      let the_fwd_map : Enc.t F.t = F.create 0 in
      let the_bck_map : EConstr.t B.t = B.create 0 in
      let the_enc_maps = ref (the_fwd_map, the_bck_map) in
      the_enc_maps_cache := Some the_enc_maps;
      the_enc_maps)
;;

type wrapper =
  { coq_ref : coq_context ref
  ; fwd_enc : Enc.t F.t
  ; bck_enc : EConstr.t B.t
  }

type 'a in_context =
  { state : wrapper ref
  ; value : 'a
  }

type 'a mm = wrapper ref -> 'a in_context

(** [run x] initializes the monad, and runs [x].
    @param ?keep_encoding
      is [true] when this is called mid-run.
      Enc.g., via [econstr_to_string]
    @param x is the command to run inside the [wrapper] state monad. *)
let run
      ?(keep_encoding : bool = false)
      ?(fresh : bool = true)
      (* ?(new_proof : bool = false) *)
      (* ?(proof : Declare.Proof.t option = None) *)
        (x : 'a mm)
  : 'a
  =
  Log.trace "mebi_wrapper.run";
  let coq_env : Environ.env = !(the_coq_env ~fresh ()) in
  let coq_ctx : Evd.evar_map = !(the_coq_ctx ()) in
  (* let proofv : proof_context = !(the_coq_proofv ~new_proof ~proof ()) in *)
  (* let coq_ref : coq_context ref = ref { coq_env; coq_ctx; proofv } in *)
  let coq_ref : coq_context ref = ref { coq_env; coq_ctx } in
  let fwd_enc, bck_enc = !(get_the_enc_maps ~keep_encoding ()) in
  let a = x (ref { coq_ref; fwd_enc; bck_enc }) in
  (* enable_logging := false; *)
  the_enc_maps_cache := Some (ref (!(a.state).fwd_enc, !(a.state).bck_enc));
  a.value
;;

let return (x : 'a) : 'a mm =
  fun (st : wrapper ref) -> { state = st; value = x }
[@@inline always]
;;

let bind (x : 'a mm) (f : 'a -> 'b mm) : 'b mm =
  fun (st : wrapper ref) ->
  let a = x st in
  f a.value a.state
[@@inline always]
;;

let map (f : 'a -> 'b) (x : 'a mm) : 'b mm =
  fun (st : wrapper ref) ->
  let x_st = x st in
  { x_st with value = f x_st.value }
[@@inline always]
;;

let product (x : 'a mm) (y : 'b mm) : ('a * 'b) mm =
  bind x (fun a -> bind y (fun b -> return (a, b)))
[@@inline always]
;;

(** Monadic for loop *)
let rec iterate
          (from_idx : int)
          (to_idx : int)
          (acc : 'a)
          (f : int -> 'a -> 'a mm)
  : 'a mm
  =
  if from_idx > to_idx
  then return acc
  else bind (f from_idx acc) (fun acc' -> iterate (from_idx + 1) to_idx acc' f)
;;

(********************************************)
(****** GET & PUT STATE *********************)
(********************************************)

(* let set_proof (new_proof : Declare.Proof.t) (st : wrapper ref) : unit in_context
  =
  Log.trace "mebi_wrapper.set_proof";
  let coq_st = !st.coq_ref in
  let names = !coq_st.proofv.names in
  let proofv = !(the_coq_proofv ~proof:(Some new_proof) ~names ()) in
  coq_st := { !coq_st with proofv };
  st := { !st with coq_ref = coq_st };
  { state = st; value = () }
;; *)

let get_env (st : wrapper ref) : Environ.env in_context =
  let coq_st = !st.coq_ref in
  { state = st; value = !coq_st.coq_env }
;;

let get_sigma (st : wrapper ref) : Evd.evar_map in_context =
  let coq_st = !st.coq_ref in
  { state = st; value = !coq_st.coq_ctx }
;;

(* let get_proofv (st : wrapper ref) : proof_context in_context =
  let coq_st = !st.coq_ref in
  { state = st; value = !coq_st.proofv }
;; *)

let get_fwd_enc (st : wrapper ref) : Enc.t F.t in_context =
  Log.trace "mebi_wrapper.get_fwd_enc";
  { state = st; value = !st.fwd_enc }
;;

let get_bck_enc (st : wrapper ref) : EConstr.t B.t in_context =
  Log.trace "mebi_wrapper.get_bck_enc";
  { state = st; value = !st.bck_enc }
;;

let state
      (f : Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
      (st : wrapper ref)
  : 'a in_context
  =
  let coq_st = !st.coq_ref in
  let sigma, a = f !coq_st.coq_env !coq_st.coq_ctx in
  coq_st := { !coq_st with coq_ctx = sigma };
  { state = st; value = a }
;;

let sandbox (m : 'a mm) (st : wrapper ref) : 'a in_context =
  let st_contents = !st in
  let res = m st in
  st := st_contents;
  { state = st; value = res.value }
;;

(********************************************)
(****** SYNTAX ******************************)
(********************************************)

module type MEBI_MONAD_SYNTAX = sig
  val ( let+ ) : 'a mm -> ('a -> 'b) -> 'b mm
  val ( let* ) : 'a mm -> ('a -> 'b mm) -> 'b mm

  val ( let$ )
    :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
    -> ('a -> 'b mm)
    -> 'b mm

  val ( let$* )
    :  (Environ.env -> Evd.evar_map -> Evd.evar_map)
    -> (unit -> 'b mm)
    -> 'b mm

  val ( let$+ ) : (Environ.env -> Evd.evar_map -> 'a) -> ('a -> 'b mm) -> 'b mm
  val ( and+ ) : 'a mm -> 'b mm -> ('a * 'b) mm
end

module Syntax : MEBI_MONAD_SYNTAX = struct
  let ( let+ ) x f = map f x
  let ( let* ) = bind
  let ( let$ ) f g = bind (state f) g
  let ( let$* ) f g = bind (state (fun e s -> f e s, ())) g
  let ( let$+ ) f g = bind (state (fun e s -> s, f e s)) g
  let ( and+ ) x y = product x y
end

(**********************************)
(****** COQ TERMS *****************)
(**********************************)

let econstr_eq (a : EConstr.t) (b : EConstr.t) : bool mm =
  let open Syntax in
  let* sigma = get_sigma in
  return (Eq.econstr sigma a b)
;;

let is_none_term (x : EConstr.t) : bool mm =
  fun (state : wrapper ref) ->
  let sigma : Evd.evar_map = !(!state.coq_ref).coq_ctx in
  { state; value = Mebi_theories.is_constant sigma x Mebi_theories.c_None }
;;

(* let env : Environ.env = !(!state.coq_ref).coq_env in
  let sigma : Evd.evar_map = !(!state.coq_ref).coq_ctx in
  let sigma, y = Mebi_setup.the_none_term env sigma in
  state
  := { !state with coq_ref = ref { !(!state.coq_ref) with coq_ctx = sigma } };
  (* Log.debug
     (Printf.sprintf
     "mebi_wrapper.is_none_term, kind\n%s"
     (Strfy.econstr_kind env sigma (Mebi_theories.c_None ()))); *)
  match Constr.kind (Mebi_setup.Convert.econstr_to_constr sigma x) with
  | App (cx, _) ->
    let cy = Mebi_setup.Convert.econstr_to_constr sigma y in
    Log.debug
      (Printf.sprintf
         "mebi_wrapper.is_none_term\n\
          - x: %s\n\
          - none: %s\n\
          - econstr equal: %b\n\
          - constr equal: %b"
         (Strfy.econstr env sigma x)
         (Strfy.econstr env sigma (Mebi_theories.c_None ()))
         (Mebi_setup.Eq.econstr sigma x y)
         (Mebi_setup.Eq.constr cx cy));
    { state; value = Mebi_setup.Eq.constr cx cy }
  | _ -> { state; value = false } *)

let type_of_econstr (t : EConstr.t) : EConstr.t mm =
  fun (state : wrapper ref) ->
  let env : Environ.env = !(!state.coq_ref).coq_env in
  let sigma : Evd.evar_map = !(!state.coq_ref).coq_ctx in
  let t : EConstr.t = Reductionops.nf_all env sigma t in
  let sigma, t = Typing.type_of env sigma t in
  (* let coq_ref = ref { !(!state.coq_ref) with coq_ctx = sigma } in *)
  (* state := { !state with coq_ref }; *)
  state
  := { !state with coq_ref = ref { !(!state.coq_ref) with coq_ctx = sigma } };
  { state; value = t }

and new_evar_of_econstr (t : EConstr.t) : EConstr.t mm =
  fun (state : wrapper ref) ->
  let env : Environ.env = !(!state.coq_ref).coq_env in
  let sigma : Evd.evar_map = !(!state.coq_ref).coq_ctx in
  let sigma, instance = Evarutil.new_evar env sigma t in
  (* coq_st := { !coq_st with coq_ctx = sigma }; *)
  (* { state = st; value = instance } *)
  state
  := { !state with coq_ref = ref { !(!state.coq_ref) with coq_ctx = sigma } };
  { state; value = t }

and constrexpr_to_econstr (t : Constrexpr.constr_expr) : EConstr.t mm =
  fun (state : wrapper ref) ->
  let env : Environ.env = !(!state.coq_ref).coq_env in
  let sigma : Evd.evar_map = !(!state.coq_ref).coq_ctx in
  let sigma, t = Convert.constrexpr_to_econstr env sigma t in
  (* coq_st := { !coq_st with coq_ctx = sigma };
  { state = st; value = t } *)
  state
  := { !state with coq_ref = ref { !(!state.coq_ref) with coq_ctx = sigma } };
  { state; value = t }

and econstr_to_constr ?(abort_on_undefined_evars : bool = false) (x : EConstr.t)
  : Constr.t mm
  =
  let open Syntax in
  let* sigma = get_sigma in
  return (Convert.econstr_to_constr ~abort_on_undefined_evars sigma x)

and econstr_to_constr_opt (x : EConstr.t) : Constr.t option mm =
  let open Syntax in
  let* sigma = get_sigma in
  return (Convert.econstr_to_constr_opt sigma x)

and globref_to_econstr (x : Names.GlobRef.t) : EConstr.t mm =
  let open Syntax in
  let* env = get_env in
  return (Convert.globref_to_econstr env x)

and normalize_econstr (t : EConstr.t) : EConstr.t mm =
  fun (state : wrapper ref) ->
  let env : Environ.env = !(!state.coq_ref).coq_env in
  let sigma : Evd.evar_map = !(!state.coq_ref).coq_ctx in
  let t : EConstr.t = Reductionops.nf_all env sigma t in
  { state; value = t }
;;

(**********************************)
(****** UTILS *********************)
(**********************************)

let map_list_mm (f : 'a -> 'b mm) (ls : 'a list) : 'b list mm =
  let ls : 'a list = List.rev ls in
  let open Syntax in
  (* let* env = get_env in *)
  let iter_body (i : int) (acc : 'b list) =
    let x : 'a = List.nth ls i in
    let* y : 'b = f x in
    return (y :: acc)
  in
  iterate 0 (List.length ls - 1) [] iter_body
;;

let type_of_constrexpr (tref : Constrexpr.constr_expr) : EConstr.t mm =
  let open Syntax in
  let* t : EConstr.t = constrexpr_to_econstr tref in
  type_of_econstr t
;;

let rec econstr_list_to_constr ?(abort_on_undefined_evars : bool = false)
  : EConstr.t list -> Constr.t list mm
  = function
  | [] -> return []
  | h :: t ->
    let open Syntax in
    let* h = econstr_to_constr ~abort_on_undefined_evars h in
    let* t = econstr_list_to_constr ~abort_on_undefined_evars t in
    return (h :: t)
;;

let rec econstr_list_to_constr_opt ?(abort_on_undefined_evars : bool = false)
  : EConstr.t list -> Constr.t option list mm
  = function
  | [] -> return []
  | h :: t ->
    let open Syntax in
    let* h = econstr_to_constr_opt h in
    let* t = econstr_list_to_constr_opt t in
    return (h :: t)
;;

(**********************************)
(****** COQ TERM TO STRING ********)
(**********************************)

let wrap (f : Environ.env -> Evd.evar_map -> 'a -> string) : 'a -> string =
  run
    ~keep_encoding:true
    ~fresh:false
    (let open Syntax in
     let* env = get_env in
     let* sigma = get_sigma in
     return (f env sigma))
;;

(* TODO: point to Strfy, use wrap to provide env and sigma *)
(* module Strfy = struct
  let constr x = (wrap Strfy.constr) x


let constr_rel_decl x : string =
  (wrap Strfy.constr_rel_decl) x
;;

let econstr x : string = (wrap Strfy.econstr) x
let term x : string = econstr  x

let econstr_rel_decl x : string =
  (wrap Strfy.econstr_rel_decl) x
;;

(* let econstr_list_to_constr_opt_string (es : EConstr.t list) : string mm =
  let open Syntax in
  let* es = econstr_list_to_constr_opt es in
  return (Strfy.list (Strfy.option (wrap Strfy.constr)) es)
;; *)
end *)

let constr_to_string (x : Constr.t) : string = (wrap Strfy.constr) x

let constr_rel_decl_to_string (x : Constr.rel_declaration) : string =
  (wrap Strfy.constr_rel_decl) x
;;

let econstr_to_string (x : EConstr.t) : string = (wrap Strfy.econstr) x

let econstr_rel_decl_to_string (x : EConstr.rel_declaration) : string =
  (wrap Strfy.econstr_rel_decl) x
;;

let econstr_list_to_constr_opt_string (es : EConstr.t list) : string mm =
  let open Syntax in
  let* es = econstr_list_to_constr_opt es in
  return (Strfy.list (Strfy.option (wrap Strfy.constr)) es)
;;

(********************************************)
(****** ERRORS ******************************)
(********************************************)

module type ERROR_TYPE = sig
  type mebi_error =
    | Invalid_KindOfTypeEConstr_Expected_Atomic of
        (Mebi_setup.coq_context ref * EConstr.t)
    | Invalid_KindOfTypeEConstr_Expected_Cast of
        (Mebi_setup.coq_context ref * EConstr.t)
    | Invalid_KindOfTypeEConstr_Expected_LetIn of
        (Mebi_setup.coq_context ref * EConstr.t)
    | Invalid_KindOfTypeEConstr_Expected_Prod of
        (Mebi_setup.coq_context ref * EConstr.t)
    | Invalid_KindOfTypeEConstr_Expected_Sort of
        (Mebi_setup.coq_context ref * EConstr.t)
    (* *)
    | UnknownEncodeKey of (coq_context ref * B.key F.t * EConstr.t)
    | UnknownDecodeKey of (coq_context ref * EConstr.t B.t * Enc.t)
    (* *)
    | NoBisimResult of unit
    (* | ProofvIsNone of unit *)
    | ParamsFailIfIncomplete of unit
    | ParamsFailIfNotBisim of unit
    | InvalidLTSArgsLength of int
    | InvalidLTSTermKind of Environ.env * Evd.evar_map * Constr.t
    | InvalidLTSSort of Sorts.family
    | InvalidTypeSort of Sorts.family
    | InvalidArity of Environ.env * Evd.evar_map * Constr.types
    | InvalidRefLTS of Names.GlobRef.t
    | InvalidRefType of Names.GlobRef.t
    | UnknownTermType of
        (Environ.env * Evd.evar_map * (EConstr.t * EConstr.t * EConstr.t list))
    | PrimaryLTSNotFound of
        (Environ.env * Evd.evar_map * EConstr.t * EConstr.t list)
    (* | UnknownDecodeKey of (Environ.env * Evd.evar_map * Enc.t * EConstr.t B.t) *)
    | ExpectedCoqIndDefOfLTSNotType of unit
    | InvalidCheckUpdatedCtx of
        (Environ.env
        * Evd.evar_map
        * EConstr.t list
        * EConstr.rel_declaration list)

  exception MEBI_exn of mebi_error

  val invalid_kind_of_econstr_expected_atomic
    :  coq_context ref
    -> EConstr.t
    -> exn

  val invalid_kind_of_econstr_expected_cast
    :  coq_context ref
    -> EConstr.t
    -> exn

  val invalid_kind_of_econstr_expected_letin
    :  coq_context ref
    -> EConstr.t
    -> exn

  val invalid_kind_of_econstr_expected_prod
    :  coq_context ref
    -> EConstr.t
    -> exn

  val invalid_kind_of_econstr_expected_sort
    :  coq_context ref
    -> EConstr.t
    -> exn

  val cannot_get_encoding_of_unencoded_econstr
    :  coq_context ref
    -> B.key F.t
    -> EConstr.t
    -> exn

  val cannot_get_decoding_of_unencoded_econstr
    :  coq_context ref
    -> EConstr.t B.t
    -> Enc.t
    -> exn

  (* *)
  val missing_bisim_result : unit -> exn

  (* val proofv_is_none : unit -> exn *)
  val params_fail_if_incomplete : unit -> exn
  val params_fail_if_not_bisim : unit -> exn
  val invalid_lts_args_length : int -> exn
  val invalid_lts_term_kind : Environ.env -> Evd.evar_map -> Constr.t -> exn
  val invalid_sort_lts : Sorts.family -> exn
  val invalid_sort_type : Sorts.family -> exn
  val invalid_arity : Environ.env -> Evd.evar_map -> Constr.types -> exn
  val invalid_ref_lts : Names.GlobRef.t -> exn
  val invalid_ref_type : Names.GlobRef.t -> exn
  val invalid_cindef_kind : unit -> exn

  val unknown_term_type
    :  Environ.env
    -> Evd.evar_map
    -> EConstr.t * EConstr.t * EConstr.t list
    -> exn

  val primary_lts_not_found
    :  Environ.env
    -> Evd.evar_map
    -> EConstr.t
    -> EConstr.t list
    -> exn

  (* val unknown_decode_key
     :  Environ.env
     -> Evd.evar_map
     -> Enc.t
     -> EConstr.t B.t
     -> exn *)

  val invalid_check_updated_ctx
    :  Environ.env
    -> Evd.evar_map
    -> EConstr.t list
    -> EConstr.rel_declaration list
    -> exn
end

module Error : ERROR_TYPE = struct
  type mebi_error =
    | Invalid_KindOfTypeEConstr_Expected_Atomic of
        (Mebi_setup.coq_context ref * EConstr.t)
    | Invalid_KindOfTypeEConstr_Expected_Cast of
        (Mebi_setup.coq_context ref * EConstr.t)
    | Invalid_KindOfTypeEConstr_Expected_LetIn of
        (Mebi_setup.coq_context ref * EConstr.t)
    | Invalid_KindOfTypeEConstr_Expected_Prod of
        (Mebi_setup.coq_context ref * EConstr.t)
    | Invalid_KindOfTypeEConstr_Expected_Sort of
        (Mebi_setup.coq_context ref * EConstr.t)
    (* *)
    | UnknownEncodeKey of (coq_context ref * B.key F.t * EConstr.t)
    | UnknownDecodeKey of (coq_context ref * EConstr.t B.t * Enc.t)
    (* *)
    | NoBisimResult of unit
    (* | ProofvIsNone of unit *)
    | ParamsFailIfIncomplete of unit
    | ParamsFailIfNotBisim of unit
    | InvalidLTSArgsLength of int
    | InvalidLTSTermKind of Environ.env * Evd.evar_map * Constr.t
    | InvalidLTSSort of Sorts.family
    | InvalidTypeSort of Sorts.family
    | InvalidArity of Environ.env * Evd.evar_map * Constr.types
    | InvalidRefLTS of Names.GlobRef.t
    | InvalidRefType of Names.GlobRef.t
    | UnknownTermType of
        (Environ.env * Evd.evar_map * (EConstr.t * EConstr.t * EConstr.t list))
    | PrimaryLTSNotFound of
        (Environ.env * Evd.evar_map * EConstr.t * EConstr.t list)
    (* | UnknownDecodeKey of (Environ.env * Evd.evar_map * Enc.t * EConstr.t B.t) *)
    | ExpectedCoqIndDefOfLTSNotType of unit
    | InvalidCheckUpdatedCtx of
        (Environ.env
        * Evd.evar_map
        * EConstr.t list
        * EConstr.rel_declaration list)

  exception MEBI_exn of mebi_error

  let invalid_kind_of_econstr_expected_atomic coq_ref x =
    MEBI_exn (Invalid_KindOfTypeEConstr_Expected_Atomic (coq_ref, x))
  ;;

  let invalid_kind_of_econstr_expected_cast coq_ref x =
    MEBI_exn (Invalid_KindOfTypeEConstr_Expected_Cast (coq_ref, x))
  ;;

  let invalid_kind_of_econstr_expected_letin coq_ref x =
    MEBI_exn (Invalid_KindOfTypeEConstr_Expected_LetIn (coq_ref, x))
  ;;

  let invalid_kind_of_econstr_expected_prod coq_ref x =
    MEBI_exn (Invalid_KindOfTypeEConstr_Expected_Prod (coq_ref, x))
  ;;

  let invalid_kind_of_econstr_expected_sort coq_ref x =
    MEBI_exn (Invalid_KindOfTypeEConstr_Expected_Sort (coq_ref, x))
  ;;

  let cannot_get_encoding_of_unencoded_econstr coq_ref fwd_map x =
    MEBI_exn (UnknownEncodeKey (coq_ref, fwd_map, x))
  ;;

  let cannot_get_decoding_of_unencoded_econstr coq_ref bck_map x =
    MEBI_exn (UnknownDecodeKey (coq_ref, bck_map, x))
  ;;

  let missing_bisim_result () = MEBI_exn (NoBisimResult ())

  (* let proofv_is_none () = MEBI_exn (ProofvIsNone ()) *)
  let params_fail_if_incomplete () = MEBI_exn (ParamsFailIfIncomplete ())
  let params_fail_if_not_bisim () = MEBI_exn (ParamsFailIfNotBisim ())

  (** Assert args length == 3 in [Command.extract_args]. *)
  let invalid_lts_args_length i = MEBI_exn (InvalidLTSArgsLength i)

  (** Assert Constr.kind tm is App _ in [Command.extract_args]. *)
  let invalid_lts_term_kind env sigma x =
    MEBI_exn (InvalidLTSTermKind (env, sigma, x))
  ;;

  (** Error when input LTS has the wrong arity *)
  let invalid_sort_lts f = MEBI_exn (InvalidLTSSort f)

  (** Error when input Type has the wrong arity *)
  let invalid_sort_type f = MEBI_exn (InvalidTypeSort f)

  (** Error when input LTS has the wrong Sort *)
  let invalid_arity env sigma t = MEBI_exn (InvalidArity (env, sigma, t))

  (** Error when input LTS reference is invalid (e.g. non existing) *)
  let invalid_ref_lts r = MEBI_exn (InvalidRefLTS r)

  let invalid_ref_type r = MEBI_exn (InvalidRefType r)

  (** Error when input LTS reference is invalid (e.g. non existing) *)
  let invalid_cindef_kind () = MEBI_exn (ExpectedCoqIndDefOfLTSNotType ())

  (** Error when term is of unknown type *)
  let unknown_term_type env sigma tmty =
    MEBI_exn (UnknownTermType (env, sigma, tmty))
  ;;

  (** Error when multiple coq-LTS provided, but none of them match term. *)
  let primary_lts_not_found env sigma t names =
    MEBI_exn (PrimaryLTSNotFound (env, sigma, t, names))
  ;;

  (** Error when multiple coq-LTS provided, but none of them match term. *)
  (* let unknown_decode_key env sigma k bckmap =
     MEBI_exn (UnknownDecodeKey (env, sigma, k, bckmap))
     ;; *)

  let invalid_check_updated_ctx env sigma x y =
    MEBI_exn (InvalidCheckUpdatedCtx (env, sigma, x, y))
  ;;

  open Pp

  let mebi_handler = function
    | Invalid_KindOfTypeEConstr_Expected_Atomic (coq_ref, x) ->
      str
        (Printf.sprintf
           "Invalid Kind of type EConstr, expected Atomic, but got: %s"
           (Strfy.econstr_types !coq_ref.coq_env !coq_ref.coq_ctx x))
    | Invalid_KindOfTypeEConstr_Expected_Cast (coq_ref, x) ->
      str
        (Printf.sprintf
           "Invalid Kind of type EConstr, expected Cast, but got: %s"
           (Strfy.econstr_types !coq_ref.coq_env !coq_ref.coq_ctx x))
    | Invalid_KindOfTypeEConstr_Expected_LetIn (coq_ref, x) ->
      str
        (Printf.sprintf
           "Invalid Kind of type EConstr, expected LetIn, but got: %s"
           (Strfy.econstr_types !coq_ref.coq_env !coq_ref.coq_ctx x))
    | Invalid_KindOfTypeEConstr_Expected_Prod (coq_ref, x) ->
      str
        (Printf.sprintf
           "Invalid Kind of type EConstr, expected Prod, but got: %s"
           (Strfy.econstr_types !coq_ref.coq_env !coq_ref.coq_ctx x))
    | Invalid_KindOfTypeEConstr_Expected_Sort (coq_ref, x) ->
      str
        (Printf.sprintf
           "Invalid Kind of type EConstr, expected Sort, but got: %s"
           (Strfy.econstr_types !coq_ref.coq_env !coq_ref.coq_ctx x))
    | UnknownEncodeKey (coq_ref, fwd_map, x) ->
      str
        (Printf.sprintf
           "Unknown encode key: %s\nEncode map: %s"
           (Strfy.econstr !coq_ref.coq_env !coq_ref.coq_ctx x)
           (Strfy.list
              (Strfy.tuple
                 (Strfy.econstr !coq_ref.coq_env !coq_ref.coq_ctx)
                 Enc.to_string)
              (List.of_seq (F.to_seq fwd_map))))
    | UnknownDecodeKey (coq_ref, bck_map, x) ->
      str
        (Printf.sprintf
           "Unknown decode key: %s\nDecode map: %s"
           (Enc.to_string x)
           (Strfy.list
              (Strfy.tuple
                 Enc.to_string
                 (Strfy.econstr !coq_ref.coq_env !coq_ref.coq_ctx))
              (List.of_seq (B.to_seq bck_map))))
    | NoBisimResult () -> str "No cached bisimilarity result found."
    (* | ProofvIsNone () -> str "Tried to access contents of proofv which is None." *)
    (*****************)
    | ParamsFailIfIncomplete () ->
      str
        "Params are configured to fail if cannot construct complete LTS from \
         term.\n\n\
         Use command \"MeBi Set FailIfIncomplete False\" to disable this \
         behaviour."
    | ParamsFailIfNotBisim () ->
      str
        "Params are configured to fail if terms not bisim.\n\n\
         Use command \"MeBi Set FailIfNotBisim False\" to disable this \
         behaviour."
    | ExpectedCoqIndDefOfLTSNotType () ->
      str
        "cindef (Coq Inductive Definition) of LTS was expected, but Type was \
         used."
    | InvalidLTSArgsLength i ->
      str
        (Printf.sprintf
           "Command.extract_args, assertion: Array.length args == 3 failed. \
            Got %i"
           i)
    | InvalidLTSTermKind (env, sigma, tm) ->
      str
        "Command.extract_args, assertion: Constr.kind tm matches App _ failed. \
         Got "
      ++ str (Strfy.constr env sigma tm)
      ++ str " which matches with "
      ++ str
           (match Constr.kind tm with
            | Rel _ -> "Rel"
            | Var _ -> "Var"
            | Meta _ -> "Meta"
            | Evar _ -> "EVar"
            | Sort _ -> "Sort"
            | Cast _ -> "Cast"
            | Prod _ -> "Prod"
            | Lambda _ -> "Lambda"
            | LetIn _ -> "LetIn"
            | App _ -> "App"
            | Const _ -> "Const"
            | Ind _ -> "Ind"
            | Construct _ -> "Construct"
            | Case _ -> "Case"
            | Fix _ -> "Fix"
            | CoFix _ -> "CoFix"
            | Proj _ -> "Proj"
            | Int _ -> "Int"
            | Float _ -> "Float"
            | String _ -> "String"
            | Array _ -> "Array")
      ++ str "."
    | InvalidCheckUpdatedCtx (env, sigma, x, y) ->
      str
        "Invalid Args to check_updated_ctx. Should both be empty, or both have \
         some."
      ++ strbrk "\n"
      ++ str
           (Printf.sprintf
              "substls: %s."
              (Strfy.list (Strfy.econstr env sigma) x))
      ++ strbrk "\n"
      ++ str
           (Printf.sprintf
              "ctx_tys: %s."
              (Strfy.list (wrap Strfy.econstr_rel_decl) y))
    | InvalidLTSSort f ->
      str "Invalid LTS Sort: expecting Prop, got " ++ Sorts.pr_sort_family f
    | InvalidTypeSort f ->
      str "Invalid Type Sort: expecting Type or Set, got "
      ++ Sorts.pr_sort_family f
    | InvalidArity (env, sigma, t) ->
      str "Invalid arity for LTS: "
      ++ str (Strfy.constr env sigma t)
      ++ strbrk "\n"
      ++ str "Expecting: forall params, ?terms -> ?labels -> ?terms -> Prop"
    | InvalidRefLTS r -> str "Invalid ref LTS: " ++ str (Strfy.global r)
    | InvalidRefType r -> str "Invalid ref Type: " ++ str (Strfy.global r)
    | UnknownTermType (env, sigma, (tm, ty, trkeys)) ->
      str
        "None of the constructors provided matched type of term to visit. \
         (unknown_term_type) "
      ++ strbrk "\n\n"
      ++ str "Term: "
      ++ str (Strfy.econstr env sigma tm)
      ++ strbrk "\n\n"
      ++ str "Type: "
      ++ str (Strfy.econstr env sigma ty)
      ++ strbrk "\n\n"
      ++ str
           (Printf.sprintf
              "Keys: %s"
              (Strfy.list (Strfy.econstr env sigma) trkeys))
      ++ strbrk "\n\n"
      ++ str
           (Printf.sprintf
              "Does Type match EConstr of any Key? = %b"
              (List.exists
                 (fun (k : EConstr.t) -> EConstr.eq_constr sigma ty k)
                 trkeys))
      ++ strbrk "\n"
      ++ str
           (let tystr = Strfy.pp (Printer.pr_econstr_env env sigma ty) in
            Printf.sprintf
              "Does Type match String of any Key? = %b"
              (List.exists
                 (fun (k : EConstr.t) ->
                   String.equal
                     tystr
                     (Strfy.pp (Printer.pr_econstr_env env sigma k)))
                 trkeys))
    | PrimaryLTSNotFound (env, sigma, t, names) ->
      str "Primary LTS Not found for term: "
      ++ str (Strfy.econstr env sigma t)
      ++ strbrk "\n\n"
      ++ str "constructor names: "
      ++ str
           (Strfy.list
              ~force_newline:true
              ~label:"Names"
              (Strfy.econstr env sigma)
              names)
  ;;

  (* | UnknownDecodeKey (env, sigma, k, bckmap) ->
      str "Unknown decode key: "
      ++ str (Enc.to_string k)
      ++ strbrk "\n\n"
      ++ str "Decode map: ["
      ++
      if Int.equal (B.length bckmap) 0
      then str " ] (empty)"
      else
        B.fold
          (fun (t : Enc.t) (v : EConstr.t) (acc : Pp.t) ->
            acc
            ++ strbrk "\n\n"
            ++ str (Enc.to_string t)
            ++ str " => "
            ++ Printer.pr_econstr_env env sigma v)
          bckmap
          (str "")
        ++ str " ]" *)

  let _ =
    CErrors.register_handler (fun e ->
      match e with MEBI_exn e -> Some (mebi_handler e) | _ -> None)
  ;;
end

(**********************************)
(****** ERROR FUNCTIONS ***********)
(**********************************)

let invalid_kind_of_econstr_expected_atomic x : 'a mm =
  fun st -> raise (Error.invalid_kind_of_econstr_expected_atomic !st.coq_ref x)
;;

let invalid_kind_of_econstr_expected_cast x : 'a mm =
  fun st -> raise (Error.invalid_kind_of_econstr_expected_cast !st.coq_ref x)
;;

let invalid_kind_of_econstr_expected_letin x : 'a mm =
  fun st -> raise (Error.invalid_kind_of_econstr_expected_letin !st.coq_ref x)
;;

let invalid_kind_of_econstr_expected_prod x : 'a mm =
  fun st -> raise (Error.invalid_kind_of_econstr_expected_prod !st.coq_ref x)
;;

let invalid_kind_of_econstr_expected_sort x : 'a mm =
  fun st -> raise (Error.invalid_kind_of_econstr_expected_sort !st.coq_ref x)
;;

let cannot_get_encoding_of_unencoded_econstr x : 'a mm =
  fun st ->
  raise
    (Error.cannot_get_encoding_of_unencoded_econstr !st.coq_ref !st.fwd_enc x)
;;

let cannot_get_decoding_of_unencoded_econstr x : 'a mm =
  fun st ->
  raise
    (Error.cannot_get_decoding_of_unencoded_econstr !st.coq_ref !st.bck_enc x)
;;

let missing_bisim_result () : 'a mm =
  fun (st : wrapper ref) -> raise (Error.missing_bisim_result ())
;;

(* let proofv_is_none () : 'a mm =
   fun (st : wrapper ref) -> raise (Error.proofv_is_none ())
   ;; *)

let params_fail_if_incomplete () : 'a mm =
  fun (st : wrapper ref) -> raise (Error.params_fail_if_incomplete ())
;;

let params_fail_if_not_bisim () : 'a mm =
  fun (st : wrapper ref) -> raise (Error.params_fail_if_not_bisim ())
;;

let invalid_check_updated_ctx x y : 'a mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  raise (Error.invalid_check_updated_ctx !coq_st.coq_env !coq_st.coq_ctx x y)
;;

let invalid_lts_args_length (x : int) : 'a mm =
  fun (st : wrapper ref) -> raise (Error.invalid_lts_args_length x)
;;

let invalid_lts_term_kind (x : Constr.t) : 'a mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  raise (Error.invalid_lts_term_kind !coq_st.coq_env !coq_st.coq_ctx x)
;;

(** Error when input LTS has the wrong arity *)
let invalid_arity (x : Constr.types) : 'a mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  raise (Error.invalid_arity !coq_st.coq_env !coq_st.coq_ctx x)
;;

(** Error when input LTS has the wrong sort *)
let invalid_sort_lts (x : Sorts.family) : 'a mm =
  fun (st : wrapper ref) -> raise (Error.invalid_sort_lts x)
;;

(** Error when input Type has the wrong sort *)
let invalid_sort_type (x : Sorts.family) : 'a mm =
  fun (st : wrapper ref) -> raise (Error.invalid_sort_type x)
;;

(** Error when input LTS reference is invalid (e.g. non existing) *)
let invalid_ref_lts (x : Names.GlobRef.t) : 'a mm =
  fun (st : wrapper ref) -> raise (Error.invalid_ref_lts x)
;;

(** Error when input Type reference is invalid (e.g. non existing) *)
let invalid_ref_type (x : Names.GlobRef.t) : 'a mm =
  fun (st : wrapper ref) -> raise (Error.invalid_ref_type x)
;;

(** Error when input LTS reference is invalid (e.g. non existing) *)
let invalid_cindef_kind unit : 'a mm =
  fun (st : wrapper ref) -> raise (Error.invalid_cindef_kind ())
;;

(** Error when term is of unknown type *)
let unknown_term_type (tmty : EConstr.t * EConstr.t * EConstr.t list) : 'a mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  raise (Error.unknown_term_type !coq_st.coq_env !coq_st.coq_ctx tmty)
;;

(** Error when multiple coq-LTS provided, but none of them match term. *)
let primary_lts_not_found ((t, names) : EConstr.t * EConstr.t list) : 'a mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  raise (Error.primary_lts_not_found !coq_st.coq_env !coq_st.coq_ctx t names)
;;

(** Error when try to decode key that does not exist in decode map. *)
(* let unknown_decode_key ((k, bckmap) : Enc.t * EConstr.t B.t) : 'a mm =
   fun (st : wrapper ref) ->
   let coq_st = !st.coq_ref in
   raise (Error.unknown_decode_key !coq_st.coq_env !coq_st.coq_ctx k bckmap)
   ;; *)

(********************************************)
(****** ENCODE/DECODE ***********************)
(********************************************)

let encode (k : EConstr.t) : Enc.t mm =
  fun (st : wrapper ref) ->
  Log.trace "mebi_wrapper.encode";
  let encoding : Enc.t = Enc.encode !st.fwd_enc !st.bck_enc k in
  Logging.Log.debug
    (Printf.sprintf
       "mebi_wrapper.encode, \"%s\" into (%s)"
       (econstr_to_string k)
       (Enc.to_string encoding));
  assert (F.mem !st.fwd_enc k);
  assert (B.mem !st.bck_enc encoding);
  { state = st; value = encoding }
;;

(** dual to [encode] except we cannot handle new values *)
let decode (k : Enc.t) : EConstr.t mm =
  Log.trace "mebi_wrapper.decode";
  let open Syntax in
  let* bck_enc = get_bck_enc in
  match Enc.decode_opt bck_enc k with
  | Some decoding ->
    Logging.Log.debug
      (Printf.sprintf
         "mebi_wrapper.decode, \"%s\" into (%s)"
         (Enc.to_string k)
         (econstr_to_string decoding));
    return decoding
  | None -> cannot_get_decoding_of_unencoded_econstr k
;;

let decode_to_string (x : Enc.t) : string =
  (* let s_mm : string mm =
     let open Syntax in
     let* y = decode x in
     let* env = get_env in
     let* sigma = get_sigma in
     return (Strfy.pp (Printer.pr_econstr_env env sigma y))
     in
     string_mm s_mm *)
  run
    ~keep_encoding:true
    ~fresh:false
    (let open Syntax in
     let* y = decode x in
     return ((wrap Strfy.econstr) y))
;;

(********************************************)
(****** GET ENCODE/DECODE OPT ***************)
(********************************************)

let get_encoding_opt (k : EConstr.t) : Enc.t option mm =
  fun (st : wrapper ref) ->
  Log.trace "mebi_wrapper.get_encoding_opt";
  match F.find_opt !st.fwd_enc k with
  | None -> { state = st; value = None }
  | Some e -> { state = st; value = Some e }
;;

(** dual to [encode] except we cannot handle new values *)
let get_decoding_opt (k : Enc.t) : EConstr.t option mm =
  fun (st : wrapper ref) ->
  Log.trace "mebi_wrapper.get_decoding_opt";
  match B.find_opt !st.bck_enc k with
  | None -> { state = st; value = None }
  | Some e -> { state = st; value = Some e }
;;

(********************************************)
(****** GET ENCODE/DECODE *******************)
(********************************************)

let get_encoding (k : EConstr.t) : Enc.t mm =
  Log.trace "mebi_wrapper.get_encoding";
  let open Syntax in
  let* opt = get_encoding_opt k in
  match opt with
  | None -> cannot_get_encoding_of_unencoded_econstr k
  | Some e -> return e
;;

(** dual to [encode] except we cannot handle new values *)
let get_decoding (k : Enc.t) : EConstr.t mm =
  Log.trace "mebi_wrapper.get_decoding";
  let open Syntax in
  let* bck_enc = get_bck_enc in
  match B.find_opt bck_enc k with
  | None -> cannot_get_decoding_of_unencoded_econstr k
  | Some e -> return e
;;

(********************************************)
(****** ENCODE/DECODE CHECKs ****************)
(********************************************)

let has_encoding (k : EConstr.t) : bool mm =
  fun (st : wrapper ref) ->
  Log.trace "mebi_wrapper.has_encoding";
  match F.find_opt !st.fwd_enc k with
  | None -> { state = st; value = false }
  | Some _ -> { state = st; value = true }
;;

(** dual to [encode] except we cannot handle new values *)
let has_decoding (k : Enc.t) : bool mm =
  fun (st : wrapper ref) ->
  Log.trace "mebi_wrapper.has_decoding";
  match B.find_opt !st.bck_enc k with
  | None -> { state = st; value = false }
  | Some _ -> { state = st; value = true }
;;

(**********************************)
(****** ENCODE/DECODE MAPs ********)
(**********************************)

let encode_map (m : 'a F.t) : 'a B.t mm =
  fun (st : wrapper ref) ->
  let encoded_map : 'a B.t = B.create (F.length m) in
  F.iter
    (fun (k : EConstr.t) (v : 'a) ->
      let encoding : Enc.t = Enc.encode !st.fwd_enc !st.bck_enc k in
      B.add encoded_map encoding v)
    m;
  { state = st; value = encoded_map }
;;

(** *)
let decode_map (m : 'a B.t) : 'a F.t mm =
  fun (st : wrapper ref) ->
  let decoded_map : 'a F.t = F.create (B.length m) in
  B.iter
    (fun (k : Enc.t) (v : 'a) ->
      let decoding : EConstr.t = Enc.decode !st.bck_enc k in
      F.add decoded_map decoding v)
    m;
  { state = st; value = decoded_map }
;;

(********************************************)
(****** COQ CONSTR TREE *********************)
(********************************************)

(* TODO: generalize this, and use a functor to map from Enc.g., [Enc.t*int tree] to
   [string*int tree]. *)

module Constr_tree = struct
  type 'a tree = Node of 'a * 'a tree list
  type t = (Enc.t * int) tree

  let eq (t1 : t) (t2 : t) : bool =
    let rec tree_eq (t1 : t) (t2 : t) : bool =
      match t1, t2 with
      | Node (a1, b1), Node (a2, b2) ->
        fst a1 == fst a2 && snd a1 == snd a2 && tree_list_eq b1 b2
    and tree_list_eq (l1 : t list) (l2 : t list) : bool =
      match l1, l2 with
      | [], [] -> true
      | h1 :: t1, h2 :: t2 -> tree_eq h1 h2 && tree_list_eq t1 t2
      | [], _ :: _ -> false
      | _ :: _, [] -> false
    in
    tree_eq t1 t2
  ;;

  let compare (t1 : t) (t2 : t) : int =
    let rec tree_compare (t1 : t) (t2 : t) : int =
      match t1, t2 with
      | Node (i1, l1), Node (i2, l2) ->
        (match Enc.compare (fst i1) (fst i2) with
         | 0 ->
           (match Int.compare (snd i1) (snd i2) with
            | 0 -> tree_list_compare l1 l2
            | n -> n)
         | n -> n)
    and tree_list_compare (l1 : t list) (l2 : t list) : int =
      match l1, l2 with
      | [], [] -> 0
      | h1 :: t1, h2 :: t2 ->
        (match tree_compare h1 h2 with
         | 0 -> tree_list_compare t1 t2 (* these should always be empty *)
         | n -> n (* prioritise the main node when comparing *))
      | [], _ :: _ -> -1
      | _ :: _, [] -> 1
    in
    tree_compare t1 t2
  ;;

  let rec pstr (t1 : t) : string =
    match t1 with
    | Node (lhs_int, rhs_int_tree_list) ->
      Printf.sprintf
        "(%s:%i) [%s]"
        (Enc.to_string (fst lhs_int))
        (snd lhs_int)
        (match List.length rhs_int_tree_list with
         | 0 -> ""
         | 1 -> pstr (List.hd rhs_int_tree_list)
         | _ -> Strfy.list pstr rhs_int_tree_list)
  ;;
end

(**********************************)
(****** DECODE CONSTR TREE ********)
(**********************************)

type decoded_tree = (string * int) Constr_tree.tree

(** decodes the parts of the tree corresponding to the LTS into string form. *)
let decode_constr_tree_lts (tree : Constr_tree.t) : decoded_tree mm =
  let open Syntax in
  let rec decode_tree (t : Constr_tree.t) : decoded_tree mm =
    match t with
    | Node (leaf, stem) ->
      let* decoded_leaf_lts : EConstr.t = decode (fst leaf) in
      let decoded_leaf = econstr_to_string decoded_leaf_lts, snd leaf in
      let* decoded_stem = decode_tree_list stem in
      return (Constr_tree.Node (decoded_leaf, decoded_stem))
  and decode_tree_list (l : Constr_tree.t list) : decoded_tree list mm =
    match l with
    | [] -> return []
    | h :: t ->
      let* decoded_h = decode_tree h in
      let* decoded_l = decode_tree_list t in
      return (decoded_h :: decoded_l)
  in
  decode_tree tree
;;

let rec pstr_decoded_tree (t1 : decoded_tree) : string =
  match t1 with
  | Node (lhs_int, rhs_int_tree_list) ->
    Printf.sprintf
      "(%s:%i) [%s]"
      (fst lhs_int)
      (snd lhs_int)
      (match List.length rhs_int_tree_list with
       | 0 -> ""
       | 1 -> pstr_decoded_tree (List.hd rhs_int_tree_list)
       | _ -> Strfy.list pstr_decoded_tree rhs_int_tree_list)
;;

(**********************************)
(****** COQ PROOF THEORIES ********)
(**********************************)
(* source: https://github.com/rocq-prover/rocq/blob/master/doc/plugin_tutorial/tuto3/src/tuto_tactic.ml *)

(* In the environment of the goal, we can get the type of an assumption
   directly by a lookup.  The other solution is to call a low-cost retyping
   function like *)
let get_type_of_hyp (id : Names.Id.t) : EConstr.t mm =
  let open Syntax in
  let* env = get_env in
  match EConstr.lookup_named id env with
  | Context.Named.Declaration.LocalAssum (_pbinder_annot, ty) ->
    Log.debug
      (Printf.sprintf
         "mebi_wrapper.get_type_of_hyp, LocalAssum: %s"
         (econstr_to_string ty));
    return ty
  | Context.Named.Declaration.LocalDef (_pbinder_annot, _constr, ty) ->
    Log.debug
      (Printf.sprintf
         "mebi_wrapper.get_type_of_hyp, LocalDef: %s := %s"
         (econstr_to_string ty)
         (econstr_to_string _constr));
    return ty
;;

(**********************************)
(****** COQ PROOF CONTEXT *********)
(**********************************)

(* let get_proof () : Declare.Proof.t mm =
   Log.trace "mebi_wrapper.get_proof";
   let open Syntax in
   let* proofv : proof_context = get_proofv in
   match proofv.proof with
   | None -> proofv_is_none ()
   | Some proof -> return proof
   ;; *)

(* let get_proof_env () : Environ.env mm =
   Log.trace "mebi_wrapper.get_proof_env";
   let open Syntax in
   let* proofv : proof_context = get_proofv in
   match proofv.proof with
   | None ->
   Log.warning "mebi_wrapper.get_proof_env, proof is None (using monad env)";
   get_env
   | Some proof ->
   return (snd (Proof.get_proof_context (Declare.Proof.get proof)))
   ;; *)

(* let get_proof_sigma () : Evd.evar_map mm =
   Log.trace "mebi_wrapper.get_proof_sigma";
   let open Syntax in
   let* proofv : proof_context = get_proofv in
   match proofv.proof with
   | None ->
   Log.warning "mebi_wrapper.get_proof_sigma, proof is None (using monad ctx)";
   get_sigma
   | Some proof ->
   return (fst (Proof.get_proof_context (Declare.Proof.get proof)))
   ;; *)

(* let get_proof_names () : Names.Id.Set.t mm =
   Log.trace "mebi_wrapper.get_proof_names";
   let open Syntax in
   let* proofv : proof_context = get_proofv in
   match proofv.names with
   | None -> return Names.Id.Set.empty
   | Some names -> return names
   ;; *)

(* let update_names
      ?(replace : bool = false)
      (new_names : Names.Id.Set.t)
      (st : wrapper ref)
  : unit in_context
  =
  let coq_st = !st.coq_ref in
  match !coq_st.proofv.names with
  | None ->
    Log.warning "mebi_wrapper.update_names, proofv.names is None (create new)";
    !coq_st.proofv.names <- Some new_names;
    { state = st; value = () }
  | Some names ->
    !coq_st.proofv.names
    <- Some (if replace then new_names else Names.Id.Set.union names new_names);
    { state = st; value = () }
;; *)

(* let add_name (name : Names.Id.t) (st : wrapper ref) : unit in_context =
  let coq_st = !st.coq_ref in
  match !coq_st.proofv.names with
  | None ->
    Log.warning "mebi_wrapper.add_name, proofv.names is None (create new)";
    !coq_st.proofv.names <- Some (Names.Id.Set.singleton name);
    { state = st; value = () }
  | Some names ->
    !coq_st.proofv.names <- Some (Names.Id.Set.add name names);
    { state = st; value = () }
;; *)

(**********************************)
(****** DEBUG PRINTOUTS ***********)
(**********************************)

let debug (f : Environ.env -> Evd.evar_map -> Pp.t) : unit mm =
  state (fun env sigma ->
    Feedback.msg_debug (f env sigma);
    sigma, ())
;;

let show_fwd_map () : unit mm =
  let open Syntax in
  let* env = get_env in
  let* sigma = get_sigma in
  let* fwd_map = get_fwd_enc in
  Log.debug
    (Printf.sprintf
       "mebi_wrapper.show_fwd_map: \n%s"
       (Strfy.list
          ~force_newline:true
          (Strfy.tuple
             ~force_newline:true
             ~indent:1
             (fun (x : EConstr.t) ->
               Strfy.tuple
                 ~is_keyval:true
                 Strfy.str
                 (Strfy.econstr env sigma)
                 ("econstr", x))
             (fun (x : Enc.t) ->
               Strfy.tuple
                 ~is_keyval:true
                 Strfy.str
                 Enc.to_string
                 ("encoding", x)))
          (Enc.fwd_to_list fwd_map)));
  return ()
;;

let show_bck_map () : unit mm =
  let open Syntax in
  let* env = get_env in
  let* sigma = get_sigma in
  let* bck_map = get_bck_enc in
  Log.debug
    (Printf.sprintf
       "mebi_wrapper.show_bck_map: \n%s"
       (Strfy.list
          ~force_newline:true
          (Strfy.tuple
             ~force_newline:true
             ~indent:1
             (fun (x : Enc.t) ->
               Strfy.tuple
                 ~is_keyval:true
                 Strfy.str
                 Enc.to_string
                 ("encoding", x))
             (fun (x : EConstr.t) ->
               Strfy.tuple
                 ~is_keyval:true
                 Strfy.str
                 (Strfy.econstr env sigma)
                 ("econstr", x)))
          (Enc.bck_to_list bck_map)));
  return ()
;;

(* let show_proof_data () : unit mm =
  fun (st : wrapper ref) ->
  Log.trace "mebi_wrapper.show_proof_data";
  let coq_st = !st.coq_ref in
  (match !coq_st.proofv.proof with
   | None -> Log.debug "mebi_wrapper.show_proof_data, proofv.proof is None"
   | Some proof ->
     let goals : Proofview.Goal.t Proofview.tactic list Proofview.tactic =
       Proofview.Goal.goals
     in
     let _goals = goals in
     let the_proof : Proof.t = Declare.Proof.get proof in
     let the_data = Proof.data the_proof in
     let goals_string = Strfy.list Strfy.evar the_data.goals in
     let all_goals_string =
       Strfy.list
         ~indent:2
         Strfy.evar
         (Evar.Set.to_list (Proof.all_goals the_proof))
     in
     let _partial_proof : EConstr.constr list =
       Proof.partial_proof the_proof
       (* Proofview.partial_proof the_data.entry *)
     in
     let _x = econstr_list_to_constr_opt_string _partial_proof in
     (* let _y = string_mm _x in *)
     let partial_proof_string =
       "TODO -- (obtaining string from wrapper causes \"Anomaly Uncaught \
        exception Not_found\")"
       (* _y *)
     in
     let stack_string =
       Strfy.list
         ~force_newline:true
         ~indent:1
         (Strfy.tuple
            (Strfy.list ~force_newline:true ~indent:2 Strfy.evar)
            (Strfy.list ~force_newline:true ~indent:2 Strfy.evar))
         the_data.stack
       (* Strfy.list2
          ~indent:2
          Strfy.evar
          Strfy.evar
          the_data.stack *)
     in
     Log.debug
       (Printf.sprintf
          "mebi_wrapper.show_proof_data, name: %s\n\
           - is done: %b\n\
           - no focused goal: %b\n\
           - unfocused: %b\n\
           - goals: %s\n\
           - all goals: %s\n\
           - stack: %s\n\
           - partial proof: %s\n\
           - pr_proof: %s\n"
          (Names.Id.to_string the_data.name)
          (Proof.is_done the_proof)
          (Proof.no_focused_goal the_proof)
          (Proof.unfocused the_proof)
          goals_string
          all_goals_string
          stack_string
          partial_proof_string
          (Strfy.pp (Proof.pr_proof the_proof))));
  (* *)
  { state = st; value = () }
;; *)

(* let show_proof () : unit mm =
   Log.trace "mebi_wrapper.show_proof";
   let open Syntax in
   let* proof = get_proof () in
   let the_proof : Proof.t = Declare.Proof.get proof in
   Log.debug
   (Printf.sprintf
   "mebi_wrapper.show_proof, Proof.pr_proof: %s"
   (Strfy.pp (Proof.pr_proof the_proof)));
   return ()
   ;; *)

(* let show_names () : unit mm =
   Log.trace "mebi_wrapper.show_names";
   let open Syntax in
   let* env = get_proof_env () in
   let* names = get_proof_names () in
   Log.debug
   (Printf.sprintf
   "mebi_wrapper.show_names: %s"
   (if Names.Id.Set.is_empty names
   then "[ ] (empty)"
   else
   Printf.sprintf
   "[%s\n]"
   (Names.Id.Set.fold
   (fun (n : Names.Id.t) (acc : string) ->
   Printf.sprintf
   "%s\n\t%s : %s"
   acc
   (Names.Id.to_string n)
   (* (Strfy.pp
   (Names.Id.print n) *)
   (let lookup_n : EConstr.named_declaration =
   EConstr.lookup_named n env
   in
   match Context.Named.Declaration.get_value lookup_n with
   | None -> "(No value found)"
   | Some v -> econstr_to_string v))
   names
   "")));
   return ()
   ;; *)

(********************************************)
(****** GRAPH *******************************)
(********************************************)

let make_transition_tbl (st : wrapper ref)
  : (module Hashtbl.S with type key = Enc.t) in_context
  =
  let eqf = Enc.eq in
  let hashf = Enc.hash in
  let module TransitionTbl =
    Hashtbl.Make (struct
      type t = Enc.t

      let equal t1 t2 = eqf t1 t2
      let hash t = hashf t
    end)
  in
  { state = st
  ; value = (module TransitionTbl : Hashtbl.S with type key = Enc.t)
  }
;;

let make_state_set (st : wrapper ref)
  : (module Set.S with type elt = Enc.t) in_context
  =
  let comparef = Enc.compare in
  let module StateSet =
    Set.Make (struct
      type t = Enc.t

      let compare t1 t2 = comparef t1 t2
    end)
  in
  { state = st; value = (module StateSet : Set.S with type elt = Enc.t) }
;;

let make_state_tree_pair_set (st : wrapper ref)
  : (module Set.S with type elt = Enc.t * Constr_tree.t) in_context
  =
  let module PairSet =
    Set.Make (struct
      type t = Enc.t * Constr_tree.t

      let compare t1 t2 =
        match Enc.compare (fst t1) (fst t2) with
        | 0 -> Constr_tree.compare (snd t1) (snd t2)
        | c -> c
      ;;
    end)
  in
  { state = st
  ; value = (module PairSet : Set.S with type elt = Enc.t * Constr_tree.t)
  }
;;

(********************************************)
(****** DEBUG *******************************)
(********************************************)

let debug_encoding () : unit mm =
  fun (st : wrapper ref) ->
  if Int.equal 0 (F.length !st.fwd_enc)
  then (
    Logging.Log.debug "mebi_wrapper.debug_encoding, fwd encoding is empty";
    if Int.equal 0 (B.length !st.bck_enc)
    then Logging.Log.debug "mebi_wrapper.debug_encoding, bck encoding is empty"
    else
      B.iter
        (fun (enc : Enc.t) (t : EConstr.t) ->
          Logging.Log.debug
            (Printf.sprintf
               "(%s) => %s "
               (Enc.to_string enc)
               (econstr_to_string t)))
        !st.bck_enc)
  else
    F.iter
      (fun (t : EConstr.t) (enc : Enc.t) ->
        Logging.Log.debug
          (Printf.sprintf
             "(%s) => %s "
             (Enc.to_string enc)
             (econstr_to_string t)))
      !st.fwd_enc;
  { state = st; value = () }
;;

let debug_econstr_kind (t : EConstr.t) : unit mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  Log.debug
    (Printf.sprintf
       "mebi_wrapper.debug_econstr_kind:\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n"
       (Printf.sprintf
          "isRel %s = %b"
          (econstr_to_string t)
          (EConstr.isRel !coq_st.coq_ctx t))
       (Printf.sprintf
          "isVar %s = %b"
          (econstr_to_string t)
          (EConstr.isVar !coq_st.coq_ctx t))
       (Printf.sprintf
          "isInd %s = %b"
          (econstr_to_string t)
          (EConstr.isInd !coq_st.coq_ctx t))
       (Printf.sprintf
          "isRef %s = %b"
          (econstr_to_string t)
          (EConstr.isRef !coq_st.coq_ctx t))
       (Printf.sprintf
          "isEvar %s = %b"
          (econstr_to_string t)
          (EConstr.isEvar !coq_st.coq_ctx t))
       (Printf.sprintf
          "isMeta %s = %b"
          (econstr_to_string t)
          (EConstr.isMeta !coq_st.coq_ctx t))
       (Printf.sprintf
          "isSort %s = %b"
          (econstr_to_string t)
          (EConstr.isSort !coq_st.coq_ctx t))
       (Printf.sprintf
          "isCast %s = %b"
          (econstr_to_string t)
          (EConstr.isCast !coq_st.coq_ctx t))
       (Printf.sprintf
          "isApp %s = %b"
          (econstr_to_string t)
          (EConstr.isApp !coq_st.coq_ctx t))
       (Printf.sprintf
          "isLambda %s = %b"
          (econstr_to_string t)
          (EConstr.isLambda !coq_st.coq_ctx t))
       (Printf.sprintf
          "isLetIn %s = %b"
          (econstr_to_string t)
          (EConstr.isLetIn !coq_st.coq_ctx t))
       (Printf.sprintf
          "isProd %s = %b"
          (econstr_to_string t)
          (EConstr.isProd !coq_st.coq_ctx t))
       (Printf.sprintf
          "isConst %s = %b"
          (econstr_to_string t)
          (EConstr.isConst !coq_st.coq_ctx t))
       (Printf.sprintf
          "isConstruct %s = %b"
          (econstr_to_string t)
          (EConstr.isConstruct !coq_st.coq_ctx t))
       (Printf.sprintf
          "isFix %s = %b"
          (econstr_to_string t)
          (EConstr.isFix !coq_st.coq_ctx t))
       (Printf.sprintf
          "isCoFix %s = %b"
          (econstr_to_string t)
          (EConstr.isCoFix !coq_st.coq_ctx t))
       (Printf.sprintf
          "isCase %s = %b"
          (econstr_to_string t)
          (EConstr.isCase !coq_st.coq_ctx t))
       (Printf.sprintf
          "isProj %s = %b"
          (econstr_to_string t)
          (EConstr.isProj !coq_st.coq_ctx t))
       (Printf.sprintf
          "isType %s = %b"
          (econstr_to_string t)
          (EConstr.isType !coq_st.coq_ctx t)));
  { state = st; value = () }
;;

let debug_constr_kind (t : Constr.t) : unit mm =
  fun (st : wrapper ref) ->
  Log.debug
    (Printf.sprintf
       "mebi_wrapper.debug_constr_kind:\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n"
       (Printf.sprintf "isRel %s = %b" (constr_to_string t) (Constr.isRel t))
       (Printf.sprintf "isVar %s = %b" (constr_to_string t) (Constr.isVar t))
       (Printf.sprintf "isInd %s = %b" (constr_to_string t) (Constr.isInd t))
       (Printf.sprintf "isRef %s = %b" (constr_to_string t) (Constr.isRef t))
       (Printf.sprintf "isEvar %s = %b" (constr_to_string t) (Constr.isEvar t))
       (Printf.sprintf "isMeta %s = %b" (constr_to_string t) (Constr.isMeta t))
       (Printf.sprintf "isSort %s = %b" (constr_to_string t) (Constr.isSort t))
       (Printf.sprintf "isCast %s = %b" (constr_to_string t) (Constr.isCast t))
       (Printf.sprintf "isApp %s = %b" (constr_to_string t) (Constr.isApp t))
       (Printf.sprintf
          "isLambda %s = %b"
          (constr_to_string t)
          (Constr.isLambda t))
       (Printf.sprintf
          "isLetIn %s = %b"
          (constr_to_string t)
          (Constr.isLetIn t))
       (Printf.sprintf "isProd %s = %b" (constr_to_string t) (Constr.isProd t))
       (Printf.sprintf
          "isConst %s = %b"
          (constr_to_string t)
          (Constr.isConst t))
       (Printf.sprintf
          "isConstruct %s = %b"
          (constr_to_string t)
          (Constr.isConstruct t))
       (Printf.sprintf "isFix %s = %b" (constr_to_string t) (Constr.isFix t))
       (Printf.sprintf
          "isCoFix %s = %b"
          (constr_to_string t)
          (Constr.isCoFix t))
       (Printf.sprintf "isCase %s = %b" (constr_to_string t) (Constr.isCase t))
       (Printf.sprintf "isProj %s = %b" (constr_to_string t) (Constr.isProj t))
       (Printf.sprintf
          "is_Prop %s = %b"
          (constr_to_string t)
          (Constr.is_Prop t))
       (Printf.sprintf
          "is_Type %s = %b"
          (constr_to_string t)
          (Constr.is_Type t))
       (Printf.sprintf "is_Set %s = %b" (constr_to_string t) (Constr.is_Set t)));
  { state = st; value = () }
;;

let debug_econstr_constr_kind (t : EConstr.t) : unit mm =
  let open Syntax in
  let* t = econstr_to_constr t in
  debug_constr_kind t
;;

(****************************************************************************)
