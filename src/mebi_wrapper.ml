(***********************************************************************)
module Log : Logger.LOGGER_TYPE = Logger.MkDefault ()

let () = Log.Config.configure_output Debug false
let () = Log.Config.configure_output Trace false
(***********************************************************************)

open Mebi_setup
module Enc = Mebi_setup.Enc
module F = Enc.F
module B = Enc.B

type fwdmap = B.key F.t
type bckmap = F.key B.t

(********************************************)
(****** WRAPPER & CONTEXT *******************)
(********************************************)

let the_enc_maps_cache : (fwdmap * bckmap) ref option ref = ref None

let reset_enc_maps () =
  let () = Enc.reset () in
  let the_fwd_map : fwdmap = F.create 0 in
  let the_bck_map : bckmap = B.create 0 in
  let the_enc_maps = ref (the_fwd_map, the_bck_map) in
  the_enc_maps_cache := Some the_enc_maps;
  the_enc_maps
;;

let get_the_enc_maps ?(keep_encoding : bool = false) () : (fwdmap * bckmap) ref =
  match !the_enc_maps_cache with
  | None -> reset_enc_maps ()
  | Some the_enc_maps ->
    if keep_encoding then the_enc_maps else reset_enc_maps ()
;;

type wrapper =
  { coq_ref : coq_context ref
  ; fwd_enc : fwdmap
  ; bck_enc : bckmap
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
  Log.trace __FUNCTION__;
  let coq_ctx : Evd.evar_map = !(the_coq_ctx ~fresh ()) in
  let coq_env : Environ.env = !(the_coq_env ()) in
  let coq_ref : coq_context ref = ref { coq_env; coq_ctx } in
  let fwd_enc, bck_enc = !(get_the_enc_maps ~keep_encoding ()) in
  let a : 'a in_context = x (ref { coq_ref; fwd_enc; bck_enc }) in
  the_enc_maps_cache := Some (ref (!(a.state).fwd_enc, !(a.state).bck_enc));
  a.value
;;

let runkeep (x : 'a mm) : 'a =
  Log.trace __FUNCTION__;
  run ~keep_encoding:true ~fresh:false x
;;

let return (x : 'a) : 'a mm =
  fun (st : wrapper ref) -> { state = st; value = x }
[@@inline always]
;;

let bind (x : 'a mm) (f : 'a -> 'b mm) : 'b mm =
  fun (st : wrapper ref) ->
  let a : 'a in_context = x st in
  f a.value a.state
[@@inline always]
;;

let map (f : 'a -> 'b) (x : 'a mm) : 'b mm =
  fun (st : wrapper ref) ->
  let x_st : 'a in_context = x st in
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

let get_env (st : wrapper ref) : Environ.env in_context =
  { state = st; value = !(!st.coq_ref).coq_env }
;;

let get_sigma (st : wrapper ref) : Evd.evar_map in_context =
  { state = st; value = !(!st.coq_ref).coq_ctx }
;;

let get_fwd_enc (st : wrapper ref) : fwdmap in_context =
  { state = st; value = !st.fwd_enc }
;;

let get_bck_enc (st : wrapper ref) : bckmap in_context =
  { state = st; value = !st.bck_enc }
;;

let state
      (f : Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
      (st : wrapper ref)
  : 'a in_context
  =
  let coq_ref : coq_context ref = !st.coq_ref in
  let sigma, a = f !coq_ref.coq_env !coq_ref.coq_ctx in
  coq_ref := { !coq_ref with coq_ctx = sigma };
  st := { !st with coq_ref };
  { state = st; value = a }
;;

let sandbox ?(using : Evd.evar_map option) (m : 'a mm) (st : wrapper ref)
  : 'a in_context
  =
  let st_contents : wrapper = !st in
  match using with
  | None ->
    let res : 'a in_context = m st in
    { state = ref st_contents; value = res.value }
  | Some sigma ->
    let coq_context : coq_context = !(!st.coq_ref) in
    let coq_ref : coq_context ref = ref { coq_context with coq_ctx = sigma } in
    let st : wrapper ref = ref { !st with coq_ref } in
    let res : 'a in_context = m st in
    { state = ref st_contents; value = res.value }
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
(****** COQ TERM TO STRING ********)
(**********************************)

let mebi_to_string (f : Environ.env -> Evd.evar_map -> 'a -> string)
  : 'a Utils.Strfy.to_string
  =
  Log.trace __FUNCTION__;
  run
    ~keep_encoding:true
    ~fresh:false
    (let open Syntax in
     let* env = get_env in
     let* sigma = get_sigma in
     return (Utils.Strfy.Of (f env sigma)))
;;

let constr_to_string (x : Constr.t) : string =
  Utils.Strfy.f_to_string (mebi_to_string Rocq_utils.Strfy.constr) x
;;

let econstr_to_string (x : EConstr.t) : string =
  Utils.Strfy.f_to_string (mebi_to_string Rocq_utils.Strfy.econstr) x
;;

let enc_to_string : Enc.t -> string = Enc.to_string

(********************************************)
(****** DEBUG *******************************)
(********************************************)

let debug_enc () : unit mm =
  fun (st : wrapper ref) ->
  let fstr = Utils.Strfy.Args Utils.Strfy.string in
  let feconstr : EConstr.t Utils.Strfy.to_string =
    Utils.Strfy.Of econstr_to_string
  in
  let fenc : Enc.t Utils.Strfy.to_string = Utils.Strfy.Of Enc.to_string in
  let fpair ((x, y) : EConstr.t * Enc.t) : string =
    Utils.Strfy.tuple
      fstr
      fstr
      ( Utils.Strfy.keyval feconstr ("econstr", x)
      , Utils.Strfy.keyval fenc ("enc", y) )
  in
  let f (prefix : string) (x : EConstr.t) (y : Enc.t) : unit =
    Logger.Default.thing ~__FUNCTION__ Debug prefix (x, y) (Of fpair);
    ()
  in
  F.iter (f "fwdenc") !st.fwd_enc;
  (* let g (prefix : string) (y : Enc.t) (x : EConstr.t) : unit = f prefix x y in *)
  (* B.iter (g "bckenc") !st.bck_enc; *)
  { state = st; value = () }
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
    | UnknownDecodeKey of (coq_context ref * bckmap * Enc.t)
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
    (* | UnknownDecodeKey of (Environ.env * Evd.evar_map * Enc.t * bckmap) *)
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
    -> bckmap
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
     -> bckmap
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
    | UnknownDecodeKey of (coq_context ref * bckmap * Enc.t)
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
    (* | UnknownDecodeKey of (Environ.env * Evd.evar_map * Enc.t * bckmap) *)
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
  open Utils.Strfy
  open Rocq_utils.Strfy

  let mebi_handler = function
    | Invalid_KindOfTypeEConstr_Expected_Atomic (coq_ref, x) ->
      Pp.str
        (Printf.sprintf
           "Invalid Kind of type EConstr, expected Atomic, but got: %s"
           (econstr_types !coq_ref.coq_env !coq_ref.coq_ctx x))
    | Invalid_KindOfTypeEConstr_Expected_Cast (coq_ref, x) ->
      Pp.str
        (Printf.sprintf
           "Invalid Kind of type EConstr, expected Cast, but got: %s"
           (econstr_types !coq_ref.coq_env !coq_ref.coq_ctx x))
    | Invalid_KindOfTypeEConstr_Expected_LetIn (coq_ref, x) ->
      Pp.str
        (Printf.sprintf
           "Invalid Kind of type EConstr, expected LetIn, but got: %s"
           (econstr_types !coq_ref.coq_env !coq_ref.coq_ctx x))
    | Invalid_KindOfTypeEConstr_Expected_Prod (coq_ref, x) ->
      Pp.str
        (Printf.sprintf
           "Invalid Kind of type EConstr, expected Prod, but got: %s"
           (econstr_types !coq_ref.coq_env !coq_ref.coq_ctx x))
    | Invalid_KindOfTypeEConstr_Expected_Sort (coq_ref, x) ->
      Pp.str
        (Printf.sprintf
           "Invalid Kind of type EConstr, expected Sort, but got: %s"
           (econstr_types !coq_ref.coq_env !coq_ref.coq_ctx x))
    | UnknownEncodeKey (coq_ref, fwd_map, x) ->
      Pp.str
        (Printf.sprintf
           "Unknown encode key: %s\nEncode map: %s"
           (econstr !coq_ref.coq_env !coq_ref.coq_ctx x)
           (list
              (Args
                 (tuple
                    (Of (econstr !coq_ref.coq_env !coq_ref.coq_ctx))
                    (Of enc_to_string)))
              (List.of_seq (F.to_seq fwd_map))))
    | UnknownDecodeKey (coq_ref, bck_map, x) ->
      Pp.str
        (Printf.sprintf
           "Unknown decode key: %s\nDecode map: %s"
           (Enc.to_string x)
           (list
              (Args
                 (tuple
                    (Of enc_to_string)
                    (Of (econstr !coq_ref.coq_env !coq_ref.coq_ctx))))
              (List.of_seq (B.to_seq bck_map))))
    | NoBisimResult () -> Pp.str "No cached bisimilarity result found."
    (* | ProofvIsNone () -> str "Tried to access contents of proofv which is None." *)
    (*****************)
    | ParamsFailIfIncomplete () ->
      Pp.str
        "Params are configured to fail if cannot construct complete LTS from \
         term.\n\n\
         Use command \"MeBi Set FailIfIncomplete False\" to disable this \
         behaviour."
    | ParamsFailIfNotBisim () ->
      Pp.str
        "Params are configured to fail if terms not bisim.\n\n\
         Use command \"MeBi Set FailIfNotBisim False\" to disable this \
         behaviour."
    | ExpectedCoqIndDefOfLTSNotType () ->
      Pp.str
        "cindef (Coq Inductive Definition) of LTS was expected, but Type was \
         used."
    | InvalidLTSArgsLength i ->
      Pp.str
        (Printf.sprintf
           "Command.extract_args, assertion: Array.length args == 3 failed. \
            Got %i"
           i)
    | InvalidLTSTermKind (env, sigma, tm) ->
      Pp.str
        "Command.extract_args, assertion: Constr.kind tm matches App _ failed. \
         Got "
      ++ Pp.str (constr env sigma tm)
      ++ Pp.str " which matches with "
      ++ Pp.str
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
      ++ Pp.str "."
    | InvalidCheckUpdatedCtx (env, sigma, x, y) ->
      Pp.str
        "Invalid Args to check_updated_ctx. Should both be empty, or both have \
         some."
      ++ strbrk "\n"
      ++ Pp.str (Printf.sprintf "substls: %s." (list (feconstr env sigma) x))
      ++ strbrk "\n"
      ++ Pp.str
           (Printf.sprintf
              "ctx_tys: %s."
              (list (mebi_to_string econstr_rel_decl) y))
    | InvalidLTSSort f ->
      Pp.str "Invalid LTS Sort: expecting Prop, got " ++ Sorts.pr_sort_family f
    | InvalidTypeSort f ->
      Pp.str "Invalid Type Sort: expecting Type or Set, got "
      ++ Sorts.pr_sort_family f
    | InvalidArity (env, sigma, t) ->
      Pp.str "Invalid arity for LTS: "
      ++ Pp.str (constr env sigma t)
      ++ strbrk "\n"
      ++ Pp.str "Expecting: forall params, ?terms -> ?labels -> ?terms -> Prop"
    | InvalidRefLTS r -> Pp.str "Invalid ref LTS: " ++ Pp.str (global r)
    | InvalidRefType r -> Pp.str "Invalid ref Type: " ++ Pp.str (global r)
    | UnknownTermType (env, sigma, (tm, ty, trkeys)) ->
      Pp.str
        "None of the constructors provided matched type of term to visit. \
         (unknown_term_type) "
      ++ strbrk "\n\n"
      ++ Pp.str "Term: "
      ++ Pp.str (econstr env sigma tm)
      ++ strbrk "\n\n"
      ++ Pp.str "Type: "
      ++ Pp.str (econstr env sigma ty)
      ++ strbrk "\n\n"
      ++ Pp.str
           (Printf.sprintf "Keys: %s" (list (Of (econstr env sigma)) trkeys))
      ++ strbrk "\n\n"
      ++ Pp.str
           (Printf.sprintf
              "Does Type match EConstr of any Key? = %b"
              (List.exists
                 (fun (k : EConstr.t) -> EConstr.eq_constr sigma ty k)
                 trkeys))
      ++ strbrk "\n"
      ++ Pp.str
           (let tystr = pp (Printer.pr_econstr_env env sigma ty) in
            Printf.sprintf
              "Does Type match String of any Key? = %b"
              (List.exists
                 (fun (k : EConstr.t) ->
                   String.equal tystr (pp (Printer.pr_econstr_env env sigma k)))
                 trkeys))
    | PrimaryLTSNotFound (env, sigma, t, names) ->
      Pp.str "Primary LTS Not found for term: "
      ++ Pp.str (econstr env sigma t)
      ++ strbrk "\n\n"
      ++ Pp.str "constructor names: "
      ++ Pp.str
           (list
              ~args:(style_args ~name:"Names" ())
              (Of (econstr env sigma))
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
(* let unknown_decode_key ((k, bckmap) : Enc.t * bckmap) : 'a mm =
   fun (st : wrapper ref) ->
   let coq_st = !st.coq_ref in
   raise (Error.unknown_decode_key !coq_st.coq_env !coq_st.coq_ctx k bckmap)
   ;; *)

(***********************************************************************)
(*** Encode ************************************************************)
(***********************************************************************)

(** [encode x] *)
let encode (x : EConstr.t) : Enc.t mm =
  fun (st : wrapper ref) ->
  let e : Enc.t = Enc.encode !st.fwd_enc !st.bck_enc x in
  { state = st; value = e }
;;

let encoding (x : EConstr.t) : Enc.t mm =
  fun (st : wrapper ref) ->
  let e : Enc.t = F.find !st.fwd_enc x in
  { state = st; value = e }
;;

let has_encoding (x : EConstr.t) : bool =
  run ~keep_encoding:true ~fresh:false (fun (st : wrapper ref) ->
    { state = st
    ; value =
        (match F.find_opt !st.fwd_enc x with None -> false | Some _ -> true)
    })
;;

let get_encoding (x : EConstr.t) : Enc.t =
  run ~keep_encoding:true ~fresh:false (encoding x)
;;

(** [encoding_opt x] retrieves the encoding [y] of [x] from the [!st.fwd_enc] map and returns [Some y] if it exists, otherwise [None].
*)
let encoding_opt (x : EConstr.t) : Enc.t option mm =
  fun (st : wrapper ref) ->
  let e_opt : Enc.t option = F.find_opt !st.fwd_enc x in
  { state = st; value = e_opt }
;;

let get_encoding_opt (x : EConstr.t) : Enc.t option =
  run ~keep_encoding:true ~fresh:false (encoding_opt x)
;;

(***********************************************************************)
(*** Decode ************************************************************)
(***********************************************************************)

(** dual to [encode] except we cannot handle new values *)
let decode (x : Enc.t) : EConstr.t mm =
  fun (st : wrapper ref) ->
  let d : EConstr.t = Enc.decode !st.bck_enc x in
  { state = st; value = d }
;;

let decoding (x : Enc.t) : EConstr.t mm =
  fun (st : wrapper ref) ->
  let d : EConstr.t = B.find !st.bck_enc x in
  { state = st; value = d }
;;

let has_decoding (x : Enc.t) : bool =
  run ~keep_encoding:true ~fresh:false (fun (st : wrapper ref) ->
    { state = st
    ; value =
        (match B.find_opt !st.bck_enc x with None -> false | Some _ -> true)
    })
;;

let get_decoding (x : Enc.t) : EConstr.t =
  run ~keep_encoding:true ~fresh:false (decoding x)
;;

let decode_opt (x : Enc.t) : EConstr.t option mm =
  fun (st : wrapper ref) ->
  let d_opt : EConstr.t option = B.find_opt !st.bck_enc x in
  { state = st; value = d_opt }
;;

let get_decode_opt (x : Enc.t) : EConstr.t option =
  run ~keep_encoding:true ~fresh:false (decode_opt x)
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

(**********************************)
(****** DECODE CONSTR TREE ********)
(**********************************)

type decoded_tree = (string * int) Mebi_constr.Tree.tree

(** decodes the parts of the tree corresponding to the LTS into string form. *)
let decode_constr_tree_lts (tree : Mebi_constr.Tree.t) : decoded_tree mm =
  let open Syntax in
  let rec decode_tree (t : Mebi_constr.Tree.t) : decoded_tree mm =
    match t with
    | Node (leaf, stem) ->
      let* decoded_leaf_lts : EConstr.t = decode (fst leaf) in
      let decoded_leaf = econstr_to_string decoded_leaf_lts, snd leaf in
      let* decoded_stem = decode_tree_list stem in
      return (Mebi_constr.Tree.Node (decoded_leaf, decoded_stem))
  and decode_tree_list (l : Mebi_constr.Tree.t list) : decoded_tree list mm =
    match l with
    | [] -> return []
    | h :: t ->
      let* decoded_h = decode_tree h in
      let* decoded_l = decode_tree_list t in
      return (decoded_h :: decoded_l)
  in
  decode_tree tree
;;

(* let rec pstr_decoded_tree
   ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
   (t1 : decoded_tree)
   : string
   =
   match t1 with
   | Mebi_constr.Tree.Node (lhs_int, rhs_int_tree_list) ->
   Printf.sprintf
   "(%s:%i) [%s]"
   (fst lhs_int)
   (snd lhs_int)
   (match List.length rhs_int_tree_list with
   | 0 -> ""
   | 1 -> pstr_decoded_tree (List.hd rhs_int_tree_list)
   | _ -> Utils.Strfy.list ~args pstr_decoded_tree rhs_int_tree_list)
   ;; *)

(**********************************)
(****** COQ PROOF THEORIES ********)
(**********************************)
(* source: https://github.com/rocq-prover/rocq/blob/master/doc/plugin_tutorial/tuto3/src/tuto_tactic.ml *)

(* In the environment of the goal, we can get the type of an assumption
   directly by a lookup.  The other solution is to call a low-cost retyping
   function like *)
(* let get_type_of_hyp (id : Names.Id.t) : EConstr.t mm =
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
;; *)

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

let debug_str (f : Environ.env -> Evd.evar_map -> string) : string mm =
  state (fun env sigma ->
    let x : string = f env sigma in
    sigma, x)
;;

(* let show_fwd_map () : unit =
   run
   ~keep_encoding:true
   ~fresh:false
   (let open Syntax in
   let* env = get_env in
   let* sigma = get_sigma in
   let* fwd_map = get_fwd_enc in
   Log.debug
   (Printf.sprintf
   "mebi_wrapper.show_fwd_map: \n%s"
   (Utils.Strfy.list
   ~force_newline:true
   (Utils.Strfy.tuple
   ~force_newline:true
   ~indent:1
   (fun (x : EConstr.t) ->
   Utils.Strfy.tuple
   ~is_keyval:true
   Utils.Strfy.str
   (Rocq_utils.Strfy.econstr env sigma)
   ("econstr", x))
   (fun (x : Enc.t) ->
   Utils.Strfy.tuple
   ~is_keyval:true
   Utils.Strfy.str
   Enc.to_string
   ("encoding", x)))
   (Enc.fwd_to_list fwd_map)));
   return ())
   ;; *)

(* let show_bck_map () : unit =
   run
   ~keep_encoding:true
   ~fresh:false
   (let open Syntax in
   let* env = get_env in
   let* sigma = get_sigma in
   let* bck_map = get_bck_enc in
   Log.debug
   (Printf.sprintf
   "mebi_wrapper.show_bck_map: \n%s"
   (Utils.Strfy.list
   ~force_newline:true
   (Utils.Strfy.tuple
   ~force_newline:true
   ~indent:1
   (fun (x : Enc.t) ->
   Utils.Strfy.tuple
   ~is_keyval:true
   Utils.Strfy.str
   Enc.to_string
   ("encoding", x))
   (fun (x : EConstr.t) ->
   Utils.Strfy.tuple
   ~is_keyval:true
   Utils.Strfy.str
   (Rocq_utils.Strfy.econstr env sigma)
   ("econstr", x)))
   (Enc.bck_to_list bck_map)));
   return ())
   ;; *)

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
     let goals_string = Utils.Strfy.list Rocq_utils.Strfy.evar the_data.goals in
     let all_goals_string =
       Utils.Strfy.list
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
       Utils.Strfy.list
         ~force_newline:true
         ~indent:1
         (Utils.Strfy.tuple
            (Utils.Strfy.list ~force_newline:true ~indent:2 Strfy.evar)
            (Utils.Strfy.list ~force_newline:true ~indent:2 Strfy.evar))
         the_data.stack
       (* Utils.Strfy.list2
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
          (Rocq_utils.Strfy.pp (Proof.pr_proof the_proof))));
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
   (Rocq_utils.Strfy.pp (Proof.pr_proof the_proof)));
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
  let module TransitionTbl =
    Hashtbl.Make (struct
      include Enc
    end)
  in
  { state = st
  ; value = (module TransitionTbl : Hashtbl.S with type key = Enc.t)
  }
;;

let make_state_set (st : wrapper ref)
  : (module Set.S with type elt = Enc.t) in_context
  =
  let module StateSet =
    Set.Make (struct
      include Enc
    end)
  in
  { state = st; value = (module StateSet : Set.S with type elt = Enc.t) }
;;

let make_state_tree_pair_set (st : wrapper ref)
  : (module Set.S with type elt = Enc.t * Mebi_constr.Tree.t) in_context
  =
  let module PairSet =
    Set.Make (struct
      type t = Enc.t * Mebi_constr.Tree.t

      let compare t1 t2 =
        Utils.compare_chain
          [ Enc.compare (fst t1) (fst t2)
          ; Mebi_constr.Tree.compare (snd t1) (snd t2)
          ]
      ;;
    end)
  in
  { state = st
  ; value = (module PairSet : Set.S with type elt = Enc.t * Mebi_constr.Tree.t)
  }
;;

(* let debug_encoding () : unit mm =
  fun (st : wrapper ref) ->
  if Int.equal 0 (F.length !st.fwd_enc)
  then (
    Log.debug "mebi_wrapper.debug_encoding, fwd encoding is empty";
    if Int.equal 0 (B.length !st.bck_enc)
    then Log.debug "mebi_wrapper.debug_encoding, bck encoding is empty"
    else
      B.iter
        (fun (enc : Enc.t) (t : EConstr.t) ->
          Log.debug
            (Printf.sprintf
               "(%s) => %s "
               (Enc.to_string enc)
               (econstr_to_string t)))
        !st.bck_enc)
  else
    F.iter
      (fun (t : EConstr.t) (enc : Enc.t) ->
        Log.debug
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
;; *)

(****************************************************************************)
