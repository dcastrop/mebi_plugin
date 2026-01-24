(***********************************************************************)
(* module Log : Logger.LOGGER_TYPE = Logger.MkDefault () *)

(* let () = Log.Config.configure_output Debug false *)
(* let () = Log.Config.configure_output Trace false *)
(***********************************************************************)

(* module M : Rocq_monad.TYPE = Rocq_monad.Make () *)

(* open Mebi_setup
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
;; *)
