(***********************************************************************)
module Log : Logger.LOGGER_TYPE = Logger.MkDefault ()

let () = Log.Config.enable_output ()
let () = Log.Config.configure_output Debug true
let () = Log.Config.configure_output Trace false
(***********************************************************************)

type econstr_decl = Rocq_utils.econstr_decl

(***********************************************************************)

let ref_to_glob (r : Libnames.qualid) : Names.GlobRef.t = Nametab.global r

let ref_list_to_glob_list (l : Libnames.qualid list) : Names.GlobRef.t list =
  Log.trace __FUNCTION__;
  List.fold_left
    (fun (acc : Names.GlobRef.t list) (s : Libnames.qualid) ->
      ref_to_glob s :: acc)
    []
    (List.rev l)
;;

(*********************************************************)

open Mebi_setup
open Mebi_wrapper

(* TODO: move from [Mebi_wrapper] *)
(* module Eq = struct end *)

(* TODO: *)
(* FIXME: one of these causes Anomaly where env is referenced too early? *)
module Strfy = struct
  (******************)
  let fname : Names.Id.t Utils.Strfy.to_string = Of Names.Id.to_string
  let fenc : Enc.t Utils.Strfy.to_string = Of Enc.to_string

  (******************)
  let constr (x : Constr.t) : string = Mebi_wrapper.constr_to_string x
  let fconstr : Constr.t Utils.Strfy.to_string = Of constr

  (******************)
  let econstr (x : EConstr.t) : string = Mebi_wrapper.econstr_to_string x
  let feconstr : EConstr.t Utils.Strfy.to_string = Of econstr

  (******************)
  let ind_constr (x : Rocq_utils.ind_constr) : string =
    runkeep
      (state (fun env sigma -> sigma, Rocq_utils.Strfy.ind_constr env sigma x))
  ;;

  let f_ind_constr : Rocq_utils.ind_constr Utils.Strfy.to_string = Of ind_constr

  (******************)
  let ind_constrs (x : Rocq_utils.ind_constr array) : string =
    runkeep
      (state (fun env sigma -> sigma, Rocq_utils.Strfy.ind_constrs env sigma x))
  ;;

  let f_ind_constrs : Rocq_utils.ind_constr array Utils.Strfy.to_string =
    Of ind_constrs
  ;;

  (******************)
  let constrkind
        ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
        (x : Constr.t)
    : string
    =
    runkeep
      (state (fun env sigma ->
         sigma, Rocq_utils.Strfy.constr_kind env sigma ~args x))
  ;;

  let fconstrkind : Constr.t Utils.Strfy.to_string = Args constrkind

  (******************)
  let encode (x : EConstr.t) : string = runkeep (encode x) |> Enc.to_string
  let fencode : EConstr.t Utils.Strfy.to_string = Of encode

  (******************)
  let decode (x : Enc.t) : string = runkeep (Mebi_wrapper.decode x) |> econstr
  let fdecode : Enc.t Utils.Strfy.to_string = Of decode

  (******************)
  let econstr_rel_decl (x : Rocq_utils.econstr_decl) : string =
    runkeep
      (state (fun env sigma ->
         sigma, Rocq_utils.Strfy.econstr_rel_decl env sigma x))
  ;;

  let feconstr_rel_decl : Rocq_utils.econstr_decl Utils.Strfy.to_string =
    Of econstr_rel_decl
  ;;

  let rocq_constructor_to_string
        ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
        (x : Model_info.rocq_constructor)
    : string
    =
    runkeep
      (state (fun env sigma ->
         ( sigma
         , Model_info.rocq_constructor_to_string
             ~args
             ~envsigma:(Some (env, sigma))
             x )))
  ;;
end

(***********************************************************************)

open Mebi_wrapper.Syntax

(** [get_lts_ind_mind gref]
    @raise invalid_ref_lts if [gref] is not a reference to an inductive type. *)
let get_lts_ind_mind (gref : Names.GlobRef.t)
  : (Names.inductive * Declarations.mind_specif) mm
  =
  Log.trace __FUNCTION__;
  let open Names.GlobRef in
  match gref with
  | IndRef ind ->
    let* env = get_env in
    let mib, mip = Inductive.lookup_mind_specif env ind in
    return (ind, (mib, mip))
  | _ ->
    Log.warning ~__FUNCTION__ "invalid gref (not IndRef)";
    invalid_ref_lts gref
;;

let assert_mip_arity_is_type (mip : Declarations.one_inductive_body) : unit mm =
  Log.trace __FUNCTION__;
  let open Declarations in
  match mip.mind_arity with
  | RegularArity s ->
    (match s.mind_sort with
     | Type _ -> return ()
     | Set -> return ()
     | _ -> invalid_sort_type (Sorts.family s.mind_sort))
  | TemplateArity t -> invalid_sort_type (Sorts.family t.template_level)
;;

let get_lts_ind_type_mind (gref : Names.GlobRef.t)
  : (Names.inductive * Declarations.mind_specif) mm
  =
  Log.trace __FUNCTION__;
  let* ind, (mib, mip) = get_lts_ind_mind gref in
  let* _ = assert_mip_arity_is_type mip in
  return (ind, (mib, mip))
;;

let assert_mip_arity_is_prop (mip : Declarations.one_inductive_body) : unit mm =
  Log.trace __FUNCTION__;
  let open Declarations in
  match mip.mind_arity with
  | RegularArity s ->
    if not (Sorts.is_prop s.mind_sort)
    then invalid_sort_lts (Sorts.family s.mind_sort)
    else return ()
  | TemplateArity t -> invalid_sort_lts (Sorts.family t.template_level)
;;

let get_lts_ind_prop_mind (gref : Names.GlobRef.t)
  : (Names.inductive * Declarations.mind_specif) mm
  =
  Log.trace __FUNCTION__;
  let* ind, (mib, mip) = get_lts_ind_mind gref in
  let* _ = assert_mip_arity_is_prop mip in
  return (ind, (mib, mip))
;;

(** [get_lts_labels_and_terms mib mip] is the mapping of terms (states) and labels (outgoing edges) from [mip].

    @raise invalid_arity
      if lts terms and labels cannot be obtained from [mip]. [mib] is only used in case of error.
*)
let get_lts_labels_and_terms
      (mib : Declarations.mutual_inductive_body)
      (mip : Declarations.one_inductive_body)
  : (Constr.rel_declaration * Constr.rel_declaration) mm
  =
  Log.trace __FUNCTION__;
  let open Declarations in
  (* get the type of [mip] from [mib]. *)
  let typ = Inductive.type_of_inductive (UVars.in_punivs (mib, mip)) in
  match mip.mind_arity_ctxt |> Utils.split_at mip.mind_nrealdecls with
  | [ t1; a; t2 ] ->
    let open Context.Rel in
    if Declaration.equal Sorts.relevance_equal Constr.equal t1 t2
    then return (a, t1)
    else invalid_arity typ
  | _ -> invalid_arity typ
;;

(** [get_lts_ind_ty gref] *)
let get_name_of_lts (gref : Names.GlobRef.t) : EConstr.t mm =
  Log.trace __FUNCTION__;
  let* ind, (mib, mip) = get_lts_ind_prop_mind gref in
  Rocq_utils.get_ind_ty ind mib |> return
;;

(**  *)
let get_ind_lts (i : Enc.t) (gref : Names.GlobRef.t) : Mebi_ind.t mm =
  Log.trace __FUNCTION__;
  let* ind, (mib, mip) = get_lts_ind_prop_mind gref in
  let* lbl, term = get_lts_labels_and_terms mib mip in
  let lts_term : EConstr.t = Rocq_utils.get_ind_ty ind mib in
  let open Mebi_ind in
  return
    { enc = i
    ; ind = lts_term
    ; kind =
        LTS
          { term_type = Rocq_utils.get_decl_type_of_constr term
          ; label_type = Rocq_utils.get_decl_type_of_constr lbl
          ; constructor_types = Mebi_ind.mip_to_lts_constructors mip
          }
    }
;;

(*********************************************************)

let econstr_eq a b : bool mm =
  state (fun env sigma -> sigma, Eq.econstr sigma a b)
;;

let econstr_normalize (x : EConstr.t) : EConstr.t mm =
  let$+ t env sigma = Reductionops.nf_all env sigma x in
  return t
;;

let econstr_kind (x : EConstr.t) : Rocq_utils.econstr_kind mm =
  state (fun env sigma -> sigma, EConstr.kind sigma x)
;;

let econstr_is_evar (x : EConstr.t) : bool mm =
  state (fun env sigma -> sigma, EConstr.isEvar sigma x)
;;

let econstr_to_constr ?(abort_on_undefined_evars : bool = false) (x : EConstr.t)
  : Constr.t mm
  =
  state (fun env sigma -> sigma, Rocq_convert.econstr_to_constr sigma x)
;;

let econstr_to_constr_opt (x : EConstr.t) : Constr.t option mm =
  state (fun env sigma -> sigma, Rocq_convert.econstr_to_constr_opt sigma x)
;;

(*********************************************************)

let constrexpr_to_econstr (x : Constrexpr.constr_expr) : EConstr.t mm =
  state (fun env sigma -> Rocq_convert.constrexpr_to_econstr env sigma x)
;;

let globref_to_econstr (x : Names.GlobRef.t) : EConstr.t mm =
  state (fun env sigma -> sigma, Rocq_convert.globref_to_econstr env x)
;;

(*********************************************************)

let type_of_econstr (x : EConstr.t) : EConstr.t mm =
  let* t : EConstr.t = econstr_normalize x in
  state (fun env sigma -> Typing.type_of env sigma t)
;;

let type_of_constrexpr (x : Constrexpr.constr_expr) : EConstr.t mm =
  let* t : EConstr.t = constrexpr_to_econstr x in
  type_of_econstr t
;;

(*********************************************************)

let new_evar_of (x : EConstr.t) : EConstr.t mm =
  state (fun env sigma -> Evarutil.new_evar env sigma x)
;;

let is_theory_term (c : unit -> EConstr.t) (x : EConstr.t) : bool mm =
  state (fun env sigma -> sigma, Mebi_theories.is_constant sigma x c)
;;

let is_none_term : EConstr.t -> bool mm = is_theory_term Mebi_theories.c_None
let is_some_term : EConstr.t -> bool mm = is_theory_term Mebi_theories.c_Some

let get_theory_term_enc_opt (f : EConstr.t -> bool mm) : Enc.t option mm =
  let open Mebi_wrapper.Syntax in
  let* fm = get_fwd_enc in
  let rec find_theory_enc_opt : (EConstr.t * Enc.t) list -> Enc.t option mm =
    function
    | [] -> return None
    | (x, y) :: tl ->
      let* is_match : bool = f x in
      if is_match then return (Some y) else find_theory_enc_opt tl
  in
  F.to_seq fm |> List.of_seq |> find_theory_enc_opt
;;

let get_none_enc_opt () : Enc.t option mm = get_theory_term_enc_opt is_none_term
let get_some_enc_opt () : Enc.t option mm = get_theory_term_enc_opt is_some_term

let try_get_theory_term_enc (f : EConstr.t -> bool mm) (x : EConstr.t)
  : Enc.t option
  =
  if Mebi_wrapper.runkeep (f x)
  then Mebi_wrapper.runkeep (get_theory_term_enc_opt f)
  else None
;;

let try_get_none_enc_opt : EConstr.t -> Enc.t option =
  try_get_theory_term_enc is_none_term
;;

let try_get_some_enc_opt : EConstr.t -> Enc.t option =
  try_get_theory_term_enc is_some_term
;;

(*********************************************************)

let encode_econstr (x : EConstr.t) : Enc.t mm = encode x

let encode_constrexpr (x : Constrexpr.constr_expr) : Enc.t mm =
  Log.trace __FUNCTION__;
  let* t' : EConstr.t = constrexpr_to_econstr x in
  encode_econstr t'
;;

let encode_ref (x : Libnames.qualid) : Enc.t mm =
  Log.trace __FUNCTION__;
  let* ind, (mib, mip) = ref_to_glob x |> get_lts_ind_type_mind in
  let x : EConstr.t = Rocq_utils.get_ind_ty ind mib in
  encode_econstr x
;;

(*********************************************************)

let get_fresh_evar (original : Rocq_utils.evar_source) : EConstr.t mm =
  Log.trace __FUNCTION__;
  state (fun env sigma -> Rocq_utils.get_fresh_evar env sigma original)
;;

(*********************************************************)

let mk_ctx_substl
      (acc : EConstr.Vars.substl)
      (xs : ('a, EConstr.t, 'b) Context.Rel.Declaration.pt list)
  : EConstr.Vars.substl mm
  =
  Log.trace __FUNCTION__;
  state (fun env sigma -> Rocq_utils.mk_ctx_substl env sigma acc xs)
;;

(***********************************************************************)

(** [extract_args ?substl term] returns an [EConstr.t] triple of arguments of an inductively defined LTS, e.g., [term -> option action -> term -> Prop].
    @param ?substl
      is a list of substitutions applied to the terms prior to being returned.
    @param term
      must be of [Constr.kind] [App(fn, args)] (i.e., the application of some inductively defined LTS, e.g., [termLTS (tpar (tact (Send A) tend) (tact (Recv A) tend)) (Some A) (tpar tend tend)]).
    @return a triple of [lhs_term, action, rhs_term]. *)
let extract_args ?(substl : EConstr.Vars.substl = []) (term : Constr.t)
  : Rocq_utils.constructor_args mm
  =
  Log.trace __FUNCTION__;
  try return (Rocq_utils.extract_args ~substl term) with
  | Rocq_utils.Rocq_utils_InvalidLtsArgLength x ->
    (* TODO: err *) invalid_lts_args_length x
  | Rocq_utils.Rocq_utils_InvalidLtsTermKind x -> invalid_lts_term_kind term
;;

(***********************************************************************)

(* exception Model_info_CouldNotExtractBinding of unit *)

(* let try_extract_binding (x:Constr.t) : (Tactypes.quantified_hypothesis * EConstr.t ) CAst.t =

   ;; *)

(* exception Model_info_CouldNotExtractBindings of unit *)

(** [try_extract_bindings]
    @raise Model_info_CouldNotExtractBindings
      if there are no bindings to extract *)
(* let try_extract_bindings ((_rel, c) : Rocq_utils.ind_constr)
  : Model_info.binding_args -> EConstr.t Tactypes.explicit_bindings
  =
  try
    let ty, tys = Rocq_utils.constr_to_app c in
    let extract_bindings (x : Constr.t) : EConstr.t Tactypes.explicit_bindings =
      Log.thing ~__FUNCTION__ Debug "x" x fconstr;
      match Constr.kind x with
      | Constr.Rel _ -> raise (Model_info_CouldNotExtractBinding ())
      | Constr.App (ty, tys) ->
        Log.thing ~__FUNCTION__ Debug "app.ty" ty fconstr;
        (match Constr.kind ty with Constr.App (_, _) -> (
          


        
        ) | _ -> raise (Model_info_CouldNotExtractBinding ()))
      | _ -> raise (Model_info_CouldNotExtractBinding ())
    in
    let cfrom, clabel, cgoto = tys.(0), tys.(1), tys.(2) in
    let state_bindings (x : Constr.t)
      : Model_state.t -> EConstr.t Tactypes.explicit_bindings
      =
      try extract_bindings x with
      | Model_info_CouldNotExtractBinding () -> fun _ -> []
    in
    let label_bindings (x : Constr.t)
      : Model_label.t -> EConstr.t Tactypes.explicit_bindings
      =
      try extract_bindings x with
      | Model_info_CouldNotExtractBinding () -> fun _ -> []
    in
    let to_return ({ from; label; goto } : Model_info.binding_args)
      : EConstr.t Tactypes.explicit_bindings
      =
      match 
      List.flatten 
        [ state_bindings cfrom from
        ; label_bindings clabel label
        ; state_bindings cgoto goto
        ]
  with 
    | [] -> raise (Model_info_CouldNotExtractBindings ())
    | xs -> fun ({ from; label; goto } : Model_info.binding_args) -> xs
    in
    match to_return with
    | [] -> raise (Model_info_CouldNotExtractBindings ())
    | xs -> fun ({ from; label; goto } : Model_info.binding_args) -> to_return
  with
  (* NOTE: in case [tys.(_)] is out of bounds. *)
  | Not_found -> raise (Model_info_CouldNotExtractBindings ())
  | Rocq_utils.Rocq_utils_ConstrIsNot_App _ ->
    raise (Model_info_CouldNotExtractBindings ())
;; *)

(* let get_constr_app (x : Constr.t) : Constr.t Rocq_utils.kind_pair =
   try Rocq_utils.constr_to_app x with
   | Rocq_utils.Rocq_utils_ConstrIsNot_App _ ->
   raise (Model_info_CouldNotExtractBindings ())
   ;; *)

(* let mkfun_map_bindings (x:Constr.t) :(Names.lident * EConstr.t) -> EConstr.t option =
   let m  : Constr.t F.t = F.create 0 in
   let rec foo (x:Constr.t) : unit =

   match Constr.kind x with
   | Rel _ ->
   | App (ty,tys) -> (

   )
   | _ -> (* TODO: *) ()
   (* and bar (xs:Constr.t array) : EConstr.t -> EConstr.t option =
   Array.fold_left (fun acc x ->
   match foo x with | None -> acc | Some x -> x ::
   ) *)

   in
   (* foo x *)
   fun _ -> None

   ;; *)

(** [try_extract_bindings]
    @raise Model_info_CouldNotExtractBindings
      if there are no bindings to extract *)
(* let try_extract_bindings ((ctx, c) : Rocq_utils.ind_constr)
   : Model_info.binding_args -> EConstr.t Tactypes.explicit_bindings
   =
   let decls : Rocq_utils.econstr_decls = Rocq_utils.get_econstr_decls ctx in
   let from, label, goto = get_constr_app c |> unpack_constr_args in
   let extract_bindings (x : Constr.t) (y : Constr.t) (z : Constr.t)
   : EConstr.t Tactypes.explicit_bindings
   =
   Log.thing ~__FUNCTION__ Debug "x" x fconstr;
   match Constr.kind x with
   | Constr.Rel _ -> raise (Model_info_CouldNotExtractBinding ())
   | Constr.App (ty, tys) ->
   Log.thing ~__FUNCTION__ Debug "app.ty" ty fconstr;
   (match Constr.kind ty with
   | Constr.App (_, _) -> []
   | _ -> raise (Model_info_CouldNotExtractBinding ()))
   | _ -> raise (Model_info_CouldNotExtractBinding ())
   in
   fun _ -> []
   ;; *)

(* let resolve_bindings (bs : EConstr.t Tactypes.explicit_bindings option list)
   : EConstr.t Tactypes.explicit_bindings
   =
   Log.trace __FUNCTION__;
   let rec resolve_bindings
   :  EConstr.t Tactypes.explicit_bindings option list
   -> EConstr.t Tactypes.explicit_bindings
   = function
   | [] -> []
   | None :: tl -> resolve_bindings tl
   | Some h :: tl -> List.concat [ h; resolve_bindings tl ]
   in
   (* NOTE: ensure no duplicate bindings *)
   resolve_bindings bs
   |> List.fold_left
   (fun (acc : EConstr.t Tactypes.explicit_bindings)
   (b : (Tactypes.quantified_hypothesis * EConstr.t) CAst.t) ->
   if
   List.exists
   (fun (b' : (Tactypes.quantified_hypothesis * EConstr.t) CAst.t) ->
   (* NOTE: we cannot access content of [CAst] *)
   CAst.eq
   (* NOTE: we only check the [snd] ([EConstr.t]) as we assume that the exact same term would be used under the same decl from some [rel_context]. *)
   (fun ((_, x) : Tactypes.quantified_hypothesis * EConstr.t)
   ((_, y) : Tactypes.quantified_hypothesis * EConstr.t)
   : bool ->
   Mebi_wrapper.runkeep
   (Mebi_wrapper.state (fun env sigma ->
   sigma, Eq.econstr sigma x y)))
   b
   b')
   acc
   then acc
   else b :: acc)
   []
   ;; *)

let _pair_map_to_string =
  fun (k : EConstr.t) (v : Names.Name.t) ->
  Printf.sprintf
    "pairmap: %s => %s"
    (econstr_to_string k)
    (Rocq_utils.Strfy.pp (Names.Name.print v))
  |> Log.debug ~__FUNCTION__
;;

(* module E : Hashtbl.S with type key = EConstr.t = Hashtbl.Make (struct
   type t = EConstr.t

   let equal (x : t) (y : t) : bool = EConstr.eq_constr !(the_coq_ctx ()) x y

   let hash (x : t) : int =
   Constr.hash
   (EConstr.to_constr
   ?abort_on_undefined_evars:(Some false)
   !(the_coq_ctx ())
   x)
   ;;
   end) *)
