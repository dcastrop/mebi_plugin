(***********************************************************************)
module Log : Logger.LOGGER_TYPE = Logger.MkDefault ()

let () = Log.Config.configure_output Debug false
let () = Log.Config.configure_output Trace false
(***********************************************************************)

let ref_to_glob (r : Libnames.qualid) : Names.GlobRef.t = Nametab.global r

let ref_list_to_glob_list (l : Libnames.qualid list) : Names.GlobRef.t list =
  List.fold_left
    (fun (acc : Names.GlobRef.t list) (s : Libnames.qualid) ->
      ref_to_glob s :: acc)
    []
    (List.rev l)
;;

(*********************************************************)

open Mebi_setup
open Mebi_wrapper
open Mebi_wrapper.Syntax

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
  let i_ctx = mip.mind_arity_ctxt in
  let _, i_idx = Utils.split_at mip.mind_nrealdecls i_ctx [] in
  match i_idx with
  | [ t1; a; t2 ] ->
    let open Context.Rel in
    if Declaration.equal Sorts.relevance_equal Constr.equal t1 t2
    then return (a, t1)
    else invalid_arity typ
  | _ -> invalid_arity typ
;;

let get_ind_info (gref : Names.GlobRef.t) : Mebi_ind.info mm =
  Log.trace __FUNCTION__;
  let open Names.GlobRef in
  match gref with
  | IndRef i ->
    let* env = get_env in
    let mib, mip = Inductive.lookup_mind_specif env i in
    let* _ = assert_mip_arity_is_type mip in
    let univ = mib.mind_univ_hyps in
    let type_term = EConstr.mkIndU (i, EConstr.EInstance.make univ) in
    let open Mebi_ind in
    return { name = type_term; constr_names = mip.mind_consnames }
  | _ ->
    Log.warning ~__FUNCTION__ "invalid gref (not IndRef)";
    invalid_ref_type gref
;;

let get_name_of_lts (gref : Names.GlobRef.t) : EConstr.t mm =
  Log.trace __FUNCTION__;
  let open Names.GlobRef in
  match gref with
  | IndRef ind ->
    let* env = get_env in
    let mib, mip = Inductive.lookup_mind_specif env ind in
    let* _ = assert_mip_arity_is_prop mip in
    let* lbl, term = get_lts_labels_and_terms mib mip in
    let univ = mib.mind_univ_hyps in
    (* lts of inductive type *)
    return (EConstr.mkIndU (ind, EConstr.EInstance.make univ))
  | _ ->
    Log.warning ~__FUNCTION__ "invalid gref (not IndRef)";
    invalid_ref_lts gref
;;

(** @raise invalid_ref_lts if [gref] is not a reference to an inductive type. *)
let get_ind_lts (i : Enc.t) (gref : Names.GlobRef.t) : Mebi_ind.t mm =
  Log.trace __FUNCTION__;
  let open Names.GlobRef in
  match gref with
  | IndRef ind ->
    let* env = get_env in
    let mib, mip = Inductive.lookup_mind_specif env ind in
    let* _ = assert_mip_arity_is_prop mip in
    let* lbl, term = get_lts_labels_and_terms mib mip in
    let univ = mib.mind_univ_hyps in
    (* lts of inductive type *)
    let lts_term = EConstr.mkIndU (ind, EConstr.EInstance.make univ) in
    let open Mebi_ind in
    return
      { enc = i
      ; info = { name = lts_term; constr_names = mip.mind_consnames }
      ; kind =
          LTS
            { trm_type =
                EConstr.of_constr (Context.Rel.Declaration.get_type term)
            ; lbl_type =
                EConstr.of_constr (Context.Rel.Declaration.get_type lbl)
            ; constr_transitions = mip.mind_nf_lc
            }
      }
  | _ ->
    Log.warning ~__FUNCTION__ "invalid gref (not IndRef)";
    invalid_ref_lts gref
;;

(*********************************************************)

let econstr_eq a b : bool mm =
  state (fun env sigma -> sigma, Eq.econstr sigma a b)
;;

let econstr_normalize (x : EConstr.t) : EConstr.t mm =
  let$+ t env sigma = Reductionops.nf_all env sigma x in
  return t
;;

let econstr_kind (x : EConstr.t) : Rocq_utils.constr_kind mm =
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

let is_none_term (x : EConstr.t) : bool mm =
  state (fun env sigma ->
    sigma, Mebi_theories.is_constant sigma x Mebi_theories.c_None)
;;

let get_none_enc_opt () : Enc.t option mm =
  let open Mebi_wrapper.Syntax in
  let* fm = get_fwd_enc in
  let rec find_none : (EConstr.t * Enc.t) list -> Enc.t option mm = function
    | [] -> return None
    | (x, y) :: tl ->
      let* is_none : bool = is_none_term x in
      if is_none then return (Some y) else find_none tl
  in
  F.to_seq fm |> List.of_seq |> find_none
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
  let* info : Mebi_ind.info = get_ind_info (ref_to_glob x) in
  encode_econstr info.name
;;
