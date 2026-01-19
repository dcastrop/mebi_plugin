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
  state (fun env sigma -> Rocq_utils.get_next env sigma original)
;;

(*********************************************************)

let subst_of_decl (substl : EConstr.Vars.substl) x : EConstr.t mm =
  Log.trace __FUNCTION__;
  let ty : EConstr.t = Context.Rel.Declaration.get_type x in
  let$+ subst _ _ = EConstr.Vars.substl substl ty in
  return subst
;;

(** [mk_ctx_subst ?substl x] returns a new [evar] made from the type of [x], using any [substl] provided.
    @param ?substl
      is a list of substitutions, (* TODO: provided so that collisions don't occur? *)
    @param x
      corresponds to a (* TODO: universally? *) quantified term of a constructor.
    @return a new [evar] for [x]. *)
let mk_ctx_subst
      (substl : EConstr.Vars.substl)
      (x : ('a, EConstr.t, 'b) Context.Rel.Declaration.pt)
  : EConstr.t mm
  =
  Log.trace __FUNCTION__;
  let* subst : EConstr.t = subst_of_decl substl x in
  let$ vt env sigma = Evarutil.new_evar env sigma subst in
  return vt
;;

(** [mk_ctx_substl acc ts] makes an [evar] for each term declaration in [ts].
    @param acc
      contains the substitutions accumulated so far, and is returned once [ts=[]]
    @param ts
      is an [EConstr.rel_declaration list] (obtained from the context of a constructor).
    @return [acc] of [evars] once [ts] is empty. *)
let rec mk_ctx_substl (acc : EConstr.Vars.substl)
  :  ('a, EConstr.t, 'b) Context.Rel.Declaration.pt list
  -> EConstr.Vars.substl mm
  =
  Log.trace __FUNCTION__;
  function
  | [] -> return acc
  | t :: ts ->
    let* vt : EConstr.t = mk_ctx_subst acc t in
    mk_ctx_substl (vt :: acc) ts
;;

(***********************************************************************)

exception ConstructorArgsExpectsArraySize3 of unit

type constructor_args =
  { lhs : EConstr.t
  ; act : EConstr.t
  ; rhs : EConstr.t
  }

let constructor_args (args : EConstr.t array) : constructor_args =
  Log.trace __FUNCTION__;
  if Int.equal (Array.length args) 3
  then { lhs = args.(0); act = args.(1); rhs = args.(2) }
  else raise (*TODO:err*) (ConstructorArgsExpectsArraySize3 ())
;;

(** [extract_args ?substl term] returns an [EConstr.t] triple of arguments of an inductively defined LTS, e.g., [term -> option action -> term -> Prop].
    @param ?substl
      is a list of substitutions applied to the terms prior to being returned.
    @param term
      must be of [Constr.kind] [App(fn, args)] (i.e., the application of some inductively defined LTS, e.g., [termLTS (tpar (tact (Send A) tend) (tact (Recv A) tend)) (Some A) (tpar tend tend)]).
    @return a triple of [lhs_term, action, rhs_term]. *)
let extract_args ?(substl : EConstr.Vars.substl = []) (term : Constr.t)
  : constructor_args mm
  =
  Log.trace __FUNCTION__;
  match Constr.kind term with
  | App (_name, args) ->
    if Array.length args == 3
    then (
      let args = EConstr.of_constr_array args in
      let args = Array.map (EConstr.Vars.substl substl) args in
      let args = constructor_args args in
      (* let* () = debug_extract_args _name args in *)
      return args)
    else (* TODO: err *) invalid_lts_args_length (Array.length args)
  | _ -> (* TODO: err *) invalid_lts_term_kind term
;;

(***********************************************************************)

exception Model_info_CouldNotExtractBinding of unit

(* let try_extract_binding (x:Constr.t) : (Tactypes.quantified_hypothesis * EConstr.t ) CAst.t =

   ;; *)

exception Model_info_CouldNotExtractBindings of unit

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

let get_constr_app (x : Constr.t) : Constr.t Rocq_utils.kind_pair =
  try Rocq_utils.constr_to_app x with
  | Rocq_utils.Rocq_utils_ConstrIsNot_App _ ->
    raise (Model_info_CouldNotExtractBindings ())
;;

let unpack_constr_args ((_, tys) : Constr.t Rocq_utils.kind_pair)
  : Constr.t * Constr.t * Constr.t
  =
  try tys.(0), tys.(1), tys.(2) with
  (* NOTE: in case [tys.(_)] is out of bounds. *)
  | Not_found -> raise (Model_info_CouldNotExtractBindings ())
;;

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

(** returns tuple list of [(binding_name * evar)] -- TODO: map these to the [_UNBOUND_REL_X] and
*)
let map_decl_evar_pairs (xs : econstr_decl list) (ys : EConstr.Vars.substl)
  : (EConstr.t * Names.Name.t) list
  =
  Log.trace __FUNCTION__;
  Log.debug ~__FUNCTION__ "C1";
  let ss = List.combine ys (List.map Context.Rel.Declaration.get_name xs) in
  (* Log.debug ~__FUNCTION__ "C2";
     List.iter (fun (x, y) -> pair_map_to_string x y) ss;
     Log.debug ~__FUNCTION__ "C2.1";
     (* let fm : Names.Name.t F.t = F.create 0 in *)
     let fm : Names.Name.t E.t = E.create 0 in
     (* let fm : (EConstr.t, Names.Name.t) Hashtbl.t = Hashtbl.create 0 in *)
     Log.debug ~__FUNCTION__ "C3";
     List.iter
     (fun (k, v) ->
     Log.thing ~__FUNCTION__ Debug "key" k Strfy.feconstr;
     Log.thing
     ~__FUNCTION__
     Debug
     "value"
     v
     (Utils.Strfy.Of (fun v -> Rocq_utils.Strfy.pp (Names.Name.print v)));
     Log.debug ~__FUNCTION__ "C3.1";
     if E.mem fm k
     then Log.thing ~__FUNCTION__ Warning "duplicate key" k Strfy.feconstr;
     Log.debug ~__FUNCTION__ "C3.2";
     E.add fm k v;
     Log.debug ~__FUNCTION__ "C3.3")
     ss;
     Log.debug ~__FUNCTION__ "C4";
     (* let fmss = Hashtbl.to_seq fm in *)
     Log.debug ~__FUNCTION__ "C5";
     (* let fm = E.of_seq fmss in *)
     Log.debug ~__FUNCTION__ "C6";
     fm *)
  ss
;;

(* ss |> List.to_seq |> F.of_seq *)

module C : Hashtbl.S with type key = Constr.t = Hashtbl.Make (struct
    type t = Constr.t

    let equal : t -> t -> bool = Constr.equal
    let hash : t -> int = Constr.hash
  end)

type binding_extractor = Names.Name.t * binding_instructions

and binding_instructions =
  | Undefined
  | Done
  | Arg of
      { root : Constr.t
      ; index : int
      ; cont : binding_instructions
      }

let rec binding_instructions_to_string : binding_instructions -> string =
  function
  | Undefined -> "Undefined"
  | Done -> "Done"
  | Arg { root; index; cont } ->
    Printf.sprintf
      "Arg { root: %s; index: %i; cont: %s}"
      (Strfy.constr root)
      index
      (binding_instructions_to_string cont)
;;

exception Mebi_utils_CannotAddAfterDone of unit

let rec add_instruction (x : binding_instructions)
  : binding_instructions -> binding_instructions
  =
  Log.trace __FUNCTION__;
  function
  | Arg { root; index; cont } ->
    Arg { root; index; cont = add_instruction x cont }
  | Undefined -> x
  | Done -> raise (Mebi_utils_CannotAddAfterDone ())
;;

let rec instruction_length : binding_instructions -> int =
  Log.trace __FUNCTION__;
  function
  | Undefined -> 0
  | Done -> 1
  | Arg { cont; _ } -> 1 + instruction_length cont
;;

exception Mebi_utils_CannotFindBindingName of EConstr.t
(*
   let find_name (name_map : Names.Name.t E.t) (x : EConstr.t) : Names.Name.t =
   Log.trace __FUNCTION__;
   match E.find_opt name_map x with
   | Some x -> x
   | None -> raise (Mebi_utils_CannotFindBindingName x)
   ;; *)

let find_name (name_map : (EConstr.t * Names.Name.t) list) (x : EConstr.t)
  : Names.Name.t
  =
  Log.trace __FUNCTION__;
  let econstr_eq (x : EConstr.t) (y : EConstr.t) = runkeep (econstr_eq x y) in
  match List.filter (fun (y, _) -> econstr_eq x y) name_map with
  | [] -> raise (Mebi_utils_CannotFindBindingName x)
  | (_, v) :: _ -> v
;;

let replace_if_shorter
      (cmap : binding_extractor C.t)
      (k : Constr.t)
      ((vn, vi) : binding_extractor)
  : unit
  =
  match C.find_opt cmap k with
  | None -> C.add cmap k (vn, vi)
  | Some (un, ui) ->
    (match Int.compare (instruction_length vi) (instruction_length ui) with
     | -1 -> C.replace cmap k (vn, vi)
     | _ -> ())
;;

let extract_constructor_binding
      (x : EConstr.t)
      (y : Constr.t)
      (name_map : (EConstr.t * Names.Name.t) list)
  : binding_extractor C.t
  =
  Log.debug __FUNCTION__;
  let econstr_kind (x : EConstr.t) = runkeep (econstr_kind x) in
  let e_constr_eq (x : EConstr.t) (y : Constr.t) =
    runkeep (EConstr.of_constr y |> econstr_eq x)
  in
  let cmap : binding_extractor C.t = C.create 0 in
  let rec f
            (acc : (Constr.t * binding_extractor) list)
            (b : binding_instructions)
            ((x, y) : EConstr.t * Constr.t)
    : unit
    =
    Log.debug __FUNCTION__;
    match econstr_kind x, Constr.kind y with
    | App (xty, xtys), App (yty, ytys) ->
      if e_constr_eq xty yty
      then (
        (* NOTE: set to [-1] so that it is [0] on first use. *)
        let (tysindex, _), _ = Utils.new_int_counter ~start:(-1) () in
        Array.iter
          (fun (xy : EConstr.t * Constr.t) ->
            let b' =
              add_instruction
                (Arg { root = yty; index = tysindex (); cont = Undefined })
                b
            in
            f acc b' xy)
          (Array.combine xtys ytys))
    | _, Rel _ ->
      replace_if_shorter cmap y (find_name name_map x, add_instruction Done b)
    | _, _ -> ()
  in
  f [] Undefined (x, y);
  cmap
;;

(* exception Mebi_utils_ConstructorBindingNotNeeded of unit *)

exception
  Mebi_utils_NoConstructorInstructionsExtracted of
    (EConstr.t * Constr.t * (EConstr.t * Names.Name.t) list)

type rocq_one_constructor_bindings =
  | One_No_Bindings
  | One_Use_Bindings of binding_extractor C.t

(* let merge_cmaps (a : binding_extractor C.t) : binding_extractor C.t -> unit =
   C.iter (fun k v ->
   match C.find_opt a k with
   | None -> C.add a k v
   | Some u -> replace_if_shorter a k u)
   ;; *)

(* let merge_cmap_list (cmap : binding_extractor C.t)
   : binding_extractor C.t list -> unit
   =
   List.iter (merge_cmaps cmap)
   ;; *)

(* let use_no_bindings (xs : rocq_one_constructor_bindings list) : bool =
   let rec f : rocq_one_constructor_bindings list -> binding_extractor C.t list =
   function
   | [] -> []
   | One_No_Bindings :: tl -> f tl
   | One_Use_Bindings ys :: tl -> ys :: f tl
   in
   List.is_empty (f xs)
   ;; *)

let extract_binding
      (x : EConstr.t)
      (y : Constr.t)
      (name_map : (EConstr.t * Names.Name.t) list)
  : rocq_one_constructor_bindings
  =
  Log.debug __FUNCTION__;
  let cmap = extract_constructor_binding x y name_map in
  match C.to_seq_values cmap |> List.of_seq with
  | [] -> raise (Mebi_utils_NoConstructorInstructionsExtracted (x, y, name_map))
  | [ (_, Done) ] -> One_No_Bindings
  | _ -> One_Use_Bindings cmap
;;

exception Mebi_utils_BindingInstruction_NotApp of EConstr.t
exception Mebi_utils_BindingInstruction_Undefined of EConstr.t * EConstr.t
exception Mebi_utils_BindingInstruction_IndexOutOfBounds of EConstr.t * int
exception Mebi_utils_BindingInstruction_NEQ of EConstr.t * Constr.t

type binding_cmaps =
  { from : binding_extractor C.t option
  ; label : binding_extractor C.t option
  ; goto : binding_extractor C.t option
  }

let use_no_bindings : binding_cmaps -> bool = function
  | { from = None; label = None; goto = None } -> true
  | _ -> false
;;

let make_cmaps
      (name_map : (EConstr.t * Names.Name.t) list)
      (from : EConstr.t * Constr.t)
      (label : EConstr.t * Constr.t)
      (goto : EConstr.t * Constr.t)
  : binding_cmaps
  =
  Log.trace __FUNCTION__;
  let f : rocq_one_constructor_bindings -> binding_extractor C.t option =
    function
    | One_No_Bindings -> None
    | One_Use_Bindings xmap -> Some xmap
  in
  let g ((a, b) : EConstr.t * Constr.t) : binding_extractor C.t option =
    try f (extract_binding a b name_map) with
    | Mebi_utils_NoConstructorInstructionsExtracted (x, y, name_map) -> None
  in
  { from = g from; label = g label; goto = g goto }
;;

(* let make_cmaps'
      (from : rocq_one_constructor_bindings)
      (label : rocq_one_constructor_bindings)
      (goto : rocq_one_constructor_bindings)
  : binding_cmaps
  =
  Log.trace __FUNCTION__;
  let f : rocq_one_constructor_bindings -> binding_extractor C.t option =
    function
    | One_No_Bindings -> None
    | One_Use_Bindings xmap -> Some xmap
  in
  { from = f from; label = f label; goto = f goto }
;; *)

let make_binding_fun (cmaps : binding_cmaps)
  : Model_info.binding_args -> EConstr.t Tactypes.explicit_bindings
  =
  Log.trace __FUNCTION__;
  let econstr_kind (x : EConstr.t) = runkeep (econstr_kind x) in
  let e_constr_eq (x : EConstr.t) (y : Constr.t) =
    runkeep (EConstr.of_constr y |> econstr_eq x)
  in
  (* NOTE: function to obtain the [Name] *)
  let f : Names.Name.t -> Tactypes.quantified_hypothesis = function
    | Names.Name.Anonymous -> Tactypes.AnonHyp (* FIXME: *) 0
    | Names.Name.Name v -> Tactypes.NamedHyp (CAst.make v)
  in
  (* NOTE: function to obtain the actual part of the state/label *)
  let rec g (x : EConstr.t) : binding_instructions -> EConstr.t = function
    | Undefined -> raise (Mebi_utils_BindingInstruction_Undefined (x, x))
    | Done -> x
    | Arg { root; index; cont } ->
      (try
         match econstr_kind x with
         | App (xty, xtys) ->
           if e_constr_eq xty root
           then (
             try g xtys.(index) cont with
             | Invalid_argument _ ->
               raise (Mebi_utils_BindingInstruction_IndexOutOfBounds (x, index)))
           else raise (Mebi_utils_BindingInstruction_NEQ (xty, root))
         | _ -> raise (Mebi_utils_BindingInstruction_NotApp x)
       with
       | Mebi_utils_BindingInstruction_Undefined (_, y) ->
         raise (Mebi_utils_BindingInstruction_Undefined (x, y)))
  in
  let h (x : EConstr.t)
    : binding_extractor C.t option -> EConstr.t Tactypes.explicit_bindings
    = function
    | None -> []
    | Some xmap ->
      C.fold
        (fun (k : Constr.t)
          ((name, instructions) : Names.Name.t * binding_instructions)
          (acc : EConstr.t Tactypes.explicit_bindings) ->
          CAst.make (f name, g x instructions) :: acc)
        xmap
        []
  in
  (* NOTE: make the function to be used later *)
  fun ({ from; label; goto } : Model_info.binding_args)
    : EConstr.t Tactypes.explicit_bindings ->
    let from : EConstr.t = runkeep (decode from.enc) in
    let label : EConstr.t = runkeep (decode label.enc) in
    let goto : EConstr.t = runkeep (decode goto.enc) in
    [ from, cmaps.from; label, cmaps.label; goto, cmaps.goto ]
    |> List.fold_left
         (fun (acc : EConstr.t Tactypes.explicit_bindings)
           ((x, xmap) : EConstr.t * binding_extractor C.t option) ->
           List.flatten [ h x xmap; acc ])
         []
;;

let bs_to_string prefix =
  fun ((k, (name, instructions)) : Constr.t * binding_extractor) ->
  Printf.sprintf
    "%s: %s => %s | %s"
    prefix
    (constr_to_string k)
    (Rocq_utils.Strfy.pp (Names.Name.print name))
    (binding_instructions_to_string instructions)
  |> Log.debug ~__FUNCTION__
;;

let extract_bindings ((ctx, c) : Rocq_utils.ind_constr)
  : Model_info.rocq_constructor_bindings
  =
  Log.trace __FUNCTION__;
  try
    let decls : econstr_decl list = Rocq_utils.get_econstr_decls ctx in
    let substl = runkeep (mk_ctx_substl [] (List.rev decls)) in
    (* Log.things ~__FUNCTION__ Debug "decls" decls Strfy.feconstr_rel_decl; *)
    (* Log.things ~__FUNCTION__ Debug "substl" substl Strfy.feconstr; *)
    let pair_map = map_decl_evar_pairs decls substl in
    (* List.iter (fun (x, y) -> pair_map_to_string x y) pair_map; *)
    let args : constructor_args = runkeep (extract_args ~substl c) in
    let from, label, goto = get_constr_app c |> unpack_constr_args in
    (* let from = extract_binding args.lhs from pair_map in *)
    (* let label = extract_binding args.act label pair_map in *)
    (* let goto = extract_binding args.rhs goto pair_map in *)
    let cmaps : binding_cmaps =
      make_cmaps pair_map (args.lhs, from) (args.act, label) (args.rhs, goto)
    in
    if use_no_bindings cmaps
    then (
      Log.thing ~__FUNCTION__ Debug "No_Bindings" c Strfy.fconstr;
      No_Bindings)
    else (
      Log.thing ~__FUNCTION__ Debug "Use_Bindings" c Strfy.fconstr;
      Use_Bindings (make_binding_fun cmaps))
  with
  | Model_info_CouldNotExtractBindings () -> No_Bindings
;;

let ind_to_rocq_constructors (x : Mebi_ind.t) : Model_info.rocq_constructor list
  =
  (* NOTE: constructor tactic index starts from 1 -- ignore 0 below *)
  let (get_constructor_index, _), _ = Utils.new_int_counter ~start:0 () in
  Array.fold_left
    (fun (acc : Model_info.rocq_constructor list)
      ({ name; constructor } : Mebi_ind.lts_constructor) ->
      let index : int = get_constructor_index () in
      let name : string = Names.Id.to_string name in
      let bindings : Model_info.rocq_constructor_bindings =
        extract_bindings constructor
      in
      { index; name; bindings } :: acc)
    []
    (Mebi_ind.get_lts_constructor_types x)
;;
