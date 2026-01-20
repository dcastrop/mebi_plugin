(***********************************************************************)
module Log : Logger.LOGGER_TYPE = Logger.MkDefault ()

let () = Log.Config.enable_output ()
let () = Log.Config.configure_output Debug true
let () = Log.Config.configure_output Trace true
(***********************************************************************)

type t =
  { enc : Mebi_setup.Enc.t
  ; ind : EConstr.t
  ; kind : kind
  }

(* and info =
  { name : EConstr.t
  ; constr_names : Names.Id.t array
  } *)
and kind =
  | Type of EConstr.t option
  | LTS of lts

and lts =
  { term_type : EConstr.t
  ; label_type :
      EConstr.t (* ; constr_transitions : Rocq_utils.ind_constr array *)
  ; constructor_types : lts_constructor array
  }

and lts_constructor =
  { name : Names.Id.t
  ; constructor : Rocq_utils.ind_constr
  }

(* TODO: remove the unnecessary Mebi_wrapper.mm from these, just add a [try with] around the outside *)
(* open Mebi_wrapper *)
exception Mebi_ind_ExpectedLTSNotType of t

let get_lts_term_type : t -> EConstr.t =
  Log.trace __FUNCTION__;
  function
  | { kind = LTS l; _ } -> l.term_type
  (* | _ -> invalid_cindef_kind () *)
  | x -> raise (Mebi_ind_ExpectedLTSNotType x)
;;

let get_lts_label_type : t -> EConstr.t =
  Log.trace __FUNCTION__;
  function
  | { kind = LTS l; _ } -> l.label_type
  (* | _ -> invalid_cindef_kind () *)
  | x -> raise (Mebi_ind_ExpectedLTSNotType x)
;;

let get_lts_constructor_types : t -> lts_constructor array =
  Log.trace __FUNCTION__;
  function
  | { kind = LTS l; _ } -> l.constructor_types
  (* | _ -> invalid_cindef_kind () *)
  | x -> raise (Mebi_ind_ExpectedLTSNotType x)
;;

let get_lts_constructor_names (x : t) : Names.Id.t array =
  Log.trace __FUNCTION__;
  get_lts_constructor_types x |> Array.map (fun { name; _ } -> name)
;;

let get_lts_constructors (x : t) : Rocq_utils.ind_constr array =
  Log.trace __FUNCTION__;
  get_lts_constructor_types x
  |> Array.map (fun { constructor; _ } -> constructor)
;;

(***********************************************************************)

exception
  Mebi_Utils_mip_InconsistentNumConstructors of Declarations.one_inductive_body

let mip_to_lts_constructors (mip : Declarations.one_inductive_body)
  : lts_constructor array
  =
  Log.trace __FUNCTION__;
  try
    Array.combine mip.mind_consnames mip.mind_nf_lc
    |> Array.fold_left
         (fun (acc : lts_constructor list)
           ((name, constructor) : Names.Id.t * Rocq_utils.ind_constr) ->
           { name; constructor } :: acc)
         []
    |> List.rev
    |> Array.of_list
  with
  | Invalid_argument _ -> raise (Mebi_Utils_mip_InconsistentNumConstructors mip)
;;

(* (** [get_lts_labels_and_terms mib mip] is the mapping of terms (states) and labels (outgoing edges) from [mip].

    @raise invalid_arity
      if lts terms and labels cannot be obtained from [mip]. [mib] is only used in case of error.
*)
let get_lts_labels_and_terms
      (mib : Declarations.mutual_inductive_body)
      (mip : Declarations.one_inductive_body)
  : (Constr.rel_declaration * Constr.rel_declaration) Mebi_wrapper.mm
  =
  Log.trace __FUNCTION__;
  let open Declarations in
  (* get the type of [mip] from [mib]. *)
  let typ = Inductive.type_of_inductive (UVars.in_punivs (mib, mip)) in
  match mip.mind_arity_ctxt |> Utils.split_at mip.mind_nrealdecls with
  | [ t1; a; t2 ] ->
    let open Context.Rel in
    if Declaration.equal Sorts.relevance_equal Constr.equal t1 t2
    then Mebi_wrapper.return (a, t1)
    else Mebi_wrapper.invalid_arity typ
  | _ -> Mebi_wrapper.invalid_arity typ
;;

(** [get_lts_ind_ty gref] *)
let get_name_of_lts (gref : Names.GlobRef.t) : EConstr.t Mebi_wrapper.mm =
  Log.trace __FUNCTION__;
  let open Mebi_wrapper.Syntax in
  let* ind, (mib, mip) = get_lts_ind_prop_mind gref in
  Rocq_utils.get_ind_ty ind mib |> return
;;

(**  *)
let get_ind_lts (i : Mebi_wrapper.Enc.t) (gref : Names.GlobRef.t) : Mebi_ind.t Mebi_wrapper.mm =
  Log.trace __FUNCTION__;
  let open Mebi_wrapper.Syntax in
  let* ind, (mib, mip) = get_lts_ind_prop_mind gref in
  let* lbl, term = get_lts_labels_and_terms mib mip in
  let lts_term : EConstr.t = Rocq_utils.get_ind_ty ind mib in
  
  Mebi_wrapper.return
    { enc = i
    ; ind = lts_term
    ; kind =
        LTS
          { term_type = Rocq_utils.get_decl_type_of_constr term
          ; label_type = Rocq_utils.get_decl_type_of_constr lbl
          ; constructor_types = Mebi_ind.mip_to_lts_constructors mip
          }
    }
;; *)
