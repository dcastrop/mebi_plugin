open Pp
open Mebi_utils
open Mebi_monad
open Mebi_monad.Monad_syntax

(* *)
open Utils.Logging
open Utils.Formatting
open Utils
open Fsm

let default_params : Params.log = Params.Default.log ~mode:(Coq ()) ()

(** [default_bound] is the total depth that will be explored of a given lts by [explore_lts]. *)
let default_bound : int = 10

(** [arity_is_prop mip] raises an error if [mip.mind_arity] is not a [prop]. *)
let arity_is_prop (mip : Declarations.one_inductive_body) : unit mm =
  let open Declarations in
  match mip.mind_arity with
  | RegularArity s ->
    if not (Sorts.is_prop s.mind_sort)
    then invalid_sort (Sorts.family s.mind_sort)
    else return ()
  | TemplateArity t -> invalid_sort (Sorts.family t.template_level)
;;

(** [get_lts_labels_and_terms mib mip] is the mapping of terms (states) and labels (outgoing edges) from [mip].

    @raise invalid_arity
      if lts terms and labels cannot be obtained from [mip]. [mib] is only used in case of error. *)
let get_lts_labels_and_terms
  (mib : Declarations.mutual_inductive_body)
  (mip : Declarations.one_inductive_body)
  : (Constr.rel_declaration * Constr.rel_declaration) mm
  =
  let open Declarations in
  (* get the type of [mip] from [mib]. *)
  let typ = Inductive.type_of_inductive (UVars.in_punivs (mib, mip)) in
  let i_ctx = mip.mind_arity_ctxt in
  let _, i_idx = split_at mip.mind_nrealdecls i_ctx [] in
  match i_idx with
  | [ t1; a; t2 ] ->
    let open Context.Rel in
    if Declaration.equal Sorts.relevance_equal Constr.equal t1 t2
    then return (a, t1)
    else invalid_arity typ
  | _ -> invalid_arity typ
;;

(** [raw_lts] is an LTS type used to describe Coq-based terms.
    @param [coq_lts] is the type constructor.
    @param [trm_type] is the type of terms for the LTS.
    @param [lbl_type] is the type of labels for the LTS.
    @param [coq_ctor_names]
      is the array of names for each constructor of the Coq term.
    @param [constructor_transitions]
      is the array of constructors of the Coq term (i.e., the transitions or outgoing edges). *)
type raw_lts =
  { coq_lts : EConstr.t
  ; trm_type : EConstr.types
  ; lbl_type : EConstr.types
  ; coq_ctor_names : Names.Id.t array
  ; constructor_transitions : (Constr.rel_context * Constr.types) array
  }

type term_type_map =
  (* (EConstr.types, (Constr.rel_context * Constr.types) array) Hashtbl.t *)
  (EConstr.types, raw_lts) Hashtbl.t

(** [_log_raw_lts] *)
let _log_raw_lts ?(params : Params.log = default_params) (rlts : raw_lts)
  : unit mm
  =
  Log.override
    ~params
    (Printf.sprintf
       "(a) Types of terms: %s.\n"
       (econstr_to_string rlts.trm_type));
  Log.override
    ~params
    (Printf.sprintf
       "(b) Types of labels: %s.\n"
       (econstr_to_string rlts.lbl_type));
  Log.override
    ~params
    (Printf.sprintf
       "(c) Constructors: %s.\n"
       (Pp.string_of_ppcmds
          (Pp.prvect_with_sep
             (fun _ -> str ", ")
             Names.Id.print
             rlts.coq_ctor_names)));
  Log.override
    ~params
    (Printf.sprintf
       "(d) Types of coq_lts: %s.\n"
       (econstr_to_string rlts.coq_lts));
  (* log
     ~params
     (Printf.sprintf
     "(f) Transitions (do not be alarmed by any _UNBOUND_ below): %s.\n"
     (if Array.length rlts.constructor_transitions < 1
     then "[] (empty)"
     else
     Printf.sprintf
     "[%s]"
     (Array.fold_left
     (fun (acc : string) (tr : Constr.rel_context * Constr.t) ->
     Printf.sprintf "%s   %s\n" acc (constr_to_string (snd tr)))
     "\n"
     rlts.constructor_transitions))); *)
  return ()
;;

(** [check_ref_lts gref] is the [raw_lts] of [gref].

    @raise invalid_ref if [gref] is not a reference to an inductive type. *)
let check_ref_lts (gref : Names.GlobRef.t) : raw_lts mm =
  let open Names.GlobRef in
  match gref with
  | IndRef i ->
    let* env = get_env in
    let mib, mip = Inductive.lookup_mind_specif env i in
    let* _ = arity_is_prop mip in
    let* lbl, term = get_lts_labels_and_terms mib mip in
    let univ = mib.mind_univ_hyps in
    (* lts of inductive type *)
    let lts = EConstr.mkIndU (i, EConstr.EInstance.make univ) in
    return
      { coq_lts = lts
      ; trm_type = EConstr.of_constr (Context.Rel.Declaration.get_type term)
      ; lbl_type = EConstr.of_constr (Context.Rel.Declaration.get_type lbl)
      ; coq_ctor_names = mip.mind_consnames
      ; constructor_transitions = mip.mind_nf_lc
      }
  (* raise error if [gref] is not an inductive type *)
  | _ -> invalid_ref gref
;;

(* FIXME: All of the code below, up to [check_valid_constructor] needs
   reworking *)
(* FIXME: Weird interaction between exceptions and monadic code. Try/cut *)
(* CANNOT be wrapped around monadic code. Otherwise, the exception is *)
(* *not* caught *)
(** Checks if two terms unify
    TODO: lots of doubts
    - Conversion.CUMUL?
    - Is [w_unify] the best way?
    - ... *)
let m_unify
  ?(params : Params.log = default_params)
  (t0 : EConstr.t)
  (t1 : EConstr.t)
  : bool mm
  =
  params.kind <- Debug ();
  (* let* _ =
     if is_output_kind_enabled params
     then
     debug (fun (env : Environ.env) (sigma : Evd.evar_map) ->
     str "Unifying (t0) :: "
     ++ Printer.pr_econstr_env env sigma t0
     ++ strbrk "\nUnifying (t1) :: "
     ++ Printer.pr_econstr_env env sigma t1)
     else return ()
     in *)
  state (fun (env : Environ.env) (sigma : Evd.evar_map) ->
    try
      let sigma = Unification.w_unify env sigma Conversion.CUMUL t0 t1 in
      (* log ~params "\t\tSuccess"; *)
      sigma, true
    with
    | Pretype_errors.PretypeError (_, _, Pretype_errors.CannotUnify (m, n, e))
      ->
      log ~params "\t\tCould not unify";
      sigma, false)
;;

(** [mk_ctx_substl] *)
let rec mk_ctx_substl (substl : EConstr.t list)
  : ('a, EConstr.t, 'b) Context.Rel.Declaration.pt list -> EConstr.t list mm
  = function
  | [] -> return substl
  | t :: ts ->
    (* get type of [t] *)
    let ty = EConstr.Vars.substl substl (Context.Rel.Declaration.get_type t) in
    (* *)
    let$ vt env sigma = Evarutil.new_evar env sigma ty in
    mk_ctx_substl (vt :: substl) ts
;;

(** Extract args of a type [LTS termL act termR]
    Prerequisite: input *must* be of this shape *)
let extract_args (substl : EConstr.Vars.substl) (tm : Constr.t) =
  match Constr.kind tm with
  | App (_, args) ->
    assert (Array.length args == 3);
    let args = EConstr.of_constr_array args in
    let args = Array.map (EConstr.Vars.substl substl) args in
    args.(0), args.(1), args.(2)
  | _ -> assert false
;;

type unif_problem =
  { termL : EConstr.t
  ; termR : EConstr.t
  }

let _pstr_unif_problem (t : unif_problem) : string =
  match t with
  | { termL; termR; _ } ->
    Printf.sprintf
      "{ L: %s;\n  R: %s; }"
      (econstr_to_string termL)
      (econstr_to_string termR)
;;

let rec unify_all
  ?(params : Params.log = default_params)
  (i : (Constr_tree.t * unif_problem) list)
  : Constr_tree.t list option t
  =
  params.kind <- Debug ();
  match i with
  | [] -> return (Some [])
  | (ctor_tree, u) :: t ->
    (* let* _ =
       if is_output_kind_enabled params
       then
       debug (fun env sigma ->
       str "UNIFYALL (termL) :::::::::: "
       ++ Printer.pr_econstr_env env sigma u.termL
       ++ strbrk "\nUNIFYALL (termR) :::::::::: "
       ++ Printer.pr_econstr_env env sigma u.termR)
       else return ()
       in *)
    let* success = m_unify ~params u.termL u.termR in
    if success
    then
      let* unified = unify_all ~params t in
      match unified with
      | None -> return None
      | Some unified -> return (Some (ctor_tree :: unified))
    else return None
;;

let sandboxed_unify
  ?(params : Params.log = default_params)
  (tgt_term : EConstr.t)
  (u : (Constr_tree.t * unif_problem) list)
  : (EConstr.t * Constr_tree.t list) option mm
  =
  (* let* _ =
     if is_output_kind_enabled params
     then
     debug (fun env sigma ->
     str "TGT:::::: " ++ Printer.pr_econstr_env env sigma tgt_term)
     else return ()
     in *)
  sandbox
    (let* success = unify_all ~params u in
     match success with
     | None -> return None
     | Some unified ->
       let$+ term env sigma = Reductionops.nf_all env sigma tgt_term in
       let$+ is_undefined _ sigma = EConstr.isEvar sigma term in
       if is_undefined then return None else return (Some (term, unified)))
;;

(** [coq_ctor]
    - [EConstr.t] action
    - [EConstr.t] destination
    - [Constr_tree.t] coq-constructor index *)
type coq_ctor = EConstr.t * EConstr.t * Constr_tree.t

(* [act] should probably come from the unification problems? *)
let rec retrieve_tgt_nodes
  ?(params : Params.log = default_params)
  (acc : coq_ctor list)
  (i : int)
  (act : EConstr.t)
  (tgt_term : EConstr.t)
  : (Constr_tree.t * unif_problem) list list -> coq_ctor list mm
  = function
  | [] -> return acc
  | u1 :: nctors ->
    let* success = sandboxed_unify ~params tgt_term u1 in
    (match success with
     | None -> retrieve_tgt_nodes ~params acc i act tgt_term nctors
     | Some (tgt, ctor_tree) ->
       let$+ act env sigma = Reductionops.nf_all env sigma act in
       retrieve_tgt_nodes
         ~params
         ((act, tgt, Node (i, ctor_tree)) :: acc)
         i
         act
         tgt_term
         nctors)
;;

(* Should return a list of unification problems *)
let rec check_updated_ctx
  ?(params : Params.log = default_params)
  (acc : (Constr_tree.t * unif_problem) list list)
  (fn_rlts : term_type_map)
  :  EConstr.t list * EConstr.rel_declaration list
  -> (Constr_tree.t * unif_problem) list list option mm
  = function
  | [], [] -> return (Some acc)
  | _ :: substl, t :: tl ->
    let$+ upd_t env sigma =
      EConstr.Vars.substl substl (Context.Rel.Declaration.get_type t)
    in
    let* sigma = get_sigma in
    (match EConstr.kind sigma upd_t with
     | App (fn, args) ->
       (* Log.override
          ~params
          (Printf.sprintf
          (* "check_updated_ctx,\nApp fn: %s\nApp args: %s" *)
          "check_updated_ctx,\nApp fn: %s"
          (econstr_to_string fn)
          (* (List.fold_left
          (fun (pacc : string) (arg : EConstr.t) ->
          Printf.sprintf "%s '%s'" pacc (econstr_to_string arg))
          ""
          (Array.to_list args)) *)); *)
       (match Hashtbl.find_opt fn_rlts fn with
        | None ->
          Log.warning
            ~params
            (Printf.sprintf
               "check_updated_ctx, fn_rlts does not have corresponding fn: %s."
               (econstr_to_string fn));
          check_updated_ctx acc fn_rlts (substl, tl)
        | Some rlts ->
          let$+ nextT env sigma = Reductionops.nf_evar sigma args.(0) in
          let* ctors =
            check_valid_constructor
              ~params
              rlts.constructor_transitions
              fn_rlts
              nextT
              (Some args.(1))
          in
          if List.is_empty ctors
          then return None
          else (
            let ctors =
              List.map
                (fun (_, (tL : EConstr.t), (i : Constr_tree.t)) ->
                  i, { termL = tL; termR = args.(2) })
                ctors
            in
            (* We need to cross-product all possible unifications. This is in case
               we have a constructor of the form LTS t11 a1 t12 -> LTS t21 a2
               t22 -> ... -> LTS tn an t2n. Repetition may occur. It is not
               unavoidable, but we should make sure we understand well the
               problem before removing the source of repetition. *)
            (* FIXME: Test this *)
            (* replace [rtls] in [rtls_ctx] *)
            (* let rtls_ctx':rlts_list =
               List.append [  ]
               in *)
            check_updated_ctx
              (List.concat_map (fun x -> List.map (fun y -> y :: x) ctors) acc)
              fn_rlts
              (substl, tl)))
     | _ -> check_updated_ctx acc fn_rlts (substl, tl))
  | _, _ -> assert false
(* Impossible! *)
(* FIXME: should fail if [t] is an evar -- but *NOT* if it contains evars! *)

(** Checks possible transitions for this term: *)
and check_valid_constructor
  ?(params : Params.log = default_params)
  (ctor_transitions : (Constr.rel_context * Constr.types) array)
  (fn_rlts : term_type_map)
  (t' : EConstr.t)
  (ma : EConstr.t option)
  : coq_ctor list mm
  =
  params.kind <- Debug ();
  let$+ t env sigma = Reductionops.nf_all env sigma t' in
  let iter_body (i : int) (ctor_vals : coq_ctor list) =
    (* let* _ =
       if is_output_kind_enabled params
       then
       debug (fun env sigma ->
       str "CHECKING CONSTRUCTOR "
       ++ int i
       ++ str ". Term: "
       ++ Printer.pr_econstr_env env sigma t)
       else return ()
       in *)
    let (ctx, tm) : Constr.rel_context * Constr.t = ctor_transitions.(i) in
    let ctx_tys : EConstr.rel_declaration list =
      List.map EConstr.of_rel_decl ctx
    in
    let* substl = mk_ctx_substl [] (List.rev ctx_tys) in
    let (termL, act, termR) : Evd.econstr * Evd.econstr * Evd.econstr =
      extract_args substl tm
    in
    let* success = m_unify t termL in
    if success
    then
      let* success = Option.cata (fun a -> m_unify a act) (return true) ma in
      if success
      then
        let* (next_ctors : (Constr_tree.t * unif_problem) list list option) =
          check_updated_ctx ~params [ [] ] fn_rlts (substl, ctx_tys)
        in
        let$+ act env sigma = Reductionops.nf_all env sigma act in
        let tgt_term : EConstr.t = EConstr.Vars.substl substl termR in
        match next_ctors with
        | None -> return ctor_vals
        | Some [] ->
          let* sigma = get_sigma in
          if EConstr.isEvar sigma tgt_term
          then return ctor_vals
          else return ((act, tgt_term, Constr_tree.Node (i, [])) :: ctor_vals)
        | Some nctors ->
          let tgt_nodes =
            retrieve_tgt_nodes ~params ctor_vals i act tgt_term nctors
          in
          tgt_nodes
      else return ctor_vals
    else return ctor_vals
  in
  iterate 0 (Array.length ctor_transitions - 1) [] iter_body
;;

(** [GraphB] is ...
    (Essentially acts as a `.mli` for the [MkGraph] module.) *)
module type GraphB = sig
  module H : Hashtbl.S with type key = EConstr.t
  module S : Set.S with type elt = EConstr.t
  module D : Set.S with type elt = EConstr.t * Constr_tree.t

  type constr_transitions = (Mebi_action.action, D.t) Hashtbl.t

  type lts_graph =
    { to_visit : EConstr.t Queue.t
    ; states : S.t
    ; transitions : constr_transitions H.t
    }

  val insert_constr_transition
    :  constr_transitions
    -> Mebi_action.action
    -> EConstr.t
    -> Constr_tree.t
    -> unit mm

  val add_new_term_constr_transition
    :  lts_graph
    -> EConstr.t
    -> Mebi_action.action
    -> EConstr.t
    -> Constr_tree.t
    -> unit mm

  val build_lts_graph
    :  ?params:Params.log
    -> term_type_map (* -> raw_lts H.t -> raw_lts H.t *)
    -> lts_graph
    -> int
    -> lts_graph mm

  val build_graph
    :  ?params:Params.log
    -> term_type_map (*  -> term_type_map *)
    -> Constrexpr.constr_expr (* -> Names.GlobRef.t list *)
    -> int
    -> lts_graph mm

  module DeCoq : sig
    type coq_translation =
      { from_coq : string H.t
      ; to_coq : (string, EConstr.t) Hashtbl.t
      }

    (* val translate_coq_terms : ?params:Params.log -> S.t -> coq_translation mm

       val translate_coq_lts
       :  ?params:Params.log
       -> constr_transitions H.t
       -> coq_translation (* -> string list *)
       -> Lts.raw_flat_lts mm *)

    val lts_graph_to_lts
      :  ?params:Params.log
      -> ?bound:int
      -> ?name:string
      -> lts_graph
      -> Constrexpr.constr_expr
      -> (Lts.lts * coq_translation) mm
  end
end

(** [MkGraph M] is ...
    [M] is a ... *)
module MkGraph
    (M : Hashtbl.S with type key = EConstr.t)
    (N : Set.S with type elt = EConstr.t)
    (P : Set.S with type elt = EConstr.t * Constr_tree.t) : GraphB = struct
  (* [H] is the hashtbl of outgoing transitions, from some [EConstr.t] and also is used for mapping term types to [raw_lts]. *)
  module H = M

  (* [S] is the set of states, of [EConstr.t]. *)
  module S = N

  (* [D] is the set of destination tuples, each comprised of a term [EConstr.t] and the corresponding [Constr_tree.t]. *)
  module D = P

  (** [constr_transitions] is a hashtbl mapping [action]s to terms of [EConstr.t] and [Constr_tree.t]. *)
  type constr_transitions = (Mebi_action.action, D.t) Hashtbl.t

  let num_transitions (ts : constr_transitions H.t) : int =
    H.fold
      (fun (_from : EConstr.t) (transitions : constr_transitions) (acc : int) ->
        Hashtbl.fold
          (fun (_a : Mebi_action.action) (destinations : D.t) (acc' : int) ->
            acc' + D.cardinal destinations)
          transitions
          acc)
      ts
      0
  ;;

  (** [lts_graph] is a record containing a queue of [EConstr.t]s [to_visit], a set of states visited (i.e., [EConstr.t]s), and a hashtbl mapping [EConstr.t] to a map of [constr_transitions], which maps [action]s to [EConstr.t]s and their [Constr_tree.t]. *)
  type lts_graph =
    { to_visit : EConstr.t Queue.t
    ; states : S.t
    ; transitions : constr_transitions H.t
    }

  (**********************************************************************)
  (******** below is a sanity check *************************************)
  (******** cannot be moved since it depends on this module *************)
  (******** (i.e., H, D, S )*********************************************)
  (**********************************************************************)

  let _pstr_econstr_states (states : S.t) : string =
    if S.is_empty states
    then "[ ] (empty)"
    else
      Printf.sprintf
        "[%s\n]"
        (S.fold
           (fun (s : S.elt) (acc : string) ->
             Printf.sprintf "%s\n\t%s" acc (econstr_to_string s))
           states
           "")
  ;;

  let _pstr_constr_transition
    (f : EConstr.t)
    (a : Mebi_action.action)
    ((d, c) : EConstr.t * Constr_tree.t)
    : string
    =
    Printf.sprintf
      "{|from: %s; label: %s; dest: %s; tree: %s|}"
      (econstr_to_string f)
      a.label
      (econstr_to_string d)
      (Constr_tree.pstr c)
  ;;

  let _pstr_list_constr_transitions
    (f : EConstr.t)
    (a : Mebi_action.action)
    (ds : D.t)
    : string list
    =
    let l = D.to_list ds in
    let hd = List.hd l
    and tl = List.tl l in
    List.fold_left
      (fun (acc : string list) d -> _pstr_constr_transition f a d :: acc)
      [ _pstr_constr_transition f a hd ]
      tl
  ;;

  let _pstr_list_term_transitions (f : EConstr.t) (cs : constr_transitions)
    : string list
    =
    let keys = List.of_seq (Hashtbl.to_seq_keys cs) in
    let hd = List.hd keys
    and tl = List.tl keys in
    let hd_ds = Hashtbl.find cs hd in
    List.fold_left
      (fun (acc : string list) (a : Mebi_action.action) ->
        List.append (_pstr_list_constr_transitions f a (Hashtbl.find cs a)) acc)
      (_pstr_list_constr_transitions f hd hd_ds)
      tl
  ;;

  let _pstr_list_transitions (ts : constr_transitions H.t) : string list list =
    let keys = List.of_seq (H.to_seq_keys ts) in
    let hd = List.hd keys
    and tl = List.tl keys in
    let hd_ts = H.find ts hd in
    List.fold_left
      (fun (acc : string list list) (f : EConstr.t) ->
        _pstr_list_term_transitions f (H.find ts f) :: acc)
      [ _pstr_list_term_transitions hd hd_ts ]
      tl
  ;;

  let _check_for_duplicate_transitions
    ?(prefix : string = "")
    ?(none : string option)
    (g : lts_graph)
    : unit
    =
    (* Log.override "_check_for_duplicate_transitions, begin."; *)
    if H.length g.transitions > 1
    then (
      let pstr_list_list : string list list =
        _pstr_list_transitions g.transitions
      in
      let dupes : (string, int) Hashtbl.t = Hashtbl.create 0 in
      let without_dupes : string list =
        List.fold_left
          (fun (acc : string list) (ts : string list) ->
            List.fold_left
              (fun (acc' : string list) (t : string) ->
                if List.mem t acc'
                then (
                  (match Hashtbl.find_opt dupes t with
                   | None -> Hashtbl.add dupes t 1
                   | Some i -> Hashtbl.replace dupes t (i + 1));
                  acc')
                else t :: acc')
              acc
              ts)
          []
          pstr_list_list
      in
      let num_dupes = Hashtbl.length dupes in
      let num_nondupes = List.length without_dupes in
      let dupe_id = new_int_counter () in
      if num_dupes > 0
      then
        Log.warning
          ~params:default_params
          (Printf.sprintf
             "%s (%d/%d) duplicate transitions found: [%s\n]"
             prefix
             num_dupes
             num_nondupes
             (Hashtbl.fold
                (fun (s : string) (i : int) (acc : string) ->
                  Printf.sprintf
                    "%s\n- #%d, had %d dupes:\n  %s\n"
                    acc
                    (dupe_id ())
                    i
                    s)
                dupes
                ""))
      else (
        match none with
        | None -> ()
        | Some s ->
          Log.override ~params:default_params (Printf.sprintf "%s%s" prefix s));
      ())
  ;;

  (* Log.override "_check_for_duplicate_transitions, begin." *)

  let _check_states ?(prefix : string = "") (g : lts_graph) : unit =
    Log.override (Printf.sprintf "%s_check_states, begin." prefix);
    H.iter
      (fun (from : EConstr.t) (actions : constr_transitions) ->
        (* check [from] is in [g.states] *)
        if Bool.not (S.mem from g.states)
        then
          Log.warning
            ~params:default_params
            (Printf.sprintf
               "%s_check_states, from state not found: %s"
               prefix
               (econstr_to_string from));
        Hashtbl.iter
          (fun (a : Mebi_action.action) (destinations : D.t) ->
            D.iter
              (fun ((destination, _constr_tree) : D.elt) ->
                if Bool.not (S.mem destination g.states)
                then
                  Log.warning
                    ~params:default_params
                    (Printf.sprintf
                       "%s_check_states, destination state not found: %s"
                       prefix
                       (econstr_to_string destination)))
              destinations)
          actions)
      g.transitions;
    Log.override "_check_states, end."
  ;;

  (**********************************************************************)
  (******** above is a sanity check *************************************)
  (**********************************************************************)

  (** [insert_constr_transition] handles adding the mapping of action [a] to tuple [(term * Constr_tree.t)] in a given [constr_transitions]. *)
  let insert_constr_transition
    (constrs : constr_transitions)
    (a : Mebi_action.action)
    (d : EConstr.t)
    (c : Constr_tree.t)
    : unit mm
    =
    (match Hashtbl.find_opt constrs a with
     | None -> Hashtbl.add constrs a (D.singleton (d, c))
     | Some ds -> Hashtbl.replace constrs a (D.add (d, c) ds));
    return ()
  ;;

  let add_new_term_constr_transition
    (g : lts_graph)
    (t : EConstr.t)
    (a : Mebi_action.action)
    (d : EConstr.t)
    (c : Constr_tree.t)
    : unit mm
    =
    H.add
      g.transitions
      t
      (Hashtbl.of_seq (List.to_seq [ a, D.singleton (d, c) ]));
    return ()
  ;;

  let get_new_states
    ?(params : Params.log = default_params)
    (t : EConstr.t)
    (g : lts_graph)
    (ctors : coq_ctor list)
    : S.t mm
    =
    let iter_body (i : int) (new_states : S.t) =
      let (act, tgt, int_tree) : coq_ctor = List.nth ctors i in
      let* sigma = get_sigma in
      (* TODO: detect tau transitions and then defer to [Fsm.tau] instead. *)
      let is_tau : bool = false in
      let* (label : string) = econstr_to_string_mm act in
      let to_add : Mebi_action.action = { label; is_tau } in
      (match H.find_opt g.transitions t with
       | None ->
         let _ = add_new_term_constr_transition g t to_add tgt int_tree in
         ()
       | Some actions ->
         let _ = insert_constr_transition actions to_add tgt int_tree in
         ());
      (* if [tgt] has not been explored then add [to_visit] *)
      if H.mem g.transitions tgt
         || EConstr.eq_constr sigma tgt t
         || S.mem tgt g.states
      then ()
      else Queue.push tgt g.to_visit;
      (* add [tgt] to [new_states] *)
      return (S.add tgt new_states)
    in
    iterate 0 (List.length ctors - 1) (S.singleton t) iter_body
  ;;

  (** [get_new_constrs t tr_rlts fn_rlts] returns the list of constructors applicable to term [t], using those provided in [tr_rlts] (and [fn_rlts]).
      If no immediate constructor is found matching [t] in [tr_rlts] (likely due to unification problems), then each constructor in [tr_rlts] is tried sequentially, until one of them returns some valid constructors.
      @raise CannotFindTypeOfTermToVisit
        if none of the constructors provided in [tr_rlts] yield constructors from [check_valid_constructors]. *)
  let get_new_constrs
    ?(params : Params.log = default_params)
    (t : EConstr.t)
    (tr_rlts : term_type_map)
    : coq_ctor list mm
    =
    let* ty = Mebi_utils.type_of_econstr t in
    match Hashtbl.find_opt tr_rlts ty with
    | None ->
      (* unknown_term_type (t, ty, List.of_seq (H.to_seq_keys tr_rlts)) *)
      unknown_term_type (t, ty, List.of_seq (Hashtbl.to_seq_keys tr_rlts))
    | Some rlts ->
      (* let temp_tr_rlts = Hashtbl.of_seq (Hashtbl.to_seq tr_rlts) in *)
      check_valid_constructor
        ~params
        rlts.constructor_transitions
        (* temp_tr_rlts *)
        tr_rlts
        (* fn_rlts *)
        t
        None
  ;;

  (** [build_lts_graph fn_rlts g bound] is an [lts_graph] [g] obtained by exploring [fn_rlts].
      @param fn_rlts maps coq-term names to [raw_lts].
      @param g is an [lts_graph] accumulated while exploring [rlts].
      @param bound is the number of states to explore until.
      @return an [lts_graph] with a maximum of [bound] many states. *)
  let rec build_lts_graph
    ?(params : Params.log = default_params)
    (tr_rlts : term_type_map)
    (* (tr_rlts : raw_lts H.t) *)
    (* (fn_rlts : raw_lts H.t) *)
      (g : lts_graph)
    (bound : int)
    : lts_graph mm
    =
    params.kind <- Debug ();
    if Queue.is_empty g.to_visit
    then return g (* finished if no more to visit*)
    else if S.cardinal g.states > bound
    then return g (* exit if bound reached *)
    else (
      let t : EConstr.t = Queue.pop g.to_visit in
      let* (new_constrs : coq_ctor list) = get_new_constrs ~params t tr_rlts in
      (* [get_new_states] also updates [g.to_visit] *)
      let* (new_states : S.t) = get_new_states ~params t g new_constrs in
      let g : lts_graph = { g with states = S.union g.states new_states } in
      build_lts_graph ~params tr_rlts (* fn_rlts *) g bound)
  ;;

  (* let build_rlts_map
     ?(params : Params.log = default_params)
     (grefs : Names.GlobRef.t list)
     : raw_lts H.t mm
     =
     (* : (raw_lts H.t * raw_lts H.t) mm *)
     let num_grefs : int = List.length grefs in
     let trmap : raw_lts H.t =
     H.create num_grefs
     (* and fnmap : raw_lts T.t = T.create (List.length grefs) *)
     (* and map_id : unit -> int = new_int_counter ()  *)
     in
     List.iter
     (fun (gref : Names.GlobRef.t) ->
     (* let rlts : raw_lts = run (check_ref_lts gref) in *)
     let rlts : raw_lts = run (check_ref_lts gref) in
     (* Log.override
     ~params
     (Printf.sprintf "= = = = = = = = =\n\trlts (#%d):" (map_id ())); *)
     let _ = _log_raw_lts ~params rlts in
     (* FIXME: avoid two keys mapping to same rlts *)
     (* - [build_graph] requires [tref -> raw_lts] *)
     (* - [check_updated_ctx] requires [fn -> raw_lts] *)
     H.add trmap rlts.trm_type rlts;
     H.add trmap rlts.coq_lts rlts
     (* H.add fnmap rlts.coq_lts rlts *))
     grefs;
     (* return (trmap, fnmap) *)
     return trmap
     ;; *)

  (** [build_graph tr_rlts fn_rlts tref bound] is the entry point for [build_lts_graph].
      @param tr_rlts maps coq-term types to [raw_lts].
      @param fn_rlts
        is passed to [build_lts_graph] and maps coq-term names to [raw_lts].
      @param tref is the original coq-term.
      @param bound is the number of states to explore until. *)
  let build_graph
    ?(params : Params.log = default_params)
    (tr_rlts : term_type_map)
    (*   (fn_rlts : term_type_map) *)
      (tref : Constrexpr.constr_expr)
    (* (grefs : Names.GlobRef.t list) *)
      (bound : int)
    : lts_graph mm
    =
    (* let* (tr_rlts : raw_lts H.t) = build_rlts_map ~params grefs in *)
    let$ t env sigma = Constrintern.interp_constr_evars env sigma tref in
    (* "show" types expected by constructors to monad *)
    (* let constr_tys = List.of_seq (Hashtbl.to_seq_keys tr_rlts) in
       let iter_body (i : int) () =
       (* let* env = get_env in
       let* sigma = get_sigma in
       let _ = Reductionops.nf_all env sigma (List.nth constr_tys i) in
       return () *)
       let _ = Mebi_utils.type_of_econstr (List.nth constr_tys i) in
       return ()
       in
       let _ = iterate 0 (List.length constr_tys - 1) () iter_body in *)
    (* should be able to get the type now -- fail otherwise *)
    let* ty = Mebi_utils.type_of_econstr t in
    match Hashtbl.find_opt tr_rlts ty with
    | None ->
      unknown_tref_type (t, ty, List.of_seq (Hashtbl.to_seq_keys tr_rlts))
    | Some rlts ->
      (* update environment by typechecking *)
      let$* u env sigma = Typing.check env sigma t rlts.trm_type in
      let$ t env sigma = sigma, Reductionops.nf_all env sigma t in
      let q = Queue.create () in
      let* _ = return (Queue.push t q) in
      let* g =
        build_lts_graph
          ~params
          tr_rlts
          (* fn_rlts *)
          { to_visit = q; states = S.empty; transitions = H.create bound }
          bound
      in
      _check_for_duplicate_transitions
        ~prefix:"build_graph, "
        ~none:"No duplicates found"
        g;
      _check_states ~prefix:"build_graph, " g;
      return g
  ;;

  module DeCoq = struct
    type coq_translation =
      { from_coq : string H.t
      ; to_coq : (string, EConstr.t) Hashtbl.t
      }

    let make_coq_translation : coq_translation =
      { from_coq = H.create 0; to_coq = Hashtbl.create 0 }
    ;;

    (* type coq_translation = coq_translation_record mm *)

    let create_translation_tbl
      ?(params : Params.log = default_params)
      (states : S.t)
      : coq_translation mm
      =
      let list_states : EConstr.t list = S.elements states in
      let iter_body (i : int) (tbl : coq_translation) =
        let s : EConstr.t = List.nth list_states i in
        let* (str : string) = econstr_to_string_mm s in
        (match H.find_opt tbl.from_coq s with
         | None ->
           (* add as new state *)
           (* let str : string = econstr_to_string s in *)
           H.add tbl.from_coq s str;
           Hashtbl.add tbl.to_coq str s
         | Some _str ->
           (* ignore *)
           Log.override
             ~params
             (Printf.sprintf
                "translate_coq_terms, already translated:\ncoq: %s\nstr: %s"
                str
                _str);
           ());
        return tbl
      in
      iterate 0 (List.length list_states - 1) make_coq_translation iter_body
    ;;

    (* let translate_coq_lts
       ?(params : Params.log = default_params)
       (transitions : constr_transitions H.t)
       (tbl : coq_translation)
       : Lts.raw_flat_lts mm
       =
       (* (str_states : string list) *)
       return
       (H.fold
       (fun (from : EConstr.t)
       (actions : constr_transitions)
       (acc : Lts.raw_flat_lts) ->
       Hashtbl.fold
       (fun (action : Mebi_action.action)
       (destinations : D.t)
       (acc' : Lts.raw_flat_lts) ->
       D.fold
       (fun ((destination, constr_tree) : D.elt)
       (acc'' : Lts.raw_flat_lts) ->
       (* TODO: add small test to check are in states *)
       ( (match H.find_opt tbl.from_coq from with
       | None ->
       let (from_str : string) = econstr_to_string from in
       Printf.sprintf "UNKNOWN_FROM_STATE: %s" from_str
       | Some s -> s)
       , action.label
       , (match H.find_opt tbl.from_coq destination with
       | None ->
       let (dest_str : string) =
       econstr_to_string destination
       in
       Printf.sprintf "UNKNOWN_DEST_STATE: %s" dest_str
       | Some s -> s)
       , Some (Constr_tree.pstr constr_tree) )
       :: acc'')
       destinations
       acc')
       actions
       acc)
       transitions
       [])
       ;; *)

    let translate_init_term
      (init_term : Constrexpr.constr_expr)
      (tbl : coq_translation)
      : string mm
      =
      let$ t env sigma = Constrintern.interp_constr_evars env sigma init_term in
      let$ init_term env sigma = sigma, Reductionops.nf_all env sigma t in
      let init_str : string = H.find tbl.from_coq init_term in
      return init_str
    ;;

    let translate_states
      ?(params : Params.log = default_params)
      (states : S.t)
      (tbl : coq_translation)
      : string list mm
      =
      return
        (S.fold
           (fun (state : S.elt) (acc : string list) ->
             (* econstr_to_string state :: acc *)
             (match H.find_opt tbl.from_coq state with
              | None ->
                let (state_str : string) = econstr_to_string state in
                Printf.sprintf "UNKNOWN_COQ_STATE: %s" state_str
              | Some s -> s)
             :: acc)
           states
           [])
    ;;

    let create_transitions_list
      ?(params : Params.log = default_params)
      (transitions : constr_transitions H.t)
      : (S.elt * Mebi_action.action * S.elt * Constr_tree.t) list mm
      =
      let raw_list : (EConstr.t * constr_transitions) list =
        List.of_seq (H.to_seq transitions)
      in
      let from_body
        (i : int)
        (new_transitions :
          (S.elt * Mebi_action.action * S.elt * Constr_tree.t) list)
        =
        let (from, actions) : EConstr.t * constr_transitions =
          List.nth raw_list i
        in
        let raw_actions : (Mebi_action.action * D.t) list =
          List.of_seq (Hashtbl.to_seq actions)
        in
        let action_body
          (j : int)
          (new_transitions :
            (S.elt * Mebi_action.action * S.elt * Constr_tree.t) list)
          =
          let (a, destinations) : Mebi_action.action * D.t =
            List.nth raw_actions j
          in
          let raw_destinations : (EConstr.t * Constr_tree.t) list =
            D.elements destinations
          in
          let destination_body
            (k : int)
            (new_transitions :
              (S.elt * Mebi_action.action * S.elt * Constr_tree.t) list)
            =
            let (destination, constr_tree) : EConstr.t * Constr_tree.t =
              List.nth raw_destinations k
            in
            return ((from, a, destination, constr_tree) :: new_transitions)
          in
          iterate 0 (List.length raw_destinations - 1) [] destination_body
        in
        iterate 0 (List.length raw_actions - 1) [] action_body
      in
      iterate 0 (List.length raw_list - 1) [] from_body
    ;;

    let translate_transitions
      (transitions_list :
        (S.elt * Mebi_action.action * S.elt * Constr_tree.t) list)
      (translation_tbl : coq_translation)
      : Lts.raw_flat_lts mm
      =
      let iter_body (i : int) (acc : Lts.raw_flat_lts) =
        let (from, a, destination, constr_tree)
          : S.elt * Mebi_action.action * S.elt * Constr_tree.t
          =
          List.nth transitions_list i
        in
        let* (_from_str : string) = econstr_to_string_mm from in
        let from_str : string =
          match H.find_opt translation_tbl.from_coq from with
          | None -> Printf.sprintf "UNKNOWN_FROM_STATE: %s" _from_str
          | Some s -> s
        in
        let a_str : string = a.label in
        let* (_dest_str : string) = econstr_to_string_mm destination in
        let dest_str : string =
          match H.find_opt translation_tbl.from_coq destination with
          | None -> Printf.sprintf "UNKNOWN_DEST_STATE: %s" _dest_str
          | Some s -> s
        in
        let constr_tree_str : string = Constr_tree.pstr constr_tree in
        return ((from_str, a_str, dest_str, Some constr_tree_str) :: acc)
      in
      iterate 0 (List.length transitions_list - 1) [] iter_body
    ;;

    let lts_graph_to_lts
      ?(params : Params.log = default_params)
      ?(bound : int = default_bound)
      ?(name : string = "unnamed")
      (g : lts_graph)
      (init_term : Constrexpr.constr_expr)
      : (Lts.lts * coq_translation) mm
      =
      let* (transitions_list :
             (S.elt * Mebi_action.action * S.elt * Constr_tree.t) list)
        =
        create_transitions_list ~params g.transitions
      in
      (* bidirectional mapping between [EConstr.t] and [string] *)
      let* translation_tbl = create_translation_tbl g.states in
      let* (flat_rlts : Lts.raw_flat_lts) =
        translate_transitions transitions_list translation_tbl
      in
      let* (init : string) = translate_init_term init_term translation_tbl in
      let is_complete : bool = Queue.is_empty g.to_visit in
      let num_states : int = S.cardinal g.states in
      let num_edges : int = num_transitions g.transitions in
      let info : Utils.model_info =
        { is_complete; bound; num_states; num_edges }
      in
      let* (string_states : string list) =
        translate_states g.states translation_tbl
      in
      let lts : Lts.lts =
        Lts.Create.lts ~init ~info (Flat (flat_rlts, Some string_states))
      in
      return (lts, translation_tbl)
    ;;

    (** Error when trying to translate an unfinished LTS to FSM. *)
    (* exception UnfinishedLTS of lts_graph *)

    (* let _lts_graph_to_lts
      ?(params : Params.log = default_params)
      ?(bound : int = default_bound)
      ?(name : string = "unnamed")
      (g : lts_graph)
      (init_term : Constrexpr.constr_expr)
      : (Lts.lts * coq_translation) mm
      =
      let* (tbl : coq_translation) = translate_coq_terms g.states in
      _check_states ~prefix:"lts_graph_to_lts, " g;
      let* (string_states : string list) = translate_coq_states g.states tbl in
      let* (init : string) = translate_init_term init_term tbl in
      let* (flat_rlts : Lts.raw_flat_lts) =
        translate_coq_lts g.transitions tbl
      in
      let is_complete : bool = Queue.is_empty g.to_visit in
      let num_states : int = S.cardinal g.states in
      let num_edges : int = num_transitions g.transitions in
      let info : Utils.model_info =
        { is_complete; bound; num_states; num_edges }
      in
      let lts : Lts.lts =
        Lts.Create.lts ~init ~info (Flat (flat_rlts, Some string_states))
      in
      if Bool.not is_complete
      then (
        let to_visit : int = Queue.length g.to_visit in
        Log.warning
          ~params
          (Printf.sprintf
             "LTS (%s) is not complete using bound (%d), still had at least \
              (%d) terms to visit."
             name
             bound
             to_visit);
        (* dump lts to *)
        let dump_filepath : string =
          Dump_to_file.write_to_file
            (Default ())
            (LTS (Printf.sprintf "%s (%d rem %d)" name bound to_visit))
            (JSON ())
            (LTS lts)
        in
        Log.warning
          ~params
          (Printf.sprintf "saved incomplete LTS to: %s\n" dump_filepath))
      else
        Log.normal ~params (Printf.sprintf "Finished translating LTS: %s" name);
      (* Log.override
         (Printf.sprintf "leaving lts_graph_to_lts: %s, %b" name is_complete); *)
      return (lts, tbl)
    ;;
    *)
  end
  (* TODO: from [LTS.lts] and [coq_translation] create a coq term and save to file *)
  (* module ReCoq = struct
     (* let from_lts *)
     end *)
end

(** [make_graph_builder] is ... *)
let make_graph_builder =
  let* h = make_constr_tbl in
  (* hashtabl of terms to (edges) or (raw_lts) *)
  let* s = make_constr_set in
  (* set of states (econstr term) *)
  let* d = make_constr_tree_set in
  (* hashtabl mapping term type or raw_lts *)
  let module G = MkGraph ((val h)) ((val s)) ((val d)) in
  return (module G : GraphB)
;;

let build_rlts_map
  ?(params : Params.log = default_params)
  (grefs : Names.GlobRef.t list)
  : term_type_map mm
  =
  let num_grefs : int = List.length grefs in
  let trmap : term_type_map = Hashtbl.create num_grefs in
  let iter_body (i : int) (acc : term_type_map) =
    let gref : Names.GlobRef.t = List.nth grefs i in
    (* let rlts : raw_lts = run (check_ref_lts gref) in *)
    let* (rlts : raw_lts) = check_ref_lts gref in
    Hashtbl.add acc rlts.trm_type rlts;
    Hashtbl.add acc rlts.coq_lts rlts;
    return acc
  in
  iterate 0 (num_grefs - 1) trmap iter_body
;;

(* List.iter
   (fun (gref : Names.GlobRef.t) ->
   let rlts : raw_lts = run (check_ref_lts gref) in
   Log.override
   ~params
   (Printf.sprintf "= = = = = = = = =\n\trlts (#%d):" (map_id ()));
   let _ = _log_raw_lts ~params rlts in
   (* FIXME: avoid two keys mapping to same rlts *)
   (* - [build_graph] requires [tref -> raw_lts] *)
   (* - [check_updated_ctx] requires [fn -> raw_lts] *)
   Hashtbl.add trmap rlts.trm_type rlts;
   Hashtbl.add fnmap rlts.coq_lts rlts)
   grefs;
   return (trmap, fnmap) *)

(** *)
let build_bounded_lts
  ?(params : Params.log = default_params)
  ?(bound : int = default_bound)
  ?(name : string = "unnamed")
  (tr_rlts : term_type_map) (* (fn_rlts : term_type_map) *)
  (tref : Constrexpr.constr_expr)
  (* (grefs : Names.GlobRef.t list) *)
    (module G : GraphB)
  : Lts.lts mm
  =
  (* graph lts *)
  (* let* graph_lts = G.build_graph ~params tref grefs bound in *)
  let* graph_lts = G.build_graph ~params tr_rlts tref bound in
  (* let* graph_lts = G.build_graph ~params tr_rlts fn_rlts tref bound in *)
  Log.override ~params (Printf.sprintf "- - - - -");
  if G.S.cardinal graph_lts.states > bound
  then
    Log.warning
      ~params
      (Printf.sprintf "LTS graph is incomplete, exceeded bound: %i.\n" bound);
  (* show edges *)
  (* log
     ~params
     (Printf.sprintf
     "(e) Graph Edges: %s.\n"
     (run (G.PStr.transitions ~params:(Log params) graph_lts.transitions))); *)
  (* show if normal output allowed from outside call *)
  (* if params.options.show_debug_output
     then params.kind <- Details ()
     else params.kind <- Normal (); *)
  (* log
     ~params
     (Printf.sprintf
     "Constructed LTS: %s.\n"
     (run (G.PStr.lts ~params:(Log params) graph_lts))); *)
  (* pure lts *)
  let* the_pure_lts, coq_translation =
    G.DeCoq.lts_graph_to_lts ~params ~bound ~name graph_lts tref
  in
  return the_pure_lts
;;

(** *)
let build_fsm_from_bounded_lts
  ?(params : Params.log = default_params)
  ?(bound : int = default_bound)
  ?(name : string = "unnamed")
  (tref : Constrexpr.constr_expr)
  (grefs : Names.GlobRef.t list)
  : Fsm.fsm mm
  =
  (* disable detailed printouts *)
  params.options.show_normal_output <- false;
  (* list of raw coq lts *)
  (* let* (tr_rlts, fn_rlts) : term_type_map * term_type_map = *)
  let* (tr_rlts : term_type_map) = build_rlts_map ~params grefs in
  (* print out the type of the term *)
  Log.override
    ~params
    (Printf.sprintf
       "(A) type of tref: %s"
       (econstr_to_string (run (Mebi_utils.type_of_tref tref))));
  (* graph module *)
  let* graphM = make_graph_builder in
  let module G = (val graphM) in
  (* get pure lts *)
  (* Utils.Logging.Log.override "D"; *)
  let* (the_lts : Lts.lts) =
    (* build_bounded_lts ~params ~bound ~name tref grefs (module G) *)
    build_bounded_lts ~params ~bound ~name tr_rlts tref (module G)
    (* build_bounded_lts ~params ~bound ~name tr_rlts fn_rlts tref (module G) *)
  in
  (* translate to fsm *)
  params.options.show_normal_output <- true;
  return (Translate.to_fsm the_lts)
;;

module Vernac = struct
  module LTS = struct
    let build
      ?(params : Params.log = default_params)
      ?(bound : int = default_bound)
      ?(name : string = "unnamed")
      ?(equiv : Names.GlobRef.t option)
      (tref : Constrexpr.constr_expr)
      (grefs : Names.GlobRef.t list)
      : Lts.lts mm
      =
      (* list of raw coq lts *)
      (* let* (tr_rlts, fn_rlts) : term_type_map * term_type_map = *)
      let* (tr_rlts : term_type_map) = build_rlts_map ~params grefs in
      (* print out the type of the term *)
      Log.override
        ~params
        (Printf.sprintf
           "(B) type of tref: %s"
           (econstr_to_string (run (Mebi_utils.type_of_tref tref))));
      (* graph module *)
      let* graphM = make_graph_builder in
      let module G = (val graphM) in
      (* get pure lts *)
      build_bounded_lts ~params ~bound ~name tr_rlts tref (module G)
    ;;

    (* build_bounded_lts ~params ~bound ~name tref grefs (module G) *)

    (* build_bounded_lts ~params ~bound ~name tr_rlts fn_rlts tref (module G) *)

    let show
      ?(params : Params.log = default_params)
      ?(bound : int = default_bound)
      ?(equiv : Names.GlobRef.t option)
      (tref : Constrexpr.constr_expr)
      (grefs : Names.GlobRef.t list)
      : unit mm
      =
      let* (the_lts : Lts.lts) = build ~params ~bound tref grefs in
      Log.normal
        ~params
        (Printf.sprintf "LTS: %s" (Lts.PStr.lts ~params:(Log params) the_lts));
      return ()
    ;;

    let dump
      ?(params : Params.log = default_params)
      ?(bound : int = default_bound)
      ?(name : string = "dump")
      ?(equiv : Names.GlobRef.t option)
      (tref : Constrexpr.constr_expr)
      (grefs : Names.GlobRef.t list)
      : unit mm
      =
      params.options.output_enabled <- false;
      let* (the_lts : Lts.lts) = build ~params ~bound ~name tref grefs in
      let make_dump : bool =
        match the_lts.info with
        | None -> true
        | Some i -> i.is_complete
      in
      if make_dump
      then (
        let dump_filepath : string =
          Dump_to_file.write_to_file
            (Default ())
            (LTS name)
            (JSON ())
            (LTS the_lts)
        in
        Log.normal
          ~params
          (Printf.sprintf "Dumped LTS into: %s.\n" dump_filepath))
      else
        Log.normal
          ~params
          (Printf.sprintf
             "Incomplete, LTS (%s) should already be dumped.\n"
             name);
      return ()
    ;;
  end

  module FSM = struct
    (** *)
    let build
      ?(params : Params.log = default_params)
      ?(bound : int = default_bound)
      ?(name : string = "unnamed")
      ?(equiv : Names.GlobRef.t option)
      (tref : Constrexpr.constr_expr)
      (grefs : Names.GlobRef.t list)
      : Fsm.fsm mm
      =
      build_fsm_from_bounded_lts ~params ~bound ~name tref grefs
    ;;

    let show
      ?(params : Params.log = default_params)
      ?(bound : int = default_bound)
      ?(equiv : Names.GlobRef.t option)
      (tref : Constrexpr.constr_expr)
      (grefs : Names.GlobRef.t list)
      : unit mm
      =
      (* get translated fsm *)
      let* the_fsm = build ~params ~bound tref grefs in
      (* show if normal output allowed from outside call *)
      if params.options.show_debug_output
      then params.kind <- Details ()
      else params.kind <- Normal ();
      log
        ~params
        (Printf.sprintf "FSM: %s" (Fsm.PStr.fsm ~params:(Log params) the_fsm));
      return ()
    ;;

    let dump
      ?(params : Params.log = default_params)
      ?(bound : int = default_bound)
      ?(name : string = "dump")
      ?(equiv : Names.GlobRef.t option)
      (tref : Constrexpr.constr_expr)
      (grefs : Names.GlobRef.t list)
      : unit mm
      =
      (* get translated fsm *)
      let* the_fsm = build ~params ~bound ~name tref grefs in
      let dump_filepath : string =
        Dump_to_file.write_to_file
          (Default ())
          (FSM name)
          (JSON ())
          (FSM the_fsm)
      in
      log ~params (Printf.sprintf "Dumped FSM into: %s.\n" dump_filepath);
      return ()
    ;;
  end

  module Minim = struct
    (** *)
    let build
      ?(params : Params.log = default_params)
      ?(bound : int = default_bound)
      ?(name : string = "unnamed")
      (tref : Constrexpr.constr_expr)
      (grefs : Names.GlobRef.t list)
      : (Fsm.fsm * Fsm.fsm) mm
      =
      (* get translated fsm *)
      let* the_fsm = FSM.build ~params ~bound ~name tref grefs in
      let (minimized_fsm, _map_of_states) : fsm * (state, state) Hashtbl.t =
        Minimize.run ~params the_fsm
      in
      return (the_fsm, minimized_fsm)
    ;;

    (** *)
    let show
      ?(params : Params.log = default_params)
      ?(bound : int = default_bound)
      (tref : Constrexpr.constr_expr)
      (grefs : Names.GlobRef.t list)
      : unit mm
      =
      (* get translated fsm *)
      let* the_fsm, minimized_fsm = build ~params ~bound tref grefs in
      (* show if normal output allowed from outside call *)
      if params.options.show_debug_output
      then params.kind <- Details ()
      else params.kind <- Normal ();
      log
        ~params
        (Printf.sprintf
           "Minimized: %s\n\nfrom: %s."
           (Fsm.PStr.fsm ~params:(Log params) minimized_fsm)
           (Fsm.PStr.fsm ~params:(Log params) the_fsm));
      return ()
    ;;

    let dump
      ?(params : Params.log = default_params)
      ?(bound : int = default_bound)
      ?(name : string = "dump")
      (tref : Constrexpr.constr_expr)
      (grefs : Names.GlobRef.t list)
      : unit mm
      =
      (* get translated fsm *)
      let* the_fsm, minimized_fsm = build ~params ~bound ~name tref grefs in
      let fsm_dump_filepath : string =
        Dump_to_file.write_to_file
          (Default ())
          (FSM (Printf.sprintf "%s tobe minim" name))
          (JSON ())
          (FSM the_fsm)
      and minim_dump_filepath : string =
        Dump_to_file.write_to_file
          (Default ())
          (FSM (Printf.sprintf "%s post minim" name))
          (JSON ())
          (FSM minimized_fsm)
      in
      log
        ~params
        (Printf.sprintf
           "Dumped FSMs into: %s\nand %s.\n"
           fsm_dump_filepath
           minim_dump_filepath);
      return ()
    ;;
  end

  module Merged = struct
    (** *)
    let build
      ?(params : Params.log = default_params)
      ?(bound : int = default_bound)
      ?(name : string = "unnamed")
      (trefA : Constrexpr.constr_expr)
      (trefB : Constrexpr.constr_expr)
      (grefs : Names.GlobRef.t list)
      : (Fsm.fsm * Fsm.fsm * Fsm.fsm) mm
      =
      (* get translated fsm *)
      let* fsm_A =
        FSM.build ~params ~bound ~name:(Printf.sprintf "%s A" name) trefA grefs
      in
      let* fsm_B =
        FSM.build ~params ~bound ~name:(Printf.sprintf "%s B" name) trefB grefs
      in
      (* merge fsm *)
      let (merged_fsm, _translation_table) : Fsm.fsm * (state, state) Hashtbl.t =
        Fsm.Merge.fsms ~params fsm_A fsm_B
      in
      return (merged_fsm, fsm_A, fsm_B)
    ;;

    (** *)
    let show
      ?(params : Params.log = default_params)
      ?(bound : int = default_bound)
      (trefA : Constrexpr.constr_expr)
      (trefB : Constrexpr.constr_expr)
      (grefs : Names.GlobRef.t list)
      : unit mm
      =
      (* get merged fsm *)
      let* merged_fsm, fsm_A, fsm_B = build ~params ~bound trefA trefB grefs in
      (* show if normal output allowed from outside call *)
      if params.options.show_debug_output
      then params.kind <- Details ()
      else params.kind <- Normal ();
      log
        ~params
        (Printf.sprintf
           "Merged FSM A with B :: %s.\n\nwhere A: %s,\n\nand B: %s.\n"
           (Fsm.PStr.fsm ~params:(Log params) merged_fsm)
           (Fsm.PStr.fsm ~params:(Log params) fsm_A)
           (Fsm.PStr.fsm ~params:(Log params) fsm_B));
      return ()
    ;;

    let dump
      ?(params : Params.log = default_params)
      ?(bound : int = default_bound)
      ?(name : string = "dump")
      (trefA : Constrexpr.constr_expr)
      (trefB : Constrexpr.constr_expr)
      (grefs : Names.GlobRef.t list)
      : unit mm
      =
      (* get merged fsm *)
      let* merged_fsm, fsm_A, fsm_B =
        build ~params ~bound ~name trefA trefB grefs
      in
      let merged_dump_filepath : string =
        Dump_to_file.write_to_file
          (Default ())
          (FSM (Printf.sprintf "%s merged fsm" name))
          (JSON ())
          (FSM merged_fsm)
      and fsm_a_dump_filepath : string =
        Dump_to_file.write_to_file
          (Default ())
          (FSM (Printf.sprintf "%s to merge A" name))
          (JSON ())
          (FSM fsm_A)
      and fsm_b_dump_filepath : string =
        Dump_to_file.write_to_file
          (Default ())
          (FSM (Printf.sprintf "%s to merge B" name))
          (JSON ())
          (FSM fsm_B)
      in
      log
        ~params
        (Printf.sprintf
           "Dumped FSMs into: %s\nand %s\nand %s.\n"
           merged_dump_filepath
           fsm_a_dump_filepath
           fsm_b_dump_filepath);
      return ()
    ;;
  end

  module Bisim = struct
    open Bisimilarity

    exception UnexpectedResultKind of Bisimilarity.result

    (** *)
    let build
      ?(params : Params.log = default_params)
      ?(bound : int = default_bound)
      ?(name : string = "unnamed")
      (trefA : Constrexpr.constr_expr)
      (trefB : Constrexpr.constr_expr)
      (grefs : Names.GlobRef.t list)
      : (Fsm.fsm * Fsm.fsm * Bisimilarity.result) mm
      =
      (* get translated fsm *)
      let* fsm_A =
        FSM.build ~params ~bound ~name:(Printf.sprintf "%s A" name) trefA grefs
      in
      let* fsm_B =
        FSM.build ~params ~bound ~name:(Printf.sprintf "%s B" name) trefB grefs
      in
      (* run bisimilarity algorithm *)
      let raw_result = RCP.KS90.run ~params (ToMerge (fsm_A, fsm_B)) () in
      let result = RCP.KS90.result ~params raw_result in
      return (fsm_A, fsm_B, result)
    ;;

    (** *)
    let show
      ?(params : Params.log = default_params)
      ?(bound : int = default_bound)
      (trefA : Constrexpr.constr_expr)
      (trefB : Constrexpr.constr_expr)
      (grefs : Names.GlobRef.t list)
      : unit mm
      =
      let* fsm_A, fsm_B, result = build ~params ~bound trefA trefB grefs in
      match result with
      | BisimResult result ->
        (* show if normal output allowed from outside call *)
        if params.options.show_debug_output
        then params.kind <- Details ()
        else params.kind <- Normal ();
        log
          ~params
          (Bisimilarity.PStr.bisim_result
             ~params
             ~merged_from:(fsm_A, fsm_B)
             result);
        return ()
      | _ -> raise (UnexpectedResultKind result)
    ;;

    let dump
      ?(params : Params.log = default_params)
      ?(bound : int = default_bound)
      ?(name : string = "dump")
      (trefA : Constrexpr.constr_expr)
      (trefB : Constrexpr.constr_expr)
      (grefs : Names.GlobRef.t list)
      : unit mm
      =
      let* fsm_A, fsm_B, result =
        build ~params ~bound ~name trefA trefB grefs
      in
      match result with
      | BisimResult result ->
        let merged_dump_filepath : string =
          Dump_to_file.write_to_file
            (Default ())
            (FSM (Printf.sprintf "%s bisim merged fsm" name))
            (JSON ())
            (FSM result.merged_fsm)
        and fsm_a_dump_filepath : string =
          Dump_to_file.write_to_file
            (Default ())
            (FSM (Printf.sprintf "%s bisim to merge A" name))
            (JSON ())
            (FSM fsm_A)
        and fsm_b_dump_filepath : string =
          Dump_to_file.write_to_file
            (Default ())
            (FSM (Printf.sprintf "%s bisim to merge B" name))
            (JSON ())
            (FSM fsm_B)
        in
        log
          ~params
          (Printf.sprintf
             "Dumped FSMs into: %s\nand %s\nand %s.\n"
             merged_dump_filepath
             fsm_a_dump_filepath
             fsm_b_dump_filepath);
        return ()
      | _ -> raise (UnexpectedResultKind result)
    ;;
  end
end

(* (\* TODO: check which are all possible next transitions *\) *)
(* (\* TODO: check following functions/modules: *\) *)
(* (\* [ ] Unification *\) *)
(* (\* [ ] Reductionops.infer_conv *\) *)
(* (\*  *\) *)

(** Builds an LTS from a Term [t : T] and an LTS [P : forall Ts, T -> A -> T -> Prop]

    Constraints:
    - [ T \& A \not\in Ts ]

    Notes:
    - Constructors of [P] are the transitions
    - States are the sets of possible transitions
    - A term [t] is represented by the state of the transitions that can be taken *)
