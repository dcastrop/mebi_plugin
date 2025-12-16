open Mebi_wrapper
open Mebi_wrapper.Syntax
open Model

(** [GraphB] is ...
    (Essentially acts as a `.mli` for the [MkGraph] module.) *)
module type GraphB = sig
  module H : Hashtbl.S with type key = Enc.t
  module S : Set.S with type elt = Enc.t
  module D : Set.S with type elt = Enc.t * Tree.t

  type constr_transitions = (Action.t, D.t) Hashtbl.t

  type lts_graph =
    { to_visit : Enc.t Queue.t
    ; init : Enc.t
    ; terminals : S.t
    ; states : S.t
    ; transitions : constr_transitions H.t
      (* ; ind_defs : (Enc.t * Mebi_ind.t) list *)
    ; ind_defs : Mebi_ind.t B.t
    ; weak : Params.WeakEnc.t option
    }

  val insert_constr_transition
    :  constr_transitions
    -> Action.t
    -> Enc.t
    -> Tree.t
    -> unit

  val add_new_term_constr_transition
    :  lts_graph
    -> Enc.t
    -> Action.t
    -> Enc.t
    -> Tree.t
    -> unit

  val build_lts_graph
    :  Mebi_ind.t
    -> Mebi_ind.t B.t
    -> lts_graph
    -> int * Params.WeakEnc.t option
    -> lts_graph mm

  val build_graph
    :  Libnames.qualid
    -> Constrexpr.constr_expr
    -> int * Params.WeakEnc.t option
    -> Names.GlobRef.t list
    -> lts_graph mm

  (* val to_lts
     :  ?cache:bool
     -> lts_graph
     -> int * Params.WeakEnc.t option
     -> Model.Lts.t mm *)

  val decoq_lts
    :  ?cache_decoding:bool
    -> lts_graph (* -> cindef * cindef B.t *)
    -> int * Params.WeakEnc.t option
    -> Lts.t mm
end

(** [MkGraph M] is ...
    [M] is a ... *)
module MkGraph
    (M : Hashtbl.S with type key = Enc.t)
    (N : Set.S with type elt = Enc.t)
    (P : Set.S with type elt = Enc.t * Tree.t) : GraphB = struct
  (***********************************************************************)

  (** custom logging for graph builder *)
  module GraphLog : Logger.LOGGER_TYPE =
    Logger.Make
      (Logger.Output.Rocq)
      (struct
        let prefix : string option = None

        let is_level_enabled : Logger.level -> bool =
          Logger.make_level_fun ~debug:false ()
        ;;
      end)

  let debug_enc ?(__FUNCTION__ : string = "") (p : string) (x : Enc.t) : unit =
    GraphLog.thing ~__FUNCTION__ Debug p x (Of Enc.to_string)
  ;;

  let debug_econstr ?(__FUNCTION__ : string = "") (p : string) (x : EConstr.t)
    : unit
    =
    GraphLog.thing ~__FUNCTION__ Debug p x (Of econstr_to_string)
  ;;

  let debug_dec
        ?(__FUNCTION__ : string = "")
        (p : string)
        (x : Enc.t)
        (y : EConstr.t)
    : unit
    =
    let f : string -> string = Printf.sprintf "%s %s" p in
    debug_enc ~__FUNCTION__ (f "enc") x;
    debug_econstr ~__FUNCTION__ (f "term") y
  ;;

  (***********************************************************************)

  (** [H] is the hashtbl of outgoing transitions, from some [EConstr.t] and also
      is used for mapping term types to [cindef]. *)
  module H : Hashtbl.S with type key = Enc.t = M

  (** [S] is the set of states, of [EConstr.t]. *)
  module S : Set.S with type elt = Enc.t = N

  (** [D] is the set of destination tuples, each comprised of a term [EConstr.t]
      and the corresponding [Tree.t]. *)
  module D : Set.S with type elt = Enc.t * Tree.t = P

  (** [constr_transitions] is a hashtbl mapping [action]s to terms of [EConstr.t] and [Tree.t].
  *)
  type constr_transitions = (Action.t, D.t) Hashtbl.t

  let constr_transitions_to_string
        ?(args : style_args = Utils.Strfy.style_args ())
        (xs : constr_transitions)
    : string
    =
    GraphLog.trace __FUNCTION__;
    let open Utils.Strfy in
    list
      ~args:{ args with name = Some "constr_transitions" }
      (tuple
         Action.to_string
         (fun
             ?(args : style_args = record_args ()) (destination_tuples : D.t) ->
         list
           (tuple
              (fun ?(args : style_args = record_args ()) x -> Enc.to_string x)
              Tree.to_string)
           (D.to_list destination_tuples)))
      (List.combine
         (Hashtbl.to_seq_keys xs |> List.of_seq)
         (Hashtbl.to_seq_values xs |> List.of_seq))
  ;;

  let _transitions_to_string
        ?(args : style_args = Utils.Strfy.style_args ())
        (xs : constr_transitions M.t)
    : string
    =
    GraphLog.trace __FUNCTION__;
    let open Utils.Strfy in
    list
      ~args:{ args with name = Some "transitions" }
      (tuple
         (fun ?(args : style_args = record_args ()) x -> Enc.to_string x)
         constr_transitions_to_string)
      (List.combine
         (M.to_seq_keys xs |> List.of_seq)
         (M.to_seq_values xs |> List.of_seq))
  ;;

  (** [lts_graph] is a record containing a queue of [EConstr.t]s [to_visit], a set of states visited (i.e., [EConstr.t]s), and a hashtbl mapping [EConstr.t] to a map of [constr_transitions], which maps [action]s to [EConstr.t]s and their [Tree.t].
  *)
  type lts_graph =
    { to_visit : Enc.t Queue.t
    ; init : Enc.t
    ; terminals : S.t
    ; states : S.t
    ; transitions : constr_transitions H.t
    ; ind_defs : Mebi_ind.t B.t
    ; weak : Params.WeakEnc.t option
    }

  (** [insert_constr_transition] handles adding the mapping of action [a] to tuple [(term * Tree.t)] in a given [constr_transitions].
  *)
  let insert_constr_transition
        (constrs : constr_transitions)
        (a : Action.t)
        (d : Enc.t)
        (c : Tree.t)
    : unit
    =
    GraphLog.trace __FUNCTION__;
    match Hashtbl.find_opt constrs a with
    | None -> Hashtbl.add constrs a (D.singleton (d, c))
    | Some ds -> Hashtbl.replace constrs a (D.add (d, c) ds)
  ;;

  let add_new_term_constr_transition
        (g : lts_graph)
        (t : Enc.t)
        (a : Action.t)
        (d : Enc.t)
        (c : Tree.t)
    : unit
    =
    GraphLog.trace __FUNCTION__;
    H.add
      g.transitions
      t
      (Hashtbl.of_seq (List.to_seq [ a, D.singleton (d, c) ]))
  ;;

  let handle_updating_transitions
        (g : lts_graph)
        (from : Enc.t)
        (action : Action.t)
        (goto : Enc.t)
        (constructors : Tree.t)
    : unit
    =
    GraphLog.trace __FUNCTION__;
    match H.find_opt g.transitions from with
    | None -> add_new_term_constr_transition g from action goto constructors
    | Some actions -> insert_constr_transition actions action goto constructors
  ;;

  let is_silent_transition (weak : Params.WeakEnc.t option) (act : EConstr.t)
    : bool option mm
    =
    GraphLog.trace __FUNCTION__;
    match weak with
    | None -> return None
    | Some weak_kind ->
      let* act_enc : Enc.t = encode act in
      let* ty : EConstr.t = Mebi_utils.type_of_econstr act in
      let* ty_enc : Enc.t = encode ty in
      debug_econstr ~__FUNCTION__ "ty act" ty;
      let open Params.WeakEnc in
      (match weak_kind with
       | OptionConstr label_enc ->
         GraphLog.trace ~__FUNCTION__ "OptionConstr (label_enc)";
         let* label_decoding : EConstr.t = decode label_enc in
         debug_dec ~__FUNCTION__ "label" label_enc label_decoding;
         (* NOTE: all non-silent actions should be of this type *)
         let* b = Mebi_utils.is_none_term act in
         return (Some b)
       | CustomConstr (tau_enc, label_enc) ->
         GraphLog.trace ~__FUNCTION__ "CustomConstr (tau_enc, label_enc)";
         let* tau_decoding : EConstr.t = decode tau_enc in
         debug_dec ~__FUNCTION__ "tau" tau_enc tau_decoding;
         let* label_decoding : EConstr.t = decode label_enc in
         debug_dec ~__FUNCTION__ "label" label_enc label_decoding;
         return (Some (Enc.equal tau_enc act_enc)))
  ;;

  let get_new_states
        ?(weak_type : Params.WeakEnc.t option = None)
        (from : Enc.t)
        (g : lts_graph)
        (ctors : Mebi_constr.t list)
    : S.t mm
    =
    GraphLog.trace __FUNCTION__;
    GraphLog.thing ~__FUNCTION__ Debug "from" from (Of Enc.to_string);
    let iter_body (i : int) (new_states : S.t) =
      let (act, tgt, int_tree) : Mebi_constr.t = List.nth ctors i in
      let* tgt_enc : Enc.t = encode tgt in
      let* act_enc : Enc.t = encode act in
      let* is_silent : bool option = is_silent_transition weak_type act in
      let label : Label.t = { enc = act_enc; is_silent; pp = None } in
      let constructor_trees : Tree.t list = [ int_tree ] in
      let to_add : Action.t = Action.create label ~constructor_trees () in
      let _ = handle_updating_transitions g from to_add tgt_enc int_tree in
      (* NOTE: if [tgt] has not been explored then add [to_visit] *)
      if H.mem g.transitions tgt_enc || S.mem tgt_enc g.states
      then ()
      else Queue.push tgt_enc g.to_visit;
      (* NOTE: add [tgt] to [new_states] *)
      return (S.add tgt_enc new_states)
    in
    iterate 0 (List.length ctors - 1) (S.singleton from) iter_body
  ;;

  let get_new_constrs
        (from : Enc.t)
        (primary : Mebi_ind.t)
        (lts_ind_def_map : Mebi_ind.t B.t)
    : Mebi_constr.t list mm
    =
    GraphLog.trace __FUNCTION__;
    let* from_term : EConstr.t = decode from in
    let* label_type : EConstr.t = Mebi_ind.get_lts_label_type primary in
    let* ind_map : Mebi_ind.t F.t = decode_map lts_ind_def_map in
    let* primary_constr_transitions = Mebi_ind.get_constr_transitions primary in
    let lts_enc : Enc.t = primary.enc in
    Mebi_unify.collect_valid_constructors
      primary_constr_transitions
      ind_map
      from_term
      label_type
      lts_enc
  ;;

  (** [build_lts_graph fn_rlts g bound] is an [lts_graph] [g] obtained by exploring [fn_rlts].
      @param fn_rlts maps coq-term names to [cindef].
      @param g is an [lts_graph] accumulated while exploring [rlts].
      @param bound is the number of states to explore until.
      @return an [lts_graph] with a maximum of [bound] many states. *)
  let rec build_lts_graph
            (the_primary_lts : Mebi_ind.t)
            (lts_ind_def_map : Mebi_ind.t B.t)
            (g : lts_graph)
            ((bound, weak_type) : int * Params.WeakEnc.t option)
    : lts_graph mm
    =
    GraphLog.trace __FUNCTION__;
    if Queue.is_empty g.to_visit
    then return g (* NOTE: finished if no more to visit*)
    else if S.cardinal g.states > bound
    then return g (* NOTE: exit if bound reached *)
    else (
      let encoded_t : Enc.t = Queue.pop g.to_visit in
      let* new_constrs : Mebi_constr.t list =
        get_new_constrs encoded_t the_primary_lts lts_ind_def_map
      in
      let* new_states : S.t =
        (* NOTE: also updates [g.to_visit ]*)
        get_new_states ~weak_type encoded_t g new_constrs
      in
      let g : lts_graph = { g with states = S.union g.states new_states } in
      build_lts_graph the_primary_lts lts_ind_def_map g (bound, weak_type))
  ;;

  (** @return
        the key for the primary lts and hashtable mapping the name of the coq definition to the rlts.
  *)
  let build_lts_ind_def_map (grefs : Names.GlobRef.t list)
    : (Enc.t * Mebi_ind.t B.t) mm
    =
    GraphLog.trace __FUNCTION__;
    let num_grefs : int = List.length grefs in
    let iter_body
          (i : int)
          ((_primary_lts_enc, acc_map) : Enc.t * Mebi_ind.t B.t)
      =
      let gref : Names.GlobRef.t = List.nth grefs i in
      let* lts_name : EConstr.t = Mebi_utils.get_name_of_lts gref in
      (* add name of inductive prop *)
      let* lts_enc : Enc.t = encode lts_name in
      let* lts_ind_def : Mebi_ind.t = Mebi_utils.get_ind_lts lts_enc gref in
      if Bool.not (B.mem acc_map lts_enc) then B.add acc_map lts_enc lts_ind_def;
      return (lts_enc, acc_map)
    in
    iterate 0 (num_grefs - 1) (Enc.init, B.create num_grefs) iter_body
  ;;

  let get_primary_lts
        (primary_lts : Libnames.qualid)
        (primary_lts_enc : Enc.t)
        (lts_ind_def_map : Mebi_ind.t B.t)
    : Mebi_ind.t mm
    =
    let open Mebi_utils in
    (* encode the primary lts *)
    let* primary_lts_ind_def : Mebi_ind.t =
      get_ind_lts primary_lts_enc (ref_to_glob primary_lts)
    in
    let* the_primary_enc : Enc.t = encode primary_lts_ind_def.info.name in
    return (B.find lts_ind_def_map the_primary_enc)
  ;;

  (** [build_graph lts_ind_def_map fn_rlts tref bound] is the entry point for [build_lts_graph].
      @param lts_ind_def_map maps coq-term types to [Mebi_ind.t].
      @param tref is the original coq-term.
      @param bound is the number of states to explore until. *)
  let build_graph
        (primary_lts : Libnames.qualid)
        (tref : Constrexpr.constr_expr)
        ((bound, weak_type) : int * Params.WeakEnc.t option)
        (grefs : Names.GlobRef.t list)
    : lts_graph mm
    =
    GraphLog.trace __FUNCTION__;
    (* NOTE: normalize the initial term *)
    let* t : EConstr.t = Mebi_utils.constrexpr_to_econstr tref in
    let* t : EConstr.t = Mebi_utils.econstr_normalize t in
    (* NOTE: encode lts inductive definitions *)
    let* (the_primary_lts_enc, lts_ind_def_map) : Enc.t * Mebi_ind.t B.t =
      build_lts_ind_def_map grefs
    in
    let* the_primary_lts =
      get_primary_lts primary_lts the_primary_lts_enc lts_ind_def_map
    in
    (* NOTE: update environment by typechecking *)
    let* primary_trm_type = Mebi_ind.get_lts_trm_type the_primary_lts in
    let$* _unit env sigma = Typing.check env sigma t primary_trm_type in
    let$ initial_trm env sigma = sigma, Reductionops.nf_all env sigma t in
    let* initial_enc = encode initial_trm in
    let to_visit : S.elt Queue.t = Queue.create () in
    let* _ = return (Queue.push initial_enc to_visit) in
    let* g : lts_graph =
      build_lts_graph
        the_primary_lts
        lts_ind_def_map
        { to_visit
        ; init = initial_enc
        ; terminals = S.empty
        ; states = S.empty
        ; transitions = H.create 0
        ; ind_defs = lts_ind_def_map
        ; weak = weak_type
        }
        (bound, weak_type)
    in
    (* NOTE: terminals are states with no out-going transitions *)
    let terminals : S.t =
      S.filter (fun (s : S.elt) -> Bool.not (H.mem g.transitions s)) g.states
    in
    return { g with terminals }
  ;;

  (***********************************************************************)

  (* let to_lts
        ?(cache : bool = false)
        ({ to_visit; init; terminals; states; transitions; ind_defs; weak } :
          lts_graph)
        ((bound, weak_type) : int * Params.WeakEnc.t option)
    : Model.Lts.t mm
    =
    GraphLog.trace __FUNCTION__;
    let enc_pp (x : Enc.t) : string option mm =
      if cache
      then
        let* dec = decode x in
        return (Some (Utils.clean_string (econstr_to_string dec)))
      else return None
    in
    let get_state (enc : Enc.t) : State.t mm =
      let* pp : string option = enc_pp enc in
      let x : State.t = { enc; pp } in
      return x
    in
    let get_states (encs : S.t) : States.t mm =
      let* pp : string option = enc_pp enc in
      let x : State.t = { enc; pp } in
      return x
    in
    return () *)

  (***********************************************************************)

  let decoq_enc ?(cache_decoding : bool = false) (enc : Enc.t)
    : string option mm
    =
    GraphLog.trace __FUNCTION__;
    if cache_decoding
    then
      let* s_decoding = decode enc in
      return (Some (Utils.clean_string (econstr_to_string s_decoding)))
    else return None
  ;;

  let decoq_state ?(cache_decoding : bool = false) (enc : Enc.t) : State.t mm =
    let* pp = decoq_enc ~cache_decoding enc in
    let s : State.t = { enc; pp } in
    return s
  ;;

  let decoq_state_opt ?(cache_decoding : bool = false) (s_enc : Enc.t)
    : State.t option mm
    =
    let* state = decoq_state ~cache_decoding s_enc in
    return (Some state)
  ;;

  let decoq_states ?(cache_decoding : bool = false) (ss : S.t) : States.t mm =
    if S.is_empty ss
    then return States.empty
    else (
      let raw_ss = S.to_list ss in
      let iter_body (i : int) (acc : States.t) : States.t mm =
        let state_enc = List.nth raw_ss i in
        let* state = decoq_state ~cache_decoding state_enc in
        return (States.add state acc)
      in
      iterate 0 (List.length raw_ss - 1) States.empty iter_body)
  ;;

  let decoq_action ?(cache_decoding : bool = false) (a : Action.t) : Action.t mm
    =
    let* pp = decoq_enc ~cache_decoding a.label.enc in
    let label : Label.t = { a.label with pp } in
    return { a with label }
  ;;

  let decoq_destinations
        ?(cache_decoding : bool = false)
        (acc_trans : Transitions.t)
        (from : State.t)
        ({ label; constructor_trees; _ } : Action.t)
        (dests : D.t)
    : Transitions.t mm
    =
    if D.is_empty dests
    then (
      Logging.Log.debug
        (Printf.sprintf
           "decoq_destinations, empty from: %s"
           (State.to_string from));
      return Transitions.empty)
    else (
      let raw_dests = D.to_list dests in
      let iter_dests (i : int) (acc_trans : Transitions.t) : Transitions.t mm =
        let dest, constr_tree = List.nth raw_dests i in
        let* goto : State.t = decoq_state ~cache_decoding dest in
        let new_trans : Transition.t =
          Transition.create from label goto ~constructor_trees ()
        in
        let acc_trans : Transitions.t = Transitions.add new_trans acc_trans in
        return acc_trans
      in
      iterate 0 (List.length raw_dests - 1) acc_trans iter_dests)
  ;;

  let decoq_actions
        ?(cache_decoding : bool = false)
        (acc : Alphabet.t * Transitions.t)
        (from : State.t)
        (actions : constr_transitions)
    : (Alphabet.t * Transitions.t) mm
    =
    if Int.equal 0 (Hashtbl.length actions)
    then (
      Logging.Log.debug
        (Printf.sprintf "decoq_actions, empty from: %s" (State.to_string from));
      return (Alphabet.empty, Transitions.empty))
    else (
      let raw_actions = List.of_seq (Hashtbl.to_seq actions) in
      let iter_actions
            (i : int)
            ((acc_alpha, acc_trans) : Alphabet.t * Transitions.t)
        : (Alphabet.t * Transitions.t) mm
        =
        let action, dests = List.nth raw_actions i in
        let* action : Action.t = decoq_action ~cache_decoding action in
        let acc_alpha : Alphabet.t = Alphabet.add action.label acc_alpha in
        let* acc_trans : Transitions.t =
          decoq_destinations ~cache_decoding acc_trans from action dests
        in
        return (acc_alpha, acc_trans)
      in
      iterate 0 (List.length raw_actions - 1) acc iter_actions)
  ;;

  let decoq_transitions
        ?(cache_decoding : bool = false)
        (transitions : constr_transitions H.t)
    : (Alphabet.t * Transitions.t) mm
    =
    if Int.equal 0 (H.length transitions)
    then return (Alphabet.empty, Transitions.empty)
    else (
      let raw_transitions = H.to_seq transitions |> List.of_seq in
      let iter_from (i : int) (acc : Alphabet.t * Transitions.t)
        : (Alphabet.t * Transitions.t) mm
        =
        let from, actions = List.nth raw_transitions i in
        let* from : State.t = decoq_state ~cache_decoding from in
        decoq_actions ~cache_decoding acc from actions
      in
      iterate
        0
        (List.length raw_transitions - 1)
        (Alphabet.empty, Transitions.empty)
        iter_from)
  ;;

  let decoq_lts_ind_def_map (lts_ind_def_map : Mebi_ind.t B.t)
    : Info.rocq_info list
    =
    let x = ref 0 in
    let xpp () =
      let y = !x in
      x := y + 1;
      y
    in
    B.fold
      (fun (lts_enc : Enc.t)
        (the_ind_def : Mebi_ind.t)
        (acc : Info.rocq_info list) ->
        match the_ind_def.kind with
        | LTS the_lts_ind_def ->
          let lts_name = econstr_to_string the_ind_def.info.name in
          let lts_name = Printf.sprintf "%s_%i" lts_name (xpp ()) in
          let lts_constrs =
            Array.fold_left
              (fun (acc : string list) (name : Names.Id.t) ->
                Names.Id.to_string name :: acc)
              []
              the_ind_def.info.constr_names
          in
          { enc = lts_enc; pp = lts_name; constructor_names = lts_constrs }
          :: acc
        | _ -> acc)
      lts_ind_def_map
      []
  ;;

  let decoq_weak_enc (w : Params.WeakEnc.t option) : string list mm =
    return [ "TODO: decoq_weak_enc" ]
  ;;

  let decoq_lts
        ?(cache_decoding : bool = false)
        (g : lts_graph)
        ((bound, weak_type) : int * Params.WeakEnc.t option)
    : Lts.t mm
    =
    GraphLog.debug __FUNCTION__;
    let* init : State.t option = decoq_state_opt ~cache_decoding g.init in
    let* states : States.t = decoq_states ~cache_decoding g.states in
    let* terminals = decoq_states ~cache_decoding g.terminals in
    let* ((alphabet, transitions) : Alphabet.t * Transitions.t) =
      decoq_transitions ~cache_decoding g.transitions
    in
    let ind_defs : Info.rocq_info list = decoq_lts_ind_def_map g.ind_defs in
    let* w : string list = decoq_weak_enc g.weak in
    let info : Info.t =
      { mebi_info =
          Some
            [ { is_complete = Queue.is_empty g.to_visit
              ; is_merged = false
              ; bound
              }
            ]
      ; rocq_info = Some ind_defs
      ; weak_info = Some w
      }
    in
    return
      (Lts.of_model
         (LTS (init, terminals, alphabet, states, transitions, info)))
  ;;
end

(***********************************************************************)

module Log : Logger.LOGGER_TYPE =
  Logger.Make
    (Logger.Output.Rocq)
    (struct
      let prefix : string option = None

      let is_level_enabled : Logger.level -> bool =
        Logger.make_level_fun ~debug:false ()
      ;;
    end)

(** [make_graph_builder] is ... *)
let make_graph_builder =
  Log.trace __FUNCTION__;
  (* NOTE: hashtabl of terms to (edges) or (cindef) *)
  let* h = make_transition_tbl in
  (* NOTE: set of states (econstr term) *)
  let* s = make_state_set in
  (* NOTE: hashtabl mapping term type or cindef *)
  let* d = make_state_tree_pair_set in
  let module G : GraphB = MkGraph ((val h)) ((val s)) ((val d)) in
  return (module G : GraphB)
;;

(** expect exactly one [Info.mebi_info] and it should be complete *)
let is_lts_complete ({ info; _ } : Lts.t) : bool =
  Option.cata
    (fun (xs : Info.mebi_info list) ->
      match xs with
      | [] -> false
      | { is_complete; _ } :: [] -> is_complete
      | _ :: _ -> false)
    false
    info.mebi_info
;;

(** *)
let fail_if_incomplete (x : Lts.t) : unit mm =
  Log.trace __FUNCTION__;
  if !Params.the_fail_if_incomplete && Bool.not (is_lts_complete x)
  then params_fail_if_incomplete ()
  else return ()
;;

(** *)
let build_lts
      (primary_lts : Libnames.qualid)
      (t : Constrexpr.constr_expr)
      (refs : Libnames.qualid list)
      (params : int * Params.WeakEnc.t option)
  : Lts.t mm
  =
  Log.trace __FUNCTION__;
  let* graphM : (module GraphB) = make_graph_builder in
  let module G = (val graphM) in
  let* graph_lts =
    Mebi_utils.ref_list_to_glob_list (primary_lts :: refs)
    |> G.build_graph primary_lts t params
  in
  Log.trace ~__FUNCTION__ "built lts graph";
  let* the_lts = G.decoq_lts ~cache_decoding:true graph_lts params in
  Log.trace ~__FUNCTION__ "converted lts graph to lts model";
  let* () = fail_if_incomplete the_lts in
  return the_lts
;;

(** *)
let build_fsm
      ?(saturate : bool = false)
      ?(minimize : bool = false)
      (primary_lts : Libnames.qualid)
      (t : Constrexpr.constr_expr)
      (refs : Libnames.qualid list)
      (params : int * Params.WeakEnc.t option)
  : Fsm.t mm
  =
  Log.trace __FUNCTION__;
  (* NOTE: build lts *)
  let* the_lts = build_lts primary_lts t refs (Params.get_fst_params ()) in
  Log.trace ~__FUNCTION__ "obtained lts model";
  Log.thing ~__FUNCTION__ Info "lts" the_lts (Args Lts.to_string);
  (* NOTE: convert to fsm *)
  let the_fsm : Fsm.t = Fsm.of_model (Lts.to_model the_lts) in
  Log.trace ~__FUNCTION__ "converted lts to fsm";
  Log.thing ~__FUNCTION__ Info "fsm" the_fsm (Args Fsm.to_string);
  (* NOTE: debug messages *)
  let b : 'a Logger.to_string = Of Utils.Strfy.bool in
  Log.thing ~__FUNCTION__ Debug "weakmode" !Params.the_weak_mode b;
  Log.thing ~__FUNCTION__ Debug "minimize" minimize b;
  Log.thing ~__FUNCTION__ Debug "saturate" saturate b;
  (* NOTE: return fsm (and do any further operations) *)
  let fsm_to_return : Fsm.t =
    if minimize
    then Algorithms.Minimize.run ~weak:!Params.the_weak_mode the_fsm |> fst
    else if saturate
    then Saturate.fsm the_fsm
    else the_fsm
  in
  return fsm_to_return
;;

(**********************)
(** Entry point *******)
(**********************)

type model_kind =
  | LTS
  | FSM

type coq_model = Constrexpr.constr_expr * Libnames.qualid
type make_model = model_kind * coq_model

type command_kind =
  | Help of Mebi_help.help_kind
  | MakeModel of make_model
  | SaturateModel of coq_model
  | MergeModels of (coq_model * coq_model)
  | MinimizeModel of coq_model
  | CheckBisimilarity of (coq_model * coq_model)
  | Info of unit

let make_model refs
  : model_kind * (Constrexpr.constr_expr * Libnames.qualid) -> unit mm
  =
  Log.trace __FUNCTION__;
  function
  | LTS, (x, primary_lts) ->
    let* the_lts : Lts.t =
      Params.get_fst_params () |> build_lts primary_lts x refs
    in
    Log.thing ~__FUNCTION__ Notice "lts" the_lts (Of Lts.to_string);
    return ()
  | FSM, (x, primary_lts) ->
    let* the_fsm : Fsm.t =
      Params.get_fst_params () |> build_fsm primary_lts x refs
    in
    Log.thing ~__FUNCTION__ Notice "fsm" the_fsm (Of Fsm.to_string);
    return ()
;;

let only_in_weak_mode (f : 'a mm) : unit mm =
  Log.trace __FUNCTION__;
  match !Params.the_weak_mode, Params.fst_weak_type () with
  | true, Some _ ->
    Log.debug ~__FUNCTION__ "weak mode, has been set. (running.)";
    let* _ = f in
    return ()
  | true, None ->
    Log.debug ~__FUNCTION__ "weak mode, not set";
    Mebi_help.show_instructions_to_set_weak ();
    return ()
  | false, Some _ ->
    Log.debug ~__FUNCTION__ "not in weak mode, (but has been set)";
    Log.notice ~__FUNCTION__ "Aborting command. (Not in weak mode)\n";
    Mebi_help.show_instructions_to_enable_weak ();
    return ()
  | false, None ->
    Log.debug ~__FUNCTION__ "not in weak mode (weak also not set)";
    Log.notice ~__FUNCTION__ "Aborting command. (Not in weak mode)\n";
    Mebi_help.show_instructions_to_enable_weak ();
    Mebi_help.show_instructions_to_set_weak ();
    return ()
;;

let saturate_model ((x, primary_lts) : coq_model) refs : unit mm =
  Log.trace __FUNCTION__;
  only_in_weak_mode
    (let* the_fsm =
       Params.get_fst_params () |> build_fsm ~saturate:true primary_lts x refs
     in
     Log.trace ~__FUNCTION__ "obtained saturated fsm";
     Log.notice "Successfully saturated FSM. (To see enable Info display.)";
     Log.thing ~__FUNCTION__ Info "saturated fsm" the_fsm (Args Fsm.to_string);
     return ())
;;

let minimize_model ((x, primary_lts) : coq_model) refs : unit mm =
  Log.trace __FUNCTION__;
  only_in_weak_mode
    (let* the_fsm =
       Params.get_fst_params () |> build_fsm ~minimize:true primary_lts x refs
     in
     Log.trace ~__FUNCTION__ "obtained minimized fsm";
     Log.notice "Successfully minimized FSM.";
     Log.thing ~__FUNCTION__ Notice "minimized fsm" the_fsm (Args Fsm.to_string);
     return ())
;;

let merge_models ((x, a), (y, b)) refs : unit mm =
  Log.trace __FUNCTION__;
  (* NOTE: construct both fsm *)
  let* the_fsm_1 = build_fsm a x refs (Params.get_fst_params ()) in
  let* the_fsm_2 = build_fsm b y refs (Params.get_snd_params ()) in
  Log.trace ~__FUNCTION__ "built both fsms";
  (* NOTE: if weak-mode then pre-saturate the fsms *)
  let (the_fsm_1, the_fsm_2) : Fsm.pair =
    if !Params.the_weak_mode
    then Saturate.fsm the_fsm_1, Saturate.fsm the_fsm_2
    else the_fsm_1, the_fsm_2
  in
  Log.trace ~__FUNCTION__ "about to merge (pre-saturated fsms if in weak-mode)";
  let the_fsm : Fsm.t = Fsm.merge the_fsm_1 the_fsm_2 in
  Log.trace ~__FUNCTION__ "merged fsms";
  Log.notice "Successfully merged FSMs. (For details enable Info display.)";
  Log.thing ~__FUNCTION__ Notice "merged fsm" the_fsm (Args Fsm.to_string);
  Log.thing ~__FUNCTION__ Info "fst fsm" the_fsm_1 (Args Fsm.to_string);
  Log.thing ~__FUNCTION__ Info "snd fsm" the_fsm_2 (Args Fsm.to_string);
  return ()
;;

(** *)
let fail_if_not_bisim (x : bool) : unit mm =
  Log.trace __FUNCTION__;
  if !Params.the_fail_if_not_bisim && x
  then return ()
  else params_fail_if_not_bisim ()
;;

let check_bisimilarity ((x, a), (y, b)) refs : unit mm =
  Log.trace __FUNCTION__;
  let* the_fsm_1 = build_fsm a x refs (Params.get_fst_params ()) in
  let* the_fsm_2 = build_fsm b y refs (Params.get_snd_params ()) in
  Log.trace ~__FUNCTION__ "built both fsms";
  Log.thing ~__FUNCTION__ Debug "init fst fsm" the_fsm_1 (Args Fsm.to_string);
  Log.thing ~__FUNCTION__ Debug "init snd fsm" the_fsm_2 (Args Fsm.to_string);
  let open Algorithms in
  let the_result : Bisimilar.result =
    Bisimilar.run ~weak:!Params.the_weak_mode (the_fsm_1, the_fsm_2)
  in
  Log.trace ~__FUNCTION__ "finished checking bisimilarity";
  (* NOTE: cache result -- used in [Mebi_proof] if called from [proof_intro] *)
  Bisimilar.set_the_result the_result;
  (* NOTE: print feedback to user *)
  let bisimilar : bool = Bisimilar.result_to_bool the_result in
  Log.thing Notice "Bisimilar" bisimilar (Args Utils.Strfy.bool);
  Log.thing Info "Details" the_result (Of Bisimilar.to_string);
  (* NOTE: debug messages *)
  let s1 : Fsm.t = the_result.the_fsm_1.saturated in
  Log.thing Debug "(saturated) FSM 1" s1 (Args Fsm.to_string);
  let s2 : Fsm.t = the_result.the_fsm_2.saturated in
  Log.thing Debug "(saturated) FSM 2" s2 (Args Fsm.to_string);
  (* NOTE: check if bisimilar before returning *)
  let* () = fail_if_not_bisim bisimilar in
  return ()
;;

let run (k : command_kind) (refs : Libnames.qualid list) : 'a mm =
  Log.trace __FUNCTION__;
  let* _ = Params.obtain_weak_kinds_from_args () in
  match k with
  | MakeModel args -> make_model refs args
  | SaturateModel args -> saturate_model args refs
  | MergeModels args -> merge_models args refs
  | MinimizeModel args -> minimize_model args refs
  | CheckBisimilarity args -> check_bisimilarity args refs
  | Info () ->
    Mebi_help.show_guidelines_and_limitations ();
    return ()
  | Help c ->
    Mebi_help.handle_help c;
    return ()
;;

let proof_intro
      (pstate : Declare.Proof.t)
      ((x, a) : coq_model)
      ((y, b) : coq_model)
      (refs : Libnames.qualid list)
  : Declare.Proof.t mm
  =
  Log.trace __FUNCTION__;
  Mebi_proof.reset_the_proof_state ();
  Params.set_fail_if_incomplete true;
  Params.set_fail_if_not_bisim true;
  let* _ = run (CheckBisimilarity ((x, a), (y, b))) refs in
  Log.trace ~__FUNCTION__ "finished checking bisimilarity, beginning proof";
  return
    (Mebi_tactics.update_proof_by_tactic
       pstate
       (Proofview.Goal.enter (fun gl ->
          Mebi_tactics.unfold_constrexpr_list gl [ x; y ])))
;;
