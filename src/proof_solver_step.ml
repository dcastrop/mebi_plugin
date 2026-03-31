exception NothingToDo

module type S = sig
  type tactic

  include Proof_solver_wrapper.S

  module Theory :
    Proof_solver_theory.S with type enc = enc and type 'a im = 'a mm

  module Tacs :
    Proof_solver_tactics.S
    with type 'a mm = 'a mm
     and type enc = enc
     and type tactic = tactic
     and type econstrset = EConstrSet.t

  val step : unit -> tactic
end

module Make
    (Log : Logger.S)
    (Enc : Encoding.S)
    (Tactic : Proof_solver_tactic.S)
    (W :
       Results.S
       with type enc = Enc.t
        and type node = Enc.Tree.Node.t
        and type tree = Enc.Tree.t
        and type trees = Enc.Trees.t)
    (ProofState :
       Proof_solver_statem.S
       with type enc = Enc.t
        and type node = Enc.Tree.Node.t
        and type state = W.Model.State.t
        and type label = W.Model.Label.t
        and type annotation = W.Model.Annotation.t
        and type transition = W.Model.Transition.t)
    (TheoryMaker : (Iter : Proof_solver_wrapper.S
                           with type enc = Enc.t
                            and type tree = Enc.Tree.t) ->
       Proof_solver_theory.S
       with type 'a mm = 'a W.M.mm
        and type 'a im = 'a Iter.mm
        and type enc = Enc.t
        and type fsm = W.Model.FSM.t)
    (X : Proof_solver_wrapper.Args) :
  S with type enc = Enc.t and type tree = Enc.Tree.t and type tactic = Tactic.t =
struct
  type tactic = Tactic.t

  module M = W.M
  module Model = W.Model
  module Decode = W.Decode
  module Bindings = W.Bindings
  module ConstructorBindings = W.ConstructorBindings

  (** [module Iter] is a custom wrapper around a [module Rocq_monad_utils] made for this specific iteration of the proof, where the [env] and [sigma] are obtained from the current [Proofview.Goal.t], and also contains specific helper functions.
  *)
  module Iter :
    Proof_solver_wrapper.S with type enc = Enc.t and type tree = Enc.Tree.t =
    Proof_solver_wrapper.Make (Log) (Enc) (X)

  include Iter

  (** [module Theory] allows the current proof-iteration step [module Iter] to interface and reason with the contents of [module Theories]. It handles cases where we also need to use both the bisimilarity-checking wrapper [module W] {i {e.g., [module Model]}.}. *)
  module Theory :
    Proof_solver_theory.S
    with type 'a mm = 'a M.mm
     and type 'a im = 'a Iter.mm
     and type enc = Enc.t
     and type fsm = W.Model.FSM.t =
    TheoryMaker (Iter)

  (** [module Tacs] contains the tactics used to solve the proofs. Not to be confused with [module Tactic] which is our custom wrapper-type around ['a Proofview.tactic]. All tactics in [module Tacs] do return [Tactic.t].
  *)
  module Tacs :
    Proof_solver_tactics.S
    with type 'a mm = 'a Iter.mm
     and type enc = Enc.t
     and type node = Enc.Tree.Node.t
     and type bindings = Bindings.t
     and type constructorbindings = ConstructorBindings.t
     and type state = Model.State.t
     and type label = Model.Label.t
     and type rocqlts = Model.Info.Meta.RocqLTS.t
     and type tactic = Tactic.t
     and type econstrset = Iter.EConstrSet.t =
    Proof_solver_tactics.Make (Log) (Enc) (Tactic) (W) (Iter) (Theory)

  (** [module ReModel] is for extracting the model component {i (e.g., state, label in [module W.Model])} corresponding to an [EConstr.t] term in the proof.
  *)
  module ReModel = struct
    exception
      CouldNotFind_State of
        { x : EConstr.t
        ; states : Model.States.t
        }

    let state (x : EConstr.t) (ys : Model.States.t) : Model.State.t M.mm =
      Log.trace __FUNCTION__;
      try
        let enc : Enc.t = M.get_encoding x in
        (* NOTE: [Model.States.compare] only cares about [base]. *)
        Model.States.find { base = enc } ys |> M.return
      with
      | M.EncodingNotFound _ ->
        log_econstr ~__FUNCTION__ ~s:"Err: M.EncodingNotFound" x;
        raise (CouldNotFind_State { x; states = ys })
      | Not_found -> raise (CouldNotFind_State { x; states = ys })
    ;;

    let _state_opt (x : EConstr.t) (ys : Model.States.t)
      : Model.State.t option M.mm
      =
      Log.trace __FUNCTION__;
      try
        let open M.Syntax in
        let* z = state x ys in
        M.return (Some z)
      with
      | CouldNotFind_State _ -> M.return None
    ;;

    exception
      CouldNotFind_Label of
        { x : EConstr.t
        ; alphabet : Model.Labels.t
        }

    let label (x : EConstr.t) (ys : Model.Labels.t) : Model.Label.t M.mm =
      Log.trace __FUNCTION__;
      let f (enc : Enc.t) : Model.Label.t M.mm =
        (* NOTE: [Model.Labels.compare] only cares about [is_silent=Some _] *)
        Model.Labels.find { base = enc; is_silent = None } ys |> M.return
      in
      try M.get_encoding x |> f with
      | M.EncodingNotFound _ ->
        log_econstr ~__FUNCTION__ ~s:"Err: M.EncodingNotFound" x;
        raise (CouldNotFind_Label { x; alphabet = ys })
      | Not_found ->
        let open M.Syntax in
        (* NOTE: is it [None]? (i.e., a silent action) *)
        (try
           let* term : Enc.t = Theory.get_None_enc_if_eq x in
           f term
         with
         | Theory.NotEqTheory ->
           (* NOTE: is it [Some]? (i.e., a visible action) *)
           (try
              let* term : Enc.t = Theory.get_Some_enc_if_eq x in
              f term
            with
            | Theory.NotEqTheory ->
              raise (CouldNotFind_Label { x; alphabet = ys })))
    ;;

    let _label_opt (x : EConstr.t) (ys : Model.Labels.t)
      : Model.Label.t option M.mm
      =
      Log.trace __FUNCTION__;
      try
        let open M.Syntax in
        let* z = label x ys in
        M.return (Some z)
      with
      | CouldNotFind_Label _ -> M.return None
    ;;

    exception
      CouldNotFind_Transition of
        { from : Model.State.t
        ; goto : Model.State.t
        ; label : Model.Label.t
        ; edges : Model.EdgeMap.t'
        }

    let transition
          (from : Model.State.t)
          (goto : Model.State.t)
          (label : Model.Label.t)
          (edges : Model.EdgeMap.t')
      : Model.Transition.t
      =
      Log.trace __FUNCTION__;
      (* TODO: export some of this to the [Model.ActionMap] ? *)
      let actions = Model.EdgeMap.find edges from in
      let labelled = Model.ActionMap.reduce_by_label actions label in
      if Model.ActionMap.length labelled |> Int.equal 0
      then raise (CouldNotFind_Transition { from; goto; label; edges })
      else (
        let actionpairs =
          Model.ActionMap.to_seq labelled
          |> List.of_seq
          |> List.filter
               (fun
                   ((action, destinations) : Model.Action.t * Model.States.t) ->
               Model.States.mem goto destinations)
        in
        match actionpairs with
        | [] -> raise (CouldNotFind_Transition { from; goto; label; edges })
        | ({ annotation; trees; _ }, _) :: [] ->
          let tree : Enc.Tree.t option = Enc.Trees.min_opt trees in
          { from; goto; label; annotation; tree }
        | h :: tl ->
          (* TODO: move this proceed to [Model] and handle this case *)
          Log.warning ~__FUNCTION__ "Multiple actionpairs found";
          raise (CouldNotFind_Transition { from; goto; label; edges }))
    ;;
  end

  (** [module Hyp] is for a single proof hypothesis. *)
  module Hyp = struct
    type t = Rocq_utils.hyp

    let compare_name (a : t) (b : t) : int =
      let a : Names.Id.t = Context.Named.Declaration.get_id a in
      let b : Names.Id.t = Context.Named.Declaration.get_id b in
      Names.Id.compare a b
    ;;

    let name_to_string (x : t) : string = Strfy.hyp_name x

    let to_atomic (x : t) : EConstr.t Rocq_utils.kind_pair mm =
      let open Syntax in
      let* sigma = get_sigma in
      Rocq_utils.hyp_to_atomic sigma x |> return
    ;;

    (** [invertibility x] returns an integer denoting whether [x] need be inverted, with the higher numbers being of more importance to invert and [0] denoting [x] does not need to be inverted.
    *)
    let invertibility (x : t) : int mm =
      Log.trace __FUNCTION__;
      let open Syntax in
      let* _, tys = to_atomic x in
      let* sigma = get_sigma in
      (* NOTE: returns true if can be inverted *)
      let rec f (x : EConstr.t) : bool =
        match EConstr.kind sigma x with
        | Var _ -> EConstr.isRef sigma x
        | App (_, tys) -> Array.exists f tys
        | _ -> false
      in
      (* NOTE: since [2] is the goto-state and [1] is the label, [g] allows us to clearly see which hyp needs to be inverted first. *)
      let g (i : int) : int =
        try if f tys.(i) then i else 0 with
        (* NOTE: handles "Index out of bounds" for accessing [tys] array. *)
        | Invalid_argument _ -> 0
      in
      g 2 + g 1 |> return
    ;;

    let _need_inversion (x : t) : bool mm =
      Log.trace __FUNCTION__;
      let open Syntax in
      let* n : int = invertibility x in
      if Int.equal n 0 then return false else return true
    ;;

    let invert (x : t) : Tactic.t mm = Tacs.inversion x

    (** [try_unfold_any x] obtains the [Atomic (ty, tys)] of the given hyp [x], and first checks to see if there is anything that can be unfolded in [ty] (via [Tacs.try_unfold_any ~in_hyp:x]) then returns it. Else, if [ty] cannot be unfolded, then we check each of [tys] and return any.
    *)
    let try_unfold_any (x : t) : Tactic.t option mm =
      let open Syntax in
      let* sigma = get_sigma in
      try
        let ty, tys = Rocq_utils.hyp_to_atomic sigma x in
        let* ty_opt : Tactic.t option = Tacs.try_unfold_any ~in_hyp:x ty in
        match ty_opt with
        | Some y -> return (Some y)
        | None ->
          (* NOTE: check if any in [tys] can be unfolded *)
          let f (i : int) (acc : Tactic.t option) : Tactic.t option mm =
            let y = tys.(i) in
            let* y : Tactic.t option = Tacs.try_unfold_any ~in_hyp:x y in
            match y with
            | None -> return acc
            | Some y ->
              (match acc with
               | None -> return (Some y)
               | Some acc -> return (Some (Tactic.seq acc y)))
          in
          iterate 0 (Array.length tys - 1) None f
      with
      | Rocq_utils.Rocq_utils_HypIsNot_Atomic _ -> return None
    ;;

    exception
      CouldNotGetTransition of
        { hyp : t
        ; fsm : Model.FSM.t
        }

    (** *)
    let get_transition (x : t) (m : Model.FSM.t) : Model.Transition.t mm =
      Log.trace __FUNCTION__;
      let open Syntax in
      let* ty, tys = to_atomic x in
      if Theory.is_fsm_constructor ty m
      then (
        try
          let from : Model.State.t = M.run (ReModel.state tys.(0) m.states) in
          Model.State.log ~__FUNCTION__ ~s:"from" from;
          log_econstr ~__FUNCTION__ ~s:"from" (Decode.state from);
          let goto : Model.State.t = M.run (ReModel.state tys.(2) m.states) in
          Model.State.log ~__FUNCTION__ ~s:"goto" goto;
          log_econstr ~__FUNCTION__ ~s:"goto" (Decode.state goto);
          let label : Model.Label.t =
            M.run (ReModel.label tys.(1) m.alphabet)
          in
          Model.Label.log ~__FUNCTION__ ~s:"label" label;
          log_econstr ~__FUNCTION__ ~s:"label" (Decode.label label);
          ReModel.transition from goto label m.edges |> return
        with
        (* | M.EncodingNotFound z ->
            log_econstr ~__FUNCTION__ ~s:"Err: M.EncodingNotFound" z;
            raise (CouldNotGetTransition { hyp = x; fsm = m }) *)
        | ReModel.CouldNotFind_State _ ->
          Log.trace ~__FUNCTION__ "Err: ReModel.CouldNotFind_State";
          raise (CouldNotGetTransition { hyp = x; fsm = m })
        | ReModel.CouldNotFind_Label _ ->
          Log.trace ~__FUNCTION__ "Err: ReModel.CouldNotFind_Label";
          raise (CouldNotGetTransition { hyp = x; fsm = m })
        | ReModel.CouldNotFind_Transition _ ->
          Log.trace ~__FUNCTION__ "Err: ReModel.CouldNotFind_Transition";
          raise (CouldNotGetTransition { hyp = x; fsm = m }))
      else (
        Log.trace ~__FUNCTION__ "(else)";
        raise (CouldNotGetTransition { hyp = x; fsm = m }))
    ;;
  end

  (** [module Concl] is for the proof conclusion. *)
  module Concl = struct
    let eq (x : EConstr.t) : bool = get_concl () |> econstr_eq x |> run

    let eq_hyp (x : Rocq_utils.hyp) : bool =
      Context.Named.Declaration.get_type x |> eq
    ;;

    let rec eq_any_hyps : Rocq_utils.hyp list -> bool mm = function
      | [] -> return false
      | h :: tl -> if eq_hyp h then return true else eq_any_hyps tl
    ;;

    let is_weak_refl () : bool mm =
      let open Syntax in
      let* ty, tys = get_concl () |> to_atomic in
      if econstr_eq tys.(3) tys.(4) |> run
      then econstr_eq tys.(5) tys.(6)
      else return false
    ;;

    let is_weak_sim () : bool mm = get_concl () |> Theory.is_weak_sim
    let is_exists () : bool mm = get_concl () |> Theory.is_exists
    let is_tau () : bool mm = get_concl () |> Theory.is_tau

    (** [try_unfold_any ()] is similar to [Hyp.try_unfold_any _], except that instead of a hypothesis, it uses the conclusion. Uses [Tacs.try_unfold_any].
    *)
    let try_unfold_any () : Tactic.t option mm =
      Log.trace __FUNCTION__;
      let open Syntax in
      let* ty, tys = get_concl () |> to_atomic in
      let* ty_opt : Tactic.t option = Tacs.try_unfold_any ty in
      match ty_opt with
      | Some y -> return (Some y)
      | None ->
        (* NOTE: check if any in [tys] can be unfolded *)
        let f (i : int) (acc : Tactic.t option) : Tactic.t option mm =
          let y = tys.(i) in
          let* y : Tactic.t option = Tacs.try_unfold_any y in
          match y with
          | None -> return acc
          | Some y ->
            (match acc with
             | None -> return (Some y)
             | Some acc -> return (Some (Tactic.seq acc y)))
        in
        iterate 0 (Array.length tys - 1) None f
    ;;

    type wk_conj =
      { wk_trans : EConstr.t
      ; wk_sim : EConstr.t
      }

    type conj =
      { a' : Model.State.t
      ; b : Model.State.t
      }

    let get_a'_from_wk_sim (wk_sim : EConstr.t) : Model.State.t mm =
      let open Syntax in
      let* _, tys = to_atomic wk_sim in
      (W.get_fsm_a ()).states |> ReModel.state tys.(5) |> M.run |> return
    ;;

    let get_b_from_wk_trans (wk_trans : EConstr.t) : Model.State.t mm =
      let open Syntax in
      let* _, tys = to_atomic wk_trans in
      (W.get_fsm_b ()).states |> ReModel.state tys.(3) |> M.run |> return
    ;;

    exception ConclDoesNotMatchConj

    let get_wk_conj () : wk_conj mm =
      Log.trace __FUNCTION__;
      let open Syntax in
      let* ty, tys = get_concl () |> to_atomic in
      let* () = Theory.ensure ty Theory.is_exists in
      let* _, _, x = to_lambda tys.(1) in
      let* _, app_tys = to_app x in
      match Array.to_list app_tys with
      | [ wk_trans; wk_sim ] -> return { wk_trans; wk_sim }
      | _ -> raise ConclDoesNotMatchConj
    ;;

    let get_conj ({ wk_trans; wk_sim } : wk_conj) : conj mm =
      Log.trace __FUNCTION__;
      let open Syntax in
      let* () = Theory.ensure wk_sim Theory.is_weak_sim in
      let* a' = get_a'_from_wk_sim wk_sim in
      let* b = get_b_from_wk_trans wk_trans in
      return { a'; b }
    ;;
  end

  (** [module Hyps] is for all the proof's hypotheses. *)
  module Hyps = struct
    (** [get_cofixes ()] filters the hyps by name according to [get_all_cofix_hyp_names ()].
    *)
    let get_cofixes () : Rocq_utils.hyp list =
      let cofix_names : Names.Id.Set.t = get_all_cofix_hyp_names () in
      get_hyps ()
      |> List.filter (fun (x : Rocq_utils.hyp) ->
        Names.Id.Set.mem (get_hyp_name x) cofix_names)
      |> List.sort Hyp.compare_name
    ;;

    (** [get_non_cofixes ()] filters the hyps by name according to [get_all_non_cofix_hyp_names ()].
    *)
    let get_non_cofixes () : Rocq_utils.hyp list =
      let cofix_names : Names.Id.Set.t = get_all_non_cofix_hyp_names () in
      get_hyps ()
      |> List.filter (fun (x : Rocq_utils.hyp) ->
        Names.Id.Set.mem (get_hyp_name x) cofix_names)
      |> List.sort Hyp.compare_name
    ;;

    let log ?(cofix_only : bool option = None) () : unit =
      match cofix_only with
      | None -> log_hyps ()
      | Some true ->
        Log.things Debug "hyps (cofixes)" (get_cofixes ()) Strfy.hyp
      | Some false ->
        Log.things Debug "hyps (non-cofixes)" (get_non_cofixes ()) Strfy.hyp
    ;;

    (** [can_solve_concl_cofix ()] returns true if there is a hyp that can solve the current a tactic to solve the current goal using one of he cofixes in the hyps.
    *)
    let can_solve_concl_cofix () : bool mm = get_cofixes () |> Concl.eq_any_hyps

    (** [clear_non_cofix ()] returns a tactic that will clear all the hyps that are named according to [get_all_non_cofix_hyp_names ()]. This is to be used at the end of a case of the proof has been solved.
        (* TODO: check if this is necessary -- or could be problematic? *) *)
    let clear_non_cofix () : Tactic.t =
      Tactic.create
        ~msg:"(Clearing non-cofix Hyps)"
        (Tactics.clear
           (Names.Id.Set.to_seq (get_all_non_cofix_hyp_names ()) |> List.of_seq))
    ;;

    (** [try_invert_any inverted_hyps] returns either [None] if no hyps can be inverted (as determined by [Hyp.invertibility]), else a [Tactic.t] that will invert the hypothesis deemed to be the most important to invert. (Only checks non-cofix hyps as by [get_non_cofixes ()].)
    *)
    let try_invert_any () : Tactic.t option mm =
      Log.trace __FUNCTION__;
      (* log_econstrs ~__FUNCTION__ "inverted hyps" !inverted_hyps; *)
      let hyps : Hyp.t list = get_non_cofixes () in
      let open Syntax in
      let f (i : int) (xopt : (int * Hyp.t) option) : (int * Hyp.t) option mm =
        let y : Hyp.t = List.nth hyps i in
        let* grade : int = Hyp.invertibility y in
        Log.debug
          ~__FUNCTION__
          (Printf.sprintf "grade %i : %s" grade (Hyp.name_to_string y));
        match xopt with
        | None -> Some (grade, y) |> return
        | Some (n, x) ->
          (match Int.compare grade n with
           | -1 -> Some (n, x) |> return
           | _ -> Some (grade, y) |> return)
      in
      let* to_invert_opt = iterate 0 (List.length hyps - 1) None f in
      match to_invert_opt with
      | None -> return None
      | Some (0, x) -> return None
      (* NOTE: we only want to invert hyps with non-zero grades. *)
      | Some (grade, x) ->
        let* y = Hyp.invert x in
        return (Some y)
    ;;

    (** [try_unfold_any ()] returns the optional [Tactic.t] that is a sequence derived from all of the hyps ([xs]) from [Hyp.try_unfold_any x] (where [x] is a hyp in [xs]).
    *)
    let try_unfold_any () : Tactic.t option mm =
      let hyps = get_non_cofixes () in
      let open Syntax in
      let f (i : int) (acc : Tactic.t option) : Tactic.t option mm =
        let x = List.nth hyps i in
        let* x = Hyp.try_unfold_any x in
        match x with
        | None -> return acc
        | Some x ->
          (match acc with
           | None -> return (Some x)
           | Some acc -> return (Some (Tactic.seq acc x)))
      in
      iterate 0 (List.length hyps - 1) None f
    ;;

    exception CannotGetTransition of Model.FSM.t

    let get_transition (m : Model.FSM.t) : Model.Transition.t mm =
      Log.trace __FUNCTION__;
      let hyps = get_non_cofixes () in
      let open Syntax in
      let f (i : int)
        : Model.Transition.t option -> Model.Transition.t option mm
        =
        (* Log.thing ~__FUNCTION__ Trace "i" i ( Utils.Strfy.int); *)
        function
        | Some x -> return (Some x)
        | None ->
          let y = List.nth hyps i in
          (try
             let y : Model.Transition.t = Hyp.get_transition y m |> run in
             return (Some y)
           with
           | Hyp.CouldNotGetTransition _ -> return None)
      in
      let* x = iterate 0 (List.length hyps - 1) None f in
      match x with None -> raise (CannotGetTransition m) | Some x -> return x
    ;;
  end

  (** [handle_new_cofix ()] returns a sequence of tactics to handle the creation of a new cofix in the hyps, followed by the necessary application of constructors and introduction of terms to get started on a new case.
  *)
  let handle_new_cofix () : Tactic.t mm =
    Log.trace __FUNCTION__;
    let open Syntax in
    let* unfold_opt = Concl.try_unfold_any () in
    match unfold_opt with
    | Some x ->
      Log.trace ~__FUNCTION__ "do unfold";
      return x
    | None ->
      Log.trace ~__FUNCTION__ "nothing to unfold";
      let* cofix : Tactic.t = Tacs.cofix () in
      let clear : Tactic.t = Hyps.clear_non_cofix () in
      let* apply_In_sim : Tactic.t = Tacs.apply_In_sim () in
      let* apply_Pack_sim : Tactic.t = Tacs.apply_Pack_sim () in
      let* intros_all : Tactic.t = Tacs.intros_all () in
      Tactic.chain [ cofix; clear; apply_In_sim; apply_Pack_sim; intros_all ]
      |> return
  ;;

  exception CouldNotFindGotoState

  let try_get_visible_transition
        ?(saturated : bool = false)
        (bisimilar : Model.States.t)
        (tys : EConstr.t array)
    : Model.Transition.t
    =
    Log.trace __FUNCTION__;
    let m : Model.FSM.t = W.get_fsm_b ~saturated () in
    let from : Model.State.t = M.run (ReModel.state tys.(3) m.states) in
    let label : Model.Label.t = M.run (ReModel.label tys.(5) m.alphabet) in
    try
      let ({ annotation; trees; _ }, destinations) : Model.ActionPair.t =
        (* NOTE: get actions [from] with [label] *)
        Model.ActionMap.reduce_by_label (Model.EdgeMap.find m.edges from) label
        |> Model.ActionMap.to_actionpairs
        (* NOTE: keep only those that are [bisimilar] *)
        |> Model.ActionPairs.filter_map (fun ((x, y) : Model.ActionPair.t) ->
          if Model.States.disjoint bisimilar y
          then None
          else Some (x, Model.States.inter bisimilar y))
        (* NOTE: get the pair with the shortest annotation (less steps to do) *)
        |> Model.ActionPairs.shortest_annotation
      in
      let tree : Enc.Tree.t option = Enc.Trees.min_opt trees in
      let goto : Model.State.t = Model.States.min_elt destinations in
      { from; goto; label; annotation; tree }
    with
    | Model.ActionPairs.IsEmpty -> raise CouldNotFindGotoState
  ;;

  exception MisMatchedStates of (Model.State.t * Model.State.t)

  let ensure_matching_states (x : Model.State.t) (y : Model.State.t) : unit =
    Log.trace __FUNCTION__;
    if Model.State.equal x y then () else raise (MisMatchedStates (x, y))
  ;;

  exception
    CouldNotGetGoalTransition of
      { b : Model.State.t
      ; wk_trans : EConstr.t
      }

  (** [handle_visible_transition hyp b wk_trans] ... introduces *)
  let handle_visible_transition
        (hyp : Model.Transition.t)
        (b : Model.State.t)
        (wk_trans : EConstr.t)
    : Tactic.t mm
    =
    Log.trace __FUNCTION__;
    (* log_econstr ~__FUNCTION__ ~s:"wk_trans" wk_trans; *)
    let bisimilar : Model.States.t = W.get_bisimilar_states hyp.goto in
    (* log_states ~__FUNCTION__ "bisimilar" bisimilar; *)
    let open Syntax in
    let* ty, tys = to_atomic wk_trans in
    (* NOTE: first try to do single action, if fail then saturated *)
    let goal : Model.Transition.t =
      try try_get_visible_transition ~saturated:true bisimilar tys with
      | CouldNotFindGotoState ->
        raise (CouldNotGetGoalTransition { b; wk_trans })
    in
    Model.Transition.log ~__FUNCTION__ ~s:"goal" goal;
    ensure_matching_states goal.from b;
    ProofState.update_statem
      (ApplyConstructors (ProofState.ApplicableConstructors.init goal));
    Tacs.ex_intro_split goal.goto
  ;;

  let handle_wk_concl
        (hyp : Model.Transition.t)
        ({ wk_trans; wk_sim } : Concl.wk_conj)
    : Tactic.t mm
    =
    Log.trace __FUNCTION__;
    let open Syntax in
    let* { a'; b } = Concl.get_conj { wk_trans; wk_sim } in
    ensure_matching_states hyp.goto a';
    if Model.Transition.is_silent hyp && W.are_states_bisimilar a' b
    then (
      Log.trace ~__FUNCTION__ "is_exists, silent";
      Tacs.ex_intro_split b)
    else (
      Log.trace ~__FUNCTION__ "is_exists, trans";
      handle_visible_transition hyp b wk_trans)
  ;;

  (* * [handle_hyp_transition ()] determines which term to introduce for [exists b'], checking whether we can do this via a silent/tau transition, and sets up the information we will need for the next state. *)
  let handle_hyp_transition () : Tactic.t mm =
    Log.trace __FUNCTION__;
    let open Syntax in
    let* hyp : Model.Transition.t = Hyps.get_transition (W.get_fsm_a ()) in
    Model.Transition.log ~__FUNCTION__ ~s:"hyp" hyp;
    ProofState.update_statem (Exists (Some hyp));
    let* { wk_trans; wk_sim } = Concl.get_wk_conj () in
    Log.trace ~__FUNCTION__ "wk_trans; wk_sim";
    let* unfold_opt = Tacs.try_unfold_any_of [ wk_trans; wk_sim ] in
    match unfold_opt with
    | Some x -> return x
    | None -> handle_wk_concl hyp { wk_trans; wk_sim }
  ;;

  (** [handle_appconstrs_entry_point args] ... *)
  let handle_appconstrs_entry_point (label : Model.Label.t) : Tactic.t mm =
    Log.trace __FUNCTION__;
    let open Syntax in
    let* constructor =
      if Model.Label.is_silent label
      then Tacs.apply_wk_none ()
      else Tacs.eapply_wk_some ()
    in
    let unfold_silent = Tacs.unfold_silent () in
    Tactic.seq constructor unfold_silent |> return
  ;;

  (** [handle_appconstrs_stop ()] ... *)
  let handle_appconstrs_stop () : Tactic.t mm =
    Log.trace __FUNCTION__;
    let open Syntax in
    let* simplify = Tacs.simplify_and_subst_all () in
    let* refl = Tacs.eapply_rt1n_refl () in
    Tactic.seq simplify refl |> return
  ;;

  let handle_appconstrs_update_args ({ this; next } : Model.Annotation.t)
    : Enc.Tree.Node.t list option * Model.Annotation.t option
    =
    Log.trace __FUNCTION__;
    Some (Enc.Trees.min this.using |> Enc.Tree.minimize), next
  ;;

  (** [handle_appconstrs_update label] ... *)
  let handle_appconstrs_update (label : Model.Label.t) : Tactic.t mm =
    Log.trace __FUNCTION__;
    let open Syntax in
    let* rt1n = Tacs.eapply_rt1n_via label in
    let* unfold = Concl.try_unfold_any () in
    match unfold with
    | None -> return rt1n
    | Some unfold -> Tactic.seq unfold rt1n |> return
  ;;

  (** [handle_appconstrs_apply x] ...
      (* NOTE: relies on the bindings we extract early on *) *)
  let handle_appconstrs_apply (x : Enc.Tree.Node.t) : Tactic.t mm =
    Log.trace __FUNCTION__;
    let open Syntax in
    let* _, tys = get_concl () |> to_atomic in
    let tys = Array.map (fun x -> econstr_normalize x |> run) tys in
    let* is_tau = Concl.is_tau () in
    (* NOTE: we can't rely on the terms in [tys] being encoded since they may be from an intermediate layer of the LTS. *)
    (if is_tau
     then (* NOTE: index (3) since [tau lts x] => [tau (term * label) x] *)
       { from = tys.(3); goto = None; label = Some (Mebi_theories.get "None") }
     else { from = tys.(0); goto = None; label = None })
    |> Tacs.apply_constructor x
  ;;

  (** [handle_ ()] ... *)
  (* let handle_ () : Tactic.t mm = raise NotImplemented *)

  (***********************************************************************)

  exception SkipNewProof
  exception ExitWeakSim
  exception ProofComplete

  let handle_new_proof
        ((a, b) : Constrexpr.constr_expr * Constrexpr.constr_expr)
    : Tactic.t mm
    =
    Log.trace __FUNCTION__;
    let x = Tacs.unfold_opt_constrexpr_list [ a; b ] in
    match x with
    | None -> raise SkipNewProof
    | Some x ->
      ProofState.update_statem WeakSim;
      return x
  ;;

  let handle_weaksim () : Tactic.t mm =
    Log.trace __FUNCTION__;
    let open Syntax in
    let* is_weak_sim : bool = Concl.is_weak_sim () in
    if is_weak_sim
    then (
      Log.trace ~__FUNCTION__ "is weak sim";
      let* is_weak_refl : bool = Concl.is_weak_refl () in
      if is_weak_refl
      then (
        Log.trace ~__FUNCTION__ "is weak refl";
        Tacs.apply_weak_sim_refl ())
      else
        let* has_hyp_cofix : bool = Hyps.can_solve_concl_cofix () in
        if has_hyp_cofix
        then Tacs.trivial ~msg:"trivial (solve cofix)" ()
        else handle_new_cofix ())
    else if ProofState.is_done ()
    then raise ProofComplete
    else
      (* NOTE: try invert any that need to be inverted *)
      let* invert_opt = Hyps.try_invert_any () in
      match invert_opt with
      | Some x -> return x
      | None ->
        Log.trace ~__FUNCTION__ "no hyps to invert";
        (* NOTE: check if we need to unfold anything in the inverted hyps. *)
        let* unfold_opt = Hyps.try_unfold_any () in
        (match unfold_opt with
         | Some x -> return x
         | None ->
           Log.trace ~__FUNCTION__ "no terms to unfold";
           raise ExitWeakSim)
  ;;

  let handle_exists (hyp_opt : Model.Transition.t option) : Tactic.t mm =
    Log.trace __FUNCTION__;
    let open Syntax in
    let* is_exists : bool = Concl.is_exists () in
    if is_exists
    then (
      match hyp_opt with
      | None -> handle_hyp_transition ()
      | Some hyp ->
        Log.trace ~__FUNCTION__ "Some hyp";
        let open Syntax in
        let* { wk_trans; wk_sim } = Concl.get_wk_conj () in
        handle_wk_concl hyp { wk_trans; wk_sim })
    else (
      (* NOTE: assume we need to finish handling a silent action. *)
      Log.trace ~__FUNCTION__ "not exists, do_refl";
      ProofState.update_statem WeakSim;
      Tacs.do_refl ())
  ;;

  (* let handle_goal_transition ({ hyp; goal } : Transition.t) : Tactic.t mm =
      Log.trace __FUNCTION__;
      raise (StateNotImplemented (GoalTransition { hyp; goal }))
    ;; *)

  let handle_apply_constructors (args : ProofState.ApplicableConstructors.t)
    : Tactic.t mm
    =
    Log.trace __FUNCTION__;
    ProofState.ApplicableConstructors.log ~__FUNCTION__ ~s:"args" args;
    match args with
    | { current = None; label; _ } ->
      (* NOTE: entry-point *)
      ProofState.update_statem
        (ApplyConstructors { args with current = Some [] });
      handle_appconstrs_entry_point label
    | { current = Some []; remaining; _ } ->
      (match remaining with
       | None ->
         (* NOTE: stop *)
         ProofState.update_statem WeakSim;
         handle_appconstrs_stop ()
       | Some anno ->
         (* NOTE: update current, prepare for next transition *)
         let current, remaining = handle_appconstrs_update_args anno in
         ProofState.update_statem
           (ApplyConstructors { args with current; remaining });
         handle_appconstrs_update anno.this.label)
    | { current = Some (h :: tl); _ } ->
      (* NOTE: continue applying constructors *)
      ProofState.update_statem
        (ApplyConstructors { args with current = Some tl });
      handle_appconstrs_apply h
  ;;

  let handle_state () : Tactic.t mm =
    ProofState.log ~__FUNCTION__ ();
    Hyps.log ~cofix_only:(Some false) ();
    log_concl ();
    match ProofState.get_statem () with
    | NewProof ab -> handle_new_proof ab
    | WeakSim -> handle_weaksim ()
    | Exists hyp_opt -> handle_exists hyp_opt
    (* | GoalTransition args -> handle_goal_transition args *)
    | ApplyConstructors xs -> handle_apply_constructors xs
    | Done -> raise NothingToDo
  ;;

  (** [step ()] ... *)
  let rec step () : Tactic.t =
    Log.trace __FUNCTION__;
    try run (handle_state ()) with
    | ProofComplete ->
      Log.trace ~__FUNCTION__ "_:ProofComplete => Done";
      ProofState.update_statem Done;
      Tactic.create ~msg:"Proof Complete" (Proofview.tclUNIT ())
    | SkipNewProof ->
      Log.trace ~__FUNCTION__ "NewProof:SkipNewProof => WeakSim";
      ProofState.update_statem WeakSim;
      step ()
    | ExitWeakSim ->
      Log.trace ~__FUNCTION__ "WeakSim:ExitWeakSim => Exists";
      ProofState.update_statem (Exists None);
      step ()
    (********************)
    | M.EncodingNotFound x ->
      Log.thing Warning "M.EncodingNotFound" x M.Strfy.econstr;
      Log.thing Warning "(using P) EConstr" x Strfy.econstr;
      Log.thing Warning "(using P) is encoded" (encoded x) (Printf.sprintf "%b");
      raise (M.EncodingNotFound x)
    | EncodingNotFound x ->
      Log.thing Warning "(M).EncodingNotFound" x Strfy.econstr;
      Log.thing Warning "(using M) EConstr" x M.Strfy.econstr;
      Log.thing
        Warning
        "(using P) is encoded"
        (M.encoded x)
        (Printf.sprintf "%b");
      raise (M.EncodingNotFound x)
  ;;
end
