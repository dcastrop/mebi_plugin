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

  (* This step's own monad ({!Proof_solver_wrapper}), reading [env] and
     [sigma] from the goal in focus rather than the global environment, with
     helpers over that goal. Included: [mm], [run], [get_concl], ... below
     are its. *)
  module Iter :
    Proof_solver_wrapper.S with type enc = Enc.t and type tree = Enc.Tree.t =
    Proof_solver_wrapper.Make (Enc) (X)

  include Iter

  (* The plugin's theory as read in this goal ({!Iter}), including what
     needs the bisimilarity result in [W] (whether a term is one of an FSM's
     LTSs, say). *)
  module Theory :
    Proof_solver_theory.S
    with type 'a mm = 'a M.mm
     and type 'a im = 'a Iter.mm
     and type enc = Enc.t
     and type fsm = W.Model.FSM.t =
    TheoryMaker (Iter)

  (* The tactics the solver uses, each a [Tactic.t] (not to be confused
     with [Tactic], the wrapper around ['a Proofview.tactic] they are
     built with). *)
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
    Proof_solver_tactics.Make (Enc) (Tactic) (W) (Iter) (Theory)

  (** [is_lts_of_either_fsm x] is whether [x] is one of the LTSs either FSM
      was extracted with ({!Theory.is_fsm_constructor}); [false] for an FSM
      that has no metadata. Raises nothing. *)
  let is_lts_of_either_fsm (x : EConstr.t) : bool =
    let of_fsm (m : Model.FSM.t) : bool =
      try Theory.is_fsm_constructor x m with _ -> false
    in
    of_fsm (W.get_fsm_a ()) || of_fsm (W.get_fsm_b ())
  ;;

  (** Reading a term of the proof back as a part of the model: the state,
      label or transition of {!W.Model} it stands for. *)
  module ReModel = struct
    (** Raised by {!state}: no state of [states] is the term [x]. *)
    exception
      CouldNotFind_State of
        { x : EConstr.t
        ; states : Model.State.Set.t
        }

    (** [state x ys] is the state of [ys] that the term [x] encodes, or, for
        a state renamed apart from the other system's copy
        ({!Wrapper.separate}), the one with an alias of that encoding. The
        lookup runs while the computation is built, so its exception reaches
        a handler around the call.

        @raise CouldNotFind_State
          if [x] has no encoding, or none of [ys] has it (raised here). *)
    let state (x : EConstr.t) (ys : Model.State.Set.t) : Model.State.t M.mm =
      Logger.trace __FUNCTION__;
      if Logger.is_enabled Output.Kind.Debug
      then Logger.debug ~__FUNCTION__ ("key: " ^ M.classify_key x);
      try
        let enc : Enc.t = M.get_encoding x in
        (* NOTE: [Model.State.Set.compare] only cares about [base]. *)
        match Model.State.Set.find_opt { base = enc } ys with
        | Some s -> M.return s
        | None ->
          (* a state renamed apart from the other system's copy
             ([Wrapper.separate]): the term encodes to [enc], the state is
             one of its aliases *)
          (match
             List.find_map
               (fun e -> Model.State.Set.find_opt { base = e } ys)
               (M.aliases_of enc)
           with
           | Some s -> M.return s
           | None -> raise Not_found)
      with
      | M.EncodingNotFound _ ->
        Logger.debug ~__FUNCTION__ "miss: term has no encoding";
        log_econstr ~__FUNCTION__ ~s:"Err: M.EncodingNotFound" x;
        raise (CouldNotFind_State { x; states = ys })
      | Not_found ->
        Logger.debug ~__FUNCTION__ "miss: encoding not among the given states";
        raise (CouldNotFind_State { x; states = ys })
    ;;

    (** Raised by {!label}: no label of [alphabet] is the term [x]. *)
    exception
      CouldNotFind_Label of
        { x : EConstr.t
        ; alphabet : Model.Label.Set.t
        }

    (** [label x ys] is the label of the alphabet [ys] that the term [x]
        encodes. A term whose encoding is not in [ys] is tried as the
        theory's [None] (a silent action), then as [Some] (a visible one),
        and looked up by that encoding. [x]'s own lookup runs while the
        computation is built; the fallback's runs when it is run (it is in
        a [let*] continuation).

        @raise CouldNotFind_Label
          if [x] has no encoding, or is neither [None] nor [Some] (raised
          here).
        @raise Not_found
          if [x] is [None] or [Some] but that encoding is not in [ys]
          (raised when run, past the handler: [TODO.md], "try around a
          monadic value"). *)
    let label (x : EConstr.t) (ys : Model.Label.Set.t) : Model.Label.t M.mm =
      Logger.trace __FUNCTION__;
      if Logger.is_enabled Output.Kind.Debug
      then Logger.debug ~__FUNCTION__ ("key: " ^ M.classify_key x);
      let f (enc : Enc.t) : Model.Label.t M.mm =
        (* NOTE: [Model.Label.Set.compare] only cares about [is_silent=Some _]
        *)
        Model.Label.Set.find { base = enc; is_silent = None } ys |> M.return
      in
      try M.get_encoding x |> f with
      | M.EncodingNotFound _ ->
        Logger.debug ~__FUNCTION__ "miss: term has no encoding";
        log_econstr ~__FUNCTION__ ~s:"Err: M.EncodingNotFound" x;
        raise (CouldNotFind_Label { x; alphabet = ys })
      | Not_found ->
        Logger.debug
          ~__FUNCTION__
          "miss: encoding not among the given alphabet, trying None/Some";
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
              Logger.debug
                ~__FUNCTION__
                "miss: None/Some fallbacks did not match either";
              raise (CouldNotFind_Label { x; alphabet = ys })))
    ;;

    (** Raised by {!transition}: [edges] has no [from -label-> goto]. *)
    exception
      CouldNotFind_Transition of
        { from : Model.State.t
        ; goto : Model.State.t
        ; label : Model.Label.t
        ; edges : Model.EdgeMap.t'
        }

    (** [transition from goto label edges] is the transition [from -label-> goto] of [edges]: of the actions that reach [goto], the one with the
        shortest annotation, and the least of its derivation trees.

        @raise CouldNotFind_Transition
          if [from] has no edges, none labelled [label], or none of those
          reaches [goto] (raised here). *)
    let transition
          (from : Model.State.t)
          (goto : Model.State.t)
          (label : Model.Label.t)
          (edges : Model.EdgeMap.t')
      : Model.Transition.t
      =
      Logger.trace __FUNCTION__;
      (* TODO: export some of this to the [Model.Action.Map] ? *)
      (* a state with no outgoing edges has no entry: a miss like any other,
         not a bare [Not_found] that [Hyps.get_transition] cannot skip *)
      let actions =
        match Model.EdgeMap.find_opt edges from with
        | Some actions -> actions
        | None -> raise (CouldNotFind_Transition { from; goto; label; edges })
      in
      let labelled = Model.Action.Map.reduce_by_label actions label in
      if Model.Action.Map.length labelled |> Int.equal 0
      then raise (CouldNotFind_Transition { from; goto; label; edges })
      else (
        let actionpairs =
          Model.Action.Map.to_seq labelled
          |> List.of_seq
          |> List.filter
               (fun
                   ((action, destinations) : Model.Action.t * Model.State.Set.t)
                  -> Model.State.Set.mem goto destinations)
        in
        match actionpairs with
        | [] -> raise (CouldNotFind_Transition { from; goto; label; edges })
        | h :: tl ->
          (* Multiple [(action, destinations)] pairs can match the same
             [(from, label, goto)] when weak-transition saturation finds
             more than one witness for it (different [Annotation.t]s over
             the same visible label/destination). Pick the one with the
             shortest annotation, same as [Model.Product.respond]
             does for the analogous "several candidates" case --
             fewer silent steps to justify means less proof work later. A
             single candidate is just the degenerate case of this fold
             ([tl = []]), so this also covers what used to be handled as a
             separate branch. *)
          if not (List.is_empty tl)
          then
            Logger.trace
              ~__FUNCTION__
              (Printf.sprintf
                 "multiple actionpairs matched (%d candidates)"
                 (1 + List.length tl));
          let ({ annotation; trees; _ } : Model.Action.t), _ =
            List.fold_left Model.Action.Pair.shorter_annotation h tl
          in
          let tree : Enc.Tree.t option = Enc.Trees.min_opt trees in
          { from; goto; label; annotation; tree })
    ;;
  end

  (** One hypothesis of the goal in focus: its grade for inversion, and
      reading it as a transition. *)
  module Hyp = struct
    type t = Rocq_utils.hyp

    (** [compare_name a b] compares [a]'s and [b]'s names. Raises
        nothing. *)
    let compare_name (a : t) (b : t) : int =
      let a : Names.Id.t = Context.Named.Declaration.get_id a in
      let b : Names.Id.t = Context.Named.Declaration.get_id b in
      Names.Id.compare a b
    ;;

    (** [name_to_string x] is [x]'s name, printed. Raises nothing. *)
    let name_to_string (x : t) : string = Strfy.hyp_name x

    (** [to_atomic x] is [x]'s type, split into its head and arguments.

        @raise Rocq_utils.Rocq_utils_HypIsNot_Atomic
          when run, if that type is not atomic (propagated from
          {!Rocq_utils.hyp_to_atomic}). *)
    let to_atomic (x : t) : EConstr.t Rocq_utils.kind_pair mm =
      let open Syntax in
      let* sigma = get_sigma in
      Rocq_utils.hyp_to_atomic sigma x |> return
    ;;

    (** Grade of a premise hypothesis that is closed and provably false:
        above any LTS step's (at most 3) and an open premise's. *)
    let refutable_grade : int = 5

    (** Grade of a premise hypothesis that mentions variables it determines
        (an output, e.g. the target in [succ_rel n m]): inverting it fixes
        them. Above any LTS step's: the LTS hypothesis it came from has
        already been inverted, and re-inverting that (kept) hypothesis is
        the Step 0 loop. *)
    let open_premise_grade : int = 4

    (** [premise_grade x] is the grade of the premise hypothesis [x] (not
        an LTS step): {!refutable_grade} if it is closed and provably false
        -- refuting it closes the goal outright, the best move there is,
        done by {!Tacs.refute_premise} -- and {!open_premise_grade} if it is
        an inductive [Prop] other than [eq] that still mentions local
        variables; else 0. A branch whose constructor has a false guard
        ([3 <= 2]) closes only by refutation. Raises nothing. *)
    let premise_grade (x : t) : int mm =
      let open Syntax in
      let* env = get_env in
      let* sigma = get_sigma in
      let ty = Context.Named.Declaration.get_type x in
      match Premise_search.prove env sigma ty with
      | Premise_search.Refuted -> return refutable_grade
      | Premise_search.Proved _ -> return 0
      | Premise_search.Unknown ->
        (* still mentions local variables, and inversion applies: an output
           to compute (equations are left to [subst]) *)
        let mentions_vars =
          Bool.not
            (Names.Id.Set.is_empty (Termops.global_vars_set env sigma ty))
        in
        let inductive_non_eq =
          match
            EConstr.kind
              sigma
              (fst
                 (EConstr.decompose_app
                    sigma
                    (Reductionops.whd_all env sigma ty)))
          with
          | Ind (ind, _) -> Bool.not (Rocqlib.check_ind_ref "core.eq.type" ind)
          | _ -> false
        in
        if
          Premise_search.is_prop env sigma ty
          && mentions_vars
          && inductive_non_eq
        then return open_premise_grade
        else return 0
    ;;

    (* Closed LTS steps already decided by [closed_step_refuted], by term. *)
    module ConstrTbl = Hashtbl.Make (struct
        type t = Constr.t

        let equal = Constr.equal
        let hash = Constr.hash
      end)

    (** {!closed_step_refuted}'s memo. *)
    let refuted_steps : bool ConstrTbl.t = ConstrTbl.create 64

    (** [closed_step_refuted env sigma ty] is whether [ty] is a closed LTS
        step that the bounded search refutes, i.e. a transition that does
        not exist. Raises nothing.

        Inverting a
        constructor such as a handshake ([p -!n-> p'], [q -?n-> q'] gives
        [p | q -tau-> p' | q']) splits into one branch per way it could have
        been derived, and the impossible ones carry such steps, fully closed
        (e.g. [step S0 (Some (Out s0)) S0] for a sender that only inputs).
        [invertibility]'s shape-based grade gives a closed step 0, so the
        branch was never closed, and the search went on to look its
        transition up in the model, which (not existing) it is not:
        [CannotGetTransition]. Found 2026-10-02 on the CCS Alternating Bit
        Protocol, whose one [step] relation is used at every layer; the
        [Proc]/[CADP] examples use a relation per layer. Asked only then, by
        [Hyps.refutable_step] -- a proof search per closed hypothesis is too
        dear to run at every step -- and memoised. *)
    let closed_step_refuted (env : Environ.env) (sigma : Evd.evar_map) ty : bool
      =
      match EConstr.to_constr_opt sigma ty with
      | None -> false
      | Some c ->
        (match ConstrTbl.find_opt refuted_steps c with
         | Some r -> r
         | None ->
           let r =
             match Premise_search.prove env sigma ty with
             | Premise_search.Refuted -> true
             | Premise_search.Proved _ | Premise_search.Unknown -> false
           in
           ConstrTbl.add refuted_steps c r;
           r)
    ;;

    (** [is_dead x] is whether the LTS step [x] has no instance whatever its
        local variables are ({!Premise_search.dead}). Raises nothing.

        Inverting such a step can only open
        branches that are all refuted later, one layer at a time. On the CCS
        Alternating Bit Protocol 61% of all LTS inversions were of such
        steps -- a sender asked for an output it does not make, a medium for
        a handshake on a name it does not use. Asked only of the step
        [try_invert_any] has already chosen to invert: asking of every
        hypothesis at every step was too slow (2026-10-02, PR #11). *)
    let is_dead (x : t) : bool mm =
      let open Syntax in
      let* env = get_env in
      let* sigma = get_sigma in
      let ty = Context.Named.Declaration.get_type x in
      return (Premise_search.dead env sigma ty)
    ;;

    (** [invertibility x] is [x]'s grade: how much it needs inverting,
        higher first, and [0] for not at all. A premise -- not atomic, an
        equation, or not a step of either FSM's LTSs -- is graded by
        {!premise_grade}. An LTS step [lts term label goto] gets 2 if
        [goto] mentions a variable, plus 1 if [label] does: at most 3, below
        both premise grades. Raises nothing. *)
    let invertibility (x : t) : int mm =
      Logger.trace __FUNCTION__;
      let open Syntax in
      let* sigma = get_sigma in
      (* Not of the form [I args] (a negation [~ P], say): never an LTS step. *)
      match Rocq_utils.hyp_to_atomic sigma x with
      | exception Rocq_utils.Rocq_utils_HypIsNot_Atomic _ -> premise_grade x
      | ty, tys ->
        (* An equation is never a transition to invert: it comes from a
           constructor's equation premise (backlog I2). The grading below
           assumes an LTS step's [term label goto] and would score [a = a] (both
           sides a local variable) 3, and inverting it changes nothing, so the
           solver picked it forever. Substitutable equations are handled by the
           [subst] after each step. *)
        let is_eq : bool =
          match EConstr.kind sigma ty with
          | Ind (ind, _) -> Rocqlib.check_ind_ref "core.eq.type" ind
          | _ -> false
        in
        (* Only LTS steps are inverted. Any other premise hypothesis -- an
           [In], a [<=], an equation -- comes from a constructor premise; the
           shape-based grading below assumes [term label goto] and could pick
           it, and inverting it (e.g. [In], a fixpoint) is meaningless. *)
        let is_lts : bool = is_lts_of_either_fsm ty in
        if is_eq || Bool.not is_lts
        then premise_grade x
        else (
          (* NOTE: returns true if can be inverted *)
          let rec f (x : EConstr.t) : bool =
            match EConstr.kind sigma x with
            | Var _ -> EConstr.isRef sigma x
            | App (_, tys) -> Array.exists f tys
            | _ -> false
          in
          (* NOTE: since [2] is the goto-state and [1] is the label, [g] allows
             us to clearly see which hyp needs to be inverted first. *)
          let g (i : int) : int =
            try if f tys.(i) then i else 0 with
            (* NOTE: handles "Index out of bounds" for accessing [tys] array. *)
            | Invalid_argument _ -> 0
          in
          g 2 + g 1 |> return)
    ;;

    (** [invert x] is the tactic inverting [x] ({!Tacs.inversion}). *)
    let invert (x : t) : Tactic.t mm = Tacs.inversion x

    (** [try_unfold_any x] is the tactic unfolding, in [x], the constants
        of its type's head ({!Tacs.try_unfold_any}), or, if there are none,
        of each of its arguments in turn; [None] if nothing unfolds, or [x]'s
        type is not atomic. Raises nothing: the [try] is inside the
        computation, so it catches {!Rocq_utils.hyp_to_atomic}'s exception
        when run. *)
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

    (** Raised by {!get_transition}: [hyp] is not a transition of [fsm]. *)
    exception
      CouldNotGetTransition of
        { hyp : t
        ; fsm : Model.FSM.t
        }

    (** [get_transition ?lts x m] is the hypothesis [x] read as a transition
        of [m]: its source, label and target, looked up in [m]. With [lts],
        only a step of that relation is read: see {!Hyps.get_transition}.

        @raise CouldNotGetTransition
          if [x] is not a step of one of [m]'s LTSs (of [lts], if given), or
          its states, label or transition are not in [m] (raised here).
        @raise Not_found
          as {!ReModel.label}, for a [None]/[Some] label outside [m]'s
          alphabet (propagated). *)
    let get_transition ?(lts : EConstr.t option) (x : t) (m : Model.FSM.t)
      : Model.Transition.t mm
      =
      Logger.trace __FUNCTION__;
      let open Syntax in
      let* ty, tys = to_atomic x in
      let of_lts : bool =
        match lts with None -> true | Some l -> run (econstr_eq ty l)
      in
      if of_lts && Theory.is_fsm_constructor ty m
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
        | ReModel.CouldNotFind_State _ ->
          Logger.trace ~__FUNCTION__ "Err: ReModel.CouldNotFind_State";
          raise (CouldNotGetTransition { hyp = x; fsm = m })
        | ReModel.CouldNotFind_Label _ ->
          Logger.trace ~__FUNCTION__ "Err: ReModel.CouldNotFind_Label";
          raise (CouldNotGetTransition { hyp = x; fsm = m })
        | ReModel.CouldNotFind_Transition _ ->
          Logger.trace ~__FUNCTION__ "Err: ReModel.CouldNotFind_Transition";
          raise (CouldNotGetTransition { hyp = x; fsm = m }))
      else (
        Logger.trace ~__FUNCTION__ "(else)";
        raise (CouldNotGetTransition { hyp = x; fsm = m }))
    ;;
  end

  (** The conclusion of the goal in focus: what kind of goal it is, and
      what it says. *)
  module Concl = struct
    (** [eq x] is whether the conclusion is [x] (syntactically, after
        normalising both). Raises nothing. *)
    let eq (x : EConstr.t) : bool = get_concl () |> econstr_eq x |> run

    (** [eq_hyp x] is whether the conclusion is [x]'s type. Raises
        nothing. *)
    let eq_hyp (x : Rocq_utils.hyp) : bool =
      Context.Named.Declaration.get_type x |> eq
    ;;

    (** [eq_any_hyps hs] is the first of [hs] whose type is the conclusion,
        if any -- the hypothesis, rather than whether one exists: the caller
        closes the goal with it directly. Raises nothing. *)
    let rec eq_any_hyps : Rocq_utils.hyp list -> Rocq_utils.hyp option mm =
      function
      | [] -> return None
      | h :: tl -> if eq_hyp h then return (Some h) else eq_any_hyps tl
    ;;

    (** [is_weak_refl ()] is whether the conclusion relates two equal
        states over one LTS: [weak_sim] or [weak_bisimilar] with arguments
        3 and 4 (the LTSs) equal and 5 and 6 (the states) equal.

        @raise Invalid_argument
          when run, if the conclusion has fewer than seven arguments
          (propagated). Also raises as {!to_atomic} (propagated). *)
    let is_weak_refl () : bool mm =
      let open Syntax in
      let* ty, tys = get_concl () |> to_atomic in
      if econstr_eq tys.(3) tys.(4) |> run
      then econstr_eq tys.(5) tys.(6)
      else return false
    ;;

    (** [is_weak_sim ()] is whether the conclusion is a [weak_sim]. Raises
        nothing. *)
    let is_weak_sim () : bool mm = get_concl () |> Theory.is_weak_sim

    (** [is_weak_bisimilar ()] is whether the conclusion is a
        [weak_bisimilar]. Raises nothing. *)
    let is_weak_bisimilar () : bool mm =
      get_concl () |> Theory.is_weak_bisimilar
    ;;

    (** [is_weak_goal ()] is whether the conclusion is one of the
        coinductive goals the solver proves: [weak_sim], or
        [weak_bisimilar]. Raises nothing. *)
    let is_weak_goal () : bool mm =
      let open Syntax in
      let* sim = is_weak_sim () in
      if sim then return true else is_weak_bisimilar ()
    ;;

    (** [is_exists ()] is whether the conclusion is an [exists]: the
        answer to a move, still to choose. Raises nothing. *)
    let is_exists () : bool mm = get_concl () |> Theory.is_exists

    (** [is_tau ()] is whether the conclusion is a [tau] step. Raises
        nothing. *)
    let is_tau () : bool mm = get_concl () |> Theory.is_tau

    (** [is_premise ()] is whether the conclusion is a constructor premise
        that is neither an LTS step nor one of the solver's own goals: a
        negation, a bounded universal, or a [Prop] headed by an inductive
        that is not [eq] (see {!is_eq}), not a MeBi theory constant, not one
        of either FSM's LTSs, and not a [clos_*] relation (backlog I2, stage
        1). Raises nothing. *)
    let is_premise () : bool mm =
      let open Syntax in
      let* sigma = get_sigma in
      let* env = get_env in
      let concl = get_concl () in
      let h, _ = EConstr.decompose_app sigma concl in
      (* A premise headed by a definition -- [n < 3] is [lt], which unfolds to
         [le (S n) 3] -- is classified by what it unfolds to. Judged on [lt]
         itself it was not a premise, so after [go : n < 3 -> succ_rel n m -> st n a m] was applied the solver took [0 < 3] for the silent-step goal it
         finishes with [rt1n_refl] (found 2026-10-02, [Test.v]
         [InversionShapes.Computed]). The plugin's own definitions keep their
         head: the theory checks below need it. *)
      let h =
        if EConstr.isConst sigma h && Bool.not (Theory.is_any_theory h)
        then
          fst
            (EConstr.decompose_app sigma (Reductionops.whd_all env sigma concl))
        else h
      in
      let is_negation : bool =
        match EConstr.kind sigma (Reductionops.whd_all env sigma concl) with
        | Prod (_, _, b) ->
          (match EConstr.kind sigma (Reductionops.whd_all env sigma b) with
           | Ind (ind, _) -> Rocqlib.check_ind_ref "core.False.type" ind
           | _ -> false)
        | _ -> false
      in
      (* [forall k, k < n -> P k], [n] a numeral: proved by cases *)
      let is_bounded : bool =
        Premise_search.is_bounded_universal env sigma concl
      in
      return
        (is_negation
         || is_bounded
         ||
         match EConstr.kind sigma h with
         | Ind ((mind, _), _) ->
           let name = Names.Id.to_string (Names.MutInd.label mind) in
           Premise_search.is_prop env sigma concl
           && Bool.not (String.starts_with ~prefix:"clos_" name)
           && Bool.not (Theory.is_any_theory h)
           && Bool.not (is_lts_of_either_fsm h)
         | _ -> false)
    ;;

    (** [is_eq ()] is whether the conclusion is an equation [_ = _]: an
        equation premise of a constructor just applied (backlog I2). Raises
        nothing. *)
    let is_eq () : bool mm =
      let open Syntax in
      let* sigma = get_sigma in
      return
        (match EConstr.kind sigma (get_concl ()) with
         | App (h, a) when Array.length a = 3 ->
           (match EConstr.kind sigma h with
            | Ind (ind, _) -> Rocqlib.check_ind_ref "core.eq.type" ind
            | _ -> false)
         | _ -> false)
    ;;

    (** [try_unfold_any ()] is {!Hyp.try_unfold_any} for the conclusion:
        the tactic unfolding the constants of its head, or else of each of
        its arguments in turn; [None] if nothing unfolds.

        @raise Rocq_utils.Rocq_utils_EConstrIsNot_Atomic
          when run, if the conclusion is not atomic (propagated from
          {!to_atomic}). *)
    let try_unfold_any () : Tactic.t option mm =
      Logger.trace __FUNCTION__;
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

    (** The two conjuncts of an [exists] conclusion: the answering system's
        weak transition, and the [weak_sim] or [weak_bisimilar] it must
        reach. *)
    type wk_conj =
      { wk_trans : EConstr.t
      ; wk_sim : EConstr.t
      }

    (** The states those conjuncts name: where the moving system went
        ([a']), and where the answering one starts ([b]). *)
    type conj =
      { a' : Model.State.t
      ; b : Model.State.t
      }

    (** [get_a'_from_wk_sim wk_sim] is FSM a's state that [wk_sim] relates,
        where the moving system went: the left argument of the relation, or
        the right one when the roles are swapped ([bisim_r]).

        @raise ReModel.CouldNotFind_State
          when run, if that term is not one of FSM a's states (propagated). *)
    let get_a'_from_wk_sim (wk_sim : EConstr.t) : Model.State.t mm =
      let open Syntax in
      let* _, tys = to_atomic wk_sim in
      let i : int = if !W.swapped then 6 else 5 in
      (W.get_fsm_a ()).states |> ReModel.state tys.(i) |> M.run |> return
    ;;

    (** [get_b_from_wk_trans wk_trans] is FSM b's state that the weak
        transition [wk_trans] starts from.

        @raise ReModel.CouldNotFind_State
          when run, if that term is not one of FSM b's states (propagated). *)
    let get_b_from_wk_trans (wk_trans : EConstr.t) : Model.State.t mm =
      let open Syntax in
      let* _, tys = to_atomic wk_trans in
      (W.get_fsm_b ()).states |> ReModel.state tys.(3) |> M.run |> return
    ;;

    (** Raised by {!get_wk_conj}: the [exists]' body is not a conjunction
        of two. *)
    exception ConclDoesNotMatchConj

    (** [get_wk_conj ()] is the two conjuncts of the [exists] conclusion.

        @raise Theories.EnsureFail
          when run, if the conclusion is not an [exists] (propagated).
        @raise ConclDoesNotMatchConj
          when run, if its body is not a conjunction of two (raised here). *)
    let get_wk_conj () : wk_conj mm =
      Logger.trace __FUNCTION__;
      let open Syntax in
      let* ty, tys = get_concl () |> to_atomic in
      let* () = Theory.ensure ty Theory.is_exists in
      let* _, _, x = to_lambda tys.(1) in
      let* _, app_tys = to_app x in
      match Array.to_list app_tys with
      | [ wk_trans; wk_sim ] -> return { wk_trans; wk_sim }
      | _ -> raise ConclDoesNotMatchConj
    ;;

    (** [lts_a ()] is the relation of the system being simulated, read from the
        [weak_sim] or [weak_bisimilar] conjunct of an [exists] conclusion:
        [@weak_sim M N A ltsM ltsN m n] gives [ltsM], or [ltsN] when swapped,
        as {!get_a'_from_wk_sim} reads [m] or [n]. [None] for any other
        conclusion. Raises nothing. *)
    let lts_a () : EConstr.t option mm =
      let open Syntax in
      match run (get_wk_conj ()) with
      | exception e when CErrors.noncritical e -> return None
      | { wk_sim; _ } ->
        let* _, tys = to_atomic wk_sim in
        let i : int = if !W.swapped then 4 else 3 in
        return (if i < Array.length tys then Some tys.(i) else None)
    ;;

    (** [get_conj c] is the states [c]'s conjuncts name.

        @raise Theories.EnsureFail
          when run, if [c.wk_sim] is neither [weak_sim] nor [weak_bisimilar]
          (propagated). Also raises as {!get_a'_from_wk_sim} and
          {!get_b_from_wk_trans} (propagated). *)
    let get_conj ({ wk_trans; wk_sim } : wk_conj) : conj mm =
      Logger.trace __FUNCTION__;
      let open Syntax in
      let* bisim : bool = Theory.is_weak_bisimilar wk_sim in
      let* () =
        if bisim then return () else Theory.ensure wk_sim Theory.is_weak_sim
      in
      let* a' = get_a'_from_wk_sim wk_sim in
      let* b = get_b_from_wk_trans wk_trans in
      return { a'; b }
    ;;

    (** [orientation ()] reads from the goal whether the two systems' roles
        are swapped ([Some true]), not swapped ([Some false]), or whether the
        goal does not say ([None], e.g. mid-way through applying constructors,
        where the last answer stands).

        After [Pack_bisim] and [intros], [bisim_l]'s goal is
        [exists n2, weak ltsN n1 n2 a /\ weak_bisimilar m2 n2] and
        [bisim_r]'s is [exists m2, weak ltsM m1 m2 a /\ weak_bisimilar m2 n2]:
        the witness sits on the left of the relation exactly when the
        {e right} system moved.

        Raises as {!get_wk_conj} (propagated; {!handle_state} reads any
        failure as [None]). *)
    let orientation () : bool option mm =
      let open Syntax in
      let* goal = is_weak_goal () in
      if goal
      then return (Some false)
      else
        let* ex = is_exists () in
        if Bool.not ex
        then return None
        else
          let* { wk_sim; _ } = get_wk_conj () in
          let* bisim = Theory.is_weak_bisimilar wk_sim in
          if Bool.not bisim
          then return (Some false)
          else
            let* sigma = get_sigma in
            let* _, tys = to_atomic wk_sim in
            return (Some (EConstr.isRelN sigma 1 tys.(5)))
    ;;
  end

  (** All the hypotheses of the goal in focus: which to invert or unfold,
      and which is the move to answer. *)
  module Hyps = struct
    (** [get_cofixes ()] is the coinduction hypotheses
        ({!get_all_cofix_hyp_names}), sorted by name. Raises nothing. *)
    let get_cofixes () : Rocq_utils.hyp list =
      let cofix_names : Names.Id.Set.t = get_all_cofix_hyp_names () in
      get_hyps ()
      |> List.filter (fun (x : Rocq_utils.hyp) ->
        Names.Id.Set.mem (get_hyp_name x) cofix_names)
      |> List.sort Hyp.compare_name
    ;;

    (** [get_non_cofixes ()] is the non-coinduction hypotheses, {b oldest
        first}: the order they were introduced in.

        [try_invert_any] breaks ties toward the {e later} candidate, meaning
        the newest hypothesis, the one the last inversion produced. Until
        2026-10-02 this list was sorted by name, which is a string order
        ([H79 < H8 < H80]), and Rocq reuses freed names, so past about ten
        hypotheses "later" was not "newest": the solver could re-pick a
        hypothesis it had already inverted and, on the CCS Alternating Bit
        Protocol, did so forever ([inversion H8], [inversion H80], ...).
        Ordering by introduction removed that loop and saved 4-10% of steps
        on [Proc/Test3] and CADP, with no proof worse (backlog Step 0, note 7;
        [ASSISTED-CHANGES.md], 2026-10-02). [Proofview.Goal.hyps] is newest
        first, hence the reversal. Raises nothing. *)
    let get_non_cofixes () : Rocq_utils.hyp list =
      let cofix_names : Names.Id.Set.t = get_all_non_cofix_hyp_names () in
      get_hyps ()
      |> List.filter (fun (x : Rocq_utils.hyp) ->
        Names.Id.Set.mem (get_hyp_name x) cofix_names)
      |> List.rev
    ;;

    (** [log ?cofix_only ()] logs the hypotheses at [Debug]: all of them,
        only the coinduction hypotheses ([Some true]), or only the others
        ([Some false]). *)
    let log ?(cofix_only : bool option = None) () : unit =
      match cofix_only with
      | None -> log_hyps ()
      | Some true ->
        Logger.things Debug "hyps (cofixes)" (get_cofixes ()) Strfy.hyp
      | Some false ->
        Logger.things Debug "hyps (non-cofixes)" (get_non_cofixes ()) Strfy.hyp
    ;;

    (** [can_solve_concl_cofix ()] is the coinduction hypothesis in scope whose
        type is the current goal, if there is one. It used to answer only
        whether such a hypothesis existed, leaving [trivial] to find it again
        by hint search. Raises nothing. *)
    let can_solve_concl_cofix () : Rocq_utils.hyp option mm =
      get_cofixes () |> Concl.eq_any_hyps
    ;;

    (** [clear_non_cofix ()] is the tactic clearing every hypothesis that is
        not a coinduction hypothesis ({!get_all_non_cofix_hyp_names}), as a
        new cofix is introduced ({!handle_new_cofix}). Whether this is
        needed, or could be a problem, is not checked (TODO). Raises
        nothing. *)
    let clear_non_cofix () : Tactic.t =
      Tactic.create
        ~msg:"(Clearing non-cofix Hyps)"
        (Tactics.clear
           (Names.Id.Set.to_seq (get_all_non_cofix_hyp_names ()) |> List.of_seq))
    ;;

    (** [try_invert_any ()] is the tactic for the non-coinduction
        hypothesis that most needs inverting ({!Hyp.invertibility}; ties go
        to the later one, see below), or [None] if every grade is 0: it
        refutes a refutable premise ({!Tacs.refute_premise}), inverts an
        open one ({!Tacs.invert_premise}), refutes a dead LTS step
        ({!Tacs.refute_dead}), and inverts any other step. Raises nothing. *)
    let try_invert_any () : Tactic.t option mm =
      Logger.trace __FUNCTION__;
      let hyps : Hyp.t list = get_non_cofixes () in
      let open Syntax in
      let f (i : int) (xopt : (int * Hyp.t) option) : (int * Hyp.t) option mm =
        let y : Hyp.t = List.nth hyps i in
        let* grade : int = Hyp.invertibility y in
        Logger.debug
          ~__FUNCTION__
          (Printf.sprintf "grade %i : %s" grade (Hyp.name_to_string y));
        match xopt with
        | None -> Some (grade, y) |> return
        | Some (n, x) ->
          (* Ties on [grade] go to the LATER hypothesis, and that is load
             bearing. [invertibility] grades on shape -- whether the label and
             goto positions hold a local variable -- so in a layered LTS a
             whole chain of transitions grades identically:

             H  : compLTS (cpar (cprc X) R) a (cpar (cprc Y) R)
             H1 : termLTS X a Y
             H4 : compLTS (cprc X) a (cprc Y)

             all score 3, this keeps the last and so picks [H4], whose
             inversion yields [termLTS X a Y] -- which is [H1] again. The goal
             does not move and the context gains a duplicate; the next step
             makes progress only because that duplicate sorts last and gets
             picked instead. Steps whose goal is unchanged and whose only new
             hypothesis is an exact duplicate were measured at 2.6% (Test1),
             2.7-4.3% (Test2), 4.9% and 14.1% (CADP/Size1) and 12.0%
             (Proc/Test3) of all steps.

             Breaking ties toward the SMALLER hypothesis instead -- the
             innermost transition, the one whose inversion actually determines
             the label and destination -- was tried on 2026-09-29 and
             REVERTED. It left 16 of the 18 baseline counts byte-identical,
             but turned [wsim_lts] and [wsim_lts_bigstep], both 396
             iterations, into proofs that had not closed after 5000
             iterations, 10 minutes and 1.4GB. Which hypothesis gets inverted
             steers the whole downstream path and the search has no plan to
             fall back on, so a local improvement here can flip a proof from
             converging to diverging. Do not change this tie-break without
             running all five cheap suites. See ASSISTED-CHANGES.md,
             2026-09-29.

             Retried 2026-10-01 gated on the mutual block ([ac98c3c]) and
             reverted the same day: the mutual block does NOT make it safe.
             With [MutualCofix True] forced, CADP's [wsim_lts_bigstep] went
             from 396 to not closing. Run the suites with the strategy forced
             both ways, not just under [Auto]. See ASSISTED-CHANGES.md,
             2026-10-01. *)
          (match Int.compare grade n with
           | -1 -> Some (n, x) |> return
           | _ -> Some (grade, y) |> return)
      in
      let* to_invert_opt = iterate 0 (List.length hyps - 1) None f in
      match to_invert_opt with
      | None -> return None
      | Some (0, x) -> return None
      (* NOTE: we only want to invert hyps with non-zero grades. *)
      | Some (grade, x) when Int.equal grade Hyp.refutable_grade ->
        let* y = Tacs.refute_premise x in
        return (Some y)
      | Some (grade, x) when Int.equal grade Hyp.open_premise_grade ->
        let* y = Tacs.invert_premise x in
        return (Some y)
      | Some (grade, x) ->
        (* An LTS step with no instance closes the goal by refutation: one
           step instead of the inversions of every layer beneath it
           (note 11, option D′). *)
        let* dead = Hyp.is_dead x in
        let* y = if dead then Tacs.refute_dead x else Hyp.invert x in
        return (Some y)
    ;;

    (** [try_unfold_any ()] is the tactic unfolding what can be unfolded
        in every non-coinduction hypothesis ({!Hyp.try_unfold_any}), in
        sequence; [None] if nothing unfolds. Raises nothing. *)
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

    (** [term_size sigma t] is the number of nodes in [t]: a smaller
        refutable step needs fewer inversions to refute. Raises nothing. *)
    let rec term_size (sigma : Evd.evar_map) (t : EConstr.t) : int =
      EConstr.fold sigma (fun n c -> n + term_size sigma c) 1 t
    ;;

    (** [refutable_step ()] is the smallest closed LTS-step hypothesis that
        cannot hold ({!Hyp.closed_step_refuted}), if any: in a branch that
        inversion opened for a derivation that does not exist, refuting it
        closes the branch. The smallest, because refuting a step inverts it
        down to the impossible part, and a large one ([res s0 (res s1 ...)])
        can need more inversions than the refutation's depth allows. Raises
        nothing. *)
    let refutable_step () : Rocq_utils.hyp option mm =
      Logger.trace __FUNCTION__;
      let open Syntax in
      let* env = get_env in
      let* sigma = get_sigma in
      let is_step (h : Rocq_utils.hyp) : bool =
        match Rocq_utils.hyp_to_atomic sigma h with
        | exception _ -> false
        | ty, _ -> is_lts_of_either_fsm ty
      in
      get_non_cofixes ()
      |> List.filter is_step
      |> List.map (fun h ->
        term_size sigma (Context.Named.Declaration.get_type h), h)
      |> List.sort (fun (a, _) (b, _) -> Int.compare a b)
      |> List.find_opt (fun (_, h) ->
        Hyp.closed_step_refuted env sigma (Context.Named.Declaration.get_type h))
      |> Stdlib.Option.map snd
      |> return
    ;;

    (** Raised by {!get_transition}: no hypothesis reads as a transition of
        the FSM. *)
    exception CannotGetTransition of Model.FSM.t

    (** [get_transition ?lts m] is the first hypothesis that reads as a
        transition of [m]. Pass [lts], the relation being simulated: [m]'s
        relations include every one in [Using], and a premise's step left by
        inversion ([rb 2 a 3] under [open_rec 0 a 3]) can name states of [m]
        too. Read as a transition of [m] it was the wrong one, or, from a
        state with no edges, an uncaught [Not_found] (2026-10-03).

        @raise CannotGetTransition
          when run, if none does (raised here).
          Also raises as {!Hyp.get_transition}, other than
          {!Hyp.CouldNotGetTransition} (propagated). *)
    let get_transition ?(lts : EConstr.t option) (m : Model.FSM.t)
      : Model.Transition.t mm
      =
      Logger.trace __FUNCTION__;
      let hyps = get_non_cofixes () in
      let open Syntax in
      let f (i : int)
        : Model.Transition.t option -> Model.Transition.t option mm
        = function
        | Some x -> return (Some x)
        | None ->
          let y = List.nth hyps i in
          (try
             let y : Model.Transition.t = Hyp.get_transition ?lts y m |> run in
             return (Some y)
           with
           | Hyp.CouldNotGetTransition _ -> return None)
      in
      let* x = iterate 0 (List.length hyps - 1) None f in
      match x with None -> raise (CannotGetTransition m) | Some x -> return x
    ;;
  end

  (** [constructors_of_goal ()] is the tactics applying the constructor and
      record of the coinductive goal in focus: [In_sim] and [Pack_sim] for
      [weak_sim], [In_bisim] and [Pack_bisim] for [weak_bisimilar]. Raises
      nothing. *)
  let constructors_of_goal () : (Tactic.t * Tactic.t) mm =
    let open Syntax in
    let* bisim = Concl.is_weak_bisimilar () in
    if bisim
    then
      let* i = Tacs.apply_In_bisim () in
      let* p = Tacs.apply_Pack_bisim () in
      return (i, p)
    else
      let* i = Tacs.apply_In_sim () in
      let* p = Tacs.apply_Pack_sim () in
      return (i, p)
  ;;

  (** [handle_open_block ()] is the tactic for the [OpenBlock] state: it
      opens the whole proof with a single mutual cofixpoint, one definition
      per pair of the precomputed product relation
      ({!Model.Product.reachable}, or the answer plan's), then applies the
      goal's constructors in every goal at once. If anything in the
      conclusion unfolds, that comes first, as this step. The next state is
      [WeakSim].

      This is the alternative to minting a fresh nested cofix each time the
      search meets a pair it has not seen. A nested cofix is visible only to
      the branch that created it and its descendants, so a pair that repeats a
      *sibling* rather than an *ancestor* cannot be closed and its whole
      subtree is re-derived; the search then enumerates simple paths through
      the product rather than its states. See backlog item B2.

      Taken under [MeBi Config Solver MutualCofix True], or when [Auto]
      chooses it ({!Proof_solver.init}).

      @raise ReModel.CouldNotFind_State
        if the goal's states are not the two FSMs' (propagated). Also raises
        as {!Concl.try_unfold_any} (propagated). *)
  let handle_open_block () : Tactic.t mm =
    Logger.trace __FUNCTION__;
    let open Syntax in
    (* The block's types come from decoded model states, and [Concl.eq] is
       syntactic, so the goal has to be normalised before its own pair can be
       resolved -- and before any of the block's hypotheses could match it. *)
    let* unfold_opt = Concl.try_unfold_any () in
    match unfold_opt with
    | Some x ->
      Logger.trace ~__FUNCTION__ "unfold before opening the block";
      return x
    | None ->
      let* ty, tys = get_concl () |> to_atomic in
      let fsm_a : Model.FSM.t = W.get_fsm_a () in
      let fsm_b : Model.FSM.t = W.get_fsm_b ~saturated:true () in
      (* The unsaturated FSM still has the silent steps a silent move may be
         answered with; see [Model.Product.respond]. *)
      let silent : Model.EdgeMap.t' = (W.get_fsm_b ()).edges in
      let pi : Model.Partition.t = W.get_bisimilar_partition () in
      (* [weak_sim] applies as [| M; N; A; ltsM; ltsN; s; t |] -- see
         [Concl.is_weak_refl], which tests 3 against 4 and 5 against 6. Only
         the last two move; reusing the rest keeps the implicit and universe
         arguments exactly as the goal has them. *)
      let root : Model.Product.Pair.t =
        ( M.run (ReModel.state tys.(5) fsm_a.states)
        , M.run (ReModel.state tys.(6) fsm_b.states) )
      in
      (* Same test as [Concl.is_weak_refl]: a pair of equal states only closes
         by [weak_sim_refl] when both sides use the same LTS. *)
      let refl : bool = econstr_eq tys.(3) tys.(4) |> run in
      let* bisim : bool = Concl.is_weak_bisimilar () in
      let pairs : Model.Product.Pair.Set.t =
        match !W.plan with
        | Some p -> p.relation
        | None ->
          if bisim
          then
            Model.Product.reachable_bisim
              ~refl
              { a = fsm_a
              ; a_saturated = W.get_fsm_a ~saturated:true ()
              ; b = W.get_fsm_b ()
              ; b_saturated = fsm_b
              }
              pi
              root
          else
            Model.Product.reachable
              ~silent
              ?sim:!W.simulators
              ~refl
              fsm_a
              fsm_b
              pi
              root
      in
      (* A reflexive leaf gets no cofixpoint of its own. Its goal would be put
         through [In_sim; Pack_sim; intros] with the rest of the block, past the
         point where [handle_weaksim] can close it by [weak_sim_refl] -- and
         [reachable] did not follow its successors, so the search would then
         stop with a pair-not-in-product error. Left out, every goal that
         reaches it is still a bare [weak_sim x x] and closes by reflexivity. *)
      let others : Model.Product.Pair.t list =
        Model.Product.Pair.Set.remove root pairs
        |> Model.Product.Pair.Set.filter (fun ((a, b) : Model.Product.Pair.t) ->
          not (refl && Model.State.equal a b))
        |> Model.Product.Pair.Set.elements
      in
      let type_of ((a, b) : Model.Product.Pair.t) : EConstr.t =
        EConstr.mkApp
          ( ty
          , Array.mapi
              (fun (i : int) (x : EConstr.t) ->
                match i with
                | 5 -> Decode.state a
                | 6 -> Decode.state b
                | _ -> x)
              tys )
      in
      (* All the names at once: [new_cofix_name] measures against the current
         goal, which does not change until the tactic runs. *)
      let used : Names.Id.Set.t ref = ref (get_hyp_names ()) in
      let fresh () : Names.Id.t =
        let n : Names.Id.t =
          Namegen.next_ident_away (Names.Id.of_string "Cofix0") !used
        in
        used := Names.Id.Set.add n !used;
        n
      in
      let root_name : Names.Id.t = fresh () in
      let block : (Names.Id.t * EConstr.t) list =
        List.map (fun p -> fresh (), type_of p) others
      in
      Logger.notice
        (Printf.sprintf "(Mutual cofix over %i pairs.)" (1 + List.length block));
      let* cofix : Tactic.t = Tacs.mutual_cofix root_name block in
      let* apply_In_sim, apply_Pack_sim = constructors_of_goal () in
      let* intros_all : Tactic.t = Tacs.intros_all () in
      (* One tactic, not two. Straight after [mutual_cofix] every goal in the
         block is syntactically identical to its own hypothesis, so a
         [handle_weaksim] that ran in between would close each of them with an
         unguarded [exact], and [Qed] would reject the proof. *)
      let setup : Tactic.t =
        Tacs.all_goals
          (Tactic.chain [ apply_In_sim; apply_Pack_sim; intros_all ])
      in
      ProofState.update_statem WeakSim;
      Tactic.seq cofix setup |> return
  ;;

  (** [handle_new_cofix ()] is the tactic introducing a new coinduction
      hypothesis (a nested [cofix]): the cofix, clearing the other
      hypotheses, the goal's constructor and record
      ({!constructors_of_goal}), and [intros]. Raises nothing.

      Callers must have normalised the conclusion first: [handle_weaksim] runs
      [Concl.try_unfold_any] to exhaustion before reaching here, so a cofix is
      always minted from a fully unfolded goal. *)
  let handle_new_cofix () : Tactic.t mm =
    Logger.trace __FUNCTION__;
    let open Syntax in
    let* cofix : Tactic.t = Tacs.cofix () in
    let clear : Tactic.t = Hyps.clear_non_cofix () in
    let* apply_In, apply_Pack = constructors_of_goal () in
    let* intros_all : Tactic.t = Tacs.intros_all () in
    (* [intros] runs on every goal [Pack] leaves: one for [weak_sim], two
       ([bisim_l], [bisim_r]) for [weak_bisimilar]. *)
    Tactic.chain [ cofix; clear; apply_In; apply_Pack; intros_all ] |> return
  ;;

  (** Raised by {!ensure_matching_states}: the two states differ. *)
  exception MisMatchedStates of (Model.State.t * Model.State.t)

  (** [ensure_matching_states x y] does nothing if [x] and [y] are the same
      state.

      @raise MisMatchedStates otherwise (raised here). *)
  let ensure_matching_states (x : Model.State.t) (y : Model.State.t) : unit =
    Logger.trace __FUNCTION__;
    if Model.State.equal x y then () else raise (MisMatchedStates (x, y))
  ;;

  (** Raised by {!handle_wk_concl}: FSM b has no answer from [b]. *)
  exception
    CouldNotGetGoalTransition of
      { b : Model.State.t
      ; wk_trans : EConstr.t
      }

  (** [handle_wk_concl hyp conj] answers the move [hyp] (FSM "a" went to
      [a']) from FSM "b"'s state [b], as {!Model.Product.answer} decides --
      the same function the mutual block's product is computed with
      ([Model.Product.successors]), on the same inputs: [b], the move's label
      and its target. Standing still introduces [b] and leaves the reflexive
      weak step to [handle_exists]; a transition introduces its target and
      hands its annotation to [ApplyConstructors].

      Until 2026-10-02 this decision was written out here a second time
      (a stay check, then [try_get_visible_transition] re-resolving [b] and
      the label from the goal, then the simulators fallback), kept in step
      with [Product.successors] by hand.

      @raise MisMatchedStates
        if the move's target is not the conclusion's [a'], or the answer
        does not start from [b] (propagated).
      @raise CouldNotGetGoalTransition
        if there is no answer (raised here).
        Also raises as {!Concl.get_conj} (propagated). *)
  let handle_wk_concl
        (hyp : Model.Transition.t)
        ({ wk_trans; wk_sim } : Concl.wk_conj)
    : Tactic.t mm
    =
    Logger.trace __FUNCTION__;
    let open Syntax in
    let* { a'; b } = Concl.get_conj { wk_trans; wk_sim } in
    ensure_matching_states hyp.goto a';
    let move_by_move () : Model.Product.answer option =
      Model.Product.answer
        ~silent:(W.get_fsm_b ()).edges
        ?sim:!W.simulators
        (W.get_fsm_b ~saturated:true ())
        (W.get_bisimilar_partition ())
        b
        hyp.label
        hyp.goto
    in
    (* With an answer plan ([MeBi Config Solver Answers], not [Default]),
       the plan's answer to this very move; a move it does not reach -- it
       should reach every one the search meets -- is answered move by move,
       and said so. *)
    let answer : Model.Product.answer option =
      match !W.plan with
      | None -> move_by_move ()
      | Some p ->
        (match
           Model.Product.Policy.choose
             p
             { swapped = !W.swapped
             ; mover = hyp.from
             ; answerer = b
             ; label = hyp.label
             ; target = hyp.goto
             }
         with
         | Some a -> Some a
         | None ->
           Logger.notice
             "(Answers: a move outside the plan; answered move by move.)";
           move_by_move ())
    in
    match answer with
    | Some Model.Product.Stay ->
      Logger.trace ~__FUNCTION__ "stay";
      Tacs.ex_intro_split b
    | Some (Model.Product.Move goal) ->
      Model.Transition.log ~__FUNCTION__ ~s:"goal" goal;
      ensure_matching_states goal.from b;
      ProofState.update_statem
        (ApplyConstructors (ProofState.ApplicableConstructors.init goal));
      Tacs.ex_intro_split goal.goto
    | None -> raise (CouldNotGetGoalTransition { b; wk_trans })
  ;;

  (** [handle_hyp_transition ()] is the tactic for an [exists] goal whose
      move is not known yet: it reads the move from the hypotheses
      ({!Hyps.get_transition}, a step of the relation being simulated),
      records it ([Exists (Some hyp)]), and unfolds the two conjuncts if
      anything in them unfolds, or else answers the move
      ({!handle_wk_concl}). When no hypothesis reads as a transition but a
      step hypothesis is refutable ({!Hyps.refutable_step}), it refutes
      that one, closing the branch, and goes back to [WeakSim].

      @raise Hyps.CannotGetTransition
        if neither (raised here). Also raises as {!handle_wk_concl}
        (propagated). *)
  let handle_hyp_transition () : Tactic.t mm =
    Logger.trace __FUNCTION__;
    let open Syntax in
    let* lts = Concl.lts_a () in
    match run (Hyps.get_transition ?lts (W.get_fsm_a ())) with
    | exception (Hyps.CannotGetTransition _ as e) ->
      (* No hypothesis is a transition of the model. In a branch inversion
         opened for a derivation that does not exist, some step hypothesis
         cannot hold: refute it and the branch closes. Back to [WeakSim],
         which handles whatever goal comes next. Otherwise, a real failure. *)
      let* h = Hyps.refutable_step () in
      (match h with
       | Some h ->
         ProofState.update_statem WeakSim;
         Tacs.refute_premise h
       | None -> raise e)
    | hyp ->
      Model.Transition.log ~__FUNCTION__ ~s:"hyp" hyp;
      ProofState.update_statem (Exists (Some hyp));
      let* { wk_trans; wk_sim } = Concl.get_wk_conj () in
      Logger.trace ~__FUNCTION__ "wk_trans; wk_sim";
      let* unfold_opt = Tacs.try_unfold_any_of [ wk_trans; wk_sim ] in
      (match unfold_opt with
       | Some x -> return x
       | None -> handle_wk_concl hyp { wk_trans; wk_sim })
  ;;

  (** [handle_appconstrs_entry_point label] is the tactic starting the
      answer's weak step labelled [label]: [wk_none] for a silent label,
      [wk_some] for a visible one, then [unfold silent]. Raises nothing. *)
  let handle_appconstrs_entry_point (label : Model.Label.t) : Tactic.t mm =
    Logger.trace __FUNCTION__;
    let open Syntax in
    let* constructor =
      if Model.Label.is_silent label
      then Tacs.apply_wk_none ()
      else Tacs.eapply_wk_some ()
    in
    let unfold_silent = Tacs.unfold_silent () in
    Tactic.seq constructor unfold_silent |> return
  ;;

  (** [handle_appconstrs_stop ()] is the tactic ending the answer: [simpl]
      and [subst] everywhere, then [rt1n_refl] for the empty rest of its
      silent path. Raises nothing. *)
  let handle_appconstrs_stop () : Tactic.t mm =
    Logger.trace __FUNCTION__;
    let open Syntax in
    let* simplify = Tacs.simplify_and_subst_all () in
    let* refl = Tacs.eapply_rt1n_refl () in
    Tactic.seq simplify refl |> return
  ;;

  (** [handle_appconstrs_update_args a] is the constructors of the first
      step of the annotation [a] (its least derivation tree, in preorder)
      and the rest of [a]. Raises nothing. *)
  let handle_appconstrs_update_args ({ this; next } : Model.Annotation.t)
    : Enc.Tree.Node.t list option * Model.Annotation.t option
    =
    Logger.trace __FUNCTION__;
    (* [preorder], not the old shortest-child [minimize]: a node's children
       are its premises, all required. See [Tree.preorder] and
       [theories/Test.v]'s [TwoPremises]. *)
    Some (Enc.Trees.min this.using |> Enc.Tree.preorder), next
  ;;

  (** [handle_appconstrs_update label] is the tactic taking the answer's
      next step: [rt1n_trans] via [label] ({!Tacs.eapply_rt1n_via}), after
      unfolding the conclusion if anything in it unfolds.

      @raise Rocq_utils.Rocq_utils_EConstrIsNot_Atomic
        as {!Concl.try_unfold_any} (propagated). *)
  let handle_appconstrs_update (label : Model.Label.t) : Tactic.t mm =
    Logger.trace __FUNCTION__;
    let open Syntax in
    let* rt1n = Tacs.eapply_rt1n_via label in
    let* unfold = Concl.try_unfold_any () in
    match unfold with
    | None -> return rt1n
    | Some unfold -> Tactic.seq unfold rt1n |> return
  ;;

  (** [handle_appconstrs_apply ?goto x] is the tactic applying the
      constructor [x] to the goal, an LTS step, with the bindings extraction
      recorded for it ({!Tacs.apply_constructor}): the step's source, the
      label [None] for a [tau] goal, and the target [goto], when known.

      @raise CErrors.UserError
        if the goal is not an LTS step (raised here, in place of
        {!Tacs.GoalNotAnLTSStep}, which [apply_constructor] raises while
        building, so the handler is reached). *)
  let handle_appconstrs_apply
        ?(goto : Model.State.t option = None)
        (x : Enc.Tree.Node.t)
    : Tactic.t mm
    =
    Logger.trace __FUNCTION__;
    let open Syntax in
    let* _, tys = get_concl () |> to_atomic in
    let tys = Array.map (fun x -> econstr_normalize x |> run) tys in
    let* is_tau = Concl.is_tau () in
    (* NOTE: we can't rely on the terms in [tys] being encoded since they may be
       from an intermediate layer of the LTS. *)
    let args : Tacs.binding_args =
      if is_tau
      then (* NOTE: index (3) since [tau lts x] => [tau (term * label) x] *)
        { from = tys.(3); goto = None; label = Some (Mebi_theories.get "None") }
      else
        (* The step's target, when known, is bound too: a constructor whose
           target is computed by a premise ([succ_rel n m -> st n a m])
           leaves that premise open otherwise, and nothing later fixes it. *)
        { from = tys.(0); goto = Option.map Decode.state goto; label = None }
    in
    try Tacs.apply_constructor x args with
    | Tacs.GoalNotAnLTSStep ->
      (* A premise the solver has no step for -- e.g. an [eq] (backlog I2)
         -- reached the focus before this constructor's goal. A user error,
         not an uncaught exception (which Rocq reports as its own anomaly). *)
      CErrors.user_err
        (Pp.str
           (Printf.sprintf
              "MeBi: cannot continue the proof: the next constructor to apply \
               is for an LTS, but the focused goal is not one of its steps:\n\
              \  %s\n\
               This happens when a constructor has a premise MeBi does not \
               handle (such as an equation); extraction warns about these when \
               it builds the LTS."
              (Strfy.econstr (get_concl ()))))
  ;;

  (***********************************************************************)

  (** Raised by {!handle_new_proof}: nothing to unfold; on to the next
      state at once. *)
  exception SkipNewProof

  (** Raised by {!handle_weaksim}: nothing to invert or unfold; on to
      [Exists] at once. *)
  exception ExitWeakSim

  (** Raised by {!handle_weaksim}: the proof is finished. *)
  exception ProofComplete

  (** [handle_new_proof (a, b)] is the tactic for the [NewProof] state:
      unfolding the two systems' terms [a] and [b]. The next state is
      [OpenBlock] under a mutual cofix, else [WeakSim].

      @raise SkipNewProof if neither unfolds (raised here). *)
  let handle_new_proof
        ((a, b) : Constrexpr.constr_expr * Constrexpr.constr_expr)
    : Tactic.t mm
    =
    Logger.trace __FUNCTION__;
    let x = Tacs.unfold_opt_constrexpr_list [ a; b ] in
    match x with
    | None -> raise SkipNewProof
    | Some x ->
      ProofState.update_statem
        (if !Api.the_mutual_cofix then OpenBlock else WeakSim);
      return x
  ;;

  (** [handle_weaksim ()] is the tactic for the [WeakSim] state. On a
      [weak_sim] or [weak_bisimilar] goal: reflexivity for two equal states
      over one LTS; otherwise unfolding the conclusion if anything unfolds,
      then closing it by the coinduction hypothesis that is the goal, or
      introducing one ({!handle_new_cofix}). On any other goal: inverting
      the hypothesis that most needs it ({!Hyps.try_invert_any}), or
      unfolding one.

      @raise ProofComplete if the proof is finished (raised here).
      @raise ExitWeakSim if there is nothing to invert or unfold (raised
                         here).
      @raise CErrors.UserError
        under a mutual cofix, for a pair outside the block (raised here). *)
  let handle_weaksim () : Tactic.t mm =
    Logger.trace __FUNCTION__;
    let open Syntax in
    let* is_weak_sim : bool = Concl.is_weak_goal () in
    if is_weak_sim
    then (
      Logger.trace ~__FUNCTION__ "is weak sim";
      let* _, tys = get_concl () |> to_atomic in
      let* is_weak_refl : bool = Concl.is_weak_refl () in
      if is_weak_refl
      then (
        Logger.trace ~__FUNCTION__ "is weak refl";
        let* bisim = Concl.is_weak_bisimilar () in
        if bisim
        then Tacs.apply_weak_bisimilar_refl ()
        else Tacs.apply_weak_sim_refl ())
      else
        (* Normalise the conclusion BEFORE consulting the coinduction
           hypotheses. The unfolding used to live inside [handle_new_cofix],
           which was harmless while every hypothesis was minted from whatever
           the goal happened to look like -- a nested [cofix] copies the goal,
           spelling included. It stops being harmless as soon as the
           hypotheses are built ahead of time from decoded model states, since
           [Concl.eq] is syntactic and would not match a goal still written in
           terms of definitions. Unfolding first makes both sides normal. *)
        let* unfold_opt : Tactic.t option = Concl.try_unfold_any () in
        match unfold_opt with
        | Some x ->
          Logger.trace ~__FUNCTION__ "unfold before cofix lookup";
          return x
        | None ->
          let* hyp_cofix : Rocq_utils.hyp option =
            Hyps.can_solve_concl_cofix ()
          in
          (match hyp_cofix with
           | Some h -> Tacs.exact_hyp h
           | None ->
             if !Api.the_mutual_cofix
             then
               (* Every pair the search can reach is supposed to be in the
                  block. Reaching one that is not means the product computed
                  up front disagrees with what the solver actually does --
                  name the pair rather than leaving a stuck goal. A user
                  error, not an uncaught exception: Rocq reports the latter
                  as an anomaly in Rocq itself. *)
               CErrors.user_err
                 (Pp.str
                    (Printf.sprintf
                       "MeBi: reached a weak_sim goal for a pair outside the \
                        mutual cofix block computed up front, so the proof \
                        cannot close it:\n\
                       \  %s\n\
                       \  %s\n\
                        This is a bug in the plugin's product computation \
                        (Model.Product). [MeBi Config Solver MutualCofix \
                        False] avoids the mutual block."
                       (Strfy.econstr tys.(5))
                       (Strfy.econstr tys.(6))))
             else handle_new_cofix ()))
    else if ProofState.is_done ()
    then raise ProofComplete
    else
      (* NOTE: try invert any that need to be inverted *)
      let* invert_opt = Hyps.try_invert_any () in
      match invert_opt with
      | Some x -> return x
      | None ->
        Logger.trace ~__FUNCTION__ "no hyps to invert";
        (* NOTE: check if we need to unfold anything in the inverted hyps. *)
        let* unfold_opt = Hyps.try_unfold_any () in
        (match unfold_opt with
         | Some x -> return x
         | None ->
           Logger.trace ~__FUNCTION__ "no terms to unfold";
           raise ExitWeakSim)
  ;;

  (** [handle_exists hyp_opt] is the tactic for the [Exists] state. On an
      [exists] goal it reads the move ({!handle_hyp_transition}), or, with
      the move known ([hyp_opt]), answers it ({!handle_wk_concl}). Any
      other goal finishes a silent answer by reflexivity, and the next state
      is [WeakSim].

      Raises as {!handle_hyp_transition} and {!handle_wk_concl}
      (propagated). *)
  let handle_exists (hyp_opt : Model.Transition.t option) : Tactic.t mm =
    Logger.trace __FUNCTION__;
    let open Syntax in
    let* is_exists : bool = Concl.is_exists () in
    if is_exists
    then (
      match hyp_opt with
      | None -> handle_hyp_transition ()
      | Some hyp ->
        Logger.trace ~__FUNCTION__ "Some hyp";
        let open Syntax in
        let* { wk_trans; wk_sim } = Concl.get_wk_conj () in
        handle_wk_concl hyp { wk_trans; wk_sim })
    else (
      (* NOTE: assume we need to finish handling a silent action. *)
      Logger.trace ~__FUNCTION__ "not exists, do_refl";
      ProofState.update_statem WeakSim;
      Tacs.do_refl ())
  ;;

  (** [handle_apply_constructors args] is the tactic for the
      [ApplyConstructors args] state. A constructor premise in focus is
      proved by the bounded search, and an equation by reflexivity, leaving
      [args] as it is. Otherwise it takes the next constructor of [args]:
      the answer's entry point, the next step of its annotation, the next
      constructor of the current step, or, with nothing left, the end of
      the answer (and the next state is [WeakSim]).

      @raise CErrors.UserError
        if a premise cannot be proved (raised here). Also raises as
        {!handle_appconstrs_apply} (propagated). *)
  let handle_apply_constructors (args : ProofState.ApplicableConstructors.t)
    : Tactic.t mm
    =
    Logger.trace __FUNCTION__;
    ProofState.ApplicableConstructors.log ~__FUNCTION__ ~s:"args" args;
    let open Syntax in
    let* is_eq = Concl.is_eq () in
    let* is_premise = if is_eq then return false else Concl.is_premise () in
    if is_premise
    then
      (* Any other premise: extraction kept this constructor because the
         bounded search proved the premise, so the same search yields a proof
         term for it here (backlog I2, stage 1). *)
      let* env = get_env in
      let* sigma = get_sigma in
      let concl = get_concl () in
      match Premise_search.prove env sigma concl with
      | Premise_search.Proved (Premise_search.Term p) -> Tacs.exact_term p
      | Premise_search.Proved (Premise_search.ByRefutation _) ->
        Tacs.prove_negation ()
      | Premise_search.Proved Premise_search.ByCases -> Tacs.prove_bounded ()
      | Premise_search.Refuted | Premise_search.Unknown ->
        CErrors.user_err
          (Pp.str
             (Printf.sprintf
                "MeBi: cannot prove the constructor premise\n\
                \  %s\n\
                 It is not closed, or not decidable by MeBi's bounded search \
                 (see [MeBi Help Premises])."
                (Strfy.econstr (get_concl ()))))
    else if is_eq
    then
      (* An equation premise has the focus. Extraction only keeps a
         constructor whose equation premises it decided hold, i.e. whose sides
         are convertible, so [reflexivity] closes it; the constructor list is
         left as it is, for the goal that gets the focus next (backlog I2). *)
      Tacs.reflexivity ()
    else (
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
             (ApplyConstructors
                { args with
                  current
                ; remaining
                ; step_goto = Some anno.this.goto
                });
           handle_appconstrs_update anno.this.label)
      | { current = Some (h :: tl); step_goto; _ } ->
        (* NOTE: continue applying constructors; only the step's first (top
           level) constructor gets its target bound *)
        ProofState.update_statem
          (ApplyConstructors { args with current = Some tl; step_goto = None });
        handle_appconstrs_apply ~goto:step_goto h)
  ;;

  (** [handle_state ()] is the tactic for the proof's state, after reading
      from the goal which system moves ({!Concl.orientation}).

      @raise NothingToDo
        if the state is [Done] (raised here). Also raises
        as each state's handler (propagated). *)
  let handle_state () : Tactic.t mm =
    ProofState.log ~__FUNCTION__ ();
    Hyps.log ~cofix_only:(Some false) ();
    log_concl ();
    (* Which system moves in the goal in focus (see [Concl.orientation]). A
       goal that does not say -- an LTS step or a premise, part-way through
       an answer -- keeps the last orientation set. *)
    (match try run (Concl.orientation ()) with _ -> None with
     | Some s -> W.swapped := s
     | None -> ());
    match ProofState.get_statem () with
    | NewProof ab -> handle_new_proof ab
    | OpenBlock -> handle_open_block ()
    | WeakSim -> handle_weaksim ()
    | Exists hyp_opt -> handle_exists hyp_opt
    | ApplyConstructors xs -> handle_apply_constructors xs
    | Done -> raise NothingToDo
  ;;

  (* See the [.mli]. [ProofComplete], [SkipNewProof] and [ExitWeakSim] move
     the state machine on: the first ends the proof with an empty tactic,
     the other two take the next state's step at once. An encoding missing
     from either table is logged, as both tables see it, and re-raised. *)
  let rec step () : Tactic.t =
    Logger.trace __FUNCTION__;
    try run (handle_state ()) with
    | ProofComplete ->
      Logger.trace ~__FUNCTION__ "_:ProofComplete => Done";
      ProofState.update_statem Done;
      Tactic.create ~msg:"Proof Complete" (Proofview.tclUNIT ())
    | SkipNewProof ->
      Logger.trace ~__FUNCTION__ "NewProof:SkipNewProof => WeakSim";
      ProofState.update_statem
        (if !Api.the_mutual_cofix then OpenBlock else WeakSim);
      step ()
    | ExitWeakSim ->
      Logger.trace ~__FUNCTION__ "WeakSim:ExitWeakSim => Exists";
      ProofState.update_statem (Exists None);
      step ()
    (********************)
    | M.EncodingNotFound x ->
      Logger.thing Warning "M.EncodingNotFound" x M.Strfy.econstr;
      Logger.thing Warning "(using P) EConstr" x Strfy.econstr;
      Logger.thing
        Warning
        "(using P) is encoded"
        (encoded x)
        (Printf.sprintf "%b");
      raise (M.EncodingNotFound x)
    | EncodingNotFound x ->
      Logger.thing Warning "(M).EncodingNotFound" x Strfy.econstr;
      Logger.thing Warning "(using M) EConstr" x M.Strfy.econstr;
      Logger.thing
        Warning
        "(using P) is encoded"
        (M.encoded x)
        (Printf.sprintf "%b");
      raise (M.EncodingNotFound x)
  ;;
end
