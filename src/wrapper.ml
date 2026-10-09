module type S = sig
  type enc
  type node
  type tree
  type trees

  module M : Rocq_monad_utils.S with type enc = enc and type tree = tree
  module Bindings : Bindings.S with type 'a mm = 'a M.mm

  module ConstructorBindings :
    Constructor_bindings.S
    with type 'a mm = 'a M.mm
     and type ind = M.Ind.t
     and type instructions = Bindings.Instructions.t
     and type bindings = Bindings.t
     and type constrmap = Bindings.ConstrMap.t'

  module Model :
    Model.S
    with type base = enc
     and type tree = tree
     and type trees = trees
     and type constructorbindings = ConstructorBindings.t

  module Decode :
    Decoder.S
    with type enc = enc
     and type state = Model.State.t
     and type states = Model.State.Set.t
     and type partition = Model.Partition.t
     and type label = Model.Label.t
     and type labels = Model.Label.Set.t
     and type note = Model.Note.t
     and type annotation = Model.Annotation.t
     and type annotations = Model.Annotation.Set.t
     and type transition = Model.Transition.t
     and type transitions = Model.Transition.Set.t
     and type action = Model.Action.t
     and type actions = Model.Action.Set.t
     and type actionmap = Model.Action.Map.t'
     and type edgemap = Model.EdgeMap.t'
     and type rocqlts = Model.Info.Meta.RocqLTS.t
     and type info = Model.Info.t
     and type lts = Model.LTS.t
     and type fsm = Model.FSM.t
     and type result = Model.Bisimilarity.Result.t
     and type bisimilarity = Model.Bisimilarity.t

  module Theory :
    Theories_enc.S
    with type enc = enc
     and type 'a mm = 'a M.mm
     and type 'a im = 'a M.mm

  module Weak : Weak.S with type enc = enc

  module Config :
    Config_loader.S with type weak = Weak.t and type 'a mm = 'a M.mm

  val result_log
    :  ?decode:bool
    -> (module Json.S with type k = 'a)
    -> (module Json.S with type k = 'a)
    -> (module Json.S with type k = 'a)

  val handle_results
    :  Output.Kind.t
    -> string
    -> 'a
    -> (module Json.S with type k = 'a)
    -> unit

  val extract_lts
    :  Libnames.qualid
    -> Constrexpr.constr_expr
    -> Libnames.qualid list
    -> Weak.t option
    -> Model.LTS.t M.mm

  val similarity : Model.Bisimilarity.t -> Model.Product.Pair.Set.t option

  module Command : sig
    val build_lts
      :  ?weak:Weak.t option
      -> Libnames.qualid
      -> Constrexpr.constr_expr
      -> Libnames.qualid list
      -> Model.LTS.t M.mm

    val build_fsm
      :  ?weak:Weak.t option
      -> Libnames.qualid
      -> Constrexpr.constr_expr
      -> Libnames.qualid list
      -> Model.FSM.t M.mm

    type t =
      | MakeLTS of rocq_args
      | MakeFSM of rocq_args
      | Saturate of rocq_args
      | Minimize of rocq_args
      | Merge of rocq_pair
      | CheckBisim of rocq_pair
      | CheckSim of rocq_pair
      | BenchmarkGraph of (rocq_args * (int * int))

    and rocq_args = Constrexpr.constr_expr * Libnames.qualid

    and rocq_pair =
      { a : rocq_args
      ; b : rocq_args
      }

    val do_make_lts
      :  Constrexpr.constr_expr * Libnames.qualid
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    val do_make_fsm
      :  Constrexpr.constr_expr * Libnames.qualid
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    val do_saturate
      :  Constrexpr.constr_expr * Libnames.qualid
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    val do_minimize
      :  Constrexpr.constr_expr * Libnames.qualid
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    val build_fsms
      :  rocq_args
      -> rocq_args
      -> Libnames.qualid list
      -> (Model.FSM.t * Model.FSM.t) M.mm

    val do_merge
      :  rocq_pair
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    val do_check_bisim
      :  rocq_pair
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    val do_check_sim
      :  rocq_pair
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    val do_benchmark_graph
      :  rocq_args * (int * int)
      -> Libnames.qualid list
      -> Decode.bisimilarity option M.mm

    val run : Libnames.qualid list -> t -> Model.Bisimilarity.t option M.mm
  end
end

module Make (Enc : Encoding.S) :
  S
  with type enc = Enc.t
   and type node = Enc.Tree.Node.t
   and type tree = Enc.Tree.t
   and type trees = Enc.Trees.t = struct
  type enc = Enc.t
  type node = Enc.Tree.Node.t
  type tree = Enc.Tree.t
  type trees = Enc.Trees.t

  module Benchmarking = Benchmarking.Make

  (* NOTE: to stop message spam when debugging, wrap the noisy call in
     [Logger.quiet (fun () -> ...)]. This used to require re-instantiating the
     whole module tree with a differently-configured logger (Logger.ReMake),
     which is why the attempt that lived here was left commented out. *)

  (* See the [.mli]. *)
  module M = Rocq_monad_utils.Make (Enc)

  (* See the [.mli]. *)
  module Bindings = Bindings.Make (M)

  (* See the [.mli]. *)
  module ConstructorBindings = Constructor_bindings.Make (M) (Bindings)

  (* See the [.mli]. *)
  module Model = Model.Make (Enc) (ConstructorBindings)
  module LTS = Model.LTS
  module FSM = Model.FSM

  (* See the [.mli]. *)
  module Decode = Decoder.Make (Enc) (M) (ConstructorBindings) (Model)

  (* See the [.mli]. *)
  module Theory = Theories_enc.Make (Enc) (M) (M) (Theories.Make (Enc) (M))

  (* See the [.mli]. *)
  module Weak = Weak.Make (Enc) (M)

  (* See the [.mli]. *)
  module Config = Config_loader.Make (Enc) (M) (Weak)

  (* See the [.mli]. *)
  let result_log
        ?(decode : bool = true)
        (type a)
        (module FEnc : Json.S with type k = a)
        (module FDec : Json.S with type k = a)
    : (module Json.S with type k = a)
    =
    let module E : Json.S with type k = a =
      (val if decode && !Api.the_output_config.decode_results
           then (module FDec : Json.S with type k = a)
           else (module FEnc : Json.S with type k = a))
    in
    (module E : Json.S with type k = a)
  ;;

  (* See the [.mli]. *)
  let handle_results
        (type a)
        (m : Output.Kind.t)
        (s : string)
        (x : a)
        (module FLog : Json.S with type k = a)
    : unit
    =
    FLog.log ~m ~s x;
    match m with
    | Result -> if !Api.the_output_config.dump_results then FLog.write s x
    | _ -> ()
  ;;

  (** [bound_to_string b] is the exploration bound [b] in words ("100
      states"; both, for a merged model). Raises nothing. *)
  let rec bound_to_string : Model.Info.Meta.Bounds.t -> string = function
    | States n -> Printf.sprintf "%i states" n
    | Transitions n -> Printf.sprintf "%i transitions" n
    | Merged (a, b) ->
      Printf.sprintf "%s and %s" (bound_to_string a) (bound_to_string b)
  ;;

  (** [cut_short_reason b (states, transitions)] is the reason an LTS cut
      short by the bound [b], with [states] and [transitions] found, is
      incomplete, and how to raise the bound. Raises nothing. *)
  let cut_short_reason
        (bounds : Model.Info.Meta.Bounds.t)
        ((states, transitions) : int * int)
    : string
    =
    Printf.sprintf
      "exploration stopped at the bound of %s, with %i states and %i \
       transitions found and more still unexplored. Raise the bound with [MeBi \
       Config Bounds As Num States <n>] (or [... Num Transitions <n>]). A \
       large LTS can still be too big to saturate."
      (bound_to_string bounds)
      states
      transitions
  ;;

  (** [approximation_reason xs (states, transitions)] is the reason an LTS
      with [states] and [transitions] is only an approximation, naming the
      first three of the approximations [xs] and counting the rest; [None]
      if there are none. Raises nothing. *)
  let approximation_reason
        (approximations : string list)
        ((states, transitions) : int * int)
    : string option
    =
    match approximations with
    | [] -> None
    | xs ->
      Some
        (Printf.sprintf
           "the extracted LTS (%i states, %i transitions) is only an \
            approximation: %s. A [MeBi Run Bisim] verdict on it may be wrong \
            (a proof cannot be: [Qed] checks every step). See the warnings \
            above and [MeBi Help Premises]."
           states
           transitions
           (let shown = List.filteri (fun i _ -> i < 3) xs in
            let more = List.length xs - List.length shown in
            String.concat "; " shown
            ^ if more > 0 then Printf.sprintf "; and %i more" more else ""))
  ;;

  (** [check_if_lts_fail ?approximations ?cut_short x] does nothing unless
      the [FailIf] flags make [x] an error: an empty LTS (one state at most,
      no transitions) with [FailIf Empty], or an incomplete one with [FailIf Incomplete]. The error says why: exploration was [cut_short] by the
      bound (the default), or [approximations] (see
      {!Rocq_monad_utils.Approximations}) made [x] approximate, or both.

      @raise Rocq_monad_utils.S.Errors.MEBI_exn
        for an empty or incomplete LTS under those flags (raised here, via
        {!Rocq_monad_utils.S.Err}). *)
  let check_if_lts_fail
        ?(approximations : string list = [])
        ?(cut_short : bool = true)
        (x : LTS.t)
    : unit
    =
    if
      !Api.the_fail_flags.empty
      && (Int.equal (Model.State.Set.cardinal x.states) 1
          || Model.State.Set.is_empty x.states)
      && Model.Transition.Set.is_empty x.transitions
    then (
      Logger.trace ~__FUNCTION__ "LTS Empty";
      M.Err.lts_empty ())
    else if !Api.the_fail_flags.incomplete
    then (
      match x with
      | { info = { meta = Some { is_complete = false; bounds; _ }; _ }; _ } ->
        result_log (module Model.LTS) (module Decode.LTS)
        |> handle_results Result "LTS Incomplete" x;
        let states = Model.State.Set.cardinal x.states
        and transitions = Model.Transition.Set.cardinal x.transitions in
        let cut_short : string option =
          if List.is_empty approximations || cut_short
          then Some (cut_short_reason bounds (states, transitions))
          else None
        in
        M.Err.lts_incomplete
          (String.concat
             " Also, "
             (List.filter_map
                Fun.id
                [ cut_short
                ; approximation_reason approximations (states, transitions)
                ])
           ^ " Accept it anyway with [MeBi Config FailIf Incomplete False].")
      | _ -> ())
    else ()
  ;;

  (** [memory_range n] is the memory [n] weak actions take, as a range from
      {!Api.bytes_per_weak_action} ("450MB--900MB"). Raises nothing. *)
  let memory_range (n : int) : string =
    let lo, hi = Api.bytes_per_weak_action in
    Printf.sprintf
      "%s--%s"
      (Api.human_bytes (n * lo))
      (Api.human_bytes (n * hi))
  ;;

  (** [estimate name x] is how many weak actions saturating [x] would
      produce ({!Model.SaturationEstimate.fsm}), after logging it at [Info].
      Raises nothing. *)
  let estimate (name : string) (x : FSM.t) : Model.SaturationEstimate.t =
    let e : Model.SaturationEstimate.t = Model.SaturationEstimate.fsm x in
    Logger.info
      (Printf.sprintf
         "Saturating %s: %s."
         name
         (Model.SaturationEstimate.to_string e));
    e
  ;;

  (** [check_saturation_size name x] guards a saturation: it estimates,
      without saturating, how many weak actions saturating [x] would produce
      ({!Model.SaturationEstimate}), and refuses past
      {!Api.the_saturation_bound} -- or, with [FailIf Oversaturated False],
      warns. Saturating [Proc/Test4] (74.6M weak actions) exhausted a 15GB
      machine; this says so up front. Does nothing for an FSM with no silent
      labels.

      @raise Rocq_monad_utils.S.Errors.MEBI_exn
        if saturation would pass the bound and [FailIf Oversaturated] is set
        (raised here). *)
  let check_saturation_size (name : string) (x : FSM.t) : unit =
    if Model.FSM.is_weak_mode x
    then (
      let e : Model.SaturationEstimate.t = estimate name x in
      let bound : int = !Api.the_saturation_bound in
      let lo, hi = Api.bytes_per_weak_action in
      if e.weak > bound
      then (
        let msg : string =
          Printf.sprintf
            "saturating %s would produce %s, above the bound of %i. That needs \
             about %s of memory (measured %i--%i bytes per weak action), and \
             can take a long time. Raise the bound with [MeBi Config Bounds \
             Saturation <n>] if your machine has the memory, or carry on with \
             only a warning with [MeBi Config FailIf Oversaturated False]."
            name
            (Model.SaturationEstimate.to_string e)
            bound
            (memory_range e.weak)
            lo
            hi
        in
        if !Api.the_fail_flags.oversaturated
        then M.Err.saturation_too_large msg
        else Logger.warning (String.capitalize_ascii msg)))
  ;;

  (** [warn_on_demand name e above] warns that the FSM [name], whose
      saturation [e] would take, is saturated on demand instead: because it
      is [above] the saturation bound, or because [MeBi Config Saturation OnDemand True] asks for it. Raises nothing.
  *)
  let warn_on_demand
        (name : string)
        (e : Model.SaturationEstimate.t)
        (above : bool)
    : unit
    =
    let bound : int = !Api.the_saturation_bound in
    Logger.warning
      (Printf.sprintf
         "%s would saturate to %s%s. Instead, MeBi saturates each state only \
          when it is needed, holding at most %i weak actions (about %s) and \
          saturating again any it had to drop, and decides bisimilarity on the \
          %i silent SCCs rather than on the saturated FSM. The verdict is the \
          same; a proof may take longer. [MeBi Config Saturation OnDemand \
          False] refuses such FSMs instead. See [MeBi Help Config Saturation]."
         (String.capitalize_ascii name)
         (Model.SaturationEstimate.to_string e)
         (if above
          then
            Printf.sprintf
              ", above the bound of %i (about %s of memory)"
              bound
              (memory_range e.weak)
          else " (on demand by [MeBi Config Saturation OnDemand True])")
         bound
         (memory_range bound)
         e.sccs)
  ;;

  (** [on_demand_for name x] is whether the bisimilarity check saturates [x]
      on demand ({!Api.the_saturation_mode}; notes/13): under [Auto], when
      saturating it whole would pass the saturation bound -- which used to be
      an error -- and then it warns; under [On_demand], always; under
      [Whole], never, with {!check_saturation_size}'s guard instead.

      @raise Rocq_monad_utils.S.Errors.MEBI_exn
        as {!check_saturation_size}, under [Whole]. *)
  let on_demand_for (name : string) (x : FSM.t) : bool =
    if Bool.not (Model.FSM.is_weak_mode x)
    then false
    else (
      match !Api.the_saturation_mode with
      | Api.Saturation_whole ->
        check_saturation_size name x;
        false
      | (Api.Saturation_auto | Api.Saturation_on_demand) as mode ->
        let e : Model.SaturationEstimate.t = estimate name x in
        let above : bool = e.weak > !Api.the_saturation_bound in
        if above || mode = Api.Saturation_on_demand
        then (
          warn_on_demand name e above;
          true)
        else false)
  ;;

  (** [make_graph_args ()] is the tables and bounds a graph is explored
      with: fresh tables keyed by encoding, and the bounds now configured.
      Raises nothing. *)
  let make_graph_args ()
    : (module Graph_type.Args with type enc = Enc.t and type tree = Enc.Tree.t)
    =
    (module struct
      type enc = Enc.t
      type tree = Enc.Tree.t

      module T : Hashtbl.S with type key = Enc.t = (val M.make_enc_hashtbl ())
      module S : Set.S with type elt = Enc.t = (val M.make_enc_set ())

      module D : Set.S with type elt = Enc.t * Enc.Tree.t =
        (val M.make_state_tree_pair_set ())

      let bounds : Api.bounds_args = !Config.the_bounds_args
    end : Graph_type.Args
      with type enc = Enc.t
       and type tree = Enc.Tree.t)
  ;;

  (** The graph explorer over the tables [X]. *)
  module G
      (X : Graph_type.Args with type enc = Enc.t and type tree = Enc.Tree.t) =
    Graph.Make (Enc) (M) (Weak) (Theory) (ConstructorBindings) (Model) (X)

  (** [mark_approximate approximations x] is [x] marked incomplete if there
      are [approximations] (an approximate LTS is incomplete, like one cut
      short by the bound), else [x]. Raises nothing. *)
  let mark_approximate (approximations : string list) (x : Model.LTS.t)
    : Model.LTS.t
    =
    match approximations, x.info.meta with
    | _ :: _, Some meta ->
      { x with
        info = { x.info with meta = Some { meta with is_complete = false } }
      }
    | _ -> x
  ;;

  (* See the [.mli]. *)
  let extract_lts
        (primary_lts : Libnames.qualid)
        (init : Constrexpr.constr_expr)
        (names : Libnames.qualid list)
        (weak : Weak.t option)
    : LTS.t M.mm
    =
    Logger.trace __FUNCTION__;
    let module G = G ((val make_graph_args ())) in
    let grefs = Rocq_utils.libnames_to_globrefs (primary_lts :: names) in
    let open M.Syntax in
    Rocq_monad_utils.Approximations.reset ();
    let* the_graph : G.t = G.build ~weak init primary_lts grefs in
    let* the_lts : Model.LTS.t = G.extract the_graph in
    let approximations = Rocq_monad_utils.Approximations.get () in
    let cut_short : bool =
      match the_lts.info.meta with
      | Some { is_complete; _ } -> Bool.not is_complete
      | None -> false
    in
    let the_lts : Model.LTS.t = mark_approximate approximations the_lts in
    check_if_lts_fail ~approximations ~cut_short the_lts;
    M.return the_lts
  ;;

  (** [refuse_walk why] fails with a user error saying that deciding weak
      similarity walks every reachable pair, which an FSM saturated on
      demand makes costly, and [why] the walk is not allowed (and how to
      allow it).

      @raise CErrors.UserError always (raised here). *)
  let refuse_walk (why : string) : 'a =
    CErrors.user_err
      (Pp.str
         (Printf.sprintf
            "MeBi: deciding weak similarity walks every pair of states \
             reachable from the two start states, and an FSM here is saturated \
             on demand (too large to saturate whole; see the warning above), \
             so that walk saturates state after state as it goes. %s"
            why))
  ;;

  (* See the [.mli]. *)
  let similarity (r : Model.Bisimilarity.t) : Model.Product.Pair.Set.t option =
    Logger.trace __FUNCTION__;
    let a : FSM.t = r.fsm_a.original in
    let b : FSM.t = r.fsm_b.original in
    let b_saturated : FSM.t = r.fsm_b.saturated in
    let on_demand : bool =
      Stdlib.Option.is_some r.fsm_a.saturated.fill
      || Stdlib.Option.is_some b_saturated.fill
    in
    match a.init, b.init with
    | None, _ | _, None -> None
    | Some ra, Some rb ->
      let compute () = Model.Product.simulation a b b_saturated (ra, rb) in
      if Bool.not on_demand
      then Some (compute ())
      else (
        match !Api.the_game_bound with
        | None ->
          refuse_walk
            "Bound it with [MeBi Config Bounds Game <n>] (pairs) to allow it."
        | Some n ->
          (try Some (Model.Product.with_cap n compute) with
           | Model.Product.Game_too_large n ->
             refuse_walk
               (Printf.sprintf
                  "The game has more than %i pairs, the bound set with [MeBi \
                   Config Bounds Game %i]: raise it to allow it."
                  n
                  n)))
  ;;

  module Command = struct
    (* See the [.mli]. *)
    let build_lts
          ?(weak : Weak.t option = None)
          (primary_lts : Libnames.qualid)
          (init : Constrexpr.constr_expr)
          (names : Libnames.qualid list)
      : LTS.t M.mm
      =
      Logger.trace __FUNCTION__;
      Config.get_weak weak |> extract_lts primary_lts init names
    ;;

    (* See the [.mli]. *)
    let build_fsm
          ?(weak : Weak.t option = None)
          (primary_lts : Libnames.qualid)
          (init : Constrexpr.constr_expr)
          (names : Libnames.qualid list)
      : FSM.t M.mm
      =
      Logger.trace __FUNCTION__;
      let open M.Syntax in
      let* the_lts = build_lts ~weak primary_lts init names in
      Model.FSM.of_lts the_lts |> M.return
    ;;

    (* See the [.mli]. *)
    let do_make_lts (x, primary_lts) refs : Model.Bisimilarity.t option M.mm =
      Logger.trace __FUNCTION__;
      let open M.Syntax in
      Logger.info "Extracting LTS...";
      let* the_lts = build_lts primary_lts x refs in
      result_log (module Model.LTS) (module Decode.LTS)
      |> handle_results Result "Finished Extracting LTS" the_lts;
      M.return None
    ;;

    (* See the [.mli]. *)
    let do_make_fsm (x, primary_lts) refs : Model.Bisimilarity.t option M.mm =
      Logger.trace __FUNCTION__;
      Logger.info "Making FSM (from extracted LTS)...";
      let open M.Syntax in
      let* the_fsm = build_fsm primary_lts x refs in
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Result "Finished Making FSM" the_fsm;
      M.return None
    ;;

    (* See the [.mli]. *)
    let do_saturate (x, primary_lts) refs : Model.Bisimilarity.t option M.mm =
      Logger.trace __FUNCTION__;
      Logger.info "Making FSM (from extracted LTS)...";
      let open M.Syntax in
      let* the_fsm = build_fsm primary_lts x refs in
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Info "Finished Making FSM" the_fsm;
      check_saturation_size "the FSM" the_fsm;
      Logger.info "Saturating FSM...";
      let the_fsm = Model.FSM.saturate the_fsm in
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Result "Finished Saturating FSM" the_fsm;
      M.return None
    ;;

    (* See the [.mli]. *)
    let do_minimize (x, primary_lts) refs : Model.Bisimilarity.t option M.mm =
      Logger.trace __FUNCTION__;
      Logger.info "Making FSM (from extracted LTS)...";
      let open M.Syntax in
      let* the_fsm = build_fsm primary_lts x refs in
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Info "Finished Making FSM" the_fsm;
      check_saturation_size "the FSM" the_fsm;
      Logger.info "Minimizing FSM...";
      let { fsm; pi } : Model.Minimization.t = Model.Minimization.fsm the_fsm in
      Decode.Partition.log ~m:Info ~s:"pi" pi;
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Result "Finished Minimizing FSM" the_fsm;
      M.return None
    ;;

    (* See the [.mli]. *)
    type t =
      | MakeLTS of rocq_args
      | MakeFSM of rocq_args
      | Saturate of rocq_args
      | Minimize of rocq_args
      | Merge of rocq_pair
      | CheckBisim of rocq_pair
      | CheckSim of rocq_pair
      | BenchmarkGraph of (rocq_args * (int * int))

    and rocq_args = Constrexpr.constr_expr * Libnames.qualid

    and rocq_pair =
      { a : rocq_args
      ; b : rocq_args
      }

    (* See the [.mli]. *)
    let build_fsms
          ((ax, alts) : rocq_args)
          ((bx, blts) : rocq_args)
          (refs : Libnames.qualid list)
      : (FSM.t * FSM.t) M.mm
      =
      Logger.trace __FUNCTION__;
      Logger.info "Making FSMs...";
      let open M.Syntax in
      Logger.info "Making FSM A...";
      let weak1 : Weak.t option = Config.get_the_weak_arg1 () in
      let* the_fsm_a = build_fsm ~weak:weak1 alts ax refs in
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Info "Finished Making FSM A" the_fsm_a;
      Logger.info "Making FSM B...";
      let weak2 : Weak.t option = Config.get_the_weak_arg2 () in
      let* the_fsm_b = build_fsm ~weak:weak2 blts bx refs in
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Info "Finished Making FSM B" the_fsm_b;
      M.return (the_fsm_a, the_fsm_b)
    ;;

    (** [refuse_conflicts a b] does nothing unless [a] and [b] share a state
        that moves differently on each side ({!Model.Bisimilarity.conflicts}):
        merged, it would be one state, and the verdict could be wrong
        (2026-10-03: [a] and [b] were reported bisimilar). Sharing states
        with the same moves -- both sides using the same relation -- is
        exact, and allowed.

        @raise CErrors.UserError on such a state, when run (raised here). *)
    let refuse_conflicts (the_fsm_a : FSM.t) (the_fsm_b : FSM.t) : unit M.mm =
      let c : Model.State.Set.t =
        Model.Bisimilarity.conflicts the_fsm_a the_fsm_b
      in
      if Model.State.Set.is_empty c
      then M.return ()
      else
        M.state (fun env sigma ->
          let example : string =
            Rocq_utils.Strfy.econstr
              env
              sigma
              (Decode.state (Model.State.Set.min_elt c))
          in
          CErrors.user_err
            (Pp.str
               (Printf.sprintf
                  "MeBi: the two systems share %i state(s), e.g. [%s], that \
                   move differently on each side: two different relations \
                   whose state terms coincide. The bisimilarity check would \
                   take each for one state and could give a wrong verdict, so \
                   it refuses. Number one system's states apart from the \
                   other's."
                  (Model.State.Set.cardinal c)
                  example)))
    ;;

    (** [separate a b] is [b] with its copies of {e all} the states it shares
        with [a] renamed apart, if any of them moves differently on each
        side; [b] itself otherwise. Each gets a fresh encoding that decodes to
        the same term ({!Rocq_monad_utils.S.alias}), so the merge keeps them
        distinct while proofs and output still read the right terms. All,
        not only the conflicting ones: a shared state with the same moves on
        both sides may lead to a conflicting one, and would then differ once
        that one is renamed on one side only. Raises nothing. *)
    let separate (the_fsm_a : FSM.t) (the_fsm_b : FSM.t) : FSM.t =
      let c = Model.Bisimilarity.conflicts the_fsm_a the_fsm_b in
      if Model.State.Set.is_empty c
      then the_fsm_b
      else (
        let shared : Model.State.Set.t =
          Model.State.Set.inter the_fsm_a.states the_fsm_b.states
        in
        let fresh : (Enc.t, Model.State.t) Hashtbl.t =
          Hashtbl.create (Model.State.Set.cardinal shared)
        in
        Model.State.Set.iter
          (fun (s : Model.State.t) ->
            Hashtbl.replace fresh s.base { base = M.alias s.base })
          shared;
        Logger.notice
          (Printf.sprintf
             "(The two systems share %i states, %i of which move differently \
              on each side: two relations whose state terms coincide. The \
              second system's copies are kept apart.)"
             (Model.State.Set.cardinal shared)
             (Model.State.Set.cardinal c));
        Model.FSM.rename
          (fun (s : Model.State.t) ->
            Stdlib.Option.value (Hashtbl.find_opt fresh s.base) ~default:s)
          the_fsm_b)
    ;;

    (* See the [.mli]. *)
    let do_merge { a; b } refs : Model.Bisimilarity.t option M.mm =
      Logger.trace __FUNCTION__;
      let open M.Syntax in
      let* the_fsm_a, the_fsm_b = build_fsms a b refs in
      let the_fsm_b = separate the_fsm_a the_fsm_b in
      let* () = refuse_conflicts the_fsm_a the_fsm_b in
      Logger.info "Merging FSMs...";
      let the_fsm = FSM.merge the_fsm_a the_fsm_b in
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Result "Finished Merging FSMs" the_fsm;
      M.return None
    ;;

    (** [fail_if_not_bisim r] does nothing unless [r] says the two systems are
        not bisimilar and [FailIf NotBisimilar] is set; then it logs [r].

        @raise Rocq_monad_utils.S.Errors.MEBI_exn in that case (raised here). *)
    let fail_if_not_bisim (x : Model.Bisimilarity.Result.t) : unit =
      if !Api.the_fail_flags.non_bisimilar
      then
        if Bool.not (Model.Bisimilarity.Result.are_bisimilar x)
        then (
          result_log (module Model.Bisimilarity.Result) (module Decode.Result)
          |> handle_results Result "Not Bisimilar" x;
          M.Err.not_bisimilar ())
    ;;

    (** [bisimilarity_of {a; b} using] is the weak bisimilarity check of the
        two systems: both FSMs built, [b]'s conflicting states renamed apart,
        each saturated (on demand above the saturation bound), and the FSMs
        and the result logged. No verdict check: see {!do_check_bisim} and
        {!do_check_sim}.

        Raises, when run, as {!build_fsms}, {!refuse_conflicts} and
        {!on_demand_for} (propagated). *)
    let bisimilarity_of { a; b } refs : Model.Bisimilarity.t M.mm =
      Logger.trace __FUNCTION__;
      let open M.Syntax in
      let* the_fsm_a, the_fsm_b = build_fsms a b refs in
      let the_fsm_b = separate the_fsm_a the_fsm_b in
      (* after [separate], a conflict would be a bug in it: still refuse *)
      let* () = refuse_conflicts the_fsm_a the_fsm_b in
      let on_demand : Model.Bisimilarity.on_demand =
        { a = on_demand_for "FSM A" the_fsm_a
        ; b = on_demand_for "FSM B" the_fsm_b
        ; budget = !Api.the_saturation_bound
        ; partition = Model.SaturationEstimate.partition
        }
      in
      Logger.info "Checking Bisimilarity of FSMs...";
      let result = Model.Bisimilarity.fsm ~on_demand the_fsm_a the_fsm_b in
      let r = result_log (module Model.FSM) (module Decode.FSM) in
      r |> handle_results Result "FSM a (original)" result.fsm_a.original;
      r |> handle_results Result "FSM a (saturated)" result.fsm_a.saturated;
      r |> handle_results Result "FSM b (original)" result.fsm_b.original;
      r |> handle_results Result "FSM b (saturated)" result.fsm_b.saturated;
      result_log
        ~decode:false
        (module Model.Bisimilarity.Result)
        (module Decode.Result)
      |> handle_results Result "Finished Merging FSMs" result.result;
      M.return result
    ;;

    (* See the [.mli]. *)
    let do_check_bisim (args : rocq_pair) refs
      : Model.Bisimilarity.t option M.mm
      =
      Logger.trace __FUNCTION__;
      let open M.Syntax in
      let* result = bisimilarity_of args refs in
      fail_if_not_bisim result.result;
      M.return (Some result)
    ;;

    (** [user_term (t, _)] is the term [t] as the user wrote it, printed,
        for a verdict. Raises, when run, Rocq's errors if [t] is ill-formed
        (propagated). *)
    let user_term ((x, _) : rocq_args) : string M.mm =
      let open M.Syntax in
      let* e = M.constrexpr_to_econstr x in
      M.state (fun env sigma -> sigma, Rocq_utils.Strfy.econstr env sigma e)
    ;;

    (** [is_similar r] is whether, by the check [r], the first system's start
        state is weakly simulated by the second's: bisimilar outright, or
        related by {!similarity}.

        Raises as {!similarity} (propagated). *)
    let is_similar (result : Model.Bisimilarity.t) : bool =
      Model.Bisimilarity.Result.are_bisimilar result.result
      ||
      match similarity result with
      | Some sim ->
        (match result.fsm_a.original.init, result.fsm_b.original.init with
         | Some ra, Some rb -> Model.Product.Pair.Set.mem (ra, rb) sim
         | _ -> false)
      | None -> false
    ;;

    (* See the [.mli]. *)
    let do_check_sim (args : rocq_pair) refs : Model.Bisimilarity.t option M.mm =
      Logger.trace __FUNCTION__;
      let open M.Syntax in
      let* result = bisimilarity_of args refs in
      let* left = user_term args.a in
      let* right = user_term args.b in
      if is_similar result
      then
        Logger.info
          (Printf.sprintf "(Similar: %s is weakly simulated by %s.)" left right)
      else (
        let msg =
          Printf.sprintf
            "%s is not weakly simulated by %s: no weak simulation relates them."
            left
            right
        in
        if !Api.the_fail_flags.non_bisimilar
        then
          CErrors.user_err
            (Pp.str
               ("MeBi: "
                ^ msg
                ^ " [MeBi Config FailIf NotBisimilar False] makes this a \
                   warning."))
        else Logger.warning msg);
      M.return (Some result)
    ;;

    (** Raised by {!extract_benchmark_args}: a list type that is not Rocq's
        [list]. *)
    exception NothingToBenchmark

    (** [extract_benchmark_args xs] is the elements of the Rocq list [xs], or
        [[xs]] if it is not a list.

        @raise NothingToBenchmark
          when run, if [xs]'s type is an application that is not [list]
          (raised here). *)
    let rec extract_benchmark_args (xs : EConstr.t) : EConstr.t list M.mm =
      Logger.debug __FUNCTION__;
      let open M.Syntax in
      let* ty = M.type_of_econstr xs in
      let* kxs = M.econstr_kind xs in
      let* kty = M.econstr_kind ty in
      match kty, kxs with
      | App (ty, _), App (_, tys) ->
        let* is_list : bool = Theory.is_list ty in
        if is_list
        then
          if Int.equal (Array.length tys) 1
          then M.return []
          else
            let* tl = extract_benchmark_args tys.(2) in
            M.return (tys.(1) :: tl)
        else raise NothingToBenchmark
      | _, _ ->
        (* NOTE: isn't a list, so treat as single lts *)
        M.return [ xs ]
    ;;

    (** A benchmark case: its name, what it times, and its argument. *)
    type graph_benchmark =
      string * (Constrexpr.constr_expr -> LTS.t) * Constrexpr.constr_expr

    (** [add_benchmark_case lts using i x cases] is [cases] with the [i]th
        case in front: named [benchmark_graph_i], timing the extraction of
        the term [x] by [lts] ({!build_lts}). Raises nothing when run; the
        case itself raises as {!build_lts}, when timed. *)
    let add_benchmark_case
          (primary_lts : Libnames.qualid)
          (refs : Libnames.qualid list)
          (i : int)
          (x : EConstr.t)
          (funs : graph_benchmark list)
      : graph_benchmark list M.mm
      =
      Logger.debug __FUNCTION__;
      let open M.Syntax in
      let test_name : string = Printf.sprintf "benchmark_graph_%i" i in
      let* x : Constrexpr.constr_expr =
        M.state (fun env sigma ->
          sigma, Rocq_utils.econstr_to_constrexpr env sigma x)
      in
      let runf = fun x -> M.run (build_lts primary_lts x refs) in
      M.return ((test_name, runf, x) :: funs)
    ;;

    (* See the [.mli]. *)
    let do_benchmark_graph
          (((xs, primary_lts), (time, repeat)) : rocq_args * (int * int))
          refs
      : Model.Bisimilarity.t option M.mm
      =
      Logger.trace __FUNCTION__;
      let open M.Syntax in
      let* xs : EConstr.t = M.constrexpr_to_econstr xs in
      let* xs : EConstr.t list = extract_benchmark_args xs in
      let* funs : graph_benchmark list =
        M.iterate
          0
          (List.length xs - 1)
          []
          (fun i -> add_benchmark_case primary_lts refs i (List.nth xs i))
      in
      let samples = Benchmark.throughputN ~style:All ~repeat time funs in
      handle_results Result "benchmark lts graph" samples (module Benchmarking);
      M.return None
    ;;

    (* See the [.mli]. *)
    let run (refs : Libnames.qualid list) (x : t)
      : Model.Bisimilarity.t option M.mm
      =
      Logger.trace __FUNCTION__;
      let open M.Syntax in
      Config.load_the_bounds_args ();
      let* () = Config.load_weak_args () in
      match x with
      | MakeLTS args -> do_make_lts args refs
      | MakeFSM args -> do_make_fsm args refs
      | Saturate args -> do_saturate args refs
      | Minimize args -> do_minimize args refs
      | Merge args -> do_merge args refs
      | CheckBisim args -> do_check_bisim args refs
      | CheckSim args -> do_check_sim args refs
      | BenchmarkGraph args -> do_benchmark_graph args refs
    ;;
  end
end

(* See the [.mli]. There is no [?log]: output goes through [Logger] against
   the sink installed at plugin load. *)
let make ?(enc : unit -> (module Encoding.S) = Api.make_enc_int) () : (module S)
  =
  let module Enc : Encoding.S = (val enc ()) in
  (module Make (Enc) : S)
;;

(** The instance the [MeBi ...] vernaculars run against.

    Every command used to call [make ()] inside its own action block, rebuilding
    [Enc], [Model], [Decode], [Theory], [Weak] and [Config] -- and a fresh
    encoding table -- on each invocation. Since the Rocq context is no longer a
    functor parameter, nothing about a command depends on the instance being
    fresh, so there is one.

    Per-command state is carried by the [~reset_encoding:true] every call site
    already passes: [Rocq_monad.run] then calls [Bi_encoding.reset], which
    clears the maps and resets [Enc.counter]. That flag was previously inert,
    because a fresh [Bi_encoding] has [the_maps = None] and so reset regardless.
    Bounds and weak-mode config are reloaded by [Command.run] itself. *)
let the_wrapper : (module S) option ref = ref None

(* Lazy rather than initialised at load: constructing it runs Api.make_enc_int,
   which should not race the sink install in Rocq_output. *)
let get () : (module S) =
  match !the_wrapper with
  | Some w -> w
  | None ->
    let w : (module S) = make () in
    the_wrapper := Some w;
    w
;;

let reset () : unit = the_wrapper := None
