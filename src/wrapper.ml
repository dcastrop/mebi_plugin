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
     and type states = Model.States.t
     and type partition = Model.Partition.t
     and type label = Model.Label.t
     and type labels = Model.Labels.t
     and type note = Model.Note.t
     and type annotation = Model.Annotation.t
     and type annotations = Model.Annotations.t
     and type transition = Model.Transition.t
     and type transitions = Model.Transitions.t
     and type action = Model.Action.t
     and type actions = Model.Actions.t
     and type actionmap = Model.ActionMap.t'
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

  module Config : sig
    val load_weak_arg : Api.weak_arg -> Weak.t M.mm
    val load_weak_arg_opt : Api.weak_arg option -> Weak.t option M.mm

    type weak_args =
      { a : Weak.t option
      ; b : Weak.t option
      }

    val the_weak_args : weak_args ref option ref
    val reset_the_weak_args : unit -> unit
    val load_weak_args : unit -> unit M.mm
    val get_the_weak_args : unit -> weak_args option
    val get_the_weak_arg1 : unit -> Weak.t option
    val get_the_weak_arg2 : unit -> Weak.t option

    (* val api_bounds_to_model_bounds : Api.bounds_args -> Model.Info.Meta.bounds *)
    val the_bounds_args : Api.bounds_args ref
    val load_the_bounds_args : unit -> unit
  end

  val extract_lts
    :  Libnames.qualid
    -> Constrexpr.constr_expr
    -> Libnames.qualid list
    -> Weak.t option
    -> Model.LTS.t M.mm

  module Command : sig
    val default_weak_arg : Weak.t option -> Weak.t option

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

    val run : Libnames.qualid list -> t -> Model.Bisimilarity.t option M.mm
  end
end

module Make (Log : Logger.S) (Ctx : Rocq_context.S) (Enc : Encoding.S) :
  S
  with type enc = Enc.t
   and type node = Enc.Tree.Node.t
   and type tree = Enc.Tree.t
   and type trees = Enc.Trees.t = struct
  type enc = Enc.t
  type node = Enc.Tree.Node.t
  type tree = Enc.Tree.t
  type trees = Enc.Trees.t

  (** [module M] ... *)
  module M = Rocq_monad_utils.Make (Log) (Ctx) (Enc)

  (** [module Bindings] ... *)
  module Bindings = Bindings.Make (Log) (M)

  (** [module ConstructorBindings] ... *)
  module ConstructorBindings = Constructor_bindings.Make (Log) (M) (Bindings)

  (** [module Model] ... *)
  module Model = Model.Make (Log) (Enc) (ConstructorBindings)

  module LTS = Model.LTS
  module FSM = Model.FSM

  (** [module Decode] handles obtaining [EConstr.t] from [module M]. *)
  module Decode = Decoder.Make (Log) (Enc) (M) (ConstructorBindings) (Model)

  (** [module Theory] ... *)
  module Theory =
    Theories_enc.Make (Log) (Enc) (M) (M) (Theories.Make (Log) (Enc) (M))

  (** [module Weak] ... *)
  module Weak = Weak.Make (Log) (Enc) (M)

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

  let check_if_lts_fail (x : LTS.t) : unit =
    if
      !Api.the_fail_flags.empty
      && (Int.equal (Model.States.cardinal x.states) 1
          || Model.States.is_empty x.states)
      && Model.Transitions.is_empty x.transitions
    then (
      Log.trace ~__FUNCTION__ "LTS Empty";
      M.Err.lts_empty ())
    else if !Api.the_fail_flags.incomplete
    then (
      match x with
      | { info = { meta = Some { is_complete = false; _ }; _ }; _ } ->
        result_log (module Model.LTS) (module Decode.LTS)
        |> handle_results Result "LTS Incomplete" x;
        M.Err.lts_incomplete "TODO..."
      | _ -> ())
    else ()
  ;;

  let fail_if_not_bisim (x : Model.Bisimilarity.Result.t) : unit =
    if !Api.the_fail_flags.non_bisimilar
    then
      if Bool.not (Model.Bisimilarity.Result.are_bisimilar x)
      then (
        result_log (module Model.Bisimilarity.Result) (module Decode.Result)
        |> handle_results Result "LTS Incomplete" x;
        M.Err.not_bisimilar ())
  ;;

  (** Config *)
  module Config = struct
    let load_weak_arg : Api.weak_arg -> Weak.t M.mm =
      let open M.Syntax in
      function
      | Api.Option label_tref ->
        let* label : EConstr.t = M.constrexpr_to_econstr label_tref in
        let label_enc : Enc.t = M.encode label in
        (* NOTE: sanity check we can decode these *)
        let _ : EConstr.t = M.decode label_enc in
        Weak.Option label_enc |> M.return
      | Api.Custom (tau_tref, label_ref) ->
        let* tau : EConstr.t = M.constrexpr_to_econstr tau_tref in
        let tau_enc : Enc.t = M.encode tau in
        let* ind, (mib, mip) =
          Nametab.global label_ref |> M.Ind.lts_type_mind
        in
        let label : EConstr.t = Rocq_utils.get_ind_ty ind mib in
        let label_enc : Enc.t = M.encode label in
        (* NOTE: sanity check we can decode these *)
        let _ : EConstr.t = M.decode tau_enc in
        let _ : EConstr.t = M.decode label_enc in
        Weak.Custom (tau_enc, label_enc) |> M.return
    ;;

    let load_weak_arg_opt : Api.weak_arg option -> Weak.t option M.mm = function
      | None -> M.return None
      | Some x ->
        let open M.Syntax in
        let* y = load_weak_arg x in
        M.return (Some y)
    ;;

    type weak_args =
      { a : Weak.t option
      ; b : Weak.t option
      }

    let the_weak_args : weak_args ref option ref = ref None
    let reset_the_weak_args () : unit = the_weak_args := None

    let load_weak_args () : unit M.mm =
      Log.trace __FUNCTION__;
      let open M.Syntax in
      match !Api.the_weak_args with
      | None ->
        the_weak_args := None;
        M.return ()
      | Some x ->
        let* a = load_weak_arg_opt !x.a in
        let* b = load_weak_arg_opt !x.b in
        the_weak_args := Some (ref { a; b });
        M.return ()
    ;;

    let get_the_weak_args () : weak_args option =
      match !the_weak_args with None -> None | Some x -> Some !x
    ;;

    let get_the_weak_arg1 () : Weak.t option =
      match get_the_weak_args () with None -> None | Some x -> x.a
    ;;

    let get_the_weak_arg2 () : Weak.t option =
      match get_the_weak_args () with None -> None | Some x -> x.b
    ;;

    (***********************************************************************)

    let the_bounds_args : Api.bounds_args ref = ref Api.default_bounds
    let load_the_bounds_args () : unit = the_bounds_args := !Api.the_bounds_args
  end

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

  module G
      (X : Graph_type.Args with type enc = Enc.t and type tree = Enc.Tree.t) =
    Graph.Make (Log) (Enc) (M) (Weak) (Theory) (ConstructorBindings) (Model) (X)

  let extract_lts
        (primary_lts : Libnames.qualid)
        (init : Constrexpr.constr_expr)
        (names : Libnames.qualid list)
        (weak : Weak.t option)
    : LTS.t M.mm
    =
    Log.trace __FUNCTION__;
    let module G = G ((val make_graph_args ())) in
    let grefs = Rocq_utils.libnames_to_globrefs (primary_lts :: names) in
    let open M.Syntax in
    let* the_graph : G.t = G.build ~weak init primary_lts grefs in
    let* the_lts : Model.LTS.t = G.extract the_graph in
    check_if_lts_fail the_lts;
    M.return the_lts
  ;;

  module Command = struct
    (** [default_weak_arg x] will return [Config.get_the_weak_arg1] in the case that [x] is [None], else returns [x].
    *)
    let default_weak_arg : Weak.t option -> Weak.t option = function
      | None -> Config.get_the_weak_arg1 ()
      | Some x -> Some x
    ;;

    let build_lts
          ?(weak : Weak.t option = None)
          (primary_lts : Libnames.qualid)
          (init : Constrexpr.constr_expr)
          (names : Libnames.qualid list)
      : LTS.t M.mm
      =
      Log.trace __FUNCTION__;
      default_weak_arg weak |> extract_lts primary_lts init names
    ;;

    let build_fsm
          ?(weak : Weak.t option = None)
          (primary_lts : Libnames.qualid)
          (init : Constrexpr.constr_expr)
          (names : Libnames.qualid list)
      : FSM.t M.mm
      =
      Log.trace __FUNCTION__;
      let open M.Syntax in
      let* the_lts = build_lts ~weak primary_lts init names in
      Model.FSM.of_lts the_lts |> M.return
    ;;

    type t =
      | MakeLTS of rocq_args
      | MakeFSM of rocq_args
      | Saturate of rocq_args
      | Minimize of rocq_args
      | Merge of rocq_pair
      | CheckBisim of rocq_pair

    (* | Info of unit  *)
    (* | Help of ...  *)
    and rocq_args = Constrexpr.constr_expr * Libnames.qualid

    and rocq_pair =
      { a : rocq_args
      ; b : rocq_args
      }

    let do_make_lts (x, primary_lts) refs : Model.Bisimilarity.t option M.mm =
      Log.trace __FUNCTION__;
      let open M.Syntax in
      Log.info "Extracting LTS...";
      let* the_lts = build_lts primary_lts x refs in
      result_log (module Model.LTS) (module Decode.LTS)
      |> handle_results Result "Finished Extracting LTS" the_lts;
      M.return None
    ;;

    let do_make_fsm (x, primary_lts) refs : Model.Bisimilarity.t option M.mm =
      Log.trace __FUNCTION__;
      Log.info "Making FSM (from extracted LTS)...";
      let open M.Syntax in
      let* the_fsm = build_fsm primary_lts x refs in
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Result "Finished Making FSM" the_fsm;
      M.return None
    ;;

    let do_saturate (x, primary_lts) refs : Model.Bisimilarity.t option M.mm =
      Log.trace __FUNCTION__;
      Log.info "Making FSM (from extracted LTS)...";
      let open M.Syntax in
      let* the_fsm = build_fsm primary_lts x refs in
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Info "Finished Making FSM" the_fsm;
      Log.info "Saturating FSM...";
      let the_fsm = Model.FSM.saturate the_fsm in
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Result "Finished Saturating FSM" the_fsm;
      M.return None
    ;;

    let do_minimize (x, primary_lts) refs : Model.Bisimilarity.t option M.mm =
      Log.trace __FUNCTION__;
      Log.info "Making FSM (from extracted LTS)...";
      let open M.Syntax in
      let* the_fsm = build_fsm primary_lts x refs in
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Info "Finished Making FSM" the_fsm;
      Log.info "Minimizing FSM...";
      let { fsm; pi } : Model.Minimize.t = Model.Minimize.fsm the_fsm in
      Decode.Partition.log ~m:Info ~s:"pi" pi;
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Result "Finished Minimizing FSM" the_fsm;
      M.return None
    ;;

    let build_fsms
          ((ax, alts) : rocq_args)
          ((bx, blts) : rocq_args)
          (refs : Libnames.qualid list)
      : (FSM.t * FSM.t) M.mm
      =
      Log.trace __FUNCTION__;
      Log.info "Making FSMs...";
      let open M.Syntax in
      Log.info "Making FSM A...";
      let weak1 : Weak.t option = Config.get_the_weak_arg1 () in
      let* the_fsm_a = build_fsm ~weak:weak1 alts ax refs in
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Info "Finished Making FSM A" the_fsm_a;
      Log.info "Making FSM B...";
      let weak2 : Weak.t option = Config.get_the_weak_arg2 () in
      let* the_fsm_b = build_fsm ~weak:weak2 blts bx refs in
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Info "Finished Making FSM B" the_fsm_b;
      M.return (the_fsm_a, the_fsm_b)
    ;;

    let do_merge { a; b } refs : Model.Bisimilarity.t option M.mm =
      Log.trace __FUNCTION__;
      let open M.Syntax in
      let* the_fsm_a, the_fsm_b = build_fsms a b refs in
      Log.info "Merging FSMs...";
      let the_fsm = FSM.merge the_fsm_a the_fsm_b in
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Result "Finished Merging FSMs" the_fsm;
      M.return None
    ;;

    let do_check_bisim { a; b } refs : Model.Bisimilarity.t option M.mm =
      Log.trace __FUNCTION__;
      let open M.Syntax in
      let* the_fsm_a, the_fsm_b = build_fsms a b refs in
      Log.info "Checking Bisimilarity of FSMs...";
      let result = Model.Bisimilarity.fsm the_fsm_a the_fsm_b in
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
      fail_if_not_bisim result.result;
      M.return (Some result)
    ;;

    let run (refs : Libnames.qualid list) (x : t)
      : Model.Bisimilarity.t option M.mm
      =
      Log.trace __FUNCTION__;
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
    ;;
  end
end

(** [make ?log ?enc ?ctx] constructs a [Wrapper.S] module.
    @param ?log
      is a function that returns a [module Logger.S]. By default, this is obtained from the configuration in [module Api], via [Api.make_logger ()]. This is then used to construct the [Encoding.S] as well as [Wrapper.S].
    @param ?enc
      is a function that takes a [module Logger.S] and returns a [module Encoding.S]. The default encoding uses [Int.t].
    @param ?ctx is the rocq-context. *)
let make
      ?(log : unit -> (module Logger.S) = Api.make_logger)
      ?(enc : (module Logger.S) -> (module Encoding.S) = Api.make_enc_int)
      ?(ctx : (module Rocq_context.S) = (module Rocq_context.Default))
      ()
  : (module S)
  =
  let module Log : Logger.S = (val log ()) in
  let module Enc : Encoding.S = (val enc (module Log)) in
  (module Make (Log) ((val ctx)) (Enc) : S)
;;
