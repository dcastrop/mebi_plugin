module type S = sig
  type weak
  type 'a mm
  type lts
  type fsm
  type bisimilarity
  type result

  val build_lts
    :  ?weak:weak option
    -> Libnames.qualid
    -> Constrexpr.constr_expr
    -> Libnames.qualid list
    -> lts mm

  val build_fsm
    :  ?weak:weak option
    -> Libnames.qualid
    -> Constrexpr.constr_expr
    -> Libnames.qualid list
    -> fsm mm

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
    -> bisimilarity option mm

  val do_make_fsm
    :  Constrexpr.constr_expr * Libnames.qualid
    -> Libnames.qualid list
    -> bisimilarity option mm

  val do_saturate
    :  Constrexpr.constr_expr * Libnames.qualid
    -> Libnames.qualid list
    -> bisimilarity option mm

  val do_minimize
    :  Constrexpr.constr_expr * Libnames.qualid
    -> Libnames.qualid list
    -> bisimilarity option mm

  val build_fsms
    :  rocq_args
    -> rocq_args
    -> Libnames.qualid list
    -> (fsm * fsm) mm

  val do_merge : rocq_pair -> Libnames.qualid list -> bisimilarity option mm
  val fail_if_not_bisim : result -> unit

  val do_check_bisim
    :  rocq_pair
    -> Libnames.qualid list
    -> bisimilarity option mm

  val run : Libnames.qualid list -> t -> bisimilarity option mm
end

module Make (Log : Logger.S) (W : Wrapper.S) :
  S
  with type weak = W.Weak.t
   and type 'a mm = 'a W.M.mm
   and type lts = W.Model.LTS.t
   and type fsm = W.Model.FSM.t
   and type bisimilarity = W.Model.Bisimilarity.t
   and type result = W.Model.Bisimilarity.Result.t = struct
  open W
  module LTS = Model.LTS
  module FSM = Model.FSM

  type weak = W.Weak.t
  type 'a mm = 'a W.M.mm
  type lts = LTS.t
  type fsm = FSM.t
  type bisimilarity = Model.Bisimilarity.t
  type result = Model.Bisimilarity.Result.t

  let build_lts
        ?(weak : Weak.t option = None)
        (primary_lts : Libnames.qualid)
        (init : Constrexpr.constr_expr)
        (names : Libnames.qualid list)
    : LTS.t M.mm
    =
    Log.trace __FUNCTION__;
    Config.get_weak weak |> extract_lts primary_lts init names
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
    let { fsm; pi } : Model.Minimization.t = Model.Minimization.fsm the_fsm in
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

  let fail_if_not_bisim (x : Model.Bisimilarity.Result.t) : unit =
    if !Api.the_fail_flags.non_bisimilar
    then
      if Bool.not (Model.Bisimilarity.Result.are_bisimilar x)
      then (
        result_log (module Model.Bisimilarity.Result) (module Decode.Result)
        |> handle_results Result "LTS Incomplete" x;
        M.Err.not_bisimilar ())
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
