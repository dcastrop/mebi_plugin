(* See the [.mli]. The engine (lib/learn) decides the goal on Sylvan's
   diagrams; with [Set MeBi Log Mode], Sylvan logs the decision's last
   checks, the certificate that the plugin turns into facts the kernel
   checks (doc/certificate.md). *)

type kind =
  | Bisim
  | Sim

(** the tactic's name, and the relation its goal must be *)
let name ~(weak : bool) (k : kind) : string =
  (if weak then "weak_" else "")
  ^ match k with Bisim -> "bisim" | Sim -> "sim"
;;

let head ~(weak : bool) (k : kind) : string =
  (if weak then "weak_" else "strong_")
  ^ match k with Bisim -> "bisimilar" | Sim -> "sim"
;;

let fail ~weak kind msg =
  CErrors.user_err (Pp.str (Printf.sprintf "%s: %s" (name ~weak kind) msg))
;;

let lts_shaped = Rocq_utils.lts_shaped

(** the relation [head ~weak k] of [theories/Bisimilarity.v], registered
    there as [mebi.bisimilarity.<name>] *)
let relation env ~weak k : EConstr.t =
  EConstr.of_constr
    (UnivGen.constr_of_monomorphic_global
       env
       (Rocqlib.lib_ref ("mebi.bisimilarity." ^ head ~weak k)))
;;

(* {1 The plugin's files}

   For a file [F.v], the directory [.F.v.mebi] next to it holds each
   lemma's files: Sylvan's proof log [L.log] for the lemma [L], written
   while the tactic proves [L] and then deleted, unless
   [Set MeBi Log Mode "keep"] (default "off"); and a log's facts
   ([<log>.<random>.facts], the proof's or [MeBi Check Log]'s), deleted
   once read. Next to
   the file being checked, never in /tmp or a cache in the home directory.
   [Set MeBi Log Directory "<dir>"] makes [.F.v.mebi] in [<dir>] instead
   (relative to [F.v]'s directory); [Unset] goes back to [F.v]'s own. *)
type log_mode =
  | Log_off
  | Log_keep

let log_mode =
  Goptions.declare_interpreted_string_option_and_ref
    (function
      | "off" -> Log_off
      | "keep" -> Log_keep
      | s ->
        CErrors.user_err
          Pp.(
            str ("Set MeBi Log Mode: \"off\" or \"keep\", not \"" ^ s ^ "\".")))
    (function Log_off -> "off" | Log_keep -> "keep")
    ~stage:Summary.Stage.Interp
    ~key:[ "MeBi"; "Log"; "Mode" ]
    ~value:Log_off
    ()
;;

let log_dir =
  Goptions.declare_stringopt_option_and_ref
    ~key:[ "MeBi"; "Log"; "Directory" ]
    ~value:None
    ()
;;

(* the file being checked: its directory and its name, from the current
   command's location (a tactic's too); for input from no file ([rocq repl]), the directory Rocq runs in and the module's name *)
let current_source () : string * string =
  match Loc.get_current_command_loc () with
  | Some { Loc.fname = Loc.InFile { file; _ }; _ } ->
    Filename.dirname file, Filename.basename file
  | _ ->
    let s = Names.DirPath.to_string (Lib.library_dp ()) in
    let m =
      match String.rindex_opt s '.' with
      | Some i -> String.sub s (i + 1) (String.length s - i - 1)
      | None -> s
    in
    Filename.current_dir_name, m ^ ".v"
;;

(* [.F.v.mebi] for the file [F.v] being checked, made if need be *)
let files_dir () : string =
  let dir, base = current_source () in
  let parent =
    match log_dir.get () with
    | None -> dir
    | Some d when Filename.is_relative d -> Filename.concat dir d
    | Some d -> d
  in
  let rec mkdir_p d =
    if Bool.not (Sys.file_exists d)
    then (
      mkdir_p (Filename.dirname d);
      Sys.mkdir d 0o755)
  in
  let d = Filename.concat parent ("." ^ base ^ ".mebi") in
  (try mkdir_p d with
   | Sys_error msg ->
     CErrors.user_err
       Pp.(str ("MeBi: cannot make the directory " ^ d ^ " (" ^ msg ^ ").")));
  if Bool.not (Sys.is_directory d)
  then CErrors.user_err Pp.(str ("MeBi: " ^ d ^ " is not a directory."));
  d
;;

(* the lemma being proved, from a tactic ([L] of [.F.v.mebi/L.log]). A
   tactic is not given its proof's name; Rocq's internal module has it
   (deprecated for other uses: here it only names a file). "goal" if there
   is none. *)
let current_lemma () : string =
  try
    Names.Id.to_string
      ((Vernacstate.Declare.get_current_proof_name [@alert "-deprecated"]) ())
  with
  | _ -> "goal"
;;

(* {1 The tactics} *)

(* a goal [relation ~weak kind] between two closed states: the engine's
   decision of it *)
let read_goal ~(weak : bool) (kind : kind) env sigma (concl : EConstr.t)
  : unit -> Learn.report
  =
  let h, args = EConstr.decompose_app sigma concl in
  let fail_c msg = fail ~weak kind msg in
  (* a goal of another of the four tactics: say which *)
  List.iter
    (fun (w, k) ->
      if
        (w <> weak || k <> kind)
        && EConstr.eq_constr sigma h (relation env ~weak:w k)
      then
        fail_c
          (Printf.sprintf
             "the goal is [%s]: use [%s]."
             (head ~weak:w k)
             (name ~weak:w k)))
    [ true, Bisim; true, Sim; false, Bisim; false, Sim ];
  if Bool.not (EConstr.eq_constr sigma h (relation env ~weak kind))
  then fail_c (Printf.sprintf "the goal is not [%s]." (head ~weak kind));
  if
    Bool.not (EConstr.Vars.closed0 sigma concl)
    || Bool.not (Names.Id.Set.is_empty (Termops.collect_vars sigma concl))
    || Bool.not (Evar.Set.is_empty (Evd.evars_of_term sigma concl))
  then
    fail_c
      "the goal mentions local hypotheses or evars; it must be between closed \
       terms.";
  match args with
  | [| _m_ty; _n_ty; _a; lts_m; lts_n; x; y |] ->
    let ind (t : EConstr.t) : Names.inductive =
      match EConstr.kind sigma t with
      | Constr.Ind (i, _) when lts_shaped env i -> i
      | _ ->
        fail_c
          "each LTS must be an inductive relation [M -> option A -> M -> Prop] \
           without parameters."
    in
    let system lts t : Learn.system =
      { lts; start = EConstr.to_constr sigma (Reductionops.nf_all env sigma t) }
    in
    let k = match kind with Bisim -> Learn.Bisim | Sim -> Learn.Sim in
    (* weak: the label type's [None] is the silent step, as in
       [theories/Bisimilarity.v]; strong: no label is *)
    fun () ->
      (try
         Learn.check
           env
           sigma
           ~strong:(Bool.not weak)
           k
           (system (ind lts_m) x)
           (system (ind lts_n) y)
       with
       | Learn.Unfollowable (lts, t) ->
         CErrors.user_err
           Pp.(
             str (name ~weak kind ^ ": the steps of ")
             ++ Printer.pr_constr_env env sigma t
             ++ str " by "
             ++ Printer.pr_global (Names.GlobRef.IndRef lts)
             ++ str " cannot be followed from the rules."))
  | _ -> fail_c (Printf.sprintf "[%s] is not fully applied." (head ~weak kind))
;;

(* the engine's verdict: a failure if the goal does not hold *)
let verdict ~(weak : bool) (kind : kind) (r : Learn.report) : unit =
  if Bool.not r.Learn.verdict
  then
    fail
      ~weak
      kind
      (match kind with
       | Bisim ->
         Printf.sprintf
           "the two are not %s bisimilar."
           (if weak then "weakly" else "strongly")
       | Sim ->
         Printf.sprintf
           "the first is not %s simulated by the second."
           (if weak then "weakly" else "strongly"))
;;

(* the GC's setting a large proof lowered for its [Qed] (and the compiled
   file's writing), restored at the next tactic *)
let gc_saved : int option ref = ref None

(* [certify ~weak ~global ~lemma kind env sigma concl]: the goal [concl]
   decided, its certificate logged, and the proof built from it (the
   term, the side effects that hold its pieces unless [global], the
   report, the time it started); the pieces named after [lemma], declared
   as global constants with [global] (a command's way: Rocq keeps each
   once), else as side effects of the proof (a tactic's way) *)
exception Upto_failed of string

let certify
      ?(debug : bool = false)
      ~(weak : bool)
      ~(global : bool)
      ~(lemma : string)
      (kind : kind)
      env
      sigma
      (concl : EConstr.t)
  : Constr.t * Evd.side_effects option * Learn.report * float
  =
  (match !gc_saved with
   | Some so ->
     Gc.set { (Gc.get ()) with Gc.space_overhead = so };
     gc_saved := None
   | None -> ());
  let decide = read_goal ~weak kind env sigma concl in
  (* the decision logs its certificate, which the proof is built from
     (doc/certificate.md): in memory, or with [Set MeBi Log Mode "keep"]
     into the file kept *)
  let keep = log_mode.get () = Log_keep in
  let src =
    if keep
    then Proof_log.File (Filename.concat (files_dir ()) (lemma ^ ".log"))
    else Proof_log.Memory
  in
  (* a log in memory freed (collection back on), once *)
  let dropped = ref false in
  let drop () =
    if Bool.not !dropped
    then (
      dropped := true;
      if Bool.not keep then Ldd.log_free ())
  in
  Learn.proof_log := Some src;
  let r =
    Fun.protect
      ~finally:(fun () -> Learn.proof_log := None)
      (fun () ->
        try decide () with
        | e ->
          drop ();
          raise e)
  in
  if debug
  then
    Feedback.msg_notice
      Pp.(
        str ("debug " ^ name ~weak kind ^ ": ")
        ++ Learn.pr
             ~strong:(Bool.not weak)
             (match kind with Bisim -> Learn.Bisim | Sim -> Learn.Sim)
             r);
  (* MEBI_CERT_STOP=decided: stop here (measuring the decision alone) *)
  if Sys.getenv_opt "MEBI_CERT_STOP" = Some "decided"
  then (
    Printf.eprintf "[mebi] decided; stopping (MEBI_CERT_STOP)\n%!";
    Stdlib.exit 3);
  (try verdict ~weak kind r with
   | e ->
     drop ();
     raise e);
  match !Learn.last_plan with
  | None ->
    drop ();
    fail ~weak kind "the decision's certificate could not be logged."
  | Some plan ->
    if Sys.getenv_opt "MEBI_PLAN_DUMP" <> None
    then Lts_proof.dump env sigma plan;
    let t0 = Unix.gettimeofday () in
    let term, effs =
      try
        if Sys.getenv_opt "MEBI_CERT_STATS" <> None
        then Printexc.record_backtrace true;
        try
          (* building the proof makes much short-lived garbage: the heap
             kept closer to what is live meanwhile *)
          let gc0 = Gc.get () in
          (* a command's pieces are kept once declared: what is left is
             garbage, so tighter still *)
          Gc.set
            { gc0 with
              Gc.space_overhead =
                min gc0.Gc.space_overhead (if global then 40 else 80)
            };
          Fun.protect
            ~finally:(fun () ->
              drop ();
              Gc.set gc0;
              (* before [Qed]: the decision's sets and tables given back
                 (the plan dropped, OCaml's dead values finalised, then
                 Sylvan's collection, which shrinks its table to fit),
                 and the proof's garbage *)
              Learn.last_plan := None;
              (* for a large decision only: the collections and the
                 compaction cost a fraction of a second each *)
              if r.Learn.pairs > 1e6
              then (
                Gc.full_major ();
                Ldd.gc ();
                Gc.compact ();
                (* the kernel's check at [Qed] makes much short-lived
                   garbage over a large live term: the heap kept closer
                   to it until the next tactic *)
                gc_saved := Some (Gc.get ()).Gc.space_overhead;
                Gc.set { (Gc.get ()) with Gc.space_overhead = 60 });
              Dd_cert.log_mark "[qed] before Qed")
            (fun () ->
              Lts_proof.qed_term
                env
                ~name:lemma
                ~log:src
                ~global
                plan
                ~strong:(Bool.not weak))
        with
        | Not_found ->
          failwith ("an internal lookup failed: " ^ Printexc.get_backtrace ())
      with
      | (Dd_cert.Fails msg | Failure msg)
        when plan.Learn.Plan.sys_a.Learn.Plan.pnorm <> []
             || plan.Learn.Plan.sys_b.Learn.Plan.pnorm <> [] ->
        raise (Upto_failed msg)
      | Dd_cert.Fails msg | Failure msg ->
        fail ~weak kind ("the proof from the certificate: " ^ msg)
    in
    term, effs, r, t0
;;

(* the proof up to reordering could not be built (rules that do not
   commute with the reorderings, say): decided and proved again without
   it *)
let certify ?debug ~weak ~global ~lemma kind env sigma concl =
  try certify ?debug ~weak ~global ~lemma kind env sigma concl with
  | Upto_failed msg ->
    Feedback.msg_info
      Pp.(
        str
          (name ~weak kind
           ^ ": no proof up to reordering ("
           ^ msg
           ^ "); proved without it."));
    Learn.upto_off := true;
    Fun.protect
      ~finally:(fun () -> Learn.upto_off := false)
      (fun () -> certify ?debug ~weak ~global ~lemma kind env sigma concl)
;;

(* the tactic: the engine decides the goal, Sylvan logs the decision's
   certificate, and the goal is closed by the proof built from it
   (doc/certificate.md, "The theorem from the certificate"); the kernel
   checks that proof at [Qed] *)
let prove ?(debug : bool = false) ~(weak : bool) (kind : kind)
  : unit Proofview.tactic
  =
  Proofview.Goal.enter (fun gl ->
    let env = Proofview.Goal.env gl in
    let sigma = Proofview.Goal.sigma gl in
    let term, effs, r, t0 =
      certify
        ~debug
        ~weak
        ~global:false
        ~lemma:(current_lemma ())
        kind
        env
        sigma
        (Proofview.Goal.concl gl)
    in
    Feedback.msg_notice
      Pp.(
        str
          (Printf.sprintf
             "%s: decided (%.0f pairs); the proof from Sylvan's certificate \
              built in %.2fs (the kernel checks it at Qed)."
             (name ~weak kind)
             r.Learn.pairs
             (Unix.gettimeofday () -. t0)));
    let term = EConstr.of_constr term in
    (* the proof's constants, each checked by the kernel when declared,
       exported with the proof at [Qed] (as [abstract]'s sub-proofs) *)
    let sigma =
      match effs with Some e -> Evd.emit_side_effects e sigma | None -> sigma
    in
    (* the universe constraints the proof needs (Lts_proof.univs) *)
    let sigma =
      Evd.merge_universe_context_set Evd.univ_rigid sigma !Lts_proof.univs
    in
    Proofview.tclTHEN
      (Proofview.Unsafe.tclEVARS sigma)
      (Tactics.exact_no_check term))
;;

(* which of the four relations a goal is *)
let relation_of ~(cmd : string) env sigma (concl : EConstr.t) : bool * kind =
  let h, _ = EConstr.decompose_app sigma concl in
  match
    List.find_opt
      (fun (w, k) -> EConstr.eq_constr sigma h (relation env ~weak:w k))
      [ true, Bisim; true, Sim; false, Bisim; false, Sim ]
  with
  | Some x -> x
  | None ->
    CErrors.user_err
      Pp.(
        str
          (cmd
           ^ ": the goal is not [weak_sim], [weak_bisimilar], [strong_sim] or \
              [strong_bisimilar] between two states."))
;;

(* [MeBi Prove id : goal]: the goal decided and proved from its
   certificate as the tactics do, but each piece of the proof a global
   constant, as a command can declare (Rocq keeps each once, checked when
   declared; a tactic's pieces are side effects, kept in their typed form
   until [Qed]), then the lemma [id], its proof naming them *)
let prove_command (id : Names.Id.t) (c : Constrexpr.constr_expr) : unit =
  let env = Global.env () in
  let sigma = Evd.from_env env in
  let sigma, ty =
    try Constrintern.interp_type_evars env sigma c with
    | e when CErrors.noncritical e ->
      CErrors.user_err
        Pp.(
          str "MeBi Prove: the statement does not typecheck: "
          ++ CErrors.print e)
  in
  let sigma =
    Pretyping.solve_remaining_evars
      (Pretyping.default_inference_flags true)
      env
      sigma
  in
  let weak, kind = relation_of ~cmd:"MeBi Prove" env sigma ty in
  let term, _, r, t0 =
    certify ~weak ~global:true ~lemma:(Names.Id.to_string id) kind env sigma ty
  in
  let sigma = Evd.minimize_universes sigma in
  let univs =
    Univ.ContextSet.union (Evd.universe_context_set sigma) !Lts_proof.univs
  in
  let t1 = Unix.gettimeofday () in
  ignore
    (Declare.declare_constant
       ~name:id
       ~kind:Decls.(IsProof Lemma)
       (Declare.DefinitionEntry
          (Declare.definition_entry
             ~opaque:true
             ~types:(EConstr.to_constr sigma ty)
             ~univs:(UState.Monomorphic_entry univs, UnivNames.empty_binders)
             term)));
  Feedback.msg_notice
    Pp.(
      str
        (Printf.sprintf
           "%s is defined: decided (%.0f pairs); the proof from Sylvan's \
            certificate built in %.2fs, checked by the kernel piece by piece; \
            the lemma checked in %.2fs."
           (Names.Id.to_string id)
           r.Learn.pairs
           (t1 -. t0)
           (Unix.gettimeofday () -. t1)))
;;

(* {1 [MeBi Check Log]: facts from Sylvan's proof log}

   doc/certificate.md, A: the records of a proof log (a file
   [Set MeBi Log Mode "keep"] left; a relative path is taken from the
   checked file's directory) as facts of theories/DdTree.v, through a file
   in [.F.v.mebi] ([Dd_cert.write_log]), read back a chunk at a time
   ([Dd_read.read]), the largest first, each chunk its own constant,
   checked when declared: no term holds them all. *)
let check_log_count = ref 0

let check_log (log : string) : unit =
  let t0 = Unix.gettimeofday () in
  let log =
    if Filename.is_relative log
    then Filename.concat (fst (current_source ())) log
    else log
  in
  incr check_log_count;
  let tag = !check_log_count in
  (* MEBI_LOG_FACTS=<file>: read that facts file (a translation kept by
     MEBI_CERT_KEEP, or a part of one) instead of translating [log]; for
     measuring the reading alone *)
  let given = Sys.getenv_opt "MEBI_LOG_FACTS" in
  let path =
    match given with
    | Some f -> f
    | None ->
      Filename.temp_file
        ~temp_dir:(files_dir ())
        (Filename.remove_extension (Filename.basename log) ^ ".")
        ".facts"
  in
  let keep =
    Stdlib.Option.is_some (Sys.getenv_opt "MEBI_CERT_KEEP")
    || Stdlib.Option.is_some given
  in
  let finish () =
    if Bool.not keep then (try Sys.remove path with Sys_error _ -> ())
  in
  let st =
    try
      match given with
      | Some _ -> None
      | None ->
        Some
          (Dd_cert.write_log ~log:(Proof_log.File log) (Dd_cert.To_file path))
    with
    | Dd_cert.Fails msg | Sys_error msg | Failure msg ->
      finish ();
      CErrors.user_err Pp.(str ("MeBi Check Log: " ^ msg))
  in
  (* the translation's tables are garbage now: give the memory back
     before reading; then a tight GC while the chunks are read and
     declared (Rocq's own, a 256MB minor heap and a space overhead of 200,
     lets uncollected garbage reach about four times the live facts),
     restored after *)
  let gc0 = Gc.get () in
  Gc.set { gc0 with Gc.space_overhead = 30; minor_heap_size = 4 * 1024 * 1024 };
  Gc.compact ();
  Dd_cert.log_mark "translated, collected; reading";
  let last = ref "" in
  let declare (i : int) (typ : Constr.types) (proof : Constr.t) : unit =
    let name = Names.Id.of_string (Printf.sprintf "mebi_log%d_c%d" tag i) in
    last := Names.Id.to_string name;
    Dd_cert.log_mark (Printf.sprintf "chunk %d built; declaring" i);
    ignore
      (Declare.declare_constant
         ~name
         ~kind:Decls.(IsProof Lemma)
         (Declare.DefinitionEntry
            (Declare.definition_entry ~opaque:true ~types:typ proof)));
    Dd_cert.log_mark (Printf.sprintf "chunk %d declared" i)
  in
  let r =
    try Dd_read.read (Global.env ()) ~declare path with
    | e ->
      Gc.set gc0;
      finish ();
      (match e with
       | Failure msg -> CErrors.user_err Pp.(str ("MeBi Check Log: " ^ msg))
       | e -> raise e)
  in
  Gc.set gc0;
  finish ();
  Feedback.msg_notice
    Pp.(
      str
        (Printf.sprintf
           "MeBi Check Log: %d chunks, %d facts, %d chunk nodes (%s), read in \
            %.1fs; %.2fs in all; the last chunk: %s"
           r.Dd_read.chunks
           r.Dd_read.facts
           r.Dd_read.nodes
           (match st with
            | Some st -> Printf.sprintf "translated in %.2fs" st.Dd_cert.seconds
            | None -> "from " ^ path)
           r.Dd_read.seconds
           (Unix.gettimeofday () -. t0)
           !last))
;;
