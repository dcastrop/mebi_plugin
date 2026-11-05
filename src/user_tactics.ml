(* See the [.mli]. The tactic runs today's proof search unchanged: it opens
   a proof of its own goal on the side, runs [Proof_solver.init] and
   [Proof_solver.solve] on it as [MeBi Sim Begin] and [MeBi Sim Solve]
   would, and closes the goal with the term found. The solver is driven
   from the command level ([Declare.Proof.by], one step at a time), so this
   is how a tactic can call it without a rewrite; and since the search is
   the same, so are its step counts. *)

type kind =
  | Bisim
  | Sim

let name = function Bisim -> "bisim" | Sim -> "sim"
let head = function Bisim -> "weak_bisimilar" | Sim -> "weak_sim"

let fail kind msg =
  CErrors.user_err (Pp.str (Printf.sprintf "%s: %s" (name kind) msg))
;;

let lts_shaped = Rocq_utils.lts_shaped
let using = Rocq_utils.lts_closure
let qualid_of_ind = Rocq_utils.qualid_of_ind

(** [extern env sigma t] is [t] as an expression to interpret again, with
    every implicit argument explicit and no notation, so that it means
    [t] wherever it is read. Raises nothing. *)
let extern (env : Environ.env) (sigma : Evd.evar_map) (t : EConstr.t)
  : Constrexpr.constr_expr
  =
  Constrextern.extern_constr
    ~flags:(PrintingFlags.make_raw (PrintingFlags.current ()))
    env
    sigma
    t
;;

(** [with_weak_args a f] runs [f] with the weak labels set to [option a]
    ([MeBi Config Weak As Option a]), unless the user set them, and
    restores them afterwards. In [theories/Bisimilarity.v], [None] is the
    silent step of every LTS, so this is always the right default. *)
let with_weak_args (a : Constrexpr.constr_expr) (f : unit -> 'a) : 'a =
  match !Api.the_weak_args with
  | Some _ -> f ()
  | None ->
    Api.the_weak_args := Some (ref { Api.a = Some (Api.Option a); b = None });
    Fun.protect ~finally:(fun () -> Api.reset_weak_args ()) f
;;

(** [open_goals p] is the open goals of the proof [p], as the debug mode
    shows them: the first with its hypotheses, then at most nine more,
    conclusions only. Written with printers Rocq 9.2 and 9.3 share
    ([Vernacgoal] is 9.3's). Raises nothing. *)
let open_goals (p : Proof.t) : Pp.t =
  let { Proof.goals; sigma; _ } = Proof.data p in
  let env = Global.env () in
  let goal (i : int) (g : Evar.t) : Pp.t =
    let info = Evd.find_undefined sigma g in
    let genv = Evd.evar_filtered_env env info in
    let concl =
      Pp.(str "  ⊢ " ++ Printer.pr_econstr_env genv sigma (Evd.evar_concl info))
    in
    let hyps =
      if i > 0
      then Pp.mt ()
      else
        Pp.prlist
          (fun d ->
            let open Context.Named.Declaration in
            Pp.(
              str "    "
              ++ Names.Id.print (get_id d)
              ++ str " : "
              ++ Printer.pr_econstr_env genv sigma (get_type d)
              ++ fnl ()))
          (List.rev (EConstr.named_context genv))
    in
    Pp.(
      str (Printf.sprintf "goal %i:" (i + 1))
      ++ fnl ()
      ++ hyps
      ++ concl
      ++ fnl ())
  in
  let n = List.length goals in
  Pp.(
    str (Printf.sprintf "(%i open)" n)
    ++ fnl ()
    ++ prlist
         (fun x -> x)
         (List.mapi goal (List.filteri (fun i _ -> i < 10) goals))
    ++ if n > 10 then str (Printf.sprintf "... and %i more" (n - 10)) else mt ())
;;

(** [search ?steps ~debug kind env sigma concl lts_m lts_n x y a] runs
    the proof search on [concl] in a proof of its own, at most [steps]
    steps (default [MaxSteps + 1]), and is its proof term with the evar map
    it was found in; [None] if [steps] ran out first. With [debug], the
    search's open goals are printed when it stops without a proof.

    @raise CErrors.UserError
      if the search ends without a proof and [steps] was not given (raised
      here), and whatever [Proof_solver.init] and [Proof_solver.solve] raise
      (propagated). *)
let search
      ?(steps : int option)
      ~(debug : bool)
      (kind : kind)
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (concl : EConstr.t)
      (lts_m : Names.inductive)
      (lts_n : Names.inductive)
      (x : EConstr.t)
      (y : EConstr.t)
      (a : EConstr.t)
  : (EConstr.t * Evd.evar_map) option
  =
  let refs = List.map qualid_of_ind (using env [ lts_m; lts_n ]) in
  let pstate =
    Declare.Proof.start
      ~info:(Declare.Info.make ())
      ~cinfo:
        (Declare.CInfo.make
         (* this proof is never saved; without a location Rocq warns
            "Using fallback loc" at every use *)
           ~loc:(Loc.initial Loc.ToplevelInput)
           ~name:(Names.Id.of_string ("mebi_" ^ name kind))
           ~typ:concl
           ())
      ((Evd.from_ctx [@alert "-deprecated"]) (Evd.ustate sigma))
  in
  let pstate =
    with_weak_args (extern env sigma a) (fun () ->
      let pstate =
        Proof_solver.init
          pstate
          refs
          (extern env sigma x, qualid_of_ind lts_m)
          (extern env sigma y, qualid_of_ind lts_n)
      in
      (* [MaxSteps n] permits [n + 1] steps, [bisim n] [n]: [solve ~bound]
         permits [bound + 1] *)
      match steps with
      | None -> Proof_solver.solve ~bound:!Api.the_max_steps pstate
      | Some 0 -> pstate
      | Some n -> Proof_solver.solve ~bound:(n - 1) ~explain:false pstate)
  in
  let p = Declare.Proof.get pstate in
  if Proof.is_done p
  then (
    let { Proof.sigma = sigma'; _ } = Proof.data p in
    match Proof.partial_proof p with
    | [ t ] -> Some (Evarutil.nf_evar sigma' t, sigma')
    | _ -> fail kind "the proof search left more than one proof term.")
  else (
    if debug
    then
      Feedback.msg_notice
        Pp.(
          str
            (Printf.sprintf
               "debug %s: the proof search stopped without a proof. Its open \
                goals:"
               (name kind))
          ++ fnl ()
          ++ open_goals p);
    match steps with
    | Some n ->
      (* as [auto n]: no proof, no change *)
      Logger.notice
        (Printf.sprintf
           "(%s %i: no proof within %i steps; the goal is left as it was.%s)"
           (name kind)
           n
           n
           (if debug
            then ""
            else " [debug " ^ name kind ^ " <n>] shows where the search is."));
      None
    | None ->
      fail
        kind
        (Printf.sprintf
           "no proof within the step bound, MaxSteps = %i (it permits %i \
            steps; [Set MeBi MaxSteps <n>] raises it, and [debug %s <n>] shows \
            where the search is after <n> steps)."
           !Api.the_max_steps
           (!Api.the_max_steps + 1)
           (name kind)))
;;

(* See the [.mli]. *)
let prove ?(steps : int option) ?(debug : bool = false) (kind : kind)
  : unit Proofview.tactic
  =
  Proofview.Goal.enter (fun gl ->
    let env = Proofview.Goal.env gl in
    let sigma = Proofview.Goal.sigma gl in
    let concl = Proofview.Goal.concl gl in
    let h, args = EConstr.decompose_app sigma concl in
    let other = match kind with Bisim -> Sim | Sim -> Bisim in
    if EConstr.eq_constr sigma h (Mebi_theories.get (head other))
    then
      fail
        kind
        (Printf.sprintf "the goal is [%s]: use [%s]." (head other) (name other));
    if Bool.not (EConstr.eq_constr sigma h (Mebi_theories.get (head kind)))
    then fail kind (Printf.sprintf "the goal is not [%s]." (head kind));
    if
      Bool.not (EConstr.Vars.closed0 sigma concl)
      || Bool.not (Names.Id.Set.is_empty (Termops.collect_vars sigma concl))
      || Bool.not (Evar.Set.is_empty (Evd.evars_of_term sigma concl))
    then
      fail
        kind
        "the goal mentions local hypotheses or evars; it must be between \
         closed terms.";
    let ind (t : EConstr.t) : Names.inductive =
      match EConstr.kind sigma t with
      | Constr.Ind (i, _) when lts_shaped env i -> i
      | _ ->
        fail
          kind
          "each LTS must be an inductive relation [M -> option A -> M -> Prop] \
           without parameters."
    in
    match args with
    | [| _m; _n; a; lts_m; lts_n; x; y |] ->
      (match
         Proof_solver.guard (fun () ->
           search
             ?steps
             ~debug
             kind
             env
             sigma
             concl
             (ind lts_m)
             (ind lts_n)
             x
             y
             a)
       with
       | None -> Proofview.tclUNIT ()
       | Some (t, sigma') ->
         Proofview.tclTHEN
           (Proofview.Unsafe.tclEVARS
              ((Evd.merge_universe_context [@alert "-deprecated"])
                 sigma
                 (Evd.ustate sigma')))
           (Tactics.exact_no_check t))
    | _ -> fail kind (Printf.sprintf "[%s] is not fully applied." (head kind)))
;;
