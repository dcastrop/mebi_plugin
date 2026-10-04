(** [MeBi Help] (backlog item F). Topics are paths, e.g. [["Config"; "Bounds"; "Saturation"]] for [MeBi Help Config Bounds Saturation]. The
    figures quoted are computed from the same constants the plugin's errors
    and notices use ([Api.bytes_per_weak_action],
    [Api.mb_per_extracted_state], [Api.default_saturation_bound]), so the
    help cannot drift from what the plugin actually reports. *)

let topics : string list list =
  [ [ "Run" ]
  ; [ "Sim" ]
  ; [ "Benchmark" ]
  ; [ "Premises" ]
  ; [ "Config" ]
  ; [ "Config"; "Bounds" ]
  ; [ "Config"; "Bounds"; "Saturation" ]
  ; [ "Config"; "Saturation" ]
  ; [ "Config"; "Weak" ]
  ; [ "Config"; "FailIf" ]
  ; [ "Config"; "Solver" ]
  ; [ "Config"; "Output" ]
  ]
;;

let name (path : string list) : string = String.concat " " ("MeBi Help" :: path)

let overview () : string =
  String.concat
    "\n"
    ([ "MeBi: build the LTS of a Rocq term from an inductive relation, check \
        (weak) bisimilarity, and search for weak_sim and weak_bisimilar \
        proofs."
     ; ""
     ; "Help topics:"
     ]
     @ List.map (fun p -> "  " ^ name p ^ ".") topics
     @ [ ""; "See also README.md." ])
;;

let saturation_table () : string =
  let lo, hi = Api.bytes_per_weak_action in
  let row (n : int) : string =
    Printf.sprintf
      "  %12i weak actions   %s - %s%s"
      n
      (Api.human_bytes (n * lo))
      (Api.human_bytes (n * hi))
      (if Int.equal n Api.default_saturation_bound then "   (default)" else "")
  in
  String.concat
    "\n"
    (List.map row [ 1_000_000; 5_000_000; 10_000_000; 20_000_000 ])
;;

let text : string list -> string option = function
  | [] -> Some (overview ())
  | [ "Run" ] ->
    Some
      "MeBi Run LTS <term> Using <lts> [<lts>...].\n\
       MeBi Run FSM | Saturate | Minimize <term> Using <lts> [<lts>...].\n\
       MeBi Run Bisim <term> With <lts> And <term> With <lts> Using <lts>...\n\
       MeBi Run Merge <term> With <lts> And <term> With <lts> Using <lts>...\n\
       MeBi Run Bisim <term> With <lts> And <term> With <lts> As Bisim|Sim \
       <name> [Using <lts>...]\n\n\
       Build the LTS reachable from <term>, then optionally saturate it (weak \
       transitions across silent steps), minimize it, or check two for (weak) \
       bisimilarity. The first relation after Using is the one <term> steps \
       by; the rest are the relations its constructors' premises mention \
       (layered LTSs). With As Bisim <name> (or As Sim <name>), Run Bisim also \
       states weak_bisimilar (or weak_sim) as the Example <name>, opens its \
       proof and begins the proof search, as MeBi Sim Begin would: continue \
       with MeBi Sim Solve <n>, then Qed. Nothing opens if the two are not \
       bisimilar (not similar, for Sim). See: MeBi Help Premises, MeBi Help \
       Sim."
  | [ "Sim" ] ->
    Some
      "MeBi Sim Begin <lts> <term> And <lts> <term> Using <lts>...\n\
       MeBi Sim Step.  MeBi Sim Solve <n>.\n\n\
       Inside a proof of [weak_sim lts1 lts2 t1 t2] (one direction) or \
       [weak_bisimilar lts1 lts2 t1 t2] (weak bisimilarity, both directions in \
       one proof): Begin computes both LTSs and their bisimilarity, then \
       Step/Solve run the proof search. Solve n permits n + 1 steps and stops \
       as soon as the proof closes. If it runs out it says which cofix \
       strategy was used; see MeBi Help Config Solver. A weak_bisimilar proof \
       needs the mutual cofix, which Auto picks for it. For weak_sim, the two \
       states need only be similar: if they are not bisimilar, Begin checks \
       the weak simulation preorder instead and refuses only if they are not \
       similar."
  | [ "Benchmark" ] ->
    Some
      "MeBi Benchmark LTS <min> <max> <term> Using <lts> [<lts>...].\n\n\
       Times repeated LTS construction over a range of sizes."
  | [ "Premises" ] ->
    Some
      (Printf.sprintf
         "Which constructor shapes are supported. An LTS is an inductive \
          relation [term -> label -> term -> Prop]. For each constructor, the \
          source term is matched, then its premises are handled:\n\
         \  - premises over an LTS given in Using (the same one, or another: \
          layered LTSs) are explored; several are allowed;\n\
         \  - any other premise is decided once it is closed (immediately, or \
          after the LTS premises fix what it mentions): an equation by \
          comparing its sides, any other inductive proposition (<=, <, In, \
          Forall, /\\, \\/, ...) by a proof search over its constructors, at \
          most %i deep (MeBi Config Premise Depth <n>). A false premise drops \
          the transition; in a proof, a true one is closed with the proof \
          found, and a false one in a hypothesis is refuted. A binder only \
          such premises mention ([In q l -> P q -> ...], no LTS step on [q]) \
          is chosen in the proof by solving those premises together;\n\
         \  - a premise that computes something -- the target, or what an LTS \
          premise needs ([In q l -> lts q a q']) -- is enumerated, each \
          solution a transition of its own; MeBi warns if the solutions may be \
          incomplete. An LTS premise whose source nothing determines ([lts q a \
          q'] with [q] free) is enumerated the same way, and each source found \
          explored;\n\
         \  - a transition with a binder nothing determines ([lts (S n) a n], \
          [n] free) may stand for infinitely many: it is left out, with a \
          warning, so the LTS may be missing transitions;\n\
         \  - an LTS with any such warning is incomplete: an error, as when \
          exploration hits the bound, unless [MeBi Config FailIf Incomplete \
          False];\n\
         \  - a negation [~ P] holds iff P is refuted by a complete search, \
          and is proved by refuting P;\n\
         \  - a bounded universal over nat, [forall k, k < n -> P k] or [k <= \
          n] (also [n > k]), [n] a number once the premise is closed, holds \
          iff every [P i] does: each is decided as above, and the proof is \
          assembled from theirs with the lemmas in MEBI.Premises; a false one \
          is refuted at a false [P i]. Only up to %i values of [k] (MeBi \
          Config Premise Range <n>): each costs a search, and the proof grows \
          with the square of the range; a wider one is left undecided, with a \
          warning saying so;\n\
         \  - with [MeBi Config Premise Tactic <tactic>], a premise the search \
          leaves undecided is tried with that tactic (proving it, or its \
          negation);\n\
         \  - anything else cannot be decided -- opaque functions or axioms, a \
          search cut off by the depth: the constructor is applied as if the \
          premise held, with a warning, so the LTS may contain transitions \
          that do not exist. A Run Bisim verdict on it may be wrong; a proof \
          cannot be, since Qed checks the premise."
         !Premise_search.max_depth
         !Premise_search.max_range)
  | [ "Config" ] ->
    Some
      "MeBi Config Reset [Bounds | Weak | FailIf | Output].\n\n\
       Settings, each with its own topic:\n\
      \  MeBi Help Config Bounds.     how big an LTS (and its saturation) may \
       get\n\
      \  MeBi Help Config Saturation. saturating whole, or on demand\n\
      \  MeBi Help Config Weak.       which label is silent (weak bisimilarity)\n\
      \  MeBi Help Config FailIf.     which outcomes are errors, not warnings\n\
      \  MeBi Help Config Solver.     the proof search's cofix strategy\n\
      \  MeBi Help Config Output.     which messages are shown"
  | [ "Config"; "Bounds" ] ->
    let lo, hi = Api.mb_per_extracted_state in
    Some
      (Printf.sprintf
         "MeBi Config Bounds As Num States <n>.\n\
          MeBi Config Bounds As Num Transitions <n>.\n\
          MeBi Config Bounds Saturation <n>.\n\
          MeBi Config Bounds Game <n>.\n\
          MeBi Config Premise Depth <n>.\n\
          MeBi Config Premise Range <n>.\n\
          MeBi Config Reset Bounds.\n\n\
          Exploration stops after <n> states (default %s) or transitions; an \
          LTS cut short is an error unless [MeBi Config FailIf Incomplete \
          False]. Extraction has measured %.2f-%.2fMB per state on top of a \
          fixed 0.1-0.3GB, so a state bound past ~%i prints a memory notice. \
          Logging the result (Output \"Result\" / \"DecodeResults\" / \
          \"DumpResults\") costs far more, ~0.65MB per state. For the \
          saturation bound see MeBi Help Config Bounds Saturation; for the \
          premise search depth and range, MeBi Help Premises."
         (match Api.default_bounds with
          | Api.States n -> Printf.sprintf "%i states" n
          | Api.Transitions n -> Printf.sprintf "%i transitions" n)
         lo
         hi
         (int_of_float (1000. /. hi)))
  | [ "Config"; "Bounds"; "Saturation" ] ->
    let lo, hi = Api.bytes_per_weak_action in
    Some
      (Printf.sprintf
         "MeBi Config Bounds Saturation <n>.   (default %i)\n\n\
          The most weak actions saturation may produce. Saturation (used by \
          Run Saturate, Minimize, Bisim and Sim Begin when a label is silent) \
          can be orders of magnitude larger than the LTS: one weak action per \
          (state, label, state reachable by tau* label tau*). Before \
          saturating, MeBi computes that number exactly and cheaply. Above the \
          bound, Run Bisim and Sim Begin saturate on demand instead, with a \
          warning (MeBi Help Config Saturation), holding at most <n> weak \
          actions; Run Saturate and Minimize refuse with Saturation_Too_Large \
          (or warn, with MeBi Config FailIf Oversaturated False). Run [MeBi \
          Config Output \"Info\" True.] to see each estimate.\n\n\
          Measured memory is %i-%i bytes per weak action (the higher for \
          larger LTSs: each keeps its shortest witness path). Choose a bound \
          your machine can hold:\n\
          %s\n\n\
          Time grows faster than the count: a partial Proc/Test4 LTS with 400k \
          weak actions takes ~13s. Proc/Test4 itself (9720 states) would \
          saturate to 74.6M."
         Api.default_saturation_bound
         lo
         hi
         (saturation_table ()))
  | [ "Config"; "Saturation" ] ->
    Some
      "MeBi Config Saturation OnDemand True | False | Auto.   (default Auto)\n\
       MeBi Config Reset Bounds.   (restores Auto)\n\n\
       How Run Bisim and Sim Begin saturate each FSM. Whole: every weak action \
       up front, which for an LTS with large silent cycles can take far more \
       memory than the machine has (Proc/Test4: 74.6M weak actions, 34-67GB). \
       On demand: each state when the check or the proof needs it, from the \
       same code, so with the same weak actions and witnesses, holding at most \
       Bounds Saturation of them (the oldest dropped and saturated again if \
       needed); bisimilarity is decided on the quotient by silent strongly \
       connected components instead (states in one silent SCC have the same \
       weak moves; Proc/Test4's 9720 states make 81), with the same verdict. A \
       proof is the same proof, step for step; it may take longer, as dropped \
       states are saturated again.\n\n\
       Auto saturates on demand exactly the FSMs whose saturation would exceed \
       Bounds Saturation, and warns when it does; True does so for every FSM; \
       False refuses those FSMs (Saturation_Too_Large), or with FailIf \
       Oversaturated False saturates them whole anyway. Run Saturate and \
       Minimize always saturate whole.\n\n\
       On demand, planning the whole proof up front walks every pair the proof \
       can reach, saturating as it goes. So Solver MutualCofix True and any \
       Solver Answers but Default are refused unless MeBi Config Bounds Game \
       <n> is set (unset by default); with it, the walk runs within <n> pairs \
       and past it they are refused. Auto estimates within the bound, and \
       otherwise (or past it) takes the nested cofix."
  | [ "Config"; "Weak" ] ->
    Some
      "MeBi Config Weak As Option <type>.\n\
       MeBi Config Weak As <term> Of <type>.\n\
       MeBi Config Weak1 / Weak2 ...   (one side of Bisim/Merge/Sim only)\n\
       MeBi Config Reset Weak.\n\n\
       Mark the silent label: [As Option T] makes [None] silent in an [option \
       T]-labelled LTS; [As c Of T] makes constructor c of T silent. With a \
       silent label set, bisimilarity is weak and FSMs are saturated first."
  | [ "Config"; "FailIf" ] ->
    Some
      "MeBi Config FailIf Empty | Incomplete | NotBisimilar | Oversaturated \
       True | False.\n\
       MeBi Config Reset FailIf.\n\n\
       Whether an empty LTS (default False), an incomplete one (True), a \
       negative bisimilarity result (True), or a saturation above Bounds \
       Saturation (True) is an error rather than a warning. An LTS is \
       incomplete if exploration was cut short by the bound, or if it is only \
       an approximation: a premise MeBi could not decide (it may contain \
       transitions that do not exist), or a premise search or transition it \
       could not complete (it may be missing some). See MeBi Help Premises."
  | [ "Config"; "Solver" ] ->
    Some
      "MeBi Config Solver MutualCofix True | False | Auto.\n\n\
       How the proof search introduces coinduction hypotheses: False mints a \
       nested cofix per new pair; True opens the proof with one mutual cofix \
       over every pair it will reach; Auto (default) measures both on the \
       model before the proof starts and picks, announcing it when it takes \
       the mutual path.\n\n\
       MeBi Config Solver Answers Default | Greedy | Minimal | Auto.\n\n\
       How the proof search chooses each answer. Every step of a weak_sim or \
       weak_bisimilar proof has one system make a move (x -a-> x') and the \
       other answer it with a weak move to some y' such that (x', y') can be \
       proved related in turn. Usually several answers would do; which one is \
       taken decides the proof's next goals, and so its length.\n\n\
       Default -- the policy the solver has always used, and the default \
       setting -- chooses each answer on its own, with no look-ahead: stand \
       still if the move was silent and the answering state is already \
       bisimilar to x'; otherwise take the weak transition with the shortest \
       witness (fewest steps to justify) into the bisimilarity class of x', \
       ending at its lowest-numbered state (the one extraction discovered \
       first). For a weak_sim goal between states that are similar but not \
       bisimilar, the same two tries against the states that simulate x'. \
       Because no choice takes the others into account, a proof can visit many \
       more pairs than it needs -- weak_bisimilar proofs especially, whose two \
       directions each pick answers the other never uses.\n\n\
       Greedy, Minimal and Auto plan every answer at Begin, before the first \
       step, and Begin announces the plan and its predicted cost. Greedy \
       prefers an answer whose pair has already been reached. Minimal answers \
       within a relation that no pair can be removed from. Auto plans all \
       three and keeps the cheapest predicted. Measured on the checked-in \
       examples (2026-10-02), Auto was never slower than Default and cut \
       weak_sim proofs by 17% and weak_bisimilar proofs by 60%; Minimal alone \
       was slower on many weak_sim proofs. Every answer is still checked by \
       Qed, so a policy can only change how long a proof is, never whether it \
       is correct."
  | [ "Config"; "Output" ] ->
    Some
      "MeBi Config Output \"<Kind>\" True | False.\n\
       MeBi Config Reset Output.\n\n\
       Toggle one message kind: Debug, Info, Notice, Warning, Error, Trace, \
       Result, Show, DecodeResults, DumpResults."
  | _ -> None
;;

let show (path : string list) : unit =
  match text path with
  | Some t -> Feedback.msg_notice (Pp.str t)
  | None ->
    Feedback.msg_notice
      (Pp.str
         (Printf.sprintf
            "No help for [%s]. %s"
            (String.concat " " path)
            (overview ())))
;;
