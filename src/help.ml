(** [MeBi Help]. Topics are paths, e.g. [["Sim"]] for [MeBi Help Sim]. *)

(* See the [.mli]. *)
let topics : string list list = [ [ "Sim" ]; [ "Debug" ]; [ "Output" ] ]

(** [name p] is the command that shows the topic [p]. Raises nothing. *)
let name (path : string list) : string = String.concat " " ("MeBi Help" :: path)

(** [overview ()] is the help overview: what MeBi does and every topic.
    Raises nothing. *)
let overview () : string =
  String.concat
    "\n"
    ([ "MeBi: (bi)simulation goals between Rocq terms of inductive LTSs, weak \
        or strong (sim, bisim, weak_sim, weak_bisim): decided on Sylvan's \
        decision diagrams; Sylvan's own computation, logged, is the \
        certificate, whose facts Rocq's kernel typechecks."
     ; ""
     ; "Help topics:"
     ]
     @ List.map (fun p -> "  " ^ name p ^ ".") topics
     @ [ ""; "See also README.md." ])
;;

(* See the [.mli]. *)
let text : string list -> string option = function
  | [] -> Some (overview ())
  | [ "Sim" ] ->
    Some
      "sim.  bisim.  weak_sim.  weak_bisim.   (each with debug in front)\n\
       MeBi Prove <id> : <goal>.\n\n\
       Each reads its goal: strong_sim, strong_bisimilar, weak_sim or \
       weak_bisimilar between two closed terms of inductive LTSs (relations M \
       -> option A -> M -> Prop, without parameters; for the weak ones, None \
       is the silent step). The engine learns each rule's steps on the \
       positions it reads, explores both systems on Sylvan's decision \
       diagrams, and plays the (weak or strong) game to its greatest fixpoint. \
       A negative verdict is an error. On a positive one, Sylvan logs the \
       decision's last checks, the certificate, and the tactic closes the goal \
       with the proof built from it (doc/certificate.md); end with Qed, where \
       the kernel checks that proof. With Set MeBi Log Mode \"keep\" the log \
       stays in .<File>.v.mebi/<lemma>.log next to the file, for MeBi Check \
       Log. MeBi Prove <id> : <goal> does the same for a goal given as a \
       statement, and declares the lemma <id>; each piece of its proof is a \
       constant the kernel checks when it is declared, so a large goal needs \
       far less memory than with a tactic (whose pieces Rocq keeps until Qed)."
  | [ "Debug" ] ->
    Some
      "debug sim | bisim | weak_sim | weak_bisim.\n\
       Print MeBi Rules <lts>.  Print MeBi Skeleton <term> Using <lts>.\n\
       MeBi Congruence <lts> <rule> ...\n\
       Set MeBi Log Mode \"off\" | \"keep\".  Set MeBi Log Directory \
       \"<dir>\".  MeBi Check Log \"<file>\".\n\
       MeBi Message \"<s>\".  MeBi Debug \"<s>\".  MeBi Divider [\"<s>\"].\n\n\
       debug <tactic> does as the tactic does, and first prints the engine's \
       report: states, pairs, summands, learned projections, game iterations, \
       time. Rules and Skeleton show the engine's view (doc/learn.md): an \
       LTS's constructors read as rules, and a start term's shapes, slots and \
       summands (doc/skeleton.md), and the rules taken as its structural \
       congruence: the reorderings found in the rules (no premise, silent, the \
       pattern's variables only reordered, each with its inverse: commutation, \
       associativity, a swap), or those MeBi Congruence names by hand \
       (doc/upto.md). MeBi Log Mode: the decision's last checks, as Sylvan \
       computes them, are kept in memory (off) or (keep) written to \
       .<File>.v.mebi/<Lemma>.log next to the file (Log Directory: where \
       .<File>.v.mebi goes instead). MeBi Check Log \"<file>\" (relative to \
       the file) turns a kept log's records (unions, differences, images, \
       images, each the one way the game's proof uses it) into facts, each \
       chunk a constant the kernel checks. Message, Debug and Divider print a \
       line (Debug's only under Set Debug \"mebi\")."
  | [ "Output" ] ->
    Some
      "Set Debug \"mebi\" | \"mebi-trace\" | \"mebi-info\".   (Set Debug \
       \"-mebi\" ... off)\n\
       Set Warnings \"-mebi\".   (hide MeBi's warnings; \"+mebi\" makes them \
       errors)\n\n\
       Notices and errors are always shown. Debug output is off unless a debug \
       flag is set: mebi (internal debugging), mebi-trace (every function \
       entered), mebi-info (routine progress). MeBi's warnings are the Rocq \
       warning mebi."
  | _ -> None
;;

(* See the [.mli]. *)
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
