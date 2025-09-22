open Logging

type help_set_kind =
  | General of unit
  | Bound of unit
  | DumpToFile of unit
  | ShowDebug of unit
  | ShowDetails of unit
  | WeakMode of unit
  | Weak of unit

type help_kind =
  | Basic of unit
  | Set of help_set_kind
  | Check of unit
  | LTS of unit
  | FSM of unit
  | Saturate of unit
  | Minimize of unit
  | Bisim of unit
  | Info of unit
  | Unrecognized of unit

let show_instructions_to_toggle_weak (weak_mode : bool) : unit =
  Log.debug
    (if weak_mode
     then
       "Checking for Weak Bisimilarity.\n\
        To check for Strong Bisimilarity use the command \"MeBi Set WeakMode \
        False\""
     else
       "Checking for Strong Bisimilarity (since weak mode is disabled)\n\
        To Check for Weak Bisimilarity use the command \"MeBi Set WeakMode \
        True\"")
;;

let show_instructions_to_enable_weak () : unit =
  Log.notice
    "Weak mode is not currently enabled.\n\
     Use the command \"MeBi Set WeakMode True\" to enable it (and \"MeBi Set \
     WeakMode False\" to disable it).\n"
;;

let show_instructions_to_set_weak () : unit =
  Log.notice
    "Cannot saturate without any silent actions.\n\
     Use the command \"MeBi Set Weak ...\" to specify how you are encoding \
     silent transitions.\n\
     For example:\n\
    \  1) \"MeBi Set Weak Option nat\" is for labels of type \"option nat\", \
     where silent actions are \"None\" and visible actions are \"Some _\"\n\
    \  2) \"MeBi Set Weak TAU of ACTION\" is for labels of inductive type \
     \"ACTION\" that has some constructor \"TAU\"\n"
;;

let show_help_basic () : unit =
  Log.notice
    "All commands for this plugin begin with \"MeBi\" and are followed one of \
     the following:\n\
     Set, Check, LTS, FSM, Saturate, Minimize, Bisim.\n\n\
     Use the command \"MeBi Help x\" for more information\n\
     (where x is one of the terms above)\n"
;;

let show_help_unrecognized_command () : unit =
  Log.notice "Command not recognized.\n";
  show_help_basic ()
;;

let show_help_set () : unit =
  Log.notice
    (Printf.sprintf
       "Certain commands set parameters the plugin uses when running other \
        commands.\n\
        These parameters are: Bound, DumpToFile, ShowDebug, WeakMode, Weak\n\n\
        Use the command \"MeBi Help Set x\" for more information\n\
        (where x is one of the parameters above)\n")
;;

let show_help_set_bound () : unit =
  Log.notice
    (Printf.sprintf
       "Use the command \"MeBi Set Bound n\" to set the upper-bound of the \
        number of unique states that can be reached when building the LTS from \
        a coq term, where \"n\" is an natural integer number.\n\n\
        Use command \"MeBi Set Bound Default\" to reset this to the default, \
        which is: %i"
       Params.default_bound)
;;

let show_help_set_dump_to_file () : unit =
  Log.notice
    "Use the command \"MeBi Set DumpToFile True\" to enable the internal \
     results (and/or models) of any command run to a (JSON) file.\n\
     Disable this behaviour using \"MeBi Set DumpToFile False\"\n"
;;

let show_help_set_show_debug () : unit =
  Log.notice
    "Use the command \"MeBi Set ShowDebug True\" to enable debug messages.\n\
     Disable them using \"MeBi Set ShowDebug False\"\n"
;;

let show_help_set_show_details () : unit =
  Log.notice
    "Use the command \"MeBi Set ShowDetails True\" to enable additional detail \
     messages.\n\
     Disable them using \"MeBi Set ShowDetails False\"\n"
;;

let show_help_set_weak_mode () : unit =
  Log.notice
    "Use the command \"MeBi Set WeakMode True\" to enable it (and \"MeBi \
     WeakMode False\" to disable it).\n"
;;

let show_help_set_weak () : unit =
  Log.notice
    "Use the command \"MeBi Set Weak ...\" to specify how you are encoding \
     silent transitions.\n\
     For example:\n\
    \  1) \"MeBi Set Weak Option nat\" is for labels of type \"option nat\", \
     where silent actions are \"None\" and visible actions are \"Some _\"\n\
    \  2) \"MeBi Set Weak TAU of ACTION\" is for labels of inductive type \
     \"ACTION\" that has some constructor \"TAU\"\n"
;;

let show_help_check () : unit =
  Log.notice
    "Use the command \"MeBi See x\" to see what any of Bound, DumpToFile, \
     ShowDebug, WeakMode, Weak are set to.\n\
     Use the command \"MeBi See All\" to see for them all.\n"
;;

let show_help_lts () : unit = Log.notice "Use the command \"MeBi LTS ...\"\n"
let show_help_fsm () : unit = Log.notice "Use the command \"MeBi FSM ...\"\n"

let show_help_saturate () : unit =
  Log.notice "Use the command \"MeBi Saturate ...\"\n"
;;

let show_help_minimize () : unit =
  Log.notice "Use the command \"MeBi Minimize ...\"\n"
;;

let show_help_bisim () : unit =
  Log.notice "Use the command \"MeBi Bisim ...\"\n"
;;

let show_help_info () : unit =
  Log.notice
    "Use the command \"MeBi Info\" to be shown information about the plugin, \
     including a some guidelines and a list of limitations."
;;

let show_guidelines_and_limitations () : unit =
  Log.notice
    "Guidelines & Limitations\n\n\
     - To construct the LTS from a term, the term must be an inductive type of \
     the shape:\n\
     \tterm -> label -> term -> Prop\n\
     where \"term\" and \"label\" are defined elsewhere.\n\n\
     - The \"label\" type must be of either Type or Set.\n\n\
     - The plugin supports multi-layered LTS definitions, where the \
     constructors of the inductive type have premises that are either \
     recursive or reference a different inductive LTS proposition type.\n\n\
     - The premises of the constructors in the inductive LTS proposition types \
     must only contain other propositions and not, e.g., equalities.\n\
     In cases such as equality, you must simply \"inline\" it by substitution.\n\
     For example, instead of writing:\n\
     \tInductive lts : nat -> bool -> nat -> Prop :=\n\
     \t| do_action : forall a t t', t' = some_fun t -> lts t a t'\n\n\
     You must write:\n\
     \tInductive lts : nat -> bool -> nat -> Prop :=\n\
     \t| do_action : forall a t, lts t a (some_fun t)\n\n\
     - \n\n\
     -----------\n\n\
     TODO:\n\
     - Make \"weak_type\" a Set, so that different LTS can have different \
     kinds of silent actions and be more easily compared, and for different \
     layers of a multi-layered LTS to have different silent actions. Then, \
     instead of calling \"MkGraph.handle_weak\" before we begin building the \
     LTS, in \"MkGraph.get_new_states\" we must check if any of the outgoing \
     actions appear in the Set of \"weak_type\"."
;;

let handle_help (c : help_kind) : unit =
  match c with
  | Basic () -> show_help_basic ()
  | Set s ->
    (match s with
     | General () -> show_help_set ()
     | Bound b -> show_help_set_bound b
     | DumpToFile () -> show_help_set_dump_to_file ()
     | ShowDebug () -> show_help_set_show_debug ()
     | ShowDetails () -> show_help_set_show_details ()
     | WeakMode () -> show_help_set_weak_mode ()
     | Weak () -> show_help_set_weak ())
  | Check () -> show_help_check ()
  | LTS () -> show_help_lts ()
  | FSM () -> show_help_fsm ()
  | Saturate () -> show_help_saturate ()
  | Minimize () -> show_help_minimize ()
  | Bisim () -> show_help_bisim ()
  | Info () -> show_help_info ()
  | Unrecognized () -> show_help_unrecognized_command ()
;;
