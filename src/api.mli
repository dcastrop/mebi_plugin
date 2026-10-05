(** The plugin's configuration, as the [MeBi Config] commands set it. Per
    message kind output settings live in {!Logger}; {!set_output} forwards
    to it. *)
type output_config =
  { mutable decode_results : bool
  ; mutable dump_results : bool
  }

(** [output_config_default ()]: a fresh default output configuration
    (results decoded, not dumped to [./_dumps/]). A fresh record each time,
    as its fields are mutable: the [MeBi Config Output] setters update the
    current one in place. *)
val output_config_default : unit -> output_config

(** The output configuration now in force ({!output_config_default} until
    changed). *)
val the_output_config : output_config ref

(** [reset_output_config ()] restores the default output configuration
    and {!Logger}'s per-kind settings. Raises nothing. *)
val reset_output_config : unit -> unit

(** [output_config_decode_results b] sets whether results are decoded
    back to Rocq terms when printed. Raises nothing. *)
val output_config_decode_results : bool -> unit

(** [output_config_dump_results b] sets whether each FSM and result is
    written as JSON into [./_dumps/]. Raises nothing. *)
val output_config_dump_results : bool -> unit

(** [set_output b k] sets the output option named [k] to [b]
    ([MeBi Config Output k b]): [DecodeResults], [DumpResults], or a
    message kind ({!Output.Kind.of_string}), forwarded to
    {!Logger.configure}. An unknown name is warned about and ignored.
    Raises nothing. *)
val set_output : bool -> string -> unit

(** [make_enc p] is the encoding module unpacked from [p]. Raises
    nothing. *)
val make_enc : (module Encoding.Packed.PackedS) -> (module Encoding.S)

(** [make_enc_int ()] is the integer encoding, the one the plugin uses.
    Raises nothing. *)
val make_enc_int : unit -> (module Encoding.S)

(** Which outcomes are errors rather than warnings ([MeBi Config FailIf]):
    an empty LTS, an incomplete one, a negative bisimilarity verdict, a
    saturation above {!the_saturation_bound}. *)
type fail_flags =
  { mutable empty : bool
  ; mutable incomplete : bool
  ; mutable non_bisimilar : bool
  ; mutable oversaturated : bool
    (** refuse to saturate past {!the_saturation_bound}, rather than warn *)
  }

(** The default {!fail_flags}: all errors except an empty LTS. *)
val the_fail_flags_default : fail_flags

(** The {!fail_flags} now in force. *)
val the_fail_flags : fail_flags ref

(** [reset_the_fail_flags ()] restores {!the_fail_flags_default}. Raises
    nothing. *)
val reset_the_fail_flags : unit -> unit

(** How the proof solver introduces its coinduction hypotheses: a fresh
    nested cofix per newly-seen pair, one mutual cofix over the whole
    precomputed product, or [Auto] to measure both and pick. [Auto] by
    default; it announces at [Notice] whenever it takes the mutual path. *)
type solver_strategy =
  | Nested
  | Mutual
  | Auto

(** The {!solver_strategy} chosen by [MeBi Config Solver MutualCofix]. *)
val the_solver_strategy : solver_strategy ref

(** What {!the_solver_strategy} resolved to for the proof now being solved.
    The step machinery reads only this. *)
val the_mutual_cofix : bool ref

(** [set_solver_strategy s] chooses [s], and sets {!the_mutual_cofix} to
    match until a proof resolves [Auto]. Raises nothing. *)
val set_solver_strategy : solver_strategy -> unit

(** [set_mutual_cofix b] records what {!the_solver_strategy} resolved to
    for the proof now being solved. Raises nothing. *)
val set_mutual_cofix : bool -> unit

(** [reset_mutual_cofix ()] restores [Auto], unresolved. Raises nothing. *)
val reset_mutual_cofix : unit -> unit

(** How the proof solver chooses each answer (see
    [Model.Product.Policy]): [Default] as it always has; [Greedy] and
    [Minimal] from a plan built at [MeBi Sim Begin]; [Auto] the plan with
    the lowest predicted cost. [Default] by default. *)
type answer_policy =
  | Answers_default
  | Answers_greedy
  | Answers_minimal
  | Answers_auto

(** The {!answer_policy} chosen by [MeBi Config Solver Answers]. *)
val the_answer_policy : answer_policy ref

(** [set_answer_policy p] chooses [p]. Raises nothing. *)
val set_answer_policy : answer_policy -> unit

(** [set_fail_flag_empty b] sets whether an empty LTS is an error, and
    shows the change. Raises nothing. *)
val set_fail_flag_empty : bool -> unit

(** [set_fail_flag_incomplete b] sets whether an incomplete LTS (exploration
    cut short, or an approximation) is an error, and shows the change.
    Raises nothing. *)
val set_fail_flag_incomplete : bool -> unit

(** [set_fail_flag_non_bisimilar b] sets whether a negative bisimilarity
    verdict is an error, and shows the change. Raises nothing. *)
val set_fail_flag_non_bisimilar : bool -> unit

(** [set_fail_flag_oversaturated b] sets whether saturating past
    {!the_saturation_bound} is refused (an error) rather than warned about,
    and shows the change. Raises nothing. *)
val set_fail_flag_oversaturated : bool -> unit

(** How far exploration may go: at most this many states, or
    transitions. *)
type bounds_args =
  | States of int
  | Transitions of int

(** The default {!bounds_args}: 100 states. *)
val default_bounds : bounds_args

(** The {!bounds_args} now in force ([MeBi Config Bounds]). *)
val the_bounds_args : bounds_args ref

(** How the bisimilarity check saturates each FSM: whole, on demand with
    the partition on the silent-SCC quotient, or ([Saturation_auto], the
    default) on demand only above the saturation bound, with a warning. See
    [notes/13]. Reset by [Reset Bounds]. *)
type saturation_mode =
  | Saturation_whole
  | Saturation_on_demand
  | Saturation_auto

val the_saturation_mode : saturation_mode ref

(** The most pairs a proof's up-front game walk may visit on demand
    ([MeBi Config Bounds Game <n>]); [None] (the default) refuses such walks.
    Reset by [Reset Bounds]. *)
val the_game_bound : int option ref

(** [set_game_bound n] sets {!the_game_bound} to [n] pairs, and shows the
    change. Raises nothing. *)
val set_game_bound : int -> unit

(** [set_saturation_mode m] sets {!the_saturation_mode}, and shows the
    change. Raises nothing. *)
val set_saturation_mode : saturation_mode -> unit

(** [reset_bounds_args ()] restores the default exploration and saturation
    bounds, the game bound, the saturation mode, and the premise search's
    depth and range ([MeBi Reset Bounds]). Raises nothing. *)
val reset_bounds_args : unit -> unit

(** [set_the_bounds_args b] sets {!the_bounds_args}, shows the change, and,
    for a state bound whose extraction may need a gigabyte or more
    ({!mb_per_extracted_state}), says so at [Notice]. Raises nothing. *)
val set_the_bounds_args : bounds_args -> unit

(** The default {!the_saturation_bound}: 1,000,000 weak actions. *)
val default_saturation_bound : int

(** The most weak actions saturation may produce before the plugin refuses
    (or, with [FailIf Oversaturated False], warns). *)
val the_saturation_bound : int ref

(** [set_saturation_bound n] sets {!the_saturation_bound} to [n], and shows
    the change. Raises nothing. *)
val set_saturation_bound : int -> unit

(** [set_premise_depth n] sets the premise search's depth
    ({!Premise_search.max_depth}), and shows the change. Raises nothing. *)
val set_premise_depth : int -> unit

(** [set_premise_range x]: decide bounded universal premises
    ([forall k, k < n -> P k]) ranging over at most [x] values of [k]. *)
val set_premise_range : int -> unit

(** [set_premise_tactic t] sets the tactic tried on premises the search
    leaves undecided ({!Premise_search.user_tactic}), and shows the change.
    Raises nothing. *)
val set_premise_tactic : unit Proofview.tactic -> unit

(** [reset_premise ()] restores the premise search's depth, range and
    tactic, and shows the change ([MeBi Reset Premise]). Raises nothing. *)
val reset_premise : unit -> unit

(** Measured heap per weak action of a saturated FSM, in bytes (low, high). *)
val bytes_per_weak_action : int * int

(** Measured peak memory per extracted state, in MB (low, high). *)
val mb_per_extracted_state : float * float

(** [human_bytes n] is [n] bytes as a short string in KB, MB or GB.
    Raises nothing. *)
val human_bytes : int -> string

(** The silent labels given to [Run Bisim]/[Sim Begin] for each of the two
    systems: [Option] for [None] as tau, or [Custom (tau, label_type)]. *)
type weak_args =
  { a : weak_arg option
  ; b : weak_arg option
  }

and weak_arg =
  | Option of Constrexpr.constr_expr
  | Custom of Constrexpr.constr_expr * Libnames.qualid

(** The {!weak_args} given, if any. *)
val the_weak_args : weak_args ref option ref

(** [reset_weak_args ()] forgets {!the_weak_args}. Raises nothing. *)
val reset_weak_args : unit -> unit

(** [set_the_weak_args a b] sets both systems' silent labels. Raises
    nothing. *)
val set_the_weak_args : weak_arg option -> weak_arg option -> unit

(** [get_the_weak_arg1 ()] is the first system's silent label, if set.
    Raises nothing. *)
val get_the_weak_arg1 : unit -> weak_arg option

(** [get_the_weak_arg2 ()] is the second system's silent label, if set.
    Raises nothing. *)
val get_the_weak_arg2 : unit -> weak_arg option

(** [set_the_weak_arg1 w] sets the first system's silent label, keeping the
    second. Raises nothing. *)
val set_the_weak_arg1 : weak_arg -> unit

(** [set_the_weak_arg2 w] sets the second system's silent label, keeping
    the first. Raises nothing. *)
val set_the_weak_arg2 : weak_arg -> unit

(** [reset_all ()] restores every setting here to its default ([MeBi Reset Config]), and shows it. Raises nothing.
*)
val reset_all : unit -> unit
