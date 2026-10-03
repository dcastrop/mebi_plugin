(** Per-message-kind output settings live in [Logger]; what remains here is the
    part that was never about logging. [config_output] and [set_output] forward
    to [Logger.configure]. *)
type output_config =
  { mutable decode_results : bool
  ; mutable dump_results : bool
  }

val output_config_default : output_config
val the_output_config : output_config ref
val reset_output_config : unit -> unit
val config_output : bool -> Output.Kind.t -> unit
val output_config_decode_results : bool -> unit
val output_config_dump_results : bool -> unit
val set_output : bool -> string -> unit

(* *)
val make_enc : (module Encoding.Packed.PackedS) -> (module Encoding.S)
val make_enc_int : unit -> (module Encoding.S)

type fail_flags =
  { mutable empty : bool
  ; mutable incomplete : bool
  ; mutable non_bisimilar : bool
  ; mutable oversaturated : bool
    (** refuse to saturate past {!the_saturation_bound}, rather than warn *)
  }

val the_fail_flags_default : fail_flags
val the_fail_flags : fail_flags ref
val reset_the_fail_flags : unit -> unit

(** How the proof solver introduces its coinduction hypotheses: a fresh
    nested cofix per newly-seen pair, one mutual cofix over the whole
    precomputed product, or [Auto] to measure both and pick. [Auto] by
    default; it announces at [Notice] whenever it takes the mutual path. *)
type solver_strategy =
  | Nested
  | Mutual
  | Auto

val the_solver_strategy : solver_strategy ref

(** What {!the_solver_strategy} resolved to for the proof now being solved.
    The step machinery reads only this. *)
val the_mutual_cofix : bool ref

val set_solver_strategy : solver_strategy -> unit
val set_mutual_cofix : bool -> unit
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

val the_answer_policy : answer_policy ref
val set_answer_policy : answer_policy -> unit
val set_fail_flag_empty : bool -> unit
val set_fail_flag_incomplete : bool -> unit
val set_fail_flag_non_bisimilar : bool -> unit
val set_fail_flag_oversaturated : bool -> unit

type bounds_args =
  | States of int
  | Transitions of int

val default_bounds : bounds_args
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
val set_saturation_mode : saturation_mode -> unit
val reset_bounds_args : unit -> unit
val set_the_bounds_args : bounds_args -> unit
val default_saturation_bound : int
val the_saturation_bound : int ref
val set_saturation_bound : int -> unit
val set_premise_depth : int -> unit
val set_premise_tactic : unit Proofview.tactic -> unit
val reset_premise : unit -> unit

(** Measured heap per weak action of a saturated FSM, in bytes (low, high). *)
val bytes_per_weak_action : int * int

(** Measured peak memory per extracted state, in MB (low, high). *)
val mb_per_extracted_state : float * float

val human_bytes : int -> string

type weak_args =
  { a : weak_arg option
  ; b : weak_arg option
  }

and weak_arg =
  | Option of Constrexpr.constr_expr
  | Custom of Constrexpr.constr_expr * Libnames.qualid

val the_weak_args : weak_args ref option ref
val reset_weak_args : unit -> unit
val set_the_weak_args : weak_arg option -> weak_arg option -> unit
val get_the_weak_arg1 : unit -> weak_arg option
val get_the_weak_arg2 : unit -> weak_arg option
val set_the_weak_arg1 : weak_arg -> unit
val set_the_weak_arg2 : weak_arg -> unit
val reset_all : unit -> unit
