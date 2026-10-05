(* module Defaults = struct
   module Log : Logger.S = Logger.Default
   module Ctx : Rocq_context.S = Rocq_context.Default
   module Enc : Encoding.S with type t = int = Encoding.Int (* module Tree = Enc_tree.Make (Enc) *)
   (* module Trees = Enc_trees.Make (Tree) *)
   end *)

(***********************************************************************)

(* Per-message-kind output settings now live in [Logger] itself rather than in
   a record here that a per-command [Logger.Make] closed over. What remains
   below is the part that was never about logging. *)

type output_config =
  { mutable decode_results : bool
  ; mutable dump_results : bool
  }

(** [dump_results] is off by default: dumping writes JSON files of every
    FSM and result into [./_dumps/] on each command (on [Proc/Test4]: ~20s
    and hundreds of MB a command), and disk writes are opt-in. It was on
    during active development (from [e53a386]) and turned off 2026-10-04;
    [MeBi Config Output "DumpResults" True] turns it on for debugging. *)
let output_config_default () : output_config =
  { decode_results = true; dump_results = false }
;;

(* See the [.mli]. *)
let the_output_config : output_config ref = ref (output_config_default ())

(* See the [.mli]. *)
let reset_output_config () : unit =
  the_output_config := output_config_default ();
  Logger.reset_config ()
;;

let config_output (x : bool) (k : Output.Kind.t) : unit = Logger.configure k x

(* See the [.mli]. *)
let output_config_decode_results (x : bool) : unit =
  !the_output_config.decode_results <- x
;;

(* See the [.mli]. *)
let output_config_dump_results (x : bool) : unit =
  !the_output_config.dump_results <- x
;;

(* See the [.mli]. *)
let set_output (x : bool) : string -> unit = function
  | "DecodeResults" -> output_config_decode_results x
  | "DumpResults" -> output_config_dump_results x
  | s ->
    (match Output.Kind.of_string s with
     | Some k -> Logger.configure k x
     | None ->
       Printf.sprintf
         "Unrecognised option \"%s\". Valid options are: Debug, Info, Notice, \
          Warning, Error, Trace, Result, Show, DecodeResults, DumpResults"
         s
       |> Logger.warning)
;;

(* See the [.mli]. *)
let make_enc (module X : Encoding.Packed.PackedS) : (module Encoding.S) =
  let module Enc : Encoding.S = Encoding.Packed.Unpack (X) in
  (module Enc : Encoding.S)
;;

(* See the [.mli]. *)
let make_enc_int () : (module Encoding.S) =
  (module (val make_enc (module Encoding.Packed.Int)) : Encoding.S)
;;

(***********************************************************************)

type fail_flags =
  { mutable empty : bool
  ; mutable incomplete : bool
  ; mutable non_bisimilar : bool
  ; mutable oversaturated : bool
  }

(* See the [.mli]. *)
let the_fail_flags_default : fail_flags =
  { empty = false
  ; incomplete = true
  ; non_bisimilar = true
  ; oversaturated = true
  }
;;

(* See the [.mli]. *)
let the_fail_flags : fail_flags ref = ref the_fail_flags_default

(* See the [.mli]. *)
let reset_the_fail_flags () : unit = the_fail_flags := the_fail_flags_default

(* See the [.mli]. *)
let set_fail_flag_empty (empty : bool) : unit =
  the_fail_flags := { !the_fail_flags with empty };
  Printf.sprintf "(MeBi Config: Set Fail-If 'Empty' Flag to: %b.)" empty
  |> Logger.show
;;

(* See the [.mli]. *)
let set_fail_flag_incomplete (incomplete : bool) : unit =
  the_fail_flags := { !the_fail_flags with incomplete };
  Printf.sprintf
    "(MeBi Config: Set Fail-If 'Incomplete' Flag to: %b.)"
    incomplete
  |> Logger.show
;;

(* See the [.mli]. *)
let set_fail_flag_non_bisimilar (non_bisimilar : bool) : unit =
  the_fail_flags := { !the_fail_flags with non_bisimilar };
  Printf.sprintf
    "(MeBi Config: Set Fail-If 'Non-bisimilar' Flag to: %b.)"
    non_bisimilar
  |> Logger.show
;;

(* See the [.mli]. *)
let set_fail_flag_oversaturated (oversaturated : bool) : unit =
  the_fail_flags := { !the_fail_flags with oversaturated };
  Printf.sprintf
    "(MeBi Config: Set Fail-If 'Oversaturated' Flag to: %b.)"
    oversaturated
  |> Logger.show
;;

(***********************************************************************)

type bounds_args =
  | States of int
  | Transitions of int

(* See the [.mli]. *)
let default_bounds : bounds_args = States 100

(* See the [.mli]. *)
let the_bounds_args : bounds_args ref = ref default_bounds

(** The most weak actions saturation may produce before the plugin refuses
    (or, with [FailIf Oversaturated False], warns): see
    [Wrapper.check_saturation_size]. *)
let default_saturation_bound : int = 1_000_000

let the_saturation_bound : int ref = ref default_saturation_bound

(** Heap per weak action of a saturated FSM, measured 2026-10-01 on
    Rocq-extracted examples: ~450 bytes on partial [Proc/Test4] LTSs (251 to
    1001 states), 550 to 860 bytes on partial [CADP/Size2] ones (500 to 2000
    states) -- each weak action carries its shortest witness path, which
    grows with the LTS. {i See [Wrapper.check_saturation_size].} *)
let bytes_per_weak_action : int * int = 450, 900

(* See the [.mli]. *)
let human_bytes (b : int) : string =
  let f : float = Float.of_int b in
  if f >= 1e9
  then Printf.sprintf "%.1fGB" (f /. 1e9)
  else if f >= 1e6
  then Printf.sprintf "%.0fMB" (f /. 1e6)
  else Printf.sprintf "%.0fKB" (f /. 1e3)
;;

(** How the bisimilarity check ([Run Bisim], [Sim Begin]) saturates each
    FSM (notes/13): whole, or one state at a time when asked about, with the
    partition computed on the quotient by silent SCCs instead. [Auto] (the
    default) saturates on demand exactly the FSMs whose saturation would
    exceed [the_saturation_bound], and warns when it does; [Whole] refuses
    those, as before 2026-10-03 (or, with [FailIf Oversaturated False],
    saturates them whole anyway); [On_demand] uses it for every FSM, for
    measuring it. [Run Saturate] and [Run Minimize] always saturate whole.
    Reset by [Reset Bounds]. *)
type saturation_mode =
  | Saturation_whole
  | Saturation_on_demand
  | Saturation_auto

(* See the [.mli]. *)
let the_saturation_mode : saturation_mode ref = ref Saturation_auto

(* See the [.mli]. *)
let set_saturation_mode (x : saturation_mode) : unit =
  the_saturation_mode := x;
  Printf.sprintf
    "(MeBi Config: Set Saturation OnDemand to: %s.)"
    (match x with
     | Saturation_whole -> "False"
     | Saturation_on_demand -> "True"
     | Saturation_auto -> "Auto")
  |> Logger.show
;;

(** The most pairs a proof's up-front game walk may visit when an FSM is
    saturated on demand ([MeBi Config Bounds Game <n>]; unset by default).
    Such a walk -- [MutualCofix True]'s pair set, a planned answer policy,
    [Auto]'s estimate -- saturates states as it goes. Unset, the explicit
    settings are refused on demand; set, the walk runs under the bound
    (notes/13, 2026-10-03). Reset by [Reset Bounds]. *)
let the_game_bound : int option ref = ref None

(* See the [.mli]. *)
let set_game_bound (x : int) : unit =
  the_game_bound := Some x;
  Printf.sprintf "(MeBi Config: Set Game Bound to: %i pairs.)" x |> Logger.show
;;

(* See the [.mli]. *)
let reset_bounds_args () : unit =
  the_bounds_args := default_bounds;
  the_saturation_bound := default_saturation_bound;
  the_game_bound := None;
  the_saturation_mode := Saturation_auto;
  Premise_search.max_depth := Premise_search.default_depth;
  Premise_search.max_range := Premise_search.default_range
;;

(** A tactic tried on premises the search leaves undecided
    ([Premise_search.user_tactic]; backlog I2, stage 4). *)
let set_premise_tactic (t : unit Proofview.tactic) : unit =
  Premise_search.user_tactic := Some t;
  Logger.show "(MeBi Config: Set Premise tactic.)"
;;

(* See the [.mli]. *)
let reset_premise () : unit =
  Premise_search.max_depth := Premise_search.default_depth;
  Premise_search.max_range := Premise_search.default_range;
  Premise_search.user_tactic := None;
  Logger.show "(MeBi Config: Reset Premise depth, range and tactic.)"
;;

(** How deep the bounded proof search for constructor premises may go
    ([Premise_search]; backlog I2). Reset by [Reset Bounds]. *)
let set_premise_depth (x : int) : unit =
  Premise_search.max_depth := x;
  Printf.sprintf "(MeBi Config: Set Premise search depth to: %i.)" x
  |> Logger.show
;;

(** [set_premise_range x]: decide bounded universal premises
    ([forall k, k < n -> P k]) ranging over at most [x] values of [k]
    ([Premise_search.max_range]). Reset by [Reset Premise] and [Reset Bounds].
*)
let set_premise_range (x : int) : unit =
  Premise_search.max_range := x;
  Printf.sprintf "(MeBi Config: Set Premise range to: %i values.)" x
  |> Logger.show
;;

(* See the [.mli]. *)
let set_saturation_bound (x : int) : unit =
  the_saturation_bound := x;
  Printf.sprintf "(MeBi Config: Set Saturation Bound to: %i weak actions.)" x
  |> Logger.show
;;

(** Peak memory per extracted state, on top of a fixed ~0.1--0.3GB: measured
    2026-10-01, after extraction stopped keeping matching evars, as 0.01MB on
    [Proc/Test4] (9720 states, 0.40GB peak) and 0.03--0.07MB on
    [CADP/Size2] (5000 states, 0.49GB). Logging the result is far costlier:
    [Output "Result"]/["DecodeResults"]/["DumpResults"] pretty-print every
    term, ~0.65MB per state on [CADP/Size2]. *)
let mb_per_extracted_state : float * float = 0.01, 0.07

(* See the [.mli]. *)
let set_the_bounds_args (x : bounds_args) : unit =
  the_bounds_args := x;
  Printf.sprintf
    "(MeBi Config: Set Bounds to: %s.)"
    (match x with
     | States i -> Printf.sprintf "%i States" i
     | Transitions i -> Printf.sprintf "%i Transitions" i)
  |> Logger.show;
  match x with
  | States i ->
    let lo, hi = mb_per_extracted_state in
    (* [gb mb] is the memory for [i] states at [mb] per state, in GB. *)
    let gb (mb : float) : float = Float.of_int i *. mb /. 1000. in
    if gb hi >= 1.
    then
      Printf.sprintf
        "(Exploring up to %i states may need %.1f--%.1fGB of memory: \
         extraction has measured %.2f--%.2fMB per state, and logging the \
         result with Output \"Result\" adds ~0.65MB per state.)"
        i
        (gb lo)
        (gb hi)
        lo
        hi
      |> Logger.notice
  | Transitions _ -> ()
;;

(***********************************************************************)

type weak_args =
  { a : weak_arg option
  ; b : weak_arg option
  }

and weak_arg =
  | Option of Constrexpr.constr_expr
  | Custom of Constrexpr.constr_expr * Libnames.qualid

(* See the [.mli]. *)
let the_weak_args : weak_args ref option ref = ref None

(* See the [.mli]. *)
let reset_weak_args () : unit = the_weak_args := None

(* See the [.mli]. *)
let set_the_weak_args (a : weak_arg option) (b : weak_arg option) : unit =
  the_weak_args := Some (ref { a; b })
;;

(* See the [.mli]. *)
let get_the_weak_arg1 () : weak_arg option =
  match !the_weak_args with None -> None | Some x -> !x.a
;;

(* See the [.mli]. *)
let get_the_weak_arg2 () : weak_arg option =
  match !the_weak_args with None -> None | Some x -> !x.b
;;

(* See the [.mli]. *)
let set_the_weak_arg1 (x : weak_arg) : unit =
  the_weak_args := Some (ref { a = Some x; b = get_the_weak_arg2 () })
;;

(* See the [.mli]. *)
let set_the_weak_arg2 (x : weak_arg) : unit =
  the_weak_args := Some (ref { a = get_the_weak_arg1 (); b = Some x })
;;

(***********************************************************************)

(** How the proof solver introduces its coinduction hypotheses.

    - [Nested] mints a fresh [cofix] each time the search meets a pair it has
      not seen. A nested cofix is visible only to the branch that created it,
      so a pair repeating a {e sibling} cannot be closed and its subtree is
      re-derived.
    - [Mutual] opens the proof with one mutual cofix naming every pair of
      [Model.Product.reachable], so every hypothesis is in scope everywhere.
    - [Auto] measures both on the model, before any proof step runs, and
      picks -- see [Model.Product.estimate].

    Defaults to [Auto], which is correct on every example in the repository
    and better than either fixed strategy: it keeps the nested path's counts
    where that path is cheaper, and takes the mutual one where the nested walk
    would blow up. It says so, at [Notice], whenever it takes the mutual path,
    since that is the deviation from what the solver historically did. See
    [ASSISTED-CHANGES.md], 2026-09-29, and backlog item B2. *)
type solver_strategy =
  | Nested
  | Mutual
  | Auto

(* See the [.mli]. *)
let the_solver_strategy : solver_strategy ref = ref Auto

(** What [the_solver_strategy] resolved to for the proof now being solved.
    [Auto] writes here once, in [Proof_solver.init]; the step machinery reads
    only this. *)
let the_mutual_cofix : bool ref = ref false

(* See the [.mli]. *)
let set_solver_strategy (x : solver_strategy) : unit =
  the_solver_strategy := x;
  the_mutual_cofix := match x with Mutual -> true | Nested | Auto -> false
;;

(* See the [.mli]. *)
let set_mutual_cofix (x : bool) : unit = the_mutual_cofix := x

(* See the [.mli]. *)
let reset_mutual_cofix () : unit =
  the_solver_strategy := Auto;
  the_mutual_cofix := false
;;

type answer_policy =
  | Answers_default
  | Answers_greedy
  | Answers_minimal
  | Answers_auto

(* See the [.mli]. *)
let the_answer_policy : answer_policy ref = ref Answers_default

(* See the [.mli]. *)
let set_answer_policy (x : answer_policy) : unit = the_answer_policy := x

(* See the [.mli]. *)
let reset_all () : unit =
  reset_mutual_cofix ();
  the_answer_policy := Answers_default;
  Premise_search.user_tactic := None;
  reset_bounds_args ();
  reset_weak_args ();
  reset_the_fail_flags ();
  reset_output_config ();
  Logger.show "(MeBi: Reset Config.)"
;;
