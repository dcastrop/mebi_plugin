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

let output_config_default : output_config =
  { decode_results = true; dump_results = true }
;;

let the_output_config : output_config ref = ref output_config_default

let reset_output_config () : unit =
  the_output_config := { decode_results = true; dump_results = true };
  Logger.reset_config ()
;;

let config_output (x : bool) (k : Output.Kind.t) : unit = Logger.configure k x

let output_config_decode_results (x : bool) : unit =
  !the_output_config.decode_results <- x
;;

let output_config_dump_results (x : bool) : unit =
  !the_output_config.dump_results <- x
;;

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

(***********************************************************************)

let make_enc (module X : Encoding.Packed.PackedS) : (module Encoding.S) =
  let module Enc : Encoding.S = Encoding.Packed.Unpack (X) in
  (module Enc : Encoding.S)
;;

let make_enc_int () : (module Encoding.S) =
  (module (val make_enc (module Encoding.Packed.Int)) : Encoding.S)
;;

(***********************************************************************)

type fail_flags =
  { mutable empty : bool
  ; mutable incomplete : bool
  ; mutable non_bisimilar : bool
  }

let the_fail_flags_default : fail_flags =
  { empty = false; incomplete = true; non_bisimilar = true }
;;

let the_fail_flags : fail_flags ref = ref the_fail_flags_default
let reset_the_fail_flags () : unit = the_fail_flags := the_fail_flags_default

let set_fail_flag_empty (empty : bool) : unit =
  the_fail_flags := { !the_fail_flags with empty };
  Printf.sprintf "(MeBi Config: Set Fail-If 'Empty' Flag to: %b.)" empty
  |> Logger.show
;;

let set_fail_flag_incomplete (incomplete : bool) : unit =
  the_fail_flags := { !the_fail_flags with incomplete };
  Printf.sprintf
    "(MeBi Config: Set Fail-If 'Incomplete' Flag to: %b.)"
    incomplete
  |> Logger.show
;;

let set_fail_flag_non_bisimilar (non_bisimilar : bool) : unit =
  the_fail_flags := { !the_fail_flags with non_bisimilar };
  Printf.sprintf
    "(MeBi Config: Set Fail-If 'Non-bisimilar' Flag to: %b.)"
    non_bisimilar
  |> Logger.show
;;

(***********************************************************************)

type bounds_args =
  | States of int
  | Transitions of int

let default_bounds : bounds_args = States 100
let the_bounds_args : bounds_args ref = ref default_bounds
let reset_bounds_args () : unit = the_bounds_args := default_bounds

let set_the_bounds_args (x : bounds_args) : unit =
  the_bounds_args := x;
  Printf.sprintf
    "(MeBi Config: Set Bounds to: %s.)"
    (match x with
     | States i -> Printf.sprintf "%i States" i
     | Transitions i -> Printf.sprintf "%i Transitions" i)
  |> Logger.show
;;

(***********************************************************************)

type weak_args =
  { a : weak_arg option
  ; b : weak_arg option
  }

and weak_arg =
  | Option of Constrexpr.constr_expr
  | Custom of Constrexpr.constr_expr * Libnames.qualid

let the_weak_args : weak_args ref option ref = ref None
let reset_weak_args () : unit = the_weak_args := None

let set_the_weak_args (a : weak_arg option) (b : weak_arg option) : unit =
  the_weak_args := Some (ref { a; b })
;;

let get_the_weak_arg1 () : weak_arg option =
  match !the_weak_args with None -> None | Some x -> !x.a
;;

let get_the_weak_arg2 () : weak_arg option =
  match !the_weak_args with None -> None | Some x -> !x.b
;;

let set_the_weak_arg1 (x : weak_arg) : unit =
  the_weak_args := Some (ref { a = Some x; b = get_the_weak_arg2 () })
;;

let set_the_weak_arg2 (x : weak_arg) : unit =
  the_weak_args := Some (ref { a = get_the_weak_arg1 (); b = Some x })
;;

(***********************************************************************)

let reset_all () : unit =
  reset_bounds_args ();
  reset_weak_args ();
  reset_the_fail_flags ();
  reset_output_config ();
  Logger.show "(MeBi: Reset Config.)"
;;
