module Defaults = struct
  module Log : Logger.S = Logger.Default
  module Ctx : Rocq_context.S = Rocq_context.Default
  module Enc : Encoding.S with type t = int = Encoding.Int (Log)
  (* module Tree = Enc_tree.Make (Log) (Enc) *)
  (* module Trees = Enc_trees.Make (Log) (Tree) *)
end

(***********************************************************************)

(* TODO: output *)

type output_config =
  { mutable debug : bool
  ; mutable info : bool
  ; mutable notice : bool
  ; mutable warning : bool
  ; mutable error : bool
  ; mutable trace : bool
  ; mutable result : bool
  ; mutable show : bool
  ; mutable decode_results : bool
  }

let output_config_default : output_config =
  { debug = false
  ; info = true
  ; notice = true
  ; warning = true
  ; error = true
  ; trace = false
  ; result = true
  ; show = true
  ; decode_results = false
  }
;;

let the_output_config : output_config ref = ref output_config_default

let config_output (x : bool) : Output.Kind.t -> unit = function
  | Debug -> !the_output_config.debug <- x
  | Info -> !the_output_config.info <- x
  | Notice -> !the_output_config.notice <- x
  | Warning -> !the_output_config.warning <- x
  | Error -> !the_output_config.error <- x
  | Trace -> !the_output_config.trace <- x
  | Result -> !the_output_config.result <- x
  | Show -> !the_output_config.show <- x
;;

let output_config_decode_results (x : bool) : unit =
  !the_output_config.decode_results <- x
;;

(***********************************************************************)

let make_logger () : (module Logger.S) =
  (module Logger.Make
            (Output.Mode.Default)
            (struct
              let prefix = None

              let level : Output.Kind.level -> bool = function
                | Debug -> !the_output_config.debug
                | Info -> !the_output_config.info
                | Notice -> !the_output_config.notice
                | Warning -> !the_output_config.warning
                | Error -> !the_output_config.error
              ;;

              let special : Output.Kind.special -> bool = function
                | Trace -> !the_output_config.trace
                | Result -> !the_output_config.result
                | Show -> !the_output_config.show
              ;;
            end) : Logger.S)
;;

(* let make_wrapper () =
   let module W = Wrapper.
   Make (Api.Defaults.Log) (Api.Defaults.Ctx) (Api.Defaults.Enc) in *)

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
  |> Logger.Default.show
;;

let set_fail_flag_incomplete (incomplete : bool) : unit =
  the_fail_flags := { !the_fail_flags with incomplete };
  Printf.sprintf
    "(MeBi Config: Set Fail-If 'Incomplete' Flag to: %b.)"
    incomplete
  |> Logger.Default.show
;;

let set_fail_flag_non_bisimilar (non_bisimilar : bool) : unit =
  the_fail_flags := { !the_fail_flags with non_bisimilar };
  Printf.sprintf
    "(MeBi Config: Set Fail-If 'Non-bisimilar' Flag to: %b.)"
    non_bisimilar
  |> Logger.Default.show
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
  |> Logger.Default.show
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

(* TODO: add configurable output stuff *)
(* let set_output_level_debug (x:bool) : unit =
  let f
    ?(debug : bool = debug)
      ?(info : bool = false)
      ?(notice : bool = true)
      ?(warning : bool = true)
      ?(error : bool = true) : level -> bool =
in
  Output.Kind.default_level := ref (
     Output.Kind.default_level_fun ~debug:x );
  
;; *)

let reset_the_logging_args () : unit =
  Output.Kind.default_level := Output.Kind.default_level_fun;
  Output.Kind.default_special := Output.Kind.default_special_fun
;;

(***********************************************************************)

let reset_all () : unit =
  reset_bounds_args ();
  reset_weak_args ();
  reset_the_fail_flags ();
  reset_the_logging_args ();
  Logger.Default.show "(MeBi: Reset Config.)"
;;
