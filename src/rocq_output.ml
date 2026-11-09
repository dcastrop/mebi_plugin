(** Routes plugin output through Rocq.

    [lib/utils] is plain OCaml and links no Rocq runtime; the libraries
    emit through its [Logger], which by default prints to [stdout]. This
    module is the Rocq half: it installs a sink that renders messages with
    [Pp] and hands them to [Feedback]. It is installed by [install ()],
    when the plugin loads. *)

(** Renders [__FUNCTION__], the optional prefix and the body into one [Pp.t],
    preserving the box structure the previous [Output.Mode.Rocq] used. *)
let render ({ fn; prefix; body; _ } : Output.message) : Pp.t =
  let open Pp in
  let a : Pp.t =
    (match fn with "" -> mt () | z -> str z ++ str ": ") |> v 0
  in
  let b : Pp.t =
    Option.cata (fun (p : string) -> str p) (mt ()) prefix |> v 0
  in
  let c : Pp.t = str body |> v 0 in
  seq [ v 0 (seq [ a; b ]); ws 0; c ] |> hv 0
;;

(** The debug flags: [Set Debug "mebi"] and so on,
    as for Rocq's own [Set Debug "unification"]. Off by default. *)
let debug_flag, _ = CDebug.create_full ~name:"mebi" ()

let trace_flag, _ = CDebug.create_full ~name:"mebi-trace" ()
let info_flag, _ = CDebug.create_full ~name:"mebi-info" ()

(** MeBi's warnings, as one Rocq warning [mebi] that is also a category
    for any finer ones later: [Set Warnings "-mebi"] hides them,
    [Set Warnings "+mebi"] makes them errors. *)
let _warning_category, warning = CWarnings.create_hybrid ~name:"mebi" ()

let warn : ?loc:Loc.t -> Pp.t -> unit =
  CWarnings.create_in warning (fun pp -> pp)
;;

(** [enabled k] is whether kind [k] is shown: Rocq's debug flags for
    [Debug], [Trace], [Info] and [Show] (routine progress and settings
    echoed) and [Result]; notices, errors and
    warnings always (a warning is then filtered by Rocq's [Set Warnings]).
    Raises nothing. *)
let enabled : Output.Kind.t -> bool = function
  | Debug -> CDebug.get_flag debug_flag
  | Trace -> CDebug.get_flag trace_flag
  | Info | Show | Result -> CDebug.get_flag info_flag
  | Notice | Warning | Error -> true
;;

(* See the [.mli]. [Trace], [Result] and [Show] have no [Feedback] level
   of their own, so they map onto debug, info and notice. *)
let sink : Logger.sink =
  fun (m : Output.message) ->
  let doc : Pp.t = render m in
  match m.kind with
  | Debug | Trace -> Feedback.msg_debug doc
  | Info | Result -> Feedback.msg_info doc
  | Notice | Show -> Feedback.msg_notice doc
  | Warning -> warn doc
  (* NOTE: as before, Error is reported via msg_debug rather than raising. *)
  | Error -> Feedback.msg_debug doc
;;

(* See the [.mli]. *)
let install () : unit =
  Logger.set_sink sink;
  Logger.set_enabled_test enabled
;;

(* Installed on module initialisation, i.e. when Rocq loads the plugin via
   `Declare ML Module`. Nothing else needs to call `install`. *)
let () = install ()
