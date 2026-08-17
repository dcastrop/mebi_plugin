(** Routes plugin output through Rocq.

    [lib/utils], [lib/terms] and [lib/model] are plain OCaml and link no Rocq
    runtime; they emit through [Logger], which by default prints to [stdout].
    This module is the Rocq half: it installs a sink that renders messages with
    [Pp] and hands them to [Feedback], plus a provider for the source location
    used to name dump files. Both are installed by [install ()], called once
    when the plugin loads.

    This is the layer the old [Output.Mode.Rocq] / [Output.Mode.OCaml] functor
    pair was expressing. The difference is that the choice is now made once at
    load time instead of being a functor parameter on every module in [lib/]. *)

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

(** [Trace], [Result] and [Show] have no [Feedback] level of their own, so they
    map onto Debug/Info/Notice as they did before. *)
let sink : Logger.sink =
  fun (m : Output.message) ->
  let doc : Pp.t = render m in
  match m.kind with
  | Debug | Trace -> Feedback.msg_debug doc
  | Info | Result -> Feedback.msg_info doc
  | Notice | Show -> Feedback.msg_notice doc
  | Warning -> Feedback.msg_warning ~quickfix:[] doc
  (* NOTE: as before, Error is reported via msg_debug rather than raising. *)
  | Error -> Feedback.msg_debug doc
;;

(** Names a dump with the [.v] file and line that triggered it. *)
let loc_provider () : string =
  match Loc.get_current_command_loc () with
  | Some { line_nb; fname = InFile { file; _ } } ->
    String.map (fun x -> if Char.equal '/' x then ' ' else x) file
    |> Printf.sprintf "line %i | %s" line_nb
  | _ -> "Unknown Location"
;;

let install () : unit =
  Logger.set_sink sink;
  Utils.FileWriter.set_loc_provider loc_provider
;;

(* Installed on module initialisation, i.e. when Rocq loads the plugin via
   `Declare ML Module`. Nothing else needs to call `install`. *)
let () = install ()
