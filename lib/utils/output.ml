(** The kinds of message the plugin can emit, and nothing else.

    This module deliberately contains no reference to the Rocq API. Routing a
    message to Rocq's [Feedback] is the job of a {i sink} installed at plugin
    load (see [Logger.set_sink] and [Mebi_plugin.Rocq_output]); when no sink is
    installed, [Logger] prints to [stdout]. That is what lets [lib/utils],
    [lib/terms] and [lib/model] build and be tested as plain OCaml libraries. *)

module Kind = struct
  (** [Debug] .. [Error] mirror Rocq's [Feedback.level]; [Trace], [Result] and
      [Show] are extensions of this plugin. The distinction only matters to a
      sink, so unlike previous versions this type is not split into
      [level]/[special]. *)
  type t =
    | Debug
    | Info
    | Notice
    | Warning
    | Error
    | Trace
    | Result
    | Show

  let all : t list =
    [ Debug; Info; Notice; Warning; Error; Trace; Result; Show ]
  ;;

  let to_string : t -> string = function
    | Debug -> "Debug"
    | Info -> "Info"
    | Notice -> "Notice"
    | Warning -> "Warning"
    | Error -> "Error"
    | Trace -> "Trace"
    | Result -> "Result"
    | Show -> "Show"
  ;;

  let of_string : string -> t option = function
    | "Debug" -> Some Debug
    | "Info" -> Some Info
    | "Notice" -> Some Notice
    | "Warning" -> Some Warning
    | "Error" -> Some Error
    | "Trace" -> Some Trace
    | "Result" -> Some Result
    | "Show" -> Some Show
    | _ -> None
  ;;

  (** Whether a kind is emitted when nothing has configured it. Matches the
      previous [Output.default_level_fun] / [default_special_fun] defaults. *)
  let default : t -> bool = function
    | Debug -> false
    | Trace -> false
    | Result -> false
    | Info | Notice | Warning | Error | Show -> true
  ;;
end

(** One message, kept in parts rather than pre-composed into a string, so that
    a sink able to lay text out properly (Rocq's [Pp]) still can. *)
type message =
  { kind : Kind.t
  ; fn : string (** [__FUNCTION__] of the emitting site, or [""]. *)
  ; prefix : string option
  ; body : string
  }
