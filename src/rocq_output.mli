(** Routes plugin output through Rocq.

    [lib/utils], [lib/terms] and [lib/model] link no Rocq runtime and emit
    through {!Logger}, which prints to [stdout] by default. This module installs
    the Rocq half: a sink rendering messages with [Pp] onto [Feedback], and the
    source-location provider used to name dump files.

    {!install} runs on module initialisation, so nothing normally needs to call
    it; [g_mebi.mlg] calls it explicitly to make the dependency visible. *)

(** [sink m] prints the message [m] through Rocq's [Feedback], at the level
    of its kind ([Trace] as debug, [Result] as info, [Show] as notice,
    [Error] as debug: it does not raise). Raises nothing. *)
val sink : Logger.sink

(** [loc_provider ()] is the [.v] file and line of the command now running,
    to name dump files by. Raises nothing. *)
val loc_provider : unit -> string

(** [install ()] makes {!Logger} print through {!sink} and name dumps with
    {!loc_provider}. Raises nothing. *)
val install : unit -> unit
