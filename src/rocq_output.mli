(** Routes plugin output through Rocq.

    [lib/utils] links no Rocq runtime, and the libraries emit through its
    {!Logger}, which prints to [stdout] by default. This module installs the
    Rocq half: a sink rendering messages with [Pp] onto [Feedback].

    {!install} runs on module initialisation, so nothing normally needs to call
    it; [g_mebi.mlg] calls it explicitly to make the dependency visible. *)

(** [sink m] prints the message [m] through Rocq's [Feedback], at the level
    of its kind ([Trace] as debug, [Result] as info, [Show] as notice,
    [Error] as debug: it does not raise). Raises nothing. *)
val sink : Logger.sink

(** [install ()] makes {!Logger} print through {!sink}. Raises nothing. *)
val install : unit -> unit
