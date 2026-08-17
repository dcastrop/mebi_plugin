(** Routes plugin output through Rocq.

    [lib/utils], [lib/terms] and [lib/model] link no Rocq runtime and emit
    through [Logger], which prints to [stdout] by default. This module installs
    the Rocq half: a sink rendering messages with [Pp] onto [Feedback], and the
    source-location provider used to name dump files.

    [install] runs on module initialisation, so nothing normally needs to call
    it; [g_mebi.mlg] calls it explicitly to make the dependency visible. *)

val sink : Logger.sink
val loc_provider : unit -> string
val install : unit -> unit
