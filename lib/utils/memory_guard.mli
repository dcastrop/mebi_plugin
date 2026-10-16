(** A soft cap on the plugin's memory ([MeBi Config Bounds Memory]).

    The plugin's memory-heavy loops (extraction, saturation, the up-front
    game walks and estimates, the proof solver's steps) call {!check} once
    per unit of work. With a limit set, {!check} reads the process's memory
    at most every {!interval} seconds and raises {!Exceeded} once it is over
    the limit, so the command fails with a message instead of the process
    being killed. Unset (the default), {!check} reads one reference and does
    nothing else.

    The cap is soft: memory is read between units of work, so one unit can
    overshoot it, and [Qed] (Rocq's kernel) is never checked. It is a
    mitigation, not a replacement for an OS-level cap. Contains no reference
    to the Rocq API. *)

(** [Exceeded { phase; used; limit }]: the process used [used] bytes,
    over [limit], when [phase] checked. *)
exception
  Exceeded of
    { phase : string
    ; used : int
    ; limit : int
    }

(** The limit in bytes, or [None] (the default) for no limit. *)
val limit : int option ref

(** [set_limit x] sets {!limit} to [x], and makes the next {!check} read
    memory whatever the time since the last read. Raises nothing. *)
val set_limit : int option -> unit

(** The least time in seconds between two reads of memory by {!check}. *)
val interval : float

(** [used ()] is the process's memory in bytes: its resident set
    ([VmRSS] in [/proc/self/status]) on Linux, otherwise the size of the
    OCaml major heap. [None] if neither can be read. Raises nothing. *)
val used : unit -> int option

(** [check phase] checks {!used} against {!limit}, if one is set and
    either {!interval} has passed since the last read or that read was
    over the limit.

    @raise Exceeded if memory is over the limit (raised here). *)
val check : string -> unit

(** [message phase used limit] is the message for {!Exceeded}. Raises
    nothing. *)
val message : string -> int -> int -> string
