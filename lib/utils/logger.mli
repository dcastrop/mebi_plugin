(** Message emission against a sink installed at plugin load.

    There is no [Logger.S] functor parameter any more. [S] had no abstract type,
    so passing it to a functor cost a parameter on every module in [lib/] and
    bought nothing. Call {!Logger.trace}, {!Logger.info} etc. directly; the
    Rocq-vs-stdout choice is made once via {!set_sink}. *)

(** Where a message goes once it passes the configuration. *)
type sink = Output.message -> unit

(** The sink that prints to [stdout]. Active until {!set_sink} is called,
    which is what makes [lib/utils], [lib/terms] and [lib/model] usable from
    a plain OCaml test binary with no Rocq runtime linked. *)
val default_sink : sink

(** [set_sink f] sends every message from now on to [f] ([src/] installs
    Rocq's [Feedback] at plugin load). Raises nothing. *)
val set_sink : sink -> unit

(** [reset_sink ()] goes back to {!default_sink}. Raises nothing. *)
val reset_sink : unit -> unit

(** {1 Configuration} *)

(** [enable ()] turns output on, under the per-kind configuration. Raises
    nothing. *)
val enable : unit -> unit

(** [disable ()] turns all output off. Raises nothing. *)
val disable : unit -> unit

(** [configure k b] sets whether kind [k] is emitted, overriding
    {!Output.Kind.default}. Raises nothing. *)
val configure : Output.Kind.t -> bool -> unit

(** [reset_config ()] drops every per-kind setting: each kind is back to
    its default. Raises nothing. *)
val reset_config : unit -> unit

(* [is_enabled] is not declared here: it comes from [include S] below. *)

(** [quiet f] is [f ()], with all output off while it runs and the previous
    setting restored afterwards, even if [f] raises. Raises whatever [f]
    raises (propagated). *)
val quiet : (unit -> 'a) -> 'a

(** {1 Emission} *)

(** The logging API: at the top level of {!Logger} against the global
    configuration, and through {!Scoped} with a module's own. None of it
    raises, beyond what the sink or a given printer raises. *)
module type S = sig
  (** [is_enabled k] is whether a message of kind [k] is emitted: output is
      on, and [k] is configured on (or is on by default). *)
  val is_enabled : Output.Kind.t -> bool

  (** [debug ?__FUNCTION__ s] emits [s] at [Debug], naming the calling
      function if given; an empty [s] emits nothing. {!info} ... {!show} are
      the same for their kinds. *)
  val debug : ?__FUNCTION__:string -> string -> unit

  val info : ?__FUNCTION__:string -> string -> unit
  val notice : ?__FUNCTION__:string -> string -> unit
  val warning : ?__FUNCTION__:string -> string -> unit
  val error : ?__FUNCTION__:string -> string -> unit
  val trace : ?__FUNCTION__:string -> string -> unit
  val result : ?__FUNCTION__:string -> string -> unit
  val show : ?__FUNCTION__:string -> string -> unit

  (** [thing ?__FUNCTION__ k prefix x f] emits [f x] at kind [k], headed
      [prefix]. [f] is applied only if [k] is enabled: it is often a Rocq
      pretty-printer, on a hot path. *)
  val thing
    :  ?__FUNCTION__:string
    -> Output.Kind.t
    -> string
    -> 'a
    -> ('a -> string)
    -> unit

  (** [things ?__FUNCTION__ k prefix xs f] emits each of [xs] as {!thing},
      headed by its index, between a ["start"] and an ["end"] headed
      [prefix]. *)
  val things
    :  ?__FUNCTION__:string
    -> Output.Kind.t
    -> string
    -> 'a list
    -> ('a -> string)
    -> unit
end

include S

(** [Scoped (X)] is a logger with its own per-kind overrides [X.overrides]
    (a kind not listed follows the global configuration), sharing the
    global sink and global on/off. Declared and used within a single file,
    never threaded through a functor. Only {!Rocq_utils} and
    {!Mebi_theories} need it.

    {b E.g.:}
    {[
    module Log = Logger.Scoped (struct
        let overrides = [ Output.Kind.Debug, false; Output.Kind.Trace, false ]
      end)
    ]} *)
module Scoped : (_ : sig
                   val overrides : (Output.Kind.t * bool) list
                 end)
    -> S
