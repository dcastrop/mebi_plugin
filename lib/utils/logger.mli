(** Message emission against a sink installed at plugin load.

    There is no [Logger.S] functor parameter any more. [S] had no abstract type,
    so passing it to a functor cost a parameter on every module in [lib/] and
    bought nothing. Call [Logger.trace], [Logger.info] etc. directly; the
    Rocq-vs-stdout choice is made once via [set_sink]. *)

type sink = Output.message -> unit

(** Prints to [stdout]. Active until [set_sink] is called, which is what makes
    [lib/utils], [lib/terms] and [lib/model] usable from a plain OCaml test
    binary with no Rocq runtime linked. *)
val default_sink : sink

val set_sink : sink -> unit
val reset_sink : unit -> unit

(** {1 Configuration} *)

val enable : unit -> unit
val disable : unit -> unit
val configure : Output.Kind.t -> bool -> unit
val reset_config : unit -> unit

(* [is_enabled] is not declared here: it comes from [include S] below. *)

(** [quiet f] runs [f] with output suppressed, restoring the previous setting
    afterwards even if [f] raises. *)
val quiet : (unit -> 'a) -> 'a

(** {1 Emission} *)

module type S = sig
  val is_enabled : Output.Kind.t -> bool
  val debug : ?__FUNCTION__:string -> string -> unit
  val info : ?__FUNCTION__:string -> string -> unit
  val notice : ?__FUNCTION__:string -> string -> unit
  val warning : ?__FUNCTION__:string -> string -> unit
  val error : ?__FUNCTION__:string -> string -> unit
  val trace : ?__FUNCTION__:string -> string -> unit
  val result : ?__FUNCTION__:string -> string -> unit
  val show : ?__FUNCTION__:string -> string -> unit

  val thing
    :  ?__FUNCTION__:string
    -> Output.Kind.t
    -> string
    -> 'a
    -> ('a -> string)
    -> unit

  val things
    :  ?__FUNCTION__:string
    -> Output.Kind.t
    -> string
    -> 'a list
    -> ('a -> string)
    -> unit

  val option
    :  ?__FUNCTION__:string
    -> Output.Kind.t
    -> string
    -> 'a option
    -> ('a -> string)
    -> unit

  val options
    :  ?__FUNCTION__:string
    -> Output.Kind.t
    -> string
    -> 'a list option
    -> ('a -> string)
    -> unit
end

include S

(** A logger with its own per-kind overrides, sharing the global sink and global
    on/off. Declared and used within a single file — never threaded through a
    functor. Only [Rocq_utils] and [Mebi_theories] need it.

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
