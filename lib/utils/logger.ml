(** Message emission.

    Previously this exposed [module type S] and [Make], and every module in
    [lib/] took a [Log : Logger.S] functor parameter so that the plugin could
    print through Rocq's [Feedback] when driven from Rocq and through [stdout]
    when driven from a test binary. But [S] had no abstract type — every member
    returned [unit] — so the functor imposed the full type-level cost and bought
    nothing. The Rocq/OCaml choice is now made once, at plugin load, by
    installing a {i sink}; see [set_sink]. *)

type sink = Output.message -> unit

(* See the [.mli]. Used when no sink is installed, i.e. under [tests/] and
   any other plain OCaml entry point. Formatting matches the old
   [Output.Mode.OCaml]. *)
let default_sink : sink =
  fun ({ kind; fn; prefix; body } : Output.message) ->
  Printf.printf
    "%s [%s] %s\n%!"
    (match fn with "" -> "" | z -> Printf.sprintf "%s:" z)
    (Output.Kind.to_string kind)
    (match prefix with None -> body | Some p -> Printf.sprintf "%s: %s" p body)
;;

(** The installed sink. *)
let the_sink : sink ref = ref default_sink

(* See the [.mli] for these two. *)
let set_sink (f : sink) : unit = the_sink := f
let reset_sink () : unit = the_sink := default_sink

(***********************************************************************)

(** Global on/off, checked before the per-kind configuration. *)
let the_enabled : bool ref = ref true

(* See the [.mli] for these, through [is_enabled]. *)
let enable () : unit = the_enabled := true
let disable () : unit = the_enabled := false

(** Per-kind overrides. Absent means [Output.Kind.default]. *)
let the_config : (Output.Kind.t, bool) Hashtbl.t = Hashtbl.create 8

let configure (k : Output.Kind.t) (b : bool) : unit =
  Hashtbl.replace the_config k b
;;

let reset_config () : unit = Hashtbl.reset the_config

let is_enabled (k : Output.Kind.t) : bool =
  !the_enabled
  &&
  match Hashtbl.find_opt the_config k with
  | None -> Output.Kind.default k
  | Some b -> b
;;

(* See the [.mli]. Replaces the old [Logger.ReMake], whose two uses were
   both commented out. *)
let quiet (f : unit -> 'a) : 'a =
  let saved : bool = !the_enabled in
  the_enabled := false;
  Fun.protect ~finally:(fun () -> the_enabled := saved) f
;;

(***********************************************************************)

(** [emit ?enabled ?__FUNCTION__ ?prefix ?override k body] hands the
    message [body] of kind [k] to the sink, if [override] or [enabled k]
    (default {!is_enabled}): the one place a message is filtered. An empty
    [body] is dropped. [enabled] lets a scoped logger substitute its own
    predicate. Raises whatever the sink raises (propagated). *)
let emit
      ?(enabled : Output.Kind.t -> bool = is_enabled)
      ?(__FUNCTION__ : string = "")
      ?(prefix : string option = None)
      ?(override : bool = false)
      (kind : Output.Kind.t)
  : string -> unit
  = function
  | "" -> ()
  | body ->
    if override || enabled kind
    then !the_sink { kind; fn = __FUNCTION__; prefix; body }
;;

(* See the [.mli]. *)
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
end

(** [Body (E)] is the logging API with [E.is_enabled] deciding whether a
    kind is emitted: shared by the top level and {!Scoped}. *)
module Body (E : sig
    val is_enabled : Output.Kind.t -> bool
  end) : S = struct
  let is_enabled = E.is_enabled

  (** [out ?__FUNCTION__ ?prefix k body] is {!emit} under
      [E.is_enabled]. *)
  let out ?(__FUNCTION__ : string = "") ?(prefix : string option = None)
    : Output.Kind.t -> string -> unit
    =
    emit ~enabled:E.is_enabled ~__FUNCTION__ ~prefix
  ;;

  (* See the [.mli] for the rest. *)
  let debug ?(__FUNCTION__ : string = "") (x : string) : unit =
    out ~__FUNCTION__ Debug x
  ;;

  let info ?(__FUNCTION__ : string = "") (x : string) : unit =
    out ~__FUNCTION__ Info x
  ;;

  let notice ?(__FUNCTION__ : string = "") (x : string) : unit =
    out ~__FUNCTION__ Notice x
  ;;

  let warning ?(__FUNCTION__ : string = "") (x : string) : unit =
    out ~__FUNCTION__ Warning x
  ;;

  let error ?(__FUNCTION__ : string = "") (x : string) : unit =
    out ~__FUNCTION__ Error x
  ;;

  let trace ?(__FUNCTION__ : string = "") (x : string) : unit =
    out ~__FUNCTION__ Trace x
  ;;

  let result ?(__FUNCTION__ : string = "") (x : string) : unit =
    out ~__FUNCTION__ Result x
  ;;

  let show ?(__FUNCTION__ : string = "") (x : string) : unit =
    out ~__FUNCTION__ Show x
  ;;

  let thing
        ?(__FUNCTION__ : string = "")
        (k : Output.Kind.t)
        (prefix : string)
        (x : 'a)
        (f : 'a -> string)
    : unit
    =
    (* [f x] is only worth paying for if the message will actually be emitted.
       [emit] filters on this same predicate, but as a function argument [f x]
       is evaluated before the call, so without this guard every call formats
       its value even with the kind disabled -- and [f] is often
       [Strfy.econstr], a Rocq pretty-printer, on a hot path. *)
    if is_enabled k
    then out ~prefix:(Some (Printf.sprintf "%s: " prefix)) ~__FUNCTION__ k (f x)
  ;;

  let things
        ?(__FUNCTION__ : string = "")
        (k : Output.Kind.t)
        (prefix : string)
        (xs : 'a list)
        (f : 'a -> string)
    : unit
    =
    if is_enabled k
    then (
      (* [e s] emits [s] headed [prefix] *)
      let e : string -> unit =
        out ~prefix:(Some (Printf.sprintf "%s: " prefix)) ~__FUNCTION__ k
      in
      let index : int ref = ref 0 in
      (* [fx x] emits [x] headed by its index *)
      let fx (x : 'a) : unit =
        thing k (Printf.sprintf "%i" !index) x f;
        index := !index + 1
      in
      e "start";
      List.iter fx xs;
      e "end")
  ;;
end

include Body (struct
    let is_enabled = is_enabled
  end)

(***********************************************************************)

(* See the [.mli]: for a module that wants output settings independent of the
   user-facing configuration. Unlike the old [Logger.Make], this is {b not}
   threaded anywhere: it is declared and used within a single file. Only
   [Rocq_utils] and [Mebi_theories] need it. *)
module Scoped (X : sig
    val overrides : (Output.Kind.t * bool) list
  end) : S = Body (struct
    let is_enabled (k : Output.Kind.t) : bool =
      !the_enabled
      &&
      match List.assoc_opt k X.overrides with
      | Some b -> b
      | None -> is_enabled k
    ;;
  end)
