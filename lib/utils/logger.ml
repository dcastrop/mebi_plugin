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
    (match prefix with None -> body | Some p -> p ^ body)
;;

(** The installed sink. *)
let the_sink : sink ref = ref default_sink

(* See the [.mli]. *)
let set_sink (f : sink) : unit = the_sink := f

(***********************************************************************)

(** [index k] is [k]'s slot in {!the_config}. *)
let index : Output.Kind.t -> int = function
  | Debug -> 0
  | Info -> 1
  | Notice -> 2
  | Warning -> 3
  | Error -> 4
  | Trace -> 5
  | Result -> 6
  | Show -> 7
;;

(** Whether each kind is on, by {!index}. An array, not a table: every
    message checks it, disabled or not, including [trace] calls on hot
    paths. *)
let the_config : bool array = Array.make (List.length Output.Kind.all) false

let reset_config () : unit =
  List.iter
    (fun k -> the_config.(index k) <- Output.Kind.default k)
    Output.Kind.all
;;

let () = reset_config ()
let configure (k : Output.Kind.t) (b : bool) : unit = the_config.(index k) <- b

(** A test installed by the plugin ({!set_enabled_test}), replacing the
    table: inside Rocq, whether a kind is on is Rocq's own setting. *)
let the_test : (Output.Kind.t -> bool) option ref = ref None

let set_enabled_test (f : Output.Kind.t -> bool) : unit = the_test := Some f

let is_enabled (k : Output.Kind.t) : bool =
  match !the_test with Some f -> f k | None -> the_config.(index k)
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
    if E.is_enabled Debug then out ~__FUNCTION__ Debug x
  ;;

  let info ?(__FUNCTION__ : string = "") (x : string) : unit =
    if E.is_enabled Info then out ~__FUNCTION__ Info x
  ;;

  let notice ?(__FUNCTION__ : string = "") (x : string) : unit =
    if E.is_enabled Notice then out ~__FUNCTION__ Notice x
  ;;

  let warning ?(__FUNCTION__ : string = "") (x : string) : unit =
    if E.is_enabled Warning then out ~__FUNCTION__ Warning x
  ;;

  let error ?(__FUNCTION__ : string = "") (x : string) : unit =
    if E.is_enabled Error then out ~__FUNCTION__ Error x
  ;;

  let trace ?(__FUNCTION__ : string = "") (x : string) : unit =
    if E.is_enabled Trace then out ~__FUNCTION__ Trace x
  ;;

  let result ?(__FUNCTION__ : string = "") (x : string) : unit =
    if E.is_enabled Result then out ~__FUNCTION__ Result x
  ;;

  let show ?(__FUNCTION__ : string = "") (x : string) : unit =
    if E.is_enabled Show then out ~__FUNCTION__ Show x
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
        thing ~__FUNCTION__ k (Printf.sprintf "%i" !index) x f;
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
   threaded anywhere: it is declared and used within a single file. *)
module Scoped (X : sig
    val overrides : (Output.Kind.t * bool) list
  end) : S = Body (struct
    let is_enabled (k : Output.Kind.t) : bool =
      match List.assoc_opt k X.overrides with
      | Some b -> b
      | None -> is_enabled k
    ;;
  end)
