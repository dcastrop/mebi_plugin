(** The plugin's monad over the Rocq context ([env], [sigma]) and the
    encoding tables ({!Bi_encoding}): a computation ['a mm] is a function
    from the current state to a value and the next state, so threading
    [sigma] through Rocq calls is implicit.

    {b Exceptions are deferred.} An ['a mm] does nothing until it is run
    ({!run}), so an exception raised inside it -- by a [state] function, or
    in the continuation of a [let*] -- is raised when it runs, not when it
    is built. A [try ... with] around the {e construction} of a monadic
    value therefore does not catch it; handle it around {!run}, or inside
    the function given to {!state}. [TODO.md] ("try around a monadic
    value") lists the places that get this wrong. *)
module type S = sig
  include Bi_encoding.S

  (** [bienc_to_list ()] is every (encoding, term) pair of the current
      table. Raises nothing. *)
  val bienc_to_list : unit -> (enc * EConstr.t) list

  (** A computation: given the state, its value and the next state. *)
  type 'a mm = wrapper ref -> 'a in_wrapper

  and wrapper =
    { ctx : Rocq_context.t ref
    ; maps : maps ref
    }

  and 'a in_wrapper =
    { state : wrapper ref
    ; value : 'a
    }

  (** [run ?reset_encoding m] is the value of [m], evaluated against this
      instance's context (as installed by [set_ctx]; by default the global
      environment), the encoding table emptied first if [reset_encoding],
      else created if missing. A stack that needs a different context is a
      different instance.

      Raises whatever [m] raises (propagated, here: the place to handle a
      monadic value's exceptions). *)
  val run : ?reset_encoding:bool -> 'a mm -> 'a

  (** [return x] is the computation whose value is [x], the state
      unchanged. *)
  val return : 'a -> 'a mm

  (** [bind m f] is [m] then [f] of its value, on [m]'s next state. Raises
      nothing when built; when run, whatever [m] or [f] raises. *)
  val bind : 'a mm -> ('a -> 'b mm) -> 'b mm

  (** [map f m] is [m] with [f] applied to its value. *)
  val map : ('a -> 'b) -> 'a mm -> 'b mm

  (** [product m n] is [m] then [n], both values paired. *)
  val product : 'a mm -> 'b mm -> ('a * 'b) mm

  (** [iterate i j acc f] is [acc] threaded through [f i], [f (i + 1)], ...,
      [f j] in turn (a monadic for loop; [acc] itself if [i > j]). When
      run, raises whatever [f] raises (propagated). *)
  val iterate : int -> int -> 'a -> (int -> 'a -> 'a mm) -> 'a mm

  (** [state f] is the computation that gives [f] the current [env] and
      [sigma] and keeps the [sigma] it returns: the way into Rocq's API.
      When run, raises whatever [f] raises (propagated; catch it inside
      [f] to handle it here). *)
  val state
    :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
    -> wrapper ref
    -> 'a in_wrapper

  (** [sandbox ?sigma m] is [m]'s value, the state left as it was ([sigma]
      changes inside [m] discarded); with [sigma], [m] runs from that evar
      map instead of the current one. When run, raises whatever [m] raises
      (propagated). *)
  val sandbox : ?sigma:Evd.evar_map -> 'a mm -> wrapper ref -> 'a in_wrapper

  (** Binding operators: [let*] binds a computation, [let+] maps one,
      [and+] pairs two, and [let$] / [let$*] / [let$+] run a function of
      [env] and [sigma] ({!state}) that returns a new [sigma] and a value,
      only a new [sigma], or only a value. *)
  module type SYNTAX = sig
    val ( let+ ) : 'a mm -> ('a -> 'b) -> 'b mm
    val ( let* ) : 'a mm -> ('a -> 'b mm) -> 'b mm

    val ( let$ )
      :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
      -> ('a -> 'b mm)
      -> 'b mm

    val ( let$* )
      :  (Environ.env -> Evd.evar_map -> Evd.evar_map)
      -> (unit -> 'b mm)
      -> 'b mm

    val ( let$+ )
      :  (Environ.env -> Evd.evar_map -> 'a)
      -> ('a -> 'b mm)
      -> 'b mm

    val ( and+ ) : 'a mm -> 'b mm -> ('a * 'b) mm
  end

  module Syntax : SYNTAX

  (** [econstr_normalize x] is [x] fully normalised ([nf_all]) in the
      current context. Raises nothing. *)
  val econstr_normalize : EConstr.t -> EConstr.t mm

  (** [encode x] is the encoding of [x] normalised, run at once in this
      instance's context; new terms are added to the table. Raises
      nothing. *)
  val encode : EConstr.t -> enc

  (** [get_ctx] is the current context. Each [get_*] reads the state
      without changing it, and raises nothing. *)
  val get_ctx : wrapper ref -> Rocq_context.t in_wrapper

  (** [get_env] is the current environment. *)
  val get_env : wrapper ref -> Environ.env in_wrapper

  (** [get_sigma] is the current evar map. *)
  val get_sigma : wrapper ref -> Evd.evar_map in_wrapper

  (** [get_maps] is the current encoding tables. *)
  val get_maps : wrapper ref -> maps in_wrapper

  (** [get_fwdmap] is the current term -> encoding table. *)
  val get_fwdmap : wrapper ref -> enc F.t in_wrapper

  (** [get_bckmap] is the current encoding -> term table. *)
  val get_bckmap : wrapper ref -> EConstr.t B.t in_wrapper

  (** [fstring f x] is [f env sigma x] in this instance's current context:
      a printer ready to use outside the monad. Raises whatever [f]
      raises. *)
  val fstring : (Environ.env -> Evd.evar_map -> 'a -> string) -> 'a -> string
end

module Make (Enc : Encoding.S) : S with type enc = Enc.t
