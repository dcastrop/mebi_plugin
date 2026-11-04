(** The signatures of {!Rocq_monad}, declared once for its [.ml] and [.mli]. *)

module type S = sig
  include Bi_encoding.S

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

  (** [fold acc xs f] is [acc] threaded through [f x] for each [x] of [xs]
      in turn (a monadic fold; [acc] itself if [xs] is empty). When run,
      raises whatever [f] raises (propagated). *)
  val fold : 'a -> 'b list -> ('b -> 'a -> 'a mm) -> 'a mm

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
      [env] and [sigma] ({!val-state}) that returns a new [sigma] and a value,
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

  (** [get_env] is the current environment. *)
  val get_env : wrapper ref -> Environ.env in_wrapper

  (** [get_sigma] is the current evar map. *)
  val get_sigma : wrapper ref -> Evd.evar_map in_wrapper

  (** [get_fwdmap] is the current term -> encoding table. *)
  val get_fwdmap : wrapper ref -> enc F.t in_wrapper

  (** [fstring f x] is [f env sigma x] in this instance's current context:
      a printer ready to use outside the monad. Raises whatever [f]
      raises. *)
  val fstring : (Environ.env -> Evd.evar_map -> 'a -> string) -> 'a -> string
end
