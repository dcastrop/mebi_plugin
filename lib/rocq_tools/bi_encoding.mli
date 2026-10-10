(** The two-way table between Rocq terms and their encodings (the
    model's states and labels are encodings): [F] maps a term to its
    encoding, [B] an encoding back to its term. One table per instance,
    shared counter ({!Encoding.S}) across instances. *)
module type S = sig
  type enc

  (** Tables keyed by term, compared and hashed under this instance's
      [sigma] ({!set_ctx}). *)
  module F : Hashtbl.S with type key = EConstr.t

  (** Tables keyed by encoding. *)
  module B : Hashtbl.S with type key = enc

  (** The two directions of one table. *)
  type maps =
    { fwd : enc F.t
    ; bck : EConstr.t B.t
    }

  (** This instance's table, once created ({!initialize}). *)
  val the_maps : maps ref option ref

  (** [reset ()] empties this instance's table {b and} resets the shared
      counter, so it is for a new command only: another instance's
      encodings would otherwise be handed out again. Raises nothing. *)
  val reset : unit -> unit

  (** [initialize ()] creates this instance's table if it does not exist
      yet, without touching the shared counter. Raises nothing. *)
  val initialize : unit -> unit

  (** Raised when the table is used before it is created. *)
  exception MapsNotInitialised of unit

  (** [get_the_maps ()] is this instance's table.

      @raise MapsNotInitialised if it was never created (raised here). *)
  val get_the_maps : unit -> maps ref

  (** [fwdmap ()] is the term -> encoding direction of the table.

      @raise MapsNotInitialised as {!get_the_maps} (propagated). *)
  val fwdmap : unit -> enc F.t

  (** [bckmap ()] is the encoding -> term direction of the table.

      @raise MapsNotInitialised as {!get_the_maps} (propagated). *)
  val bckmap : unit -> EConstr.t B.t

  (** [classify_key x] is a count, as "evar=E univ=U var=V", of the three
      things that can make a syntactic key miss a term it should match:
      undefined evars, non-empty universe instances, and local-context
      variables. A diagnostic for backlog item A1; see the implementation
      for what a zero does and does not prove. Raises nothing. *)
  val classify_key : EConstr.t -> string

  (** Raised by {!get_encoding}: the term has no encoding. *)
  exception EncodingNotFound of EConstr.t

  (** [get_encoding x] is [x]'s encoding (looked up as given, not
      normalised).

      @raise EncodingNotFound if [x] has none (raised here).
      @raise MapsNotInitialised as {!get_the_maps} (propagated). *)
  val get_encoding : EConstr.t -> enc

  (** [encode x] is [x]'s encoding, a fresh one added to the table if it
      has none.

      @raise MapsNotInitialised as {!get_the_maps} (propagated). *)
  val encode : EConstr.t -> enc

  (** [encoded x] is whether [x] has an encoding.

      @raise MapsNotInitialised as {!get_the_maps} (propagated). *)
  val encoded : EConstr.t -> bool

  (** Raised by {!get_econstr}: the encoding has no term. *)
  exception DecodingNotFound of enc

  (** [get_econstr e] is the term [e] encodes.

      @raise DecodingNotFound if [e] encodes none (raised here).
      @raise MapsNotInitialised as {!get_the_maps} (propagated). *)
  val get_econstr : enc -> EConstr.t

  (** Raised by {!decode}: the encoding has no term. *)
  exception CannotDecode of enc

  (** [decode e] is the term [e] encodes; as {!get_econstr}, with its own
      exception.

      @raise CannotDecode
        if [e] encodes none (raised here, for
        {!get_econstr}'s {!DecodingNotFound}).
      @raise MapsNotInitialised as {!get_the_maps} (propagated). *)
  val decode : enc -> EConstr.t

  (** [decode_opt e] is the term [e] encodes, or [None].

      @raise MapsNotInitialised as {!get_the_maps} (propagated). *)
  val decode_opt : enc -> EConstr.t option

  (** [opt_decode e] is the term the encoding [e] (if any) encodes, or
      [None].

      @raise MapsNotInitialised as {!get_the_maps} (propagated). *)
  val opt_decode : enc option -> EConstr.t option

  (** [decode_map m] is [m] with every key decoded.

      @raise CannotDecode if a key encodes no term (propagated from
                          {!decode}). *)
  val decode_map : 'a B.t -> 'a F.t

  (** [encode_map m] is [m] with every key encoded (new terms added to the
      table).

      @raise MapsNotInitialised as {!get_the_maps} (propagated). *)
  val encode_map : 'a F.t -> 'a B.t

  (** [to_list ()] is every (encoding, term) pair of the table, by
      encoding.

      @raise MapsNotInitialised as {!get_the_maps} (propagated). *)
  val to_list : unit -> (enc * EConstr.t) list

  (** [alias e] is a fresh encoding that decodes to the same term as [e]
      (the term still encodes to [e]): a second state for one term, for
      when two systems' state terms coincide but their states must not (see
      [Wrapper]'s conflict handling). Remembered for {!aliases_of}, cleared
      with the table.

      @raise DecodingNotFound
        if [e] encodes no term (propagated from
        {!get_econstr}). *)
  val alias : enc -> enc

  (** [aliases_of e] is the aliases made for [e] ({!alias}), newest first.
      Raises nothing. *)
  val aliases_of : enc -> enc list

  (** [set_ctx s] makes [s] the context the term keys of {!F} are compared
      and hashed under. Install it once, when the instance is created; it
      defaults to {!Rocq_context.global}. A table whose context moves can
      hash an entry under one [sigma] and look it up under another, so
      nothing should call this repeatedly. Raises nothing. *)
  val set_ctx : Rocq_context.source -> unit

  (** [current_ctx ()] is the [env]/[sigma] this instance was given, read
      now; [Rocq_monad.run] seeds the monad state from it. Raises
      nothing. *)
  val current_ctx : unit -> Rocq_context.t
end

module Make (Enc : Encoding.S) : S with type enc = Enc.t
