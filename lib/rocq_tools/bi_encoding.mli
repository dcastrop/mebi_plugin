module type S = sig
  type enc

  module F : Hashtbl.S with type key = EConstr.t
  module B : Hashtbl.S with type key = enc

  type maps =
    { fwd : enc F.t
    ; bck : EConstr.t B.t
    }

  val the_maps : maps ref option ref
  val reset : unit -> unit
  val initialize : unit -> unit

  exception MapsNotInitialised of unit

  val get_the_maps : unit -> maps ref
  val fwdmap : unit -> enc F.t
  val bckmap : unit -> EConstr.t B.t

  exception EncodingNotFound of EConstr.t

  val get_encoding : EConstr.t -> enc
  val encode : EConstr.t -> enc
  val encoded : EConstr.t -> bool

  exception DecodingNotFound of enc

  val get_econstr : enc -> EConstr.t

  exception CannotDecode of enc

  val decode : enc -> EConstr.t
  val decode_opt : enc -> EConstr.t option
  val opt_decode : enc option -> EConstr.t option
  val decode_map : 'a B.t -> 'a F.t
  val encode_map : 'a F.t -> 'a B.t
  val to_list : unit -> (enc * EConstr.t) list

  (** The [EConstr.t] keys of [F] are compared and hashed under a [sigma], so
      this instance's table has to know which context to read it from. Install
      it once, when the instance is created; it defaults to
      [Rocq_context.global]. A table whose context moves can hash an entry under
      one [sigma] and look it up under another, so nothing should be calling
      this repeatedly. *)
  val set_ctx : Rocq_context.source -> unit

  (** The [env]/[sigma] this instance was given, read now. [Rocq_monad.run]
      seeds the monad state from it. *)
  val current_ctx : unit -> Rocq_context.t
end

module Make (Enc : Encoding.S) : S with type enc = Enc.t
