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
end

module Make (Log : Logger.S) (Ctx : Rocq_context.S) (Enc : Encoding.S) :
  S with type enc = Enc.t