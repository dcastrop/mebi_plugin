module Make : (_ : Logger.S)
    (_ : Rocq_context.SRocq_context)
    (Enc : Encoding.S)
    -> sig
  module F : Hashtbl.S with type key = EConstr.t
  module B : Hashtbl.S with type key = Enc.t

  type maps =
    { fwd : Enc.t F.t
    ; bck : EConstr.t B.t
    }

  val the_maps : maps ref option ref
  val reset : unit -> unit
  val initialize : unit -> unit

  exception MapsNotInitialised of unit

  val get_the_maps : unit -> maps ref
  val fwdmap : unit -> Enc.t F.t
  val bckmap : unit -> EConstr.t B.t

  exception EncodingNotFound of EConstr.t

  val get_encoding : EConstr.t -> Enc.t
  val encode : EConstr.t -> Enc.t
  val encoded : EConstr.t -> bool

  exception DecodingNotFound of Enc.t

  val get_econstr : Enc.t -> EConstr.t

  exception CannotDecode of Enc.t

  val decode : Enc.t -> EConstr.t
  val decode_opt : Enc.t -> EConstr.t option
  val decode_map : 'a B.t -> 'a F.t
  val encode_map : 'a F.t -> 'a B.t
  val to_list : unit -> (Enc.t * EConstr.t) list
end
