module type S = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val to_string : t -> string
end

module MakeFromEnc (Enc : Encoding.SEncoding) : S with type t = Enc.t = struct
  type t = Enc.t

  let equal (a : t) (b : t) = Enc.equal a b
  let compare (a : t) (b : t) = Enc.compare a b
  let hash (x : t) : int = Enc.hash x
  let to_string (x : t) : string = Enc.to_string x
end

module MakeFromEConstr (Ctx : Rocq_context.SRocq_context) :
  S with type t = EConstr.t = struct
  type t = EConstr.t

  let equal (a : t) (b : t) : bool = EConstr.eq_constr !(Ctx.sigma ()) a b
  let compare (a : t) (b : t) : int = 0

  let hash (x : t) : int =
    Constr.hash
      (EConstr.to_constr ~abort_on_undefined_evars:false !(Ctx.sigma ()) x)
  ;;

  let to_string (x : t) : string =
    Rocq_utils.Strfy.econstr !(Ctx.env ()) !(Ctx.sigma ()) x
  ;;
end
