  type t =
    { env : Environ.env
    ; sigma : Evd.evar_map
    }

  module type SRocq_context = sig
    val get : unit -> t ref
    val env : unit -> Environ.env ref
    val sigma : unit -> Evd.evar_map ref
    val update : Environ.env ref -> Evd.evar_map ref -> unit
  end

  module type S = sig
    val env : unit -> Environ.env ref
    val sigma : unit -> Evd.evar_map ref
  end

  module Make (X : S) : SRocq_context = struct
    let get () : t ref = ref { env = !(X.env ()); sigma = !(X.sigma ()) }
    let env () : Environ.env ref = ref !(get ()).env
    let sigma () : Evd.evar_map ref = ref !(get ()).sigma

    let update (env : Environ.env ref) (sigma : Evd.evar_map ref) : unit =
      get () := { env = !env; sigma = !sigma }
    ;;
  end

  module Default : SRocq_context = Make (struct
      let env () : Environ.env ref = ref (Global.env ())
      let sigma () : Evd.evar_map ref = ref (Evd.from_env !(env ()))
    end)

    
module MakeEConstrMap (Ctx : SRocq_context) :
  Hashtbl.S with type key = EConstr.t = Hashtbl.Make (struct
    type t = EConstr.t

    let equal (a : t) (b : t) : bool = EConstr.eq_constr !(Ctx.sigma ()) a b

    let hash (x : t) : int =
      Constr.hash
        (EConstr.to_constr ~abort_on_undefined_evars:false !(Ctx.sigma ()) x)
    ;;
  end)
