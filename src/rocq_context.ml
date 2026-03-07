type t =
  { env : Environ.env
  ; sigma : Evd.evar_map
  }

module type S = sig
  val get : unit -> t ref
  val env : unit -> Environ.env ref
  val sigma : unit -> Evd.evar_map ref
  val update : Environ.env ref -> Evd.evar_map ref -> unit
end

module Make (X : sig
    val env : unit -> Environ.env ref
    val sigma : unit -> Evd.evar_map ref
  end) : S = struct
  let get () : t ref = ref { env = !(X.env ()); sigma = !(X.sigma ()) }
  let env () : Environ.env ref = ref !(get ()).env
  let sigma () : Evd.evar_map ref = ref !(get ()).sigma

  let update (env : Environ.env ref) (sigma : Evd.evar_map ref) : unit =
    get () := { env = !env; sigma = !sigma }
  ;;
end

module Default : S = Make (struct
    let env () : Environ.env ref = ref (Global.env ())
    let sigma () : Evd.evar_map ref = ref (Evd.from_env !(env ()))
  end)

module MakeFromGoal (X : sig
    val gl : Proofview.Goal.t ref
  end) : S = struct
  let get : unit -> t ref =
    fun () ->
    ref { env = Proofview.Goal.env !X.gl; sigma = Proofview.Goal.sigma !X.gl }
  ;;

  let env () : Environ.env ref = ref !(get ()).env
  let sigma () : Evd.evar_map ref = ref !(get ()).sigma

  let update (env : Environ.env ref) (sigma : Evd.evar_map ref) : unit =
    get () := { env = !env; sigma = !sigma }
  ;;
end
