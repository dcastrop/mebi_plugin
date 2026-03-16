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

module Make : (_ : sig
                 val env : unit -> Environ.env ref
                 val sigma : unit -> Evd.evar_map ref
               end)
    -> S

module Default : S

module MakeFromGoal : (_ : sig
                         val gl : Proofview.Goal.t ref
                       end)
    -> S
