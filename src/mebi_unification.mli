module Pair : sig
  type t = EConstr.t * EConstr.t

  val to_string : Environ.env -> Evd.evar_map -> t -> string
end

module Problem : sig
  type t = Pair.t * Mebi_constr.Tree.t

  val to_string : Environ.env -> Evd.evar_map -> t -> string
end

module Problems : sig
  type t = Problem.t list

  val to_string : Environ.env -> Evd.evar_map -> t -> string
end
