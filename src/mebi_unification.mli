
type constructor_args =
  { lhs : EConstr.t
  ; act : EConstr.t
  ; rhs : EConstr.t
  }


module Constructor_arg : sig
  module Fresh : sig
    type t = {
      sigma : Evd.evar_map;
      evar : Evd.econstr;
      original : Evd.econstr;
    }

    val get_next :
      Environ.env ->
      Evd.evar_map ->
      Evd.econstr ->
      Evd.evar_map * t
  end

  type t = Normal of Evd.econstr | Fresh of Fresh.t

  val to_string : Environ.env -> Evd.evar_map -> t -> string
end

module Pair : sig
  type t = { a : Constructor_arg.t; b : Evd.econstr }

  val to_string :
    ?indent:int -> Environ.env -> Evd.evar_map -> t -> string

  val fresh :
    Environ.env ->
    Evd.evar_map ->
    Evd.econstr ->
    Evd.econstr ->
    Evd.evar_map * t

  val normal : Evd.econstr -> Evd.econstr -> t

  val make :
    Environ.env ->
    Evd.evar_map ->
    Evd.econstr ->
    Evd.econstr ->
    Evd.evar_map * t


  val unify :
    ?debug:bool ->
    Environ.env ->
    Evd.evar_map ->
    t ->
    Evd.evar_map * Constructor_arg.Fresh.t option * bool
end

module Problem : sig
  type t = Evd.econstr * Pair.t * Mebi_constr.Tree.t

  val to_string :
    ?indent:int -> Environ.env -> Evd.evar_map -> t -> string

end

module Problems : sig
  type t = Problem.t list

  val to_string :
    ?indent:int -> Environ.env -> Evd.evar_map -> t -> string

  val list_to_string :
    ?indent:int ->
    Environ.env ->
    Evd.evar_map ->
    t list ->
    string

end

module Constructors : sig
  type t = Mebi_constr.t list

  val to_string :
    ?indent:int -> Environ.env -> Evd.evar_map -> t -> string

  type r = Evd.econstr * Evd.econstr * Mebi_constr.Tree.t list

  val retrieve :
    ?debug:bool ->
    int ->
    t ->
    Evd.econstr ->
    Evd.econstr ->
    Mebi_setup.Enc.t * Problems.t list ->
    t Mebi_wrapper.mm
end
