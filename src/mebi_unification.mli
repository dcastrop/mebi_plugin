val default_debug : bool
val debugerr : bool

type constructor_args = {
  lhs : Evd.econstr;
  act : Evd.econstr;
  rhs : Evd.econstr;
}

module Constructor_arg : sig
  type t = Normal of Evd.econstr | Fresh of fresh

  and fresh = {
    sigma : Evd.evar_map;
    evar : Evd.econstr;
    original : Evd.econstr;
  }

  val to_string : Environ.env -> Evd.evar_map -> t -> string
end

module Pair : sig
  type t = { a : Constructor_arg.t; b : Evd.econstr }

  val to_string :
    ?indent:int -> Environ.env -> Evd.evar_map -> t -> string

  val _debug_fresh :
    Environ.env ->
    'a ->
    Evd.evar_map ->
    Evd.econstr ->
    Evd.econstr ->
    Evd.econstr ->
    unit

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

  val debug_unify : Environ.env -> Evd.evar_map -> t -> unit

  val debug_unifyerr :
    Environ.env ->
    Evd.evar_map ->
    t ->
    Evd.econstr ->
    Evd.econstr ->
    unit

  val w_unify :
    Environ.env -> Evd.evar_map -> t -> Evd.evar_map

  val unify : ?debug:bool -> t -> bool Mebi_wrapper.mm
end

module Problem : sig
  type t = Pair.t * Mebi_constr.Tree.t

  val to_string :
    ?indent:int -> Environ.env -> Evd.evar_map -> t -> string

  val unify_opt :
    ?debug:bool ->
    t ->
    Mebi_constr.Tree.t option Mebi_wrapper.mm
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

  val unify_opt :
    ?debug:bool ->
    t ->
    Mebi_constr.Tree.t list option Mebi_wrapper.mm
end

module Constructors : sig
  type t = Mebi_constr.t list

  val to_string :
    ?indent:int -> Environ.env -> Evd.evar_map -> t -> string

  type r = Evd.econstr * Mebi_constr.Tree.t list

  val sandbox_unify_all_opt :
    ?debug:bool ->
    Evd.econstr ->
    Problems.t ->
    r option Mebi_wrapper.mm

  val retrieve :
    ?debug:bool ->
    int ->
    Mebi_constr.t list ->
    Evd.econstr ->
    Evd.econstr ->
    Mebi_setup.Enc.t * Problems.t list ->
    t Mebi_wrapper.mm
end
