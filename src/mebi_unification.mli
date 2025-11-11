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
    Evd.evar_map ->
    Evd.evar_map ->
    Evd.econstr ->
    Evd.econstr ->
    Evd.econstr ->
    unit

  val naming_template : Namegen.intro_pattern_naming_expr
  val get_next_naming : Evd.evar_map -> Names.Id.t option

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

  val unify :
    ?debug:bool ->
    Environ.env ->
    Evd.evar_map ->
    t ->
    Evd.evar_map * Constructor_arg.fresh option * bool
end

module Problem : sig
  type t = Pair.t * Mebi_constr.Tree.t

  val to_string :
    ?indent:int -> Environ.env -> Evd.evar_map -> t -> string

  val unify_opt :
    ?debug:bool ->
    t ->
    (Constructor_arg.fresh option * Mebi_constr.Tree.t) option
    Mebi_wrapper.mm
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

  val append_fresh_opt :
    Constructor_arg.fresh list ->
    Constructor_arg.fresh option ->
    Constructor_arg.fresh list

  val unify_opt :
    ?debug:bool ->
    t ->
    (Constructor_arg.fresh list * Mebi_constr.Tree.t list)
    option
    Mebi_wrapper.mm
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
    (Constructor_arg.fresh list * r) option Mebi_wrapper.mm

  val retrieve :
    ?debug:bool ->
    int ->
    Constructor_arg.fresh list * t ->
    Evd.econstr ->
    Evd.econstr ->
    Mebi_setup.Enc.t * Problems.t list ->
    (Constructor_arg.fresh list * t) Mebi_wrapper.mm
end
