val default_debug : bool
val debugerr : bool

type constructor_args = {
  lhs : Evd.econstr;
  act : Evd.econstr;
  rhs : Evd.econstr;
}

module Constructor_arg : sig
  module Fresh : sig
    type t = {
      sigma : Evd.evar_map;
      evar : Evd.econstr;
      original : Evd.econstr;
    }

    exception CouldNotGetNextFreshEvarName of unit

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

  val _debug_fresh :
    Environ.env ->
    Evd.evar_map ->
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

  val debug_unify :
    Environ.env ->
    Evd.evar_map ->
    Evd.econstr ->
    Evd.econstr ->
    unit

  val debug_unifyerr :
    Environ.env ->
    Evd.evar_map ->
    Evd.econstr ->
    Evd.econstr ->
    Evd.econstr ->
    Evd.econstr ->
    unit

  val w_unify :
    ?debug:bool ->
    Environ.env ->
    Evd.evar_map ->
    Evd.econstr ->
    Evd.econstr ->
    Evd.evar_map * bool

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

  val unify_opt :
    ?debug:bool ->
    t ->
    (Evd.econstr
    * Constructor_arg.Fresh.t option
    * Mebi_constr.Tree.t)
    option
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
    Constructor_arg.Fresh.t list ->
    Constructor_arg.Fresh.t option ->
    Constructor_arg.Fresh.t list

  val unify_opt :
    ?debug:bool ->
    t ->
    (Evd.econstr
    * Constructor_arg.Fresh.t option
    * Mebi_constr.Tree.t)
    list
    option
    Mebi_wrapper.mm
end

module Constructors : sig
  type t = Mebi_constr.t list

  val to_string :
    ?indent:int -> Environ.env -> Evd.evar_map -> t -> string

  type r = Evd.econstr * Evd.econstr * Mebi_constr.Tree.t list

  exception NotApp of unit

  val _debug_unbox_fresh :
    Evd.econstr ->
    Constructor_arg.Fresh.t ->
    unit Mebi_wrapper.mm

  val sandbox_unbox_fresh :
    Evd.econstr ->
    Constructor_arg.Fresh.t ->
    Evd.econstr Mebi_wrapper.mm

  val unbox_fresh :
    Evd.econstr ->
    Evd.econstr ->
    (Evd.econstr
    * Constructor_arg.Fresh.t option
    * Mebi_constr.Tree.t)
    list ->
    r Mebi_wrapper.mm

  val sandbox_unify_all_opt :
    ?debug:bool ->
    Evd.econstr ->
    Evd.econstr ->
    Problems.t ->
    r option Mebi_wrapper.mm

  val retrieve :
    ?debug:bool ->
    int ->
    t ->
    Evd.econstr ->
    Evd.econstr ->
    Mebi_setup.Enc.t * Problems.t list ->
    t Mebi_wrapper.mm
end
