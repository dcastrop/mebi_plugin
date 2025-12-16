
type constructor_args = {
  lhs : Evd.econstr;
  act : Evd.econstr;
  rhs : Evd.econstr;
}

module Pair : sig
  type t = { a : Evd.econstr; b : Evd.econstr }

  val to_string :
    Environ.env ->
    Evd.evar_map ->
    ?args:Utils.Strfy.style_args ->
    t ->
    string

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
    ?args:Utils.Strfy.style_args ->
    Evd.econstr ->
    Evd.econstr ->
    unit

  val debug_unifyerr :
    Environ.env ->
    Evd.evar_map ->
    ?args:Utils.Strfy.style_args ->
    Evd.econstr ->
    Evd.econstr ->
    Evd.econstr ->
    Evd.econstr ->
    unit

  val w_unify :
    Environ.env ->
    Evd.evar_map ->
    Evd.econstr ->
    Evd.econstr ->
    Evd.evar_map * bool

  val unify :
    Environ.env ->
    Evd.evar_map ->
    t ->
    Evd.evar_map * bool
end

module Problem : sig
  type t = {
    act : Pair.t;
    goto : Pair.t;
    tree : Mebi_constr.Tree.t;
  }

  val to_string :
    Environ.env ->
    Evd.evar_map ->
    ?args:Utils.Strfy.style_args ->
    t ->
    string

  val unify_pair_opt :
    Pair.t -> bool Mebi_wrapper.mm

  val unify_opt :
    t ->
    Mebi_constr.Tree.t option Mebi_wrapper.mm
end

module Problems : sig
  type t = { sigma : Evd.evar_map; to_unify : Problem.t list }

  val empty : unit -> t Mebi_wrapper.mm
  val is_empty : t -> bool
  val list_is_empty : t list -> bool

  val to_string :
    Environ.env -> ?args:Utils.Strfy.style_args -> t -> string

  val list_to_string :
    Environ.env ->
    ?args:Utils.Strfy.style_args ->
    t list ->
    string

  val unify_list_opt :
    Problem.t list ->
    Mebi_constr.Tree.t list option Mebi_wrapper.mm

  val sandbox_unify_all_opt :
    Evd.econstr ->
    Evd.econstr ->
    t ->
    (Evd.econstr * Evd.econstr * Mebi_constr.Tree.t list)
    option
    Mebi_wrapper.mm
end

module Constructors : sig
  type t = Mebi_constr.t list

  val to_string :
    Environ.env ->
    Evd.evar_map ->
    ?args:Utils.Strfy.style_args ->
    t ->
    string

  val retrieve :
    int ->
    t ->
    Evd.econstr ->
    Evd.econstr ->
    Mebi_setup.Enc.t * Problems.t list ->
    t Mebi_wrapper.mm
end
