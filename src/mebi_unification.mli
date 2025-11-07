module Pair : sig
  type t = Evd.econstr * Evd.econstr

  val to_string : Environ.env -> Evd.evar_map -> t -> string
  val debug_unify : ('a -> string) -> 'a -> 'a -> unit

  val debug_unifyerr :
    ('a -> string) -> 'a -> 'a -> 'a -> 'a -> unit

  val unify : ?debug:bool -> t -> bool Mebi_wrapper.mm
end

module Problem : sig
  type t = Pair.t * Mebi_constr.Tree.t

  val to_string : Environ.env -> Evd.evar_map -> t -> string

  val unify_opt :
    ?debug:bool ->
    t ->
    Mebi_constr.Tree.t option Mebi_wrapper.mm
end

module Problems : sig
  type t = Problem.t list

  val to_string : Environ.env -> Evd.evar_map -> t -> string

  val unify_opt :
    ?debug:bool ->
    t ->
    Mebi_constr.Tree.t list option Mebi_wrapper.mm
end

module Constructor : sig
  type t = Evd.econstr * Mebi_constr.Tree.t list

  val sandbox_unify_all_opt :
    ?debug:bool ->
    Evd.econstr ->
    Problems.t ->
    t option Mebi_wrapper.mm
end

module Constructors : sig
  type t = Mebi_constr.t list

  val retrieve :
    ?debug:bool ->
    int ->
    Mebi_constr.t list ->
    Evd.econstr ->
    Evd.econstr ->
    Mebi_wrapper.Enc.t * Problems.t list ->
    Mebi_constr.t list Mebi_wrapper.mm
end
