
module C : sig
  type key = Constr.t
  type !'a t

  val create : int -> 'a t
  val clear : 'a t -> unit
  val reset : 'a t -> unit
  val copy : 'a t -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val remove : 'a t -> key -> unit
  val find : 'a t -> key -> 'a
  val find_opt : 'a t -> key -> 'a option
  val find_all : 'a t -> key -> 'a list
  val replace : 'a t -> key -> 'a -> unit
  val mem : 'a t -> key -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val filter_map_inplace :
    (key -> 'a -> 'a option) -> 'a t -> unit

  val fold :
    (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

  val length : 'a t -> int
  val stats : 'a t -> Hashtbl.statistics
  val to_seq : 'a t -> (key * 'a) Seq.t
  val to_seq_keys : 'a t -> key Seq.t
  val to_seq_values : 'a t -> 'a Seq.t
  val add_seq : 'a t -> (key * 'a) Seq.t -> unit
  val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
  val of_seq : (key * 'a) Seq.t -> 'a t
end

module Instruction : sig
  type t =
    | Undefined
    | Done
    | Arg of { root : Constr.t; index : int; cont : t }

  exception Rocq_bindings_CannotAppendDone of unit

  val append : t -> t -> t
  val length : t -> int
  val to_string : Environ.env -> Evd.evar_map -> t -> string
end

type t = No_Bindings | Use_Bindings of arg_maps

and arg_maps = {
  from : map option;
  action : map option;
  goto : map option;
}

and map = extractor_binding C.t
and extractor_binding = Names.Name.t * Instruction.t
and constructor = { index : int; name : string; bindings : t }

val update_map : map -> Constr.t -> extractor_binding -> unit

val to_string :
  ?args:Utils.Strfy.style_args ->
  ?envsigma:(Environ.env * Evd.evar_map) option ->
  t ->
  string

val constructor_to_string :
  ?args:Utils.Strfy.style_args ->
  ?envsigma:(Environ.env * Evd.evar_map) option ->
  constructor ->
  string

exception Rocq_bindings_CannotFindBindingName of EConstr.t

val find_name :
  Evd.evar_map ->
  (EConstr.t * Names.Name.t) list ->
  EConstr.t ->
  Names.Name.t

val extract_binding_map :
  Environ.env ->
  Evd.evar_map ->
  (EConstr.t * Names.Name.t) list ->
  EConstr.t ->
  Constr.t ->
  map

val make_map :
  Environ.env ->
  Evd.evar_map ->
  (EConstr.t * Names.Name.t) list ->
  EConstr.t * Constr.t ->
  map option

val use_no_bindings : map option list -> bool

val extract :
  Environ.env ->
  Evd.evar_map ->
  (EConstr.t * Names.Name.t) list ->
  EConstr.t * Constr.t ->
  EConstr.t * Constr.t ->
  EConstr.t * Constr.t ->
  t

val extract_info :
  Environ.env ->
  Evd.evar_map ->
  'a Rocq_ind.t ->
  Evd.evar_map * constructor list

val get_quantified_hyp :
  Names.Name.t -> Tactypes.quantified_hypothesis

exception Rocq_bindings_BindingInstruction_NotApp of EConstr.t

exception
  Rocq_bindings_BindingInstruction_Undefined of
    EConstr.t * EConstr.t

exception
  Rocq_bindings_BindingInstruction_IndexOutOfBounds of
    EConstr.t * int

exception
  Rocq_bindings_BindingInstruction_NEQ of EConstr.t * Constr.t

val get_bound_term :
  Environ.env ->
  Evd.evar_map ->
  EConstr.t ->
  Instruction.t ->
  EConstr.t

val get_explicit_bindings :
  Environ.env ->
  Evd.evar_map ->
  EConstr.t * map option ->
  EConstr.t Tactypes.explicit_bindings

val get :
  Environ.env ->
  Evd.evar_map ->
  EConstr.t ->
  EConstr.t option ->
  EConstr.t option ->
  t ->
  EConstr.t Tactypes.bindings
