type t = Rocq_bindings.t
and map = Rocq_bindings.map
and extractor_binding = Rocq_bindings.extractor_binding

module Instructions = Rocq_bindings.Instructions

val update_map
  :  Rocq_bindings.map
  -> Constr.t
  -> Rocq_bindings.extractor_binding
  -> unit

val add_instruction
  :  Rocq_bindings.Instructions.t
  -> Rocq_bindings.Instructions.t
  -> Rocq_bindings.Instructions.t

exception Mebi_bindings_CannotFindBindingName of EConstr.t

val find_name
  :  Evd.evar_map
  -> (EConstr.t * Names.Name.t) list
  -> EConstr.t
  -> Names.Name.t

val extract_binding_map
  :  Environ.env
  -> Evd.evar_map
  -> (EConstr.t * Names.Name.t) list
  -> EConstr.t
  -> Constr.t
  -> map

val make_map
  :  Environ.env
  -> Evd.evar_map
  -> (EConstr.t * Names.Name.t) list
  -> EConstr.t * Constr.t
  -> map option

val use_no_bindings : map option list -> bool

val extract
  :  Environ.env
  -> Evd.evar_map
  -> (EConstr.t * Names.Name.t) list
  -> EConstr.t * Constr.t
  -> EConstr.t * Constr.t
  -> EConstr.t * Constr.t
  -> t

val extract_info
  :  Environ.env
  -> Evd.evar_map
  -> Mebi_ind.t
  -> Evd.evar_map * Model_info.rocq_constructor list

val get_quantified_hyp : Names.Name.t -> Tactypes.quantified_hypothesis

exception Mebi_bindings_BindingInstruction_NotApp of EConstr.t
exception Mebi_bindings_BindingInstruction_Undefined of EConstr.t * EConstr.t
exception Mebi_bindings_BindingInstruction_IndexOutOfBounds of EConstr.t * int
exception Mebi_bindings_BindingInstruction_NEQ of EConstr.t * Constr.t

val get_bound_term
  :  Environ.env
  -> Evd.evar_map
  -> EConstr.t
  -> Instructions.t
  -> EConstr.t

val get_explicit_bindings
  :  Environ.env
  -> Evd.evar_map
  -> EConstr.t * map option
  -> EConstr.t Tactypes.explicit_bindings

val get
  :  Environ.env
  -> Evd.evar_map
  -> EConstr.t
  -> EConstr.t option
  -> EConstr.t option
  -> t
  -> EConstr.t Tactypes.bindings
