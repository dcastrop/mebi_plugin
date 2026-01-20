
type t = Rocq_bindings.t
and map = Rocq_bindings.map
and extractor_binding = Rocq_bindings.extractor_binding

module Instructions = Rocq_bindings.Instructions

val update_map :
  Rocq_bindings.map ->
  Constr.t ->
  Rocq_bindings.extractor_binding ->
  unit

val add_instruction :
  Rocq_bindings.Instructions.t ->
  Rocq_bindings.Instructions.t ->
  Rocq_bindings.Instructions.t

exception Mebi_bindings_CannotFindBindingName of Evd.econstr

val find_name :
  (Evd.econstr * Names.Name.t) list ->
  Evd.econstr ->
  Names.Name.t Mebi_wrapper.mm

val extract_binding_map :
  (Evd.econstr * Names.Name.t) list ->
  Evd.econstr ->
  Constr.t ->
  map Mebi_wrapper.mm

val make_map :
  (Evd.econstr * Names.Name.t) list ->
  Evd.econstr * Constr.t ->
  map option Mebi_wrapper.mm

val use_no_bindings : map option list -> bool

val extract :
  (Evd.econstr * Names.Name.t) list ->
  Evd.econstr * Constr.t ->
  Evd.econstr * Constr.t ->
  Evd.econstr * Constr.t ->
  t Mebi_wrapper.mm

val extract_info :
  Mebi_ind.t -> Model_info.rocq_constructor list Mebi_wrapper.mm

val get_quantified_hyp :
  Names.Name.t -> Tactypes.quantified_hypothesis

exception
  Mebi_bindings_BindingInstruction_NotApp of Evd.econstr

exception
  Mebi_bindings_BindingInstruction_Undefined of
    Evd.econstr * Evd.econstr

exception
  Mebi_bindings_BindingInstruction_IndexOutOfBounds of
    Evd.econstr * int

exception
  Mebi_bindings_BindingInstruction_NEQ of
    Evd.econstr * Constr.t

val get_bound_term :
  Evd.econstr -> Instructions.t -> Evd.econstr Mebi_wrapper.mm

val get_explicit_bindings :
  Evd.econstr * map option ->
  Evd.econstr Tactypes.explicit_bindings Mebi_wrapper.mm

val get :
  Evd.econstr ->
  Evd.econstr option ->
  Evd.econstr option ->
  t ->
  Evd.econstr Tactypes.bindings Mebi_wrapper.mm
