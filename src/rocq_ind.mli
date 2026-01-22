

module LTS : sig
  type t = {
    term_type : EConstr.t;
    label_type : EConstr.t;
    constructor_types : constructor array;
  }

  and constructor = {
    name : Names.Id.t;
    constructor : Rocq_utils.ind_constr;
  }
end

type 'a t = { enc : 'a; ind : EConstr.t; kind : kind }
and kind = Type of EConstr.t option | LTS of LTS.t

exception Rocq_ind_UnexpectedKind of kind

val get_lts : 'a t -> LTS.t
val get_lts_term_type : 'a t -> EConstr.t
val get_lts_label_type : 'a t -> EConstr.t
val get_lts_constructor_types : 'a t -> LTS.constructor array
val get_lts_constructor_names : 'a t -> Names.Id.t array
val get_lts_constructors : 'a t -> Rocq_utils.ind_constr array

exception
  Rocq_ind_mip_InconsistentNumConstructors of
    Declarations.one_inductive_body

val mip_to_lts_constructors :
  Declarations.one_inductive_body -> LTS.constructor array
