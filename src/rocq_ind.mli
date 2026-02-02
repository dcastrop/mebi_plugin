module LTS : sig
  type t =
    { term_type : Evd.econstr
    ; label_type : Evd.econstr
    ; constructor_types : constructor array
    }

  and constructor =
    { name : Names.variable
    ; constructor : Rocq_utils.ind_constr
    }
end

type 'a t =
  { enc : 'a
  ; ind : Evd.econstr
  ; kind : kind
  }

and kind =
  | Type of Evd.econstr option
  | LTS of LTS.t

val to_string : ('a -> string) -> Environ.env -> Evd.evar_map ->  'a t -> string

exception Rocq_ind_UnexpectedKind of kind

val get_lts : 'a t -> LTS.t
val get_lts_term_type : 'a t -> Evd.econstr
val get_lts_label_type : 'a t -> Evd.econstr
val get_lts_constructor_types : 'a t -> LTS.constructor array
val get_lts_constructor_names : 'a t -> Names.variable array
val get_lts_constructors : 'a t -> Rocq_utils.ind_constr array

exception
  Rocq_ind_mip_InconsistentNumConstructors of Declarations.one_inductive_body

val mip_to_lts_constructors
  :  Declarations.one_inductive_body
  -> LTS.constructor array
