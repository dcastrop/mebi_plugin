(** A head and its arguments, as [AtomicType (ty, tys)] and [App (f, args)]
    give them. *)
type 'a kind_pair = 'a * 'a array

(** Raised by {!econstr_to_atomic}: a type, but not an atomic one. *)
exception
  Rocq_utils_EConstrIsNot_Atomic of
    (Evd.evar_map * Evd.econstr * EConstr.kind_of_type)

(** Raised by {!econstr_to_atomic}: not a type at all. *)
exception Rocq_utils_EConstrIsNotA_Type of (Evd.evar_map * Evd.econstr * string)

(** [econstr_to_atomic sigma x] is the head and arguments of the atomic
    type [x].

    @raise Rocq_utils_EConstrIsNot_Atomic
      if [x] is a type but not an
      atomic one (a product, sort, ...; raised here).
    @raise Rocq_utils_EConstrIsNotA_Type
      if [x] is not a type (raised here,
      for [kind_of_type]'s [Failure]). *)
val econstr_to_atomic : Evd.evar_map -> Evd.econstr -> Evd.econstr kind_pair

(** A [Constr.t]'s kind. *)
type constr_kind =
  ( Constr.t
    , Constr.t
    , Sorts.Quality.t
    , UVars.Instance.t
    , Sorts.relevance )
    Constr.kind_of_term

(** Raised by {!constr_to_app}: not an application. *)
exception
  Rocq_utils_ConstrIsNot_App of
    (Constr.t
    * ( Constr.t
        , Constr.t
        , Sorts.t
        , UVars.Instance.t
        , Sorts.relevance )
        Constr.kind_of_term)

(** [constr_to_app x] is the head and arguments of the application [x].

    @raise Rocq_utils_ConstrIsNot_App if [x] is not an application (raised
                                      here). *)
val constr_to_app : Constr.t -> Constr.t kind_pair

(** An [EConstr.t]'s kind. *)
type econstr_kind =
  ( Evd.econstr
    , Evd.econstr
    , Evd.esorts
    , EConstr.EInstance.t
    , Evd.erelevance )
    Constr.kind_of_term

(** Raised by {!econstr_to_app}: not an application. *)
exception
  Rocq_utils_EConstrIsNot_App of (Evd.evar_map * Evd.econstr * econstr_kind)

(** [econstr_to_app sigma x] is the head and arguments of the application
    [x].

    @raise Rocq_utils_EConstrIsNot_App
      if [x] is not an application (raised
      here). *)
val econstr_to_app : Evd.evar_map -> Evd.econstr -> Evd.econstr kind_pair

(** A [fun]'s binder, its type and its body. *)
type lambda_triple =
  (Names.Name.t, Evd.erelevance) Context.pbinder_annot
  * Evd.econstr
  * Evd.econstr

(** Raised by {!econstr_to_lambda}: not a [fun]. *)
exception
  Rocq_utils_EConstrIsNot_Lambda of (Evd.evar_map * Evd.econstr * econstr_kind)

(** [econstr_to_lambda sigma x] is the binder, type and body of the [fun]
    [x].

    @raise Rocq_utils_EConstrIsNot_Lambda if [x] is not a [fun] (raised here).
*)
val econstr_to_lambda : Evd.evar_map -> Evd.econstr -> lambda_triple

(** A named hypothesis of a goal. *)
type hyp =
  (Evd.econstr, Evd.econstr, Evd.erelevance) Context.Named.Declaration.pt

(** Raised by {!hyp_to_atomic}: the hypothesis' type is not atomic. *)
exception
  Rocq_utils_HypIsNot_Atomic of (Evd.evar_map * hyp * EConstr.kind_of_type)

(** [hyp_to_atomic sigma h] is the head and arguments of [h]'s type, an
    atomic type.

    @raise Rocq_utils_HypIsNot_Atomic
      if that type is not atomic (raised
      here, for {!econstr_to_atomic}'s [Rocq_utils_EConstrIsNot_Atomic]).
    @raise Rocq_utils_EConstrIsNotA_Type if it is not a type (propagated). *)
val hyp_to_atomic : Evd.evar_map -> hyp -> Evd.econstr kind_pair

(** A constructor: its context (binders) and its conclusion. *)
type ind_constr = Constr.rel_context * Constr.t

(** A local declaration, [Constr] side. *)
type constr_decl = Constr.rel_declaration

(** A local declaration, [EConstr] side. *)
type econstr_decl = EConstr.rel_declaration

(** [get_econstr_decls ctx] is [ctx]'s declarations as [EConstr] ones. Raises nothing.
*)
val get_econstr_decls : Constr.rel_context -> econstr_decl list

(** [list_of_constr_kinds x] is each kind name ("App", "Prod", ...) with
    whether [x] is of that kind; for debugging. Raises nothing. *)
val list_of_constr_kinds : Constr.t -> (string * bool) list

(** [list_of_econstr_kinds sigma x] is as {!list_of_constr_kinds}, for an
    [EConstr]. Raises nothing. *)
val list_of_econstr_kinds : Evd.evar_map -> Evd.econstr -> (string * bool) list

(** [list_of_econstr_kinds_of_type sigma x] is each kind of type
    ("SortType", "ProdType", ...) with whether [x] is one; [false] for all if
    [x] is not a type. Raises nothing. *)
val list_of_econstr_kinds_of_type
  :  Evd.evar_map
  -> Evd.econstr
  -> (string * bool) list

(** [list_of_kinds sigma f x] is the names of the kinds [f sigma x] says [x]
    has. Raises nothing. *)
val list_of_kinds
  :  Evd.evar_map
  -> (Evd.evar_map -> 'a -> (string * bool) list)
  -> 'a
  -> string list

(** [get_decl_type_of_constr d] is [d]'s type, as an [EConstr]. Raises nothing.
*)
val get_decl_type_of_constr : constr_decl -> Evd.econstr

(** [get_decl_type_of_econstr d] is [d]'s type. Raises nothing. *)
val get_decl_type_of_econstr : econstr_decl -> Evd.econstr

(** [get_ind_ty ind mib] is the inductive [ind] as a term, at the universe
    instance of its body [mib]. Raises nothing. *)
val get_ind_ty
  :  Names.inductive
  -> Declarations.mutual_inductive_body
  -> Evd.econstr

(** [type_of_econstr_rel ?substl d] is [d]'s type, with [substl]
    substituted if given. Raises nothing. *)
val type_of_econstr_rel
  :  ?substl:Evd.econstr list
  -> econstr_decl
  -> Evd.econstr

(** [type_of_econstr env sigma x] is [x]'s type ([Typing.type_of]) and the
    evar map after typing it.

    Raises Rocq's typing errors if [x] is ill-typed (propagated). *)
val type_of_econstr
  :  Environ.env
  -> Evd.evar_map
  -> Evd.econstr
  -> Evd.evar_map * Evd.econstr

(** Rocq values as strings, for logs and messages. Each raises nothing.
    Several are still placeholders that return "TODO: ..." instead of the
    value: [ind_constr], [ind_constrs], [constr_kind], [econstr_type],
    [econstr_types], [econstr_kind], [concl] (via [econstr_types]), [hyp]
    and [goal] ([TODO.md]). *)
module Strfy : sig
  (** [pp ?clean x] is [x] as a string, whitespace cleaned up unless [~clean:false].
  *)
  val pp : ?clean:bool -> Pp.t -> string

  (** [name_id x] is the identifier [x]. *)
  val name_id : Names.variable -> string

  (** [name n] is [n], or "Anonymous". *)
  val name : Names.Name.t -> string

  (** [global r] is the global reference [r]. *)
  val global : Names.GlobRef.t -> string

  (** [evar e] is the evar [e]'s number. *)
  val evar : Evar.t -> string

  (** [evar' env sigma e] is the evar [e] as Rocq names it. *)
  val evar' : Environ.env -> Evd.evar_map -> Evar.t -> string

  (** [constr env sigma x] is [x] printed. *)
  val constr : Environ.env -> Evd.evar_map -> Constr.t -> string

  (** [constr_opt env sigma x] is [x] printed, if any. *)
  val constr_opt : Environ.env -> Evd.evar_map -> Constr.t option -> string

  (** [constr_rel_decl env sigma d] is the declaration [d] printed. *)
  val constr_rel_decl : Environ.env -> Evd.evar_map -> constr_decl -> string

  (** [constr_rel_context env sigma c] is the context [c] printed. *)
  val constr_rel_context
    :  Environ.env
    -> Evd.evar_map
    -> Constr.rel_context
    -> string

  (** Placeholder: "TODO: ind_constr". *)
  val ind_constr : 'a -> 'b -> ind_constr -> string

  (** Placeholder: "TODO: ind_constrs". *)
  val ind_constrs : 'a -> 'b -> ind_constr array -> string

  (** Placeholder: "TODO: constr_kind". *)
  val constr_kind : 'a -> 'b -> Constr.t -> string

  (** [econstr env sigma x] is [x] printed. *)
  val econstr : Environ.env -> Evd.evar_map -> Evd.econstr -> string

  (** [econstr_rel_decl env sigma d] is the declaration [d] printed. *)
  val econstr_rel_decl : Environ.env -> Evd.evar_map -> econstr_decl -> string

  (** Placeholder: "TODO: econstr_type". *)
  val econstr_type
    :  'a
    -> 'b
    -> string * Evd.econstr * Evd.econstr * Evd.econstr array
    -> string

  (** Placeholder: "TODO: econstr_types". *)
  val econstr_types : 'a -> 'b -> Evd.econstr -> string

  (** Placeholder: "TODO: econstr_kind". *)
  val econstr_kind : 'a -> 'b -> Evd.econstr -> string

  (** Placeholder, as {!econstr_types}: "TODO: econstr_types". *)
  val concl : 'a -> 'b -> Evd.econstr -> string

  (** [erel env sigma r] is "relevant" or "irrelevant". *)
  val erel : 'a -> Evd.evar_map -> Evd.erelevance -> string

  (** [hyp_name h] is the hypothesis' name. *)
  val hyp_name : hyp -> string

  (** [hyp_value env sigma h] is the hypothesis' body printed, if it has one. *)
  val hyp_value : Environ.env -> Evd.evar_map -> hyp -> string

  (** [hyp_type env sigma h] is the hypothesis' type printed. *)
  val hyp_type : Environ.env -> Evd.evar_map -> hyp -> string

  (** Placeholder: "TODO: hyp". *)
  val hyp : 'a -> 'b -> hyp -> string

  (** Placeholder: "TODO: goal". *)
  val goal : Proofview.Goal.t -> string
end

(** [the_next ()] is a fresh evar name, [UnifEvar0], [UnifEvar1], ...;
    never the same one twice in a session. Raises nothing. *)
val the_next : unit -> Names.variable

(** Raised by {!get_next_evar} if Rocq gives no name. *)
exception CouldNotGetNextFreshEvarName of unit

(** [get_next_evar env sigma ty] is a new evar of type [ty], named by
    {!the_next}, and the evar map with it.

    @raise CouldNotGetNextFreshEvarName if Rocq gives no name (raised
                                        here). *)
val get_next_evar
  :  Environ.env
  -> Evd.evar_map
  -> Evd.econstr
  -> Evd.evar_map * Evd.econstr

(** What a new evar's type is taken from: another term's type, or a given type.
*)
type evar_source =
  | TypeOf of Evd.econstr
  | OfType of Evd.econstr

(** [get_next env sigma src] is a new evar ({!get_next_evar}) of the type
    [src] gives, and the evar map with it.

    @raise CouldNotGetNextFreshEvarName
      as {!get_next_evar} (propagated);
      and, for [TypeOf t], Rocq's typing errors if [t] is ill-typed. *)
val get_next
  :  Environ.env
  -> Evd.evar_map
  -> evar_source
  -> Evd.evar_map * Evd.econstr

(** [get_fresh_evar env sigma src] is {!get_next}, and raises as it does. *)
val get_fresh_evar
  :  Environ.env
  -> Evd.evar_map
  -> evar_source
  -> Evd.evar_map * Evd.econstr

(** [subst_of_decl substl d] is [d]'s type with [substl] substituted. Raises nothing.
*)
val subst_of_decl
  :  EConstr.Vars.substl
  -> ('a, Evd.econstr, 'b) Context.Rel.Declaration.pt
  -> Evd.econstr

(** [mk_ctx_subst env sigma substl d] is a new evar for the binder [d], of
    [d]'s type with [substl] (the evars for the binders before it)
    substituted. Raises nothing. *)
val mk_ctx_subst
  :  Environ.env
  -> Evd.evar_map
  -> EConstr.Vars.substl
  -> ('a, Evd.econstr, 'b) Context.Rel.Declaration.pt
  -> Evd.evar_map * Evd.econstr

(** [mk_ctx_substl env sigma acc ds] is a new evar for each binder of [ds]
    in turn ({!mk_ctx_subst}), each seeing the ones before it, prepended to
    [acc] (so most recent first): the substitution that instantiates a
    constructor's context. Raises nothing. *)
val mk_ctx_substl
  :  Environ.env
  -> Evd.evar_map
  -> EConstr.Vars.substl
  -> ('a, Evd.econstr, 'b) Context.Rel.Declaration.pt list
  -> Evd.evar_map * EConstr.Vars.substl

(** [map_decl_evar_pairs ds evars] is each evar paired with its binder's
    name, pairing [evars] with [ds] in order.

    @raise Invalid_argument
      if the lists' lengths differ (propagated from
      [List.combine]). *)
val map_decl_evar_pairs
  :  econstr_decl list
  -> EConstr.Vars.substl
  -> (Evd.econstr * Names.Name.t) list

(** Raised by {!constructor_args}: not three arguments. *)
exception ConstructorArgsExpectsArraySize3 of unit

(** An LTS step's source, label and target. *)
type constructor_args =
  { lhs : Evd.econstr
  ; act : Evd.econstr
  ; rhs : Evd.econstr
  }

(** [constructor_args args] is the step [lts lhs act rhs] whose arguments
    are [args].

    @raise ConstructorArgsExpectsArraySize3
      if [args] has not exactly three
      elements (raised here). *)
val constructor_args : Evd.econstr array -> constructor_args

(** Raised by {!extract_args}: an LTS application without three arguments. *)
exception Rocq_utils_InvalidLtsArgLength of int

(** Raised by {!extract_args}: not an application. *)
exception Rocq_utils_InvalidLtsTermKind of Constr.t

(** [extract_args ?substl term] is the source, label and target of [term],
    the application of an LTS relation (e.g. [termLTS p (Some A) q]), with
    [substl] substituted in each.

    @raise Rocq_utils_InvalidLtsArgLength
      if it has not three arguments
      (raised here).
    @raise Rocq_utils_InvalidLtsTermKind
      if [term] is not an application
      (raised here). *)
val extract_args : ?substl:EConstr.Vars.substl -> Constr.t -> constructor_args

(** Meant for {!unpack_constr_args}, whose handler can never fire; never
    raised ([TODO.md]). *)
exception Rocq_utils_CouldNotExtractBinding of unit

(** [unpack_constr_args (_, args)] is the first three of [args].

    @raise Invalid_argument
      if there are fewer than three (propagated from
      the array access; the handler meant to turn that into
      {!Rocq_utils_CouldNotExtractBinding} catches [Not_found] instead, so
      it never fires -- [TODO.md]). *)
val unpack_constr_args : Constr.t kind_pair -> Constr.t * Constr.t * Constr.t

(** [econstr_to_constrexpr env sigma x] is [x] externalised, as printing would show it. Raises nothing.
*)
val econstr_to_constrexpr
  :  Environ.env
  -> Evd.evar_map
  -> Evd.econstr
  -> Constrexpr.constr_expr

(** [constrexpr_to_econstr env sigma e] is [e] interpreted, possibly
    leaving evars, and the evar map after.

    Raises Rocq's interpretation errors if [e] is ill-formed
    (propagated). *)
val constrexpr_to_econstr
  :  Environ.env
  -> Evd.evar_map
  -> Constrexpr.constr_expr
  -> Evd.evar_map * Evd.econstr

(** [econstr_to_constr ?abort_on_undefined_evars sigma x] is [x] as a
    [Constr]; undefined evars are kept unless [abort_on_undefined_evars],
    in which case Rocq raises (propagated). *)
val econstr_to_constr
  :  ?abort_on_undefined_evars:bool
  -> Evd.evar_map
  -> Evd.econstr
  -> Constr.t

(** [econstr_to_constr_opt sigma x] is [x] as a [Constr], or [None] if it has undefined evars. Raises nothing.
*)
val econstr_to_constr_opt : Evd.evar_map -> Evd.econstr -> Constr.t option

(** [globref_to_econstr env r] is the term of the global reference [r]. Raises nothing for a monomorphic [r].
*)
val globref_to_econstr : Environ.env -> Names.GlobRef.t -> Evd.econstr

(** [is_constant sigma x c] is whether [x] is an application headed by
    [c ()]. Raises nothing. *)
val is_constant : Evd.evar_map -> Evd.econstr -> (unit -> Evd.econstr) -> bool

(** [libnames_to_globrefs qs] is the global reference each name in [qs]
    refers to.

    @raise Not_found if one refers to nothing (propagated from
                     [Nametab.global]). *)
val libnames_to_globrefs : Libnames.qualid list -> Names.GlobRef.t list

(** [extract_benchmark_args env sigma e] is [e] interpreted and
    externalised again, as a one-element list (the name suggests a list of
    terms, which it does not split), and the evar map after.

    Raises Rocq's interpretation errors (propagated). *)
val extract_benchmark_args
  :  Environ.env
  -> Evd.evar_map
  -> Constrexpr.constr_expr
  -> Evd.evar_map * Constrexpr.constr_expr list
