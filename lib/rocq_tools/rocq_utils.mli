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
      here, for {!econstr_to_atomic}'s {!Rocq_utils_EConstrIsNot_Atomic}).
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

(** [get_decl_type_of_constr d] is [d]'s type, as an [EConstr]. Raises nothing.
*)
val get_decl_type_of_constr : constr_decl -> Evd.econstr

(** [get_ind_ty ind mib] is the inductive [ind] as a term, at the universe
    instance of its body [mib]. Raises nothing. *)
val get_ind_ty
  :  Names.inductive
  -> Declarations.mutual_inductive_body
  -> Evd.econstr

(** [type_of_econstr env sigma x] is [x]'s type ([Typing.type_of]) and the
    evar map after typing it.

    Raises Rocq's typing errors if [x] is ill-typed (propagated). *)
val type_of_econstr
  :  Environ.env
  -> Evd.evar_map
  -> Evd.econstr
  -> Evd.evar_map * Evd.econstr

(** Rocq values as strings, for logs and messages. Each raises nothing. *)
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

  (** [ind_constr env sigma (ctx, t)] is the constructor type [t] printed
      under its binders [ctx], as [ctx |- t]. Raises nothing. *)
  val ind_constr : Environ.env -> Evd.evar_map -> ind_constr -> string

  (** [constr_kind env sigma x] is the name of [x]'s kind ([App], [Rel],
      ...) and [x] printed. Raises nothing. *)
  val constr_kind : Environ.env -> Evd.evar_map -> Constr.t -> string

  (** [econstr env sigma x] is [x] printed. *)
  val econstr : Environ.env -> Evd.evar_map -> Evd.econstr -> string

  (** [econstr_rel_decl env sigma d] is the declaration [d] printed. *)
  val econstr_rel_decl : Environ.env -> Evd.evar_map -> econstr_decl -> string

  (** [econstr_kind env sigma x] is the name of [x]'s kind ([App], [Rel],
      ...) and [x] printed. Raises nothing. *)
  val econstr_kind : Environ.env -> Evd.evar_map -> Evd.econstr -> string

  (** [concl env sigma c] is the conclusion [c] printed, as
      {!econstr_types}. Raises nothing. *)
  val concl : Environ.env -> Evd.evar_map -> Evd.econstr -> string

  (** [hyp_name h] is the hypothesis' name. *)
  val hyp_name : hyp -> string

  (** [hyp_value env sigma h] is the hypothesis' body printed, if it has one. *)
  val hyp_value : Environ.env -> Evd.evar_map -> hyp -> string

  (** [hyp_type env sigma h] is the hypothesis' type printed. *)
  val hyp_type : Environ.env -> Evd.evar_map -> hyp -> string

  (** [hyp env sigma h] is the hypothesis printed as [name : type], or
      [name := body : type] if it has a body. Raises nothing. *)
  val hyp : Environ.env -> Evd.evar_map -> hyp -> string

  (** [goal g] is the goal [g] printed, hypotheses and conclusion, with
      Rocq's debug printer. Raises nothing. *)
  val goal : Proofview.Goal.t -> string
end

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

(** [unpack_constr_args (_, args)] is the first three of [args].

    @raise Rocq_utils_CouldNotExtractBinding
      if there are fewer than three (raised here). *)
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

(** [libnames_to_globrefs qs] is the global reference each name in [qs]
    refers to.

    @raise Not_found if one refers to nothing (propagated from
                     [Nametab.global]). *)
val libnames_to_globrefs : Libnames.qualid list -> Names.GlobRef.t list

(** [lts_shaped env ind] is whether the inductive [ind] has the type of an
    LTS, [M -> option A -> M -> Prop] ([theories/Bisimilarity.v]'s [LTS]),
    without parameters. Raises nothing. *)
val lts_shaped : Environ.env -> Names.inductive -> bool

(** [lts_closure env roots] is what a [Using] list names for the LTSs
    [roots]: [roots] and every LTS-shaped inductive ({!lts_shaped}) their
    constructors' premises mention, transitively, first found first. The
    [bisim] and [sim] tactics, and the [MeBi Run] commands given no
    [Using], extract with it. Raises nothing. *)
val lts_closure : Environ.env -> Names.inductive list -> Names.inductive list

(** [qualid_of_ind ind] is a name for [ind] in the current scope.
    @raise Not_found if it has none (propagated). *)
val qualid_of_ind : Names.inductive -> Libnames.qualid
