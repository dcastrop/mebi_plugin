(** The extraction machinery over {!Rocq_monad}: term utilities, the
    plugin's errors, inductive LTS definitions ([Ind]), and the unification
    that finds a term's transitions ([Unification]). Remember that monadic
    values raise when run, not when built ({!Rocq_monad}). *)
module type S = sig
  type tree

  include Rocq_monad.S

  (** [fresh_evar src] is a new evar of the type [src] gives
      ({!Rocq_utils.get_next}).

      @raise Rocq_utils.CouldNotGetNextFreshEvarName
        when run, if Rocq
        gives no name (propagated); and, for [TypeOf t], Rocq's typing
        errors. *)
  val fresh_evar : Rocq_utils.evar_source -> EConstr.t mm

  (** [econstr_eq ?enc a b] is whether [a] and [b] are equal: by their
      encodings (normalised; the default), or syntactically under the
      current evar map with [~enc:false]. Raises nothing. *)
  val econstr_eq : ?enc:bool -> EConstr.t -> EConstr.t -> bool mm

  (** [econstr_compare a b] is [a] against [b] by their encodings. Raises
      nothing. *)
  val econstr_compare : EConstr.t -> EConstr.t -> int

  (** [get_encoding x] is the encoding of [x] normalised, run at once.

      @raise Bi_encoding.S.EncodingNotFound if it has none (propagated). *)
  val get_encoding : EConstr.t -> enc

  (** [econstr_kind x] is the kind of [x] normalised. Raises nothing. *)
  val econstr_kind : EConstr.t -> Rocq_utils.econstr_kind mm

  (** [econstr_is_evar x] is whether [x] is an evar. Raises nothing. *)
  val econstr_is_evar : EConstr.t -> bool mm

  (** [econstr_to_constr ?abort_on_undefined_evars x] is [x] as a [Constr];
      undefined evars are kept unless [abort_on_undefined_evars], in which
      case Rocq raises, when run (propagated). *)
  val econstr_to_constr
    :  ?abort_on_undefined_evars:bool
    -> EConstr.t
    -> Constr.t mm

  (** [econstr_to_constr_opt x] is [x] as a [Constr], or [None] if it has
      undefined evars. Raises nothing. *)
  val econstr_to_constr_opt : EConstr.t -> Constr.t option mm

  (** [constrexpr_to_econstr e] is [e] interpreted, possibly leaving
      evars.

      Raises Rocq's interpretation errors when run (propagated). *)
  val constrexpr_to_econstr : Constrexpr.constr_expr -> EConstr.t mm

  (** [to_atomic x] is the head and arguments of the atomic type [x].

      @raise Rocq_utils.Rocq_utils_EConstrIsNot_Atomic
        when run, if [x] is a
        type but not an atomic one (propagated).
      @raise Rocq_utils.Rocq_utils_EConstrIsNotA_Type
        when run, if [x] is
        not a type (propagated). *)
  val to_atomic : EConstr.t -> EConstr.t Rocq_utils.kind_pair mm

  (** [to_lambda x] is the binder, type and body of the [fun] [x].

      @raise Rocq_utils.Rocq_utils_EConstrIsNot_Lambda
        when run, if [x] is not a [fun] (propagated). *)
  val to_lambda : EConstr.t -> Rocq_utils.lambda_triple mm

  (** [to_app x] is the head and arguments of the application [x].

      @raise Rocq_utils.Rocq_utils_EConstrIsNot_App
        when run, if [x] is not
        an application (propagated). *)
  val to_app : EConstr.t -> EConstr.t Rocq_utils.kind_pair mm

  (** [exists_eq x ys decode] is whether [x] equals (by encoding) the term
      [decode y] for some [y] of [ys]. Raises nothing. *)
  val exists_eq : EConstr.t -> 'a list -> ('a -> EConstr.t) -> bool mm

  (** [type_of_econstr x] is the type of [x] normalised.

      Raises Rocq's typing errors when run, if [x] is ill-typed
      (propagated). *)
  val type_of_econstr : EConstr.t -> EConstr.t mm

  (** [type_of_constrexpr e] is the type of [e] interpreted.

      Raises Rocq's interpretation and typing errors when run
      (propagated). *)
  val type_of_constrexpr : Constrexpr.constr_expr -> EConstr.t mm

  (** {!Rocq_monad_strfy} for this monad: printers in the current context. *)
  module Strfy : sig
    val constr : Constr.t -> string
    val constr_kind : Constr.t -> string
    val econstr : EConstr.t -> string
    val econstr_kind : EConstr.t -> string
    val econstr_rel_decl : EConstr.rel_declaration -> string
    val hyp_name : Rocq_utils.hyp -> string
    val hyp_type : Rocq_utils.hyp -> string
    val hyp : Rocq_utils.hyp -> string
    val hyp_value : Rocq_utils.hyp -> string
    val econstr_bindings : EConstr.t Tactypes.bindings -> string
  end

  (** [log_econstr ?__FUNCTION__ ?m ?s x] logs [x], labelled [s], at kind
      [m] (default [Debug]). Raises nothing. *)
  val log_econstr
    :  ?__FUNCTION__:string
    -> ?m:Output.Kind.t
    -> ?s:string
    -> EConstr.t
    -> unit

  (** [log_econstrs ...] is {!log_econstr} for a list. Raises nothing. *)
  val log_econstrs
    :  ?__FUNCTION__:string
    -> ?m:Output.Kind.t
    -> ?s:string
    -> EConstr.t list
    -> unit

  (** [log_constr ...] is {!log_econstr} for a [Constr]. Raises nothing. *)
  val log_constr
    :  ?__FUNCTION__:string
    -> ?m:Output.Kind.t
    -> ?s:string
    -> Constr.t
    -> unit

  (** [log_constrs ...] is {!log_constr} for a list. Raises nothing. *)
  val log_constrs
    :  ?__FUNCTION__:string
    -> ?m:Output.Kind.t
    -> ?s:string
    -> Constr.t list
    -> unit

  (** The plugin's own errors, as one exception [MEBI_exn] carrying a
      description, each with a printer registered with Rocq (so a user sees
      a message, not an anomaly). The functions build the exception; {!Err}
      raises it. *)
  module type SErrors = sig
    type t =
      | LTS_Empty
      | LTS_Incomplete of string
      | Saturation_Too_Large of string
      | Not_Bisimilar
      | Invalid_Ind_Kind_Type of EConstr.t option
      | Invalid_Sort_LTS of Sorts.Quality.t
      | Invalid_Sort_Type of Sorts.Quality.t
      | Invalid_Ref_LTS of Names.GlobRef.t
      | Invalid_Ref_Type of Names.GlobRef.t
      | Invalid_Arity of (Environ.env * Evd.evar_map * Constr.t)
      | InvalidCheckUpdatedCtx of
          (Environ.env
          * Evd.evar_map
          * EConstr.t list
          * EConstr.rel_declaration list)
      | InvalidLTSArgsLength of int
      | InvalidLTSTermKind of Environ.env * Evd.evar_map * Constr.t

    exception MEBI_exn of t

    val lts_empty : unit -> exn
    val lts_incomplete : string -> exn
    val saturation_too_large : string -> exn
    val not_bisimilar : unit -> exn
    val invalid_ind_kind_type : EConstr.t option -> exn
    val invalid_sort_lts : Sorts.Quality.t -> exn
    val invalid_sort_type : Sorts.Quality.t -> exn
    val invalid_ref_lts : Names.GlobRef.t -> exn
    val invalid_ref_type : Names.GlobRef.t -> exn
    val invalid_arity : Environ.env -> Evd.evar_map -> Constr.t -> exn

    val invalid_check_updated_ctx
      :  Environ.env
      -> Evd.evar_map
      -> EConstr.t list
      -> EConstr.rel_declaration list
      -> exn

    val invalid_lts_args_length : int -> exn
    val invalid_lts_term_kind : Environ.env -> Evd.evar_map -> Constr.t -> exn
  end

  (** See {!SErrors}. *)
  module Errors : SErrors

  (** Each function raises the corresponding {!SErrors} exception
      ([MEBI_exn]): directly when applied, or, for those returning ['a mm]
      (which need the context to describe the error), when run. *)
  module type SErr = sig
    val lts_empty : unit -> 'a
    val lts_incomplete : string -> 'a
    val saturation_too_large : string -> 'a
    val not_bisimilar : unit -> 'a
    val invalid_ind_kind_type : EConstr.t option -> 'a
    val invalid_sort_lts : Sorts.Quality.t -> 'a
    val invalid_sort_type : Sorts.Quality.t -> 'a
    val invalid_ref_lts : Names.GlobRef.t -> 'a
    val invalid_ref_type : Names.GlobRef.t -> 'a
    val invalid_arity : Constr.t -> 'a mm

    val invalid_check_updated_ctx
      :  EConstr.t list
      -> EConstr.rel_declaration list
      -> 'a mm

    val invalid_lts_args_length : int -> 'a
    val invalid_lts_term_kind : Constr.t -> 'a mm
  end

  (** See {!SErr}. *)
  module Err : SErr

  (** An inductive relation given to a command: its encoding, its term, and
      whether it is an LTS (with its term and label types and its
      constructors) or a type. *)
  module Ind : sig
    module LTS : sig
      type t =
        { term_type : EConstr.t
        ; label_type : EConstr.t
        ; constructor_types : constructor array
        }

      and constructor =
        { name : Names.Id.t
        ; constructor : Rocq_utils.ind_constr
        }

      include Json.S with type k = t (** @closed *)
    end

    type t =
      { enc : enc
      ; ind : EConstr.t
      ; kind : kind
      }

    and kind =
      | Type of EConstr.t option
      | LTS of LTS.t

    include Json.S with type k = t (** @closed *)

    (** [get_lts i] is [i]'s LTS description.

        @raise Errors.MEBI_exn
          [Invalid_Ind_Kind_Type] if [i] is a type, not
          an LTS (raised here). *)
    val get_lts : t -> LTS.t

    (** [get_lts_term_type i] is the LTS's state type. Raises as
        {!get_lts}. *)
    val get_lts_term_type : t -> EConstr.t

    (** [get_lts_label_type i] is the LTS's label type. Raises as
        {!get_lts}. *)
    val get_lts_label_type : t -> EConstr.t

    (** [get_lts_constructor_types i] is the LTS's constructors. Raises as
        {!get_lts}. *)
    val get_lts_constructor_types : t -> LTS.constructor array

    (** [lookup ind] is the definition of the inductive [ind] in the current
        environment.

        @raise Not_found
          when run, if [ind] is not defined (propagated from
          [Inductive.lookup_mind_specif]). *)
    val lookup : Names.inductive -> Declarations.mind_specif mm

    (** [get_lts_constructor_names i] is the names of the LTS's
        constructors. Raises as {!get_lts}. *)
    val get_lts_constructor_names : t -> Names.Id.t array

    (** [get_lts_constructors i] is the LTS's constructors (context and
        conclusion each). Raises as {!get_lts}. *)
    val get_lts_constructors : t -> Rocq_utils.ind_constr array

    (** [assert_mip_arity_is_type_or_set mip] checks the inductive [mip]
        lives in [Type] or [Set].

        @raise Errors.MEBI_exn
          [Invalid_Sort_Type] if not (raised here, when
          applied). *)
    val assert_mip_arity_is_type_or_set
      :  Declarations.one_inductive_body
      -> unit mm

    (** [assert_mip_arity_is_prop mip] checks the inductive [mip] lives in
        [Prop], as an LTS relation must.

        @raise Errors.MEBI_exn
          [Invalid_Sort_LTS] if not (raised here, when
          applied). *)
    val assert_mip_arity_is_prop : Declarations.one_inductive_body -> unit mm

    (** [lts_mind r] is the inductive [r] refers to, with its definition.

        @raise Errors.MEBI_exn
          [Invalid_Ref_LTS] if [r] is not an inductive
          (raised here, when applied).
        @raise Not_found when run, as {!lookup} (propagated). *)
    val lts_mind
      :  Names.GlobRef.t
      -> (Names.inductive * Declarations.mind_specif) mm

    (** [lts_type_mind r] is {!lts_mind}, checked to live in [Type] or [Set].
        Raises as {!lts_mind}, and, when run,
        {!assert_mip_arity_is_type_or_set}'s error. *)
    val lts_type_mind
      :  Names.GlobRef.t
      -> (Names.inductive * Declarations.mind_specif) mm

    (** [lts_prop_mind r] is {!lts_mind}, checked to live in [Prop]. Raises as
        {!lts_mind}, and, when run, {!assert_mip_arity_is_prop}'s error. *)
    val lts_prop_mind
      :  Names.GlobRef.t
      -> (Names.inductive * Declarations.mind_specif) mm

    (** [lts_labels_and_terms (mib, mip)] is the declarations of the label
        and the state of the LTS relation [mip], whose indices must be
        [state -> label -> state] with both states of the same type.

        @raise Errors.MEBI_exn
          [Invalid_Arity] when run, if its indices do
          not have that shape (raised here). *)
    val lts_labels_and_terms
      :  Declarations.mind_specif
      -> (Constr.rel_declaration * Constr.rel_declaration) mm

    (** [lts r] is the LTS relation [r] refers to, described: its
        encoding, term, state and label types, and constructors.

        Raises, when run, the errors of {!lts_prop_mind} and
        {!lts_labels_and_terms} (propagated). *)
    val lts : Names.GlobRef.t -> t mm
  end

  (** [mk_ctx_substl acc ds] is {!Rocq_utils.mk_ctx_substl} in the current
      context: an evar per binder of [ds], prepended to [acc]. Raises
      nothing. *)
  val mk_ctx_substl
    :  EConstr.Vars.substl
    -> ('a, EConstr.t, 'b) Context.Rel.Declaration.pt list
    -> EConstr.Vars.substl mm

  (** [extract_args ?substl term] is the source, label and target of the
      LTS step [term], with [substl] substituted ({!Rocq_utils.extract_args}).

      @raise Errors.MEBI_exn
        [InvalidLTSArgsLength] if [term] has not three
        arguments (raised here, when applied).
      @raise Errors.MEBI_exn
        [InvalidLTSTermKind] when run, if [term] is not
        an application (raised here). *)
  val extract_args
    :  ?substl:EConstr.Vars.substl
    -> Constr.t
    -> Rocq_utils.constructor_args mm

  (** A transition found for a term: its label and target (encoded) and the
      tree of constructors that derives it. *)
  module Constructor : sig
    type t = enc * enc * tree

    include Json.S with type k = t (** @closed *)

    (** [encode act goto tree] is the transition with label [act] and
        target [goto] (each encoded) derived by [tree]. Raises nothing. *)
    val encode : EConstr.t -> EConstr.t -> tree -> t
  end

  (** [make_state_tree_pair_set ()] is a set module over (encoding,
      derivation tree) pairs, ordered by encoding then tree. Raises
      nothing. *)
  val make_state_tree_pair_set
    :  unit
    -> (module Set.S with type elt = enc * tree)

  (** Finding a term's transitions: matching each constructor of its LTS
      against the term, then the constructor's premises -- other LTS steps,
      found recursively and unified with what the premise requires, and
      other propositions, decided by {!Premise_search} now or once the LTS
      premises have fixed what they mention. *)
  module Unification : sig
    module Pair : sig
      type t =
        { to_check : EConstr.t
        ; acc : EConstr.t
        }

      include Json.S with type k = t (** @closed *)

      (** [fresh env sigma p] is [p] with [to_check] replaced by a new evar of
          its type, and the evar map with it.

          Raises Rocq's typing errors (propagated).

          @raise Rocq_utils.CouldNotGetNextFreshEvarName (propagated). *)
      val fresh : Environ.env -> Evd.evar_map -> t -> Evd.evar_map * t

      (** [make env sigma to_check acc] is the pair, [to_check] made a fresh
          evar ({!fresh}) if it is an evar. Raises as {!fresh}. *)
      val make
        :  Environ.env
        -> Evd.evar_map
        -> EConstr.t
        -> EConstr.t
        -> Evd.evar_map * t

      (** [unify env sigma p] is the evar map with [p.to_check] unified with
          [p.acc] (up to cumulativity), and whether that succeeded.

          [false] when Rocq finds no unifier ([CannotUnify], an occur-check,
          binder types that differ).

          Raises Rocq's other unification errors, where it gives up rather
          than finds none (propagated). *)
      val unify : Environ.env -> Evd.evar_map -> t -> Evd.evar_map * bool

      (** [unifies to_check acc] is {!unify} in the current context, keeping
          the evar map if it succeeds. Raises as {!unify}, when run. *)
      val unifies : EConstr.t -> EConstr.t -> bool mm
    end

    module Problem : sig
      type t =
        { act : Pair.t
        ; goto : Pair.t
        ; tree : tree
        }

      include Json.S with type k = t (** @closed *)

      (** [unify_pair_opt p] is {!Pair.unify} in the current context. Raises
          as it does, when run. *)
      val unify_pair_opt : Pair.t -> bool mm

      (** [unify_opt p] is [p]'s tree if both its label and target pairs
          unify, else [None]. Raises as {!Pair.unify}, when run. *)
      val unify_opt : t -> tree option mm

      (** [of_constructor args c] is the problem of fitting the transition
          [c] (an LTS premise's solution) to what the premise requires
          ([args]): its label and target paired with [args]'s.

          @raise Bi_encoding.S.CannotDecode
            if [c]'s encodings decode to no
            term (propagated). *)
      val of_constructor : Rocq_utils.constructor_args -> Constructor.t -> t
    end

    module Problems : sig
      type deferred = enc * EConstr.t * EConstr.t array

      type t =
        { sigma : Evd.evar_map
        ; to_unify : Problem.t list
        ; deferred : deferred list
        }

      include Json.S with type k = t (** @closed *)

      (** [empty ()] is no problems, at the current evar map. Raises
          nothing. *)
      val empty : unit -> t mm

      (** [is_empty p] is whether [p] has nothing to unify. Raises nothing. *)
      val is_empty : t -> bool

      (** [unify_list_opt ps] is the trees of [ps], if all of them unify (in
          order), else [None]. Raises as {!Pair.unify}, when run. *)
      val unify_list_opt : Problem.t list -> tree list option mm

      (** [sandbox_unify_all lts_enc act goto ps] is every transition a
          constructor gives, in a sandbox at [ps]'s evar map: if [ps]'s
          problems all unify, one (label, target, trees) per way the deferred
          premises hold ([resolve_deferred]); none otherwise. Undecided and
          possibly incomplete premises are warned about, and a transition
          with an unknown left in it is dropped, with a warning.

          Raises as {!Pair.unify} and {!Premise_search}, when run. *)
      val sandbox_unify_all
        :  enc
        -> EConstr.t
        -> EConstr.t
        -> t
        -> (EConstr.t * EConstr.t * tree list) list mm
    end

    module ListOfProblems : sig
      type t = Problems.t list

      include Json.S with type k = t (** @closed *)

      (** [is_empty ps] is whether [ps] is empty, or a single problem set
          with nothing to unify. Raises nothing. *)
      val is_empty : t -> bool

      (** [cross_product ps acc] is every way to extend each problem set of
          [acc] with one of [ps]'s problems (keeping [acc]'s deferred
          premises), at [ps]'s evar map. Raises nothing. *)
      val cross_product : Problems.t -> t -> t
    end

    module Constructors : sig
      type t = Constructor.t list

      include Json.S with type k = t (** @closed *)

      (** [retrieve i acc act tgt (lts_enc, problem_sets)] is [acc] plus the
          transitions constructor [i] gives for each problem set
          ({!Problems.sandbox_unify_all}), each with the derivation tree
          rooted at constructor [i]. Raises as
          {!Problems.sandbox_unify_all}, when run. *)
      val retrieve
        :  int
        -> t
        -> EConstr.t
        -> EConstr.t
        -> enc * ListOfProblems.t
        -> t mm

      (** [to_problems args cs] is the problems of fitting each of [cs] to
          [args] ({!Problem.of_constructor}). Raises as
          {!Problem.of_constructor}. *)
      val to_problems : Rocq_utils.constructor_args -> t -> Problems.t mm

      (** [axiom act tgt (lts_enc, i) acc] is [acc] plus the transition
          [act]/[tgt] of constructor [i], which has no LTS premise; if [act]
          or [tgt] still has an unknown in it (a binder nothing fixes), the
          transition is left out, with a warning. Raises nothing. *)
      val axiom : EConstr.t -> EConstr.t -> enc * int -> t -> t mm
    end

    (** [check_constructor_args_unify lhs act args] is whether the
        constructor's source unifies with [lhs] and then its label with
        [act]. Raises as {!Pair.unify}, when run. *)
    val check_constructor_args_unify
      :  EConstr.t
      -> EConstr.t
      -> Rocq_utils.constructor_args
      -> bool mm

    (** [check_valid_constructors cs indmap from act lts_enc] is every
        transition from [from] that a constructor of [cs] gives: each
        constructor whose source and label unify with [from] and a fresh
        [act] is explored ({!explore_valid_constructor}). [indmap] is the
        LTSs given in [Using], by term.

        Raises, when run, the errors of {!extract_args} (a malformed
        constructor), unification and premise search (propagated). *)
    val check_valid_constructors
      :  Ind.LTS.constructor array
      -> Ind.t F.t
      -> EConstr.t
      -> EConstr.t
      -> enc
      -> Constructors.t mm

    (** [explore_valid_constructor indmap from lts_enc args (i, acc) (substl, decls)] is [acc] plus the transitions constructor [i] gives
        from [from]: its premises checked ({!check_updated_ctx}), then
        combined ({!check_for_next_constructors}). Raises as
        {!check_valid_constructors}. *)
    val explore_valid_constructor
      :  Ind.t F.t
      -> EConstr.t
      -> enc
      -> Rocq_utils.constructor_args
      -> int * Constructors.t
      -> EConstr.Vars.substl * EConstr.rel_declaration list
      -> Constructors.t mm

    (** [check_updated_ctx lts_enc acc indmap (substl, decls)] is the
        problems a constructor's premises set, or [None] if one is false:
        walking its binders (last to first), an LTS premise is explored
        recursively and its solutions added as problems; any other premise
        is decided now or deferred until the LTS premises are unified.

        Raises as {!check_valid_constructors}; also
        [Errors.MEBI_exn] [InvalidCheckUpdatedCtx] when run, if [substl] and
        [decls] differ in length (cannot happen). *)
    val check_updated_ctx
      :  enc
      -> ListOfProblems.t
      -> Ind.t F.t
      -> EConstr.Vars.substl * EConstr.rel_declaration list
      -> (enc * ListOfProblems.t) option mm

    (** [check_for_next_constructors i act tgt acc found] is [acc] plus the
        transitions constructor [i] gives once its premises are checked
        ([found]): none if a premise was false; with no LTS premise, one
        per way its deferred premises hold ({!Constructors.axiom}); else
        those its LTS premises' solutions give ({!Constructors.retrieve}).
        Raises as {!check_valid_constructors}. *)
    val check_for_next_constructors
      :  int
      -> EConstr.t
      -> EConstr.t
      -> Constructors.t
      -> (enc * ListOfProblems.t) option
      -> Constructors.t mm

    (** [collect_valid_constructors cs indmap from label_type lts_enc] is
        every transition from [from] in the LTS whose constructors are [cs]
        ({!check_valid_constructors}, with a fresh label of [label_type]).
        Raises as {!check_valid_constructors}. *)
    val collect_valid_constructors
      :  Ind.LTS.constructor array
      -> Ind.t F.t
      -> EConstr.t
      -> EConstr.t
      -> enc
      -> Constructors.t mm
  end

  (** [make_enc_hashtbl ()] is a hash table module over encodings. Raises nothing.
  *)
  val make_enc_hashtbl : unit -> (module Hashtbl.S with type key = enc)

  (** [make_enc_set ()] is a set module over encodings. Raises nothing. *)
  val make_enc_set : unit -> (module Set.S with type elt = enc)

  (** [make_econstr_set ()] is a set module over terms, ordered by
      encoding. Raises nothing. *)
  val make_econstr_set : unit -> (module Set.S with type elt = EConstr.t)
end

(** What made the LTS being extracted approximate (undecided premises,
    incomplete premise searches, undetermined transitions), noted by the
    extraction's warnings every time they apply. [reset] before an
    extraction, [get] after it. *)
module Approximations : sig
  (** [reset ()] forgets every note. Raises nothing. *)
  val reset : unit -> unit

  (** [note s] records [s]. Raises nothing. *)
  val note : string -> unit

  (** [get ()] is the notes recorded since {!reset}, oldest first. Raises nothing.
  *)
  val get : unit -> string list
end

module Make (Enc : Encoding.S) :
  S with type enc = Enc.t and type tree = Enc.Tree.t
