(** The signatures of {!Rocq_monad_utils}, declared once for its [.ml] and [.mli].
*)

module type S = sig
  type tree

  include Rocq_monad.S

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

  (** [log_constr ...] is {!log_econstr} for a [Constr]. Raises nothing. *)
  val log_constr
    :  ?__FUNCTION__:string
    -> ?m:Output.Kind.t
    -> ?s:string
    -> Constr.t
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
      | LTS_Has_Parameters of string
      | Invalid_Arity of (Environ.env * Evd.evar_map * Constr.t)
      | InvalidCheckUpdatedCtx of
          (Environ.env
          * Evd.evar_map
          * EConstr.t list
          * EConstr.rel_declaration list)
      | InvalidLTSArgsLength of int
      | InvalidLTSTermKind of Environ.env * Evd.evar_map * Constr.t

    val lts_empty : unit -> exn
    val lts_incomplete : string -> exn
    val saturation_too_large : string -> exn
    val not_bisimilar : unit -> exn
    val invalid_ind_kind_type : EConstr.t option -> exn
    val invalid_sort_lts : Sorts.Quality.t -> exn
    val invalid_sort_type : Sorts.Quality.t -> exn
    val invalid_ref_lts : Names.GlobRef.t -> exn
    val invalid_ref_type : Names.GlobRef.t -> exn
    val lts_has_parameters : string -> exn
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
    val lts_has_parameters : string -> 'a
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

    (** [lts_type_mind r] is {!lts_mind}, checked to live in [Type] or [Set].
        Raises as {!lts_mind}, and, when run,
        {!assert_mip_arity_is_type_or_set}'s error. *)
    val lts_type_mind
      :  Names.GlobRef.t
      -> (Names.inductive * Declarations.mind_specif) mm

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
    end

    module Problem : sig
      type t =
        { act : Pair.t
        ; goto : Pair.t
        ; tree : tree
        }

      include Json.S with type k = t (** @closed *)
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
    end

    module ListOfProblems : sig
      type t = Problems.t list

      include Json.S with type k = t (** @closed *)

      (** [is_empty ps] is whether [ps] is empty, or a single problem set
          with nothing to unify. Raises nothing. *)
      val is_empty : t -> bool
    end

    module Constructors : sig
      type t = Constructor.t list

      include Json.S with type k = t (** @closed *)
    end

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
end
