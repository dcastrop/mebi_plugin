module Make : (Log : Logger.S) (Ctx : Rocq_context.S) (Enc : Encoding.S) -> sig
  include module type of
      Rocq_monad.Make
        (Logger.ReMake
           (Log)
           (struct
             let level : Output.Kind.level -> bool = function
               | Debug -> false
               | Info -> true
               | Notice -> true
               | Warning -> true
               | Error -> true
             ;;

             let special : Output.Kind.special -> bool = function
               | Trace -> false
               | Result -> true
               | Show -> true
             ;;
           end))
           (Ctx)
        (Enc)

  val fresh_evar : Rocq_utils.evar_source -> EConstr.t mm
  val econstr_eq : ?enc:bool -> EConstr.t -> EConstr.t -> bool mm
  val econstr_compare : EConstr.t -> EConstr.t -> int
  val get_encoding : EConstr.t -> Enc.t
  val econstr_kind : EConstr.t -> Rocq_utils.econstr_kind mm
  val econstr_is_evar : EConstr.t -> bool mm

  val econstr_to_constr
    :  ?abort_on_undefined_evars:bool
    -> EConstr.t
    -> Constr.t mm

  val econstr_to_constr_opt : EConstr.t -> Constr.t option mm
  val constrexpr_to_econstr : Constrexpr.constr_expr -> EConstr.t mm
  val exists_eq : EConstr.t -> 'a list -> ('a -> EConstr.t) -> bool mm
  val type_of_econstr : EConstr.t -> EConstr.t mm
  val type_of_constrexpr : Constrexpr.constr_expr -> EConstr.t mm

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
    val rocq_ind : ('a -> string) -> 'a Rocq_ind.t -> string
    val econstr_bindings : EConstr.t Tactypes.bindings -> string
  end

  val log_econstr : ?__FUNCTION__:string -> ?s:string -> EConstr.t -> unit
  val log_econstrs : ?__FUNCTION__:string -> ?s:string -> EConstr.t list -> unit

  module type SErrors = sig
    type t =
      | LTS_Empty
      | LTS_Incomplete of string
      | Not_Bisimilar
      | Invalid_Ind_Kind of Rocq_ind.kind
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
    val not_bisimilar : unit -> exn
    val invalid_ind_kind : Rocq_ind.kind -> exn
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

  module Errors : SErrors

  module type SErr = sig
    val lts_empty : unit -> 'a
    val lts_incomplete : string -> 'a
    val not_bisimilar : unit -> 'a
    val invalid_ind_kind : Rocq_ind.kind -> 'a
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

  module Err : SErr

  module Ind : sig
    type t = Enc.t Rocq_ind.t

    val get_lts : t -> Rocq_ind.LTS.t
    val get_lts_term_type : t -> EConstr.t
    val get_lts_label_type : t -> EConstr.t
    val get_lts_constructor_types : t -> Rocq_ind.LTS.constructor array
    val to_string : Environ.env -> Evd.evar_map -> t -> string
    val lookup : Names.inductive -> Declarations.mind_specif mm

    val assert_mip_arity_is_type_or_set
      :  Declarations.one_inductive_body
      -> unit mm

    val assert_mip_arity_is_prop : Declarations.one_inductive_body -> unit mm

    val lts_mind
      :  Names.GlobRef.t
      -> (Names.inductive * Declarations.mind_specif) mm

    val lts_type_mind
      :  Names.GlobRef.t
      -> (Names.inductive * Declarations.mind_specif) mm

    val lts_prop_mind
      :  Names.GlobRef.t
      -> (Names.inductive * Declarations.mind_specif) mm

    val lts_labels_and_terms
      :  Declarations.mind_specif
      -> (Constr.rel_declaration * Constr.rel_declaration) mm

    val lts : Names.GlobRef.t -> t mm
  end

  val mk_ctx_substl
    :  EConstr.Vars.substl
    -> ('a, EConstr.t, 'b) Context.Rel.Declaration.pt list
    -> EConstr.Vars.substl mm

  val extract_args
    :  ?substl:EConstr.Vars.substl
    -> Constr.t
    -> Rocq_utils.constructor_args mm

  module Constructor : sig
    type t = Enc.t * Enc.t * Enc.Tree.t

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    val encode : EConstr.t -> EConstr.t -> Enc.Tree.t -> t
  end

  val make_state_tree_pair_set
    :  unit
    -> (module Set.S with type elt = Enc.t * Enc.Tree.t)

  module Unification : sig
    module Pair : sig
      type t =
        { to_check : EConstr.t
        ; acc : EConstr.t
        }

      type k = t

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?s:string -> k -> unit
      val fresh : Environ.env -> Evd.evar_map -> t -> Evd.evar_map * t

      val make
        :  Environ.env
        -> Evd.evar_map
        -> EConstr.t
        -> EConstr.t
        -> Evd.evar_map * t

      val unify : Environ.env -> Evd.evar_map -> t -> Evd.evar_map * bool
      val unifies : EConstr.t -> EConstr.t -> bool mm
    end

    module Problem : sig
      type t =
        { act : Pair.t
        ; goto : Pair.t
        ; tree : Enc.Tree.t
        }

      type k = t

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?s:string -> k -> unit
      val unify_pair_opt : Pair.t -> bool mm
      val unify_opt : t -> Enc.Tree.t option mm
      val of_constructor : Rocq_utils.constructor_args -> Constructor.t -> t
    end

    module Problems : sig
      type t =
        { sigma : Evd.evar_map
        ; to_unify : Problem.t list
        }

      type k = t

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?s:string -> k -> unit
      val empty : unit -> t mm
      val is_empty : t -> bool
      val unify_list_opt : Problem.t list -> Enc.Tree.t list option mm

      val sandbox_unify_all_opt
        :  EConstr.t
        -> EConstr.t
        -> t
        -> (EConstr.t * EConstr.t * Enc.Tree.t list) option mm
    end

    module ListOfProblems : sig
      type t = Problems.t list
      type k = Problems.t list

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?s:string -> k -> unit
      val is_empty : t -> bool
      val cross_product : Problems.t -> t -> t
    end

    module Constructors : sig
      type t = Constructor.t list

      val json : ?as_elt:bool -> t -> Yojson.t
      val to_string : ?pretty:bool -> t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit

      val retrieve
        :  int
        -> t
        -> EConstr.t
        -> EConstr.t
        -> Enc.t * ListOfProblems.t
        -> t mm

      val to_problems : Rocq_utils.constructor_args -> t -> Problems.t mm
      val axiom : EConstr.t -> EConstr.t -> Enc.t * int -> t -> t mm
    end

    val check_constructor_args_unify
      :  EConstr.t
      -> EConstr.t
      -> Rocq_utils.constructor_args
      -> bool mm

    val check_valid_constructors
      :  Rocq_ind.LTS.constructor array
      -> Ind.t F.t
      -> EConstr.t
      -> EConstr.t
      -> Enc.t
      -> Constructors.t mm

    val explore_valid_constructor
      :  Ind.t F.t
      -> EConstr.t
      -> Enc.t
      -> Rocq_utils.constructor_args
      -> int * Constructors.t
      -> EConstr.Vars.substl * EConstr.rel_declaration list
      -> Constructors.t mm

    val check_updated_ctx
      :  Enc.t
      -> ListOfProblems.t
      -> Ind.t F.t
      -> EConstr.Vars.substl * EConstr.rel_declaration list
      -> (Enc.t * ListOfProblems.t) option mm

    val check_for_next_constructors
      :  int
      -> EConstr.t
      -> EConstr.t
      -> Constructors.t
      -> (Enc.t * ListOfProblems.t) option
      -> Constructors.t mm

    val collect_valid_constructors
      :  Rocq_ind.LTS.constructor array
      -> Ind.t F.t
      -> EConstr.t
      -> EConstr.t
      -> Enc.t
      -> Constructors.t mm
  end

  val make_enc_hashtbl : unit -> (module Hashtbl.S with type key = Enc.t)
  val make_enc_set : unit -> (module Set.S with type elt = Enc.t)
  val make_econstr_set : unit -> (module Set.S with type elt = EConstr.t)

  module Bindings : sig
    module Instructions : sig
      type t =
        | Undefined
        | Done
        | Arg of
            { root : Constr.t
            ; index : int
            ; cont : t
            }

      val json : ?as_elt:bool -> t -> Yojson.t
      val to_string : ?pretty:bool -> t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit

      exception Rocq_bindings_CannotAppendDone of unit

      val append : t -> t -> t
      val length : t -> int
    end

    module ConstrMap : sig
      include Hashtbl.S with type key = Constr.t

      type v = Names.Name.t * Instructions.t
      type t' = v t

      val json : ?as_elt:bool -> t' -> Yojson.t
      val to_string : ?pretty:bool -> t' -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t' -> unit
      val update : t' -> Constr.t -> v -> unit

      exception Rocq_bindings_CannotFindBindingName of EConstr.t

      val find_name
        :  (EConstr.t * Names.Name.t) list
        -> EConstr.t
        -> Names.Name.t mm

      val extract_binding_map
        :  (EConstr.t * Names.Name.t) list
        -> EConstr.t
        -> Constr.t
        -> t' mm

      val make_opt
        :  (EConstr.t * Names.Name.t) list
        -> EConstr.t * Constr.t
        -> t' option mm
    end

    type t =
      | No_Bindings
      | Use_Bindings of
          { from : ConstrMap.t' option
          ; action : ConstrMap.t' option
          ; goto : ConstrMap.t' option
          }

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    val use_no_bindings : ConstrMap.t' option list -> bool

    val extract
      :  (EConstr.t * Names.Name.t) list
      -> EConstr.t * Constr.t
      -> EConstr.t * Constr.t
      -> EConstr.t * Constr.t
      -> t mm
  end

  module ConstructorBindings : sig
    type t =
      { index : int
      ; name : string
      ; bindings : Bindings.t
      }

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    val extract_info : Enc.t Rocq_ind.t -> t list mm
    val get_quantified_hyp : Names.Name.t -> Tactypes.quantified_hypothesis

    exception Rocq_bindings_BindingInstruction_NotApp of EConstr.t

    exception
      Rocq_bindings_BindingInstruction_Undefined of EConstr.t * EConstr.t

    exception
      Rocq_bindings_BindingInstruction_IndexOutOfBounds of EConstr.t * int

    exception Rocq_bindings_BindingInstruction_NEQ of EConstr.t * Constr.t

    val get_bound_term : EConstr.t -> Bindings.Instructions.t -> EConstr.t mm

    val get_explicit_bindings
      :  EConstr.t * Bindings.ConstrMap.t' option
      -> EConstr.t Tactypes.explicit_bindings mm

    val get
      :  EConstr.t
      -> EConstr.t option
      -> EConstr.t option
      -> Bindings.t
      -> EConstr.t Tactypes.bindings mm
  end
end
