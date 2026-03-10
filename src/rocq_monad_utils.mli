module Make : (Log : Logger.S)
    (Ctx : Rocq_context.S)
    (Enc : Encoding.S)
    (* (Tree : sig
       module Node : sig
       type t = Enc.t * int

       val compare : t -> t -> int
       val equal : t -> t -> bool
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
       end
       with type t = Enc.t * int

       type 'a tree = N of 'a * 'a tree list
       type t = Node.t tree

       val add : t -> t -> t
       val add_list : t -> t list -> t list
       val equal : t -> t -> bool
       val compare : t -> t -> int
       val minimize : t -> Node.t list

       exception CannotMinimizeEmptyList of unit

       val min : t list -> Node.t list
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
       end
       with type Node.t = Enc.t * int) *)
    -> sig
  include module type of Rocq_monad.Make (Log) (Ctx) (Enc)

  val fresh_evar : Rocq_utils.evar_source -> Evd.econstr mm
  val econstr_eq : ?enc:bool -> Evd.econstr -> Evd.econstr -> bool mm
  val econstr_compare : Evd.econstr -> Evd.econstr -> int
  val get_encoding : Evd.econstr -> Enc.t
  val econstr_kind : Evd.econstr -> Rocq_utils.econstr_kind mm
  val econstr_is_evar : Evd.econstr -> bool mm

  val econstr_to_constr
    :  ?abort_on_undefined_evars:bool
    -> Evd.econstr
    -> Constr.t mm

  val econstr_to_constr_opt : Evd.econstr -> Constr.t option mm
  val constrexpr_to_econstr : Constrexpr.constr_expr -> Evd.econstr mm
  val exists_eq : Evd.econstr -> 'a list -> ('a -> Evd.econstr) -> bool mm
  val type_of_econstr : Evd.econstr -> Evd.econstr mm
  val type_of_constrexpr : Constrexpr.constr_expr -> Evd.econstr mm

  module Strfy : sig
    val constr : Constr.t -> string
    val constr_kind : Constr.t -> string
    val econstr : Evd.econstr -> string
    val econstr_kind : Evd.econstr -> string
    val econstr_rel_decl : EConstr.rel_declaration -> string
    val hyp_name : Rocq_utils.hyp -> string
    val hyp_type : Rocq_utils.hyp -> string
    val hyp : Rocq_utils.hyp -> string
    val hyp_value : Rocq_utils.hyp -> string
    val rocq_ind : ('a -> string) -> 'a Rocq_ind.t -> string
    val econstr_bindings : Evd.econstr Tactypes.bindings -> string
  end

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
          * Evd.econstr list
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
      -> Evd.econstr list
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
      :  Evd.econstr list
      -> EConstr.rel_declaration list
      -> 'a mm

    val invalid_lts_args_length : int -> 'a
    val invalid_lts_term_kind : Constr.t -> 'a mm
  end

  module Err : SErr

  module Ind : sig
    type t = Enc.t Rocq_ind.t

    val get_lts : t -> Rocq_ind.LTS.t
    val get_lts_term_type : t -> Evd.econstr
    val get_lts_label_type : t -> Evd.econstr
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
    -> ('a, Evd.econstr, 'b) Context.Rel.Declaration.pt list
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
    val encode : Evd.econstr -> Evd.econstr -> Enc.Tree.t -> t
  end

  val make_state_tree_pair_set
    :  unit
    -> (module Set.S with type elt = Enc.t * Enc.Tree.t)

  module Unification : sig
    module Pair : sig
      type t =
        { to_check : Evd.econstr
        ; acc : Evd.econstr
        }

      type k = t

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?s:string -> k -> unit
      val fresh : Environ.env -> Evd.evar_map -> t -> Evd.evar_map * t

      val make
        :  Environ.env
        -> Evd.evar_map
        -> Evd.econstr
        -> Evd.econstr
        -> Evd.evar_map * t

      val unify : Environ.env -> Evd.evar_map -> t -> Evd.evar_map * bool
      val unifies : Evd.econstr -> Evd.econstr -> bool mm
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
        :  Evd.econstr
        -> Evd.econstr
        -> t
        -> (Evd.econstr * Evd.econstr * Enc.Tree.t list) option mm
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
        -> Evd.econstr
        -> Evd.econstr
        -> Enc.t * ListOfProblems.t
        -> t mm

      val to_problems : Rocq_utils.constructor_args -> t -> Problems.t mm
      val axiom : Evd.econstr -> Evd.econstr -> Enc.t * int -> t -> t mm
    end

    val check_constructor_args_unify
      :  Evd.econstr
      -> Evd.econstr
      -> Rocq_utils.constructor_args
      -> bool mm

    val check_valid_constructors
      :  Rocq_ind.LTS.constructor array
      -> Ind.t F.t
      -> Evd.econstr
      -> Evd.econstr
      -> Enc.t
      -> Constructors.t mm

    val explore_valid_constructor
      :  Ind.t F.t
      -> Evd.econstr
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
      -> Evd.econstr
      -> Evd.econstr
      -> Constructors.t
      -> (Enc.t * ListOfProblems.t) option
      -> Constructors.t mm

    val collect_valid_constructors
      :  Rocq_ind.LTS.constructor array
      -> Ind.t F.t
      -> Evd.econstr
      -> Evd.econstr
      -> Enc.t
      -> Constructors.t mm
  end

  val make_enc_hashtbl : unit -> (module Hashtbl.S with type key = Enc.t)
  val make_enc_set : unit -> (module Set.S with type elt = Enc.t)
  val make_econstr_set : unit -> (module Set.S with type elt = Evd.econstr)

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

      exception Rocq_bindings_CannotFindBindingName of Evd.econstr

      val find_name
        :  (Evd.econstr * Names.Name.t) list
        -> Evd.econstr
        -> Names.Name.t mm

      val extract_binding_map
        :  (Evd.econstr * Names.Name.t) list
        -> Evd.econstr
        -> Constr.t
        -> t' mm

      val make_opt
        :  (Evd.econstr * Names.Name.t) list
        -> Evd.econstr * Constr.t
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
      :  (Evd.econstr * Names.Name.t) list
      -> Evd.econstr * Constr.t
      -> Evd.econstr * Constr.t
      -> Evd.econstr * Constr.t
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

    exception Rocq_bindings_BindingInstruction_NotApp of Evd.econstr

    exception
      Rocq_bindings_BindingInstruction_Undefined of Evd.econstr * Evd.econstr

    exception
      Rocq_bindings_BindingInstruction_IndexOutOfBounds of Evd.econstr * int

    exception Rocq_bindings_BindingInstruction_NEQ of Evd.econstr * Constr.t

    val get_bound_term
      :  Evd.econstr
      -> Bindings.Instructions.t
      -> Evd.econstr mm

    val get_explicit_bindings
      :  Evd.econstr * Bindings.ConstrMap.t' option
      -> Evd.econstr Tactypes.explicit_bindings mm

    val get
      :  EConstr.t
      -> EConstr.t option
      -> EConstr.t option
      -> Bindings.t
      -> Evd.econstr Tactypes.bindings mm
  end
end
