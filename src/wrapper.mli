module Make : (Log : Logger.SLogger)
    (C : Rocq_context.SRocq_context)
    (E : Encoding.SEncoding)
    -> sig
  module M : sig
    module Ctx : sig
      val get : unit -> Rocq_context.t ref
      val env : unit -> Environ.env ref
      val sigma : unit -> Evd.evar_map ref
      val update : Environ.env ref -> Evd.evar_map ref -> unit
    end

    module Enc : sig
      type t = Rocq_monad.Make(Log)(C)(E).Enc.t

      val init : t
      val next : t -> t
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
      val to_string : t -> string
      val counter : t ref
      val reset : unit -> unit
      val incr : unit -> t
    end

    module F : sig
      type key = EConstr.t
      type 'a t = 'a Bi_encoding.Make(Log)(Ctx)(Enc).F.t

      val create : int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key -> 'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_opt : 'a t -> key -> 'a option
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key -> 'a -> unit
      val mem : 'a t -> key -> bool
      val iter : (key -> 'a -> unit) -> 'a t -> unit
      val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
      val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
      val length : 'a t -> int
      val stats : 'a t -> Hashtbl.statistics
      val to_seq : 'a t -> (key * 'a) Seq.t
      val to_seq_keys : 'a t -> key Seq.t
      val to_seq_values : 'a t -> 'a Seq.t
      val add_seq : 'a t -> (key * 'a) Seq.t -> unit
      val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
      val of_seq : (key * 'a) Seq.t -> 'a t
    end

    module B : sig
      type key = Enc.t
      type 'a t = 'a Bi_encoding.Make(Log)(Ctx)(Enc).B.t

      val create : int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key -> 'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_opt : 'a t -> key -> 'a option
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key -> 'a -> unit
      val mem : 'a t -> key -> bool
      val iter : (key -> 'a -> unit) -> 'a t -> unit
      val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
      val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
      val length : 'a t -> int
      val stats : 'a t -> Hashtbl.statistics
      val to_seq : 'a t -> (key * 'a) Seq.t
      val to_seq_keys : 'a t -> key Seq.t
      val to_seq_values : 'a t -> 'a Seq.t
      val add_seq : 'a t -> (key * 'a) Seq.t -> unit
      val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
      val of_seq : (key * 'a) Seq.t -> 'a t
    end

    type maps = Bi_encoding.Make(Log)(Ctx)(Enc).maps =
      { fwd : Enc.t F.t
      ; bck : EConstr.t B.t
      }

    val the_maps : maps ref option ref
    val reset : unit -> unit

    exception MapsNotInitialised of unit

    val get_the_maps : unit -> maps ref
    val fwdmap : unit -> Enc.t F.t
    val bckmap : unit -> EConstr.t B.t

    exception EncodingNotFound of EConstr.t

    val get_encoding : EConstr.t -> Enc.t
    val encode : EConstr.t -> Enc.t
    val encoded : EConstr.t -> bool

    exception DecodingNotFound of Enc.t

    val get_econstr : Enc.t -> EConstr.t

    exception CannotDecode of Enc.t

    val decode : Enc.t -> EConstr.t
    val decode_opt : Enc.t -> EConstr.t option
    val decode_map : 'a B.t -> 'a F.t
    val encode_map : 'a F.t -> 'a B.t
    val to_list : unit -> (Enc.t * EConstr.t) list
    val bienc_to_list : unit -> (Enc.t * EConstr.t) list

    type 'a mm = wrapper ref -> 'a in_wrapper

    and wrapper =
      { ctx : Rocq_context.t ref
      ; maps : maps ref
      }

    and 'a in_wrapper =
      { state : wrapper ref
      ; value : 'a
      }

    val run : ?reset_encoding:bool -> 'a mm -> 'a
    val return : 'a -> 'a mm
    val bind : 'a mm -> ('a -> 'b mm) -> 'b mm
    val map : ('a -> 'b) -> 'a mm -> 'b mm
    val product : 'a mm -> 'b mm -> ('a * 'b) mm
    val iterate : int -> int -> 'a -> (int -> 'a -> 'a mm) -> 'a mm

    val state
      :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
      -> wrapper ref
      -> 'a in_wrapper

    val sandbox : ?sigma:Evd.evar_map -> 'a mm -> wrapper ref -> 'a in_wrapper

    module type SYNTAX = sig
      val ( let+ ) : 'a mm -> ('a -> 'b) -> 'b mm
      val ( let* ) : 'a mm -> ('a -> 'b mm) -> 'b mm

      val ( let$ )
        :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
        -> ('a -> 'b mm)
        -> 'b mm

      val ( let$* )
        :  (Environ.env -> Evd.evar_map -> Evd.evar_map)
        -> (unit -> 'b mm)
        -> 'b mm

      val ( let$+ )
        :  (Environ.env -> Evd.evar_map -> 'a)
        -> ('a -> 'b mm)
        -> 'b mm

      val ( and+ ) : 'a mm -> 'b mm -> ('a * 'b) mm
    end

    module Syntax : sig
      val ( let+ ) : 'a mm -> ('a -> 'b) -> 'b mm
      val ( let* ) : 'a mm -> ('a -> 'b mm) -> 'b mm

      val ( let$ )
        :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
        -> ('a -> 'b mm)
        -> 'b mm

      val ( let$* )
        :  (Environ.env -> Evd.evar_map -> Evd.evar_map)
        -> (unit -> 'b mm)
        -> 'b mm

      val ( let$+ )
        :  (Environ.env -> Evd.evar_map -> 'a)
        -> ('a -> 'b mm)
        -> 'b mm

      val ( and+ ) : 'a mm -> 'b mm -> ('a * 'b) mm
    end

    val econstr_normalize : EConstr.t -> EConstr.t mm
    val get_ctx : wrapper ref -> Rocq_context.t in_wrapper
    val get_env : wrapper ref -> Environ.env in_wrapper
    val get_sigma : wrapper ref -> Evd.evar_map in_wrapper
    val get_maps : wrapper ref -> maps in_wrapper
    val get_fwdmap : wrapper ref -> Enc.t F.t in_wrapper
    val get_bckmap : wrapper ref -> EConstr.t B.t in_wrapper
    val fstring : (Environ.env -> Evd.evar_map -> 'a -> string) -> 'a -> string

    module Tree : sig
      module type STreeNode = sig
        type t = Enc.t * int

        val to_string : t -> string
      end

      module TreeNode : sig
        type t = Enc.t * int

        val to_string : t -> string
      end

      type 'a tree = 'a Enc_tree.Make(Enc).tree = Node of 'a * 'a tree list
      type t = TreeNode.t tree

      val add : t -> t -> t
      val add_list : t -> t list -> t list
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val minimize : t -> TreeNode.t list

      exception CannotMinimizeEmptyList of unit

      val min : t list -> TreeNode.t list
      val to_string : t -> string
      val list_to_string : ?args:Utils.Strfy.style_args -> t list -> string
    end

    module Constructor : sig
      type t = EConstr.t * EConstr.t * Tree.t

      val to_string : Environ.env -> Evd.evar_map -> t -> string
    end

    val make_state_tree_pair_set
      :  unit
      -> (module Set.S with type elt = Enc.t * Tree.t)

    val make_hashtbl : unit -> (module Hashtbl.S with type key = Enc.t)
    val make_set : unit -> (module Set.S with type elt = Enc.t)

    (* val make_econstr_set : unit -> (module Set.S with type elt = EConstr.t) *)
    val fresh_evar : Rocq_utils.evar_source -> EConstr.t mm
    val econstr_eq : EConstr.t -> EConstr.t -> bool
    val econstr_compare : EConstr.t -> EConstr.t -> int
    val econstr_kind : EConstr.t -> Rocq_utils.econstr_kind mm
    val econstr_is_evar : EConstr.t -> bool mm

    val econstr_to_constr
      :  ?abort_on_undefined_evars:bool
      -> EConstr.t
      -> Constr.t mm

    val econstr_to_constr_opt : EConstr.t -> Constr.t option mm
    val constrexpr_to_econstr : Constrexpr.constr_expr -> EConstr.t mm
    val exists_eq : EConstr.t -> 'a list -> ('a -> EConstr.t) -> bool
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

    module Errors : sig
      type t = Rocq_monad_utils.Make(Log)(C)(E).Errors.t =
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

    module Err : sig
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

    module Unification : sig
      module type SPair = sig
        type t =
          { to_check : EConstr.t
          ; acc : EConstr.t
          }

        val to_string : Environ.env -> Evd.evar_map -> t -> string

        val make
          :  Environ.env
          -> Evd.evar_map
          -> EConstr.t
          -> EConstr.t
          -> Evd.evar_map * t

        val unify : Environ.env -> Evd.evar_map -> t -> Evd.evar_map * bool
      end

      module Pair : sig
        type t = Rocq_monad_utils.Make(Log)(C)(E).Unification.Pair.t =
          { to_check : EConstr.t
          ; acc : EConstr.t
          }

        val to_string : Environ.env -> Evd.evar_map -> t -> string

        val make
          :  Environ.env
          -> Evd.evar_map
          -> EConstr.t
          -> EConstr.t
          -> Evd.evar_map * t

        val unify : Environ.env -> Evd.evar_map -> t -> Evd.evar_map * bool
      end

      module type SProblem = sig
        type t =
          { act : Pair.t
          ; goto : Pair.t
          ; tree : Tree.t
          }

        val to_string : Environ.env -> Evd.evar_map -> t -> string
        val unify_opt : t -> Tree.t option mm
      end

      module Problem : sig
        type t =
          { act : Pair.t
          ; goto : Pair.t
          ; tree : Tree.t
          }

        val to_string : Environ.env -> Evd.evar_map -> t -> string
        val unify_opt : t -> Tree.t option mm
      end

      module type SProblems = sig
        type t =
          { sigma : Evd.evar_map
          ; to_unify : Problem.t list
          }

        val empty : unit -> t mm
        val list_is_empty : t list -> bool
        val to_string : Environ.env -> t -> string
        val list_to_string : Environ.env -> t list -> string

        val sandbox_unify_all_opt
          :  EConstr.t
          -> EConstr.t
          -> t
          -> (EConstr.t * EConstr.t * Tree.t list) option mm
      end

      module Problems : sig
        type t =
          { sigma : Evd.evar_map
          ; to_unify : Problem.t list
          }

        val empty : unit -> t mm
        val list_is_empty : t list -> bool
        val to_string : Environ.env -> t -> string
        val list_to_string : Environ.env -> t list -> string

        val sandbox_unify_all_opt
          :  EConstr.t
          -> EConstr.t
          -> t
          -> (EConstr.t * EConstr.t * Tree.t list) option mm
      end

      module type SConstructors = sig
        type t = Constructor.t list

        val to_string : Environ.env -> Evd.evar_map -> t -> string

        val retrieve
          :  int
          -> t
          -> EConstr.t
          -> EConstr.t
          -> Enc.t * Problems.t list
          -> t mm
      end

      module Constructors : sig
        type t = Constructor.t list

        val to_string : Environ.env -> Evd.evar_map -> t -> string

        val retrieve
          :  int
          -> t
          -> EConstr.t
          -> EConstr.t
          -> Enc.t * Problems.t list
          -> t mm
      end

      val constr_to_problem
        :  Rocq_utils.constructor_args
        -> Constructor.t
        -> Problem.t

      val map_problems
        :  Rocq_utils.constructor_args
        -> Constructors.t
        -> Problems.t mm

      val cross_product : Problems.t list -> Problems.t -> Problems.t list
      val does_constructor_unify : EConstr.t -> EConstr.t -> bool mm

      val check_constructor_args_unify
        :  EConstr.t
        -> EConstr.t
        -> Rocq_utils.constructor_args
        -> bool mm

      val axiom_constructor
        :  EConstr.t
        -> EConstr.t
        -> Enc.t * int
        -> Constructors.t
        -> Constructors.t mm

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
        -> Problems.t list
        -> Ind.t F.t
        -> EConstr.Vars.substl * EConstr.rel_declaration list
        -> (Enc.t * Problems.t list) option mm

      val check_for_next_constructors
        :  int
        -> EConstr.t
        -> EConstr.t
        -> Constructors.t
        -> (Enc.t * Problems.t list) option
        -> Constructors.t mm

      val collect_valid_constructors
        :  Rocq_ind.LTS.constructor array
        -> Ind.t F.t
        -> EConstr.t
        -> EConstr.t
        -> Enc.t
        -> Constructors.t mm
    end
  end

  module Model : sig
    module Tree : sig
      module type STreeNode = sig
        type t = M.Enc.t * int

        val to_string : t -> string
      end

      module TreeNode : sig
        type t = M.Enc.t * int

        val to_string : t -> string
      end

      type 'a tree = 'a Enc_tree.Make(M.Enc).tree = Node of 'a * 'a tree list
      type t = TreeNode.t tree

      val add : t -> t -> t
      val add_list : t -> t list -> t list
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val minimize : t -> TreeNode.t list

      exception CannotMinimizeEmptyList of unit

      val min : t list -> TreeNode.t list
      val to_string : t -> string
      val list_to_string : ?args:Utils.Strfy.style_args -> t list -> string
    end

    module Trees : sig
      type elt = Tree.t
      type t = Set.Make(Tree).t

      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t

      exception EmptyHasNoMin

      val min : t -> elt
      val min_opt : t -> elt option
    end

    module State : sig
      type t =
        { term : M.Enc.t
        ; pp : string option
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
      val to_string : ?pretty:bool -> t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module States : sig
      module S : sig
        type elt = State.t
        type t

        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end

      type elt = State.t
      type t = S.t

      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
      val add_to_opt : State.t -> t option -> t

      exception StateHasNoOrigin of (State.t * t * t)

      val origin_of_state : State.t -> t -> t -> int
      val has_shared_origin : t -> t -> t -> bool
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module Label : sig
      type t =
        { term : M.Enc.t
        ; pp : string option
        ; is_silent : bool option
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
      val is_silent : t -> bool
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module Labels : sig
      module S : sig
        type elt = Label.t
        type t

        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end

      type elt = Label.t
      type t = S.t

      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
      val non_silent : t -> t
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module Note : sig
      type t =
        { from : State.t
        ; label : Label.t
        ; using : Trees.t
        ; goto : State.t
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val is_silent : t -> bool
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module Annotation : sig
      type t =
        { this : Note.t
        ; next : t option
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val is_empty : t -> bool
      val length : t -> int
      val shorter : t -> t -> t
      val exists : Note.t -> t -> bool
      val exists_label : Label.t -> t -> bool
      val append : Note.t -> t -> t
      val last : t -> Note.t

      exception CannotDropLastOfSingleton of t

      val drop_last : t -> t
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module Annotations : sig
      module S : sig
        type elt = Annotation.t
        type t

        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end

      type elt = Annotation.t
      type t = S.t

      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
      val extrapolate : Annotation.t -> t
    end

    module Transition : sig
      type t =
        { from : State.t
        ; goto : State.t
        ; label : Label.t
        ; annotation : Annotation.t option
        ; constructor_tree : Tree.t option
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val is_silent : t -> bool
      val annotation_is_empty : t -> bool
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module Transitions : sig
      module S : sig
        type elt = Transition.t
        type t

        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end

      type elt = Transition.t
      type t = S.t

      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
      val labels : t -> Labels.t
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module Action : sig
      type t =
        { label : Label.t
        ; annotation : Annotation.t option
        ; constructor_trees : Trees.t
        }

      val wk_equal : t -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
      val is_silent : t -> bool
      val annotation_is_empty : t -> bool
      val annotation_length : t -> int
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module ActionPair : sig
      type t = Action.t * States.t

      val compare : t -> t -> int
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
      val try_update : t -> t list -> t option * t list
      val merge_lists : t list -> t list -> t list
    end

    module ActionPairs : sig
      module S : sig
        type elt = ActionPair.t
        type t = Set.Make(ActionPair).t

        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end

      type elt = ActionPair.t
      type t = Set.Make(ActionPair).t

      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
      val destinations : t -> States.t

      exception IsEmpty

      val shorest_annotation : t -> ActionPair.t
      val merge_list : t -> ActionPair.t list -> t
    end

    module Actions : sig
      module S : sig
        type elt = Action.t
        type t

        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end

      type elt = Action.t
      type t = S.t

      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
      val labelled : t -> Label.t -> t
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module ActionMap : sig
      module M : sig
        type key = Action.t
        type !'a t

        val create : int -> 'a t
        val clear : 'a t -> unit
        val reset : 'a t -> unit
        val copy : 'a t -> 'a t
        val add : 'a t -> key -> 'a -> unit
        val remove : 'a t -> key -> unit
        val find : 'a t -> key -> 'a
        val find_opt : 'a t -> key -> 'a option
        val find_all : 'a t -> key -> 'a list
        val replace : 'a t -> key -> 'a -> unit
        val mem : 'a t -> key -> bool
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
        val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
        val length : 'a t -> int
        val stats : 'a t -> Hashtbl.statistics
        val to_seq : 'a t -> (key * 'a) Seq.t
        val to_seq_keys : 'a t -> key Seq.t
        val to_seq_values : 'a t -> 'a Seq.t
        val add_seq : 'a t -> (key * 'a) Seq.t -> unit
        val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
        val of_seq : (key * 'a) Seq.t -> 'a t
      end

      type key = Action.t
      type 'a t = 'a M.t

      val create : int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key -> 'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_opt : 'a t -> key -> 'a option
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key -> 'a -> unit
      val mem : 'a t -> key -> bool
      val iter : (key -> 'a -> unit) -> 'a t -> unit
      val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
      val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
      val length : 'a t -> int
      val stats : 'a t -> Hashtbl.statistics
      val to_seq : 'a t -> (key * 'a) Seq.t
      val to_seq_keys : 'a t -> key Seq.t
      val to_seq_values : 'a t -> 'a Seq.t
      val add_seq : 'a t -> (key * 'a) Seq.t -> unit
      val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
      val of_seq : (key * 'a) Seq.t -> 'a t

      type t' = States.t t

      val update : t' -> Action.t -> States.t -> unit
      val destinations : t' -> States.t
      val reduce_by_label : t' -> Label.t -> t'
      val to_actions : t' -> Actions.t
      val to_actionpairs : t' -> ActionPairs.t
      val of_actionpairs : ActionPairs.t -> t'
      val merge : t' -> t' -> t'
      val to_string : t' -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t' -> unit
    end

    module Edge : sig
      type t =
        { from : State.t
        ; goto : State.t
        ; action : Action.t
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val is_silent : t -> bool
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module Edges : sig
      module S : sig
        type elt = Edge.t
        type t

        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end

      type elt = Edge.t
      type t = S.t

      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
      val labelled : t -> Label.t -> t
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module EdgeMap : sig
      module M : sig
        type key = State.t
        type !'a t

        val create : int -> 'a t
        val clear : 'a t -> unit
        val reset : 'a t -> unit
        val copy : 'a t -> 'a t
        val add : 'a t -> key -> 'a -> unit
        val remove : 'a t -> key -> unit
        val find : 'a t -> key -> 'a
        val find_opt : 'a t -> key -> 'a option
        val find_all : 'a t -> key -> 'a list
        val replace : 'a t -> key -> 'a -> unit
        val mem : 'a t -> key -> bool
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
        val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
        val length : 'a t -> int
        val stats : 'a t -> Hashtbl.statistics
        val to_seq : 'a t -> (key * 'a) Seq.t
        val to_seq_keys : 'a t -> key Seq.t
        val to_seq_values : 'a t -> 'a Seq.t
        val add_seq : 'a t -> (key * 'a) Seq.t -> unit
        val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
        val of_seq : (key * 'a) Seq.t -> 'a t
      end

      type key = State.t
      type 'a t = 'a M.t

      val create : int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key -> 'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_opt : 'a t -> key -> 'a option
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key -> 'a -> unit
      val mem : 'a t -> key -> bool
      val iter : (key -> 'a -> unit) -> 'a t -> unit
      val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
      val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
      val length : 'a t -> int
      val stats : 'a t -> Hashtbl.statistics
      val to_seq : 'a t -> (key * 'a) Seq.t
      val to_seq_keys : 'a t -> key Seq.t
      val to_seq_values : 'a t -> 'a Seq.t
      val add_seq : 'a t -> (key * 'a) Seq.t -> unit
      val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
      val of_seq : (key * 'a) Seq.t -> 'a t

      type t' = ActionMap.t' t

      val update : t' -> State.t -> Action.t -> States.t -> unit
      val destinations : t' -> State.t -> States.t
      val get_actions : t' -> State.t -> Actions.t
      val reduce_by_label : t' -> Label.t -> t'
      val get_edges : t' -> State.t -> Edges.t
      val to_edges : t' -> Edges.t
      val of_edges : Edges.t -> t'
      val merge : t' -> t' -> t'
      val to_string : t' -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t' -> unit
    end

    module Partition : sig
      module S : sig
        type elt = States.t
        type t

        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end

      type elt = S.elt
      type t = S.t

      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
      val get_bisimilar : State.t -> t -> States.t
      val filter_reachable : States.t -> t -> t
      val reachable : State.t -> EdgeMap.t' -> t -> t
      val reachable_by_label : State.t -> Label.t -> EdgeMap.t' -> t -> t
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module Info : sig
      type t =
        { meta : meta option
        ; weak_labels : Labels.t
        }

      and meta =
        { is_complete : bool
        ; is_merged : bool
        ; bounds : bounds
        ; lts : lts list
        }

      and bounds =
        | States of int
        | Transitions of int

      and lts =
        { enc : M.Enc.t
        ; constructors : Rocq_bindings.constructor list
        }

      val merge : t -> t -> t
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module LTS : sig
      type t =
        { init : State.t option
        ; terminals : Partition.elt
        ; alphabet : Labels.t
        ; states : Partition.elt
        ; transitions : Transitions.t
        ; info : Info.t
        }

      val to_string : t -> string
    end

    module FSM : sig
      type t =
        { init : State.t option
        ; terminals : Partition.elt
        ; alphabet : Labels.t
        ; states : Partition.elt
        ; edges : EdgeMap.t'
        ; info : Info.t
        }

      val merge : t -> t -> t
      val is_weak_mode : t -> bool
      val to_string : t -> string
    end

    module Convert : sig
      val transitions_to_edgemap : Transitions.t -> EdgeMap.t'
      val lts_to_fsm : LTS.t -> FSM.t
    end

    module Saturate : sig
      module Log : Logger.SLogger

      module WIP : sig
        type t =
          { from : State.t
          ; via : Label.t
          ; trees : Trees.t
          }

        val is_silent : t -> bool
        val is_named : t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val create : State.t -> Action.t -> t

        exception IsEmptyList

        val list_to_annotation : State.t -> t list -> Annotation.t
      end

      module Trace : sig
        module NT : sig end

        type t =
          { this : WIP.t
          ; next : next option
          }

        and next =
          | Next of t
          | Goto of State.t

        val create : WIP.t -> t
        val compare : t -> t -> int
        val compare_next : next -> next -> int

        exception Invalid

        val has_named : ?validate:bool -> t -> bool
        val validate : t -> unit

        exception CouldNotFindGoto

        val get_goto : t -> State.t

        exception CouldNotFindNamed

        val get_named : t -> Label.t
        val get_named_opt : t -> Label.t option

        exception FailAdd_AlreadyNamed
        exception FailAdd_AlreadyHasGoto

        val add : WIP.t -> t -> t

        exception FailSetGoto_AlreadyHasGoto

        val set_goto : State.t -> t -> t

        exception FailSeq_AlreadyNamed
        exception FailSeq_AlreadyHasGoto

        val seq : t -> t -> t
        val seq_opt : t option -> t -> t
        val get : WIP.t -> t -> t
        val upto_named : t -> t

        exception GotoNotSet

        val to_annotation : t -> Annotation.t
      end

      module Traces : sig
        type elt = Trace.t
        type t = Set.Make(Trace).t

        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
        val get : WIP.t -> t -> t
      end

      type data =
        { named : Label.t option
        ; current : Trace.t option
        ; visited : States.t
        ; traces : Traces.t ref
        ; can_collect_traces : bool ref
        ; old_edges : EdgeMap.t'
        }

      val initial_data : Traces.t ref -> EdgeMap.t' -> data
      val has_named : data -> bool
      val update_traces : data -> Trace.t -> unit
      val update_named : Action.t -> data -> data
      val update_current : WIP.t -> data -> data
      val update_visited : State.t -> data -> data
      val already_visited : State.t -> data -> bool
      val skip_action : Action.t -> data -> bool
      val get_old_actions : State.t -> data -> ActionMap.t' option
      val update_acc : Trace.t -> Label.t -> ActionPairs.t -> ActionPairs.t
      val stop : data -> State.t -> ActionPairs.t -> ActionPairs.t

      val finish_with_trace
        :  Trace.t
        -> data
        -> Label.t
        -> ActionPairs.t
        -> ActionPairs.t

      val finish_with_trace_upto
        :  Trace.t
        -> data
        -> Label.t
        -> ActionPairs.t
        -> ActionPairs.t

      val check_from : data -> State.t -> ActionPairs.t -> ActionPairs.t

      val check_actions
        :  data
        -> State.t
        -> ActionMap.t'
        -> ActionPairs.t
        -> ActionPairs.t

      val collect_from_traces
        :  data
        -> State.t
        -> Action.t
        -> States.t
        -> ActionPairs.t
        -> ActionPairs.t

      val continue_check_destinations
        :  data
        -> State.t
        -> Action.t
        -> States.t
        -> ActionPairs.t
        -> ActionPairs.t

      val check_destinations
        :  data
        -> State.t
        -> States.t
        -> ActionPairs.t
        -> ActionPairs.t

      val edge_action_destinations
        :  data
        -> State.t
        -> States.t
        -> ActionPairs.t

      val edge_actions
        :  State.t
        -> ActionMap.t'
        -> EdgeMap.t'
        -> Traces.t ref
        -> ActionPairs.t

      val edge
        :  ActionMap.t'
        -> State.t
        -> ActionMap.t'
        -> EdgeMap.t'
        -> Traces.t ref
        -> unit

      val edges : Labels.t -> States.t -> EdgeMap.t' -> EdgeMap.t'
      val fsm : ?only_if_weak:bool -> FSM.t -> FSM.t
    end

    module Minimize : sig
      type t =
        { fsm : FSM.t
        ; pi : Partition.t
        }

      exception CannotSplitEmptyBlock of unit

      val ensure_nonempty : States.t -> unit

      val split_block
        :  Partition.t
        -> State.t
        -> EdgeMap.t'
        -> States.t
        -> States.t * States.t option

      exception Split_OnlyReturnedOneBlock_ButNeqBlock of (States.t * States.t)

      val ensure_equal : States.t -> States.t -> unit

      val for_each_label
        :  Partition.t ref
        -> bool ref
        -> EdgeMap.t'
        -> States.t ref
        -> Label.t
        -> unit

      val for_each_block
        :  Partition.t ref
        -> bool ref
        -> Labels.t
        -> EdgeMap.t'
        -> States.t
        -> unit

      val partition_states : FSM.t -> Partition.t
      val fsm : FSM.t -> t
      val to_string : t -> string
    end

    module Bisimilar : sig
      type t =
        { fsm_a : fsm_pair
        ; fsm_b : fsm_pair
        ; merged : FSM.t
        ; result : result
        }

      and result =
        { bisim_states : Partition.t
        ; non_bisim_states : Partition.t
        }

      and fsm_pair =
        { original : FSM.t
        ; saturated : FSM.t
        }

      val fsm_pair : FSM.t -> fsm_pair
      val are_bisimilar : result -> bool
      val the_cached_result : t option ref
      val set_the_result : t -> unit

      exception NoCachedResult of unit

      val get_the_result : unit -> t
      val split : Partition.t -> States.t -> States.t -> result
      val fsm : FSM.t -> FSM.t -> t
      val fsm_pair_to_string : fsm_pair -> string
      val result_to_string : result -> string
      val to_string : t -> string
    end
  end

  module IsTheory : sig
    val is_theory : EConstr.t -> EConstr.t -> bool M.mm
    val is_exists : EConstr.t -> bool M.mm
    val is_weak_sim : EConstr.t -> bool M.mm
    val is_weak : EConstr.t -> bool M.mm
    val is_tau : EConstr.t -> bool M.mm
    val is_silent : EConstr.t -> bool M.mm
    val is_silent1 : EConstr.t -> bool M.mm
    val is_LTS : EConstr.t -> bool M.mm
    val is_None : EConstr.t -> bool M.mm
    val is_Some : EConstr.t -> bool M.mm
    val get_theory_enc : (EConstr.t -> bool M.mm) -> M.Enc.t M.mm

    exception NoEncodingFoundFor_TheoriesNone of unit

    val get_None_enc : unit -> M.Enc.t M.mm

    exception NoEncodingFoundFor_TheoriesSome of unit

    val get_Some_enc : unit -> M.Enc.t M.mm

    exception NotEqTheory of unit

    val get_theory_enc_if_eq
      :  EConstr.t
      -> (EConstr.t -> bool M.mm)
      -> M.Enc.t M.mm

    val get_None_enc_if_eq : EConstr.t -> M.Enc.t M.mm
    val get_Some_enc_if_eq : EConstr.t -> M.Enc.t M.mm
  end

  module Weak : sig
    type t =
      | Option of M.Enc.t
      | Custom of M.Enc.t * M.Enc.t

    val eq : t -> t -> bool
    val to_string : t -> string M.mm
  end

  module Config : sig
    val load_weak_arg : Api.weak_arg -> Weak.t M.mm
    val load_weak_arg_opt : Api.weak_arg option -> Weak.t option M.mm

    type weak_args =
      { a : Weak.t option
      ; b : Weak.t option
      }

    val the_weak_args : weak_args ref option ref
    val reset_the_weak_args : unit -> unit
    val load_weak_args : unit -> unit M.mm
    val get_the_weak_args : unit -> weak_args option
    val get_the_weak_arg1 : unit -> Weak.t option
    val get_the_weak_arg2 : unit -> Weak.t option
    val api_bounds_to_model_bounds : Api.bounds_args -> Model.Info.bounds
    val the_bounds_args : Model.Info.bounds ref
    val load_the_bounds_args : unit -> unit
  end

  module type X_Args = sig
    val primary_lts : Libnames.qualid
    val grefs : Names.GlobRef.t list
    val weak : Weak.t option
    val bounds : Model.Info.bounds
  end

  module Graph : (T0 : sig
                    type key = M.Enc.t
                    type !'a t

                    val create : int -> 'a t
                    val clear : 'a t -> unit
                    val reset : 'a t -> unit
                    val copy : 'a t -> 'a t
                    val add : 'a t -> key -> 'a -> unit
                    val remove : 'a t -> key -> unit
                    val find : 'a t -> key -> 'a
                    val find_opt : 'a t -> key -> 'a option
                    val find_all : 'a t -> key -> 'a list
                    val replace : 'a t -> key -> 'a -> unit
                    val mem : 'a t -> key -> bool
                    val iter : (key -> 'a -> unit) -> 'a t -> unit

                    val filter_map_inplace
                      :  (key -> 'a -> 'a option)
                      -> 'a t
                      -> unit

                    val fold
                      :  (key -> 'a -> 'acc -> 'acc)
                      -> 'a t
                      -> 'acc
                      -> 'acc

                    val length : 'a t -> int
                    val stats : 'a t -> Hashtbl.statistics
                    val to_seq : 'a t -> (key * 'a) Seq.t
                    val to_seq_keys : 'a t -> key Seq.t
                    val to_seq_values : 'a t -> 'a Seq.t
                    val add_seq : 'a t -> (key * 'a) Seq.t -> unit
                    val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
                    val of_seq : (key * 'a) Seq.t -> 'a t
                  end)
      (V0 : sig
         type elt = T0.key
         type t

         val empty : t
         val add : elt -> t -> t
         val singleton : elt -> t
         val remove : elt -> t -> t
         val union : t -> t -> t
         val inter : t -> t -> t
         val disjoint : t -> t -> bool
         val diff : t -> t -> t
         val cardinal : t -> int
         val elements : t -> elt list
         val min_elt : t -> elt
         val min_elt_opt : t -> elt option
         val max_elt : t -> elt
         val max_elt_opt : t -> elt option
         val choose : t -> elt
         val choose_opt : t -> elt option
         val find : elt -> t -> elt
         val find_opt : elt -> t -> elt option
         val find_first : (elt -> bool) -> t -> elt
         val find_first_opt : (elt -> bool) -> t -> elt option
         val find_last : (elt -> bool) -> t -> elt
         val find_last_opt : (elt -> bool) -> t -> elt option
         val iter : (elt -> unit) -> t -> unit
         val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
         val map : (elt -> elt) -> t -> t
         val filter : (elt -> bool) -> t -> t
         val filter_map : (elt -> elt option) -> t -> t
         val partition : (elt -> bool) -> t -> t * t
         val split : elt -> t -> t * bool * t
         val is_empty : t -> bool
         val mem : elt -> t -> bool
         val equal : t -> t -> bool
         val compare : t -> t -> int
         val subset : t -> t -> bool
         val for_all : (elt -> bool) -> t -> bool
         val exists : (elt -> bool) -> t -> bool
         val to_list : t -> elt list
         val of_list : elt list -> t
         val to_seq_from : elt -> t -> elt Seq.t
         val to_seq : t -> elt Seq.t
         val to_rev_seq : t -> elt Seq.t
         val add_seq : elt Seq.t -> t -> t
         val of_seq : elt Seq.t -> t
       end)
      (_ : sig
         type elt = V0.elt * M.Tree.t
         type t

         val empty : t
         val add : elt -> t -> t
         val singleton : elt -> t
         val remove : elt -> t -> t
         val union : t -> t -> t
         val inter : t -> t -> t
         val disjoint : t -> t -> bool
         val diff : t -> t -> t
         val cardinal : t -> int
         val elements : t -> elt list
         val min_elt : t -> elt
         val min_elt_opt : t -> elt option
         val max_elt : t -> elt
         val max_elt_opt : t -> elt option
         val choose : t -> elt
         val choose_opt : t -> elt option
         val find : elt -> t -> elt
         val find_opt : elt -> t -> elt option
         val find_first : (elt -> bool) -> t -> elt
         val find_first_opt : (elt -> bool) -> t -> elt option
         val find_last : (elt -> bool) -> t -> elt
         val find_last_opt : (elt -> bool) -> t -> elt option
         val iter : (elt -> unit) -> t -> unit
         val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
         val map : (elt -> elt) -> t -> t
         val filter : (elt -> bool) -> t -> t
         val filter_map : (elt -> elt option) -> t -> t
         val partition : (elt -> bool) -> t -> t * t
         val split : elt -> t -> t * bool * t
         val is_empty : t -> bool
         val mem : elt -> t -> bool
         val equal : t -> t -> bool
         val compare : t -> t -> int
         val subset : t -> t -> bool
         val for_all : (elt -> bool) -> t -> bool
         val exists : (elt -> bool) -> t -> bool
         val to_list : t -> elt list
         val of_list : elt list -> t
         val to_seq_from : elt -> t -> elt Seq.t
         val to_seq : t -> elt Seq.t
         val to_rev_seq : t -> elt Seq.t
         val add_seq : elt Seq.t -> t -> t
         val of_seq : elt Seq.t -> t
       end)
      (_ : X_Args)
      -> sig
    module V : sig
      module V2 : sig
        type elt = V0.elt
        type t

        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end

      type elt = V2.elt
      type t = V2.t

      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
      val to_string : t -> string
    end

    module D : sig
      module D2 : sig
        type elt = V.elt * M.Tree.t
        type t

        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end

      type elt = V.elt * M.Tree.t
      type t = D2.t

      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
      val to_string : t -> string
    end

    module A : sig
      module A2 : sig
        type key = Model.Action.t
        type !'a t

        val create : int -> 'a t
        val clear : 'a t -> unit
        val reset : 'a t -> unit
        val copy : 'a t -> 'a t
        val add : 'a t -> key -> 'a -> unit
        val remove : 'a t -> key -> unit
        val find : 'a t -> key -> 'a
        val find_opt : 'a t -> key -> 'a option
        val find_all : 'a t -> key -> 'a list
        val replace : 'a t -> key -> 'a -> unit
        val mem : 'a t -> key -> bool
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
        val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
        val length : 'a t -> int
        val stats : 'a t -> Hashtbl.statistics
        val to_seq : 'a t -> (key * 'a) Seq.t
        val to_seq_keys : 'a t -> key Seq.t
        val to_seq_values : 'a t -> 'a Seq.t
        val add_seq : 'a t -> (key * 'a) Seq.t -> unit
        val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
        val of_seq : (key * 'a) Seq.t -> 'a t
      end

      type key = A2.key
      type 'a t = 'a A2.t

      val create : int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key -> 'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_opt : 'a t -> key -> 'a option
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key -> 'a -> unit
      val mem : 'a t -> key -> bool
      val iter : (key -> 'a -> unit) -> 'a t -> unit
      val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
      val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
      val length : 'a t -> int
      val stats : 'a t -> Hashtbl.statistics
      val to_seq : 'a t -> (key * 'a) Seq.t
      val to_seq_keys : 'a t -> key Seq.t
      val to_seq_values : 'a t -> 'a Seq.t
      val add_seq : 'a t -> (key * 'a) Seq.t -> unit
      val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
      val of_seq : (key * 'a) Seq.t -> 'a t

      type t' = D.t t

      val size : t' -> int
      val update : t' -> Model.Action.t -> D.t -> unit
      val to_string : t' -> string
    end

    module T : sig
      module T2 : sig
        type key = V.elt
        type !'a t

        val create : int -> 'a t
        val clear : 'a t -> unit
        val reset : 'a t -> unit
        val copy : 'a t -> 'a t
        val add : 'a t -> key -> 'a -> unit
        val remove : 'a t -> key -> unit
        val find : 'a t -> key -> 'a
        val find_opt : 'a t -> key -> 'a option
        val find_all : 'a t -> key -> 'a list
        val replace : 'a t -> key -> 'a -> unit
        val mem : 'a t -> key -> bool
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
        val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
        val length : 'a t -> int
        val stats : 'a t -> Hashtbl.statistics
        val to_seq : 'a t -> (key * 'a) Seq.t
        val to_seq_keys : 'a t -> key Seq.t
        val to_seq_values : 'a t -> 'a Seq.t
        val add_seq : 'a t -> (key * 'a) Seq.t -> unit
        val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
        val of_seq : (key * 'a) Seq.t -> 'a t
      end

      type key = T2.key
      type 'a t = 'a T2.t

      val create : int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key -> 'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_opt : 'a t -> key -> 'a option
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key -> 'a -> unit
      val mem : 'a t -> key -> bool
      val iter : (key -> 'a -> unit) -> 'a t -> unit
      val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
      val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
      val length : 'a t -> int
      val stats : 'a t -> Hashtbl.statistics
      val to_seq : 'a t -> (key * 'a) Seq.t
      val to_seq_keys : 'a t -> key Seq.t
      val to_seq_values : 'a t -> 'a Seq.t
      val add_seq : 'a t -> (key * 'a) Seq.t -> unit
      val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
      val of_seq : (key * 'a) Seq.t -> 'a t

      type t' = A.t' t

      val update : t' -> V.elt -> A.key -> D.t -> unit
      val size : t' -> int
      val to_string : t' -> string
    end

    type t =
      { to_visit : V.elt Queue.t
      ; init : V.elt
      ; states : V.t
      ; transitions : T.t'
      ; ind_defs : M.Ind.t M.B.t
      ; weak : Weak.t option
      }

    val empty : V0.elt -> M.Ind.t M.B.t -> t
    val is_silent_transition : EConstr.t -> Weak.t option -> bool option M.mm

    module type Y_Args = sig
      val primary_lts : M.Ind.t
      val rocq_defs : M.Ind.t M.B.t
      val stop : t -> bool
    end

    module type Z_Args = sig
      val pp : bool
      val g : t ref
      val ind_defs : M.Ind.t M.B.t
    end

    module Make : (_ : Y_Args) -> sig
      val next_to_visit : t -> T.key
      val update_to_visit : t -> T.key -> unit
      val update_states : t -> V.t -> t
      val get_new_constrs : T.key -> M.Constructor.t list M.mm
      val get_new_states : t -> T.key -> V.t M.mm
      val build : t -> t M.mm
    end

    module Extract : (_ : Z_Args) -> sig
      val pp : V.elt -> string option
      val state : V.elt -> Model.State.t
      val states : unit -> Model.Partition.elt
      val terminals : unit -> Model.Partition.elt
      val label : Model.Action.t -> Model.Label.t
      val transitions : unit -> Model.Transitions.t
      val constructor_info : unit -> Model.Info.lts list M.mm
      val meta : unit -> Model.Info.meta M.mm
      val weak_labels : Model.Labels.t -> Model.Labels.t M.mm
      val lts : unit -> Model.LTS.t M.mm
    end

    val build_ind_defs : unit -> M.Ind.t M.B.t M.mm
    val find_primary_lts : M.Ind.t M.B.t -> M.Ind.t M.mm
    val initial_term : Constrexpr.constr_expr -> EConstr.t M.mm
    val make_yargs : M.Ind.t -> M.Ind.t M.B.t -> 'a -> (module Y_Args)
    val make_zargs : M.Ind.t M.B.t -> t ref -> (module Z_Args)
    val build : Constrexpr.constr_expr -> Model.LTS.t M.mm
  end

  val make_xargs
    :  Libnames.qualid
    -> Names.GlobRef.t list
    -> Weak.t option
    -> (module X_Args)

  val fail_if_empty : Model.LTS.t -> unit
  val fail_if_incomplete : Model.LTS.t -> unit
  val fail_if_not_bisim : Model.Bisimilar.result -> unit

  val extract_lts
    :  Libnames.qualid
    -> Constrexpr.constr_expr
    -> Libnames.qualid list
    -> Weak.t option
    -> Model.LTS.t M.mm

  module Command : sig
    val default_weak_arg : Weak.t option -> Weak.t option

    val build_lts
      :  ?weak:Weak.t option
      -> Libnames.qualid
      -> Constrexpr.constr_expr
      -> Libnames.qualid list
      -> Model.LTS.t M.mm

    val build_fsm
      :  ?weak:Weak.t option
      -> Libnames.qualid
      -> Constrexpr.constr_expr
      -> Libnames.qualid list
      -> Model.FSM.t M.mm

    type t =
      | MakeLTS of rocq_args
      | MakeFSM of rocq_args
      | Saturate of rocq_args
      | Minimize of rocq_args
      | Merge of rocq_pair
      | CheckBisim of rocq_pair

    and rocq_args = Constrexpr.constr_expr * Libnames.qualid

    and rocq_pair =
      { a : rocq_args
      ; b : rocq_args
      }

    val build_fsms
      :  rocq_args
      -> rocq_args
      -> Libnames.qualid list
      -> (Model.FSM.t * Model.FSM.t) M.mm

    val run : Libnames.qualid list -> t -> Model.Bisimilar.t option M.mm
  end
end

module Default : () -> sig
  module M : sig
    module Ctx : sig
      val get : unit -> Rocq_context.t ref
      val env : unit -> Environ.env ref
      val sigma : unit -> Evd.evar_map ref
      val update : Environ.env ref -> Evd.evar_map ref -> unit
    end

    module Enc : sig
      type t

      val init : t
      val next : t -> t
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
      val to_string : t -> string
      val counter : t ref
      val reset : unit -> unit
      val incr : unit -> t
    end

    module F : sig
      type key = EConstr.t
      type 'a t = 'a Bi_encoding.Make(Logger.Default)(Ctx)(Enc).F.t

      val create : int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key -> 'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_opt : 'a t -> key -> 'a option
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key -> 'a -> unit
      val mem : 'a t -> key -> bool
      val iter : (key -> 'a -> unit) -> 'a t -> unit
      val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
      val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
      val length : 'a t -> int
      val stats : 'a t -> Hashtbl.statistics
      val to_seq : 'a t -> (key * 'a) Seq.t
      val to_seq_keys : 'a t -> key Seq.t
      val to_seq_values : 'a t -> 'a Seq.t
      val add_seq : 'a t -> (key * 'a) Seq.t -> unit
      val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
      val of_seq : (key * 'a) Seq.t -> 'a t
    end

    module B : sig
      type key = Enc.t
      type 'a t = 'a Bi_encoding.Make(Logger.Default)(Ctx)(Enc).B.t

      val create : int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key -> 'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_opt : 'a t -> key -> 'a option
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key -> 'a -> unit
      val mem : 'a t -> key -> bool
      val iter : (key -> 'a -> unit) -> 'a t -> unit
      val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
      val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
      val length : 'a t -> int
      val stats : 'a t -> Hashtbl.statistics
      val to_seq : 'a t -> (key * 'a) Seq.t
      val to_seq_keys : 'a t -> key Seq.t
      val to_seq_values : 'a t -> 'a Seq.t
      val add_seq : 'a t -> (key * 'a) Seq.t -> unit
      val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
      val of_seq : (key * 'a) Seq.t -> 'a t
    end

    type maps = Bi_encoding.Make(Logger.Default)(Ctx)(Enc).maps =
      { fwd : Enc.t F.t
      ; bck : EConstr.t B.t
      }

    val the_maps : maps ref option ref
    val reset : unit -> unit

    exception MapsNotInitialised of unit

    val get_the_maps : unit -> maps ref
    val fwdmap : unit -> Enc.t F.t
    val bckmap : unit -> EConstr.t B.t

    exception EncodingNotFound of EConstr.t

    val get_encoding : EConstr.t -> Enc.t
    val encode : EConstr.t -> Enc.t
    val encoded : EConstr.t -> bool

    exception DecodingNotFound of Enc.t

    val get_econstr : Enc.t -> EConstr.t

    exception CannotDecode of Enc.t

    val decode : Enc.t -> EConstr.t
    val decode_opt : Enc.t -> EConstr.t option
    val decode_map : 'a B.t -> 'a F.t
    val encode_map : 'a F.t -> 'a B.t
    val to_list : unit -> (Enc.t * EConstr.t) list
    val bienc_to_list : unit -> (Enc.t * EConstr.t) list

    type 'a mm = wrapper ref -> 'a in_wrapper

    and wrapper =
      { ctx : Rocq_context.t ref
      ; maps : maps ref
      }

    and 'a in_wrapper =
      { state : wrapper ref
      ; value : 'a
      }

    val run : ?reset_encoding:bool -> 'a mm -> 'a
    val return : 'a -> 'a mm
    val bind : 'a mm -> ('a -> 'b mm) -> 'b mm
    val map : ('a -> 'b) -> 'a mm -> 'b mm
    val product : 'a mm -> 'b mm -> ('a * 'b) mm
    val iterate : int -> int -> 'a -> (int -> 'a -> 'a mm) -> 'a mm

    val state
      :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
      -> wrapper ref
      -> 'a in_wrapper

    val sandbox : ?sigma:Evd.evar_map -> 'a mm -> wrapper ref -> 'a in_wrapper

    module type SYNTAX = sig
      val ( let+ ) : 'a mm -> ('a -> 'b) -> 'b mm
      val ( let* ) : 'a mm -> ('a -> 'b mm) -> 'b mm

      val ( let$ )
        :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
        -> ('a -> 'b mm)
        -> 'b mm

      val ( let$* )
        :  (Environ.env -> Evd.evar_map -> Evd.evar_map)
        -> (unit -> 'b mm)
        -> 'b mm

      val ( let$+ )
        :  (Environ.env -> Evd.evar_map -> 'a)
        -> ('a -> 'b mm)
        -> 'b mm

      val ( and+ ) : 'a mm -> 'b mm -> ('a * 'b) mm
    end

    module Syntax : sig
      val ( let+ ) : 'a mm -> ('a -> 'b) -> 'b mm
      val ( let* ) : 'a mm -> ('a -> 'b mm) -> 'b mm

      val ( let$ )
        :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
        -> ('a -> 'b mm)
        -> 'b mm

      val ( let$* )
        :  (Environ.env -> Evd.evar_map -> Evd.evar_map)
        -> (unit -> 'b mm)
        -> 'b mm

      val ( let$+ )
        :  (Environ.env -> Evd.evar_map -> 'a)
        -> ('a -> 'b mm)
        -> 'b mm

      val ( and+ ) : 'a mm -> 'b mm -> ('a * 'b) mm
    end

    val econstr_normalize : EConstr.t -> EConstr.t mm
    val get_ctx : wrapper ref -> Rocq_context.t in_wrapper
    val get_env : wrapper ref -> Environ.env in_wrapper
    val get_sigma : wrapper ref -> Evd.evar_map in_wrapper
    val get_maps : wrapper ref -> maps in_wrapper
    val get_fwdmap : wrapper ref -> Enc.t F.t in_wrapper
    val get_bckmap : wrapper ref -> EConstr.t B.t in_wrapper
    val fstring : (Environ.env -> Evd.evar_map -> 'a -> string) -> 'a -> string

    module Tree : sig
      module type STreeNode = sig
        type t = Enc.t * int

        val to_string : t -> string
      end

      module TreeNode : sig
        type t = Enc.t * int

        val to_string : t -> string
      end

      type 'a tree = 'a Enc_tree.Make(Enc).tree = Node of 'a * 'a tree list
      type t = TreeNode.t tree

      val add : t -> t -> t
      val add_list : t -> t list -> t list
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val minimize : t -> TreeNode.t list

      exception CannotMinimizeEmptyList of unit

      val min : t list -> TreeNode.t list
      val to_string : t -> string
      val list_to_string : ?args:Utils.Strfy.style_args -> t list -> string
    end

    module Constructor : sig
      type t = EConstr.t * EConstr.t * Tree.t

      val to_string : Environ.env -> Evd.evar_map -> t -> string
    end

    val make_state_tree_pair_set
      :  unit
      -> (module Set.S with type elt = Enc.t * Tree.t)

    val make_hashtbl : unit -> (module Hashtbl.S with type key = Enc.t)
    val make_set : unit -> (module Set.S with type elt = Enc.t)

    (* val make_econstr_set : unit -> (module Set.S with type elt = EConstr.t) *)
    val fresh_evar : Rocq_utils.evar_source -> EConstr.t mm
    val econstr_eq : EConstr.t -> EConstr.t -> bool
    val econstr_compare : EConstr.t -> EConstr.t -> int
    val econstr_kind : EConstr.t -> Rocq_utils.econstr_kind mm
    val econstr_is_evar : EConstr.t -> bool mm

    val econstr_to_constr
      :  ?abort_on_undefined_evars:bool
      -> EConstr.t
      -> Constr.t mm

    val econstr_to_constr_opt : EConstr.t -> Constr.t option mm
    val constrexpr_to_econstr : Constrexpr.constr_expr -> EConstr.t mm
    val exists_eq : EConstr.t -> 'a list -> ('a -> EConstr.t) -> bool
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

    module Errors : sig
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

    module Err : sig
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

    module Unification : sig
      module type SPair = sig
        type t =
          { to_check : EConstr.t
          ; acc : EConstr.t
          }

        val to_string : Environ.env -> Evd.evar_map -> t -> string

        val make
          :  Environ.env
          -> Evd.evar_map
          -> EConstr.t
          -> EConstr.t
          -> Evd.evar_map * t

        val unify : Environ.env -> Evd.evar_map -> t -> Evd.evar_map * bool
      end

      module Pair : sig
        type t =
          { to_check : EConstr.t
          ; acc : EConstr.t
          }

        val to_string : Environ.env -> Evd.evar_map -> t -> string

        val make
          :  Environ.env
          -> Evd.evar_map
          -> EConstr.t
          -> EConstr.t
          -> Evd.evar_map * t

        val unify : Environ.env -> Evd.evar_map -> t -> Evd.evar_map * bool
      end

      module type SProblem = sig
        type t =
          { act : Pair.t
          ; goto : Pair.t
          ; tree : Tree.t
          }

        val to_string : Environ.env -> Evd.evar_map -> t -> string
        val unify_opt : t -> Tree.t option mm
      end

      module Problem : sig
        type t =
          { act : Pair.t
          ; goto : Pair.t
          ; tree : Tree.t
          }

        val to_string : Environ.env -> Evd.evar_map -> t -> string
        val unify_opt : t -> Tree.t option mm
      end

      module type SProblems = sig
        type t =
          { sigma : Evd.evar_map
          ; to_unify : Problem.t list
          }

        val empty : unit -> t mm
        val list_is_empty : t list -> bool
        val to_string : Environ.env -> t -> string
        val list_to_string : Environ.env -> t list -> string

        val sandbox_unify_all_opt
          :  EConstr.t
          -> EConstr.t
          -> t
          -> (EConstr.t * EConstr.t * Tree.t list) option mm
      end

      module Problems : sig
        type t =
          { sigma : Evd.evar_map
          ; to_unify : Problem.t list
          }

        val empty : unit -> t mm
        val list_is_empty : t list -> bool
        val to_string : Environ.env -> t -> string
        val list_to_string : Environ.env -> t list -> string

        val sandbox_unify_all_opt
          :  EConstr.t
          -> EConstr.t
          -> t
          -> (EConstr.t * EConstr.t * Tree.t list) option mm
      end

      module type SConstructors = sig
        type t = Constructor.t list

        val to_string : Environ.env -> Evd.evar_map -> t -> string

        val retrieve
          :  int
          -> t
          -> EConstr.t
          -> EConstr.t
          -> Enc.t * Problems.t list
          -> t mm
      end

      module Constructors : sig
        type t = Constructor.t list

        val to_string : Environ.env -> Evd.evar_map -> t -> string

        val retrieve
          :  int
          -> t
          -> EConstr.t
          -> EConstr.t
          -> Enc.t * Problems.t list
          -> t mm
      end

      val constr_to_problem
        :  Rocq_utils.constructor_args
        -> Constructor.t
        -> Problem.t

      val map_problems
        :  Rocq_utils.constructor_args
        -> Constructors.t
        -> Problems.t mm

      val cross_product : Problems.t list -> Problems.t -> Problems.t list
      val does_constructor_unify : EConstr.t -> EConstr.t -> bool mm

      val check_constructor_args_unify
        :  EConstr.t
        -> EConstr.t
        -> Rocq_utils.constructor_args
        -> bool mm

      val axiom_constructor
        :  EConstr.t
        -> EConstr.t
        -> Enc.t * int
        -> Constructors.t
        -> Constructors.t mm

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
        -> Problems.t list
        -> Ind.t F.t
        -> EConstr.Vars.substl * EConstr.rel_declaration list
        -> (Enc.t * Problems.t list) option mm

      val check_for_next_constructors
        :  int
        -> EConstr.t
        -> EConstr.t
        -> Constructors.t
        -> (Enc.t * Problems.t list) option
        -> Constructors.t mm

      val collect_valid_constructors
        :  Rocq_ind.LTS.constructor array
        -> Ind.t F.t
        -> EConstr.t
        -> EConstr.t
        -> Enc.t
        -> Constructors.t mm
    end
  end

  module Model : sig
    module Tree : sig
      module type STreeNode = sig
        type t = M.Enc.t * int

        val to_string : t -> string
      end

      module TreeNode : sig
        type t = M.Enc.t * int

        val to_string : t -> string
      end

      type 'a tree = 'a Enc_tree.Make(M.Enc).tree = Node of 'a * 'a tree list
      type t = TreeNode.t tree

      val add : t -> t -> t
      val add_list : t -> t list -> t list
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val minimize : t -> TreeNode.t list

      exception CannotMinimizeEmptyList of unit

      val min : t list -> TreeNode.t list
      val to_string : t -> string
      val list_to_string : ?args:Utils.Strfy.style_args -> t list -> string
    end

    module Trees : sig
      type elt = Tree.t
      type t = Set.Make(Tree).t

      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t

      exception EmptyHasNoMin

      val min : t -> elt
      val min_opt : t -> elt option
    end

    module State : sig
      type t =
        { term : M.Enc.t
        ; pp : string option
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
      val to_string : ?pretty:bool -> t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module States : sig
      module S : sig
        type elt = State.t
        type t

        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end

      type elt = State.t
      type t = S.t

      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
      val add_to_opt : State.t -> t option -> t

      exception StateHasNoOrigin of (State.t * t * t)

      val origin_of_state : State.t -> t -> t -> int
      val has_shared_origin : t -> t -> t -> bool
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module Label : sig
      type t =
        { term : M.Enc.t
        ; pp : string option
        ; is_silent : bool option
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
      val is_silent : t -> bool
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module Labels : sig
      module S : sig
        type elt = Label.t
        type t

        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end

      type elt = Label.t
      type t = S.t

      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
      val non_silent : t -> t
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module Note : sig
      type t =
        { from : State.t
        ; label : Label.t
        ; using : Trees.t
        ; goto : State.t
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val is_silent : t -> bool
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module Annotation : sig
      type t =
        { this : Note.t
        ; next : t option
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val is_empty : t -> bool
      val length : t -> int
      val shorter : t -> t -> t
      val exists : Note.t -> t -> bool
      val exists_label : Label.t -> t -> bool
      val append : Note.t -> t -> t
      val last : t -> Note.t

      exception CannotDropLastOfSingleton of t

      val drop_last : t -> t
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module Annotations : sig
      module S : sig
        type elt = Annotation.t
        type t

        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end

      type elt = Annotation.t
      type t = S.t

      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
      val extrapolate : Annotation.t -> t
    end

    module Transition : sig
      type t =
        { from : State.t
        ; goto : State.t
        ; label : Label.t
        ; annotation : Annotation.t option
        ; constructor_tree : Tree.t option
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val is_silent : t -> bool
      val annotation_is_empty : t -> bool
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module Transitions : sig
      module S : sig
        type elt = Transition.t
        type t

        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end

      type elt = Transition.t
      type t = S.t

      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
      val labels : t -> Labels.t
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module Action : sig
      type t =
        { label : Label.t
        ; annotation : Annotation.t option
        ; constructor_trees : Trees.t
        }

      val wk_equal : t -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
      val is_silent : t -> bool
      val annotation_is_empty : t -> bool
      val annotation_length : t -> int
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module ActionPair : sig
      type t = Action.t * States.t

      val compare : t -> t -> int
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
      val try_update : t -> t list -> t option * t list
      val merge_lists : t list -> t list -> t list
    end

    module ActionPairs : sig
      module S : sig
        type elt = ActionPair.t
        type t = Set.Make(ActionPair).t

        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end

      type elt = ActionPair.t
      type t = Set.Make(ActionPair).t

      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
      val destinations : t -> States.t

      exception IsEmpty

      val shorest_annotation : t -> ActionPair.t
      val merge_list : t -> ActionPair.t list -> t
    end

    module Actions : sig
      module S : sig
        type elt = Action.t
        type t

        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end

      type elt = Action.t
      type t = S.t

      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
      val labelled : t -> Label.t -> t
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module ActionMap : sig
      module M : sig
        type key = Action.t
        type !'a t

        val create : int -> 'a t
        val clear : 'a t -> unit
        val reset : 'a t -> unit
        val copy : 'a t -> 'a t
        val add : 'a t -> key -> 'a -> unit
        val remove : 'a t -> key -> unit
        val find : 'a t -> key -> 'a
        val find_opt : 'a t -> key -> 'a option
        val find_all : 'a t -> key -> 'a list
        val replace : 'a t -> key -> 'a -> unit
        val mem : 'a t -> key -> bool
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
        val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
        val length : 'a t -> int
        val stats : 'a t -> Hashtbl.statistics
        val to_seq : 'a t -> (key * 'a) Seq.t
        val to_seq_keys : 'a t -> key Seq.t
        val to_seq_values : 'a t -> 'a Seq.t
        val add_seq : 'a t -> (key * 'a) Seq.t -> unit
        val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
        val of_seq : (key * 'a) Seq.t -> 'a t
      end

      type key = Action.t
      type 'a t = 'a M.t

      val create : int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key -> 'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_opt : 'a t -> key -> 'a option
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key -> 'a -> unit
      val mem : 'a t -> key -> bool
      val iter : (key -> 'a -> unit) -> 'a t -> unit
      val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
      val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
      val length : 'a t -> int
      val stats : 'a t -> Hashtbl.statistics
      val to_seq : 'a t -> (key * 'a) Seq.t
      val to_seq_keys : 'a t -> key Seq.t
      val to_seq_values : 'a t -> 'a Seq.t
      val add_seq : 'a t -> (key * 'a) Seq.t -> unit
      val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
      val of_seq : (key * 'a) Seq.t -> 'a t

      type t' = States.t t

      val update : t' -> Action.t -> States.t -> unit
      val destinations : t' -> States.t
      val reduce_by_label : t' -> Label.t -> t'
      val to_actions : t' -> Actions.t
      val to_actionpairs : t' -> ActionPairs.t
      val of_actionpairs : ActionPairs.t -> t'
      val merge : t' -> t' -> t'
      val to_string : t' -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t' -> unit
    end

    module Edge : sig
      type t =
        { from : State.t
        ; goto : State.t
        ; action : Action.t
        }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val is_silent : t -> bool
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module Edges : sig
      module S : sig
        type elt = Edge.t
        type t

        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end

      type elt = Edge.t
      type t = S.t

      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
      val labelled : t -> Label.t -> t
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module EdgeMap : sig
      module M : sig
        type key = State.t
        type !'a t

        val create : int -> 'a t
        val clear : 'a t -> unit
        val reset : 'a t -> unit
        val copy : 'a t -> 'a t
        val add : 'a t -> key -> 'a -> unit
        val remove : 'a t -> key -> unit
        val find : 'a t -> key -> 'a
        val find_opt : 'a t -> key -> 'a option
        val find_all : 'a t -> key -> 'a list
        val replace : 'a t -> key -> 'a -> unit
        val mem : 'a t -> key -> bool
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
        val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
        val length : 'a t -> int
        val stats : 'a t -> Hashtbl.statistics
        val to_seq : 'a t -> (key * 'a) Seq.t
        val to_seq_keys : 'a t -> key Seq.t
        val to_seq_values : 'a t -> 'a Seq.t
        val add_seq : 'a t -> (key * 'a) Seq.t -> unit
        val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
        val of_seq : (key * 'a) Seq.t -> 'a t
      end

      type key = State.t
      type 'a t = 'a M.t

      val create : int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key -> 'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_opt : 'a t -> key -> 'a option
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key -> 'a -> unit
      val mem : 'a t -> key -> bool
      val iter : (key -> 'a -> unit) -> 'a t -> unit
      val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
      val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
      val length : 'a t -> int
      val stats : 'a t -> Hashtbl.statistics
      val to_seq : 'a t -> (key * 'a) Seq.t
      val to_seq_keys : 'a t -> key Seq.t
      val to_seq_values : 'a t -> 'a Seq.t
      val add_seq : 'a t -> (key * 'a) Seq.t -> unit
      val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
      val of_seq : (key * 'a) Seq.t -> 'a t

      type t' = ActionMap.t' t

      val update : t' -> State.t -> Action.t -> States.t -> unit
      val destinations : t' -> State.t -> States.t
      val get_actions : t' -> State.t -> Actions.t
      val reduce_by_label : t' -> Label.t -> t'
      val get_edges : t' -> State.t -> Edges.t
      val to_edges : t' -> Edges.t
      val of_edges : Edges.t -> t'
      val merge : t' -> t' -> t'
      val to_string : t' -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t' -> unit
    end

    module Partition : sig
      module S : sig
        type elt = States.t
        type t

        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end

      type elt = S.elt
      type t = S.t

      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
      val get_bisimilar : State.t -> t -> States.t
      val filter_reachable : States.t -> t -> t
      val reachable : State.t -> EdgeMap.t' -> t -> t
      val reachable_by_label : State.t -> Label.t -> EdgeMap.t' -> t -> t
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module Info : sig
      type t =
        { meta : meta option
        ; weak_labels : Labels.t
        }

      and meta =
        { is_complete : bool
        ; is_merged : bool
        ; bounds : bounds
        ; lts : lts list
        }

      and bounds =
        | States of int
        | Transitions of int

      and lts =
        { enc : M.Enc.t
        ; constructors : Rocq_bindings.constructor list
        }

      val merge : t -> t -> t
      val to_string : t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end

    module LTS : sig
      type t =
        { init : State.t option
        ; terminals : Partition.elt
        ; alphabet : Labels.t
        ; states : Partition.elt
        ; transitions : Transitions.t
        ; info : Info.t
        }

      val to_string : t -> string
    end

    module FSM : sig
      type t =
        { init : State.t option
        ; terminals : Partition.elt
        ; alphabet : Labels.t
        ; states : Partition.elt
        ; edges : EdgeMap.t'
        ; info : Info.t
        }

      val merge : t -> t -> t
      val is_weak_mode : t -> bool
      val to_string : t -> string
    end

    module Convert : sig
      val transitions_to_edgemap : Transitions.t -> EdgeMap.t'
      val lts_to_fsm : LTS.t -> FSM.t
    end

    module Saturate : sig
      module Log : Logger.SLogger

      module WIP : sig
        type t =
          { from : State.t
          ; via : Label.t
          ; trees : Trees.t
          }

        val is_silent : t -> bool
        val is_named : t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val create : State.t -> Action.t -> t

        exception IsEmptyList

        val list_to_annotation : State.t -> t list -> Annotation.t
      end

      module Trace : sig
        module NT : sig end

        type t =
          { this : WIP.t
          ; next : next option
          }

        and next =
          | Next of t
          | Goto of State.t

        val create : WIP.t -> t
        val compare : t -> t -> int
        val compare_next : next -> next -> int

        exception Invalid

        val has_named : ?validate:bool -> t -> bool
        val validate : t -> unit

        exception CouldNotFindGoto

        val get_goto : t -> State.t

        exception CouldNotFindNamed

        val get_named : t -> Label.t
        val get_named_opt : t -> Label.t option

        exception FailAdd_AlreadyNamed
        exception FailAdd_AlreadyHasGoto

        val add : WIP.t -> t -> t

        exception FailSetGoto_AlreadyHasGoto

        val set_goto : State.t -> t -> t

        exception FailSeq_AlreadyNamed
        exception FailSeq_AlreadyHasGoto

        val seq : t -> t -> t
        val seq_opt : t option -> t -> t
        val get : WIP.t -> t -> t
        val upto_named : t -> t

        exception GotoNotSet

        val to_annotation : t -> Annotation.t
      end

      module Traces : sig
        type elt = Trace.t
        type t = Set.Make(Trace).t

        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
        val get : WIP.t -> t -> t
      end

      type data =
        { named : Label.t option
        ; current : Trace.t option
        ; visited : States.t
        ; traces : Traces.t ref
        ; can_collect_traces : bool ref
        ; old_edges : EdgeMap.t'
        }

      val initial_data : Traces.t ref -> EdgeMap.t' -> data
      val has_named : data -> bool
      val update_traces : data -> Trace.t -> unit
      val update_named : Action.t -> data -> data
      val update_current : WIP.t -> data -> data
      val update_visited : State.t -> data -> data
      val already_visited : State.t -> data -> bool
      val skip_action : Action.t -> data -> bool
      val get_old_actions : State.t -> data -> ActionMap.t' option
      val update_acc : Trace.t -> Label.t -> ActionPairs.t -> ActionPairs.t
      val stop : data -> State.t -> ActionPairs.t -> ActionPairs.t

      val finish_with_trace
        :  Trace.t
        -> data
        -> Label.t
        -> ActionPairs.t
        -> ActionPairs.t

      val finish_with_trace_upto
        :  Trace.t
        -> data
        -> Label.t
        -> ActionPairs.t
        -> ActionPairs.t

      val check_from : data -> State.t -> ActionPairs.t -> ActionPairs.t

      val check_actions
        :  data
        -> State.t
        -> ActionMap.t'
        -> ActionPairs.t
        -> ActionPairs.t

      val collect_from_traces
        :  data
        -> State.t
        -> Action.t
        -> States.t
        -> ActionPairs.t
        -> ActionPairs.t

      val continue_check_destinations
        :  data
        -> State.t
        -> Action.t
        -> States.t
        -> ActionPairs.t
        -> ActionPairs.t

      val check_destinations
        :  data
        -> State.t
        -> States.t
        -> ActionPairs.t
        -> ActionPairs.t

      val edge_action_destinations
        :  data
        -> State.t
        -> States.t
        -> ActionPairs.t

      val edge_actions
        :  State.t
        -> ActionMap.t'
        -> EdgeMap.t'
        -> Traces.t ref
        -> ActionPairs.t

      val edge
        :  ActionMap.t'
        -> State.t
        -> ActionMap.t'
        -> EdgeMap.t'
        -> Traces.t ref
        -> unit

      val edges : Labels.t -> States.t -> EdgeMap.t' -> EdgeMap.t'
      val fsm : ?only_if_weak:bool -> FSM.t -> FSM.t
    end

    module Minimize : sig
      type t =
        { fsm : FSM.t
        ; pi : Partition.t
        }

      exception CannotSplitEmptyBlock of unit

      val ensure_nonempty : States.t -> unit

      val split_block
        :  Partition.t
        -> State.t
        -> EdgeMap.t'
        -> States.t
        -> States.t * States.t option

      exception Split_OnlyReturnedOneBlock_ButNeqBlock of (States.t * States.t)

      val ensure_equal : States.t -> States.t -> unit

      val for_each_label
        :  Partition.t ref
        -> bool ref
        -> EdgeMap.t'
        -> States.t ref
        -> Label.t
        -> unit

      val for_each_block
        :  Partition.t ref
        -> bool ref
        -> Labels.t
        -> EdgeMap.t'
        -> States.t
        -> unit

      val partition_states : FSM.t -> Partition.t
      val fsm : FSM.t -> t
      val to_string : t -> string
    end

    module Bisimilar : sig
      type t =
        { fsm_a : fsm_pair
        ; fsm_b : fsm_pair
        ; merged : FSM.t
        ; result : result
        }

      and result =
        { bisim_states : Partition.t
        ; non_bisim_states : Partition.t
        }

      and fsm_pair =
        { original : FSM.t
        ; saturated : FSM.t
        }

      val fsm_pair : FSM.t -> fsm_pair
      val are_bisimilar : result -> bool
      val the_cached_result : t option ref
      val set_the_result : t -> unit

      exception NoCachedResult of unit

      val get_the_result : unit -> t
      val split : Partition.t -> States.t -> States.t -> result
      val fsm : FSM.t -> FSM.t -> t
      val fsm_pair_to_string : fsm_pair -> string
      val result_to_string : result -> string
      val to_string : t -> string
    end
  end

  module IsTheory : sig
    val is_theory : EConstr.t -> EConstr.t -> bool M.mm
    val is_exists : EConstr.t -> bool M.mm
    val is_weak_sim : EConstr.t -> bool M.mm
    val is_weak : EConstr.t -> bool M.mm
    val is_tau : EConstr.t -> bool M.mm
    val is_silent : EConstr.t -> bool M.mm
    val is_silent1 : EConstr.t -> bool M.mm
    val is_LTS : EConstr.t -> bool M.mm
    val is_None : EConstr.t -> bool M.mm
    val is_Some : EConstr.t -> bool M.mm
    val get_theory_enc : (EConstr.t -> bool M.mm) -> M.Enc.t M.mm

    exception NoEncodingFoundFor_TheoriesNone of unit

    val get_None_enc : unit -> M.Enc.t M.mm

    exception NoEncodingFoundFor_TheoriesSome of unit

    val get_Some_enc : unit -> M.Enc.t M.mm

    exception NotEqTheory of unit

    val get_theory_enc_if_eq
      :  EConstr.t
      -> (EConstr.t -> bool M.mm)
      -> M.Enc.t M.mm

    val get_None_enc_if_eq : EConstr.t -> M.Enc.t M.mm
    val get_Some_enc_if_eq : EConstr.t -> M.Enc.t M.mm
  end

  module Weak : sig
    type t =
      | Option of M.Enc.t
      | Custom of M.Enc.t * M.Enc.t

    val eq : t -> t -> bool
    val to_string : t -> string M.mm
  end

  module Config : sig
    val load_weak_arg : Api.weak_arg -> Weak.t M.mm
    val load_weak_arg_opt : Api.weak_arg option -> Weak.t option M.mm

    type weak_args =
      { a : Weak.t option
      ; b : Weak.t option
      }

    val the_weak_args : weak_args ref option ref
    val reset_the_weak_args : unit -> unit
    val load_weak_args : unit -> unit M.mm
    val get_the_weak_args : unit -> weak_args option
    val get_the_weak_arg1 : unit -> Weak.t option
    val get_the_weak_arg2 : unit -> Weak.t option
    val api_bounds_to_model_bounds : Api.bounds_args -> Model.Info.bounds
    val the_bounds_args : Model.Info.bounds ref
    val load_the_bounds_args : unit -> unit
  end

  module type X_Args = sig
    val primary_lts : Libnames.qualid
    val grefs : Names.GlobRef.t list
    val weak : Weak.t option
    val bounds : Model.Info.bounds
  end

  module Graph : (T0 : sig
                    type key = M.Enc.t
                    type !'a t

                    val create : int -> 'a t
                    val clear : 'a t -> unit
                    val reset : 'a t -> unit
                    val copy : 'a t -> 'a t
                    val add : 'a t -> key -> 'a -> unit
                    val remove : 'a t -> key -> unit
                    val find : 'a t -> key -> 'a
                    val find_opt : 'a t -> key -> 'a option
                    val find_all : 'a t -> key -> 'a list
                    val replace : 'a t -> key -> 'a -> unit
                    val mem : 'a t -> key -> bool
                    val iter : (key -> 'a -> unit) -> 'a t -> unit

                    val filter_map_inplace
                      :  (key -> 'a -> 'a option)
                      -> 'a t
                      -> unit

                    val fold
                      :  (key -> 'a -> 'acc -> 'acc)
                      -> 'a t
                      -> 'acc
                      -> 'acc

                    val length : 'a t -> int
                    val stats : 'a t -> Hashtbl.statistics
                    val to_seq : 'a t -> (key * 'a) Seq.t
                    val to_seq_keys : 'a t -> key Seq.t
                    val to_seq_values : 'a t -> 'a Seq.t
                    val add_seq : 'a t -> (key * 'a) Seq.t -> unit
                    val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
                    val of_seq : (key * 'a) Seq.t -> 'a t
                  end)
      (V0 : sig
         type elt = T0.key
         type t

         val empty : t
         val add : elt -> t -> t
         val singleton : elt -> t
         val remove : elt -> t -> t
         val union : t -> t -> t
         val inter : t -> t -> t
         val disjoint : t -> t -> bool
         val diff : t -> t -> t
         val cardinal : t -> int
         val elements : t -> elt list
         val min_elt : t -> elt
         val min_elt_opt : t -> elt option
         val max_elt : t -> elt
         val max_elt_opt : t -> elt option
         val choose : t -> elt
         val choose_opt : t -> elt option
         val find : elt -> t -> elt
         val find_opt : elt -> t -> elt option
         val find_first : (elt -> bool) -> t -> elt
         val find_first_opt : (elt -> bool) -> t -> elt option
         val find_last : (elt -> bool) -> t -> elt
         val find_last_opt : (elt -> bool) -> t -> elt option
         val iter : (elt -> unit) -> t -> unit
         val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
         val map : (elt -> elt) -> t -> t
         val filter : (elt -> bool) -> t -> t
         val filter_map : (elt -> elt option) -> t -> t
         val partition : (elt -> bool) -> t -> t * t
         val split : elt -> t -> t * bool * t
         val is_empty : t -> bool
         val mem : elt -> t -> bool
         val equal : t -> t -> bool
         val compare : t -> t -> int
         val subset : t -> t -> bool
         val for_all : (elt -> bool) -> t -> bool
         val exists : (elt -> bool) -> t -> bool
         val to_list : t -> elt list
         val of_list : elt list -> t
         val to_seq_from : elt -> t -> elt Seq.t
         val to_seq : t -> elt Seq.t
         val to_rev_seq : t -> elt Seq.t
         val add_seq : elt Seq.t -> t -> t
         val of_seq : elt Seq.t -> t
       end)
      (_ : sig
         type elt = V0.elt * M.Tree.t
         type t

         val empty : t
         val add : elt -> t -> t
         val singleton : elt -> t
         val remove : elt -> t -> t
         val union : t -> t -> t
         val inter : t -> t -> t
         val disjoint : t -> t -> bool
         val diff : t -> t -> t
         val cardinal : t -> int
         val elements : t -> elt list
         val min_elt : t -> elt
         val min_elt_opt : t -> elt option
         val max_elt : t -> elt
         val max_elt_opt : t -> elt option
         val choose : t -> elt
         val choose_opt : t -> elt option
         val find : elt -> t -> elt
         val find_opt : elt -> t -> elt option
         val find_first : (elt -> bool) -> t -> elt
         val find_first_opt : (elt -> bool) -> t -> elt option
         val find_last : (elt -> bool) -> t -> elt
         val find_last_opt : (elt -> bool) -> t -> elt option
         val iter : (elt -> unit) -> t -> unit
         val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
         val map : (elt -> elt) -> t -> t
         val filter : (elt -> bool) -> t -> t
         val filter_map : (elt -> elt option) -> t -> t
         val partition : (elt -> bool) -> t -> t * t
         val split : elt -> t -> t * bool * t
         val is_empty : t -> bool
         val mem : elt -> t -> bool
         val equal : t -> t -> bool
         val compare : t -> t -> int
         val subset : t -> t -> bool
         val for_all : (elt -> bool) -> t -> bool
         val exists : (elt -> bool) -> t -> bool
         val to_list : t -> elt list
         val of_list : elt list -> t
         val to_seq_from : elt -> t -> elt Seq.t
         val to_seq : t -> elt Seq.t
         val to_rev_seq : t -> elt Seq.t
         val add_seq : elt Seq.t -> t -> t
         val of_seq : elt Seq.t -> t
       end)
      (_ : X_Args)
      -> sig
    module V : sig
      module V2 : sig
        type elt = V0.elt
        type t

        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end

      type elt = V2.elt
      type t = V2.t

      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
      val to_string : t -> string
    end

    module D : sig
      module D2 : sig
        type elt = V.elt * M.Tree.t
        type t

        val empty : t
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val disjoint : t -> t -> bool
        val diff : t -> t -> t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val iter : (elt -> unit) -> t -> unit
        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
        val map : (elt -> elt) -> t -> t
        val filter : (elt -> bool) -> t -> t
        val filter_map : (elt -> elt option) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val split : elt -> t -> t * bool * t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val subset : t -> t -> bool
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val to_list : t -> elt list
        val of_list : elt list -> t
        val to_seq_from : elt -> t -> elt Seq.t
        val to_seq : t -> elt Seq.t
        val to_rev_seq : t -> elt Seq.t
        val add_seq : elt Seq.t -> t -> t
        val of_seq : elt Seq.t -> t
      end

      type elt = V.elt * M.Tree.t
      type t = D2.t

      val empty : t
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val disjoint : t -> t -> bool
      val diff : t -> t -> t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val iter : (elt -> unit) -> t -> unit
      val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
      val map : (elt -> elt) -> t -> t
      val filter : (elt -> bool) -> t -> t
      val filter_map : (elt -> elt option) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val split : elt -> t -> t * bool * t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val subset : t -> t -> bool
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val to_list : t -> elt list
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val to_rev_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
      val to_string : t -> string
    end

    module A : sig
      module A2 : sig
        type key = Model.Action.t
        type !'a t

        val create : int -> 'a t
        val clear : 'a t -> unit
        val reset : 'a t -> unit
        val copy : 'a t -> 'a t
        val add : 'a t -> key -> 'a -> unit
        val remove : 'a t -> key -> unit
        val find : 'a t -> key -> 'a
        val find_opt : 'a t -> key -> 'a option
        val find_all : 'a t -> key -> 'a list
        val replace : 'a t -> key -> 'a -> unit
        val mem : 'a t -> key -> bool
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
        val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
        val length : 'a t -> int
        val stats : 'a t -> Hashtbl.statistics
        val to_seq : 'a t -> (key * 'a) Seq.t
        val to_seq_keys : 'a t -> key Seq.t
        val to_seq_values : 'a t -> 'a Seq.t
        val add_seq : 'a t -> (key * 'a) Seq.t -> unit
        val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
        val of_seq : (key * 'a) Seq.t -> 'a t
      end

      type key = A2.key
      type 'a t = 'a A2.t

      val create : int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key -> 'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_opt : 'a t -> key -> 'a option
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key -> 'a -> unit
      val mem : 'a t -> key -> bool
      val iter : (key -> 'a -> unit) -> 'a t -> unit
      val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
      val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
      val length : 'a t -> int
      val stats : 'a t -> Hashtbl.statistics
      val to_seq : 'a t -> (key * 'a) Seq.t
      val to_seq_keys : 'a t -> key Seq.t
      val to_seq_values : 'a t -> 'a Seq.t
      val add_seq : 'a t -> (key * 'a) Seq.t -> unit
      val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
      val of_seq : (key * 'a) Seq.t -> 'a t

      type t' = D.t t

      val size : t' -> int
      val update : t' -> Model.Action.t -> D.t -> unit
      val to_string : t' -> string
    end

    module T : sig
      module T2 : sig
        type key = V.elt
        type !'a t

        val create : int -> 'a t
        val clear : 'a t -> unit
        val reset : 'a t -> unit
        val copy : 'a t -> 'a t
        val add : 'a t -> key -> 'a -> unit
        val remove : 'a t -> key -> unit
        val find : 'a t -> key -> 'a
        val find_opt : 'a t -> key -> 'a option
        val find_all : 'a t -> key -> 'a list
        val replace : 'a t -> key -> 'a -> unit
        val mem : 'a t -> key -> bool
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
        val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
        val length : 'a t -> int
        val stats : 'a t -> Hashtbl.statistics
        val to_seq : 'a t -> (key * 'a) Seq.t
        val to_seq_keys : 'a t -> key Seq.t
        val to_seq_values : 'a t -> 'a Seq.t
        val add_seq : 'a t -> (key * 'a) Seq.t -> unit
        val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
        val of_seq : (key * 'a) Seq.t -> 'a t
      end

      type key = T2.key
      type 'a t = 'a T2.t

      val create : int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key -> 'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_opt : 'a t -> key -> 'a option
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key -> 'a -> unit
      val mem : 'a t -> key -> bool
      val iter : (key -> 'a -> unit) -> 'a t -> unit
      val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
      val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
      val length : 'a t -> int
      val stats : 'a t -> Hashtbl.statistics
      val to_seq : 'a t -> (key * 'a) Seq.t
      val to_seq_keys : 'a t -> key Seq.t
      val to_seq_values : 'a t -> 'a Seq.t
      val add_seq : 'a t -> (key * 'a) Seq.t -> unit
      val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
      val of_seq : (key * 'a) Seq.t -> 'a t

      type t' = A.t' t

      val update : t' -> V.elt -> A.key -> D.t -> unit
      val size : t' -> int
      val to_string : t' -> string
    end

    type t =
      { to_visit : V.elt Queue.t
      ; init : V.elt
      ; states : V.t
      ; transitions : T.t'
      ; ind_defs : M.Ind.t M.B.t
      ; weak : Weak.t option
      }

    val empty : V0.elt -> M.Ind.t M.B.t -> t
    val _log_to_visit : t -> unit
    val _log_ind_defs : M.Ind.t M.B.t -> unit
    val is_silent_transition : EConstr.t -> Weak.t option -> bool option M.mm

    module type Y_Args = sig
      val primary_lts : M.Ind.t
      val rocq_defs : M.Ind.t M.B.t
      val stop : t -> bool
    end

    module type Z_Args = sig
      val pp : bool
      val g : t ref
      val ind_defs : M.Ind.t M.B.t
    end

    module Make : (_ : Y_Args) -> sig
      val next_to_visit : t -> T.key
      val update_to_visit : t -> T.key -> unit
      val update_states : t -> V.t -> t
      val get_new_constrs : T.key -> M.Constructor.t list M.mm
      val get_new_states : t -> T.key -> V.t M.mm
      val build : t -> t M.mm
    end

    module Extract : (_ : Z_Args) -> sig
      val pp : V.elt -> string option
      val state : V.elt -> Model.State.t
      val states : unit -> Model.Partition.elt
      val terminals : unit -> Model.Partition.elt
      val label : A.key -> Model.Label.t
      val transitions : unit -> Model.Transitions.t
      val constructor_info : unit -> Model.Info.lts list M.mm
      val meta : unit -> Model.Info.meta M.mm
      val weak_labels : Model.Labels.t -> Model.Labels.t M.mm
      val lts : unit -> Model.LTS.t M.mm
    end

    val build_ind_defs : unit -> M.Ind.t M.B.t M.mm
    val find_primary_lts : M.Ind.t M.B.t -> M.Ind.t M.mm
    val initial_term : Constrexpr.constr_expr -> EConstr.t M.mm
    val make_yargs : M.Ind.t -> M.Ind.t M.B.t -> 'a -> (module Y_Args)
    val make_zargs : M.Ind.t M.B.t -> t ref -> (module Z_Args)
    val build : Constrexpr.constr_expr -> Model.LTS.t M.mm
  end

  val make_xargs
    :  Libnames.qualid
    -> Names.GlobRef.t list
    -> Weak.t option
    -> (module X_Args)

  val fail_if_empty : Model.LTS.t -> unit
  val fail_if_incomplete : Model.LTS.t -> unit
  val fail_if_not_bisim : Model.Bisimilar.result -> unit

  val extract_lts
    :  Libnames.qualid
    -> Constrexpr.constr_expr
    -> Libnames.qualid list
    -> Weak.t option
    -> Model.LTS.t M.mm

  module Command : sig
    val default_weak_arg : Weak.t option -> Weak.t option

    val build_lts
      :  ?weak:Weak.t option
      -> Libnames.qualid
      -> Constrexpr.constr_expr
      -> Libnames.qualid list
      -> Model.LTS.t M.mm

    val build_fsm
      :  ?weak:Weak.t option
      -> Libnames.qualid
      -> Constrexpr.constr_expr
      -> Libnames.qualid list
      -> Model.FSM.t M.mm

    type t =
      | MakeLTS of rocq_args
      | MakeFSM of rocq_args
      | Saturate of rocq_args
      | Minimize of rocq_args
      | Merge of rocq_pair
      | CheckBisim of rocq_pair

    and rocq_args = Constrexpr.constr_expr * Libnames.qualid

    and rocq_pair =
      { a : rocq_args
      ; b : rocq_args
      }

    val do_make_lts
      :  Constrexpr.constr_expr * Libnames.qualid
      -> Libnames.qualid list
      -> Model.Bisimilar.t option M.mm

    val do_make_fsm
      :  Constrexpr.constr_expr * Libnames.qualid
      -> Libnames.qualid list
      -> Model.Bisimilar.t option M.mm

    val do_saturate
      :  Constrexpr.constr_expr * Libnames.qualid
      -> Libnames.qualid list
      -> Model.Bisimilar.t option M.mm

    val do_minimize
      :  Constrexpr.constr_expr * Libnames.qualid
      -> Libnames.qualid list
      -> Model.Bisimilar.t option M.mm

    val build_fsms
      :  rocq_args
      -> rocq_args
      -> Libnames.qualid list
      -> (Model.FSM.t * Model.FSM.t) M.mm

    val do_merge
      :  rocq_pair
      -> Libnames.qualid list
      -> Model.Bisimilar.t option M.mm

    val do_check_bisim
      :  rocq_pair
      -> Libnames.qualid list
      -> Model.Bisimilar.t option M.mm

    val run : Libnames.qualid list -> t -> Model.Bisimilar.t option M.mm
  end
end
