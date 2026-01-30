module type S = sig
  val gl : Proofview.Goal.t ref
end

module ProofSolver : (_ : S)
  (E : Encoding.SEncoding)
  -> sig
  val gl : Proofview.Goal.t ref

  module W : sig
    module M : sig
      module Ctx : sig
        val get : unit -> Rocq_context.t ref
        val env : unit -> Environ.env ref
        val sigma : unit -> Evd.evar_map ref

        val update :
          Environ.env ref -> Evd.evar_map ref -> unit
      end

      module Enc : sig
        type t = Rocq_monad.Make(Rocq_context.Default)(E).Enc.t

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
        type key = Evd.econstr

        type 'a t =
          'a Bi_encoding.Make(Ctx)(Enc).F.t

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

        val filter_map_inplace :
          (key -> 'a -> 'a option) -> 'a t -> unit

        val fold :
          (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

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

        type 'a t =
          'a Bi_encoding.Make(Ctx)(Enc).B.t

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

        val filter_map_inplace :
          (key -> 'a -> 'a option) -> 'a t -> unit

        val fold :
          (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

        val length : 'a t -> int
        val stats : 'a t -> Hashtbl.statistics
        val to_seq : 'a t -> (key * 'a) Seq.t
        val to_seq_keys : 'a t -> key Seq.t
        val to_seq_values : 'a t -> 'a Seq.t
        val add_seq : 'a t -> (key * 'a) Seq.t -> unit
        val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
        val of_seq : (key * 'a) Seq.t -> 'a t
      end

      type maps =
            Bi_encoding.Make(Ctx)(Enc).maps = {
        fwd : B.key F.t;
        bck : Evd.econstr B.t;
      }

      val the_maps : unit -> maps ref
      val reset : unit -> unit
      val fwdmap : unit -> Enc.t F.t
      val bckmap : unit -> Evd.econstr B.t
      val get_encoding : Evd.econstr -> Enc.t
      val encode : Evd.econstr -> Enc.t
      val encoded : Evd.econstr -> bool
      val get_econstr : Enc.t -> Evd.econstr

      exception CannotDecode of Enc.t

      val decode : Enc.t -> Evd.econstr
      val decode_opt : Enc.t -> Evd.econstr option
      val decode_map : 'a B.t -> 'a F.t
      val encode_map : 'a F.t -> 'a B.t
      val to_list : unit -> (Enc.t * Evd.econstr) list

      val make_hashtbl :
        (module Hashtbl.S with type key = Enc.t)

      val make_set : (module Set.S with type elt = Enc.t)
      val bienc_to_list : unit -> (Enc.t * Evd.econstr) list

      type 'a mm = wrapper ref -> 'a in_wrapper

      and wrapper = {
        ctx : Rocq_context.t ref;
        maps : maps ref;
      }

      and 'a in_wrapper = {
        state : wrapper ref;
        value : 'a;
      }

      val run : ?reset_encoding:bool -> 'a mm -> 'a
      val return : 'a -> 'a mm
      val bind : 'a mm -> ('a -> 'b mm) -> 'b mm
      val map : ('a -> 'b) -> 'a mm -> 'b mm
      val product : 'a mm -> 'b mm -> ('a * 'b) mm

      val iterate :
        int -> int -> 'a -> (int -> 'a -> 'a mm) -> 'a mm

      val state :
        (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a) ->
        wrapper ref ->
        'a in_wrapper

      val sandbox :
        ?sigma:Evd.evar_map ->
        'a mm ->
        wrapper ref ->
        'a in_wrapper

      module type SYNTAX = sig
        val ( let+ ) : 'a mm -> ('a -> 'b) -> 'b mm
        val ( let* ) : 'a mm -> ('a -> 'b mm) -> 'b mm

        val ( let$ ) :
          (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a) ->
          ('a -> 'b mm) ->
          'b mm

        val ( let$* ) :
          (Environ.env -> Evd.evar_map -> Evd.evar_map) ->
          (unit -> 'b mm) ->
          'b mm

        val ( let$+ ) :
          (Environ.env -> Evd.evar_map -> 'a) ->
          ('a -> 'b mm) ->
          'b mm

        val ( and+ ) : 'a mm -> 'b mm -> ('a * 'b) mm
      end

      module Syntax : sig
        val ( let+ ) : 'a mm -> ('a -> 'b) -> 'b mm
        val ( let* ) : 'a mm -> ('a -> 'b mm) -> 'b mm

        val ( let$ ) :
          (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a) ->
          ('a -> 'b mm) ->
          'b mm

        val ( let$* ) :
          (Environ.env -> Evd.evar_map -> Evd.evar_map) ->
          (unit -> 'b mm) ->
          'b mm

        val ( let$+ ) :
          (Environ.env -> Evd.evar_map -> 'a) ->
          ('a -> 'b mm) ->
          'b mm

        val ( and+ ) : 'a mm -> 'b mm -> ('a * 'b) mm
      end

      val get_ctx : wrapper ref -> Rocq_context.t in_wrapper
      val get_env : wrapper ref -> Environ.env in_wrapper
      val get_sigma : wrapper ref -> Evd.evar_map in_wrapper
      val get_maps : wrapper ref -> maps in_wrapper
      val get_fwdmap : wrapper ref -> Enc.t F.t in_wrapper

      val get_bckmap :
        wrapper ref -> Evd.econstr B.t in_wrapper

      val fstring :
        (Environ.env -> Evd.evar_map -> 'a -> string) ->
        'a ->
        string

      module Tree : sig
        module type STreeNode = sig
          type t = Enc.t * int

          val to_string : t -> string
        end

        module TreeNode : sig
          type t = Enc.t * int

          val to_string : t -> string
        end

        type 'a tree = 'a Enc_tree.Make(Enc).tree =
          | Node of 'a * 'a tree list

        type t = TreeNode.t tree

        val add : t -> t -> t
        val add_list : t -> t list -> t list
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val minimize : t -> TreeNode.t list

        exception CannotMinimizeEmptyList of unit

        val min : t list -> TreeNode.t list
        val to_string : t -> string

        val list_to_string :
          ?args:Utils.Strfy.style_args -> t list -> string
      end

      module Constructor : sig
        type t = Evd.econstr * Evd.econstr * Tree.t

        val to_string :
          Environ.env -> Evd.evar_map -> t -> string
      end

      val make_state_tree_pair_set :
        (module Set.S with type elt = Enc.t * Tree.t)

      val fresh_evar : Rocq_utils.evar_source -> Evd.econstr mm
      val econstr_eq : Evd.econstr -> Evd.econstr -> bool mm
      val econstr_normalize : Evd.econstr -> Evd.econstr mm

      val econstr_kind :
        Evd.econstr -> Rocq_utils.econstr_kind mm

      val econstr_is_evar : Evd.econstr -> bool mm

      val econstr_to_constr :
        ?abort_on_undefined_evars:bool ->
        Evd.econstr ->
        Constr.t mm

      val econstr_to_constr_opt :
        Evd.econstr -> Constr.t option mm

      val constrexpr_to_econstr :
        Constrexpr.constr_expr -> Evd.econstr mm

      val exists_eq :
        Evd.econstr ->
        'a list ->
        ('a -> Evd.econstr) ->
        bool mm

      module Strfy : sig
        val econstr : Evd.econstr -> string

        val econstr_rel_decl :
          EConstr.rel_declaration -> string

        val hyp_name : Rocq_utils.hyp -> string
        val hyp_type : Rocq_utils.hyp -> string
        val hyp_value : Rocq_utils.hyp -> string
      end

      module type SErrors = sig
        type t =
          | Invalid_Sort_LTS of Sorts.family
          | Invalid_Sort_Type of Sorts.family
          | InvalidCheckUpdatedCtx of
              (Environ.env
              * Evd.evar_map
              * Evd.econstr list
              * EConstr.rel_declaration list)
          | InvalidLTSArgsLength of int
          | InvalidLTSTermKind of
              Environ.env * Evd.evar_map * Constr.t

        exception MEBI_exn of t

        val invalid_sort_lts : Sorts.family -> exn
        val invalid_sort_type : Sorts.family -> exn

        val invalid_check_updated_ctx :
          Environ.env ->
          Evd.evar_map ->
          Evd.econstr list ->
          EConstr.rel_declaration list ->
          exn

        val invalid_lts_args_length : int -> exn

        val invalid_lts_term_kind :
          Environ.env -> Evd.evar_map -> Constr.t -> exn
      end

      module Errors : sig
        type t =
              Rocq_monad_utils.Make(Rocq_context.Default)(E)
              .Errors
              .t =
          | Invalid_Sort_LTS of Sorts.family
          | Invalid_Sort_Type of Sorts.family
          | InvalidCheckUpdatedCtx of
              (Environ.env
              * Evd.evar_map
              * Evd.econstr list
              * EConstr.rel_declaration list)
          | InvalidLTSArgsLength of int
          | InvalidLTSTermKind of
              Environ.env * Evd.evar_map * Constr.t

        exception MEBI_exn of t

        val invalid_sort_lts : Sorts.family -> exn
        val invalid_sort_type : Sorts.family -> exn

        val invalid_check_updated_ctx :
          Environ.env ->
          Evd.evar_map ->
          Evd.econstr list ->
          EConstr.rel_declaration list ->
          exn

        val invalid_lts_args_length : int -> exn

        val invalid_lts_term_kind :
          Environ.env -> Evd.evar_map -> Constr.t -> exn
      end

      module type SErr = sig
        val invalid_check_updated_ctx :
          Evd.econstr list ->
          EConstr.rel_declaration list ->
          'a mm

        val invalid_lts_args_length : int -> 'a
        val invalid_lts_term_kind : Constr.t -> 'a mm
      end

      module Err : sig
        val invalid_check_updated_ctx :
          Evd.econstr list ->
          EConstr.rel_declaration list ->
          'a mm

        val invalid_lts_args_length : int -> 'a
        val invalid_lts_term_kind : Constr.t -> 'a mm
      end

      val mk_ctx_substl :
        EConstr.Vars.substl ->
        ('a, Evd.econstr, 'b) Context.Rel.Declaration.pt list ->
        EConstr.Vars.substl mm

      val extract_args :
        ?substl:EConstr.Vars.substl ->
        Constr.t ->
        Rocq_utils.constructor_args mm

      module Unification : sig
        module type SPair = sig
          type t = {
            to_check : Evd.econstr;
            acc : Evd.econstr;
          }

          val to_string :
            Environ.env -> Evd.evar_map -> t -> string

          val make :
            Environ.env ->
            Evd.evar_map ->
            Evd.econstr ->
            Evd.econstr ->
            Evd.evar_map * t

          val unify :
            Environ.env ->
            Evd.evar_map ->
            t ->
            Evd.evar_map * bool
        end

        module Pair : sig
          type t =
                Rocq_monad_utils.Make(Rocq_context.Default)(E)
                .Unification
                .Pair
                .t = {
            to_check : Evd.econstr;
            acc : Evd.econstr;
          }

          val to_string :
            Environ.env -> Evd.evar_map -> t -> string

          val make :
            Environ.env ->
            Evd.evar_map ->
            Evd.econstr ->
            Evd.econstr ->
            Evd.evar_map * t

          val unify :
            Environ.env ->
            Evd.evar_map ->
            t ->
            Evd.evar_map * bool
        end

        module type SProblem = sig
          type t = {
            act : Pair.t;
            goto : Pair.t;
            tree : Tree.t;
          }

          val to_string :
            Environ.env -> Evd.evar_map -> t -> string

          val unify_opt : t -> Tree.t option mm
        end

        module Problem : sig
          type t = {
            act : Pair.t;
            goto : Pair.t;
            tree : Tree.t;
          }

          val to_string :
            Environ.env -> Evd.evar_map -> t -> string

          val unify_opt : t -> Tree.t option mm
        end

        module type SProblems = sig
          type t = {
            sigma : Evd.evar_map;
            to_unify : Problem.t list;
          }

          val empty : unit -> t mm
          val list_is_empty : t list -> bool
          val to_string : Environ.env -> t -> string
          val list_to_string : Environ.env -> t list -> string

          val sandbox_unify_all_opt :
            Evd.econstr ->
            Evd.econstr ->
            t ->
            (Evd.econstr * Evd.econstr * Tree.t list) option mm
        end

        module Problems : sig
          type t = {
            sigma : Evd.evar_map;
            to_unify : Problem.t list;
          }

          val empty : unit -> t mm
          val list_is_empty : t list -> bool
          val to_string : Environ.env -> t -> string
          val list_to_string : Environ.env -> t list -> string

          val sandbox_unify_all_opt :
            Evd.econstr ->
            Evd.econstr ->
            t ->
            (Evd.econstr * Evd.econstr * Tree.t list) option mm
        end

        module type SConstructors = sig
          type t = Constructor.t list

          val to_string :
            Environ.env -> Evd.evar_map -> t -> string

          val retrieve :
            int ->
            t ->
            Evd.econstr ->
            Evd.econstr ->
            Enc.t * Problems.t list ->
            t mm
        end

        module Constructors : sig
          type t = Constructor.t list

          val to_string :
            Environ.env -> Evd.evar_map -> t -> string

          val retrieve :
            int ->
            t ->
            Evd.econstr ->
            Evd.econstr ->
            Enc.t * Problems.t list ->
            t mm
        end

        val constr_to_problem :
          Rocq_utils.constructor_args ->
          Constructor.t ->
          Problem.t

        val map_problems :
          Rocq_utils.constructor_args ->
          Constructors.t ->
          Problems.t mm

        val cross_product :
          Problems.t list -> Problems.t -> Problems.t list

        val does_constructor_unify :
          Evd.econstr -> Evd.econstr -> bool mm

        val check_constructor_args_unify :
          Evd.econstr ->
          Evd.econstr ->
          Rocq_utils.constructor_args ->
          bool mm

        val axiom_constructor :
          Evd.econstr ->
          Evd.econstr ->
          Enc.t * int ->
          Constructors.t ->
          Constructors.t mm

        val check_valid_constructors :
          Rocq_ind.LTS.constructor array ->
          Enc.t Rocq_ind.t F.t ->
          Evd.econstr ->
          Evd.econstr ->
          Enc.t ->
          Constructors.t mm

        val explore_valid_constructor :
          Enc.t Rocq_ind.t F.t ->
          Evd.econstr ->
          Enc.t ->
          Rocq_utils.constructor_args ->
          int * Constructors.t ->
          EConstr.Vars.substl * EConstr.rel_declaration list ->
          Constructors.t mm

        val check_updated_ctx :
          Enc.t ->
          Problems.t list ->
          Enc.t Rocq_ind.t F.t ->
          EConstr.Vars.substl * EConstr.rel_declaration list ->
          (Enc.t * Problems.t list) option mm

        val check_for_next_constructors :
          int ->
          Evd.econstr ->
          Evd.econstr ->
          Constructors.t ->
          (Enc.t * Problems.t list) option ->
          Constructors.t mm

        val collect_valid_constructors :
          Rocq_ind.LTS.constructor array ->
          Enc.t Rocq_ind.t F.t ->
          Evd.econstr ->
          Evd.econstr ->
          Enc.t ->
          Constructors.t mm
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

        type 'a tree =
              'a Enc_tree.Make(M.Enc).tree =
          | Node of 'a * 'a tree list

        type t = TreeNode.t tree

        val add : t -> t -> t
        val add_list : t -> t list -> t list
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val minimize : t -> TreeNode.t list

        exception CannotMinimizeEmptyList of unit

        val min : t list -> TreeNode.t list
        val to_string : t -> string

        val list_to_string :
          ?args:Utils.Strfy.style_args -> t list -> string
      end

      module Trees : sig
        type elt = Tree.t
        type t = Model.Make(M.Enc).Trees.t

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

      module State : sig
        type t = Model.Make(M.Enc).State.t = {
          term : M.Enc.t;
          pp : string option;
        }

        val equal : t -> t -> bool
        val compare : t -> t -> int
        val hash : t -> int
        val to_string : t -> string
      end

      module States : sig
        module S : sig
          type elt = State.t
          type t = Model.Make(M.Enc).States.S.t

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
      end

      module Label : sig
        type t = Model.Make(M.Enc).Label.t = {
          term : M.Enc.t;
          is_silent : bool option;
        }

        val equal : t -> t -> bool
        val compare : t -> t -> int
        val hash : t -> int
        val to_string : t -> string
        val is_silent : t -> bool
      end

      module Labels : sig
        module S : sig
          type elt = Label.t
          type t = Model.Make(M.Enc).Labels.S.t

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
        val to_string : t -> string
      end

      module Note : sig
        type t = Model.Make(M.Enc).Note.t = {
          from : State.t;
          label : Label.t;
          using : Trees.t;
          goto : State.t;
        }

        val equal : t -> t -> bool
        val compare : t -> t -> int
        val to_string : t -> string
        val is_silent : t -> bool
      end

      module Annotation : sig
        type t = Model.Make(M.Enc).Annotation.t = {
          this : Note.t;
          next : t option;
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
      end

      module Annotations : sig
        module S : sig
          type elt = Annotation.t

          type t =
            Model.Make(M.Enc).Annotations.S.t

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
      end

      module Transition : sig
        type t = Model.Make(M.Enc).Transition.t = {
          from : State.t;
          goto : State.t;
          label : Label.t;
          annotation : Annotation.t option;
          constructor_trees : Trees.t;
        }

        val equal : t -> t -> bool
        val compare : t -> t -> int
        val is_silent : t -> bool
        val annotation_is_empty : t -> bool
        val to_string : t -> string
      end

      module Transitions : sig
        module S : sig
          type elt = Transition.t

          type t =
            Model.Make(M.Enc).Transitions.S.t

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
        val to_string : t -> string
      end

      module Action : sig
        type t = Model.Make(M.Enc).Action.t = {
          label : Label.t;
          annotation : Annotation.t option;
          constructor_trees : Trees.t;
        }

        val wk_equal : t -> t -> bool
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val hash : t -> int
        val is_silent : t -> bool
        val annotation_is_empty : t -> bool
        val to_string : t -> string
      end

      module ActionPair : sig
        type t = Action.t * States.t

        val to_string : t -> string
        val compare : t -> t -> int
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

        val merge_saturated_tuples :
          ActionPair.t list ->
          ActionPair.t list ->
          ActionPair.t list

        val try_update_saturated_tuple :
          ActionPair.t ->
          ActionPair.t list ->
          ActionPair.t option * ActionPair.t list
      end

      module Actions : sig
        module S : sig
          type elt = Action.t
          type t = Model.Make(M.Enc).Actions.S.t

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
      end

      module ActionMap : sig
        module M : sig
          type key = Action.t

          type 'a t =
            'a Model.Make(M.Enc).ActionMap.M.t

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

          val filter_map_inplace :
            (key -> 'a -> 'a option) -> 'a t -> unit

          val fold :
            (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

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

        val filter_map_inplace :
          (key -> 'a -> 'a option) -> 'a t -> unit

        val fold :
          (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

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
        val get_destinations : t' -> States.t
        val reduce_by_label : t' -> Label.t -> t'
        val to_actions : t' -> Actions.t
        val to_actionpairs : t' -> ActionPairs.t
        val of_actionpairs : ActionPairs.t -> t'
        val merge : t' -> t' -> t'
        val to_string : t' -> string
      end

      module Edge : sig
        type t = Model.Make(M.Enc).Edge.t = {
          from : State.t;
          goto : State.t;
          action : Action.t;
        }

        val equal : t -> t -> bool
        val compare : t -> t -> int
        val is_silent : t -> bool
        val to_string : t -> string
      end

      module Edges : sig
        module S : sig
          type elt = Edge.t
          type t = Model.Make(M.Enc).Edges.S.t

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
      end

      module EdgeMap : sig
        module M : sig
          type key = State.t

          type 'a t =
            'a Model.Make(M.Enc).EdgeMap.M.t

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

          val filter_map_inplace :
            (key -> 'a -> 'a option) -> 'a t -> unit

          val fold :
            (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

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

        val filter_map_inplace :
          (key -> 'a -> 'a option) -> 'a t -> unit

        val fold :
          (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

        val length : 'a t -> int
        val stats : 'a t -> Hashtbl.statistics
        val to_seq : 'a t -> (key * 'a) Seq.t
        val to_seq_keys : 'a t -> key Seq.t
        val to_seq_values : 'a t -> 'a Seq.t
        val add_seq : 'a t -> (key * 'a) Seq.t -> unit
        val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
        val of_seq : (key * 'a) Seq.t -> 'a t

        type t' = ActionMap.t' t

        val update :
          t' -> State.t -> Action.t -> States.t -> unit

        val get_destinations : t' -> State.t -> States.t
        val get_actions : t' -> State.t -> Actions.t
        val reduce_by_label : t' -> Label.t -> t'
        val get_edges : t' -> State.t -> Edges.t
        val to_edges : t' -> Edges.t
        val of_edges : Edges.t -> t'
        val merge : t' -> t' -> t'
        val to_string : t' -> string
      end

      module Partition : sig
        module S : sig
          type elt = States.t
          type t = Model.Make(M.Enc).Partition.S.t

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
        val get_reachable : t -> State.t -> EdgeMap.t' -> t
        val to_string : t -> string
      end

      module Info : sig
        type t = Model.Make(M.Enc).Info.t = {
          meta : meta option;
          weak_labels : Labels.t;
        }

        and meta = Model.Make(M.Enc).Info.meta = {
          is_complete : bool;
          is_merged : bool;
          bounds : bounds;
          lts : lts list;
        }

        and bounds =
              Model.Make(M.Enc).Info.bounds =
          | States of int
          | Transitions of int

        and lts = Model.Make(M.Enc).Info.lts = {
          enc : M.Enc.t;
          constructors : Rocq_bindings.constructor list;
        }

        val merge : t -> t -> t
        val to_string : t -> string
      end

      module LTS : sig
        type t = Model.Make(M.Enc).LTS.t = {
          init : State.t option;
          terminals : Partition.elt;
          alphabet : Labels.t;
          states : Partition.elt;
          transitions : Transitions.t;
          info : Info.t;
        }

        val to_string : t -> string
      end

      module FSM : sig
        type t = Model.Make(M.Enc).FSM.t = {
          init : State.t option;
          terminals : Partition.elt;
          alphabet : Labels.t;
          states : Partition.elt;
          edges : EdgeMap.t';
          info : Info.t;
        }

        val merge : t -> t -> t
        val to_string : t -> string
      end

      module Convert : sig
        val transitions_to_edgemap :
          Transitions.t -> EdgeMap.t'

        val lts_to_fsm : LTS.t -> FSM.t
      end

      module Saturate : sig
        type data =
              Model.Make(M.Enc).Saturate.data = {
          named : Action.t option;
          notes : wip list;
          visited : Partition.elt;
          old_edges : EdgeMap.t';
        }

        and wip =
              Model.Make(M.Enc).Saturate.wip = {
          from : State.t;
          via : Label.t;
          trees : Trees.t;
        }

        val wip : State.t -> Action.t -> wip
        val initial_data : EdgeMap.t' -> data
        val update_named : Action.t -> data -> data
        val update_notes : State.t -> Action.t -> data -> data
        val update_visited : State.t -> data -> data
        val already_visited : State.t -> data -> bool
        val skip_action : Action.t -> data -> bool

        val get_old_actions :
          State.t -> data -> ActionMap.t' option

        exception Model_Saturate_WIP_IsEmptyList of unit

        val wip_to_annotation :
          State.t -> wip list -> Annotation.t

        exception
          Model_Saturate_WIP_HadNoNamedActions of wip list

        exception
          Model_Saturate_WIP_HadMultipleNamedActions of
            wip list

        val validate_wips : wip list -> unit

        val extrapolate_annotations :
          Annotation.t -> Annotations.t

        val stop :
          data -> State.t -> ActionPairs.t -> ActionPairs.t

        val check_from :
          data -> State.t -> ActionPairs.t -> ActionPairs.t

        val check_actions :
          data ->
          State.t ->
          ActionMap.t' ->
          ActionPairs.t ->
          ActionPairs.t

        val check_destinations :
          data ->
          State.t ->
          Partition.elt ->
          ActionPairs.t ->
          ActionPairs.t

        val edge_action_destinations :
          data -> State.t -> Partition.elt -> ActionPairs.t

        val edge_actions :
          State.t ->
          ActionMap.t' ->
          EdgeMap.t' ->
          ActionPairs.t

        val edge :
          ActionMap.t' ->
          State.t ->
          ActionMap.t' ->
          EdgeMap.t' ->
          unit

        val edges :
          Labels.t -> Partition.elt -> EdgeMap.t' -> EdgeMap.t'

        val fsm : ?only_if_weak:bool option -> FSM.t -> FSM.t
      end

      module Minimize : sig
        type t = Model.Make(M.Enc).Minimize.t = {
          fsm : FSM.t;
          pi : Partition.t;
        }

        exception
          Split_OnlyReturnedOneBlock_ButNeqBlock of
            (States.t * States.t)

        val ensure_equal : States.t -> States.t -> unit

        exception CannotSplitEmptyBlock of unit

        val ensure_nonempty : States.t -> unit

        val split_block :
          Partition.t ->
          State.t ->
          EdgeMap.t' ->
          States.t ->
          States.t * States.t option

        val for_each_label :
          Partition.t ref ->
          bool ref ->
          EdgeMap.t' ->
          States.t ref ->
          Label.t ->
          unit

        val for_each_block :
          Partition.t ref ->
          bool ref ->
          Labels.t ->
          EdgeMap.t' ->
          States.t ->
          unit

        val partition_states : FSM.t -> Partition.t
        val fsm : ?weak:bool -> FSM.t -> t
      end

      module Bisimilar : sig
        type t = Model.Make(M.Enc).Bisimilar.t = {
          fsm_a : fsm_pair;
          fsm_b : fsm_pair;
          merged : FSM.t;
          result : result;
        }

        and result =
              Model.Make(M.Enc).Bisimilar.result = {
          bisim_states : Partition.t;
          non_bisim_states : Partition.t;
        }

        and fsm_pair =
              Model.Make(M.Enc).Bisimilar.fsm_pair = {
          original : FSM.t;
          saturated : FSM.t;
        }

        val fsm_pair : ?weak:bool -> FSM.t -> fsm_pair
        val the_cached_result : t option ref
        val set_the_result : t -> unit

        exception NoCachedResult of unit

        val get_the_result : unit -> t

        val split :
          Partition.t -> States.t -> States.t -> result

        val fsm : ?weak:bool -> FSM.t -> FSM.t -> t
      end
    end
  end

  module M = W.M
  module Model = W.Model

  module P : sig
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
      type key = Evd.econstr
      type 'a t = 'a Bi_encoding.Make(Ctx)(Enc).F.t

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

      val filter_map_inplace :
        (key -> 'a -> 'a option) -> 'a t -> unit

      val fold :
        (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

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
      type 'a t = 'a Bi_encoding.Make(Ctx)(Enc).B.t

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

      val filter_map_inplace :
        (key -> 'a -> 'a option) -> 'a t -> unit

      val fold :
        (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

      val length : 'a t -> int
      val stats : 'a t -> Hashtbl.statistics
      val to_seq : 'a t -> (key * 'a) Seq.t
      val to_seq_keys : 'a t -> key Seq.t
      val to_seq_values : 'a t -> 'a Seq.t
      val add_seq : 'a t -> (key * 'a) Seq.t -> unit
      val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
      val of_seq : (key * 'a) Seq.t -> 'a t
    end

    type maps = Bi_encoding.Make(Ctx)(Enc).maps = {
      fwd : Enc.t F.t;
      bck : Evd.econstr B.t;
    }

    val the_maps : unit -> maps ref
    val reset : unit -> unit
    val fwdmap : unit -> Enc.t F.t
    val bckmap : unit -> Evd.econstr B.t
    val get_encoding : Evd.econstr -> Enc.t
    val encode : Evd.econstr -> Enc.t
    val encoded : Evd.econstr -> bool
    val get_econstr : Enc.t -> Evd.econstr

    exception CannotDecode of Enc.t

    val decode : Enc.t -> Evd.econstr
    val decode_opt : Enc.t -> Evd.econstr option
    val decode_map : 'a B.t -> 'a F.t
    val encode_map : 'a F.t -> 'a B.t
    val to_list : unit -> (Enc.t * Evd.econstr) list
    val make_hashtbl : (module Hashtbl.S with type key = Enc.t)
    val make_set : (module Set.S with type elt = Enc.t)
    val bienc_to_list : unit -> (Enc.t * Evd.econstr) list

    type 'a mm = wrapper ref -> 'a in_wrapper
    and wrapper = { ctx : Rocq_context.t ref; maps : maps ref }
    and 'a in_wrapper = { state : wrapper ref; value : 'a }

    val run : ?reset_encoding:bool -> 'a mm -> 'a
    val return : 'a -> 'a mm
    val bind : 'a mm -> ('a -> 'b mm) -> 'b mm
    val map : ('a -> 'b) -> 'a mm -> 'b mm
    val product : 'a mm -> 'b mm -> ('a * 'b) mm

    val iterate :
      int -> int -> 'a -> (int -> 'a -> 'a mm) -> 'a mm

    val state :
      (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a) ->
      wrapper ref ->
      'a in_wrapper

    val sandbox :
      ?sigma:Evd.evar_map ->
      'a mm ->
      wrapper ref ->
      'a in_wrapper

    module type SYNTAX = sig
      val ( let+ ) : 'a mm -> ('a -> 'b) -> 'b mm
      val ( let* ) : 'a mm -> ('a -> 'b mm) -> 'b mm

      val ( let$ ) :
        (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a) ->
        ('a -> 'b mm) ->
        'b mm

      val ( let$* ) :
        (Environ.env -> Evd.evar_map -> Evd.evar_map) ->
        (unit -> 'b mm) ->
        'b mm

      val ( let$+ ) :
        (Environ.env -> Evd.evar_map -> 'a) ->
        ('a -> 'b mm) ->
        'b mm

      val ( and+ ) : 'a mm -> 'b mm -> ('a * 'b) mm
    end

    module Syntax : sig
      val ( let+ ) : 'a mm -> ('a -> 'b) -> 'b mm
      val ( let* ) : 'a mm -> ('a -> 'b mm) -> 'b mm

      val ( let$ ) :
        (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a) ->
        ('a -> 'b mm) ->
        'b mm

      val ( let$* ) :
        (Environ.env -> Evd.evar_map -> Evd.evar_map) ->
        (unit -> 'b mm) ->
        'b mm

      val ( let$+ ) :
        (Environ.env -> Evd.evar_map -> 'a) ->
        ('a -> 'b mm) ->
        'b mm

      val ( and+ ) : 'a mm -> 'b mm -> ('a * 'b) mm
    end

    val get_ctx : wrapper ref -> Rocq_context.t in_wrapper
    val get_env : wrapper ref -> Environ.env in_wrapper
    val get_sigma : wrapper ref -> Evd.evar_map in_wrapper
    val get_maps : wrapper ref -> maps in_wrapper
    val get_fwdmap : wrapper ref -> Enc.t F.t in_wrapper
    val get_bckmap : wrapper ref -> Evd.econstr B.t in_wrapper

    val fstring :
      (Environ.env -> Evd.evar_map -> 'a -> string) ->
      'a ->
      string

    module Tree : sig
      module type STreeNode = sig
        type t = Enc.t * int

        val to_string : t -> string
      end

      module TreeNode : sig
        type t = Enc.t * int

        val to_string : t -> string
      end

      type 'a tree = 'a Enc_tree.Make(Enc).tree =
        | Node of 'a * 'a tree list

      type t = TreeNode.t tree

      val add : t -> t -> t
      val add_list : t -> t list -> t list
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val minimize : t -> TreeNode.t list

      exception CannotMinimizeEmptyList of unit

      val min : t list -> TreeNode.t list
      val to_string : t -> string

      val list_to_string :
        ?args:Utils.Strfy.style_args -> t list -> string
    end

    module Constructor : sig
      type t = Evd.econstr * Evd.econstr * Tree.t

      val to_string :
        Environ.env -> Evd.evar_map -> t -> string
    end

    val make_state_tree_pair_set :
      (module Set.S with type elt = Enc.t * Tree.t)

    val fresh_evar : Rocq_utils.evar_source -> Evd.econstr mm
    val econstr_eq : Evd.econstr -> Evd.econstr -> bool mm
    val econstr_normalize : Evd.econstr -> Evd.econstr mm

    val econstr_kind :
      Evd.econstr -> Rocq_utils.econstr_kind mm

    val econstr_is_evar : Evd.econstr -> bool mm

    val econstr_to_constr :
      ?abort_on_undefined_evars:bool ->
      Evd.econstr ->
      Constr.t mm

    val econstr_to_constr_opt :
      Evd.econstr -> Constr.t option mm

    val constrexpr_to_econstr :
      Constrexpr.constr_expr -> Evd.econstr mm

    val exists_eq :
      Evd.econstr -> 'a list -> ('a -> Evd.econstr) -> bool mm

    module Strfy : sig
      val econstr : Evd.econstr -> string
      val econstr_rel_decl : EConstr.rel_declaration -> string
      val hyp_name : Rocq_utils.hyp -> string
      val hyp_type : Rocq_utils.hyp -> string
      val hyp_value : Rocq_utils.hyp -> string
    end

    module type SErrors = sig
      type t =
        | Invalid_Sort_LTS of Sorts.family
        | Invalid_Sort_Type of Sorts.family
        | InvalidCheckUpdatedCtx of
            (Environ.env
            * Evd.evar_map
            * Evd.econstr list
            * EConstr.rel_declaration list)
        | InvalidLTSArgsLength of int
        | InvalidLTSTermKind of
            Environ.env * Evd.evar_map * Constr.t

      exception MEBI_exn of t

      val invalid_sort_lts : Sorts.family -> exn
      val invalid_sort_type : Sorts.family -> exn

      val invalid_check_updated_ctx :
        Environ.env ->
        Evd.evar_map ->
        Evd.econstr list ->
        EConstr.rel_declaration list ->
        exn

      val invalid_lts_args_length : int -> exn

      val invalid_lts_term_kind :
        Environ.env -> Evd.evar_map -> Constr.t -> exn
    end

    module Errors : SErrors

    module type SErr = sig
      val invalid_check_updated_ctx :
        Evd.econstr list ->
        EConstr.rel_declaration list ->
        'a mm

      val invalid_lts_args_length : int -> 'a
      val invalid_lts_term_kind : Constr.t -> 'a mm
    end

    module Err : SErr

    val mk_ctx_substl :
      EConstr.Vars.substl ->
      ('a, Evd.econstr, 'b) Context.Rel.Declaration.pt list ->
      EConstr.Vars.substl mm

    val extract_args :
      ?substl:EConstr.Vars.substl ->
      Constr.t ->
      Rocq_utils.constructor_args mm

    module Unification : sig
      module type SPair = sig
        type t = { to_check : Evd.econstr; acc : Evd.econstr }

        val to_string :
          Environ.env -> Evd.evar_map -> t -> string

        val make :
          Environ.env ->
          Evd.evar_map ->
          Evd.econstr ->
          Evd.econstr ->
          Evd.evar_map * t

        val unify :
          Environ.env ->
          Evd.evar_map ->
          t ->
          Evd.evar_map * bool
      end

      module Pair : SPair

      module type SProblem = sig
        type t = { act : Pair.t; goto : Pair.t; tree : Tree.t }

        val to_string :
          Environ.env -> Evd.evar_map -> t -> string

        val unify_opt : t -> Tree.t option mm
      end

      module Problem : SProblem

      module type SProblems = sig
        type t = {
          sigma : Evd.evar_map;
          to_unify : Problem.t list;
        }

        val empty : unit -> t mm
        val list_is_empty : t list -> bool
        val to_string : Environ.env -> t -> string
        val list_to_string : Environ.env -> t list -> string

        val sandbox_unify_all_opt :
          Evd.econstr ->
          Evd.econstr ->
          t ->
          (Evd.econstr * Evd.econstr * Tree.t list) option mm
      end

      module Problems : SProblems

      module type SConstructors = sig
        type t = Constructor.t list

        val to_string :
          Environ.env -> Evd.evar_map -> t -> string

        val retrieve :
          int ->
          t ->
          Evd.econstr ->
          Evd.econstr ->
          Enc.t * Problems.t list ->
          t mm
      end

      module Constructors : SConstructors

      val constr_to_problem :
        Rocq_utils.constructor_args ->
        Constructor.t ->
        Problem.t

      val map_problems :
        Rocq_utils.constructor_args ->
        Constructors.t ->
        Problems.t mm

      val cross_product :
        Problems.t list -> Problems.t -> Problems.t list

      val does_constructor_unify :
        Evd.econstr -> Evd.econstr -> bool mm

      val check_constructor_args_unify :
        Evd.econstr ->
        Evd.econstr ->
        Rocq_utils.constructor_args ->
        bool mm

      val axiom_constructor :
        Evd.econstr ->
        Evd.econstr ->
        Enc.t * int ->
        Constructors.t ->
        Constructors.t mm

      val check_valid_constructors :
        Rocq_ind.LTS.constructor array ->
        Enc.t Rocq_ind.t F.t ->
        Evd.econstr ->
        Evd.econstr ->
        Enc.t ->
        Constructors.t mm

      val explore_valid_constructor :
        Enc.t Rocq_ind.t F.t ->
        Evd.econstr ->
        Enc.t ->
        Rocq_utils.constructor_args ->
        int * Constructors.t ->
        EConstr.Vars.substl * EConstr.rel_declaration list ->
        Constructors.t mm

      val check_updated_ctx :
        Enc.t ->
        Problems.t list ->
        Enc.t Rocq_ind.t F.t ->
        EConstr.Vars.substl * EConstr.rel_declaration list ->
        (Enc.t * Problems.t list) option mm

      val check_for_next_constructors :
        int ->
        Evd.econstr ->
        Evd.econstr ->
        Constructors.t ->
        (Enc.t * Problems.t list) option ->
        Constructors.t mm

      val collect_valid_constructors :
        Rocq_ind.LTS.constructor array ->
        Enc.t Rocq_ind.t F.t ->
        Evd.econstr ->
        Evd.econstr ->
        Enc.t ->
        Constructors.t mm
    end
  end

  val get_concl : unit -> Evd.econstr
  val get_hyps : unit -> Rocq_utils.hyp list
  val get_hyp_names : unit -> Names.Id.Set.t

  val next_name_of :
    Names.Id.Set.t -> Names.variable -> Names.variable

  val new_name_of_string : string -> Names.variable
  val new_cofix_name : unit -> Names.variable
  val new_H_name : unit -> Names.variable

  module Tactic : sig
    type t = { this : tactic; next : t option }

    and tactic = {
      get : unit Proofview.tactic;
      msg : (Output_kind.t * string) option;
    }

    val tactic :
      ?level:Output_kind.t ->
      ?msg:string ->
      unit Proofview.tactic ->
      t

    val seq : t -> t -> t
    val empty : unit -> t

    exception EmptyTacticChain of unit

    val chain : ?nonempty:bool -> t list -> t
  end

  module Tacs : sig
    val inversion : Rocq_utils.hyp -> Tactic.t P.mm
    val subst_all : unit -> Tactic.t P.mm
    val simplify_concl : unit -> Tactic.t P.mm
    val simplify_hyp : Rocq_utils.hyp -> Tactic.t P.mm
    val simplify_hyps : unit -> Tactic.t P.mm
    val simplify_all : unit -> Tactic.t P.mm
    val simplify_and_subst_all : unit -> Tactic.t P.mm
    val cofix : unit -> Tactic.t P.mm
    val intros_all : unit -> Tactic.t P.mm
    val intro_as : string -> Tactic.t P.mm
    val apply : Evd.econstr -> Tactic.t P.mm
    val eapply : Evd.econstr -> Tactic.t P.mm

    exception CannotUnfoldConstr of Constr.t

    val unfold_constr :
      ?in_hyp:Rocq_utils.hyp -> Constr.t -> Tactic.t P.mm

    val unfold_econstr :
      ?in_hyp:Rocq_utils.hyp -> Evd.econstr -> Tactic.t P.mm

    val unfold_constrexpr :
      ?in_hyp:Rocq_utils.hyp ->
      Constrexpr.constr_expr ->
      Tactic.t P.mm
  end

  module Decode : sig
    val enc : M.Enc.t -> Evd.econstr
    val handle : M.Enc.t -> exn -> Evd.econstr

    exception CouldNotDecode_State of Model.State.t

    val state : Model.State.t -> Evd.econstr

    exception CouldNotDecode_Label of Model.Label.t

    val label : Model.Label.t -> Evd.econstr

    exception CouldNotDecode_LTS_Constructor of Model.Info.lts

    val lts_constructor : Model.Info.lts -> Evd.econstr
  end

  module Theory : sig
    val apply_Pack_sim : unit -> Tactic.t P.mm
    val apply_In_sim : unit -> Tactic.t P.mm
    val apply_wk_some : unit -> Tactic.t P.mm
    val apply_wk_none : unit -> Tactic.t P.mm
    val apply_rt1n_refl : unit -> Tactic.t P.mm
    val apply_rt1n_trans : unit -> Tactic.t P.mm
    val eapply_rt1n_refl : unit -> Tactic.t P.mm
    val eapply_rt1n_trans : unit -> Tactic.t P.mm
    val unfold_silent : unit -> Tactic.t P.mm
    val unfold_silent1 : unit -> Tactic.t P.mm
    val is_theory : Evd.econstr -> Evd.econstr -> bool P.mm
    val is_exists : Evd.econstr -> bool P.mm
    val is_weak_sim : Evd.econstr -> bool P.mm
    val is_weak : Evd.econstr -> bool P.mm
    val is_tau : Evd.econstr -> bool P.mm
    val is_silent : Evd.econstr -> bool P.mm
    val is_silent1 : Evd.econstr -> bool P.mm
    val is_LTS : Evd.econstr -> bool P.mm
    val is_None : Evd.econstr -> bool P.mm
    val is_Some : Evd.econstr -> bool P.mm

    val get_theory_enc :
      (Evd.econstr -> bool P.mm) -> M.Enc.t M.mm

    exception NoEncodingFoundFor_TheoriesNone of unit

    val get_None_enc : unit -> M.Enc.t M.mm

    exception NoEncodingFoundFor_TheoriesSome of unit

    val get_Some_enc : unit -> M.Enc.t M.mm

    exception NotEqTheory of unit

    val get_theory_enc_if_eq :
      Evd.econstr -> (Evd.econstr -> bool P.mm) -> M.Enc.t M.mm

    val get_None_enc_if_eq : Evd.econstr -> M.Enc.t M.mm
    val get_Some_enc_if_eq : Evd.econstr -> M.Enc.t M.mm

    exception FSM_HasNoSilentLabel of Model.FSM.t

    val is_fsm_silent_label :
      Evd.econstr -> Model.FSM.t -> bool M.mm

    exception FSM_HasNoVisibleLabel of Model.FSM.t

    val is_fsm_visible_label :
      Evd.econstr -> Model.FSM.t -> bool M.mm

    exception FSM_HasNoWeakLabels of Model.FSM.t

    val is_fsm_weak_labels :
      Evd.econstr -> Model.FSM.t -> bool M.mm

    exception FSM_HasNoConstructors of Model.FSM.t

    val is_fsm_constructor :
      Evd.econstr -> Model.FSM.t -> bool M.mm
  end

  module ReModel : sig
    exception
      CouldNotFind_State of (Evd.econstr * Model.Partition.elt)

    val state :
      Evd.econstr -> Model.Partition.elt -> Model.State.t M.mm

    exception
      CouldNotFind_Label of (Evd.econstr * Model.Labels.t)

    val label :
      Evd.econstr -> Model.Labels.t -> Model.Label.t M.mm
  end

  module Trans : sig
    module Make : (X : sig
                     type state
                     type label

                     val eq_state : state -> state -> bool M.mm
                     val eq_label : label -> label -> bool M.mm
                   end)
      -> sig
      type t =
        | Full of {
            from : X.state;
            label : X.label;
            goto : X.state;
          }
        | Partial of partial

      and partial =
        | NoGoto of { from : X.state; label : X.label }
        | NoLabel of { from : X.state; goto : X.state }
        | Just of { from : X.state }

      exception LabelArgNotEqExisting of (X.label * partial)
      exception GotoArgNotEqExisting of (X.state * partial)
      exception ArgsFillNothing of unit

      val fill :
        ?label:X.label -> ?goto:X.state -> partial -> t M.mm
    end

    module RocqTrans : sig
      type t =
        | Full of {
            from : Evd.econstr;
            label : Evd.econstr;
            goto : Evd.econstr;
          }
        | Partial of partial

      and partial =
        | NoGoto of { from : Evd.econstr; label : Evd.econstr }
        | NoLabel of { from : Evd.econstr; goto : Evd.econstr }
        | Just of { from : Evd.econstr }

      exception
        LabelArgNotEqExisting of (Evd.econstr * partial)

      exception GotoArgNotEqExisting of (Evd.econstr * partial)
      exception ArgsFillNothing of unit

      val fill :
        ?label:Evd.econstr ->
        ?goto:Evd.econstr ->
        partial ->
        t M.mm
    end

    module EncTrans : sig
      type t =
        | Full of {
            from : M.Enc.t;
            label : M.Enc.t;
            goto : M.Enc.t;
          }
        | Partial of partial

      and partial =
        | NoGoto of { from : M.Enc.t; label : M.Enc.t }
        | NoLabel of { from : M.Enc.t; goto : M.Enc.t }
        | Just of { from : M.Enc.t }

      exception LabelArgNotEqExisting of (M.Enc.t * partial)
      exception GotoArgNotEqExisting of (M.Enc.t * partial)
      exception ArgsFillNothing of unit

      val fill :
        ?label:M.Enc.t -> ?goto:M.Enc.t -> partial -> t M.mm
    end

    module ModelTrans : sig
      type t =
        | Full of {
            from : Model.State.t;
            label : Model.Label.t;
            goto : Model.State.t;
          }
        | Partial of partial

      and partial =
        | NoGoto of {
            from : Model.State.t;
            label : Model.Label.t;
          }
        | NoLabel of {
            from : Model.State.t;
            goto : Model.State.t;
          }
        | Just of { from : Model.State.t }

      exception
        LabelArgNotEqExisting of (Model.Label.t * partial)

      exception
        GotoArgNotEqExisting of (Model.State.t * partial)

      exception ArgsFillNothing of unit

      val fill :
        ?label:Model.Label.t ->
        ?goto:Model.State.t ->
        partial ->
        t M.mm
    end

    type t =
      | RocqTransition of RocqTrans.t
      | EncTransition of EncTrans.t
      | ModelTransition of ModelTrans.t
      | Complete of Model.Transition.t
  end

  module Hyp : sig end
end
