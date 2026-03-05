val cache_pp : bool

module Make : (Log : Logger.S)
  (C : Rocq_context.SRocq_context)
  (E : Encoding.SEncoding)
  -> sig
  module M : module type of Rocq_monad_utils.Make(Log)(C)(E)

  module Model : module type of Model.Make(Log)(M.Enc)

  module LTS = Model.LTS
  module FSM = Model.FSM

  module IsTheory : sig
    val is_theory : Evd.econstr -> Evd.econstr -> bool M.mm
    val is_exists : Evd.econstr -> bool M.mm
    val is_weak_sim : Evd.econstr -> bool M.mm
    val is_weak : Evd.econstr -> bool M.mm
    val is_tau : Evd.econstr -> bool M.mm
    val is_silent : Evd.econstr -> bool M.mm
    val is_silent1 : Evd.econstr -> bool M.mm
    val is_LTS : Evd.econstr -> bool M.mm
    val is_None : Evd.econstr -> bool M.mm
    val is_Some : Evd.econstr -> bool M.mm

    val get_theory_enc :
      (Evd.econstr -> bool M.mm) -> M.Enc.t M.mm

    exception NoEncodingFoundFor_TheoriesNone of unit

    val get_None_enc : unit -> M.Enc.t M.mm

    exception NoEncodingFoundFor_TheoriesSome of unit

    val get_Some_enc : unit -> M.Enc.t M.mm

    exception NotEqTheory of unit

    val get_theory_enc_if_eq :
      Evd.econstr -> (Evd.econstr -> bool M.mm) -> M.Enc.t M.mm

    val get_None_enc_if_eq : Evd.econstr -> M.Enc.t M.mm
    val get_Some_enc_if_eq : Evd.econstr -> M.Enc.t M.mm
  end

  module Weak : sig
    type t = Option of M.Enc.t | Custom of M.Enc.t * M.Enc.t

    val eq : t -> t -> bool
    val to_string : t -> string M.mm
  end

  module Config : sig
    val load_weak_arg : Api.weak_arg -> Weak.t M.mm

    val load_weak_arg_opt :
      Api.weak_arg option -> Weak.t option M.mm

    type weak_args = { a : Weak.t option; b : Weak.t option }

    val the_weak_args : weak_args ref option ref
    val reset_the_weak_args : unit -> unit
    val load_weak_args : unit -> unit M.mm
    val get_the_weak_args : unit -> weak_args option
    val get_the_weak_arg1 : unit -> Weak.t option
    val get_the_weak_arg2 : unit -> Weak.t option

    val api_bounds_to_model_bounds :
      Api.bounds_args -> Model.Info.bounds

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

                    val iter :
                      (key -> 'a -> unit) -> 'a t -> unit

                    val filter_map_inplace :
                      (key -> 'a -> 'a option) -> 'a t -> unit

                    val fold :
                      (key -> 'a -> 'acc -> 'acc) ->
                      'a t ->
                      'acc ->
                      'acc

                    val length : 'a t -> int
                    val stats : 'a t -> Hashtbl.statistics
                    val to_seq : 'a t -> (key * 'a) Seq.t
                    val to_seq_keys : 'a t -> key Seq.t
                    val to_seq_values : 'a t -> 'a Seq.t

                    val add_seq :
                      'a t -> (key * 'a) Seq.t -> unit

                    val replace_seq :
                      'a t -> (key * 'a) Seq.t -> unit

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
    (D0 : sig
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
    (X : X_Args)
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

      type t' = A.t' t

      val update : t' -> V.elt -> A.key -> D.t -> unit
      val size : t' -> int
      val to_string : t' -> string
    end

    type t = {
      to_visit : V.elt Queue.t;
      init : V.elt;
      states : V.t;
      transitions : T.t';
      ind_defs : M.Ind.t M.B.t;
      weak : Weak.t option;
    }

    val empty : V0.elt -> M.Ind.t M.B.t -> t
    val _log_to_visit : t -> unit
    val _log_ind_defs : M.Ind.t M.B.t -> unit

    val is_silent_transition :
      Evd.econstr -> Weak.t option -> bool option M.mm

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

    module Make : (Y : Y_Args) -> sig
      val next_to_visit : t -> T.key
      val update_to_visit : t -> T.key -> unit
      val update_states : t -> V.t -> t
      val get_new_constrs : T.key -> M.Constructor.t list M.mm
      val get_new_states : t -> T.key -> V.t M.mm
      val build : t -> t M.mm
    end

    module Extract : (Z : Z_Args) -> sig
      val pp : V.elt -> string option
      val state : V.elt -> Model.EdgeMap.key
      val states : unit -> Model.Partition.elt
      val terminals : unit -> Model.Partition.elt
      val label : A.key -> Model.Label.t
      val transitions : unit -> Model.Transitions.t
      val constructor_info : unit -> Model.Info.lts list M.mm
      val meta : unit -> Model.Info.meta M.mm
      val weak_labels : Model.Labels.t -> Model.Labels.t M.mm
      val lts : unit -> LTS.t M.mm
    end

    val build_ind_defs : unit -> M.Ind.t M.B.t M.mm
    val find_primary_lts : M.Ind.t M.B.t -> M.Ind.t M.mm

    val initial_term :
      Constrexpr.constr_expr -> Evd.econstr M.mm

    val make_yargs :
      M.Ind.t -> M.Ind.t M.B.t -> 'a -> (module Y_Args)

    val make_zargs : M.Ind.t M.B.t -> t ref -> (module Z_Args)
    val build : Constrexpr.constr_expr -> LTS.t M.mm
  end

  val make_xargs :
    Libnames.qualid ->
    Names.GlobRef.t list ->
    Weak.t option ->
    (module X_Args)

  val fail_if_empty : LTS.t -> unit
  val fail_if_incomplete : LTS.t -> unit
  val fail_if_not_bisim : Model.Bisimilar.result -> unit

  val extract_lts :
    Libnames.qualid ->
    Constrexpr.constr_expr ->
    Libnames.qualid list ->
    Weak.t option ->
    LTS.t M.mm

  module Command : sig
    val default_weak_arg : Weak.t option -> Weak.t option

    val build_lts :
      ?weak:Weak.t option ->
      Libnames.qualid ->
      Constrexpr.constr_expr ->
      Libnames.qualid list ->
      LTS.t M.mm

    val build_fsm :
      ?weak:Weak.t option ->
      Libnames.qualid ->
      Constrexpr.constr_expr ->
      Libnames.qualid list ->
      FSM.t M.mm

    type t =
      | MakeLTS of rocq_args
      | MakeFSM of rocq_args
      | Saturate of rocq_args
      | Minimize of rocq_args
      | Merge of rocq_pair
      | CheckBisim of rocq_pair

    and rocq_args = Constrexpr.constr_expr * Libnames.qualid
    and rocq_pair = { a : rocq_args; b : rocq_args }

    val do_make_lts :
      Constrexpr.constr_expr * Libnames.qualid ->
      Libnames.qualid list ->
      Model.Bisimilar.t option M.mm

    val do_make_fsm :
      Constrexpr.constr_expr * Libnames.qualid ->
      Libnames.qualid list ->
      Model.Bisimilar.t option M.mm

    val do_saturate :
      Constrexpr.constr_expr * Libnames.qualid ->
      Libnames.qualid list ->
      Model.Bisimilar.t option M.mm

    val do_minimize :
      Constrexpr.constr_expr * Libnames.qualid ->
      Libnames.qualid list ->
      Model.Bisimilar.t option M.mm

    val build_fsms :
      rocq_args ->
      rocq_args ->
      Libnames.qualid list ->
      (FSM.t * FSM.t) M.mm

    val do_merge :
      rocq_pair ->
      Libnames.qualid list ->
      Model.Bisimilar.t option M.mm

    val do_check_bisim :
      rocq_pair ->
      Libnames.qualid list ->
      Model.Bisimilar.t option M.mm

    val run :
      Libnames.qualid list ->
      t ->
      Model.Bisimilar.t option M.mm
  end
end

module Default : () -> module type of Make (Logger.Default) ((val Api.default_context ()))
    ((val Api.default_encoding ()))
