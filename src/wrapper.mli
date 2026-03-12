module Make : (Log : Logger.S) (Ctx : Rocq_context.S) (Enc : Encoding.S) -> sig
  module M : module type of Rocq_monad_utils.Make (Log) (Ctx) (Enc)

  module Model :
      module type of Model.Make (Log) (Enc) (M.Bindings) (M.ConstructorBindings)

  module Decode : sig
    val enc : Enc.t -> EConstr.t
    val handle : Enc.t -> exn -> EConstr.t

    exception CouldNotDecode_State of Model.States.elt

    val state : Model.States.elt -> EConstr.t

    exception CouldNotDecode_Label of Model.Labels.elt

    val label : Model.Labels.elt -> EConstr.t

    exception CouldNotDecode_LTS_Constructor of Model.Info.Meta.RocqLTS.t

    val lts_constructor : Model.Info.Meta.RocqLTS.t -> EConstr.t
    module Base : sig
      type k = Enc.t

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit

      module Tree : sig
        module Node : sig
          type k = Enc.Tree.Node.t

          val json : ?as_elt:bool -> k -> Yojson.t
          val to_string : ?pretty:bool -> k -> string

          val log :
            ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit
        end

        type k = Enc.Tree.t

        val json : ?as_elt:bool -> k -> Yojson.t
        val to_string : ?pretty:bool -> k -> string

        val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit
      end

      module Trees : sig
        type k = Enc.Trees.t

        val json : ?as_elt:bool -> k -> Yojson.t
        val to_string : ?pretty:bool -> k -> string

        val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit
      end
    end

    module State : sig
      type k = Model.Edge.state

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit
    end

    module States : sig
      type k = Model.ActionMap.states

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit
    end

    module Label : sig
      type k = Model.Action.label

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit
    end

    module Labels : sig
      type k = Model.Actions.labels

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit
    end

    module Note : sig
      type k = Model.Annotation.note

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit
    end

    module Annotation : sig
      type k = Model.Action.annotation

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit
    end

    module Annotations : sig
      type k = Model.Annotations.t

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit
    end

    module Transition : sig
      type k = Model.Transition.t

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit
    end

    module Transitions : sig
      type k = Model.EdgeMap.transitions

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit
    end

    module Action : sig
      type k = Model.Action.t

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit
    end

    module ActionMap : sig
      type k = States.k Model.ActionMap.t

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit
    end

    module EdgeMap : sig
      type k = Model.ActionMap.t' Model.EdgeMap.t

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit
    end

    module Info : sig
      module Meta : sig
        module RocqLTS : sig
          type k = Model.Info.Meta.RocqLTS.t

          val json : ?as_elt:bool -> k -> Yojson.t
          val to_string : ?pretty:bool -> k -> string

          val log :
            ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit
        end

        type k = Model.Info.Meta.t

        val json : ?as_elt:bool -> k -> Yojson.t
        val to_string : ?pretty:bool -> k -> string

        val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit 
      end

      type k = Model.Info.t

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit
    end

    module LTS : sig
      type k = Model.LTS.t

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit
    end

    module FSM : sig
      type k = Model.FSM.t

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit
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
    val get_theory_enc : (EConstr.t -> bool M.mm) -> Enc.t M.mm

    exception NoEncodingFoundFor_TheoriesNone of unit

    val get_None_enc : unit -> Enc.t M.mm

    exception NoEncodingFoundFor_TheoriesSome of unit

    val get_Some_enc : unit -> Enc.t M.mm

    exception NotEqTheory of unit

    val get_theory_enc_if_eq
      :  EConstr.t
      -> (EConstr.t -> bool M.mm)
      -> Enc.t M.mm

    val get_None_enc_if_eq : EConstr.t -> Enc.t M.mm
    val get_Some_enc_if_eq : EConstr.t -> Enc.t M.mm
  end

  module Weak : sig
    type t =
      | Option of Enc.t
      | Custom of Enc.t * Enc.t

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

    (* val api_bounds_to_model_bounds : Api.bounds_args -> Model.Info.Meta.bounds *)
    val the_bounds_args : Api.bounds_args ref
    val load_the_bounds_args : unit -> unit
  end

  module type X_Args = sig
    val primary_lts : Libnames.qualid
    val grefs : Names.GlobRef.t list
    val weak : Weak.t option
    val bounds : Api.bounds_args
  end

  module Graph : (T0 : sig
                    type key = Enc.t
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
      (D0 : sig
         type elt = V0.elt * Enc.Tree.t
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
        type elt = V.elt * Enc.Tree.t
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

      type elt = V.elt * Enc.Tree.t
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
      val state : V.elt -> Model.EdgeMap.key
      val states : unit -> Model.Partition.elt
      val terminals : unit -> Model.Partition.elt
      val label : A.key -> Model.Label.t
      val transitions : unit -> Model.Transitions.t
      val constructor_info : unit -> Model.Info.Meta.RocqLTS.t list M.mm
      val meta : unit -> Model.Info.Meta.t M.mm
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
  val fail_if_not_bisim : Model.Bisimilarity.result -> unit

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
      -> Model.Bisimilarity.t option M.mm

    val do_make_fsm
      :  Constrexpr.constr_expr * Libnames.qualid
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    val do_saturate
      :  Constrexpr.constr_expr * Libnames.qualid
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    val do_minimize
      :  Constrexpr.constr_expr * Libnames.qualid
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    val build_fsms
      :  rocq_args
      -> rocq_args
      -> Libnames.qualid list
      -> (Model.FSM.t * Model.FSM.t) M.mm

    val do_merge
      :  rocq_pair
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    val do_check_bisim
      :  rocq_pair
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    val run : Libnames.qualid list -> t -> Model.Bisimilarity.t option M.mm
  end
end

module Default : () ->
    module type of Make (Api.Defaults.Log) (Api.Defaults.Ctx) (Api.Defaults.Enc)
