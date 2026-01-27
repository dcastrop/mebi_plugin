module type S = sig
  module M : Rocq_monad.Utils.S

  module Model : sig
    module Tree : sig
      module Enc : sig
        type t = M.B.key

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

      module type STreeNode = sig
        type t = Enc.t * int

        val to_string : t -> string
      end

      module TreeNode : sig
        type t = Enc.t * int

        val to_string : t -> string
      end

      type 'a tree = Node of 'a * 'a tree list
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

    module Enc : sig
      type t = Tree.Enc.t

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

    module Trees : sig
      type elt = Tree.t
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

    module type SState = sig
      type t = { term : Enc.t; pp : string option }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
      val to_string : t -> string
    end

    module State : SState

    module type SStates = sig
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

      type t = S.t

      val to_string : t -> string
    end

    module States : SStates

    module type SLabel = sig
      type t = { term : Enc.t; is_silent : bool option }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
      val to_string : t -> string
      val is_silent : t -> bool
    end

    module Label : SLabel

    module type SLabels = sig
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

      type t = S.t

      val to_string : t -> string
    end

    module Labels : SLabels

    module type SNote = sig
      type t = {
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

    module Note : SNote

    module type SAnnotation = sig
      type t = { this : Note.t; next : t option }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val to_string : t -> string
      val is_empty : t -> bool
      val length : t -> int
      val shorter : t -> t -> t
      val exists : Note.t -> t -> bool
      val exists_label : Label.t -> t -> bool
      val append : Note.t -> t -> t
      val last : t -> Note.t

      exception CannotDropLastOfSingleton of t

      val drop_last : t -> t
    end

    module Annotation : SAnnotation

    module type STransition = sig
      type t = {
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

    module type SAnnotations = sig
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

      type t = S.t

      val to_string : t -> string
    end

    module Annotations : SAnnotations
    module Transition : STransition

    module type STransitions = sig
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

      type t = S.t

      val to_string : t -> string
    end

    module Transitions : STransitions

    module type SAction = sig
      type t = {
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

    module Action : SAction

    module type SActions = sig
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

      type t = S.t

      val labelled : t -> Label.t -> t
      val to_string : t -> string
    end

    module Actions : SActions

    module type SActionMap = sig
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

      type t = States.t M.t

      val update : t -> Action.t -> States.t -> unit
      val get_destinations : t -> States.t
      val to_string : t -> string
    end

    module ActionMap : SActionMap

    module type SEdge = sig
      type t = {
        from : State.t;
        goto : State.t;
        action : Action.t;
      }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val is_silent : t -> bool
      val to_string : t -> string
    end

    module Edge : SEdge

    module type SEdges = sig
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

      type t = S.t

      val labelled : t -> Label.t -> t
      val to_string : t -> string
    end

    module Edges : SEdges

    module type SEdgeMap = sig
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

      type t = ActionMap.t M.t

      val update : t -> State.t -> Action.t -> States.t -> unit
      val get_destinations : t -> State.t -> States.t
      val get_actions : t -> State.t -> Actions.t
      val get_edges : t -> State.t -> Edges.t
      val to_string : t -> string
    end

    module EdgeMap : SEdgeMap

    module type SPartition = sig
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

      type t = S.t

      val get_reachable : t -> State.t -> EdgeMap.t -> t
      val to_string : t -> string
    end

    module Partition : SPartition

    module type SInfo = sig
      type t = { meta : meta option; weak_labels : Labels.t }

      and meta = {
        is_complete : bool;
        is_merged : bool;
        bounds : bounds;
        lts : lts list;
      }

      and bounds = States of int | Transitions of int

      and lts = {
        enc : Enc.t;
        constructors : Rocq_bindings.constructor list;
      }

      val to_string : t -> string
    end

    module Info : SInfo

    module type SLTS = sig
      type t = {
        init : State.t option;
        terminals : States.t;
        alphabet : Labels.t;
        states : States.t;
        transitions : Transitions.t;
        info : Info.t;
      }

      val to_string : t -> string
    end

    module LTS : SLTS

    module type SFSM = sig
      type t = {
        init : State.t option;
        terminals : States.t;
        alphabet : Labels.t;
        states : States.t;
        edges : EdgeMap.t;
        info : Info.t;
      }

      val to_string : t -> string
    end

    module FSM : SFSM

    module type SConvert = sig
      val transitions_to_edgemap : Transitions.t -> EdgeMap.t
      val lts_to_fsm : LTS.t -> FSM.t
    end

    module Convert : SConvert
  end
end

module Make : (_ : Rocq_context.SRocq_context)
  (E : Encoding.SEncoding)
  -> sig
  module M : sig
    module M : sig
      module Context : Rocq_context.SRocq_context

      module BiEnc : sig
        module Enc : sig
          type t = E.t

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

        module FwdMap : sig
          type key = Evd.econstr
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

        module BckMap : sig
          type key = E.t
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

        type fwdmap = E.t FwdMap.t
        type bckmap = Evd.econstr BckMap.t
        type maps = { fwd : fwdmap; bck : bckmap }

        val the_maps : unit -> maps ref
        val reset : unit -> unit
        val encode : Evd.econstr -> Enc.t
        val encoded : Evd.econstr -> bool

        exception CannotDecode of Enc.t

        val decode : Enc.t -> Evd.econstr
        val decode_opt : Enc.t -> Evd.econstr option
        val encode_map : 'a FwdMap.t -> 'a BckMap.t
        val decode_map : 'a BckMap.t -> 'a FwdMap.t
        val to_list : unit -> (Enc.t * Evd.econstr) list

        val make_hashtbl :
          (module Hashtbl.S with type key = Enc.t)

        val make_set : (module Set.S with type elt = Enc.t)
      end

      module FwdMap : sig
        type key = Evd.econstr
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

      module BckMap : sig
        type key = E.t
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

      type fwdmap = BiEnc.fwdmap
      type bckmap = BiEnc.bckmap
      type maps = BiEnc.maps

      val encode : Evd.econstr -> BiEnc.Enc.t
      val encoded : Evd.econstr -> bool
      val decode : BiEnc.Enc.t -> Evd.econstr
      val decode_opt : BiEnc.Enc.t -> Evd.econstr option

      val bienc_to_list :
        unit -> (BiEnc.Enc.t * Evd.econstr) list

      type 'a mm = wrapper ref -> 'a in_wrapper

      and wrapper = {
        ctx : Rocq_context.t ref;
        maps : maps ref;
      }

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

      val get_ctx : wrapper ref -> Rocq_context.t in_wrapper
      val get_env : wrapper ref -> Environ.env in_wrapper
      val get_sigma : wrapper ref -> Evd.evar_map in_wrapper
      val get_maps : wrapper ref -> BiEnc.maps in_wrapper
      val get_fwdmap : wrapper ref -> fwdmap in_wrapper
      val get_bckmap : wrapper ref -> bckmap in_wrapper

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

      module Syntax : SYNTAX

      val fstring :
        (Environ.env -> Evd.evar_map -> 'a -> string) ->
        'a ->
        string

      module Tree : sig
        module Enc : sig
          type t = E.t

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

        module type STreeNode = sig
          type t = E.t * int

          val to_string : t -> string
        end

        module TreeNode : sig
          type t = E.t * int

          val to_string : t -> string
        end

        type 'a tree = Node of 'a * 'a tree list
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

      module type SConstructor = sig
        module Tree : sig
          module Enc : sig
            type t = E.t

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

          module type STreeNode = sig
            type t = E.t * int

            val to_string : t -> string
          end

          module TreeNode : sig
            type t = E.t * int

            val to_string : t -> string
          end

          type 'a tree = Node of 'a * 'a tree list
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

        type t = Evd.econstr * Evd.econstr * Tree.t

        val to_string :
          Environ.env -> Evd.evar_map -> t -> string
      end

      module Constructor : sig
        module Tree : sig
          module Enc : sig
            type t = E.t

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

          module type STreeNode = sig
            type t = E.t * int

            val to_string : t -> string
          end

          module TreeNode : sig
            type t = E.t * int

            val to_string : t -> string
          end

          type 'a tree = Node of 'a * 'a tree list
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

        type t = Evd.econstr * Evd.econstr * Tree.t

        val to_string :
          Environ.env -> Evd.evar_map -> t -> string
      end

      val make_state_tree_pair_set :
        (module Set.S
           with type elt = BiEnc.Enc.t * Constructor.Tree.t)
    end

    module Enc : sig
      type t = E.t

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

    module BiEnc : sig
      module Enc : sig
        type t = E.t

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

      module FwdMap : sig
        type key = Evd.econstr
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

      module BckMap : sig
        type key = E.t
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

      type fwdmap = E.t FwdMap.t
      type bckmap = Evd.econstr BckMap.t
      type maps = { fwd : fwdmap; bck : bckmap }

      val the_maps : unit -> maps ref
      val reset : unit -> unit
      val encode : Evd.econstr -> Enc.t
      val encoded : Evd.econstr -> bool

      exception CannotDecode of Enc.t

      val decode : Enc.t -> Evd.econstr
      val decode_opt : Enc.t -> Evd.econstr option
      val encode_map : 'a FwdMap.t -> 'a BckMap.t
      val decode_map : 'a BckMap.t -> 'a FwdMap.t
      val to_list : unit -> (Enc.t * Evd.econstr) list

      val make_hashtbl :
        (module Hashtbl.S with type key = Enc.t)

      val make_set : (module Set.S with type elt = Enc.t)
    end

    module F : sig
      type key = Evd.econstr
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

    module B : sig
      type key = E.t
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

    module Syntax : M.SYNTAX

    module Tree : sig
      module Enc : sig
        type t = E.t

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

      module type STreeNode = sig
        type t = E.t * int

        val to_string : t -> string
      end

      module TreeNode : sig
        type t = E.t * int

        val to_string : t -> string
      end

      type 'a tree = Node of 'a * 'a tree list
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
      module Tree : sig
        module Enc : sig
          type t = E.t

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

        module type STreeNode = sig
          type t = E.t * int

          val to_string : t -> string
        end

        module TreeNode : sig
          type t = E.t * int

          val to_string : t -> string
        end

        type 'a tree = Node of 'a * 'a tree list
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

      type t = Evd.econstr * Evd.econstr * Tree.t

      val to_string :
        Environ.env -> Evd.evar_map -> t -> string
    end

    type 'a mm = 'a M.mm
    type 'a in_wrapper = 'a M.in_wrapper
    type wrapper = M.wrapper

    val env : unit -> Environ.env mm
    val sigma : unit -> Evd.evar_map mm
    val run : ?reset_encoding:bool -> 'a mm -> 'a
    val return : 'a -> 'a mm

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

    val fresh_evar : Rocq_utils.evar_source -> Evd.econstr mm
    val econstr_eq : Evd.econstr -> Evd.econstr -> bool mm
    val econstr_normalize : Evd.econstr -> Evd.econstr mm
    val econstr_is_evar : Evd.econstr -> bool mm

    val econstr_to_constr :
      ?abort_on_undefined_evars:bool ->
      Evd.econstr ->
      Constr.t mm

    val econstr_to_constr_opt :
      Evd.econstr -> Constr.t option mm

    val econstr_kind :
      Evd.econstr -> Rocq_utils.econstr_kind mm

    module type SStrfy = sig
      val mm :
        (Environ.env -> Evd.evar_map -> 'a -> string) ->
        'a ->
        string

      val econstr : Evd.econstr -> string
      val econstr_rel_decl : EConstr.rel_declaration -> string
    end

    module Strfy : SStrfy

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
  end

  module Model : sig
    module Tree : sig
      module Enc : sig
        type t = E.t

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

      module type STreeNode = sig
        type t = E.t * int

        val to_string : t -> string
      end

      module TreeNode : sig
        type t = E.t * int

        val to_string : t -> string
      end

      type 'a tree = Node of 'a * 'a tree list
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

    module Enc : sig
      type t = E.t

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

    module Trees : sig
      type elt = Tree.t
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

    module type SState = sig
      type t = { term : E.t; pp : string option }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
      val to_string : t -> string
    end

    module State : SState

    module type SStates = sig
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

      type t = S.t

      val to_string : t -> string
    end

    module States : SStates

    module type SLabel = sig
      type t = { term : E.t; is_silent : bool option }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
      val to_string : t -> string
      val is_silent : t -> bool
    end

    module Label : SLabel

    module type SLabels = sig
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

      type t = S.t

      val to_string : t -> string
    end

    module Labels : SLabels

    module type SNote = sig
      type t = {
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

    module Note : SNote

    module type SAnnotation = sig
      type t = { this : Note.t; next : t option }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val to_string : t -> string
      val is_empty : t -> bool
      val length : t -> int
      val shorter : t -> t -> t
      val exists : Note.t -> t -> bool
      val exists_label : Label.t -> t -> bool
      val append : Note.t -> t -> t
      val last : t -> Note.t

      exception CannotDropLastOfSingleton of t

      val drop_last : t -> t
    end

    module Annotation : SAnnotation

    module type STransition = sig
      type t = {
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

    module type SAnnotations = sig
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

      type t = S.t

      val to_string : t -> string
    end

    module Annotations : SAnnotations
    module Transition : STransition

    module type STransitions = sig
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

      type t = S.t

      val to_string : t -> string
    end

    module Transitions : STransitions

    module type SAction = sig
      type t = {
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

    module Action : SAction

    module type SActions = sig
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

      type t = S.t

      val labelled : t -> Label.t -> t
      val to_string : t -> string
    end

    module Actions : SActions

    module type SActionMap = sig
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

      type t = States.t M.t

      val update : t -> Action.t -> States.t -> unit
      val get_destinations : t -> States.t
      val to_string : t -> string
    end

    module ActionMap : SActionMap

    module type SEdge = sig
      type t = {
        from : State.t;
        goto : State.t;
        action : Action.t;
      }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val is_silent : t -> bool
      val to_string : t -> string
    end

    module Edge : SEdge

    module type SEdges = sig
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

      type t = S.t

      val labelled : t -> Label.t -> t
      val to_string : t -> string
    end

    module Edges : SEdges

    module type SEdgeMap = sig
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

      type t = ActionMap.t M.t

      val update : t -> State.t -> Action.t -> States.t -> unit
      val get_destinations : t -> State.t -> States.t
      val get_actions : t -> State.t -> Actions.t
      val get_edges : t -> State.t -> Edges.t
      val to_string : t -> string
    end

    module EdgeMap : SEdgeMap

    module type SPartition = sig
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

      type t = S.t

      val get_reachable : t -> State.t -> EdgeMap.t -> t
      val to_string : t -> string
    end

    module Partition : SPartition

    module type SInfo = sig
      type t = { meta : meta option; weak_labels : Labels.t }

      and meta = {
        is_complete : bool;
        is_merged : bool;
        bounds : bounds;
        lts : lts list;
      }

      and bounds = States of int | Transitions of int

      and lts = {
        enc : E.t;
        constructors : Rocq_bindings.constructor list;
      }

      val to_string : t -> string
    end

    module Info : SInfo

    module type SLTS = sig
      type t = {
        init : State.t option;
        terminals : States.t;
        alphabet : Labels.t;
        states : States.t;
        transitions : Transitions.t;
        info : Info.t;
      }

      val to_string : t -> string
    end

    module LTS : SLTS

    module type SFSM = sig
      type t = {
        init : State.t option;
        terminals : States.t;
        alphabet : Labels.t;
        states : States.t;
        edges : EdgeMap.t;
        info : Info.t;
      }

      val to_string : t -> string
    end

    module FSM : SFSM

    module type SConvert = sig
      val transitions_to_edgemap : Transitions.t -> EdgeMap.t
      val lts_to_fsm : LTS.t -> FSM.t
    end

    module Convert : SConvert
  end
end
