module Make : (Log : Logger.S) (Enc : Encoding.S) -> sig
  module State : module type of Model_state.Make (Log) (Enc)
  module States : module type of Model_states.Make (Log) (State)
  module Label : module type of Model_label.Make (Log) (Enc)
  module Labels : module type of Model_labels.Make (Log) (Label)
  module Tree : module type of Enc_tree.Make (Log) (Enc)
  module Trees : module type of Enc_trees.Make (Log) (Tree)

  module Note :
      module type of
        Model_annotation_note.Make (Log) (State) (Label) (Tree) (Trees)

  module Annotation :
      module type of Model_annotation.Make (Log) (State) (Label) (Note)

  module Annotations :
      module type of Model_annotations.Make (Log) (Label) (Note) (Annotation)

  module Transition :
      module type of
        Model_transition.Make (Log) (State) (Label) (Tree) (Note) (Annotation)

  module Transitions :
      module type of
        Model_transitions.Make (Log) (State) (Label) (Labels) (Tree) (Note)
          (Annotation)
          (Transition)

  module Action :
      module type of
        Model_action.Make (Log) (Label) (Tree) (Trees) (Note) (Annotation)

  module Actions :
      module type of
        Model_actions.Make (Log) (Label) (Labels) (Tree) (Trees) (Note)
          (Annotation)
          (Action)

  module ActionPair :
      module type of
        Model_actionpair.Make (Log) (State) (States) (Label) (Tree) (Trees)
          (Note)
          (Annotation)
          (Action)

  module ActionPairs :
      module type of
        Model_actionpairs.Make (Log) (State) (States) (Action) (ActionPair)

  module ActionMap :
      module type of
        Model_actionmap.Make (Log) (State) (States) (Label) (Labels) (Tree)
          (Trees)
          (Note)
          (Annotation)
          (Action)
          (Actions)
          (ActionPair)
          (ActionPairs)

  module Edge : module type of Model_edge.Make (Log) (State) (Label) (Action)

  module Edges :
      module type of Model_edges.Make (Log) (State) (Label) (Action) (Edge)

  module EdgeMap :
      module type of
        Model_edgemap.Make (Log) (State) (States) (Label) (Labels) (Action)
          (Actions)
          (ActionPair)
          (ActionPairs)
          (ActionMap)
          (Edge)
          (Edges)

  module Partition :
      module type of
        Model_state_partition.Make (Log) (State) (States) (Label) (Labels)
          (Action)
          (Actions)
          (ActionPair)
          (ActionPairs)
          (ActionMap)
          (Edge)
          (Edges)
          (EdgeMap)

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
      { enc : Enc.t
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
    module Log : sig
      module Config : sig
        val get : Output_config.t ref
        val reset : unit -> unit
        val enable_output : unit -> unit
        val disable_output : unit -> unit
        val configure_output : Output_kind.t -> bool -> unit

        val do_output
          :  ?__FUNCTION__:string
          -> ?prefix:string option
          -> ?override:bool
          -> Output_kind.t
          -> string
          -> unit

        val show_mode : unit -> unit
        val show_enabled : unit -> unit
        val show_kind : Output_kind.t -> unit
      end

      val enabled : bool ref
      val prefix : string option
      val debug : ?__FUNCTION__:string -> string -> unit
      val info : ?__FUNCTION__:string -> string -> unit
      val notice : ?__FUNCTION__:string -> string -> unit
      val warning : ?__FUNCTION__:string -> string -> unit
      val error : ?__FUNCTION__:string -> string -> unit
      val trace : ?__FUNCTION__:string -> string -> unit
      val result : ?__FUNCTION__:string -> string -> unit
      val show : ?__FUNCTION__:string -> string -> unit

      val thing
        :  ?__FUNCTION__:string
        -> ?args:Utils.Strfy.style_args
        -> Output_kind.t
        -> string
        -> 'a
        -> 'a Utils.Strfy.to_string
        -> unit

      val things
        :  ?__FUNCTION__:string
        -> ?args:Utils.Strfy.style_args
        -> Output_kind.t
        -> string
        -> 'a list
        -> 'a Utils.Strfy.to_string
        -> unit

      val option
        :  ?__FUNCTION__:string
        -> ?args:Utils.Strfy.style_args
        -> Output_kind.t
        -> string
        -> 'a option
        -> 'a Utils.Strfy.to_string
        -> unit

      val options
        :  ?__FUNCTION__:string
        -> ?args:Utils.Strfy.style_args
        -> Output_kind.t
        -> string
        -> 'a list option
        -> 'a Utils.Strfy.to_string
        -> unit
    end

    module WIP : sig
      type t =
        { from : State.t
        ; via : Label.t
        ; trees : Trees.t
        }

      val to_string : t -> string
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

      val to_string : t -> string
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
      val to_string : t -> string
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

    val edge_action_destinations : data -> State.t -> States.t -> ActionPairs.t

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
