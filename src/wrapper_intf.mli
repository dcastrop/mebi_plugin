(** The signatures of {!Wrapper}, declared once for its [.ml] and [.mli]. *)

module type S = sig
  type enc
  type node
  type tree
  type trees

  (** The monad and Rocq utilities over this encoding. *)
  module M : Rocq_monad_utils.S with type enc = enc and type tree = tree

  (** Where constructors' binders sit ({!Bindings}). *)
  module Bindings : Bindings.S with type 'a mm = 'a M.mm

  (** Constructors' explicit bindings for proofs ({!Constructor_bindings}). *)
  module ConstructorBindings :
    Constructor_bindings.S
    with type 'a mm = 'a M.mm
     and type ind = M.Ind.t
     and type instructions = Bindings.Instructions.t
     and type bindings = Bindings.t
     and type constrmap = Bindings.ConstrMap.t'

  (** The model: LTSs, FSMs and their algorithms ({!Model}). *)
  module Model :
    Model.S
    with type base = enc
     and type tree = tree
     and type trees = trees
     and type constructorbindings = ConstructorBindings.t

  (** The model printed with its Rocq terms ({!Decoder}). *)
  module Decode :
    Decoder.S
    with type enc = enc
     and type state = Model.State.t
     and type states = Model.State.Set.t
     and type partition = Model.Partition.t
     and type label = Model.Label.t
     and type labels = Model.Label.Set.t
     and type note = Model.Note.t
     and type annotation = Model.Annotation.t
     and type annotations = Model.Annotation.Set.t
     and type transition = Model.Transition.t
     and type transitions = Model.Transition.Set.t
     and type action = Model.Action.t
     and type actions = Model.Action.Set.t
     and type actionmap = Model.Action.Map.t'
     and type edgemap = Model.EdgeMap.t'
     and type rocqlts = Model.Info.Meta.RocqLTS.t
     and type info = Model.Info.t
     and type lts = Model.LTS.t
     and type fsm = Model.FSM.t
     and type result = Model.Bisimilarity.Result.t
     and type bisimilarity = Model.Bisimilarity.t

  (** The plugin's theory terms, by encoding ({!Theories_enc}). *)
  module Theory :
    Theories_enc.S
    with type enc = enc
     and type 'a mm = 'a M.mm
     and type 'a im = 'a M.mm

  (** Silent labels ({!Weak}). *)
  module Weak : Weak.S with type enc = enc

  (** The configuration loaded for a command ({!Config_loader}). *)
  module Config :
    Config_loader.S with type weak = Weak.t and type 'a mm = 'a M.mm

  (** [similarity r] is, for a bisimilarity check's result [r], the greatest
      weak simulation from FSM a to FSM b among the pairs reachable from
      their start states ({!Model.Product.simulation}), or [None] if either
      FSM has no start state. When an FSM is saturated on demand, that walk
      saturates state after state, so it needs [Set MeBi MaxGame <n>]
      and stays within it.

      @raise CErrors.UserError
        when an FSM is saturated on demand and no game bound is set, or the
        game is larger than it (raised here). *)
  val similarity : Model.Bisimilarity.t -> Model.Product.Pair.Set.t option

  (** The [MeBi Run] commands. *)
  module Command : sig

    (** A [MeBi Run] command: what to build or check, from which term and
        relation. *)
    type t =
      | MakeLTS of rocq_args
      | MakeFSM of rocq_args
      | Saturate of rocq_args
      | Minimize of rocq_args
      | Merge of rocq_pair
      | CheckBisim of rocq_pair
      | RunBisim of rocq_pair
      | CheckSim of rocq_pair
      | BenchmarkGraph of (rocq_args * (int * int))

    and rocq_args = Constrexpr.constr_expr * Libnames.qualid

    and rocq_pair =
      { a : rocq_args
      ; b : rocq_args
      }

    (** [run using x] runs the command [x], with the relations [using]
        available to constructors' premises, after loading the bounds and
        silent labels configured: a bisimilarity result for [CheckBisim] and
        [CheckSim], [None] otherwise.

        Raises as the command it runs, when run (propagated). *)
    val run : Libnames.qualid list -> t -> Model.Bisimilarity.t option M.mm
  end
end
