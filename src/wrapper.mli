module type S = sig
  type enc
  type node
  type tree
  type trees

  module M : Rocq_monad_utils.S with type enc = enc and type tree = tree
  module Bindings : Bindings.S with type 'a mm = 'a M.mm

  module ConstructorBindings :
    Constructor_bindings.S
    with type 'a mm = 'a M.mm
     and type ind = M.Ind.t
     and type instructions = Bindings.Instructions.t
     and type bindings = Bindings.t
     and type constrmap = Bindings.ConstrMap.t'

  module Model :
    Model.S
    with type base = enc
     and type tree = tree
     and type trees = trees
     and type constructorbindings = ConstructorBindings.t

  module Decode :
    Decoder.S
    with type enc = enc
     and type state = Model.State.t
     and type states = Model.States.t
     and type partition = Model.Partition.t
     and type label = Model.Label.t
     and type labels = Model.Labels.t
     and type note = Model.Note.t
     and type annotation = Model.Annotation.t
     and type annotations = Model.Annotations.t
     and type transition = Model.Transition.t
     and type transitions = Model.Transitions.t
     and type action = Model.Action.t
     and type actions = Model.Actions.t
     and type actionmap = Model.ActionMap.t'
     and type edgemap = Model.EdgeMap.t'
     and type rocqlts = Model.Info.Meta.RocqLTS.t
     and type info = Model.Info.t
     and type lts = Model.LTS.t
     and type fsm = Model.FSM.t
     and type result = Model.Bisimilarity.Result.t
     and type bisimilarity = Model.Bisimilarity.t

  module Theory :
    Theories_enc.S
    with type enc = enc
     and type 'a mm = 'a M.mm
     and type 'a im = 'a M.mm

  module Weak : Weak.S with type enc = enc

  module Config :
    Config_loader.S with type weak = Weak.t and type 'a mm = 'a M.mm

  val result_log
    :  ?decode:bool
    -> (module Json.S with type k = 'a)
    -> (module Json.S with type k = 'a)
    -> (module Json.S with type k = 'a)

  val handle_results
    :  Output.Kind.t
    -> string
    -> 'a
    -> (module Json.S with type k = 'a)
    -> unit

  val extract_lts
    :  Libnames.qualid
    -> Constrexpr.constr_expr
    -> Libnames.qualid list
    -> Weak.t option
    -> Model.LTS.t M.mm

  module Command : sig
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

module Make (Log : Logger.S) (Ctx : Rocq_context.S) (Enc : Encoding.S) :
  S
  with type enc = Enc.t
   and type node = Enc.Tree.Node.t
   and type tree = Enc.Tree.t
   and type trees = Enc.Trees.t

val make
  :  ?log:(unit -> (module Logger.S))
  -> ?enc:((module Logger.S) -> (module Encoding.S))
  -> ?ctx:(module Rocq_context.S)
  -> unit
  -> (module S)
