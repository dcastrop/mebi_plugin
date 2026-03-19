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
    val get_theory_enc : (EConstr.t -> bool M.mm) -> enc M.mm

    exception NoEncodingFoundFor_TheoriesNone of unit

    val get_None_enc : unit -> enc M.mm

    exception NoEncodingFoundFor_TheoriesSome of unit

    val get_Some_enc : unit -> enc M.mm

    exception NotEqTheory of unit

    val get_theory_enc_if_eq : EConstr.t -> (EConstr.t -> bool M.mm) -> enc M.mm
    val get_None_enc_if_eq : EConstr.t -> enc M.mm
    val get_Some_enc_if_eq : EConstr.t -> enc M.mm
  end

  module Weak : Weak.S with type enc = enc

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

  module Graph : (T0 : Hashtbl.S with type key = enc)
      (V0 : Set.S with type elt = enc)
      (D0 : Set.S with type elt = enc * tree)
      (X : X_Args)
      -> sig
    module V : sig
      include Set.S with type elt = enc

      val to_string : t -> string
    end

    module D : sig
      include Set.S with type elt = enc * tree

      val to_string : t -> string
    end

    module A : sig
      include Hashtbl.S with type key = Model.Action.t

      type t' = D.t t

      val size : t' -> int
      val update : t' -> Model.Action.t -> D.t -> unit
      val to_string : t' -> string
    end

    module T : sig
      include Hashtbl.S with type key = enc

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
  val fail_if_not_bisim : Model.Bisimilarity.Result.t -> unit

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
