exception NothingToDo
exception NotImplemented

module Make : (Log : Logger.S) (Enc : Encoding.S) -> sig
  module Tree : module type of Enc_tree.Make (Log) (Enc)
  module Trees : module type of Enc_trees.Make (Log) (Tree)

  module W : sig
    include module type of
        Wrapper.Make (Log) (Rocq_context.Default) (Enc) (Tree) (Trees)

    val the_result : Model.Bisimilar.t ref option ref

    exception NoResultFound

    val get_the_result : unit -> Model.Bisimilar.t
    val get_fsm_a : ?saturated:bool -> unit -> Model.FSM.t
    val get_fsm_b : ?saturated:bool -> unit -> Model.FSM.t

    exception CannotOverrideResult of Model.Bisimilar.t

    val set_the_result : Model.Bisimilar.t -> unit

    exception BisimilarityResultNotFound

    val check_bisimilarity
      :  Libnames.qualid list
      -> Constrexpr.constr_expr * Libnames.qualid
      -> Constrexpr.constr_expr * Libnames.qualid
      -> unit

    module Decode : sig
      val enc : Enc.t -> EConstr.t
      val handle : Enc.t -> exn -> EConstr.t

      exception CouldNotDecode_State of Model.States.elt

      val state : Model.States.elt -> EConstr.t

      exception CouldNotDecode_Label of Model.Labels.elt

      val label : Model.Labels.elt -> EConstr.t

      exception CouldNotDecode_LTS_Constructor of Model.Info.Meta.RocqLTS.t

      val lts_constructor : Model.Info.Meta.RocqLTS.t -> EConstr.t
    end
  end

  val check_bisimilarity
    :  Libnames.qualid list
    -> Constrexpr.constr_expr * Libnames.qualid
    -> Constrexpr.constr_expr * Libnames.qualid
    -> unit

  module M = W.M
  module Model = W.Model
  module Decode = W.Decode

  module ApplicableConstructors : sig
    type t =
      { current : Tree.Node.t list option
      ; annotation : Model.Annotation.t option
      ; label : Model.Label.t
      ; destination : Model.EdgeMap.key
      }
  end

  module State : sig
    type t =
      | NewProof of (Constrexpr.constr_expr * Constrexpr.constr_expr)
      | WeakSim
      | Exists of Model.Transition.t option
      (* | GoalTransition of Transition.t *)
      | ApplyConstructors of ApplicableConstructors.t
      | Done

    val to_string : t -> string
  end

  type state =
    { p : Declare.Proof.t
    ; x : State.t
    }

  val the_state : state ref option ref

  exception NoStateFound

  val get_the_state : unit -> state ref
  val get_pstate : unit -> Declare.Proof.t
  val get_state : unit -> State.t
  val is_done : unit -> bool
  val set_the_state : Declare.Proof.t -> State.t -> unit
  val update_pstate : Declare.Proof.t -> unit
  val update_state : State.t -> unit

  val init
    :  Declare.Proof.t
    -> Constrexpr.constr_expr * Constrexpr.constr_expr
    -> unit

  module Tactic : sig
    type t =
      { this : tactic
      ; next : t option
      }

    and tactic =
      { get : unit Proofview.tactic
      ; msg : (Output_kind.t * string) option
      }

    val tactic
      :  ?level:Output_kind.t
      -> ?msg:string
      -> unit Proofview.tactic
      -> t

    val do_nothing : unit -> t
    val unpack : t -> unit Proofview.tactic
    val seq : t -> t -> t
    val empty : unit -> t

    exception EmptyTacticChain

    val chain : ?nonempty:bool -> t list -> t
  end

  module P : (_ : sig
                val gl : Proofview.Goal.t ref
              end)
      -> sig
    val gl : unit -> Proofview.Goal.t

    include module type of
        Rocq_monad_utils.Make
          (Log)
          (Rocq_context.Make (struct
               let env : unit -> Environ.env ref =
                 fun () -> ref (Proofview.Goal.env (gl ()))
               ;;

               let sigma : unit -> Evd.evar_map ref =
                 fun () -> ref (Proofview.Goal.sigma (gl ()))
               ;;
             end))
          (Enc)
          (Tree)

    val to_atomic : EConstr.t -> EConstr.t Rocq_utils.kind_pair mm
    val get_concl : unit -> EConstr.t
    val get_hyps : unit -> Rocq_utils.hyp list
    val get_hyp_name : Rocq_utils.hyp -> Names.Id.t
    val get_hyp_names : unit -> Names.Id.Set.t
    val next_name_of : Names.Id.Set.t -> Names.Id.t -> Names.Id.t
    val new_name_of_string : string -> Names.Id.t
    val new_cofix_name : unit -> Names.Id.t
    val new_H_name : unit -> Names.Id.t
    val get_all_cofix_hyp_names : unit -> Names.Id.Set.t
    val get_all_non_cofix_hyp_names : unit -> Names.Id.Set.t

    module Tacs : sig
      val inversion : Rocq_utils.hyp -> Tactic.t mm
      val subst_all : unit -> Tactic.t mm
      val simplify_concl : unit -> Tactic.t mm
      val simplify_hyp : Rocq_utils.hyp -> Tactic.t mm
      val simplify_hyps : unit -> Tactic.t mm
      val simplify_all : unit -> Tactic.t mm
      val simplify_and_subst_all : unit -> Tactic.t mm
      val cofix : unit -> Tactic.t mm
      val trivial : ?msg:string -> unit -> Tactic.t mm
      val intros_all : unit -> Tactic.t mm
      val intro_as : string -> Tactic.t mm
      val apply : EConstr.t -> Tactic.t mm
      val apply_Pack_sim : unit -> Tactic.t mm
      val apply_In_sim : unit -> Tactic.t mm
      val apply_wk_none : unit -> Tactic.t mm
      val apply_rt1n_refl : unit -> Tactic.t mm
      val apply_rt1n_trans : unit -> Tactic.t mm
      val eapply : EConstr.t -> Tactic.t mm
      val eapply_wk_some : unit -> Tactic.t mm
      val eapply_rt1n_refl : unit -> Tactic.t mm
      val eapply_rt1n_trans : unit -> Tactic.t mm

      exception CannotUnfoldConstr of Constr.t

      val unfold_constr : ?in_hyp:Rocq_utils.hyp -> Constr.t -> Tactic.t mm
      val unfold_econstr : ?in_hyp:Rocq_utils.hyp -> EConstr.t -> Tactic.t mm

      val unfold_constrexpr
        :  ?in_hyp:Rocq_utils.hyp
        -> Constrexpr.constr_expr
        -> Tactic.t mm

      val unfold_opt_constrexpr_list
        :  ?in_hyp:Rocq_utils.hyp
        -> Constrexpr.constr_expr list
        -> Tactic.t option mm

      val unfold_silent : unit -> Tactic.t mm
      val unfold_silent1 : unit -> Tactic.t mm
    end

    module Theory : sig
      val is_theory : EConstr.t -> EConstr.t -> bool mm
      val is_exists : EConstr.t -> bool mm
      val is_weak_sim : EConstr.t -> bool mm
      val is_weak : EConstr.t -> bool mm
      val is_tau : EConstr.t -> bool mm
      val is_silent : EConstr.t -> bool mm
      val is_silent1 : EConstr.t -> bool mm
      val is_LTS : EConstr.t -> bool mm
      val is_None : EConstr.t -> bool mm
      val is_Some : EConstr.t -> bool mm
      val get_theory_enc : (EConstr.t -> bool mm) -> Enc.t M.mm

      exception NoEncodingFoundFor_TheoriesNone

      val get_None_enc : unit -> Enc.t M.mm

      exception NoEncodingFoundFor_TheoriesSome

      val get_Some_enc : unit -> Enc.t M.mm

      exception NotEqTheory

      val get_theory_enc_if_eq
        :  EConstr.t
        -> (EConstr.t -> bool mm)
        -> Enc.t M.mm

      val get_None_enc_if_eq : EConstr.t -> Enc.t M.mm
      val get_Some_enc_if_eq : EConstr.t -> Enc.t M.mm

      exception FSM_HasNoSilentLabel of Model.FSM.t

      val is_fsm_silent_label : EConstr.t -> Model.FSM.t -> bool

      exception FSM_HasNoVisibleLabel of Model.FSM.t

      val is_fsm_visible_label : EConstr.t -> Model.FSM.t -> bool

      exception FSM_HasNoWeakLabels of Model.FSM.t

      val is_fsm_weak_labels : EConstr.t -> Model.FSM.t -> bool

      exception FSM_HasNoConstructors of Model.FSM.t

      val is_fsm_constructor : EConstr.t -> Model.FSM.t -> bool
    end

    module ReModel : sig
      exception
        CouldNotFind_State of
          { x : EConstr.t
          ; states : Model.States.t
          }

      val state : EConstr.t -> Model.States.t -> Model.States.elt M.mm

      exception
        CouldNotFind_Label of
          { x : EConstr.t
          ; alphabet : Model.Labels.t
          }

      val label : EConstr.t -> Model.Labels.t -> Model.Labels.elt M.mm

      exception
        CouldNotFind_Transition of
          { from : Model.State.t
          ; goto : Model.State.t
          ; label : Model.Label.t
          ; edges : Model.EdgeMap.t'
          }

      val transition
        :  Model.State.t
        -> Model.State.t
        -> Model.Label.t
        -> Model.EdgeMap.t'
        -> Model.Transition.t
    end

    module Concl : sig
      val is_weak_sim : unit -> bool mm
      val is_exists : unit -> bool mm
    end

    module Hyps : sig
      val get_cofixes : unit -> Rocq_utils.hyp list
      val can_solve_concl_cofix : unit -> bool mm
      val clear_non_cofix : unit -> Tactic.t
    end

    val handle_new_cofix : unit -> Tactic.t mm

    exception StateNotImplemented of State.t
    exception StateCouldNothandle of State.t
    exception SkipNewProof

    val handle_new_proof
      :  Constrexpr.constr_expr * Constrexpr.constr_expr
      -> Tactic.t mm

    val handle_weaksim : unit -> Tactic.t mm
    val handle_exists : Model.Transition.t option -> Tactic.t mm

    (* val handle_goal_transition : Transition.t -> Tactic.t mm *)
    val handle_apply_constructors : ApplicableConstructors.t -> Tactic.t mm
    val handle_state : unit -> Tactic.t mm
    val step : unit -> Tactic.t
  end

  val get_updated_pstate : unit Proofview.tactic -> Declare.Proof.t
  val step : Declare.Proof.t -> Declare.Proof.t
end

(* val the_proof_solver : (module S) ref option ref *)
val reset_the_proof_solver : unit -> unit

exception NoProofSolverFound

(* val get_the_proof_solver : unit -> (module S) ref *)
(* val new_proof_solver : unit -> (module S) ref *)
val is_done : unit -> bool

exception BisimilarityResultNotFound

val init
  :  Declare.Proof.t
  -> Libnames.qualid list
  -> Constrexpr.constr_expr * Libnames.qualid
  -> Constrexpr.constr_expr * Libnames.qualid
  -> Declare.Proof.t

val step : Declare.Proof.t -> Declare.Proof.t
val solve : ?bound:int -> Declare.Proof.t -> Declare.Proof.t
