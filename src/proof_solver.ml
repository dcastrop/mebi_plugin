exception NothingToDo of unit

module Make (Log : Logger.SLogger) (E : Encoding.SEncoding) = struct
  (** [W] is for running the main part of the algorithm (pre-proof). *)
  module W = struct
    include Wrapper.Make (Log) (Rocq_context.Default) (E)

    let the_result : Model.Bisimilar.t ref option ref = ref None

    exception NoResultFound of unit

    let get_the_result () : Model.Bisimilar.t =
      match !the_result with None -> raise (NoResultFound ()) | Some x -> !x
    ;;

    let get_fsm_a ?(saturated : bool = false) () : Model.FSM.t =
      if saturated
      then (get_the_result ()).fsm_a.saturated
      else (get_the_result ()).fsm_a.original
    ;;

    let get_fsm_b ?(saturated : bool = false) () : Model.FSM.t =
      if saturated
      then (get_the_result ()).fsm_b.saturated
      else (get_the_result ()).fsm_b.original
    ;;

    exception CannotOverrideResult of Model.Bisimilar.t

    let set_the_result (x : Model.Bisimilar.t) : unit =
      match !the_result with
      | None -> the_result := Some (ref x)
      | Some y -> raise (CannotOverrideResult !y)
    ;;

    exception BisimilarityResultNotFound of unit

    let check_bisimilarity
          (refs : Libnames.qualid list)
          (a : Constrexpr.constr_expr * Libnames.qualid)
          (b : Constrexpr.constr_expr * Libnames.qualid)
      : unit
      =
      let r : Model.Bisimilar.t option =
        Command.run refs (Command.CheckBisim { a; b })
        |> M.run ~reset_encoding:true
      in
      match r with
      | None -> raise (BisimilarityResultNotFound ())
      | Some r -> set_the_result r
    ;;

    (** [Decode] handles obtaining [EConstr.t] from [module M]. *)
    module Decode = struct
      let enc (x : M.Enc.t) : EConstr.t = M.decode x

      let handle (x : M.Enc.t) (e : exn) : EConstr.t =
        try enc x with M.CannotDecode _ -> raise e
      ;;

      exception CouldNotDecode_State of Model.State.t

      let state (x : Model.State.t) : EConstr.t =
        handle x.term (CouldNotDecode_State x)
      ;;

      exception CouldNotDecode_Label of Model.Label.t

      let label (x : Model.Label.t) : EConstr.t =
        handle x.term (CouldNotDecode_Label x)
      ;;

      exception CouldNotDecode_LTS_Constructor of Model.Info.lts

      let lts_constructor (x : Model.Info.lts) : EConstr.t =
        handle x.enc (CouldNotDecode_LTS_Constructor x)
      ;;
    end
  end

  let check_bisimilarity
    :  Libnames.qualid list
    -> Constrexpr.constr_expr * Libnames.qualid
    -> Constrexpr.constr_expr * Libnames.qualid
    -> unit
    =
    W.check_bisimilarity
  ;;

  module M = W.M
  module Model = W.Model
  module Decode = W.Decode

  (***********************************************************************)

  module Transition = struct
    module Opt = struct
      (** [t] is similar to [Model.Transition.t] {!type:Model.Transition.t} but where [goto] and [label] are optional.
          @see 'Model.ml' for {!type:Model.Transition.t}. *)
      type t =
        { from : W.Model.State.t
        ; goto : W.Model.State.t option
        ; label : W.Model.Label.t option
        ; annotation : W.Model.Annotation.t option
        ; constructor_tree : W.Model.Tree.t
        }
    end

    type t =
      { hyp : W.Model.Transition.t
      ; goal : Opt.t
      }
  end

  module ApplicableConstructors = struct
    type t =
      { current : W.Model.Tree.TreeNode.t list
      ; annotation : W.Model.Annotation.t option
      ; destination : W.Model.State.t
      }
  end

  (** internal proof state *)
  module State = struct
    (** [t] represents the internal state-machine used to solve proofs.
        (Note: We are required to split some of the states over several steps since we need to update the proof iteratively in order to apply what we need to apply. E.g., it is easier to apply the constructors one after the other, since we require the proof to be updated by the previous appliction in order to apply the next.)
        - [NewProof] the start state. We unfold any terms that we can before proceeding to [WeakSim].
        - [WeakSim] either: (i) check if can be solved by cofix in hyps, or (ii) create new cofix. Either stays in [WeakSim] or proceeds to [Exists], or [Done].
        - [Exists] means that the conclusion begins with an [exists a n2] (where [n2] is some state reached from [n] after taking action [a]). Either (i) we stay in [Exists] to invert the hypothesis, or (ii) we proceed to [GoalTransition] after extracting the transition made by fsm 1 from the hyps (possible requiring inversion beforehand), and applying [ex_intro] and [split] tactics to the conclusion.
        - [GoalTransition] is where we determine which transition fsm 2 will make in response to the one made by fsm 1 (in the hyps). We instantiate the necessary values for the label [a] and goto state [n2] and select the constructors to apply. Then proceed to [ApplyConstructors].
        - [ApplyConstructors] is for applying the constructors we know we need to apply in order for fsm 2 to reach a state that is bisimilar to that reached by fsm 1.
        - [Done] means the proof is finished. *)
    type t =
      | NewProof of (Constrexpr.constr_expr * Constrexpr.constr_expr)
      | WeakSim
      | Exists
      | GoalTransition of Transition.t
      | ApplyConstructors of ApplicableConstructors.t
      | Done

    let to_string : t -> string = function
      | NewProof _ -> "NewProof"
      | WeakSim -> "WeakSim"
      | Exists -> "Exists"
      | GoalTransition _ -> "GoalTransition"
      | ApplyConstructors _ -> "ApplyConstructors"
      | Done -> "Done"
    ;;
  end

  (* TODO: rethink if we need to store the proofstate here *)
  type state =
    { p : Declare.Proof.t
    ; x : State.t
    }

  let the_state : state ref option ref = ref None

  exception NoStateFound of unit

  let get_the_state () : state ref =
    match !the_state with None -> raise (NoStateFound ()) | Some x -> x
  ;;

  let get_pstate () : Declare.Proof.t = !(get_the_state ()).p
  let get_state () : State.t = !(get_the_state ()).x

  let log_state () : unit =
    Log.thing Debug "proofstate =>" (get_state ()) (Of State.to_string)
  ;;

  (** [is_done ()] is true if the state [x] is [Done], else checks if [p] is done (via [Proof.is_done]).
  *)
  let is_done () : bool =
    match !(get_the_state ()) with
    | { p; x = Done } -> true
    | { p; x } -> Proof.is_done (Declare.Proof.get p)
  ;;

  let set_the_state (pstate : Declare.Proof.t) (x : State.t) : unit =
    the_state := Some (ref { p = pstate; x })
  ;;

  let update_pstate (pstate : Declare.Proof.t) : unit =
    the_state := Some (ref { !(get_the_state ()) with p = pstate })
  ;;

  let update_state (state : State.t) : unit =
    the_state := Some (ref { !(get_the_state ()) with x = state })
  ;;

  let init
        (pstate : Declare.Proof.t)
        (x : Constrexpr.constr_expr * Constrexpr.constr_expr)
    : unit
    =
    set_the_state pstate (NewProof x)
  ;;

  (* let get_gl () : Proofview.Goal.t = !(get_the_state ()).p *)

  (***********************************************************************)

  module Tactic = struct
    type t =
      { this : tactic
      ; next : t option
      }

    and tactic =
      { get : unit Proofview.tactic
      ; msg : (Output_kind.t * string) option
      }

    (** [tactic ?msg tactic] *)
    let tactic
          ?(level : Output_kind.t = Info)
          ?(msg : string option)
          (x : unit Proofview.tactic)
      : t
      =
      { this =
          { msg = Option.cata (fun m -> Some (level, m)) None msg; get = x }
      ; next = None
      }
    ;;

    let do_nothing () : t =
      tactic ~level:Debug ~msg:"(skip)" (Proofview.tclUNIT ())
    ;;

    let unpack (x : tactic) : unit Proofview.tactic =
      let () =
        match x.msg with
        | None -> ()
        | Some (Debug, z) -> Log.debug ~__FUNCTION__ z
        | Some (Info, z) -> Log.info ~__FUNCTION__ z
        | Some (Notice, z) -> Log.notice ~__FUNCTION__ z
        | Some (Warning, z) -> Log.warning ~__FUNCTION__ z
        | Some (Error, z) -> Log.error ~__FUNCTION__ z
        | Some (Trace, z) -> Log.trace ~__FUNCTION__ z
        | Some (Result, z) -> Log.result ~__FUNCTION__ z
        | Some (Show, z) -> Log.show ~__FUNCTION__ z
      in
      x.get
    ;;

    let rec unpack_all : t -> unit Proofview.tactic = function
      | { this; next = None } -> unpack this
      | { this; next = Some next } ->
        Proofview.tclTHEN (unpack this) (unpack_all next)
    ;;

    (** [seq a b] appends [b] to the sequence of [a]. *)
    let rec seq : t -> t -> t = function
      | { this; next = None } -> fun (b : t) -> { this; next = Some b }
      | { this; next = Some next } ->
        fun (b : t) -> { this; next = Some (seq next b) }
    ;;

    let empty () : t = tactic (Proofview.tclUNIT ())

    exception EmptyTacticChain of unit

    (** [chain ?nonempty (x::xs)] applies [seq x (chain xs)].
        @raise EmptyTacticChain if the list is empty and [?nonempty] s true. *)
    let rec chain ?(nonempty : bool = false) : t list -> t = function
      | [] -> if nonempty then raise (EmptyTacticChain ()) else empty ()
      | h :: [] -> h
      | h :: tl -> seq h (chain tl)
    ;;
  end

  (***********************************************************************)

  (** [module P] is a monad that contains the proof environment and context. *)
  module P (X : sig
      val gl : Proofview.Goal.t ref
    end) =
  struct
    let gl () : Proofview.Goal.t = !X.gl

    include
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
        (W.M.Enc)

    let to_atomic (x : EConstr.t) : EConstr.t Rocq_utils.kind_pair mm =
      let open Syntax in
      let* sigma = get_sigma in
      Rocq_utils.econstr_to_atomic sigma x |> return
    ;;

    let get_concl () : EConstr.t = Proofview.Goal.concl (gl ())
    let get_hyps () : Rocq_utils.hyp list = Proofview.Goal.hyps (gl ())

    let log_concl () : unit =
      Log.thing Debug "concl" (get_concl ()) (Of Strfy.econstr)
    ;;

    let log_hyps () : unit =
      Log.things Debug "hyps" (get_hyps ()) (Of Strfy.hyp)
    ;;

    let get_hyp_name (x : Rocq_utils.hyp) : Names.Id.t =
      Context.Named.Declaration.get_id x
    ;;

    let get_hyp_names () : Names.Id.Set.t = Context.Named.to_vars (get_hyps ())

    let next_name_of (names : Names.Id.Set.t) (x : Names.Id.t) : Names.Id.t =
      Namegen.next_ident_away x names
    ;;

    let new_name_of_string (x : string) : Names.Id.t =
      next_name_of (get_hyp_names ()) (Names.Id.of_string x)
    ;;

    let new_cofix_name () : Names.Id.t = new_name_of_string "Cofix0"
    let new_H_name () : Names.Id.t = new_name_of_string "H0"

    let get_all_cofix_hyp_names () : Names.Id.Set.t =
      Names.Id.Set.filter
        (fun (x : Names.Id.t) ->
          Names.Id.equal (Nameops.root_of_id x) (Names.Id.of_string "Cofix"))
        (get_hyp_names ())
    ;;

    let get_all_non_cofix_hyp_names () : Names.Id.Set.t =
      Names.Id.Set.diff (get_hyp_names ()) (get_all_cofix_hyp_names ())
    ;;

    (***********************************************************************)

    (** tactics *)
    module Tacs = struct
      let inversion (x : Rocq_utils.hyp) : Tactic.t mm =
        Inv.inv_tac (Context.Named.Declaration.get_id x)
        |> Tactic.tactic
             ~msg:
               (Printf.sprintf
                  "inversion %s"
                  (Strfy.hyp_name x) (* (Strfy.hyp_type x) *))
        |> return
      ;;

      let subst_all () : Tactic.t mm =
        Equality.subst_all ()
        |> Tactic.tactic ~msg:(Printf.sprintf "(subst all)")
        |> return
      ;;

      let simplify_concl () : Tactic.t mm =
        Tactics.simpl_in_concl |> Tactic.tactic ~msg:"simpl" |> return
      ;;

      let simplify_hyp (x : Rocq_utils.hyp) : Tactic.t mm =
        Tactics.simpl_in_hyp (Context.Named.Declaration.get_id x, Locus.InHyp)
        |> Tactic.tactic ~msg:(Printf.sprintf "simpl in %s" (Strfy.hyp_name x))
        |> return
      ;;

      let simplify_hyps () : Tactic.t mm =
        match get_hyps () with
        | [] -> Tactic.empty () |> return
        | x :: [] -> simplify_hyp x
        | x :: xs ->
          let open Syntax in
          let* x : Tactic.t = simplify_hyp x in
          let f (i : int) (x : Tactic.t) : Tactic.t mm =
            let y : Rocq_utils.hyp = List.nth xs i in
            let* y : Tactic.t = simplify_hyp y in
            Tactic.seq x y |> return
          in
          iterate 0 (List.length xs - 1) x f
      ;;

      let simplify_all () : Tactic.t mm =
        let open Syntax in
        let* concl : Tactic.t = simplify_concl () in
        let* hyps : Tactic.t = simplify_hyps () in
        Tactic.seq concl hyps |> return
      ;;

      let simplify_and_subst_all () : Tactic.t mm =
        let open Syntax in
        let* simpls : Tactic.t = simplify_all () in
        let* substs : Tactic.t = subst_all () in
        Tactic.seq simpls substs |> return
      ;;

      let cofix () : Tactic.t mm =
        let name : Names.Id.t = new_cofix_name () in
        Tactics.cofix name
        |> Tactic.tactic
             ~msg:(Printf.sprintf "cofix %s" (Names.Id.to_string name))
        |> return
      ;;

      let trivial ?(msg : string = "trivial") () : Tactic.t mm =
        Tactic.tactic ~msg (Auto.gen_trivial ~debug:Hints.Info [] None)
        |> return
      ;;

      let ex_intro (x : Model.State.t) : Tactic.t mm =
        let bindings = Tactypes.ImplicitBindings [ M.decode x.term ] in
        Tactic.tactic (Tactics.constructor_tac true None 1 bindings) |> return
      ;;

      let split () : Tactic.t mm =
        Tactic.tactic (Tactics.split Tactypes.NoBindings) |> return
      ;;

      let ex_intro_split (x : Model.State.t) : Tactic.t mm =
        let open Syntax in
        let* ex_intro : Tactic.t = ex_intro x in
        let* split : Tactic.t = split () in
        Tactic.seq ex_intro split |> return
      ;;

      let intros_all () : Tactic.t mm =
        Tactics.intros |> Tactic.tactic ~msg:"intros" |> return
      ;;

      (** [intro_as x] applies the introduction tactic using the (next non-conficting) name [x].
      *)
      let intro_as (x : string) : Tactic.t mm =
        let name : Names.Id.t = new_name_of_string x in
        Tactics.introduction name
        |> Tactic.tactic
             ~msg:(Printf.sprintf "intro %s" (Names.Id.to_string name))
        |> return
      ;;

      (* *)
      let apply (x : EConstr.t) : Tactic.t mm =
        Tactics.apply x
        |> Tactic.tactic ~msg:(Printf.sprintf "apply %s" (Strfy.econstr x))
        |> return
      ;;

      let apply_Pack_sim () : Tactic.t mm = apply (Theories.c_Pack_sim ())
      let apply_In_sim () : Tactic.t mm = apply (Theories.c_In_sim ())
      let apply_wk_some () : Tactic.t mm = apply (Theories.c_wk_some ())
      let apply_wk_none () : Tactic.t mm = apply (Theories.c_wk_none ())
      let apply_rt1n_refl () : Tactic.t mm = apply (Theories.c_rt1n_refl ())
      let apply_rt1n_trans () : Tactic.t mm = apply (Theories.c_rt1n_trans ())

      let eapply (x : EConstr.t) : Tactic.t mm =
        Tactics.eapply x
        |> Tactic.tactic ~msg:(Printf.sprintf "eapply %s" (Strfy.econstr x))
        |> return
      ;;

      let eapply_rt1n_refl () : Tactic.t mm = eapply (Theories.c_rt1n_refl ())
      let eapply_rt1n_trans () : Tactic.t mm = eapply (Theories.c_rt1n_trans ())

      (* *)
      exception CannotUnfoldConstr of Constr.t

      (** [unfold_constr x]
          @raise CannotUnfoldConstr
            of [x] if [Constr.kind x] is not [Const (_, _)]. *)
      let unfold_constr ?(in_hyp : Rocq_utils.hyp option) (x : Constr.t)
        : Tactic.t mm
        =
        let f (name : Names.Constant.t) : unit Proofview.tactic =
          Option.cata
            (fun (y : Rocq_utils.hyp) ->
              Tactics.unfold_in_hyp
                [ Locus.AllOccurrences, Evaluable.EvalConstRef name ]
                (Context.Named.Declaration.get_id y, Locus.InHyp))
            (Tactics.unfold_constr (Names.GlobRef.ConstRef name))
            in_hyp
        in
        match Constr.kind x with
        | Const (name, _) ->
          f name
          |> Tactic.tactic
               ~msg:(Printf.sprintf "unfold %s" (Names.Constant.to_string name))
          |> return
        | _ -> raise (CannotUnfoldConstr x)
      ;;

      let unfold_econstr ?(in_hyp : Rocq_utils.hyp option) (x : EConstr.t)
        : Tactic.t mm
        =
        let open Syntax in
        let* y : Constr.t = econstr_to_constr x in
        (* NOTE: below helps keep this function cleaner to use. i.e., [unfold_econstr ~in_hyp:x] rather than [~in_hyp:(Some x)] *)
        match in_hyp with
        | None -> unfold_constr y
        | Some in_hyp -> unfold_constr ~in_hyp y
      ;;

      let unfold_constrexpr
            ?(in_hyp : Rocq_utils.hyp option)
            (x : Constrexpr.constr_expr)
        : Tactic.t mm
        =
        let open Syntax in
        let* y : EConstr.t = constrexpr_to_econstr x in
        (* NOTE: below helps keep this function cleaner to use. i.e., [unfold_constrexpr ~in_hyp:x] rather than [~in_hyp:(Some x)] *)
        match in_hyp with
        | None -> unfold_econstr y
        | Some in_hyp -> unfold_econstr ~in_hyp y
      ;;

      let unfold_opt_constrexpr_list ?(in_hyp : Rocq_utils.hyp option)
        : Constrexpr.constr_expr list -> Tactic.t option mm
        =
        (* NOTE: optional argument wrapper *)
        let unfold_constrexpr (x : Constrexpr.constr_expr) : Tactic.t mm =
          match in_hyp with
          | None -> unfold_constrexpr x
          | Some in_hyp -> unfold_constrexpr ~in_hyp x
        in
        (* NOTE: iterate and combine tactics *)
        let open Syntax in
        let iterate h xs =
          let f (i : int) (ys : Tactic.t) =
            let* x = List.nth xs i |> unfold_constrexpr in
            Tactic.seq h x |> return
          in
          iterate 0 (List.length xs - 1) h f
        in
        function
        | [] -> return None
        | h :: tl ->
          let* h = unfold_constrexpr h in
          let* x = iterate h tl in
          return (Some x)
      ;;

      let unfold_silent () : Tactic.t mm = unfold_econstr (Theories.c_silent ())

      let unfold_silent1 () : Tactic.t mm =
        unfold_econstr (Theories.c_silent1 ())
      ;;
    end

    module Theory = struct
      (** [is_any_theory x] is [true] if term [x] is equal to any of the terms presented in [Theories].
      *)
      let is_any_theory (x : EConstr.t) : bool =
        Theories.collect_bisimilarity_theories ()
        |> List.exists (fun (y : EConstr.t) -> run (econstr_eq x y))
      ;;

      (** [is_theory x y] checks if term [x] is equal to theory term [y], catching the exception thrown when [EConstr.kind_of_type x] is not [AtomicType (ty, tys)].
      *)
      let is_theory (x : EConstr.t) (y : EConstr.t) : bool mm =
        try
          let open Syntax in
          let* ty, tys = to_atomic x in
          econstr_eq y ty
        with
        | Rocq_utils.Rocq_utils_EConstrIsNotA_Type _ -> return false
      ;;

      (** exists *)
      let is_exists (x : EConstr.t) : bool mm = is_theory x (Theories.c_ex ())

      (** weak simulation*)
      let is_weak_sim (x : EConstr.t) : bool mm =
        is_theory x (Theories.c_weak_sim ())
      ;;

      (** weak transition *)
      let is_weak (x : EConstr.t) : bool mm = is_theory x (Theories.c_weak ())

      let is_tau (x : EConstr.t) : bool mm = is_theory x (Theories.c_tau ())

      let is_silent (x : EConstr.t) : bool mm =
        is_theory x (Theories.c_silent ())
      ;;

      let is_silent1 (x : EConstr.t) : bool mm =
        is_theory x (Theories.c_silent1 ())
      ;;

      let is_LTS (x : EConstr.t) : bool mm = is_theory x (Theories.c_LTS ())
      let is_None (x : EConstr.t) : bool mm = is_theory x (Theories.c_None ())
      let is_Some (x : EConstr.t) : bool mm = is_theory x (Theories.c_Some ())

      (** *)
      let get_theory_enc (f : EConstr.t -> bool mm) : M.Enc.t M.mm =
        let open M.Syntax in
        let* fm = M.get_fwdmap in
        let rec find_theory : (EConstr.t * M.Enc.t) list -> M.Enc.t M.mm =
          function
          | [] -> raise Not_found
          | (x, y) :: tl ->
            let is_match : bool = run (f x) in
            if is_match then M.return y else find_theory tl
        in
        M.F.to_seq fm |> List.of_seq |> find_theory
      ;;

      exception NoEncodingFoundFor_TheoriesNone of unit

      let get_None_enc () : M.Enc.t M.mm =
        try get_theory_enc is_None with
        | Not_found -> raise (NoEncodingFoundFor_TheoriesNone ())
      ;;

      exception NoEncodingFoundFor_TheoriesSome of unit

      let get_Some_enc () : M.Enc.t M.mm =
        try get_theory_enc is_Some with
        | Not_found -> raise (NoEncodingFoundFor_TheoriesSome ())
      ;;

      exception NotEqTheory of unit

      (** *)
      let get_theory_enc_if_eq (x : EConstr.t) (f : EConstr.t -> bool mm)
        : M.Enc.t M.mm
        =
        let is_eq : bool = run (f x) in
        try if is_eq then get_theory_enc f else raise Not_found with
        | Not_found -> raise (NotEqTheory ())
      ;;

      let get_None_enc_if_eq (x : EConstr.t) : M.Enc.t M.mm =
        get_theory_enc_if_eq x is_None
      ;;

      let get_Some_enc_if_eq (x : EConstr.t) : M.Enc.t M.mm =
        get_theory_enc_if_eq x is_Some
      ;;

      exception FSM_HasNoSilentLabel of Model.FSM.t

      let is_fsm_silent_label (x : EConstr.t) (m : Model.FSM.t) : bool M.mm =
        match
          Model.Labels.filter Model.Label.is_silent m.info.weak_labels
          |> Model.Labels.to_list
        with
        | [] -> raise (FSM_HasNoSilentLabel m)
        | ys -> M.exists_eq x ys Decode.label
      ;;

      exception FSM_HasNoVisibleLabel of Model.FSM.t

      (** i.e., not silent label *)
      let is_fsm_visible_label (x : EConstr.t) (m : Model.FSM.t) : bool M.mm =
        match
          Model.Labels.filter
            (fun y -> Model.Label.is_silent y |> Bool.not)
            m.info.weak_labels
          |> Model.Labels.to_list
        with
        | [] -> raise (FSM_HasNoVisibleLabel m)
        | ys -> M.exists_eq x ys Decode.label
      ;;

      exception FSM_HasNoWeakLabels of Model.FSM.t

      let is_fsm_weak_labels (x : EConstr.t) (m : Model.FSM.t) : bool M.mm =
        let open M.Syntax in
        let* is_silent = is_fsm_silent_label x m in
        if is_silent then M.return true else is_fsm_visible_label x m
      ;;

      exception FSM_HasNoConstructors of Model.FSM.t

      let is_fsm_constructor (x : EConstr.t) (m : Model.FSM.t) : bool M.mm =
        match m with
        | { info = { meta = None; _ }; _ } -> raise (FSM_HasNoConstructors m)
        | { info = { meta = Some { lts; _ }; _ }; _ } ->
          if is_any_theory x
          then M.return false
          else M.exists_eq x lts Decode.lts_constructor
      ;;
    end

    (** remodel *)
    module ReModel = struct
      exception
        CouldNotFind_State of
          { x : EConstr.t
          ; states : Model.States.t
          }

      let state (x : EConstr.t) (ys : Model.States.t) : Model.State.t M.mm =
        try
          let term : M.Enc.t = M.get_encoding x in
          (* NOTE: [Model.States.compare] only cares about [term]. *)
          Model.States.find { term; pp = None } ys |> M.return
        with
        | Not_found -> raise (CouldNotFind_State { x; states = ys })
      ;;

      exception
        CouldNotFind_Label of
          { x : EConstr.t
          ; alphabet : Model.Labels.t
          }

      let label (x : EConstr.t) (ys : Model.Labels.t) : Model.Label.t M.mm =
        let f (term : M.Enc.t) : Model.Label.t M.mm =
          (* NOTE: [Model.Labels.compare] only cares about [is_silent=Some _] *)
          Model.Labels.find { term; is_silent = None; pp = None } ys |> M.return
        in
        try M.get_encoding x |> f with
        | Not_found ->
          let open M.Syntax in
          (* NOTE: is it [None]? (i.e., a silent action) *)
          (try
             let* term : M.Enc.t = Theory.get_None_enc_if_eq x in
             f term
           with
           | Theory.NotEqTheory () ->
             (* NOTE: is it [Some]? (i.e., a visible action) *)
             (try
                let* term : M.Enc.t = Theory.get_Some_enc_if_eq x in
                f term
              with
              | Theory.NotEqTheory () ->
                raise (CouldNotFind_Label { x; alphabet = ys })))
      ;;

      exception
        CouldNotFind_Transition of
          { from : Model.State.t
          ; goto : Model.State.t
          ; label : Model.Label.t
          ; edges : Model.EdgeMap.t'
          }

      let transition
            (from : Model.State.t)
            (goto : Model.State.t)
            (label : Model.Label.t)
            (edges : Model.EdgeMap.t')
        : Model.Transition.t
        =
        (* TODO: export some of this to the [Model.ActionMap] ? *)
        (* try *)
        let actions = Model.EdgeMap.find edges from in
        let labelled = Model.ActionMap.reduce_by_label actions label in
        if Model.ActionMap.length labelled |> Int.equal 0
        then raise (CouldNotFind_Transition { from; goto; label; edges })
        else (
          let actionpairs =
            Model.ActionMap.to_seq labelled
            |> List.of_seq
            |> List.filter
                 (fun
                     ((action, destinations) : Model.Action.t * Model.States.t)
                    -> Model.States.mem goto destinations)
          in
          match actionpairs with
          | [] -> raise (CouldNotFind_Transition { from; goto; label; edges })
          | ({ annotation; constructor_trees; _ }, _) :: [] ->
            { from
            ; goto
            ; label
            ; annotation
            ; constructor_tree = Model.Trees.min constructor_trees
            }
          | h :: tl ->
            (* TODO: move this proceed to [Model] and handle this case *)
            Log.warning ~__FUNCTION__ "Multiple actionpairs found";
            raise (CouldNotFind_Transition { from; goto; label; edges }))
      ;;
    end

    (***********************************************************************)

    module Hyp = struct
      exception NotImplemented of unit

      type t = Rocq_utils.hyp

      let to_string (x : t) : string = Strfy.hyp x

      let to_atomic (x : t) : EConstr.t Rocq_utils.kind_pair mm =
        let open Syntax in
        let* sigma = get_sigma in
        Rocq_utils.hyp_to_atomic sigma x |> return
      ;;

      (** [invertibility x] returns an integer denoting whether [x] need be inverted, with the higher numbers being of more importance to invert and [0] denoting [x] does not need to be inverted.
      *)
      let invertibility (x : t) : int mm =
        let open Syntax in
        let* _, tys = to_atomic x in
        let* sigma = get_sigma in
        (* NOTE: returns true if can be inverted *)
        let f (x : EConstr.t) : bool =
          EConstr.isRef sigma x && EConstr.isVar sigma x
        in
        (* NOTE: since [2] is the goto-state and [1] is the label, [g] allows us to clearly see which hyp needs to be inverted first. *)
        let g (i : int) : int =
          try if f tys.(i) then i else 0 with
          (* NOTE: handles "Index out of bounds" for accessing [tys] array. *)
          | Invalid_argument _ -> 0
        in
        g 2 + g 1 |> return
      ;;

      let need_inversion (x : t) : bool mm =
        let open Syntax in
        let* n : int = invertibility x in
        if Int.equal n 0 then return false else return true
      ;;

      let invert (x : t) : Tactic.t mm = Tacs.inversion x

      exception
        CouldNotGetTransition of
          { hyp : t
          ; fsm : Model.FSM.t
          }

      (** *)
      let get_transition (x : t) (m : Model.FSM.t) : Model.Transition.t mm =
        Log.trace __FUNCTION__;
        let open Syntax in
        let* ty, tys = to_atomic x in
        let is_fsm_constructor : bool =
          M.run (Theory.is_fsm_constructor ty m)
        in
        if is_fsm_constructor
        then (
          try
            let from : Model.State.t = M.run (ReModel.state tys.(0) m.states) in
            let goto : Model.State.t = M.run (ReModel.state tys.(2) m.states) in
            let label : Model.Label.t =
              M.run (ReModel.label tys.(1) m.alphabet)
            in
            ReModel.transition from goto label m.edges |> return
          with
          | ReModel.CouldNotFind_State _ ->
            raise (CouldNotGetTransition { hyp = x; fsm = m })
          | ReModel.CouldNotFind_Label _ ->
            raise (CouldNotGetTransition { hyp = x; fsm = m })
          | ReModel.CouldNotFind_Transition _ ->
            raise (CouldNotGetTransition { hyp = x; fsm = m }))
        else raise (NotImplemented ())
      ;;
    end

    (** conclusion *)
    module Concl = struct
      exception NotImplemented of unit

      let eq (x : EConstr.t) : bool mm = get_concl () |> econstr_eq x

      let eq_hyp (x : Rocq_utils.hyp) : bool mm =
        Context.Named.Declaration.get_type x |> eq
      ;;

      let rec eq_any_hyps : Rocq_utils.hyp list -> bool mm = function
        | [] -> return false
        | h :: tl ->
          let open Syntax in
          let* is_eq : bool = eq_hyp h in
          if is_eq then return true else eq_any_hyps tl
      ;;

      let is_weak_sim () : bool mm = get_concl () |> Theory.is_weak_sim
      let is_exists () : bool mm = get_concl () |> Theory.is_exists
    end

    (** hypothesis *)
    module Hyps = struct
      exception NotImplemented of unit

      (** [get_cofixes ()] filters the hyps by name according to [get_all_cofix_hyp_names ()].
      *)
      let get_cofixes () : Rocq_utils.hyp list =
        let cofix_names : Names.Id.Set.t = get_all_cofix_hyp_names () in
        get_hyps ()
        |> List.filter (fun (x : Rocq_utils.hyp) ->
          Names.Id.Set.mem (get_hyp_name x) cofix_names)
      ;;

      (** [get_non_cofixes ()] filters the hyps by name according to [get_all_non_cofix_hyp_names ()].
      *)
      let get_non_cofixes () : Rocq_utils.hyp list =
        let cofix_names : Names.Id.Set.t = get_all_non_cofix_hyp_names () in
        get_hyps ()
        |> List.filter (fun (x : Rocq_utils.hyp) ->
          Names.Id.Set.mem (get_hyp_name x) cofix_names)
      ;;

      (** [can_solve_concl_cofix ()] returns true if there is a hyp that can solve the current a tactic to solve the current goal using one of he cofixes in the hyps.
      *)
      let can_solve_concl_cofix () : bool mm =
        get_cofixes () |> Concl.eq_any_hyps
      ;;

      (** [clear_non_cofix ()] returns a tactic that will clear all the hyps that are named according to [get_all_non_cofix_hyp_names ()]. This is to be used at the end of a case of the proof has been solved.
          (* TODO: check if this is necessary -- or could be problematic? *) *)
      let clear_non_cofix () : Tactic.t =
        Tactic.tactic
          ~msg:"(Clearing non-cofix Hyps)"
          (Tactics.clear
             (Names.Id.Set.to_list (get_all_non_cofix_hyp_names ())))
      ;;

      (** [try_invert_any ()] returns either [None] if no hyps can be inverted (as determined by [Hyp.invertibility]), else a [Tactic.t] that will invert the hypothesis deemed to be the most important to invert. (Only checks non-cofix hyps as by [get_non_cofixes ()].)
      *)
      let try_invert_any () : Tactic.t option mm =
        let hyps = get_non_cofixes () in
        let open Syntax in
        let f (i : int) (xopt : (int * Hyp.t) option) : (int * Hyp.t) option mm =
          let y = List.nth hyps i in
          let* grade = Hyp.invertibility y in
          match xopt with
          | None -> Some (grade, y) |> return
          | Some (n, x) ->
            (match Int.compare grade n with
             | -1 -> Some (n, x) |> return
             | _ -> Some (grade, y) |> return)
        in
        let* to_invert_opt = iterate 0 (List.length hyps - 1) None f in
        match to_invert_opt with
        | None -> return None
        | Some (0, x) -> return None
        (* NOTE: we only want to invert hyps with non-zero grades. *)
        | Some (grade, x) ->
          let* y = Hyp.invert x in
          return (Some y)
      ;;

      exception CannotGetTransition of Model.FSM.t

      let get_transition (m : Model.FSM.t) : Model.Transition.t mm =
        Log.trace __FUNCTION__;
        let hyps = get_hyps () in
        let open Syntax in
        let f (i : int)
          : Model.Transition.t option -> Model.Transition.t option mm
          = function
          | Some x -> return (Some x)
          | None ->
            let y = List.nth hyps i in
            (try
               let* y : Model.Transition.t = Hyp.get_transition y m in
               return (Some y)
             with
             | Hyp.CouldNotGetTransition _ -> return None)
        in
        let* x = iterate 0 (List.length hyps - 1) None f in
        match x with
        | None -> raise (CannotGetTransition m)
        | Some x -> return x
      ;;
    end

    (***********************************************************************)

    (** [handle_new_cofix ()] returns a sequence of tactics to handle the creation of a new cofix in the hyps, followed by the necessary application of constructors and introduction of terms to get started on a new case.
    *)
    let handle_new_cofix () : Tactic.t mm =
      Log.trace __FUNCTION__;
      let open Syntax in
      let* cofix : Tactic.t = Tacs.cofix () in
      let clear : Tactic.t = Hyps.clear_non_cofix () in
      let* apply_In_sim : Tactic.t = Tacs.apply_In_sim () in
      let* apply_Pack_sim : Tactic.t = Tacs.apply_Pack_sim () in
      let* intros_all : Tactic.t = Tacs.intros_all () in
      Tactic.chain [ cofix; clear; apply_In_sim; apply_Pack_sim; intros_all ]
      |> return
    ;;

    (***********************************************************************)

    exception StateNotImplemented of State.t
    exception StateCouldNothandle of State.t
    exception SkipNewProof of unit
    exception ExitWeakSim of unit

    let handle_new_proof
          ((a, b) : Constrexpr.constr_expr * Constrexpr.constr_expr)
      : Tactic.t mm
      =
      Log.trace __FUNCTION__;
      let open Syntax in
      let* x = Tacs.unfold_opt_constrexpr_list [ a; b ] in
      match x with
      | None -> raise (SkipNewProof ())
      | Some x ->
        update_state WeakSim;
        return x
    ;;

    let handle_weaksim () : Tactic.t mm =
      Log.trace __FUNCTION__;
      let open Syntax in
      let* is_weak_sim : bool = Concl.is_weak_sim () in
      if is_weak_sim
      then
        let* has_hyp_cofix : bool = Hyps.can_solve_concl_cofix () in
        if has_hyp_cofix
        then Tacs.trivial ~msg:"trivial (solve cofix)" ()
        else handle_new_cofix ()
      else
        (* NOTE: try invert any that need to be inverted *)
        let* invert_opt = Hyps.try_invert_any () in
        match invert_opt with
        | None -> raise (ExitWeakSim ())
        | Some x -> return x
    ;;

    let handle_exists () : Tactic.t mm =
      Log.trace __FUNCTION__;
      let open Syntax in
      let* is_exists : bool = Concl.is_exists () in
      if is_exists
      then (
        Log.trace ~__FUNCTION__ "is_exists";
        let* hyp : Model.Transition.t = Hyps.get_transition (W.get_fsm_a ()) in
        (* TODO: cannot proceed until unfold is also fixed *)
        (* Tacs.ex_intro_split (); *)
        raise (StateNotImplemented Exists))
      else raise (StateCouldNothandle Exists)
    ;;

    let handle_goal_transition ({ hyp; goal } : Transition.t) : Tactic.t mm =
      Log.trace __FUNCTION__;
      raise (StateNotImplemented (GoalTransition { hyp; goal }))
    ;;

    let handle_apply_constructors
          ({ current; annotation; destination } : ApplicableConstructors.t)
      : Tactic.t mm
      =
      Log.trace __FUNCTION__;
      raise
        (StateNotImplemented
           (ApplyConstructors { current; annotation; destination }))
    ;;

    let handle_state () : Tactic.t mm =
      log_state ();
      log_hyps ();
      log_concl ();
      match get_state () with
      | NewProof ab -> handle_new_proof ab
      | WeakSim -> handle_weaksim ()
      | Exists -> handle_exists ()
      | GoalTransition args -> handle_goal_transition args
      | ApplyConstructors xs -> handle_apply_constructors xs
      | Done -> raise (NothingToDo ())
    ;;

    (** [step ()] ... *)
    let rec step () : Tactic.t =
      Log.trace __FUNCTION__;
      try run (handle_state ()) with
      | SkipNewProof () ->
        Log.trace ~__FUNCTION__ "BeginProof:SkipNewProof => WeakSim";
        update_state WeakSim;
        step ()
      | ExitWeakSim () ->
        Log.trace ~__FUNCTION__ "WeakSim:ExitWeakSim => Exists";
        update_state Exists;
        step ()
      (********************)
      | M.EncodingNotFound x ->
        Log.thing Warning "M.EncodingNotFound" x (Of M.Strfy.econstr);
        Log.thing Warning "(using P)" x (Of Strfy.econstr);
        raise (M.EncodingNotFound x)
      | EncodingNotFound x ->
        Log.thing Warning "(M).EncodingNotFound" x (Of Strfy.econstr);
        Log.thing Warning "(using M)" x (Of M.Strfy.econstr);
        raise (M.EncodingNotFound x)
    ;;
  end

  (** [get_updated_pstate x] returns the [pstate] updated by tactic [x]. *)
  let get_updated_pstate (x : unit Proofview.tactic) : Declare.Proof.t =
    let new_pstate, is_safe_tactic = Declare.Proof.by x (get_pstate ()) in
    if Bool.not is_safe_tactic
    then Log.warning ~__FUNCTION__ "unsafe tactic used";
    new_pstate
  ;;

  (** [step pstate] enters a fresh module [P] for this specific [pstate], and returns one updated with
  *)
  let step (pstate : Declare.Proof.t) : Declare.Proof.t =
    Log.trace __FUNCTION__;
    update_pstate pstate;
    Proofview.Goal.enter (fun gl ->
      let module P =
        P (struct
          let gl = ref gl
        end)
      in
      let x = P.step () in
      let y = P.run (P.Tacs.simplify_and_subst_all ()) in
      Tactic.seq x y |> Tactic.unpack_all)
    |> get_updated_pstate
  ;;
end

(***********************************************************************)

module Log = Logger.Default

module type S = module type of Make (Log) ((val Api.default_encoding ()))

let the_proof_solver : (module S) ref option ref = ref None
let reset_the_proof_solver () : unit = the_proof_solver := None

exception NoProofSolverFound of unit

let get_the_proof_solver () : (module S) ref =
  match !the_proof_solver with
  | None -> raise (NoProofSolverFound ())
  | Some x -> x
;;

let new_proof_solver () : (module S) ref =
  let module M : S = Make (Log) ((val Api.default_encoding ())) in
  the_proof_solver := Some (ref (module M : S));
  get_the_proof_solver ()
;;

let is_done () : bool =
  let module Ps = (val !(get_the_proof_solver ())) in
  Ps.is_done ()
;;

(***********************************************************************)

exception BisimilarityResultNotFound of unit

(** [init ] ... *)
let init
      (pstate : Declare.Proof.t)
      (refs : Libnames.qualid list)
      (a : Constrexpr.constr_expr * Libnames.qualid)
      (b : Constrexpr.constr_expr * Libnames.qualid)
  : Declare.Proof.t
  =
  let ps : (module S) ref = new_proof_solver () in
  let module Ps : S = (val !ps) in
  Ps.check_bisimilarity refs a b;
  Ps.init pstate (fst a, fst b);
  pstate
;;

(** [step] ... *)
let step (pstate : Declare.Proof.t) : Declare.Proof.t =
  let module Ps = (val !(get_the_proof_solver ())) in
  Ps.step pstate
;;

(** [solve] ... *)
let solve ?(bound : int = 10) (pstate : Declare.Proof.t) : Declare.Proof.t =
  Log.trace __FUNCTION__;
  let fint = Utils.Strfy.Of Utils.Strfy.int in
  let fbool = Utils.Strfy.Of Utils.Strfy.bool in
  let rec f (n : int) (p : Declare.Proof.t) : int * Declare.Proof.t =
    Log.thing ~__FUNCTION__ Trace "iter" n (Of Utils.Strfy.int);
    match Int.compare n bound with
    | -1 ->
      if is_done ()
      then n, p
      else (try step p |> f (n + 1) with NothingToDo () -> n, p)
    | _ ->
      Log.thing ~__FUNCTION__ Warning "Stopping, exceeded bound" bound fint;
      n, p
  in
  let num, pstate = f 0 pstate in
  Log.thing ~__FUNCTION__ Notice "(Stopped) Solved: " (is_done ()) fbool;
  pstate
;;
