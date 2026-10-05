module type S = sig
  type 'a mm
  type enc
  type node
  type bindings
  type constructorbindings
  type state
  type label
  type rocqlts
  type tactic
  type econstrset

  val inversion : Rocq_utils.hyp -> tactic mm

  (** [refute_premise h]: close the goal from [h], a premise hypothesis
      known to be false ({!Premise_search.refute_hyp_tac}; backlog I2). *)
  val refute_premise : Rocq_utils.hyp -> tactic mm

  (** [refute_dead h]: close the goal from [h], an LTS step that
      [Premise_search.dead] says has no instance, by refuting it; if that
      fails, [inversion h], so a wrong verdict costs one step, never the
      proof. *)
  val refute_dead : Rocq_utils.hyp -> tactic mm

  val subst_all : unit -> tactic mm
  val simplify : unit -> tactic mm
  val simplify_concl : unit -> tactic mm
  val simplify_hyp : Rocq_utils.hyp -> tactic mm
  val simplify_hyps : unit -> tactic mm
  val simplify_and_subst_all : unit -> tactic mm

  (** [reflexivity ()] closes an equation premise goal (up to reduction). *)
  val reflexivity : unit -> tactic mm

  (** [exact_term p] closes the goal with proof term [p]. *)
  val exact_term : EConstr.t -> tactic mm

  (** [invert_premise h]: [simpl in h; inversion h; clear h; subst], for a
      premise hypothesis that mentions variables it determines (an output, such
      as a target [m] in [succ_rel n m]; backlog I2, stage 2). *)
  val invert_premise : Rocq_utils.hyp -> tactic mm

  (** [prove_negation ()] proves a goal [~ P] whose [P] is refutable. *)
  val prove_negation : unit -> tactic mm

  (** [prove_bounded ()] proves a bounded universal premise
      ([forall k, k < n -> P k]) instance by instance. *)
  val prove_bounded : unit -> tactic mm

  val cofix : unit -> tactic mm
  val mutual_cofix : Names.Id.t -> (Names.Id.t * Evd.econstr) list -> tactic mm
  val all_goals : tactic -> tactic
  val trivial : ?msg:string -> unit -> tactic mm
  val exact_hyp : Rocq_utils.hyp -> tactic mm
  val ex_intro : state -> tactic mm
  val split : unit -> tactic mm
  val ex_intro_split : state -> tactic mm
  val intros_all : unit -> tactic mm
  val apply : Evd.econstr -> tactic mm
  val apply_Pack_sim : unit -> tactic mm
  val apply_In_sim : unit -> tactic mm
  val apply_wk_none : unit -> tactic mm
  val apply_rt1n_refl : unit -> tactic mm
  val apply_weak_sim_refl : unit -> tactic mm
  val apply_Pack_bisim : unit -> tactic mm
  val apply_In_bisim : unit -> tactic mm
  val apply_weak_bisimilar_refl : unit -> tactic mm
  val eapply : Evd.econstr -> tactic mm
  val eapply_wk_some : unit -> tactic mm
  val eapply_rt1n_refl : unit -> tactic mm
  val eapply_rt1n_trans : unit -> tactic mm
  val eapply_rt1n_via : label -> tactic mm

  exception CannotUnfoldConstr of Constr.t

  val unfold_constr : ?in_hyp:Rocq_utils.hyp -> Constr.t -> tactic

  val f_unfold_hyp
    :  (?in_hyp:Rocq_utils.hyp -> 'a -> tactic)
    -> ?in_hyp:Rocq_utils.hyp option
    -> 'a
    -> tactic

  val unfold_econstr : ?in_hyp:Rocq_utils.hyp -> Evd.econstr -> tactic

  val unfold_constrexpr
    :  ?in_hyp:Rocq_utils.hyp
    -> Constrexpr.constr_expr
    -> tactic

  val unfold_opt_constrexpr_list
    :  ?in_hyp:Rocq_utils.hyp
    -> Constrexpr.constr_expr list
    -> tactic option

  val unfold_silent : unit -> tactic
  val do_refl : unit -> tactic mm
  val collect_component_econstrs : Evd.evar_map -> Evd.econstr -> econstrset
  val can_be_unfolded : Evd.evar_map -> Evd.econstr -> bool
  val try_unfold_any : ?in_hyp:Rocq_utils.hyp -> Evd.econstr -> tactic option mm
  val try_unfold_any_of : Evd.econstr list -> tactic option mm

  exception NoRocqLTSFoundWithEnc of enc

  val find_lts : enc -> rocqlts list -> rocqlts

  exception NoConstructorFoundWithIndex of int

  val find_constructor : int -> constructorbindings list -> constructorbindings

  type binding_args =
    { from : Evd.econstr
    ; goto : Evd.econstr option
    ; label : Evd.econstr option
    }

  val get_constructor_bindings
    :  binding_args
    -> bindings
    -> Evd.econstr Tactypes.bindings

  val try_get_constructor_bindings
    :  node
    -> binding_args
    -> Evd.econstr Tactypes.bindings

  (** Raised by {!apply_constructor} when the focused goal is not a step of
      the constructor's LTS (its bindings cannot be read off it). *)
  exception GoalNotAnLTSStep

  val apply_constructor : node -> binding_args -> tactic mm
end

module Make
    (Enc : Encoding.S)
    (Tactic : Proof_solver_tactic.S)
    (W :
       Results.S
       with type enc = Enc.t
        and type node = Enc.Tree.Node.t
        and type tree = Enc.Tree.t
        and type trees = Enc.Trees.t)
    (Iter : Proof_solver_wrapper.S with type enc = Enc.t)
    (Theory :
       Proof_solver_theory.S
       with type 'a mm = 'a W.M.mm
        and type 'a im = 'a Iter.mm
        and type enc = Enc.t
        and type fsm = W.Model.FSM.t) :
  S
  with type 'a mm = 'a Iter.mm
   and type enc = Enc.t
   and type node = Enc.Tree.Node.t
   and type bindings = W.Bindings.t
   and type constructorbindings = W.ConstructorBindings.t
   and type state = W.Model.State.t
   and type label = W.Model.Label.t
   and type rocqlts = W.Model.Info.Meta.RocqLTS.t
   and type tactic = Tactic.t
   and type econstrset = Iter.EConstrSet.t = struct
  type 'a mm = 'a Iter.mm

  module Model = W.Model
  module Decode = W.Decode
  module Bindings = W.Bindings
  module ConstructorBindings = W.ConstructorBindings

  type enc = Enc.t
  type node = Enc.Tree.Node.t
  type bindings = Bindings.t
  type constructorbindings = ConstructorBindings.t
  type state = Model.State.t
  type label = Model.Label.t
  type rocqlts = Model.Info.Meta.RocqLTS.t
  type tactic = Tactic.t
  type econstrset = Iter.EConstrSet.t

  open Iter

  (* See the [.mli]. *)
  let inversion (x : Rocq_utils.hyp) : Tactic.t mm =
    Inv.inv_tac (Context.Named.Declaration.get_id x)
    |> Tactic.create ~msg:(Printf.sprintf "inversion %s" (Strfy.hyp_name x))
    |> return
  ;;

  (* See the [.mli]: {!Premise_search.refute_hyp_tac} unfolds, inverts
     ([inversion_clear]), and refutes whatever premise each remaining goal is
     left with. *)
  let refute_premise (x : Rocq_utils.hyp) : Tactic.t mm =
    Premise_search.refute_hyp_tac (Context.Named.Declaration.get_id x)
    |> Tactic.create
         ~msg:(Printf.sprintf "(refute premise %s)" (Strfy.hyp_name x))
    |> return
  ;;

  (* See the [.mli]. *)
  let refute_dead (x : Rocq_utils.hyp) : Tactic.t mm =
    let id = Context.Named.Declaration.get_id x in
    Proofview.tclORELSE (Premise_search.refute_hyp_tac id) (fun _ ->
      Inv.inv_tac id)
    |> Tactic.create
         ~msg:
           (Printf.sprintf
              "(refute dead %s, else inversion %s)"
              (Strfy.hyp_name x)
              (Strfy.hyp_name x))
    |> return
  ;;

  (* See the [.mli]. [inversion; clear; subst], not [inversion_clear]: the
     latter reverts the hypotheses that depend on the premise's variables --
     the transition hypothesis [H : lts n a m] -- and reintroduces them with
     a fresh, unconstrained [m], so [get_transition] never sees the computed
     target. Checked by hand, 2026-10-02. *)
  let invert_premise (x : Rocq_utils.hyp) : Tactic.t mm =
    let id = Context.Named.Declaration.get_id x in
    Proofview.tclTHEN
      (Tactics.simpl_in_hyp (id, Locus.InHyp))
      (Proofview.tclTHEN
         (Inv.inv_tac id)
         (Proofview.tclTHEN
            (Tacticals.tclTRY (Tactics.clear [ id ]))
            (Equality.subst_all ())))
    |> Tactic.create
         ~msg:
           (Printf.sprintf
              "simpl in %s; inversion %s; clear %s; subst"
              (Strfy.hyp_name x)
              (Strfy.hyp_name x)
              (Strfy.hyp_name x))
    |> return
  ;;

  (* See the [.mli]. *)
  let prove_negation () : Tactic.t mm =
    Premise_search.negation_tac
    |> Tactic.create ~msg:"(prove negated premise)"
    |> return
  ;;

  (* See the [.mli]. *)
  let prove_bounded () : Tactic.t mm =
    Premise_search.premise_tac ()
    |> Tactic.create ~msg:"(prove bounded universal premise)"
    |> return
  ;;

  (* See the [.mli]. *)
  let subst_all () : Tactic.t mm =
    Equality.subst_all ()
    |> Tactic.create ~kind:Info ~msg:(Printf.sprintf "(subst all)")
    |> return
  ;;

  (* See the [.mli]: [simpl_option None] is [simpl in *]. *)
  let simplify () : Tactic.t mm =
    Tactics.simpl_option None |> Tactic.create ~msg:"simpl" |> return
  ;;

  (* See the [.mli]. *)
  let simplify_concl () : Tactic.t mm =
    Tactics.simpl_in_concl |> Tactic.create ~msg:"simpl" |> return
  ;;

  (* See the [.mli]. *)
  let simplify_hyp (x : Rocq_utils.hyp) : Tactic.t mm =
    Tactics.simpl_in_hyp (Context.Named.Declaration.get_id x, Locus.InHyp)
    |> Tactic.create ~msg:(Printf.sprintf "simpl in %s" (Strfy.hyp_name x))
    |> return
  ;;

  (* See the [.mli]. *)
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

  (* See the [.mli]. *)
  let exact_term (p : EConstr.t) : Tactic.t mm =
    Logger.trace __FUNCTION__;
    Tactics.exact_check p
    |> Tactic.create ~msg:"exact (premise proof)"
    |> return
  ;;

  (* See the [.mli]. *)
  let reflexivity () : Tactic.t mm =
    Logger.trace __FUNCTION__;
    Tactics.reflexivity_red true |> Tactic.create ~msg:"reflexivity" |> return
  ;;

  (* See the [.mli]. *)
  let simplify_and_subst_all () : Tactic.t mm =
    let open Syntax in
    let* simpls : Tactic.t = simplify () in
    let* substs : Tactic.t = subst_all () in
    Tactic.seq simpls substs |> return
  ;;

  (* See the [.mli]. *)
  let cofix () : Tactic.t mm =
    let name : Names.Id.t = new_cofix_name () in
    FixTactics.cofix name
    |> Tactic.create ~msg:(Printf.sprintf "cofix %s" (Names.Id.to_string name))
    |> return
  ;;

  (* See the [.mli]. *)
  let mutual_cofix (root : Names.Id.t) (others : (Names.Id.t * EConstr.t) list)
    : Tactic.t mm
    =
    FixTactics.mutual_cofix root others
    |> Tactic.create
         ~msg:
           (Printf.sprintf
              "cofix %s with (%i others)"
              (Names.Id.to_string root)
              (List.length others))
    |> return
  ;;

  (* See the [.mli]. [Proofview.Goal.enter] focuses each goal in turn and
     runs the tactic on it, which is this engine's "to every goal"; the
     solver's own [step] relies on the same. *)
  let all_goals (x : Tactic.t) : Tactic.t =
    Proofview.Goal.enter (fun _ -> Tactic.unpack x)
    |> Tactic.create ~msg:"(to all goals)"
  ;;

  (* See the [.mli]. *)
  let trivial ?(msg : string = "trivial") () : Tactic.t mm =
    let f : string list option -> unit Proofview.tactic =
      if Logger.is_enabled Output.Kind.Info
      then Auto.gen_trivial ~debug:Hints.Info []
      else Auto.gen_trivial []
    in
    Tactic.create ~msg (f None) |> return
  ;;

  (* See the [.mli]. *)
  let exact_hyp (x : Rocq_utils.hyp) : Tactic.t mm =
    let name : Names.Id.t = Context.Named.Declaration.get_id x in
    Tactics.exact_check (EConstr.mkVar name)
    |> Tactic.create ~msg:(Printf.sprintf "exact %s" (Names.Id.to_string name))
    |> return
  ;;

  (* See the [.mli]. *)
  let ex_intro (x : Model.State.t) : Tactic.t mm =
    let t : EConstr.t = Decode.state x in
    let bindings = Tactypes.ImplicitBindings [ t ] in
    let msg = Printf.sprintf "exists %s" (Strfy.econstr t) in
    Tactic.create ~msg (Tactics.constructor_tac true None 1 bindings) |> return
  ;;

  (* See the [.mli]. *)
  let split () : Tactic.t mm =
    Tactic.create (Tactics.split Tactypes.NoBindings) |> return
  ;;

  (* See the [.mli]. *)
  let ex_intro_split (x : Model.State.t) : Tactic.t mm =
    let open Syntax in
    let* ex_intro : Tactic.t = ex_intro x in
    let* split : Tactic.t = split () in
    Tactic.seq ex_intro split |> return
  ;;

  (* See the [.mli]. *)
  let intros_all () : Tactic.t mm =
    Tactics.intros |> Tactic.create ~msg:"intros" |> return
  ;;

  (* See the [.mli]. *)
  let apply (x : EConstr.t) : Tactic.t mm =
    Tactics.apply x
    |> Tactic.create ~msg:(Printf.sprintf "apply %s" (Strfy.econstr x))
    |> return
  ;;

  (* See the [.mli]. *)
  let apply_Pack_sim () : Tactic.t mm = apply (Mebi_theories.get "Pack_sim")

  (* See the [.mli]. *)
  let apply_In_sim () : Tactic.t mm = apply (Mebi_theories.get "In_sim")

  (* See the [.mli]. *)
  let apply_wk_none () : Tactic.t mm = apply (Mebi_theories.get "wk_none")

  (* See the [.mli]. *)
  let apply_rt1n_refl () : Tactic.t mm = apply (Mebi_theories.get "rt1n_refl")

  (* See the [.mli]. *)
  let apply_weak_sim_refl () : Tactic.t mm =
    apply (Mebi_theories.get "weak_sim_refl")
  ;;

  (* See the [.mli]. *)
  let apply_Pack_bisim () : Tactic.t mm = apply (Mebi_theories.get "Pack_bisim")

  (* See the [.mli]. *)
  let apply_In_bisim () : Tactic.t mm = apply (Mebi_theories.get "In_bisim")

  (* See the [.mli]. *)
  let apply_weak_bisimilar_refl () : Tactic.t mm =
    apply (Mebi_theories.get "weak_bisimilar_refl")
  ;;

  (* See the [.mli]. *)
  let eapply (x : EConstr.t) : Tactic.t mm =
    Tactics.eapply x
    |> Tactic.create ~msg:(Printf.sprintf "eapply %s" (Strfy.econstr x))
    |> return
  ;;

  (* See the [.mli]. *)
  let eapply_wk_some () : Tactic.t mm = eapply (Mebi_theories.get "wk_some")

  (* See the [.mli]. *)
  let eapply_rt1n_refl () : Tactic.t mm = eapply (Mebi_theories.get "rt1n_refl")

  (* See the [.mli]. *)
  let eapply_rt1n_trans () : Tactic.t mm =
    eapply (Mebi_theories.get "rt1n_trans")
  ;;

  (* See the [.mli]. *)
  let eapply_rt1n_via (x : Model.Label.t) : Tactic.t mm =
    if Model.Label.is_silent x
    then eapply_rt1n_trans ()
    else eapply_rt1n_refl ()
  ;;

  exception CannotUnfoldConstr of Constr.t

  (* See the [.mli]. *)
  let unfold_constr ?(in_hyp : Rocq_utils.hyp option) (x : Constr.t) : Tactic.t =
    Logger.trace __FUNCTION__;
    match Constr.kind x with
    | Const (name, _) ->
      let f (name : Names.Constant.t) : unit Proofview.tactic =
        match in_hyp with
        | None -> Tactics.unfold_constr (Names.GlobRef.ConstRef name)
        | Some y ->
          Proofview.tclTHEN
            (Tactics.unfold_in_hyp
               [ Locus.AllOccurrences, Evaluable.EvalConstRef name ]
               (Context.Named.Declaration.get_id y, Locus.InHyp))
            (Tactics.unfold_constr (Names.GlobRef.ConstRef name))
      in
      f name
      |> Tactic.create
           ~msg:(Printf.sprintf "unfold %s" (Names.Constant.to_string name))
    | _ -> raise (CannotUnfoldConstr x)
  ;;

  (* See the [.mli]. *)
  let f_unfold_hyp
        (f : ?in_hyp:Rocq_utils.hyp -> 'a -> Tactic.t)
        ?(in_hyp : Rocq_utils.hyp option = None)
        (x : 'a)
    : Tactic.t
    =
    Logger.trace __FUNCTION__;
    match in_hyp with None -> f x | Some in_hyp -> f ~in_hyp x
  ;;

  (* See the [.mli]. *)
  let unfold_econstr ?(in_hyp : Rocq_utils.hyp option) (x : EConstr.t)
    : Tactic.t
    =
    Logger.trace __FUNCTION__;
    econstr_to_constr x |> run |> f_unfold_hyp unfold_constr ~in_hyp
  ;;

  (* See the [.mli]. *)
  let unfold_constrexpr
        ?(in_hyp : Rocq_utils.hyp option)
        (x : Constrexpr.constr_expr)
    : Tactic.t
    =
    Logger.trace __FUNCTION__;
    constrexpr_to_econstr x |> run |> f_unfold_hyp unfold_econstr ~in_hyp
  ;;

  (* See the [.mli]. *)
  let unfold_opt_constrexpr_list ?(in_hyp : Rocq_utils.hyp option)
    : Constrexpr.constr_expr list -> Tactic.t option
    =
    Logger.trace __FUNCTION__;
    function
    | [] -> None
    | xs ->
      let ys : Tactic.t list =
        List.filter_map
          (fun (x : Constrexpr.constr_expr) ->
            try Some (f_unfold_hyp unfold_constrexpr ~in_hyp x) with
            | CannotUnfoldConstr _ -> None)
          xs
      in
      (match ys with [] -> None | ys -> Some (Tactic.chain ys))
  ;;

  (* See the [.mli]. *)
  let unfold_silent () : Tactic.t = unfold_econstr (Mebi_theories.get "silent")

  (* See the [.mli]. *)
  let do_refl () : Tactic.t mm =
    Logger.trace __FUNCTION__;
    let open Syntax in
    let* wk_none = apply_wk_none () in
    let unfold_silent = unfold_silent () in
    let* rt1n_refl = apply_rt1n_refl () in
    Tactic.chain [ wk_none; unfold_silent; rt1n_refl ] |> return
  ;;

  (* See the [.mli]. *)
  let collect_component_econstrs (sigma : Evd.evar_map) (x : EConstr.t)
    : EConstrSet.t
    =
    Logger.trace __FUNCTION__;
    let is_constr_ref (x : EConstr.t) : bool =
      EConstr.isRef sigma x && EConstr.isConst sigma x
    in
    let acc_constr_ref (x : EConstr.t) (acc : EConstrSet.t) : EConstrSet.t =
      if is_constr_ref x then EConstrSet.add x acc else acc
    in
    let rec f (acc : EConstrSet.t) (y : EConstr.t) : EConstrSet.t =
      let acc : EConstrSet.t = acc_constr_ref y acc in
      try
        let ty, tys = Rocq_utils.econstr_to_atomic sigma y in
        let acc : EConstrSet.t =
          match EConstr.kind sigma ty with
          | Case (_, _, _, _, _, c, _) ->
            (match EConstr.kind sigma c with
             | App (ty, _) -> acc_constr_ref ty acc
             | _ -> acc)
          | _ -> acc
        in
        let acc : EConstrSet.t = acc_constr_ref ty acc in
        Array.fold_left
          (fun (acc : EConstrSet.t) (z : EConstr.t) -> f acc z)
          acc
          tys
      with
      | Rocq_utils.Rocq_utils_EConstrIsNotA_Type _ -> acc
    in
    f EConstrSet.empty x
  ;;

  (** [unfoldable_definition z ty] is whether a constant defined as [z], of
      type [ty], is worth unfolding: a function or fixpoint (of a product
      type), an application of type a constant or inductive, a constructor
      of type a constant, or an alias of another constant (e.g.
      [SomeModule.example_1]). Raises nothing. *)
  let unfoldable_definition (z : Constr.t) (const_type : Constr.t) : bool =
    match Constr.kind z with
    | Fix _ -> Constr.isProd const_type
    | Lambda _ -> Constr.isProd const_type
    | App _ ->
      Constr.isRef const_type
      && (Constr.isConst const_type || Constr.isInd const_type)
    | Construct _ ->
      Constr.isRef z && Constr.isConst const_type && Constr.isRef const_type
    | _ ->
      Constr.isConst z
      && Constr.isRef z
      && Constr.isConst const_type
      && Constr.isRef const_type
  ;;

  (* See the [.mli]. *)
  let can_be_unfolded (sigma : Evd.evar_map) (x : EConstr.t) : bool =
    Logger.trace __FUNCTION__;
    try
      let g, i = EConstr.destRef sigma x in
      match g with
      | ConstRef y ->
        (match Global.lookup_constant y with
         | { const_body = Def z; const_type; _ } ->
           unfoldable_definition z const_type
         | _ -> false)
      | _ -> false
    with
    | Constr.DestKO ->
      log_econstr ~__FUNCTION__ ~s:"Err: Constr.DestKO, x" x;
      false
  ;;

  (* See the [.mli]. Within one term the constants are distinct
     ({!collect_component_econstrs} returns a set). Across terms only
     {!try_unfold_any_of} combines several, at one call site
     ([Proof_solver_step.handle_hyp_transition], on the conclusion's
     [wk_trans] and [wk_sim]), where a constant in both would be unfolded
     twice. Measured 2026-10-01 over the whole proof suite (27 [Solve]s, 829
     calls): the two never share an unfoldable constant (14 calls find 6
     distinct, the rest none), so no dedup is done. Backlog item A4. *)
  let try_unfold_any ?(in_hyp : Rocq_utils.hyp option) (x : EConstr.t)
    : Tactic.t option mm
    =
    Logger.trace __FUNCTION__;
    let open Syntax in
    let* sigma = get_sigma in
    (* NOTE: [collect_component_econstrs] ensures no duplicates. *)
    match collect_component_econstrs sigma x |> EConstrSet.to_list with
    | [] -> return None
    | to_check ->
      let ys =
        List.filter_map
          (fun (x : EConstr.t) ->
            if Theory.is_any_theory x
            then None
            else if can_be_unfolded sigma x
            then Some (f_unfold_hyp unfold_econstr ~in_hyp x)
            else None)
          to_check
      in
      (match ys with
       | [] -> return None
       | ys -> Some (Tactic.chain ys) |> return)
  ;;

  (* See the [.mli]. *)
  let rec try_unfold_any_of : EConstr.t list -> Tactic.t option mm =
    Logger.trace __FUNCTION__;
    function
    | [] -> return None
    | h :: tl ->
      let open Syntax in
      let* h_opt = try_unfold_any h in
      let* tl_opt = try_unfold_any_of tl in
      (match h_opt, tl_opt with
       | Some x, Some y -> Some (Tactic.seq x y) |> return
       | None, Some y -> return (Some y)
       | Some x, None -> return (Some x)
       | None, None -> return None)
  ;;

  exception NoRocqLTSFoundWithEnc of Enc.t

  (* See the [.mli]. *)
  let find_lts (lts_enc : Enc.t)
    : Model.Info.Meta.RocqLTS.t list -> Model.Info.Meta.RocqLTS.t
    =
    Logger.trace __FUNCTION__;
    try
      List.find (fun ({ base; _ } : Model.Info.Meta.RocqLTS.t) ->
        Enc.equal base lts_enc)
    with
    | Not_found -> raise (NoRocqLTSFoundWithEnc lts_enc)
  ;;

  exception NoConstructorFoundWithIndex of int

  (* See the [.mli]. *)
  let find_constructor (constructor_index : int)
    : ConstructorBindings.t list -> ConstructorBindings.t
    =
    Logger.trace __FUNCTION__;
    try
      List.find (fun ({ index; _ } : ConstructorBindings.t) ->
        Int.equal index constructor_index)
    with
    | Not_found -> raise (NoConstructorFoundWithIndex constructor_index)
  ;;

  type binding_args =
    { from : EConstr.t
    ; goto : EConstr.t option
    ; label : EConstr.t option
    }

  (* See the [.mli]. *)
  let get_constructor_bindings
        ({ from; goto; label } : binding_args)
        (bindings : Bindings.t)
    : EConstr.t Tactypes.bindings
    =
    Logger.trace __FUNCTION__;
    W.ConstructorBindings.get from label goto bindings |> W.M.run
  ;;

  (* See the [.mli]. *)
  let try_get_constructor_bindings
        ((enc, index) : Enc.Tree.Node.t)
        (args : binding_args)
    : EConstr.t Tactypes.bindings
    =
    Logger.trace __FUNCTION__;
    match (W.get_fsm_b ()).info.meta with
    | None -> Tactypes.NoBindings
    | Some { lts; _ } ->
      let { constructors; _ } : Model.Info.Meta.RocqLTS.t = find_lts enc lts in
      let { bindings; _ } : ConstructorBindings.t =
        find_constructor index constructors
      in
      get_constructor_bindings args bindings
  ;;

  (** [is_lts_goal sigma g] is whether the goal [g] is an LTS step of
      either FSM, or already solved. Raises nothing. *)
  let is_lts_goal (sigma : Evd.evar_map) (gl : Proofview_monad.goal_with_state)
    : bool
    =
    let ev = Proofview.drop_state gl in
    (not (Evd.is_undefined sigma ev))
    ||
    let concl = Evd.evar_concl (Evd.find_undefined sigma ev) in
    let h, _ = EConstr.decompose_app sigma concl in
    let lts_of (m : Model.FSM.t) : bool =
      try Theory.is_fsm_constructor h m with _ -> false
    in
    lts_of (W.get_fsm_a ()) || lts_of (W.get_fsm_b ())
  ;;

  (** Run right after a constructor is applied, while all the subgoals it
      made are visible (a solver step only ever sees the first goal): move
      the premises that are not LTS steps behind the LTS ones, keeping each
      group's order. The derivation tree replays only LTS premises, in
      order, so this leaves its replay unchanged; a premise that computes
      what an LTS premise needs ([In q l -> lts q a q']) then arrives with
      [q] already fixed by that premise's constructor (backlog I2, stage 2).
      With no such premises (every existing example) it moves nothing. *)
  let move_premises_last : unit Proofview.tactic =
    let open Proofview.Notations in
    Proofview.tclEVARMAP
    >>= fun sigma ->
    Proofview.Unsafe.tclGETGOALS
    >>= fun gls ->
    let lts, others = List.partition (is_lts_goal sigma) gls in
    Proofview.Unsafe.tclSETGOALS (lts @ others)
  ;;

  (** [goal_concl sigma g] is the conclusion of the goal [g]. Raises
      nothing for a goal still open. *)
  let goal_concl (sigma : Evd.evar_map) (gl : Proofview_monad.goal_with_state)
    : EConstr.t
    =
    Evd.evar_concl (Evd.find_undefined sigma (Proofview.drop_state gl))
  ;;

  (** [union_evars sigma ts] is every evar the terms [ts] mention. Raises
      nothing. *)
  let union_evars (sigma : Evd.evar_map) (ts : EConstr.t list) : Evar.Set.t =
    List.fold_left
      (fun acc t -> Evar.Set.union acc (Evd.evars_of_term sigma t))
      Evar.Set.empty
      ts
  ;;

  (** [open_witnesses sigma lts others] is the witnesses no LTS premise will
      fix -- the evars of the premise goals [others] that none of the LTS
      goals [lts] mentions -- and the premise goals that mention one of them
      but no evar an LTS goal mentions, which are the ones to solve now.
      Raises nothing. *)
  let open_witnesses
        (sigma : Evd.evar_map)
        (lts : Proofview_monad.goal_with_state list)
        (others : Proofview_monad.goal_with_state list)
    : Evar.Set.t * EConstr.t list
    =
    let in_lts : Evar.Set.t =
      union_evars sigma (List.map (goal_concl sigma) lts)
    in
    let premises : EConstr.t list = List.map (goal_concl sigma) others in
    let witnesses : Evar.Set.t =
      Evar.Set.diff (union_evars sigma premises) in_lts
    in
    let involved : EConstr.t list =
      List.filter
        (fun p ->
          let e = Evd.evars_of_term sigma p in
          (not (Evar.Set.is_empty (Evar.Set.inter e witnesses)))
          && Evar.Set.is_empty (Evar.Set.inter e in_lts))
        premises
    in
    witnesses, involved
  ;;

  (** [conjunction env p ps] is the conjunction [p /\ q1 /\ ...] of [p] and
      each of [ps], left-nested. Raises Rocq's errors if [and] is not
      registered (propagated; it always is). *)
  let conjunction (env : Environ.env) (p : EConstr.t) (ps : EConstr.t list)
    : EConstr.t
    =
    let and_ : EConstr.t =
      EConstr.of_constr
        (UnivGen.constr_of_monomorphic_global
           env
           (Rocqlib.lib_ref "core.and.type"))
    in
    List.fold_left (fun acc q -> EConstr.mkApp (and_, [| acc; q |])) p ps
  ;;

  (** [fixes_closed ws sol] is whether the evar map [sol] gives every evar
      of [ws] a closed value. Raises nothing. *)
  let fixes_closed (ws : Evar.Set.t) (sol : Evd.evar_map) : bool =
    Evar.Set.for_all
      (fun ev ->
        match Evd.find_defined sol ev with
        | None -> false
        | Some info ->
          (match Evd.evar_body info with
           | Evd.Evar_defined c ->
             Evar.Set.is_empty
               (Evd.evars_of_term sol (Reductionops.nf_evar sol c))))
      ws
  ;;

  (** Run after [move_premises_last], while the constructor's subgoals are all
      visible. A binder that appears only in premises that are not LTS steps
      ([q] in [base q a q' -> open_c n a q'] with [base] not in [Using]) is an
      evar no LTS premise's replay will fix, and the premise goal it leaves open
      ([base ?q a 1]) cannot be proved as it is: the search proves closed goals.
      Such witnesses are chosen here, by enumerating every premise goal that
      mentions them together (a choice that suits one premise may fail another:
      [In q [0; 1]] and [base q a 2]) and committing the first solution that
      fixes each of them to a closed term. Any such solution will do: no other
      goal mentions them. A goal that also mentions an evar an LTS premise fixes
      is left alone, so as not to pre-empt that replay. Until 2026-10-03 the
      solver stopped on such a goal ("cannot prove the constructor premise"). *)
  let fix_premise_witnesses : unit Proofview.tactic =
    let open Proofview.Notations in
    Proofview.tclENV
    >>= fun env ->
    Proofview.tclEVARMAP
    >>= fun sigma ->
    Proofview.Unsafe.tclGETGOALS
    >>= fun gls ->
    let lts, others = List.partition (is_lts_goal sigma) gls in
    let witnesses, involved = open_witnesses sigma lts others in
    match involved with
    | [] -> Proofview.tclUNIT ()
    | p :: ps ->
      let mentioned : Evar.Set.t = union_evars sigma involved in
      let sols, _complete =
        Premise_search.enumerate env sigma (conjunction env p ps)
      in
      (match
         List.find_opt (fixes_closed (Evar.Set.inter witnesses mentioned)) sols
       with
       | Some sol -> Proofview.Unsafe.tclEVARS sol
       | None -> Proofview.tclUNIT ())
  ;;

  exception GoalNotAnLTSStep

  (* See the [.mli]. *)
  let apply_constructor ((enc, index) : Enc.Tree.Node.t) (args : binding_args)
    : Tactic.t mm
    =
    Logger.trace __FUNCTION__;
    (* NOTE: constructors index from 1 *)
    let index : int = index + 1 in
    let msg : string = Printf.sprintf "constructor %i" index in
    let bindings =
      try try_get_constructor_bindings (enc, index) args with
      | ConstructorBindings.BindingInstruction_NotApp _ ->
        raise GoalNotAnLTSStep
    in
    Logger.thing ~__FUNCTION__ Debug "bindings" bindings Strfy.econstr_bindings;
    (* [econstructor], not [constructor]: a binder that appears only in a
       premise ([q] in [In q l -> lts q a q' -> sys l a q']) has no binding to
       come from, and is left for unification to fill (backlog I2, stage 2).
       When every binder is bound the two are the same. *)
    Tactic.create
      ~msg
      (Proofview.tclTHEN
         (Proofview.tclTHEN
            (Tactics.constructor_tac true None index bindings)
            move_premises_last)
         fix_premise_witnesses)
    |> return
  ;;
end
