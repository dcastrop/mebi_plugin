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
  val subst_all : unit -> tactic mm
  val simplify : unit -> tactic mm
  val simplify_concl : unit -> tactic mm
  val simplify_hyp : Rocq_utils.hyp -> tactic mm
  val simplify_hyps : unit -> tactic mm
  val simplify_all : unit -> tactic mm
  val simplify_and_subst_all : unit -> tactic mm
  val cofix : unit -> tactic mm
  val trivial : ?msg:string -> unit -> tactic mm
  val ex_intro : state -> tactic mm
  val split : unit -> tactic mm
  val ex_intro_split : state -> tactic mm
  val intros_all : unit -> tactic mm
  val intro_as : string -> tactic mm
  val apply : Evd.econstr -> tactic mm
  val apply_Pack_sim : unit -> tactic mm
  val apply_In_sim : unit -> tactic mm
  val apply_wk_none : unit -> tactic mm
  val apply_rt1n_refl : unit -> tactic mm
  val apply_rt1n_trans : unit -> tactic mm
  val apply_weak_sim_refl : unit -> tactic mm
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
  val unfold_silent1 : unit -> tactic
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

  val apply_constructor : node -> binding_args -> tactic mm
end

module Make
    (Log : Logger.S)
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

  (* *)
  open Iter

  let inversion (x : Rocq_utils.hyp) : Tactic.t mm =
    Inv.inv_tac (Context.Named.Declaration.get_id x)
    |> Tactic.create
         ~msg:
           (Printf.sprintf
              "inversion %s"
              (Strfy.hyp_name x) (* (Strfy.hyp_type x) *))
    |> return
  ;;

  let subst_all () : Tactic.t mm =
    Equality.subst_all ()
    |> Tactic.create ~kind:Info ~msg:(Printf.sprintf "(subst all)")
    |> return
  ;;

  (** by specifying [None] it appears to be the same as using [simpl in *]. *)
  let simplify () : Tactic.t mm =
    Tactics.simpl_option None |> Tactic.create ~msg:"simpl" |> return
  ;;

  let simplify_concl () : Tactic.t mm =
    Tactics.simpl_in_concl |> Tactic.create ~msg:"simpl" |> return
  ;;

  let simplify_hyp (x : Rocq_utils.hyp) : Tactic.t mm =
    Tactics.simpl_in_hyp (Context.Named.Declaration.get_id x, Locus.InHyp)
    |> Tactic.create ~msg:(Printf.sprintf "simpl in %s" (Strfy.hyp_name x))
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

  (** not needed, manual/explicit version of [simplfy ()] *)
  let simplify_all () : Tactic.t mm =
    let open Syntax in
    let* concl : Tactic.t = simplify_concl () in
    let* hyps : Tactic.t = simplify_hyps () in
    Tactic.seq concl hyps |> return
  ;;

  let simplify_and_subst_all () : Tactic.t mm =
    let open Syntax in
    (* let* simpls : Tactic.t = simplify_all () in *)
    let* simpls : Tactic.t = simplify () in
    let* substs : Tactic.t = subst_all () in
    Tactic.seq simpls substs |> return
  ;;

  let cofix () : Tactic.t mm =
    let name : Names.Id.t = new_cofix_name () in
    Tactics.cofix name
    |> Tactic.create ~msg:(Printf.sprintf "cofix %s" (Names.Id.to_string name))
    |> return
  ;;

  (** [trivial ?msg ()] applies the [Auto.gen_trivial] (i.e., [trivial]) tactic. If the [module Log] is configured to display [Output.Kind.Info] messages, then the equivalent of tactic [info_trivial] is used instead.
  *)
  let trivial ?(msg : string = "trivial") () : Tactic.t mm =
    let f : string list option -> unit Proofview.tactic =
      if Log.Config.is_enabled Output.Kind.Info
      then Auto.gen_trivial ~debug:Hints.Info []
      else Auto.gen_trivial []
    in
    Tactic.create ~msg (f None) |> return
  ;;

  let ex_intro (x : Model.State.t) : Tactic.t mm =
    let t : EConstr.t = Decode.state x in
    let bindings = Tactypes.ImplicitBindings [ t ] in
    let msg = Printf.sprintf "exists %s" (Strfy.econstr t) in
    Tactic.create ~msg (Tactics.constructor_tac true None 1 bindings) |> return
  ;;

  let split () : Tactic.t mm =
    Tactic.create (Tactics.split Tactypes.NoBindings) |> return
  ;;

  let ex_intro_split (x : Model.State.t) : Tactic.t mm =
    let open Syntax in
    let* ex_intro : Tactic.t = ex_intro x in
    let* split : Tactic.t = split () in
    Tactic.seq ex_intro split |> return
  ;;

  let intros_all () : Tactic.t mm =
    Tactics.intros |> Tactic.create ~msg:"intros" |> return
  ;;

  (** [intro_as x] applies the introduction tactic using the (next non-conficting) name [x].
  *)
  let intro_as (x : string) : Tactic.t mm =
    let name : Names.Id.t = new_name_of_string x in
    Tactics.introduction name
    |> Tactic.create ~msg:(Printf.sprintf "intro %s" (Names.Id.to_string name))
    |> return
  ;;

  (* *)
  let apply (x : EConstr.t) : Tactic.t mm =
    Tactics.apply x
    |> Tactic.create ~msg:(Printf.sprintf "apply %s" (Strfy.econstr x))
    |> return
  ;;

  let apply_Pack_sim () : Tactic.t mm = apply (Mebi_theories.c_Pack_sim ())
  let apply_In_sim () : Tactic.t mm = apply (Mebi_theories.c_In_sim ())
  let apply_wk_none () : Tactic.t mm = apply (Mebi_theories.c_wk_none ())
  let apply_rt1n_refl () : Tactic.t mm = apply (Mebi_theories.c_rt1n_refl ())
  let apply_rt1n_trans () : Tactic.t mm = apply (Mebi_theories.c_rt1n_trans ())

  let apply_weak_sim_refl () : Tactic.t mm =
    apply (Mebi_theories.c_weak_sim_refl ())
  ;;

  (* let apply_wk_bisim_refl () : Tactic.t mm = apply (Mebi_theories.c_wk_bisim_refl ()) *)

  let eapply (x : EConstr.t) : Tactic.t mm =
    Tactics.eapply x
    |> Tactic.create ~msg:(Printf.sprintf "eapply %s" (Strfy.econstr x))
    |> return
  ;;

  let eapply_wk_some () : Tactic.t mm = eapply (Mebi_theories.c_wk_some ())
  let eapply_rt1n_refl () : Tactic.t mm = eapply (Mebi_theories.c_rt1n_refl ())

  let eapply_rt1n_trans () : Tactic.t mm =
    eapply (Mebi_theories.c_rt1n_trans ())
  ;;

  (** {b counter intuitively, this applies a transition if the label is silent.} This is because we use this to determine if we need to unfold the [weak] transition from the [Bisimilarity.v] theory.
  *)
  let eapply_rt1n_via (x : Model.Label.t) : Tactic.t mm =
    if Model.Label.is_silent x
    then eapply_rt1n_trans ()
    else eapply_rt1n_refl ()
  ;;

  (* *)
  exception CannotUnfoldConstr of Constr.t

  (** [unfold_constr ?in_hyp x] ... {e NOTE: term [x] is always unfolded. If [?in_hyp] is provided then we {b also} unfold [x] [in_hyp].}
      @raise CannotUnfoldConstr of [x] if [Constr.kind x] is not [Const (_, _)].
  *)
  let unfold_constr ?(in_hyp : Rocq_utils.hyp option) (x : Constr.t) : Tactic.t =
    Log.trace __FUNCTION__;
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

  (** [handle_unfold_hyp_opt f ?in_hyp x] helps keep this function cleaner to use. i.e., [unfold_econstr ~in_hyp:x] rather than [~in_hyp:(Some x)].
  *)
  let f_unfold_hyp
        (f : ?in_hyp:Rocq_utils.hyp -> 'a -> Tactic.t)
        ?(in_hyp : Rocq_utils.hyp option = None)
        (x : 'a)
    : Tactic.t
    =
    Log.trace __FUNCTION__;
    match in_hyp with None -> f x | Some in_hyp -> f ~in_hyp x
  ;;

  let unfold_econstr ?(in_hyp : Rocq_utils.hyp option) (x : EConstr.t)
    : Tactic.t
    =
    Log.trace __FUNCTION__;
    (* let* y : Constr.t = econstr_to_constr x in *)
    econstr_to_constr x |> run |> f_unfold_hyp unfold_constr ~in_hyp
  ;;

  let unfold_constrexpr
        ?(in_hyp : Rocq_utils.hyp option)
        (x : Constrexpr.constr_expr)
    : Tactic.t
    =
    Log.trace __FUNCTION__;
    (* let open Syntax in *)
    (* let* y : EConstr.t = constrexpr_to_econstr x in *)
    constrexpr_to_econstr x |> run |> f_unfold_hyp unfold_econstr ~in_hyp
  ;;

  let unfold_opt_constrexpr_list ?(in_hyp : Rocq_utils.hyp option)
    : Constrexpr.constr_expr list -> Tactic.t option
    =
    Log.trace __FUNCTION__;
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

  let unfold_silent () : Tactic.t = unfold_econstr (Mebi_theories.c_silent ())
  let unfold_silent1 () : Tactic.t = unfold_econstr (Mebi_theories.c_silent1 ())

  (* *)
  let do_refl () : Tactic.t mm =
    Log.trace __FUNCTION__;
    let open Syntax in
    let* wk_none = apply_wk_none () in
    let unfold_silent = unfold_silent () in
    let* rt1n_refl = apply_rt1n_refl () in
    Tactic.chain [ wk_none; unfold_silent; rt1n_refl ] |> return
  ;;

  (* *)
  let collect_component_econstrs (sigma : Evd.evar_map) (x : EConstr.t)
    : EConstrSet.t
    =
    Log.trace __FUNCTION__;
    (* log_econstr ~__FUNCTION__ ~s:"x" x; *)
    let is_constr_ref (x : EConstr.t) : bool =
      EConstr.isRef sigma x && EConstr.isConst sigma x
    in
    let acc_constr_ref (x : EConstr.t) (acc : EConstrSet.t) : EConstrSet.t =
      if is_constr_ref x then EConstrSet.add x acc else acc
    in
    let rec f (acc : EConstrSet.t) (y : EConstr.t) : EConstrSet.t =
      (* log_econstr ~__FUNCTION__ ~s:"y" y; *)
      let acc : EConstrSet.t = acc_constr_ref y acc in
      try
        let ty, tys = Rocq_utils.econstr_to_atomic sigma y in
        (* log_econstr ~__FUNCTION__ ~s:"ty" ty; *)
        (* _log_econstr_kind ~__FUNCTION__ "ty" ty; *)
        let acc : EConstrSet.t =
          match EConstr.kind sigma ty with
          | Case (_, _, _, _, _, c, _) ->
            (* _log_econstr_kind ~__FUNCTION__ "c" c; *)
            (match EConstr.kind sigma c with
             | App (ty, _) -> acc_constr_ref ty acc
             | _ -> acc)
          | _ -> acc
        in
        (* log_econstrs ~__FUNCTION__ "tys" (Array.to_list tys); *)
        let acc : EConstrSet.t = acc_constr_ref ty acc in
        Array.fold_left
          (fun (acc : EConstrSet.t) (z : EConstr.t) -> f acc z)
          acc
          tys
      with
      | Rocq_utils.Rocq_utils_EConstrIsNotA_Type _ ->
        (* log_econstr ~__FUNCTION__ ~s:"Err: Rocq_utils_EConstrIsNotA_Type" x; *)
        acc
    in
    f EConstrSet.empty x
  ;;

  (** [can_be_unfolded sigma x] returns [true] if [x] can be {e unfolded}, i.e., refers to a definition, e.g., of a definition, fixpoint or example.
  *)
  let can_be_unfolded (sigma : Evd.evar_map) (x : EConstr.t) : bool =
    Log.trace __FUNCTION__;
    try
      let g, i = EConstr.destRef sigma x in
      match g with
      | ConstRef y ->
        (match Global.lookup_constant y with
         | { const_body = Def z; const_type; _ } ->
           (* log_econstr ~__FUNCTION__ ~s:"x" x; *)
           (* _log_constr_kind ~__FUNCTION__ "(z kinds, z)" z; *)
           (* _log_constr_kind ~__FUNCTION__ "(kinds, const_type)" const_type; *)
           (match Constr.kind z with
            | Fix _ ->
              (* log_econstr ~__FUNCTION__ ~s:"is Fix" x; *)
              Constr.isProd const_type
            | Lambda _ ->
              (* log_econstr ~__FUNCTION__ ~s:"is Lambda" x; *)
              Constr.isProd const_type
            | App _ ->
              (* log_econstr ~__FUNCTION__ ~s:"is App" x; *)
              Constr.isRef const_type
              && (Constr.isConst const_type || Constr.isInd const_type)
            | Construct _ ->
              (* log_econstr ~__FUNCTION__ ~s:"is Construct" x; *)
              Constr.isRef z
              && Constr.isConst const_type
              && Constr.isRef const_type
            | _ ->
              (* NOTE: unfold, e.g., [SomeModule.example_1] *)
              (* log_econstr ~__FUNCTION__ ~s:"(skip)" x; *)
              Constr.isConst z
              && Constr.isRef z
              && Constr.isConst const_type
              && Constr.isRef const_type)
         | _ -> false)
      | _ -> false
    with
    | Constr.DestKO ->
      log_econstr ~__FUNCTION__ ~s:"Err: Constr.DestKO, x" x;
      false
  ;;

  (** [try_unfold_any x]
      (* TODO: make sure you remove duplicate things to unfold. *) *)
  let try_unfold_any ?(in_hyp : Rocq_utils.hyp option) (x : EConstr.t)
    : Tactic.t option mm
    =
    Log.trace __FUNCTION__;
    (* log_econstr ~__FUNCTION__ ~s:"x" x; *)
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

  let rec try_unfold_any_of : EConstr.t list -> Tactic.t option mm =
    Log.trace __FUNCTION__;
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

  (***********************************************************************)

  exception NoRocqLTSFoundWithEnc of Enc.t

  let find_lts (lts_enc : Enc.t)
    : Model.Info.Meta.RocqLTS.t list -> Model.Info.Meta.RocqLTS.t
    =
    Log.trace __FUNCTION__;
    try
      List.find (fun ({ base; _ } : Model.Info.Meta.RocqLTS.t) ->
        Enc.equal base lts_enc)
    with
    | Not_found -> raise (NoRocqLTSFoundWithEnc lts_enc)
  ;;

  exception NoConstructorFoundWithIndex of int

  let find_constructor (constructor_index : int)
    : ConstructorBindings.t list -> ConstructorBindings.t
    =
    Log.trace __FUNCTION__;
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

  let get_constructor_bindings
        ({ from; goto; label } : binding_args)
        (bindings : Bindings.t)
    : EConstr.t Tactypes.bindings
    =
    Log.trace __FUNCTION__;
    W.ConstructorBindings.get from label goto bindings |> W.M.run
  ;;

  (** if we have no way of obtaining the bindings (i.e., not info.meta) then we use no bindings.
      (* TODO: check if we can optimize this so we use [NoBindings] where possible *)
  *)
  let try_get_constructor_bindings
        ((enc, index) : Enc.Tree.Node.t)
        (args : binding_args)
    : EConstr.t Tactypes.bindings
    =
    Log.trace __FUNCTION__;
    match (W.get_fsm_b ()).info.meta with
    | None -> Tactypes.NoBindings
    | Some { lts; _ } ->
      let { constructors; _ } : Model.Info.Meta.RocqLTS.t = find_lts enc lts in
      let { bindings; _ } : ConstructorBindings.t =
        find_constructor index constructors
      in
      get_constructor_bindings args bindings
  ;;

  let apply_constructor ((enc, index) : Enc.Tree.Node.t) (args : binding_args)
    : Tactic.t mm
    =
    Log.trace __FUNCTION__;
    (* NOTE: constructors index from 1 *)
    let index : int = index + 1 in
    let msg : string = Printf.sprintf "constructor %i" index in
    (* let open Syntax in *)
    let bindings = try_get_constructor_bindings (enc, index) args in
    Log.thing ~__FUNCTION__ Debug "bindings" bindings Strfy.econstr_bindings;
    Tactic.create ~msg (Tactics.one_constructor index bindings) |> return
  ;;
end
