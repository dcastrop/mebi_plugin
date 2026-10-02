(** Bounded proof search for constructor premises that are not over an LTS
    (backlog item I2, stage 1 of [notes/9-general-premise-support.md]).

    Shared by extraction (which only needs the verdict) and the proof solver
    (which [exact]s the proof found). Three outcomes, never a guess:
    [Proved p] with a proof term [p]; [Refuted] only when the search was
    {e complete}; [Unknown] otherwise. *)

(** How a premise was proved. A negation [~ P] has no proof term from
    constructor search; it holds because [P] was refuted, and the solver
    proves it with {!negation_tac} instead. *)
type proof =
  | Term of EConstr.t
  | ByRefutation of EConstr.t

type result =
  | Proved of proof
  | Refuted
  | Unknown

(** A user tactic tried on premises the search leaves undecided
    ([MeBi Config Premise Tactic]): proving [P] means it holds, proving
    [~ P] that it is false. *)
let user_tactic : unit Proofview.tactic option ref = ref None

(** The most nested constructor applications a search may try. *)
let default_depth : int = 16

let max_depth : int ref = ref default_depth

let is_prop (env : Environ.env) (sigma : Evd.evar_map) (t : EConstr.t) : bool =
  try
    match Retyping.get_sort_quality_of env sigma t with
    | UnivGen.QualityOrSet.Qual q -> Sorts.Quality.is_qprop q
    | UnivGen.QualityOrSet.Set -> false
  with
  | _ -> false
;;

let is_eq_ind (ind : Names.inductive) : bool =
  Rocqlib.check_ind_ref "core.eq.type" ind
;;

let closed (sigma : Evd.evar_map) (x : EConstr.t) : bool =
  Evar.Set.is_empty (Evd.evars_of_term sigma x)
;;

(** A term built only from constructors (after normalization): the only
    kind of argument on which a failed match {e proves} a premise false. Type
    arguments (whose type is a sort) are exempt. [le (f x) 3] with an opaque
    [f] is not ground, so no constructor matching it is not a refutation. *)
let rec ground (env : Environ.env) (sigma : Evd.evar_map) (x : EConstr.t) : bool
  =
  let h, args = EConstr.decompose_app sigma x in
  match EConstr.kind sigma h with
  | Construct _ -> Array.for_all (arg_ground env sigma) args
  | _ -> false

and arg_ground (env : Environ.env) (sigma : Evd.evar_map) (x : EConstr.t) : bool
  =
  let is_type_arg : bool =
    try
      EConstr.isSort
        sigma
        (Reductionops.whd_all env sigma (Retyping.get_type_of env sigma x))
    with
    | _ -> false
  in
  is_type_arg || ground env sigma x
;;

(** A closed term with nothing opaque left after full normalization: no
    opaque constant or axiom, free variable, evar, or stuck [match]/fixpoint
    -- only constructors, inductives, sorts and binders. Conversion is
    complete on such terms, so a parameter like [fun k => k <= 1] is fine,
    while [f x] with an opaque [f] is not. *)
let evaluated (env : Environ.env) (sigma : Evd.evar_map) (x : EConstr.t) : bool =
  let rec ok (x : EConstr.t) : bool =
    match EConstr.kind sigma x with
    | Rel _ | Sort _ | Ind _ | Construct _ -> true
    | Cast (c, _, t) -> ok c && ok t
    | Prod (_, a, b) | Lambda (_, a, b) -> ok a && ok b
    | LetIn (_, a, t, b) -> ok a && ok t && ok b
    | App (h, args) -> ok h && Array.for_all ok args
    | _ -> false
  in
  ok (Reductionops.nf_all env sigma x)
;;

exception NoUnify

let unify
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (a : EConstr.t)
      (b : EConstr.t)
  : Evd.evar_map
  =
  try snd (Unification.w_unify env sigma Conversion.CONV a b) with
  | Pretype_errors.PretypeError _ | Evarconv.UnableToUnify _ -> raise NoUnify
;;

(** [search env sigma depth goal]: [Some (sigma', proof)] for the first proof
    found, and whether the search was complete (no depth cut, no undecidable
    leaf, only ground arguments where a match failed). *)
let rec search
          (env : Environ.env)
          (sigma : Evd.evar_map)
          (depth : int)
          (goal : EConstr.t)
  : (Evd.evar_map * EConstr.t) option * bool
  =
  let goal = Reductionops.whd_all env sigma goal in
  let h, args = EConstr.decompose_app sigma goal in
  match EConstr.kind sigma h with
  | Ind (ind, u) when is_eq_ind ind && Array.length args = 3 ->
    (* Equations are decided as by extraction: convertible holds, a
       constructor difference refutes, anything else is unknown. *)
    let l = Reductionops.nf_all env sigma args.(1) in
    let r = Reductionops.nf_all env sigma args.(2) in
    if Reductionops.is_conv env sigma l r
    then (
      let refl = EConstr.mkConstructU ((ind, 1), u) in
      Some (sigma, EConstr.mkApp (refl, [| args.(0); args.(1) |])), true)
    else if ground env sigma l && ground env sigma r
    then None, true
    else None, false
  | Ind (ind, u) when is_prop env sigma goal ->
    if depth <= 0
    then None, false
    else (
      let mib, oib = Inductive.lookup_mind_specif env ind in
      let n = Array.length oib.Declarations.mind_consnames in
      (* A failed match refutes only if nothing opaque could be hiding a
         proof: parameters (uniform across constructors) must be
         [evaluated], and indices -- where a match actually fails -- ground
         constructor terms (or types). *)
      let np = mib.Declarations.mind_nparams in
      let refutable =
        Array.for_all
          Fun.id
          (Array.mapi
             (fun i a ->
               if i < np then evaluated env sigma a else arg_ground env sigma a)
             args)
      in
      let rec try_ctor (i : int) (complete : bool) =
        if i > n
        then None, complete
        else (
          match try_constructor env sigma depth goal ((ind, i), u) with
          | Some r, _ -> Some r, complete
          | None, c -> try_ctor (i + 1) (complete && c))
      in
      let found, complete = try_ctor 1 true in
      found, complete && refutable)
  | _ -> (* not an inductive proposition: undecidable here *) None, false

(** Try one constructor: fresh evars for its binders, its conclusion unified
    with [goal], then every [Prop] binder still open searched for, left to
    right, at [depth - 1]. *)
and try_constructor
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (depth : int)
      (goal : EConstr.t)
      (c : Names.constructor * EConstr.EInstance.t)
  : (Evd.evar_map * EConstr.t) option * bool
  =
  let ctor = EConstr.mkConstructU c in
  let rec binders sigma (ty : EConstr.t) (acc : (EConstr.t * EConstr.t) list) =
    match EConstr.kind sigma (Reductionops.whd_all env sigma ty) with
    | Prod (_, a, b) ->
      let sigma, e = Evarutil.new_evar env sigma a in
      binders sigma (EConstr.Vars.subst1 e b) ((e, a) :: acc)
    | _ -> sigma, ty, List.rev acc
  in
  let sigma, concl, evs =
    binders sigma (Retyping.get_type_of env sigma ctor) []
  in
  match unify env sigma concl goal with
  | exception NoUnify -> None, true
  | sigma ->
    (* [committed]: an earlier premise was solved while it still had open
       variables, so only its {e first} proof was taken and others might
       instantiate them differently. A later failure is then not a refutation
       (e.g. [R x y -> R y z -> R x z] with [y] free). *)
    let rec premises sigma (committed : bool) = function
      | [] -> Some sigma, true
      | (e, a) :: tl ->
        let a = Reductionops.nf_evar sigma a in
        if
          EConstr.isEvar sigma (Reductionops.whd_evar sigma e)
          && is_prop env sigma a
        then (
          match search env sigma (depth - 1) a with
          | None, c -> None, c && not committed
          | Some (sigma', p), _ ->
            (match unify env sigma' e p with
             | exception NoUnify -> None, false
             | sigma' -> premises sigma' (committed || not (closed sigma a)) tl))
        else premises sigma committed tl
    in
    (match premises sigma false evs with
     | None, c -> None, c
     | Some sigma, _ ->
       let proof =
         Reductionops.nf_evar
           sigma
           (EConstr.mkApp (ctor, Array.of_list (List.map fst evs)))
       in
       (* a binder nothing determined (an unconstrained witness): not a
          closed proof *)
       if closed sigma proof then Some (sigma, proof), true else None, false)
;;

(** [P -> False] (after head reduction, so [~ P] too): [Some P]. *)
let negated (env : Environ.env) (sigma : Evd.evar_map) (goal : EConstr.t)
  : EConstr.t option
  =
  match EConstr.kind sigma (Reductionops.whd_all env sigma goal) with
  | Prod (_, a, b) when EConstr.Vars.noccurn sigma 1 b ->
    (match EConstr.kind sigma (Reductionops.whd_all env sigma b) with
     | Ind (ind, _) when Rocqlib.check_ind_ref "core.False.type" ind -> Some a
     | _ -> None)
  | _ -> None
;;

let search_closed (env : Environ.env) (sigma : Evd.evar_map) (goal : EConstr.t)
  : result
  =
  match search env sigma !max_depth goal with
  | Some (sigma, p), _ ->
    (* belt and braces: the term must typecheck against the goal *)
    (match Typing.check env sigma p goal with
     | _ -> Proved (Term p)
     | exception _ -> Unknown)
  | None, true -> Refuted
  | None, false -> Unknown
;;

(** Run the user tactic, if any, on [typ]: its closed proof term. *)
let by_tactic (env : Environ.env) (sigma : Evd.evar_map) (typ : EConstr.t)
  : EConstr.t option
  =
  match !user_tactic with
  | None -> None
  | Some tac ->
    (try
       match
         Subproof.build_by_tactic_opt
           env
           ~uctx:(Evd.ustate sigma)
           ~poly:PolyFlags.default
           ~typ
           tac
       with
       | Some (c, _, _, _, _) -> Some (EConstr.of_constr c)
       | None -> None
     with
     | e when CErrors.noncritical e -> None)
;;

let negation_of (goal : EConstr.t) : EConstr.t =
  let false_ =
    EConstr.of_constr
      (UnivGen.constr_of_monomorphic_global
         (Global.env ())
         (Rocqlib.lib_ref "core.False.type"))
  in
  EConstr.mkArrowR goal false_
;;

(** The user tactic, if any, on [goal] and then on [~ goal]. *)
let by_user_tactic (env : Environ.env) (sigma : Evd.evar_map) (goal : EConstr.t)
  : result
  =
  match by_tactic env sigma goal with
  | Some p -> Proved (Term p)
  | None ->
    (match by_tactic env sigma (negation_of goal) with
     | Some _ -> Refuted
     | None -> Unknown)
;;

let prove (env : Environ.env) (sigma : Evd.evar_map) (goal : EConstr.t) : result
  =
  let goal = Reductionops.nf_evar sigma goal in
  if not (closed sigma goal)
  then Unknown
  else (
    let decided =
      match negated env sigma goal with
      | Some p ->
        (* [~ P] holds iff [P] is refuted, by a complete search *)
        (match search_closed env sigma p with
         | Proved _ -> Refuted
         | Refuted -> Proved (ByRefutation p)
         | Unknown -> Unknown)
      | None -> search_closed env sigma goal
    in
    match decided with Unknown -> by_user_tactic env sigma goal | r -> r)
;;

(** [refute_hyp_tac id]: close the goal from hypothesis [id], a closed
    premise that [prove] refutes. An inductive one is unfolded ([simpl in],
    so a fixpoint like [In] becomes [or]/[eq]/[False]) and cleared by
    [inversion_clear], and every goal that leaves is refuted the same way
    from a new refutable hypothesis; a negation [~ P] is applied to the
    proof of [P]. [inversion_clear], not [inversion]: a kept hypothesis is
    picked again forever (the Step 0 loop). *)
let rec refute_hyp_tac ?(depth : int = !max_depth) (id : Names.Id.t)
  : unit Proofview.tactic
  =
  let open Proofview.Notations in
  Proofview.Goal.enter (fun gl ->
    let env = Proofview.Goal.env gl in
    let sigma = Proofview.Goal.sigma gl in
    let ty = Context.Named.Declaration.get_type (EConstr.lookup_named id env) in
    match negated env sigma ty with
    | Some p ->
      (match search_closed env sigma p with
       | Proved (Term pf) ->
         let false_elim = EConstr.mkApp (EConstr.mkVar id, [| pf |]) in
         Tactics.exfalso <*> Tactics.exact_check false_elim
       | _ ->
         Tacticals.tclZEROMSG (Pp.str "MeBi: cannot refute a negated premise"))
    | None
      when match search_closed env sigma ty with
           | Refuted -> false
           | Proved _ | Unknown -> true ->
      (* refuted by the user tactic, not by search: use its proof of [~ ty] *)
      (match by_tactic env sigma (negation_of ty) with
       | Some np ->
         Tactics.exfalso
         <*> Tactics.exact_check (EConstr.mkApp (np, [| EConstr.mkVar id |]))
       | None -> Tacticals.tclZEROMSG (Pp.str "MeBi: cannot refute a premise"))
    | None ->
      if depth <= 0
      then Tacticals.tclZEROMSG (Pp.str "MeBi: premise refutation too deep")
      else
        Tactics.simpl_in_hyp (id, Locus.InHyp)
        <*> Inv.inv_clear_tac id
        <*> Proofview.Goal.enter (fun gl ->
          let env = Proofview.Goal.env gl in
          let sigma = Proofview.Goal.sigma gl in
          let refutable =
            List.find_opt
              (fun d ->
                let t = Context.Named.Declaration.get_type d in
                is_prop env sigma t
                && closed sigma t
                && match prove env sigma t with Refuted -> true | _ -> false)
              (Proofview.Goal.hyps gl)
          in
          match refutable with
          | Some d ->
            refute_hyp_tac
              ~depth:(depth - 1)
              (Context.Named.Declaration.get_id d)
          | None ->
            Tacticals.tclZEROMSG (Pp.str "MeBi: no refutable premise left")))
;;

(** [negation_tac]: prove a goal [~ P] whose [P] [prove] refutes:
    [intro H], then {!refute_hyp_tac} [H]. *)
let negation_tac : unit Proofview.tactic =
  (* [hnf] first: [~ P] is the constant [not], not yet a product. *)
  Proofview.tclTHEN
    Tactics.hnf_in_concl
    (Tactics.intro_using_then (Names.Id.of_string "H_premise") (fun id ->
       refute_hyp_tac id))
;;
