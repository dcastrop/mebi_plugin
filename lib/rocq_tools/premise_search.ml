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
  | ByCases

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

(** A term with nothing opaque left after full normalization: no opaque
    constant or axiom, free variable, or stuck [match]/fixpoint (open
    variables are fine)
    -- only constructors, inductives, sorts and binders. Conversion is
    complete on such terms, so a parameter like [fun k => k <= 1] is fine,
    while [f x] with an opaque [f] is not. *)
let evaluated (env : Environ.env) (sigma : Evd.evar_map) (x : EConstr.t) : bool =
  let rec ok (x : EConstr.t) : bool =
    match EConstr.kind sigma x with
    | Rel _ | Sort _ | Ind _ | Construct _ -> true
    (* an open variable is not opaque: unification against it is
       first-order and complete *)
    | Evar _ -> true
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

(** [unify_conclusion env sigma concl goal]: [unify], and failing that,
    argument by argument, left to right, each of [concl]'s arguments
    normalized once the earlier ones have instantiated its evars. A
    constructor whose index is computed from its binders ([ev k (dbl k)],
    [termLTS (tfix t) None (subst (tfix t) t)]) can defeat [w_unify] on the
    whole application, though the goal holds: [k := 2] first makes
    [dbl k] reduce to [4]. A success is a genuine solution; the proof found
    is still type-checked ([search_closed]). *)
let unify_conclusion
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (concl : EConstr.t)
      (goal : EConstr.t)
  : Evd.evar_map
  =
  try unify env sigma concl goal with
  | NoUnify ->
    let h1, a1 = EConstr.decompose_app sigma concl in
    let h2, a2 = EConstr.decompose_app sigma goal in
    if Array.length a1 <> Array.length a2 then raise NoUnify;
    let sigma = unify env sigma h1 h2 in
    let sigma = ref sigma in
    Array.iteri
      (fun i x ->
        let x =
          Reductionops.nf_all env !sigma (Reductionops.nf_evar !sigma x)
        in
        sigma := unify env !sigma x a2.(i))
      a1;
    !sigma
;;

(** The most solutions one search may enumerate; reaching it makes the
    search incomplete (some solutions may be missing). *)
let max_solutions : int = 64

(** An index argument a match can fail on decidably: built from
    constructors, with evars allowed at the leaves (an open pattern, such as
    a target still to be computed -- first-order unification against it is
    complete), or a type. *)
let rec pattern (env : Environ.env) (sigma : Evd.evar_map) (x : EConstr.t)
  : bool
  =
  let x = Reductionops.whd_evar sigma x in
  if EConstr.isEvar sigma x
  then true
  else (
    let h, args = EConstr.decompose_app sigma x in
    match EConstr.kind sigma h with
    | Construct _ -> Array.for_all (arg_pattern env sigma) args
    | _ -> false)

and arg_pattern (env : Environ.env) (sigma : Evd.evar_map) (x : EConstr.t)
  : bool
  =
  let is_type_arg : bool =
    try
      EConstr.isSort
        sigma
        (Reductionops.whd_all env sigma (Retyping.get_type_of env sigma x))
    with
    | _ -> false
  in
  is_type_arg || pattern env sigma (Reductionops.nf_all env sigma x)
;;

(** [search ~all env sigma depth goal]: the solutions -- each an evar map in
    which [goal]'s open variables may be instantiated, and a proof of the
    instantiated [goal] -- and whether they are {e all} of them (no depth
    cut, no undecidable leaf, no failed match on a non-pattern argument, cap
    not reached). With [~all:false] it stops at the first solution, and
    completeness then only matters when there is none. *)
let rec search
          ~(all : bool)
          (env : Environ.env)
          (sigma : Evd.evar_map)
          (depth : int)
          (goal : EConstr.t)
  : (Evd.evar_map * EConstr.t) list * bool
  =
  let goal = Reductionops.whd_all env sigma goal in
  let h, args = EConstr.decompose_app sigma goal in
  match EConstr.kind sigma h with
  | Ind (ind, u) when is_eq_ind ind && Array.length args = 3 ->
    let l = Reductionops.nf_all env sigma args.(1) in
    let r = Reductionops.nf_all env sigma args.(2) in
    let refl (sigma : Evd.evar_map) =
      EConstr.mkApp
        (EConstr.mkConstructU ((ind, 1), u), [| args.(0); args.(1) |])
      |> fun p -> sigma, p
    in
    if closed sigma l && closed sigma r
    then
      (* decided as by extraction: convertible holds, a constructor
         difference refutes, anything else is unknown *)
      if Reductionops.is_conv env sigma l r
      then [ refl sigma ], true
      else if ground env sigma l && ground env sigma r
      then [], true
      else [], false
    else (
      (* open: solve it by unification -- the unique solution when one side
         is a pattern, e.g. a target [m = S n] *)
      match unify env sigma l r with
      | sigma' -> [ refl sigma' ], pattern env sigma l && pattern env sigma r
      | exception NoUnify -> [], pattern env sigma l && pattern env sigma r)
  | Ind (ind, u) when is_prop env sigma goal ->
    if depth <= 0
    then [], false
    else (
      let mib, oib = Inductive.lookup_mind_specif env ind in
      let n = Array.length oib.Declarations.mind_consnames in
      (* A failed match counts (towards completeness) only if nothing opaque
         could be hiding a solution: parameters [evaluated], indices
         patterns. *)
      let np = mib.Declarations.mind_nparams in
      let decidable =
        Array.for_all
          Fun.id
          (Array.mapi
             (fun i a ->
               if i < np then evaluated env sigma a else arg_pattern env sigma a)
             args)
      in
      let rec try_ctor (i : int) acc (complete : bool) =
        if i > n || ((not all) && acc <> [])
        then acc, complete
        else (
          let sols, c =
            try_constructor ~all env sigma depth goal ((ind, i), u)
          in
          try_ctor (i + 1) (acc @ sols) (complete && c))
      in
      let sols, complete = try_ctor 1 [] true in
      if List.length sols > max_solutions
      then List.filteri (fun i _ -> i < max_solutions) sols, false
      else sols, complete && decidable)
  | _ -> (* not an inductive proposition: undecidable here *) [], false

(** One constructor: fresh evars for its binders, its conclusion unified
    with [goal], then every [Prop] binder still open searched for, left to
    right, at [depth - 1], every solution of one continuing into the next. *)
and try_constructor
      ~(all : bool)
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (depth : int)
      (goal : EConstr.t)
      (c : Names.constructor * EConstr.EInstance.t)
  : (Evd.evar_map * EConstr.t) list * bool
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
  match unify_conclusion env sigma concl goal with
  | exception NoUnify ->
    (* A failed match rules this constructor out only if the conclusion's
       indices are patterns too: first-order unification against those is
       complete. One computed by a function of the binders -- [do_fix]'s
       target [subst (tfix t) t] -- can fail to unify where the goal holds,
       so that is not a refutation. Until 2026-10-02 it counted as one, and
       [termLTS (tfix p) None (subst (tfix p) p)], true, was "refuted". *)
    let (((ind, _), _) : Names.constructor * EConstr.EInstance.t) = c in
    let mib, _ = Inductive.lookup_mind_specif env ind in
    let np = mib.Declarations.mind_nparams in
    let _, cargs = EConstr.decompose_app sigma concl in
    ( []
    , Array.for_all
        Fun.id
        (Array.mapi (fun i a -> i < np || arg_pattern env sigma a) cargs) )
  | sigma ->
    (* every partial solution continues into the next premise *)
    let rec premises (states : Evd.evar_map list) (complete : bool) = function
      | [] -> states, complete
      | (e, a) :: tl ->
        let step (sigma : Evd.evar_map) =
          let a = Reductionops.nf_evar sigma a in
          if
            EConstr.isEvar sigma (Reductionops.whd_evar sigma e)
            && is_prop env sigma a
          then (
            (* An open sub-premise is always fully enumerated: which of its
               solutions is taken can decide whether a later premise holds
               ([R x y -> R y z -> R x z] with [y] free), so stopping at the
               first could turn a solvable goal into a "refutation". A
               closed one instantiates nothing later premises see, so its
               first proof is as good as any. *)
            let all = all || not (closed sigma a) in
            let sols, c = search ~all env sigma (depth - 1) a in
            ( List.filter_map
                (fun (sigma', p) ->
                  match unify env sigma' e p with
                  | sigma'' -> Some sigma''
                  | exception NoUnify -> None)
                sols
            , c ))
          else [ sigma ], true
        in
        let results = List.map step states in
        premises
          (List.concat_map fst results)
          (complete && List.for_all snd results)
          tl
    in
    let states, complete = premises [ sigma ] true evs in
    let proofs =
      List.filter_map
        (fun sigma ->
          let proof =
            Reductionops.nf_evar
              sigma
              (EConstr.mkApp (ctor, Array.of_list (List.map fst evs)))
          in
          (* a binder of the proof itself left undetermined (an unconstrained
             witness) is not a proof; the goal's own variables may stay open
             -- the caller instantiates or rejects them *)
          if
            List.for_all
              (fun (e, a) ->
                (not (is_prop env sigma a))
                || closed sigma (Reductionops.nf_evar sigma e))
              evs
          then Some (sigma, proof)
          else None)
        states
    in
    proofs, complete && List.length proofs = List.length states
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
  match search ~all:false env sigma !max_depth goal with
  | (sigma, p) :: _, _ ->
    let p = Reductionops.nf_evar sigma p in
    (* belt and braces: a closed term that typechecks against the goal *)
    if not (closed sigma p)
    then Unknown
    else (
      match Typing.check env sigma p goal with
      | _ -> Proved (Term p)
      | exception _ -> Unknown)
  | [], true -> Refuted
  | [], false -> Unknown
;;

(** [enumerate env sigma goal]: for a premise that may still mention open
    variables, every way to make it hold -- each an evar map instantiating
    them -- and whether that is all of them (backlog I2, stage 2). *)
let enumerate (env : Environ.env) (sigma : Evd.evar_map) (goal : EConstr.t)
  : Evd.evar_map list * bool
  =
  let goal = Reductionops.nf_evar sigma goal in
  match negated env sigma goal with
  | Some p when closed sigma p ->
    (match search_closed env sigma p with
     | Proved _ -> [], true
     | Refuted -> [ sigma ], true
     | Unknown -> [], false)
  | Some _ -> [], false
  | None ->
    let sols, complete = search ~all:true env sigma !max_depth goal in
    List.map fst sols, complete
;;

(** [abstract_vars env sigma t]: [t] with every local variable it mentions
    (a hypothesis of the proof, such as an inverted step's label [n] or
    target [q']) replaced by a fresh evar of the same type. The search treats
    a variable as opaque, so it decides nothing about
    [step p (Some (Out n)) q']; over evars it asks whether {e any} [n] and
    [q'] would do. *)
let abstract_vars (env : Environ.env) (sigma : Evd.evar_map) (t : EConstr.t)
  : Evd.evar_map * EConstr.t
  =
  let sigma, subs =
    Names.Id.Set.fold
      (fun id (sigma, subs) ->
        match EConstr.lookup_named id env with
        | decl ->
          let sigma, e =
            Evarutil.new_evar
              env
              sigma
              (Context.Named.Declaration.get_type decl)
          in
          sigma, (id, e) :: subs
        | exception Not_found -> sigma, subs)
      (Termops.global_vars_set env sigma t)
      (sigma, [])
  in
  sigma, EConstr.Vars.replace_vars sigma subs t
;;

(* [dead]'s verdicts, keyed by the proposition with its local variables
   numbered in order of occurrence -- so [step (var 18) (Some (Out n)) q']
   and the same with [n0] and [q'1] share an entry -- and by the depth. *)
module ConstrTbl = Hashtbl.Make (struct
    type t = Constr.t

    let equal = Constr.equal
    let hash = Constr.hash
  end)

let dead_memo : (int * bool) ConstrTbl.t = ConstrTbl.create 64

(** [dead env sigma t]: the proposition [t] has no instance for any value of
    the local variables it mentions -- the search over [abstract_vars] found
    no solution and was complete. A hypothesis of this type is false in every
    context, whatever else is known about its variables. Not for negations,
    whose search would need a proof of the negated proposition. Memoised. *)
let dead (env : Environ.env) (sigma : Evd.evar_map) (t : EConstr.t) : bool =
  let decide () =
    is_prop env sigma t
    && Option.is_empty (negated env sigma t)
    &&
    let sigma, t = abstract_vars env sigma t in
    (* one solution is enough to be alive, so no need to enumerate them all;
       when there is none, completeness is meaningful either way *)
    match search ~all:false env sigma !max_depth t with
    | [], true -> true
    | _ -> false
  in
  let rec occurring acc x =
    match EConstr.kind sigma x with
    | Var id -> if List.exists (Names.Id.equal id) acc then acc else id :: acc
    | _ -> EConstr.fold sigma occurring acc x
  in
  match
    EConstr.Vars.subst_vars sigma (List.rev (occurring [] t)) t
    |> EConstr.to_constr_opt sigma
  with
  | None -> decide ()
  | Some c ->
    (match ConstrTbl.find_opt dead_memo c with
     | Some (d, r) when Int.equal d !max_depth -> r
     | _ ->
       let r = decide () in
       ConstrTbl.replace dead_memo c (!max_depth, r);
       r)
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

(** A bounded universal over [nat]: [forall k, k <= n -> P k] ([strict]
    false) or [forall k, k < n -> P k] ([strict] true; [k < n] is
    [S k <= n], and [n > k] the same), its bound [n] a numeral once
    normalized. [pred] is [fun k => P k]; [le] is Peano's [le]. *)
type bounded =
  { pred : EConstr.t
  ; strict : bool
  ; bound : int
  ; le : Names.inductive * EConstr.EInstance.t
  }

(** The largest bound unfolded: each value of [k] is a proof search, so a
    larger one is left undecided (and warned about as such). *)
let max_bounded : int = 1024

let nat_ind () : Names.inductive option =
  match Rocqlib.lib_ref "num.nat.type" with
  | Names.GlobRef.IndRef ind -> Some ind
  | _ | (exception _) -> None
;;

(** [numeral i]: [S (... (S O))]. *)
let numeral (ind : Names.inductive) (i : int) : EConstr.t =
  let o = EConstr.mkConstructU ((ind, 1), EConstr.EInstance.empty) in
  let s = EConstr.mkConstructU ((ind, 2), EConstr.EInstance.empty) in
  let rec f acc i =
    if i <= 0 then acc else f (EConstr.mkApp (s, [| acc |])) (i - 1)
  in
  f o i
;;

(** [to_int sigma x]: the number [x] spells, if it is a numeral. *)
let to_int (ind : Names.inductive) (sigma : Evd.evar_map) (x : EConstr.t)
  : int option
  =
  let rec f acc x =
    if acc > max_bounded
    then None
    else (
      let h, args = EConstr.decompose_app sigma x in
      match EConstr.kind sigma h, args with
      | Construct ((i, 1), _), [||] when Names.Ind.CanOrd.equal i ind ->
        Some acc
      | Construct ((i, 2), _), [| y |] when Names.Ind.CanOrd.equal i ind ->
        f (acc + 1) y
      | _ -> None)
  in
  f 0 x
;;

let bounded_universal
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (goal : EConstr.t)
  : bounded option
  =
  match nat_ind () with
  | None -> None
  | Some nat ->
    (match EConstr.kind sigma (Reductionops.whd_all env sigma goal) with
     | Prod (na, t, b) ->
       let is_nat =
         match EConstr.kind sigma (Reductionops.whd_all env sigma t) with
         | Ind (i, _) -> Names.Ind.CanOrd.equal i nat
         | _ -> false
       in
       let env' =
         EConstr.push_rel (Context.Rel.Declaration.LocalAssum (na, t)) env
       in
       (match EConstr.kind sigma (Reductionops.whd_all env' sigma b) with
        | Prod (_, d, p) when is_nat && EConstr.Vars.noccurn sigma 1 p ->
          let h, args =
            EConstr.decompose_app sigma (Reductionops.whd_all env' sigma d)
          in
          (match EConstr.kind sigma h with
           | Ind (le, u)
             when Rocqlib.check_ind_ref "num.nat.le" le
                  && Array.length args = 2
                  && EConstr.Vars.noccurn sigma 1 args.(1) ->
             let x = Reductionops.whd_all env' sigma args.(0) in
             let strict =
               if EConstr.isRelN sigma 1 x
               then Some false
               else (
                 let hx, ax = EConstr.decompose_app sigma x in
                 match EConstr.kind sigma hx, ax with
                 | Construct ((i, 2), _), [| y |]
                   when Names.Ind.CanOrd.equal i nat
                        && EConstr.isRelN
                             sigma
                             1
                             (Reductionops.whd_all env' sigma y) ->
                   Some true
                 | _ -> None)
             in
             let n = EConstr.Vars.lift (-1) args.(1) in
             (match
                strict, to_int nat sigma (Reductionops.nf_all env sigma n)
              with
              | Some strict, Some bound ->
                Some
                  { pred = EConstr.mkLambda (na, t, EConstr.Vars.lift (-1) p)
                  ; strict
                  ; bound
                  ; le = le, u
                  }
              | _ -> None)
           | _ -> None)
        | _ -> None)
     | _ -> None)
;;

let is_bounded_universal env sigma t : bool =
  Stdlib.Option.is_some (bounded_universal env sigma t)
;;

(** The values [k] ranges over: [0 .. bound - 1] or [0 .. bound]. *)
let bounded_range (b : bounded) : int list =
  List.init (if b.strict then b.bound else b.bound + 1) Fun.id
;;

(** [P i], beta-reduced. *)
let bounded_instance (sigma : Evd.evar_map) (b : bounded) (i : int) : EConstr.t =
  let nat = match nat_ind () with Some n -> n | None -> assert false in
  Reductionops.beta_applist sigma (b.pred, [ numeral nat i ])
;;

(** [prove env sigma goal]: a closed premise decided. A bounded universal
    ({!bounded_universal}) is decided value by value: it holds if every
    instance does ([Proved ByCases], the proof built by {!premise_tac} when
    asked for), and is refuted by any one refuted instance. *)
let rec prove (env : Environ.env) (sigma : Evd.evar_map) (goal : EConstr.t)
  : result
  =
  let goal = Reductionops.nf_evar sigma goal in
  if not (closed sigma goal)
  then Unknown
  else (
    let decided =
      match negated env sigma goal with
      | Some p ->
        (* [~ P] holds iff [P] is refuted, by a complete search *)
        (match decide_closed env sigma p with
         | Proved _ -> Refuted
         | Refuted -> Proved (ByRefutation p)
         | Unknown -> Unknown)
      | None -> decide_closed env sigma goal
    in
    match decided with Unknown -> by_user_tactic env sigma goal | r -> r)

and decide_closed (env : Environ.env) (sigma : Evd.evar_map) (goal : EConstr.t)
  : result
  =
  match bounded_universal env sigma goal with
  | None -> search_closed env sigma goal
  | Some b ->
    let rec each (all_proved : bool) = function
      | [] -> if all_proved then Proved ByCases else Unknown
      | i :: tl ->
        (match prove env sigma (bounded_instance sigma b i) with
         | Refuted -> Refuted
         | Proved _ -> each all_proved tl
         | Unknown -> each false tl)
    in
    each true (bounded_range b)
;;

(** [bounded_counterexample env sigma ty]: for a bounded universal [ty]
    refuted at [i], the bound and [i]. *)
let bounded_counterexample
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (ty : EConstr.t)
  : (bounded * int) option
  =
  match bounded_universal env sigma ty with
  | None -> None
  | Some b ->
    List.find_opt
      (fun i ->
        match prove env sigma (bounded_instance sigma b i) with
        | Refuted -> true
        | _ -> false)
      (bounded_range b)
    |> Stdlib.Option.map (fun i -> b, i)
;;

(** [bounded_le_proof b i]: a proof of [i <= bound] ([S i <= bound] if
    [strict]), as [le_S] applied to [le_n]. *)
let bounded_le_proof (b : bounded) (i : int) : EConstr.t =
  let nat = match nat_ind () with Some n -> n | None -> assert false in
  let le, u = b.le in
  let le_n = EConstr.mkConstructU ((le, 1), u) in
  let le_S = EConstr.mkConstructU ((le, 2), u) in
  let lo = if b.strict then i + 1 else i in
  let x = numeral nat lo in
  let rec f p m =
    if m >= b.bound
    then p
    else f (EConstr.mkApp (le_S, [| x; numeral nat m; p |])) (m + 1)
  in
  f (EConstr.mkApp (le_n, [| x |])) lo
;;

(** [refute_hyp_tac id]: close the goal from hypothesis [id], which cannot
    hold: a closed premise that [prove] refutes, or an open one that is
    {!dead}. An inductive one is unfolded ([simpl in], so a fixpoint like
    [In] becomes [or]/[eq]/[False]) and cleared by [inversion_clear], and
    every goal that leaves is refuted the same way ({!refute_goal}); a
    negation [~ P] is applied to the proof of [P]. [inversion_clear], not
    [inversion]: a kept hypothesis is picked again forever (the Step 0
    loop). *)
let rec refute_hyp_tac ?(depth : int = !max_depth) (id : Names.Id.t)
  : unit Proofview.tactic
  =
  refute_hyp_from ~depth ~before:None id

(* [before]: the hypotheses (name and type) in scope when the outermost
   refutation started, so that {!refute_goal} can tell those it introduced.
   Not names alone: [inversion_clear] frees a name and Rocq hands it to the
   next premise it introduces, which then looked old and was never checked
   (measured on the CCS ABP: a quarter of the refutations failed so). *)
and refute_hyp_from
      ~(depth : int)
      ~(before : (Names.Id.t * EConstr.t) list option)
      (id : Names.Id.t)
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
      when depth > 0
           && Stdlib.Option.is_some (bounded_counterexample env sigma ty) ->
      (* a bounded universal with a false instance [P i]: instantiate it,
         [id i (_ : i <= n)], and refute that *)
      let b, i =
        match bounded_counterexample env sigma ty with
        | Some x -> x
        | None -> assert false
      in
      let nat = match nat_ind () with Some n -> n | None -> assert false in
      let inst =
        EConstr.mkApp
          (EConstr.mkVar id, [| numeral nat i; bounded_le_proof b i |])
      in
      Generalize.generalize [ inst ]
      <*> Tactics.intro_using_then (Names.Id.of_string "H_instance") (fun id' ->
        refute_hyp_from ~depth:(depth - 1) ~before id')
    | None
      when Bool.not (dead env sigma ty)
           &&
           match search_closed env sigma ty with
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
      else (
        let before =
          match before with
          | Some b -> b
          | None ->
            List.map
              (fun d ->
                ( Context.Named.Declaration.get_id d
                , Context.Named.Declaration.get_type d ))
              (Proofview.Goal.hyps gl)
        in
        Tactics.simpl_in_hyp (id, Locus.InHyp)
        <*> Inv.inv_clear_tac id
        <*> refute_goal ~depth:(depth - 1) ~before))

(** [refute_goal ~depth ~before]: close the goal in focus, which an
    inversion of a hypothesis that cannot hold left behind. In order: a
    {!dead} hypothesis the refutation introduced (memoised, and where the
    falsity usually is); a closed one [prove] refutes, anywhere in the
    context (the only case before 2026-10-02's dead-hypothesis refutation,
    and dear: [prove] is not memoised and runs on every hypothesis); else,
    when the premises are only false {e together} -- a handshake's
    [p -!n-> p'] and [q -?n-> q'], each possible for some [n], never for
    the same one -- [inversion_clear] the newest one it introduced, which
    fixes the shared variable in each branch, and go on. *)
and refute_goal ~(depth : int) ~(before : (Names.Id.t * EConstr.t) list)
  : unit Proofview.tactic
  =
  let open Proofview.Notations in
  Proofview.Goal.enter (fun gl ->
    let env = Proofview.Goal.env gl in
    let sigma = Proofview.Goal.sigma gl in
    let hyps = Proofview.Goal.hyps gl in
    let id_of = Context.Named.Declaration.get_id in
    let ty_of = Context.Named.Declaration.get_type in
    (* newest first, as [Proofview.Goal.hyps] lists them *)
    let introduced =
      List.filter
        (fun d ->
          Bool.not
            (List.exists
               (fun (id, t) ->
                 Names.Id.equal id (id_of d)
                 && EConstr.eq_constr sigma t (ty_of d))
               before))
        hyps
    in
    let refute (d : EConstr.named_declaration) =
      refute_hyp_from ~depth ~before:(Some before) (id_of d)
    in
    match List.find_opt (fun d -> dead env sigma (ty_of d)) introduced with
    | Some d -> refute d
    | None ->
      let refutable =
        List.find_opt
          (fun d ->
            let t = ty_of d in
            is_prop env sigma t
            && closed sigma t
            && match prove env sigma t with Refuted -> true | _ -> false)
          hyps
      in
      (match refutable with
       | Some d -> refute d
       | None ->
         let splittable (d : EConstr.named_declaration) : bool =
           let t = ty_of d in
           is_prop env sigma t
           && Option.is_empty (negated env sigma t)
           &&
           match
             EConstr.kind
               sigma
               (fst
                  (EConstr.decompose_app
                     sigma
                     (Reductionops.whd_all env sigma t)))
           with
           | Ind (ind, _) -> Bool.not (is_eq_ind ind)
           | _ -> false
         in
         (match List.find_opt splittable introduced with
          | Some d when depth > 0 ->
            Tactics.simpl_in_hyp (id_of d, Locus.InHyp)
            <*> Inv.inv_clear_tac (id_of d)
            <*> refute_goal ~depth:(depth - 1) ~before
          | _ -> Tacticals.tclZEROMSG (Pp.str "MeBi: no refutable premise left"))))
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

(** A [MEBI.Premises] lemma (see [theories/Premises.v]). *)
let premises_lemma (name : string) : EConstr.t option =
  let path =
    Names.DirPath.make (List.rev_map Names.Id.of_string [ "MEBI"; "Premises" ])
  in
  match
    Nametab.global_of_path (Libnames.make_path path (Names.Id.of_string name))
  with
  | gr ->
    Some
      (EConstr.of_constr
         (UnivGen.constr_of_monomorphic_global (Global.env ()) gr))
  | exception Not_found -> None
;;

(** [premise_tac]: prove the closed premise in focus as {!prove} decided
    it. *)
let rec premise_tac () : unit Proofview.tactic =
  Proofview.Goal.enter (fun gl ->
    let env = Proofview.Goal.env gl in
    let sigma = Proofview.Goal.sigma gl in
    match prove env sigma (Proofview.Goal.concl gl) with
    | Proved (Term p) -> Tactics.exact_check p
    | Proved (ByRefutation _) -> negation_tac
    | Proved ByCases -> bounded_tac ()
    | Refuted | Unknown ->
      Tacticals.tclZEROMSG (Pp.str "MeBi: cannot prove the premise"))

(** [bounded_tac]: prove a bounded universal [forall k, k < n -> P k] (or
    [<=]): each instance [P i] proved on its own by {!premise_tac}, and the
    universal from those by [MEBI.Premises]' lemmas, one per value of [k]:
    [bounded_lt_S P m (bounded_le_S P (m-1) (... (bounded_le_0 P p0) ...) pm)].
*)
and bounded_tac () : unit Proofview.tactic =
  Proofview.Goal.enter (fun gl ->
    let env = Proofview.Goal.env gl in
    let sigma = Proofview.Goal.sigma gl in
    let fail msg = Tacticals.tclZEROMSG (Pp.str ("MeBi: " ^ msg)) in
    match bounded_universal env sigma (Proofview.Goal.concl gl) with
    | None -> fail "not a bounded universal"
    | Some b ->
      let nat = match nat_ind () with Some n -> n | None -> assert false in
      let instance (i : int) : EConstr.t option =
        let typ = bounded_instance sigma b i in
        match
          Subproof.build_by_tactic_opt
            env
            ~uctx:(Evd.ustate sigma)
            ~poly:PolyFlags.default
            ~typ
            (premise_tac ())
        with
        | Some (c, _, _, _, _) -> Some (EConstr.of_constr c)
        | None -> None
        | exception e when CErrors.noncritical e -> None
      in
      let lemmas =
        List.map
          premises_lemma
          [ "bounded_le_0"; "bounded_le_S"; "bounded_lt_0"; "bounded_lt_S" ]
      in
      (match lemmas with
       | [ Some le_0; Some le_S; Some lt_0; Some lt_S ] ->
         (* [forall k, k <= m -> P k], from the proofs of [P 0 .. P m] *)
         let rec le_chain (m : int) : EConstr.t option =
           match instance m with
           | None -> None
           | Some pm when m = 0 -> Some (EConstr.mkApp (le_0, [| b.pred; pm |]))
           | Some pm ->
             Stdlib.Option.map
               (fun rest ->
                 EConstr.mkApp
                   (le_S, [| b.pred; numeral nat (m - 1); rest; pm |]))
               (le_chain (m - 1))
         in
         let proof =
           if not b.strict
           then le_chain b.bound
           else if b.bound = 0
           then Some (EConstr.mkApp (lt_0, [| b.pred |]))
           else
             Stdlib.Option.map
               (fun rest ->
                 EConstr.mkApp
                   (lt_S, [| b.pred; numeral nat (b.bound - 1); rest |]))
               (le_chain (b.bound - 1))
         in
         (match proof with
          | Some p -> Tactics.exact_check p
          | None -> fail "cannot prove an instance of a bounded universal")
       | _ -> fail "bounded universals need [MEBI.Premises] loaded"))
;;
