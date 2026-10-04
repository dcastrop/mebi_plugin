(** Bounded proof search for constructor premises that are not over an LTS
    (backlog item I2, stage 1 of [notes/9-general-premise-support.md]).

    Shared by extraction (which only needs the verdict) and the proof solver
    (which [exact]s the proof found). Three outcomes, never a guess:
    [Proved p] with a proof term [p]; [Refuted] only when the search was
    {e complete}; [Unknown] otherwise. *)

(** How a premise was proved (see the [.mli]). A negation [~ P] has no
    proof term from constructor search: it holds because [P] was refuted,
    and the solver proves it with {!negation_tac}. *)
type proof =
  | Term of EConstr.t
  | ByRefutation of EConstr.t
  | ByCases

(* See the [.mli]. *)
type result =
  | Proved of proof
  | Refuted
  | Unknown

(* See the [.mli]. *)
let user_tactic : unit Proofview.tactic option ref = ref None

(* See the [.mli]. *)
let default_depth : int = 16

(* See the [.mli]. *)
let max_depth : int ref = ref default_depth

(* See the [.mli]. *)
let is_prop (env : Environ.env) (sigma : Evd.evar_map) (t : EConstr.t) : bool =
  try
    match Retyping.get_sort_quality_of env sigma t with
    | UnivGen.QualityOrSet.Qual q -> Sorts.Quality.is_qprop q
    | UnivGen.QualityOrSet.Set -> false
  with
  | _ -> false
;;

(** [is_eq_ind ind] is whether [ind] is Rocq's [eq]. Raises nothing. *)
let is_eq_ind (ind : Names.inductive) : bool =
  Rocqlib.check_ind_ref "core.eq.type" ind
;;

(** [closed sigma x] is whether [x] has no evars. Raises nothing. *)
let closed (sigma : Evd.evar_map) (x : EConstr.t) : bool =
  Evar.Set.is_empty (Evd.evars_of_term sigma x)
;;

(** [ground env sigma x] is whether [x], normalised, is built only from
    constructors (type arguments exempt): the only kind of argument on
    which a failed match {e proves} a premise false. [le (f x) 3] with an
    opaque [f] is not ground, so no constructor matching it is not a
    refutation. Raises nothing. *)
let rec ground (env : Environ.env) (sigma : Evd.evar_map) (x : EConstr.t) : bool
  =
  let h, args = EConstr.decompose_app sigma x in
  match EConstr.kind sigma h with
  | Construct _ -> Array.for_all (arg_ground env sigma) args
  | _ -> false

(** [arg_ground env sigma x] is whether the argument [x] is a type or
    {!ground}. Raises nothing. *)
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

(** [evaluated env sigma x] is whether [x], fully normalised, has nothing
    opaque left: no opaque constant or axiom, free variable, or stuck
    [match]/fixpoint, only constructors, inductives, sorts, binders and
    evars. Conversion is complete on such terms, so a parameter like
    [fun k => k <= 1] is fine, while [f x] with an opaque [f] is not.
    Raises nothing. *)
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

(** Raised by {!unify} and {!unify_conclusion}: the terms do not unify. *)
exception NoUnify

(** [unify env sigma a b] is [sigma] with [a] and [b] unified (up to
    conversion).

    @raise NoUnify
      if they do not unify (raised here, for Rocq's
      unification errors). *)
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

(** [unify_conclusion env sigma concl goal] is [sigma] with the
    constructor conclusion [concl] unified with [goal]: whole, and failing
    that argument by argument, left to right, each of [concl]'s arguments
    normalised once the earlier ones have instantiated its evars. A
    constructor whose index is computed from its binders ([ev k (dbl k)],
    [termLTS (tfix t) None (subst (tfix t) t)]) can defeat whole
    unification though the goal holds: [k := 2] first makes [dbl k] reduce
    to [4]. A success is a genuine solution; the proof is still
    type-checked ({!search_closed}).

    @raise NoUnify if neither way unifies them (propagated from {!unify}). *)
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

(** [pattern env sigma x] is whether [x] is an index a match can fail on
    decidably: built from constructors with evars allowed at the leaves (an
    open pattern, such as a target still to be computed: first-order
    unification against it is complete). Raises nothing. *)
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

(** [arg_pattern env sigma x] is whether the argument [x] is a type or,
    normalised, a {!pattern}. Raises nothing. *)
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

(** [search ~all env sigma depth goal] is the solutions of [goal] -- each
    an evar map in which [goal]'s open variables may be instantiated, with a
    proof of the instantiated [goal] -- and whether they are {e all} of them
    (no depth cut, no undecidable leaf, no failed match on a non-pattern
    argument, cap not reached). With [~all:false] it stops at the first
    solution, and completeness then only matters when there is none.

    An equation is decided by conversion when closed (a constructor
    difference refutes it) and solved by unification when open; an
    inductive proposition by trying each constructor ({!try_constructor})
    at [depth - 1]; anything else is undecidable here.

    Raises nothing (unification failures are caught). *)
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

(** [try_constructor ~all env sigma depth goal c] is the solutions of
    [goal] that start with the constructor [c], and whether they are all of
    them: fresh evars for [c]'s binders, its conclusion unified with [goal]
    ({!unify_conclusion}), then every [Prop] binder still open searched for,
    left to right, at [depth - 1], every solution of one continuing into the
    next. A failed match rules [c] out only if its conclusion's indices are
    patterns too. An open sub-premise is always fully enumerated: which of
    its solutions is taken can decide whether a later premise holds.

    Raises nothing (unification failures are caught). *)
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

(** [negated env sigma goal] is [P] if [goal] is [P -> False] after head
    reduction (so [~ P] too), else [None]. Raises nothing. *)
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

(** [search_closed env sigma goal] is the verdict of a {!search} for one
    proof of the closed [goal]: [Proved] with its proof term (checked to be
    closed and to type-check), [Refuted] if the search found none and was
    complete, else [Unknown]. Raises nothing. *)
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

(* See the [.mli]. *)
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

(* See the [.mli]. *)
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

(** {!dead}'s verdicts, keyed by the proposition with its local variables
    numbered in order of occurrence -- so [step (var 18) (Some (Out n)) q']
    and the same with [n0] and [q'1] share an entry -- and by the depth. *)
let dead_memo : (int * bool) ConstrTbl.t = ConstrTbl.create 64

(* See the [.mli]. *)
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

(** [by_tactic env sigma typ] is the closed proof term of [typ] the
    {!user_tactic} builds, or [None] if there is none or it fails. Raises
    nothing. *)
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

(** [negation_of goal] is [goal -> False]. Raises nothing. *)
let negation_of (goal : EConstr.t) : EConstr.t =
  let false_ =
    EConstr.of_constr
      (UnivGen.constr_of_monomorphic_global
         (Global.env ())
         (Rocqlib.lib_ref "core.False.type"))
  in
  EConstr.mkArrowR goal false_
;;

(** [by_user_tactic env sigma goal] is the {!user_tactic}'s verdict on
    [goal]: [Proved] if it proves [goal], [Refuted] if it proves [~ goal],
    else [Unknown]. Raises nothing. *)
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

(** A bounded universal premise, as {!bounded_universal} recognises it:
    [forall k : nat, k <= n -> P k], or [forall k : nat, k < n -> P k]
    ([k < n] is [S k <= n] by definition, and [n > k] is [k < n]):
    - [pred]: [fun k => P k];
    - [strict]: [true] for [<], [false] for [<=];
    - [bound]: [n], read as a number;
    - [le]: Peano's [le] with its universe instance, whose constructors
      [le_n] and [le_S] build the proofs of [i <= n] that instantiating
      the premise needs ({!bounded_le_proof}). *)
type bounded =
  { pred : EConstr.t
  ; strict : bool
  ; bound : int
  ; le : Names.inductive * EConstr.EInstance.t
  }

(* See the [.mli]. Measured 2026-10-04: a cheap instance ([k = k]) costs
   little even at 1000 values (6s, 0.4GB for a whole proof), but one
   refuted by inversion over unary numerals ([k <> 5000]) took 12s / 0.6GB
   at 100 values and 190s / 3.9GB at 1000. *)
let default_range : int = 256

(* See the [.mli]. Deciding a premise costs a search per value, and its
   proof term grows with the square of the range (each step of the chain
   carries a unary numeral), so a wider premise is left undecided, with its
   own warning. *)
let max_range : int ref = ref default_range

(** [nat_ind ()] is Peano's [nat], as registered with Rocq
    ([num.nat.type]), or [None] if it is not loaded. Raises nothing. *)
let nat_ind () : Names.inductive option =
  match Rocqlib.lib_ref "num.nat.type" with
  | Names.GlobRef.IndRef ind -> Some ind
  | _ | (exception _) -> None
;;

(** [the_nat_ind ()] is {!nat_ind}, for code that only runs once a bounded
    universal has been recognised, which needed [nat].

    @raise Failure
      if [nat] is not loaded (raised here; cannot happen after
      a bounded universal was recognised). *)
let the_nat_ind () : Names.inductive =
  match nat_ind () with
  | Some n -> n
  | None -> failwith "MeBi: [nat] is not loaded"
;;

(** [numeral nat i] is the unary numeral for [i] in [nat],
    [S (... (S O))] with [i] [S]s. Raises nothing. *)
let numeral (nat : Names.inductive) (i : int) : EConstr.t =
  let o = EConstr.mkConstructU ((nat, 1), EConstr.EInstance.empty) in
  let s = EConstr.mkConstructU ((nat, 2), EConstr.EInstance.empty) in
  let rec wrap acc i =
    if i <= 0 then acc else wrap (EConstr.mkApp (s, [| acc |])) (i - 1)
  in
  wrap o i
;;

(** [is_constructor nat j sigma h] is whether [h] is the [j]-th
    constructor of [nat] ([1] is [O], [2] is [S]). Raises nothing. *)
let is_constructor
      (nat : Names.inductive)
      (j : int)
      (sigma : Evd.evar_map)
      (h : EConstr.t)
  : bool
  =
  match EConstr.kind sigma h with
  | Construct ((i, j'), _) -> Names.Ind.CanOrd.equal i nat && Int.equal j j'
  | _ -> false
;;

(** What a bound [n] reads as: a number, a number above the cap given to
    {!read_bound} (not read any further), or not a number at all. *)
type reading =
  | Number of int
  | Above_cap
  | Not_a_number

(** [read_bound env sigma nat cap x] is what the [nat] term [x] evaluates
    to: a number, [Above_cap] past [cap], or [Not_a_number] if it does not
    reduce to a numeral (it is open, or stuck on something opaque). [x] is
    head-reduced one [S] at a time, so a bound like [2 ^ 30] is given up on
    past [cap] rather than computed in full. Raises nothing. *)
let read_bound
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (nat : Names.inductive)
      (cap : int)
      (x : EConstr.t)
  : reading
  =
  let rec count acc x =
    if acc > cap
    then Above_cap
    else (
      let h, args =
        EConstr.decompose_app sigma (Reductionops.whd_all env sigma x)
      in
      match args with
      | [||] when is_constructor nat 1 sigma h -> Number acc
      | [| y |] when is_constructor nat 2 sigma h -> count (acc + 1) y
      | _ -> Not_a_number)
  in
  count 0 x
;;

(** [split_forall_implies env sigma goal] is, for [goal] of the shape
    [forall (k : T), D k -> B k] (after head reduction) where [B] does not
    depend on the proof of [D k], the binder's name and type, the
    environment under [k], and [D] and [B], both still under [k] (it is
    [Rel 1] in them), [B] with the proof's binder removed; [None] for any
    other shape. Raises nothing. *)
let split_forall_implies
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (goal : EConstr.t)
  : (Names.Name.t EConstr.binder_annot
    * EConstr.types
    * Environ.env
    * EConstr.t
    * EConstr.t)
      option
  =
  match EConstr.kind sigma (Reductionops.whd_all env sigma goal) with
  | Prod (na, t, b) ->
    let env' =
      EConstr.push_rel (Context.Rel.Declaration.LocalAssum (na, t)) env
    in
    (match EConstr.kind sigma (Reductionops.whd_all env' sigma b) with
     | Prod (_, d, body) when EConstr.Vars.noccurn sigma 1 body ->
       Some (na, t, env', d, EConstr.Vars.lift (-1) body)
     | _ -> None)
  | _ -> None
;;

(** [is_nat env sigma nat t] is whether the type [t] reduces to [nat].
    Raises nothing. *)
let is_nat
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (nat : Names.inductive)
      (t : EConstr.types)
  : bool
  =
  match EConstr.kind sigma (Reductionops.whd_all env sigma t) with
  | Ind (i, _) -> Names.Ind.CanOrd.equal i nat
  | _ -> false
;;

(** [as_le env' sigma d] is, for a hypothesis type [d] under the binder
    [k] (in [env']) that reduces to Peano's [le x n] with [n] not
    mentioning [k], [le] with its universe instance, [x] (still under [k])
    and [n] (lowered out from under [k]); [None] otherwise. Raises
    nothing. *)
let as_le (env' : Environ.env) (sigma : Evd.evar_map) (d : EConstr.t)
  : ((Names.inductive * EConstr.EInstance.t) * EConstr.t * EConstr.t) option
  =
  let h, args =
    EConstr.decompose_app sigma (Reductionops.whd_all env' sigma d)
  in
  match EConstr.kind sigma h with
  | Ind (le, u)
    when Rocqlib.check_ind_ref "num.nat.le" le
         && Array.length args = 2
         && EConstr.Vars.noccurn sigma 1 args.(1) ->
    Some ((le, u), args.(0), EConstr.Vars.lift (-1) args.(1))
  | _ -> None
;;

(** [strictness env' sigma nat x] is, for the left side [x] of [x <= n]
    under the binder [k] (in [env']), [Some false] if [x] is [k] itself
    ([k <= n]), [Some true] if it is [S k] ([k < n]), and [None] for
    anything else. Raises nothing. *)
let strictness
      (env' : Environ.env)
      (sigma : Evd.evar_map)
      (nat : Names.inductive)
      (x : EConstr.t)
  : bool option
  =
  let is_k (y : EConstr.t) : bool =
    EConstr.isRelN sigma 1 (Reductionops.whd_all env' sigma y)
  in
  if is_k x
  then Some false
  else (
    let h, args =
      EConstr.decompose_app sigma (Reductionops.whd_all env' sigma x)
    in
    match args with
    | [| y |] when is_constructor nat 2 sigma h && is_k y -> Some true
    | _ -> None)
;;

(** What {!recognise} makes of a premise. *)
type recognised =
  | Bounded of bounded (** a bounded universal within {!max_range} *)
  | Above_range
  (** a bounded universal ranging over more than {!max_range} values *)
  | Not_bounded (** anything else, including a bound that is not a number *)

(** [recognise env sigma goal] is whether [goal] is a bounded universal
    over [nat] ([forall k, k < n -> P k] or [forall k, k <= n -> P k]),
    and if so whether its [n] (or [n + 1]) values of [k] are within
    {!max_range}: {!split_forall_implies}, {!is_nat}, {!as_le},
    {!strictness}, then {!read_bound}. Raises nothing. *)
let recognise (env : Environ.env) (sigma : Evd.evar_map) (goal : EConstr.t)
  : recognised
  =
  let ( let* ) = Stdlib.Option.bind in
  let shape =
    let* nat = nat_ind () in
    let* na, t, env', d, body = split_forall_implies env sigma goal in
    let* () = if is_nat env sigma nat t then Some () else None in
    let* le, x, n = as_le env' sigma d in
    let* strict = strictness env' sigma nat x in
    Some (nat, EConstr.mkLambda (na, t, body), strict, le, n)
  in
  match shape with
  | None -> Not_bounded
  | Some (nat, pred, strict, le, n) ->
    (* [<] ranges over [n] values, [<=] over [n + 1] *)
    let cap = if strict then !max_range else !max_range - 1 in
    (match read_bound env sigma nat cap n with
     | Number bound -> Bounded { pred; strict; bound; le }
     | Above_cap -> Above_range
     | Not_a_number -> Not_bounded)
;;

(** [bounded_universal env sigma goal] is [goal] as a {!type-bounded}, if
    it is a bounded universal within {!max_range} ({!recognise}); [None]
    otherwise, when it is decided as before (left undecided). Raises
    nothing. *)
let bounded_universal
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (goal : EConstr.t)
  : bounded option
  =
  match recognise env sigma goal with
  | Bounded b -> Some b
  | Above_range | Not_bounded -> None
;;

(* See the [.mli]. *)
let above_range (env : Environ.env) (sigma : Evd.evar_map) (t : EConstr.t)
  : bool
  =
  match recognise env sigma t with
  | Above_range -> true
  | Bounded _ | Not_bounded -> false
;;

(* See the [.mli]. *)
let is_bounded_universal
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (t : EConstr.t)
  : bool
  =
  Stdlib.Option.is_some (bounded_universal env sigma t)
;;

(** [bounded_range b] is the values [k] ranges over, in order:
    [0 .. n - 1] for [<], [0 .. n] for [<=]. Raises nothing. *)
let bounded_range (b : bounded) : int list =
  List.init (if b.strict then b.bound else b.bound + 1) Fun.id
;;

(** [bounded_instance sigma b i] is the instance [P i] of [b]'s body,
    beta-reduced. Raises nothing. *)
let bounded_instance (sigma : Evd.evar_map) (b : bounded) (i : int) : EConstr.t =
  Reductionops.beta_applist sigma (b.pred, [ numeral (the_nat_ind ()) i ])
;;

(* See the [.mli]. *)
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

(** [decide_closed env sigma goal] is the verdict on the closed,
    non-negated premise [goal]: a bounded universal by {!decide_bounded},
    anything else by the constructor search ({!search_closed}). Raises
    nothing. *)
and decide_closed (env : Environ.env) (sigma : Evd.evar_map) (goal : EConstr.t)
  : result
  =
  match bounded_universal env sigma goal with
  | Some b -> decide_bounded env sigma b
  | None -> search_closed env sigma goal

(** [decide_bounded env sigma b] is the verdict on the bounded universal
    [b], each instance [P i] decided by {!prove} in order: [Refuted] at the
    first refuted instance (one is enough, so the rest are not tried);
    [Proved ByCases] if all are proved (the proof is built later, by
    {!premise_tac}, only if a proof asks for it); otherwise [Unknown].
    Raises nothing. *)
and decide_bounded (env : Environ.env) (sigma : Evd.evar_map) (b : bounded)
  : result
  =
  let rec each (all_proved : bool) = function
    | [] -> if all_proved then Proved ByCases else Unknown
    | i :: rest ->
      (match prove env sigma (bounded_instance sigma b i) with
       | Refuted -> Refuted
       | Proved _ -> each all_proved rest
       | Unknown -> each false rest)
  in
  each true (bounded_range b)
;;

(** [bounded_counterexample env sigma ty] is, if [ty] is a bounded
    universal with a refuted instance, the {!type-bounded} and the first
    such [i]; [None] otherwise. Raises nothing. *)
let bounded_counterexample
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (ty : EConstr.t)
  : (bounded * int) option
  =
  let refuted (b : bounded) (i : int) : bool =
    match prove env sigma (bounded_instance sigma b i) with
    | Refuted -> true
    | Proved _ | Unknown -> false
  in
  Stdlib.Option.bind (bounded_universal env sigma ty) (fun b ->
    List.find_opt (refuted b) (bounded_range b)
    |> Stdlib.Option.map (fun i -> b, i))
;;

(** [bounded_le_proof b i] is a closed proof of the hypothesis [b] puts on
    [k = i]: [i <= n] for [<=], [S i <= n] for [<] (with [x] that left
    side, [le_S x (n-1) (... (le_S x x (le_n x)))]). Raises nothing. *)
let bounded_le_proof (b : bounded) (i : int) : EConstr.t =
  let nat = the_nat_ind () in
  let le, u = b.le in
  let le_n = EConstr.mkConstructU ((le, 1), u) in
  let le_S = EConstr.mkConstructU ((le, 2), u) in
  let lo = if b.strict then i + 1 else i in
  let x = numeral nat lo in
  (* [p : x <= m], extended one [le_S] at a time up to [x <= n] *)
  let rec extend (p : EConstr.t) (m : int) : EConstr.t =
    if m >= b.bound
    then p
    else extend (EConstr.mkApp (le_S, [| x; numeral nat m; p |])) (m + 1)
  in
  extend (EConstr.mkApp (le_n, [| x |])) lo
;;

(** [instantiate_bounded_hyp id b i k] is a tactic that, for the
    hypothesis [id], a bounded universal [b], adds its instance
    [id i _ : P i] as a new hypothesis (by [generalize] and [intro], under
    a fresh name) and goes on with [k] on that name. Raises nothing; fails
    (as a tactic) only if [k] does. *)
let instantiate_bounded_hyp
      (id : Names.Id.t)
      (b : bounded)
      (i : int)
      (k : Names.Id.t -> unit Proofview.tactic)
  : unit Proofview.tactic
  =
  let inst =
    EConstr.mkApp
      (EConstr.mkVar id, [| numeral (the_nat_ind ()) i; bounded_le_proof b i |])
  in
  Proofview.tclTHEN
    (Generalize.generalize [ inst ])
    (Tactics.intro_using_then (Names.Id.of_string "H_instance") k)
;;

(* See the [.mli]. *)
let rec refute_hyp_tac ?(depth : int = !max_depth) (id : Names.Id.t)
  : unit Proofview.tactic
  =
  refute_hyp_from ~depth ~before:None id

(** [refute_hyp_from ~depth ~before id] is {!refute_hyp_tac}, tracking
    [before]: the hypotheses (name and type) in scope when the outermost
    refutation started, so that {!refute_goal} can tell those it
    introduced. Not names alone: [inversion_clear] frees a name and Rocq
    hands it to the next premise it introduces, which then looked old and
    was never checked (measured on the CCS ABP: a quarter of the refutations
    failed so). In order: a negation is applied to the proof of its body; a
    bounded universal is instantiated at a refuted instance and that
    refuted; a premise only the user tactic refutes uses its proof of the
    negation; anything else is unfolded and inverted, its goals refuted by
    {!refute_goal}.

    Raises nothing; fails (as a tactic) when [id] cannot be refuted within
    [depth]. *)
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
    | None ->
      (* A refutation by the user tactic, not by search: [ty] is not dead
         and the search does not refute it, so whatever [prove] refuted it
         with was the tactic; use its proof of [~ ty]. *)
      let by_user_tactic () : unit Proofview.tactic =
        match by_tactic env sigma (negation_of ty) with
        | Some np ->
          Tactics.exfalso
          <*> Tactics.exact_check (EConstr.mkApp (np, [| EConstr.mkVar id |]))
        | None -> Tacticals.tclZEROMSG (Pp.str "MeBi: cannot refute a premise")
      in
      (* Unfold [ty], invert it away, and refute every goal that leaves. *)
      let by_inversion () : unit Proofview.tactic =
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
          <*> refute_goal ~depth:(depth - 1) ~before)
      in
      (match
         if depth > 0 then bounded_counterexample env sigma ty else None
       with
       | Some (b, i) ->
         (* a bounded universal with a false instance [P i]: add [P i] as
            a hypothesis and refute that *)
         instantiate_bounded_hyp id b i (fun id' ->
           refute_hyp_from ~depth:(depth - 1) ~before id')
       | None ->
         let user_refuted =
           Bool.not (dead env sigma ty)
           &&
           match search_closed env sigma ty with
           | Refuted -> false
           | Proved _ | Unknown -> true
         in
         if user_refuted then by_user_tactic () else by_inversion ()))

(** [refute_goal ~depth ~before] is a tactic that closes the goal in
    focus, which an inversion of a hypothesis that cannot hold left behind.
    In order: a {!dead} hypothesis the refutation introduced (memoised, and
    where the falsity usually is); a closed one {!prove} refutes, anywhere
    in the context; else, when the premises are only false {e together} --
    a handshake's [p -!n-> p'] and [q -?n-> q'], each possible for some
    [n], never for the same one -- [inversion_clear] the newest one it
    introduced, which fixes the shared variable in each branch, and go on.

    Raises nothing; fails (as a tactic) when none of these applies within
    [depth]. *)
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

(* See the [.mli]. *)
let negation_tac : unit Proofview.tactic =
  (* [hnf] first: [~ P] is the constant [not], not yet a product. *)
  Proofview.tclTHEN
    Tactics.hnf_in_concl
    (Tactics.intro_using_then (Names.Id.of_string "H_premise") (fun id ->
       refute_hyp_tac id))
;;

(** [premises_lemma name] is the lemma [MEBI.Premises.name] (see
    [theories/Premises.v]) as a term, or [None] if that file is not
    loaded. Raises nothing. *)
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

(** The four lemmas a bounded universal's proof is built from, each over a
    predicate [P : nat -> Prop]:
    - [le_0]: [P 0 -> forall k, k <= 0 -> P k];
    - [le_S m]: [(forall k, k <= m -> P k) -> P (S m) -> forall k, k <= S m -> P k];
    - [lt_0]: [forall k, S k <= 0 -> P k];
    - [lt_S m]: [(forall k, k <= m -> P k) -> forall k, S k <= S m -> P k]. *)
type bounded_lemmas =
  { le_0 : EConstr.t
  ; le_S : EConstr.t
  ; lt_0 : EConstr.t
  ; lt_S : EConstr.t
  }

(** [bounded_lemmas ()] is the {!type-bounded_lemmas}, or [None] if
    [MEBI.Premises] is not loaded. Raises nothing. *)
let bounded_lemmas () : bounded_lemmas option =
  let ( let* ) = Stdlib.Option.bind in
  let* le_0 = premises_lemma "bounded_le_0" in
  let* le_S = premises_lemma "bounded_le_S" in
  let* lt_0 = premises_lemma "bounded_lt_0" in
  let* lt_S = premises_lemma "bounded_lt_S" in
  Some { le_0; le_S; lt_0; lt_S }
;;

(** [subproof env sigma typ tac] is the closed proof term [tac] builds for
    [typ], run as a proof of its own, or [None] if it fails. Raises
    nothing. *)
let subproof
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (typ : EConstr.types)
      (tac : unit Proofview.tactic)
  : EConstr.t option
  =
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
  | exception e when CErrors.noncritical e -> None
;;

(** [bounded_le_chain l b instance m] is a proof of
    [forall k, k <= m -> P k] for [b]'s [P], built from [instance i], a
    proof of [P i] for each [i <= m]: [le_S P (m-1) (... (le_0 P p0) ...) pm]; [None] if an instance has no proof. Raises nothing.
*)
let rec bounded_le_chain
          (l : bounded_lemmas)
          (b : bounded)
          (instance : int -> EConstr.t option)
          (m : int)
  : EConstr.t option
  =
  let ( let* ) = Stdlib.Option.bind in
  let* pm = instance m in
  if m = 0
  then Some (EConstr.mkApp (l.le_0, [| b.pred; pm |]))
  else
    let* rest = bounded_le_chain l b instance (m - 1) in
    Some
      (EConstr.mkApp
         (l.le_S, [| b.pred; numeral (the_nat_ind ()) (m - 1); rest; pm |]))
;;

(** [bounded_proof l b instance] is a proof of the bounded universal [b]
    from a proof of each instance ([instance i], for every [i] in
    {!bounded_range}): for [<=], {!bounded_le_chain} up to [n]; for [<],
    [lt_0] if [n = 0] (nothing to prove), else [lt_S] on the chain up to
    [n - 1]; [None] if an instance has no proof. Raises nothing. *)
let bounded_proof
      (l : bounded_lemmas)
      (b : bounded)
      (instance : int -> EConstr.t option)
  : EConstr.t option
  =
  if not b.strict
  then bounded_le_chain l b instance b.bound
  else if b.bound = 0
  then Some (EConstr.mkApp (l.lt_0, [| b.pred |]))
  else
    Stdlib.Option.map
      (fun chain ->
        EConstr.mkApp
          (l.lt_S, [| b.pred; numeral (the_nat_ind ()) (b.bound - 1); chain |]))
      (bounded_le_chain l b instance (b.bound - 1))
;;

(* See the [.mli]. *)
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

(** [bounded_tac ()] is a tactic that proves the bounded universal in
    focus: each instance [P i] as a proof of its own, by {!premise_tac},
    then the universal from those ({!bounded_proof}), checked by [exact].
    Raises nothing; fails (as a tactic) if the goal is not a bounded
    universal, [MEBI.Premises] is not loaded, or an instance has no
    proof. *)
and bounded_tac () : unit Proofview.tactic =
  Proofview.Goal.enter (fun gl ->
    let env = Proofview.Goal.env gl in
    let sigma = Proofview.Goal.sigma gl in
    let fail msg = Tacticals.tclZEROMSG (Pp.str ("MeBi: " ^ msg)) in
    match
      bounded_universal env sigma (Proofview.Goal.concl gl), bounded_lemmas ()
    with
    | None, _ -> fail "not a bounded universal"
    | _, None -> fail "bounded universals need [MEBI.Premises] loaded"
    | Some b, Some l ->
      let instance (i : int) : EConstr.t option =
        subproof env sigma (bounded_instance sigma b i) (premise_tac ())
      in
      (match bounded_proof l b instance with
       | Some p -> Tactics.exact_check p
       | None -> fail "cannot prove an instance of a bounded universal"))
;;
