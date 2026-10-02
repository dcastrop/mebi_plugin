(** Bounded proof search for constructor premises that are not over an LTS
    (backlog item I2, stage 1 of [notes/9-general-premise-support.md]).

    Shared by extraction (which only needs the verdict) and the proof solver
    (which [exact]s the proof found). Three outcomes, never a guess:
    [Proved p] with a proof term [p]; [Refuted] only when the search was
    {e complete}; [Unknown] otherwise. *)

type result =
  | Proved of EConstr.t
  | Refuted
  | Unknown

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

let prove (env : Environ.env) (sigma : Evd.evar_map) (goal : EConstr.t) : result
  =
  let goal = Reductionops.nf_evar sigma goal in
  if not (closed sigma goal)
  then Unknown
  else (
    match search env sigma !max_depth goal with
    | Some (sigma, p), _ ->
      (* belt and braces: the term must typecheck against the goal *)
      (match Typing.check env sigma p goal with
       | _ -> Proved p
       | exception _ -> Unknown)
    | None, true -> Refuted
    | None, false -> Unknown)
;;
