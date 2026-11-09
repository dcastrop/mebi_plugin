(* See the [.mli]. doc/learn.md. *)

(** A hash of the whole term, consistent with [Constr.equal]: names,
    applications, variables and literals are hashed; any other node (a
    binder, a match) only by its kind, since [Constr.equal] ignores binder
    names. ([Hashtbl.hash] looks at 10 nodes, so large terms that differ
    deep down collide; [Constr.hcons] differs between Rocq 9.2 and 9.3.) *)
let rec deep_hash (t : Constr.t) : int =
  let ( ++ ) h x = ((h * 31) + x) land max_int in
  match Constr.kind t with
  | Constr.App (f, a) ->
    Array.fold_left (fun h x -> h ++ deep_hash x) (deep_hash f) a
  | Constr.Construct ((ind, i), _) -> Names.Ind.CanOrd.hash ind ++ i
  | Constr.Ind (ind, _) -> Names.Ind.CanOrd.hash ind ++ 1
  | Constr.Const (c, _) -> Names.Constant.CanOrd.hash c ++ 2
  | Constr.Rel i -> 3 ++ i
  | Constr.Meta i -> 4 ++ i
  | Constr.Int i -> 5 ++ Uint63.hash i
  | Constr.Cast (t, _, _) -> deep_hash t
  | _ -> 6
;;

module CTbl = Hashtbl.Make (struct
    type t = Constr.t

    let equal = Constr.equal
    let hash = deep_hash
  end)

(** The steps of closed terms, per LTS and term. *)
module STbl = Hashtbl.Make (struct
    type t = Names.inductive * Constr.t

    let equal (i, a) (j, b) = Names.Ind.CanOrd.equal i j && Constr.equal a b
    let hash (i, a) = (Names.Ind.CanOrd.hash i * 31) + deep_hash a
  end)

(** Values, shapes and labels, each given an integer (from 1: 0 is "no
    slot"), shared by both systems so that labels compare. *)
type tables =
  { values : int CTbl.t
  ; value_of : (int, Constr.t) Hashtbl.t
  ; shapes : (string, int) Hashtbl.t
  ; shape_of : (int, Skeleton.shape) Hashtbl.t
  ; labels : int CTbl.t
  ; label_of : (int, Constr.t) Hashtbl.t
  ; perm : (string, int array) Hashtbl.t
    (** per shape (its key): the rank of each slot, in which its paths
        are numbered when first met (the start shape of a system of one
        shape, [explore]); none, left to right *)
  }

let tables () =
  { values = CTbl.create 64
  ; value_of = Hashtbl.create 64
  ; shapes = Hashtbl.create 8
  ; shape_of = Hashtbl.create 8
  ; labels = CTbl.create 8
  ; label_of = Hashtbl.create 8
  ; perm = Hashtbl.create 4
  }
;;

(* {1 Serialisation: shapes, positions and values as numbers}

   A state is serialised from its container representation
   (lib/skeleton/container.ml): its shape becomes a number, each position
   a vector index, each value a number. A position is a path in the
   shape, numbered the first time a shape of this system has it, so that
   a step that changes the shape at one place leaves every value
   elsewhere where it was: only the positions at or below that place
   change. *)

(** a system's positions: each path's vector index (from 1), each shape
    number's position of each slot, and back *)
type layout =
  { paths : (Container.path, int) Hashtbl.t
  ; spos : (int, int array) Hashtbl.t
  ; sat : (int * int, int) Hashtbl.t
  }

let layout () =
  { paths = Hashtbl.create 16
  ; spos = Hashtbl.create 8
  ; sat = Hashtbl.create 32
  }
;;

let intern tbl back (t : Constr.t) : int =
  match CTbl.find_opt tbl t with
  | Some i -> i
  | None ->
    let i = CTbl.length tbl + 1 in
    CTbl.add tbl t i;
    Hashtbl.add back i t;
    i
;;

type system =
  { lts : Names.inductive
  ; start : Constr.t
  }

(* a closed term whose steps the rules do not determine (a premise the
   interpreter cannot follow), with its LTS *)
exception Unfollowable of Names.inductive * Constr.t
exception Too_wide of int

(** A system's vectors: [shape; the value at each position], padded to
    [width] positions with 0. *)
type encoder =
  { env : Environ.env
  ; structural : Names.constructor list
  ; width : int
  ; tb : tables
  ; lay : layout
  }

(* the positions of the slots of shape [sid], numbering the paths it has
   that are new (in its slots' rank if it has one); raises [Too_wide] if
   one is past the width *)
let positions (e : encoder) (sid : int) : int array =
  match Hashtbl.find_opt e.lay.spos sid with
  | Some a -> a
  | None ->
    let shape = Hashtbl.find e.tb.shape_of sid in
    let ps = Array.of_list (Container.leaf_paths shape) in
    let n = Array.length ps in
    let order =
      match Hashtbl.find_opt e.tb.perm (Skeleton.key shape) with
      | Some r when Array.length r = n ->
        List.sort (fun i j -> compare r.(i) r.(j)) (List.init n Fun.id)
      | _ -> List.init n Fun.id
    in
    List.iter
      (fun j ->
        if Bool.not (Hashtbl.mem e.lay.paths ps.(j))
        then Hashtbl.add e.lay.paths ps.(j) (Hashtbl.length e.lay.paths + 1))
      order;
    let a = Array.map (Hashtbl.find e.lay.paths) ps in
    Array.iter (fun q -> if q > e.width then raise (Too_wide q)) a;
    Hashtbl.add e.lay.spos sid a;
    Array.iteri (fun j q -> Hashtbl.replace e.lay.sat (sid, q) j) a;
    a
;;

(* slot [j] of shape [sid] is at position [pos_of e sid j]; position [p]
   holds slot [slot_at e sid p], or a number past the shape's slots if it
   is none of them (padding) *)
let pos_of (e : encoder) (sid : int) (j : int) : int = (positions e sid).(j)

let slot_at (e : encoder) (sid : int) (p : int) : int =
  ignore (positions e sid);
  match Hashtbl.find_opt e.lay.sat (sid, p) with Some j -> j | None -> max_int
;;

let encode (e : encoder) (t : Constr.t) : int array =
  let shape, vals = Skeleton.split e.env e.structural t in
  let n = List.length vals in
  if n > e.width then raise (Too_wide n);
  let k = Skeleton.key shape in
  let sid =
    match Hashtbl.find_opt e.tb.shapes k with
    | Some i -> i
    | None ->
      let i = Hashtbl.length e.tb.shapes + 1 in
      Hashtbl.add e.tb.shapes k i;
      Hashtbl.add e.tb.shape_of i shape;
      i
  in
  let v = Array.make (e.width + 1) 0 in
  v.(0) <- sid;
  List.iteri
    (fun i x -> v.(pos_of e sid i) <- intern e.tb.values e.tb.value_of x)
    vals;
  v
;;

let decode (e : encoder) (v : int array) : Constr.t =
  let shape = Hashtbl.find e.tb.shape_of v.(0) in
  let n = List.length (Skeleton.slots shape) in
  Skeleton.rebuild
    shape
    (Array.init n (fun i -> Hashtbl.find e.tb.value_of v.(pos_of e v.(0) i)))
;;

let shape_id (tb : tables) (shape : Skeleton.shape) : int =
  let k = Skeleton.key shape in
  match Hashtbl.find_opt tb.shapes k with
  | Some i -> i
  | None ->
    let i = Hashtbl.length tb.shapes + 1 in
    Hashtbl.add tb.shapes k i;
    Hashtbl.add tb.shape_of i shape;
    i
;;

(* {1 The interpreter: one summand, on a partial state}

   A partial state is its shape with the read slots' values and, for every
   other slot, a placeholder [Meta i]. [fire] follows the summand's rule
   chain: it matches each rule's source, evaluates decidable premises, and
   computes the label and the target, where a placeholder stands for a
   slot carried unchanged. A closed premise's steps come from its LTS's
   rules ([steps], [interp]). *)

exception No_match

(** a computation needed an unread slot: the summand reads more than the
    analysis said, so its projection falls back to whole states *)
exception Stuck

let fields env (t : Constr.t) =
  let h, args = Constr.decompose_app_list t in
  match Constr.kind h with
  | Constr.Construct (((ind, _) as c), _) ->
    let mib, _ = Inductive.lookup_mind_specif env ind in
    let np = mib.Declarations.mind_nparams in
    if List.length args >= np
    then Some (c, List.filteri (fun i _ -> i >= np) args)
    else None
  | _ -> None
;;

let has_meta (t : Constr.t) : bool =
  let r = ref false in
  let rec go t =
    match Constr.kind t with
    | Constr.Meta _ -> r := true
    | _ -> Constr.iter go t
  in
  go t;
  !r
;;

let unbound = Constr.mkMeta 1_000_000

(** [cmatch env b p t]: binds the pattern [p]'s variables (de Bruijn, at
    depth 0) against the term [t] *)
let rec cmatch env (b : (int, Constr.t) Hashtbl.t) (p : Constr.t) (t : Constr.t)
  : unit
  =
  match Constr.kind p with
  | Constr.Rel i ->
    (match Hashtbl.find_opt b i with
     | None -> Hashtbl.replace b i t
     | Some t' ->
       if has_meta t || has_meta t' then raise Stuck;
       if Bool.not (Constr.equal t t') then raise No_match)
  | Constr.Cast (p, _, _) -> cmatch env b p t
  | _ ->
    (match fields env p with
     | Some (c, ps) ->
       (match Constr.kind t with
        | Constr.Meta _ -> raise Stuck
        | _ ->
          (match fields env t with
           | Some (c', ts)
             when Names.Construct.CanOrd.equal c c'
                  && List.length ps = List.length ts ->
             List.iter2 (cmatch env b) ps ts
           | Some _ -> raise No_match
           | None -> raise Stuck))
     | None ->
       (* a closed term that is not a constructor (a constant such as
          [Acquire]): compare values *)
       if has_meta t then raise Stuck;
       if Bool.not (Vars.closed0 p) then raise Stuck;
       let nf x =
         EConstr.to_constr
           Evd.empty
           (Reductionops.nf_all env Evd.empty (EConstr.of_constr x))
       in
       if Bool.not (Constr.equal (nf p) t) then raise No_match)
;;

let subst (b : (int, Constr.t) Hashtbl.t) (n : int) (t : Constr.t) : Constr.t =
  Vars.substl
    (List.init n (fun i ->
       match Hashtbl.find_opt b (i + 1) with Some x -> x | None -> unbound))
    t
;;

(** [compute env sigma t]: [t] normalised, around its placeholders *)
let rec compute env sigma (t : Constr.t) : Constr.t =
  if Bool.not (has_meta t)
  then
    EConstr.to_constr
      sigma
      (Reductionops.nf_all env sigma (EConstr.of_constr t))
  else (
    match Constr.kind t with
    | Constr.Meta i -> if i = 1_000_000 then raise Stuck else t
    | _ ->
      let h, args = Constr.decompose_app_list t in
      (match Constr.kind h with
       | Constr.Construct _ ->
         Constr.mkApp (h, Array.of_list (List.map (compute env sigma) args))
       | _ -> raise Stuck))
;;

type ctx =
  { env : Environ.env
  ; sigma : Evd.evar_map
  ; rules : (Names.inductive, Rules.rule list) Hashtbl.t
  ; dtrue : Constr.t
  ; dfalse : Constr.t
  ; memo : (Constr.t * Constr.t) list STbl.t
  ; mutable depth : int
  ; mutable interpreted : int (** closed terms' steps, from the rules *)
  }

(** how deep premises over closed terms are interpreted (a rule whose
    premise steps from its own source would recurse for ever): deeper, the
    step is [Unfollowable] *)
let max_depth = 64

let rules_of (c : ctx) (lts : Names.inductive) : Rules.rule list =
  match Hashtbl.find_opt c.rules lts with
  | Some r -> r
  | None ->
    List.iter
      (fun (l : Rules.lts) -> Hashtbl.replace c.rules l.Rules.ind l.Rules.rules)
      (Rules.of_lts c.env c.sigma lts);
    Stdlib.Option.value (Hashtbl.find_opt c.rules lts) ~default:[]
;;

(** [fire c lts src chain]: the steps of the partial term [src] by [lts]
    along [chain] (rule names, and [<..>] for a closed premise's step):
    each label, target, and the rest of the chain *)
let rec fire
          (c : ctx)
          (lts : Names.inductive)
          (src : Constr.t)
          (chain : string list)
  : (Constr.t * Constr.t * string list) list
  =
  match chain with
  | [] -> []
  | name :: rest when String.length name > 0 && name.[0] = '<' ->
    if has_meta src then raise Stuck;
    List.map (fun (l, t) -> l, t, rest) (steps c lts src)
  | name :: rest ->
    (match
       List.find_opt
         (fun (r : Rules.rule) -> Names.Id.to_string r.Rules.name = name)
         (rules_of c lts)
     with
     | None -> []
     | Some r ->
       let n = List.length r.Rules.ctx in
       let b = Hashtbl.create 8 in
       (match cmatch c.env b r.Rules.src src with
        | exception No_match -> []
        | () ->
          List.map
            (fun (b, chain') ->
              ( compute c.env c.sigma (subst b n r.Rules.lbl)
              , compute c.env c.sigma (subst b n r.Rules.tgt)
              , chain' ))
            (prems c n r.Rules.premises b rest)))

and prems c n (ps : Rules.premise list) b chain =
  match ps with
  | [] -> [ b, chain ]
  | Rules.Decide { witness; _ } :: ps' ->
    let w = subst b n witness in
    if has_meta w then raise Stuck;
    let v = compute c.env c.sigma w in
    if Constr.equal v c.dtrue
    then prems c n ps' b chain
    else if Constr.equal v c.dfalse
    then []
    else
      (* neither: the witness is stuck on something opaque *)
      raise Stuck
  | Rules.Opaque _ :: _ -> raise Stuck
  | Rules.Step { lts; src; lbl; tgt } :: ps' ->
    let s = compute c.env c.sigma (subst b n src) in
    List.concat_map
      (fun (l, t, chain') ->
        let b' = Hashtbl.copy b in
        match
          cmatch c.env b' lbl l;
          cmatch c.env b' tgt t
        with
        | exception No_match -> []
        | () -> prems c n ps' b' chain')
      (fire c lts s chain)

(** [steps c lts src]: every step of the closed term [src] by [lts], from
    [lts]'s rules; memoised. [Unfollowable] where the interpreter cannot
    follow them. *)
and steps c lts (src : Constr.t) : (Constr.t * Constr.t) list =
  match STbl.find_opt c.memo (lts, src) with
  | Some r -> r
  | None ->
    let explicit () = raise (Unfollowable (lts, src)) in
    let r =
      if c.depth >= max_depth
      then explicit ()
      else (
        c.depth <- c.depth + 1;
        match interp c lts src with
        | r ->
          c.depth <- c.depth - 1;
          c.interpreted <- c.interpreted + 1;
          r
        | exception Stuck ->
          c.depth <- c.depth - 1;
          explicit ())
    in
    STbl.add c.memo (lts, src) r;
    r

(** [interp c lts src]: the steps of the closed term [src] by every rule
    of [lts]. Raises [Stuck] where a rule's source is not a pattern it can
    match, or a label or target needs a variable nothing binds. *)
and interp c lts (src : Constr.t) : (Constr.t * Constr.t) list =
  List.concat_map
    (fun (r : Rules.rule) ->
      let n = List.length r.Rules.ctx in
      let b = Hashtbl.create 8 in
      match cmatch c.env b r.Rules.src src with
      | exception No_match -> []
      | () ->
        List.map
          (fun b ->
            ( compute c.env c.sigma (subst b n r.Rules.lbl)
            , compute c.env c.sigma (subst b n r.Rules.tgt) ))
          (prems_all c n r.Rules.premises b))
    (rules_of c lts)

and prems_all c n (ps : Rules.premise list) b =
  match ps with
  | [] -> [ b ]
  | Rules.Decide { witness; _ } :: ps' ->
    let w = subst b n witness in
    if has_meta w then raise Stuck;
    let v = compute c.env c.sigma w in
    if Constr.equal v c.dtrue
    then prems_all c n ps' b
    else if Constr.equal v c.dfalse
    then []
    else
      (* neither: the witness is stuck on something opaque *)
      raise Stuck
  | Rules.Opaque _ :: _ ->
    (* not computable: the interpreter cannot follow it *)
    raise Stuck
  | Rules.Step { lts; src; lbl; tgt } :: ps' ->
    let s = compute c.env c.sigma (subst b n src) in
    if has_meta s then raise Stuck;
    List.concat_map
      (fun (l, t) ->
        let b' = Hashtbl.copy b in
        match
          cmatch c.env b' lbl l;
          cmatch c.env b' tgt t
        with
        | exception No_match -> []
        | () -> prems_all c n ps' b')
      (steps c lts s)
;;

(* {1 Summands, learned} *)

type code =
  | RW
  | RO
  | WO
  | Copy

(* {1 Derivations, for the proof from the certificate}

   The steps of a closed term with how the rules give each (doc/certificate.md,
   "The theorem from the certificate"): the rule, its variables' values,
   its premises' derivations; as the interpreter finds the steps, with the
   same matching and normalisation. *)

type deriv =
  { drule : Rules.rule
  ; dlts : Names.inductive
  ; dvals : Constr.t array
    (** each variable's value, outermost first; a
        variable nothing binds is a placeholder *)
  ; dprems : dprem list
  ; dlbl : Constr.t
  ; dtgt : Constr.t
  }

and dprem =
  | DStep of deriv
  | DDecide of Constr.t (** the proposition, its variables substituted *)

let new_ctx env sigma : ctx =
  { env
  ; sigma
  ; rules = Hashtbl.create 4
  ; dtrue =
      UnivGen.constr_of_monomorphic_global
        env
        (Rocqlib.lib_ref "core.bool.true")
  ; dfalse =
      UnivGen.constr_of_monomorphic_global
        env
        (Rocqlib.lib_ref "core.bool.false")
  ; memo = STbl.create 64
  ; depth = 0
  ; interpreted = 0
  }
;;

(** [derivations env sigma lts src]: every step of the closed term [src]
    by [lts], each with its derivation *)
let derivations env sigma =
  let c = new_ctx env sigma in
  let memo : (Names.inductive * Constr.t, deriv list) Hashtbl.t =
    Hashtbl.create 64
  in
  let rec all lts src : deriv list =
    match Hashtbl.find_opt memo (lts, src) with
    | Some r -> r
    | None ->
      let r =
        List.concat_map
          (fun (r : Rules.rule) ->
            let n = List.length r.Rules.ctx in
            let b = Hashtbl.create 8 in
            match cmatch c.env b r.Rules.src src with
            | exception No_match -> []
            | () ->
              List.map
                (fun (b, ps) ->
                  { drule = r
                  ; dlts = lts
                  ; dvals =
                      Array.init n (fun i ->
                        match Hashtbl.find_opt b (n - i) with
                        | Some x -> x
                        | None -> unbound)
                  ; dprems = List.rev ps
                  ; dlbl = compute c.env c.sigma (subst b n r.Rules.lbl)
                  ; dtgt = compute c.env c.sigma (subst b n r.Rules.tgt)
                  })
                (prems n r.Rules.premises b []))
          (rules_of c lts)
      in
      Hashtbl.add memo (lts, src) r;
      r
  and prems n (ps : Rules.premise list) b acc =
    match ps with
    | [] -> [ b, acc ]
    | Rules.Decide { witness; prop } :: ps' ->
      let w = subst b n witness in
      let v = compute c.env c.sigma w in
      if Constr.equal v c.dtrue
      then prems n ps' b (DDecide (subst b n prop) :: acc)
      else []
    | Rules.Opaque _ :: _ -> raise Stuck
    | Rules.Step { lts; src; lbl; tgt } :: ps' ->
      let s = compute c.env c.sigma (subst b n src) in
      List.concat_map
        (fun (d : deriv) ->
          let b' = Hashtbl.copy b in
          match
            cmatch c.env b' lbl d.dlbl;
            cmatch c.env b' tgt d.dtgt
          with
          | exception No_match -> []
          | () -> prems n ps' b' (DStep d :: acc))
        (all lts s)
  in
  all
;;

(** [derive_chain env sigma lts src chain]: the steps of the partial term
    [src] (placeholders [Meta i] for slots it does not read) along the
    summand's [chain], each with its derivation, as [fire] finds them; a
    closed premise's steps ([<..>] in the chain) come from {!derivations}.
    Raises [Failure] where [fire] is stuck. *)
let derive_chain env sigma =
  let c = new_ctx env sigma in
  let closed = derivations env sigma in
  let rec fire_d lts (src : Constr.t) (chain : string list)
    : (deriv * string list) list
    =
    match chain with
    | [] -> []
    | name :: rest when String.length name > 0 && name.[0] = '<' ->
      if has_meta src then raise Stuck;
      List.map (fun d -> d, rest) (closed lts src)
    | name :: rest ->
      (match
         List.find_opt
           (fun (r : Rules.rule) -> Names.Id.to_string r.Rules.name = name)
           (rules_of c lts)
       with
       | None -> []
       | Some r ->
         let n = List.length r.Rules.ctx in
         let b = Hashtbl.create 8 in
         (match cmatch c.env b r.Rules.src src with
          | exception No_match -> []
          | () ->
            List.map
              (fun (b, ps, chain') ->
                ( { drule = r
                  ; dlts = lts
                  ; dvals =
                      Array.init n (fun i ->
                        match Hashtbl.find_opt b (n - i) with
                        | Some x -> x
                        | None -> unbound)
                  ; dprems = List.rev ps
                  ; dlbl = compute c.env c.sigma (subst b n r.Rules.lbl)
                  ; dtgt = compute c.env c.sigma (subst b n r.Rules.tgt)
                  }
                , chain' ))
              (prems n r.Rules.premises b [] rest)))
  and prems n (ps : Rules.premise list) b acc chain =
    match ps with
    | [] -> [ b, acc, chain ]
    | Rules.Decide { witness; prop } :: ps' ->
      let w = subst b n witness in
      if has_meta w then raise Stuck;
      let v = compute c.env c.sigma w in
      if Constr.equal v c.dtrue
      then prems n ps' b (DDecide (subst b n prop) :: acc) chain
      else if Constr.equal v c.dfalse
      then []
      else raise Stuck
    | Rules.Opaque _ :: _ -> raise Stuck
    | Rules.Step { lts; src; lbl; tgt } :: ps' ->
      let s = compute c.env c.sigma (subst b n src) in
      List.concat_map
        (fun ((d : deriv), chain') ->
          let b' = Hashtbl.copy b in
          match
            cmatch c.env b' lbl d.dlbl;
            cmatch c.env b' tgt d.dtgt
          with
          | exception No_match -> []
          | () -> prems n ps' b' (DStep d :: acc) chain')
        (fire_d lts s chain)
  in
  fun lts src chain ->
    match fire_d lts src chain with
    | r ->
      List.filter_map (fun (d, rest) -> if rest = [] then Some d else None) r
    | exception Stuck -> failwith "a derivation the interpreter cannot follow"
;;

(** where Sylvan's proof log goes for the next decision, if anywhere (the
    plugin sets it, from [Set MeBi Log]; doc/certificate.md, A) *)
let proof_log : Proof_log.source option ref = ref None

module Plan = struct
  type prel =
    { label : int
    ; meta : int
    ; rel : int
    }

  type punion =
    { acc : int
    ; arg : int
    ; res : int
    }

  type pimgs =
    { src : int
    ; rels : prel list
    ; imgs : int list
    ; unions : punion list
    ; res : int
    }

  type pround =
    { fresh : int
    ; p : pimgs
    ; tz : int
    ; fresh' : int
    ; tz' : int
    }

  type pclosure =
    { cstart : int
    ; rounds : pround list
    ; final : int
    }

  type pmove =
    { mrel : prel
    ; img : int
    ; ok : int
    }

  type plabel =
    { lab : int
    ; okset : int
    ; okfrom : (pimgs * pclosure) option
    ; moves : pmove list
    }

  (** a summand of the mover, for completeness: its source shape, the
      positions it reads ([keep], position 0 the shape), its rule chain,
      its relations (label, codes, diagram), the projections it learned
      ([seen]); and the coverage of [z] by them: [f] the image of [z] by
      the shape's filter [sh] (complete), [g] its image by [seen] read
      only (sound), their difference empty *)
  type psum =
    { sid : int
    ; keep : bool list
    ; chain : string list
    ; srels : (int * string * Ldd.t * int) list
    ; seen : Ldd.t
    ; escaped : bool
    ; sh : prel
    ; dsee : prel
    ; f : int
    ; g : int
    }

  type pdir =
    { mover_a : bool
    ; tzc : pclosure
    ; plabels : plabel list
    ; cover : psum list
    }

  (** a system, for its encoding: its width (positions after the shape),
      its shapes (number, shape, each slot's position), its start *)
  type psys =
    { pwidth : int
    ; pshapes : (int * Skeleton.shape * int array) list
    ; pstart : Constr.t
    ; plts : Names.inductive
    }

  type t =
    { z : int
    ; start : int array
    ; b_first : bool
    ; widths : int * int
    ; tau : int option
    ; label_terms : (int * Constr.t) list
    ; dirs : pdir list
    ; values : (int * Constr.t) list
    ; sys_a : psys
    ; sys_b : psys
    ; rinfo :
        ((int * int * int) * (int * string * Ldd.t * int * string list)) list
    }
end

let last_plan : Plan.t option ref = ref None

(** One learned relation: its meta codes (one per vector position), its
    label, and its tuples. *)
type rel =
  { rcodes : code array
  ; label : int
  ; rel : Ldd.t
  }

(** A summand of one source shape: its chain, the positions it reads
    (shape first), the meta codes its static target gives every vector
    position, and what it has learned. Its relations are per label and per
    meta codes: a result that needs more positions written than the static
    codes say (its shape changed: a value gained structure) goes in a
    relation of its own, with those positions written. Projections that
    escaped are learned per whole state, every position read and written. *)
type summand =
  { sid : int
  ; chain : string list
  ; keep : bool list (** the projection: position 0 (the shape), then slots *)
  ; codes : code array
  ; nslots : int (** the source shape's slots *)
  ; mutable seen : Ldd.t
  ; mutable rels : rel list
  ; mutable escaped : int array list
  ; mutable gseen : Ldd.t
  }

let levels (codes : code array) : Ldd.level list =
  List.concat_map
    (function
      | RW -> [ Ldd.Read; Ldd.Write ]
      | RO -> [ Ldd.Only_read ]
      | WO -> [ Ldd.Only_write ]
      | Copy -> [ Ldd.Copy ])
    (Array.to_list codes)
;;

(** [meta_levels ~offset codes]: Sylvan's meta for a relation coded
    [codes] applied [offset] positions into the vector: [offset] copied
    levels, then the relation's, without its trailing copied levels (past
    the meta's end Sylvan carries the rest of the vector unchanged, with
    no step per node) *)
let meta_levels ?(offset = 0) (codes : code array) : Ldd.level list =
  let rec trim = function Ldd.Copy :: rest -> trim rest | l -> l in
  List.rev
    (trim (List.rev (List.init offset (fun _ -> Ldd.Copy) @ levels codes)))
;;

(** [inverse codes rel]: the relation [rel] (positions coded [codes])
    backwards, each tuple's read and written values swapped at its [RW]
    positions; [None] if a position is only written ([WO]: backwards, any
    value could have been there) *)
let inverse (codes : code array) (rel : Ldd.t) : Ldd.t option =
  if Array.exists (fun c -> c = WO) codes
  then None
  else (
    let swap (t : int array) =
      let t = Array.copy t in
      let i = ref 0 in
      Array.iter
        (function
          | RW ->
            let a = t.(!i) in
            t.(!i) <- t.(!i + 1);
            t.(!i + 1) <- a;
            i := !i + 2
          | RO | WO -> incr i
          | Copy -> ())
        codes;
      t
    in
    Some (Ldd.of_list (List.map swap (Ldd.elements ~limit:max_int rel))))
;;

let make_summand
      (e : encoder)
      (sid : int)
      (src : Skeleton.shape)
      (sm : Skeleton.summand)
  : summand
  =
  let width = e.width in
  let ks = List.length (Skeleton.slots src) in
  let tslots = Skeleton.target_slots sm.Skeleton.target in
  let kt = List.length tslots in
  if kt > width then raise (Too_wide kt);
  (* the target's shape and the position of each of its slots: a value
     stays where it is if it keeps its path *)
  let tsid =
    shape_id
      e.tb
      (Container.renumber (Skeleton.target_shape sm.Skeleton.target))
  in
  let src_at = slot_at e sid
  and tgt_at = slot_at e tsid in
  let in_place i j = pos_of e sid i = pos_of e tsid j in
  let moved_elsewhere =
    List.concat
      (List.mapi
         (fun j o ->
           match o with
           | Skeleton.Moved i when Bool.not (in_place i j) -> [ i ]
           | _ -> [])
         tslots)
  in
  let written p =
    let js = src_at p
    and jt = tgt_at p in
    (jt < kt
     &&
     match List.nth tslots jt with
     | Skeleton.Moved i -> Bool.not (in_place i jt)
     | Skeleton.Computed _ -> true)
    || (jt >= kt && js < ks)
  in
  (* a slot the step overwrites is read too, so that the relation can be
     read backwards (doc/certificate.md, A); a position that is no slot of
     the source holds 0, read as such *)
  let overwritten =
    List.filter_map
      (fun p -> if written p && src_at p < ks then Some (src_at p) else None)
      (List.init width (fun q -> q + 1))
  in
  let reads =
    List.sort_uniq compare (sm.Skeleton.reads @ moved_elsewhere @ overwritten)
  in
  let codes =
    Array.init (width + 1) (fun p ->
      if p = 0
      then if tsid = sid then RO else RW
      else (
        let js = src_at p in
        match js < ks && List.mem js reads, written p with
        | true, true -> RW
        | true, false -> RO
        | false, true -> RW
        | false, false -> Copy))
  in
  let keep =
    true
    :: List.init width (fun q ->
      let js = src_at (q + 1) in
      js < ks && List.mem js reads)
  in
  { sid
  ; chain = sm.Skeleton.chain
  ; keep
  ; codes
  ; nslots = ks
  ; seen = Ldd.empty ()
  ; rels = []
  ; escaped = []
  ; gseen = Ldd.empty ()
  }
;;

let add_rel (rels : rel list) (codes : code array) (l : int) tuples =
  let add = Ldd.of_list tuples in
  match List.partition (fun r -> r.label = l && r.rcodes = codes) rels with
  | [ old ], rest ->
    let u = Ldd.union old.rel add in
    Ldd.release old.rel;
    Ldd.release add;
    { old with rel = u } :: rest
  | _ -> { rcodes = codes; label = l; rel = add } :: rels
;;

(** [add_batch sm batch]: [batch]'s tuples, keyed by meta codes and label,
    added to [sm]'s relations *)
let add_batch (sm : summand) batch =
  Hashtbl.iter
    (fun (codes, l) ts -> sm.rels <- add_rel sm.rels codes l ts)
    batch
;;

let batch_add batch key tuple =
  Hashtbl.replace
    batch
    key
    (tuple :: Stdlib.Option.value (Hashtbl.find_opt batch key) ~default:[])
;;

(** A runtime target: its shape, and each slot moved from a source slot or
    a value. *)
type tslot =
  | Mv of int
  | Val of int

let split_target (e : encoder) (t : Constr.t) : string * int * tslot list =
  let shape, vals = Skeleton.split e.env e.structural t in
  if List.length vals > e.width then raise (Too_wide (List.length vals));
  let slots =
    List.map
      (fun v ->
        match Constr.kind v with
        | Constr.Meta i -> Mv i
        | _ -> Val (intern e.tb.values e.tb.value_of v))
      vals
  in
  Skeleton.key shape, shape_id e.tb shape, slots
;;

(** A system explored: its reachable states, its summands and costs. *)
type explored =
  { reach : Ldd.t
  ; summands : summand list
  ; vwidth : int
  ; start_v : int array
  ; states : float
  ; projections : int
  ; escapes : int
  ; rounds : int
  ; enc : encoder (** to decode its vectors *)
  }

let explore (c : ctx) (e : encoder) (sys : system) : explored =
  let vwidth = e.width + 1 in
  (* the order of the slots in the vector (MEBI_ORDER=off: left to
     right): for a system of one shape, the slots its summands use most
     first, so that each summand's positions are close to the front and
     the rest of the vector is copied (as a dependency-matrix ordering
     does, Meijer and van de Pol 2016); the diagrams, and so the
     certificates, are smaller *)
  if Sys.getenv_opt "MEBI_ORDER" <> Some "off"
  then (
    let shape, vals = Skeleton.split e.env e.structural sys.start in
    let k = Skeleton.key shape in
    let n = List.length vals in
    if n > 1 && Bool.not (Hashtbl.mem e.tb.perm k)
    then (
      let sms = Skeleton.summands_of e.env c.sigma sys.lts shape in
      if
        sms <> []
        && List.for_all
             (fun (sm : Skeleton.summand) ->
               Skeleton.key (Skeleton.target_shape sm.Skeleton.target) = k)
             sms
      then (
        let count = Array.make n 0 in
        List.iter
          (fun (sm : Skeleton.summand) ->
            let tslots = Skeleton.target_slots sm.Skeleton.target in
            let written =
              List.concat
                (List.mapi
                   (fun j o ->
                     match o with
                     | Skeleton.Moved i when i = j -> []
                     | _ -> [ j ])
                   tslots)
            in
            List.iter
              (fun j -> if j < n then count.(j) <- count.(j) + 1)
              (List.sort_uniq compare (sm.Skeleton.reads @ written)))
          sms;
        let order =
          List.stable_sort
            (fun i j -> compare count.(j) count.(i))
            (List.init n Fun.id)
        in
        let a = Array.make n 0 in
        List.iteri (fun o j -> a.(j) <- o) order;
        Hashtbl.replace e.tb.perm k a)));
  let start_v = encode e sys.start in
  let by_shape : (int, summand list) Hashtbl.t = Hashtbl.create 8 in
  let summands_of sid =
    match Hashtbl.find_opt by_shape sid with
    | Some l -> l
    | None ->
      let shape = Hashtbl.find e.tb.shape_of sid in
      let l =
        List.map
          (make_summand e sid shape)
          (Skeleton.summands_of e.env c.sigma sys.lts shape)
      in
      Hashtbl.add by_shape sid l;
      l
  in
  let projections = ref 0
  and escapes = ref 0 in
  let all_rw = Array.make vwidth RW in
  (* the source term of a projection: read slots hold values, the others
     placeholders *)
  let partial
        (sid : int)
        (shape : Skeleton.shape)
        (keep : bool list)
        (pv : int array)
    : Constr.t
    =
    let k = List.length (Skeleton.slots shape) in
    let vals = Array.init k (fun i -> Constr.mkMeta i) in
    let idx = ref 1 in
    List.iteri
      (fun p kp ->
        if kp && p > 0
        then (
          let j = slot_at e sid p in
          if j < k then vals.(j) <- Hashtbl.find e.tb.value_of pv.(!idx);
          incr idx))
      keep;
    Skeleton.rebuild shape vals
  in
  (* learn whole states [vs] for summand [sm] (its escaped projections) *)
  let learn_global (sm : summand) (vs : int array list) =
    let batch = Hashtbl.create 4 in
    List.iter
      (fun v ->
        if Bool.not (Ldd.mem sm.gseen v)
        then (
          sm.gseen <- Ldd.union sm.gseen (Ldd.cube v);
          let src = decode e v in
          let results =
            match fire c sys.lts src sm.chain with
            | r -> r
            | exception Stuck ->
              (* the interpreter cannot follow this state along the
                 summand: all its steps (a superset of this summand's,
                 always correct to add) *)
              List.map (fun (l, t) -> l, t, []) (steps c sys.lts src)
          in
          List.iter
            (fun (l, t, _) ->
              let li = intern e.tb.labels e.tb.label_of l in
              let tv = encode e t in
              batch_add
                batch
                (all_rw, li)
                (Array.init (2 * vwidth) (fun i ->
                   if i mod 2 = 0 then v.(i / 2) else tv.(i / 2))))
            results))
      vs;
    add_batch sm batch
  in
  let project_of (sm : summand) (v : int array) : int array =
    Array.of_list
      (List.filteri
         (fun _ x -> x >= 0)
         (List.mapi (fun p kp -> if kp then v.(p) else -1) sm.keep))
  in
  let learn (sm : summand) (frontier : Ldd.t) (visited : Ldd.t) =
    let proj = Ldd.project frontier sm.keep in
    let fresh = Ldd.minus proj sm.seen in
    Ldd.release proj;
    let shape = Hashtbl.find e.tb.shape_of sm.sid in
    let batch = Hashtbl.create 4 in
    List.iter
      (fun pv ->
        if pv.(0) = sm.sid
        then (
          incr projections;
          let old =
            (* the value at each vector position this projection holds *)
            let a = Array.make vwidth 0 in
            let idx = ref 0 in
            List.iteri
              (fun p kp ->
                if kp
                then (
                  a.(p) <- pv.(!idx);
                  incr idx))
              sm.keep;
            a
          in
          match fire c sys.lts (partial sm.sid shape sm.keep pv) sm.chain with
          | exception Stuck ->
            incr escapes;
            sm.escaped <- pv :: sm.escaped
          | results ->
            let read j = List.nth sm.keep (pos_of e sm.sid j) in
            let local =
              List.map
                (fun (l, t, _) ->
                  let _, tsid, tslots = split_target e t in
                  (* a value moved to another position must have been
                     read *)
                  let ok =
                    List.for_all
                      Fun.id
                      (List.mapi
                         (fun j -> function
                           | Mv i ->
                             pos_of e sm.sid i = pos_of e tsid j || read i
                           | Val _ -> true)
                         tslots)
                  in
                  ok, l, tsid, Array.of_list tslots)
                results
            in
            if List.exists (fun (ok, _, _, _) -> Bool.not ok) local
            then (
              incr escapes;
              sm.escaped <- pv :: sm.escaped)
            else
              List.iter
                (fun (_, l, tsid, tslots) ->
                  let li = intern e.tb.labels e.tb.label_of l in
                  let kt = Array.length tslots in
                  let nw p =
                    if p = 0
                    then tsid
                    else (
                      let j = slot_at e tsid p in
                      if j < kt
                      then (
                        match tslots.(j) with
                        | Mv i -> old.(pos_of e sm.sid i)
                        | Val v -> v)
                      else 0)
                  in
                  (* the positions this result writes: a read slot's value
                     comes back as a value, not a placeholder, so a value
                     equal to a read slot's own is no write; an unread slot
                     left in place is none either, so a write-only position
                     becomes a copy *)
                  let needs p =
                    if p = 0
                    then tsid <> sm.sid
                    else (
                      let j = slot_at e tsid p in
                      if j < kt
                      then (
                        match tslots.(j) with
                        | Mv i -> pos_of e sm.sid i <> p
                        | Val v -> Bool.not (List.nth sm.keep p && v = old.(p)))
                      else slot_at e sm.sid p < sm.nslots)
                  in
                  (* written where the projection does not read: no slot
                     of the source there, so 0, read as such *)
                  let wo p =
                    if slot_at e sm.sid p >= sm.nslots then RW else WO
                  in
                  let codes =
                    Array.mapi
                      (fun p c ->
                        match c with
                        | RW ->
                          if needs p || List.nth sm.keep p then RW else Copy
                        | WO -> if needs p then wo p else Copy
                        | RO -> if needs p then RW else RO
                        | Copy -> if needs p then wo p else Copy)
                      sm.codes
                  in
                  let tuple =
                    List.concat
                      (List.mapi
                         (fun p code ->
                           match code with
                           | RW -> [ old.(p); nw p ]
                           | RO -> [ old.(p) ]
                           | WO -> [ nw p ]
                           | Copy -> [])
                         (Array.to_list codes))
                  in
                  batch_add batch (codes, li) (Array.of_list tuple))
                local))
      (Ldd.elements ~limit:max_int fresh);
    let s = Ldd.union sm.seen fresh in
    Ldd.release sm.seen;
    Ldd.release fresh;
    sm.seen <- s;
    add_batch sm batch;
    (* escaped projections: their whole states, among those visited *)
    if sm.escaped <> []
    then
      learn_global
        sm
        (List.filter
           (fun v ->
             v.(0) = sm.sid
             && List.exists (fun pv -> project_of sm v = pv) sm.escaped)
           (Ldd.elements ~limit:max_int visited))
  in
  let visited = ref (Ldd.cube start_v) in
  let frontier = ref (Ldd.cube start_v) in
  let rounds = ref 0 in
  let escaped_states (sm : summand) (s : Ldd.t) =
    if sm.escaped <> []
    then
      learn_global
        sm
        (List.filter
           (fun v ->
             v.(0) = sm.sid
             && List.exists (fun pv -> project_of sm v = pv) sm.escaped)
           (Ldd.elements ~limit:max_int s))
  in
  (* chaining: each summand in turn, learned on and applied to the round's
     frontier grown by what the summands before it found *)
  while Bool.not (Ldd.is_empty !frontier) do
    incr rounds;
    Ldd.reclaim ();
    let shapes =
      List.map
        (fun v -> v.(0))
        (Ldd.elements ~limit:max_int (Ldd.project !frontier [ true ]))
    in
    let sms = List.concat_map summands_of shapes in
    let cur = ref (Ldd.union !frontier (Ldd.empty ())) in
    let added = ref (Ldd.empty ()) in
    List.iter
      (fun sm ->
        learn sm !cur !visited;
        let next =
          List.fold_left
            (fun acc r ->
              Ldd.union
                acc
                (Ldd.relprod
                   !cur
                   ~rel:r.rel
                   ~meta:(Ldd.rel_meta (meta_levels r.rcodes))))
            (Ldd.empty ())
            sm.rels
        in
        let f = Ldd.minus next !visited in
        Ldd.release next;
        if Bool.not (Ldd.is_empty f)
        then (
          let v = Ldd.union !visited f
          and c = Ldd.union !cur f
          and a = Ldd.union !added f in
          Ldd.release !visited;
          Ldd.release !cur;
          Ldd.release !added;
          visited := v;
          cur := c;
          added := a;
          (* an escaped projection's states found now: learn them too *)
          List.iter (fun sm -> escaped_states sm f) sms);
        Ldd.release f)
      sms;
    Ldd.release !cur;
    Ldd.release !frontier;
    frontier := !added
  done;
  { reach = !visited
  ; summands = Hashtbl.fold (fun _ l acc -> l @ acc) by_shape []
  ; vwidth
  ; start_v
  ; states = Ldd.count !visited
  ; projections = !projections
  ; escapes = !escapes
  ; rounds = !rounds
  ; enc = e
  }
;;

(* {1 Components (doc/compositional.md; measurement, MEBI_COMPOSE)} *)

(** [context_key env lts r]: if [r] is a context rule of [lts], the
    position it descends into, as a string (constructor names and argument
    positions from the source's root): its only premise is a step of [lts]
    with the conclusion's label, and its source and target differ from the
    premise's in one position only, where the premise has a variable [x]
    (then [x']) and the conclusion [C[x]] (then [C[x']]), every other
    argument copied *)
let context_key env (lts : Names.inductive) (r : Rules.rule) : string option =
  match r.Rules.premises with
  | [ Rules.Step { lts = l; src = p; lbl; tgt = p' } ]
    when Names.Ind.CanOrd.equal l lts && Constr.equal lbl r.Rules.lbl ->
    let name c =
      Names.Id.to_string
        (Nametab.basename_of_global (Names.GlobRef.ConstructRef c))
    in
    (* the positions where [s] and [p] differ: path, and both subterms *)
    let rec diffs s p path acc =
      if Constr.equal s p
      then acc
      else (
        match fields env s, fields env p with
        | Some (c, ss), Some (c', ps)
          when Names.Construct.CanOrd.equal c c'
               && List.length ss = List.length ps ->
          snd
            (List.fold_left2
               (fun (i, acc) si pi ->
                 i + 1, diffs si pi ((name c, i) :: path) acc)
               (0, acc)
               ss
               ps)
        | _ -> (List.rev path, s, p) :: acc)
    in
    let rel t = match Constr.kind t with Constr.Rel i -> Some i | _ -> None in
    (* the path from [s] to its one occurrence of [Rel x] *)
    let rec find x s path =
      match Constr.kind s with
      | Constr.Rel i when i = x -> [ List.rev path ]
      | _ ->
        (match fields env s with
         | Some (c, ss) ->
           List.concat
             (List.mapi (fun i si -> find x si ((name c, i) :: path)) ss)
         | None -> [])
    in
    let rec rep x x' t =
      match Constr.kind t with
      | Constr.Rel i when i = x -> Constr.mkRel x'
      | _ -> Constr.map (rep x x') t
    in
    (match diffs r.Rules.src p [] [], diffs r.Rules.tgt p' [] [] with
     | [ (pa, s1, p1) ], [ (pb, t1, p1') ] when pa = pb ->
       (match rel p1, rel p1' with
        | Some x, Some x' ->
          (match find x s1 [] with
           | [ inner ] when inner <> [] && Constr.equal (rep x x' s1) t1 ->
             let pr (c, i) = Printf.sprintf "%s.%d" c i in
             Some
               (String.concat "/" (List.map pr pa)
                ^ ":"
                ^ String.concat "/" (List.map pr inner))
           | _ -> None)
        | _ -> None)
     | _ -> None)
  | _ -> None
;;

type kind =
  | Bisim
  | Sim

type report =
  { verdict : bool
  ; states : float * float
  ; pairs : float
  ; labels : int
  ; interpreted : int
  ; projections : int * int
  ; escapes : int * int
  ; summands : int * int
  ; width : int * int
  ; rounds : int * int
  ; iterations : int
  ; nodes : int
  ; seconds : float
  }

let check
      (env : Environ.env)
      (sigma : Evd.evar_map)
      ?(width = 8)
      ?(strong = false)
      (kind : kind)
      (a : system)
      (b : system)
  : report
  =
  let t0 = Unix.gettimeofday () in
  Ldd.init ();
  let tb = tables () in
  let c =
    { env
    ; sigma
    ; rules = Hashtbl.create 4
    ; dtrue =
        UnivGen.constr_of_monomorphic_global
          env
          (Rocqlib.lib_ref "core.bool.true")
    ; dfalse =
        UnivGen.constr_of_monomorphic_global
          env
          (Rocqlib.lib_ref "core.bool.false")
    ; memo = STbl.create 64
    ; depth = 0
    ; interpreted = 0
    }
  in
  let encoder (s : system) w =
    { env
    ; structural = Skeleton.structural_of env sigma s.lts
    ; width = w
    ; tb
    ; lay = layout ()
    }
  in
  let rec run_explore s w =
    match explore c (encoder s w) s with
    | x -> x
    | exception Too_wide n -> run_explore s (max n (2 * w))
  in
  let ea = run_explore a width in
  let eb = run_explore b width in
  (* the silent label: [None]; none at all for strong (bi)similarity *)
  let tau =
    let none = Rocqlib.lib_ref "core.option.None" in
    if strong
    then None
    else
      CTbl.fold
        (fun t i acc ->
          match Constr.kind (fst (Constr.decompose_app t)) with
          | Constr.Construct (c, _)
            when Names.GlobRef.CanOrd.equal (Names.GlobRef.ConstructRef c) none
            ->
            Some i
          | _ -> acc)
        tb.labels
        None
  in
  (* MEBI_COMPOSE=1 (measurement only, doc/compositional.md, step 2):
     the components of each side (from its context rules, per rule chain),
     and the weak game with every move answered by the same component of
     the other side alone: whether local answers suffice *)
  (match Sys.getenv_opt "MEBI_COMPOSE", tau with
   | None, _ | _, None -> ()
   | Some _, Some t ->
     let t0 = Unix.gettimeofday () in
     let comp_of (s : system) =
       let keys =
         List.map
           (fun (r : Rules.rule) ->
             Names.Id.to_string r.Rules.name, context_key env s.lts r)
           (rules_of c s.lts)
       in
       fun (sm : summand) ->
         let rec go = function
           | n :: rest ->
             (match List.assoc_opt n keys with
              | Some (Some k) -> k :: go rest
              | _ -> [])
           | [] -> []
         in
         String.concat " " (go sm.chain)
     in
     (* MEBI_COMPOSE=all: one component, the whole state (a check of this
        code: the verdict must be the usual one) *)
     let one = Sys.getenv_opt "MEBI_COMPOSE" = Some "all" in
     let ca = if one then fun _ -> "*" else comp_of a
     and cb = if one then fun _ -> "*" else comp_of b in
     let comps cof (x : explored) =
       List.sort_uniq compare (List.map cof x.summands)
     in
     let ka = comps ca ea
     and kb = comps cb eb in
     let show ks =
       String.concat
         ", "
         (List.map (fun k -> if k = "" then "(the whole state)" else k) ks)
     in
     Printf.eprintf "[compose] components: %s | %s\n%!" (show ka) (show kb);
     if List.length ka <> List.length kb || List.mem "" ka || List.mem "" kb
     then Printf.eprintf "[compose] not applicable: components do not align\n%!"
     else (
       (* aligned by equal keys if the two lists agree, else by order *)
       let pairs = List.combine ka kb in
       let wa = ea.vwidth in
       let uni = Ldd.product ea.reach ~width:wa eb.reach in
       let rels (x : explored) cof ~offset k l =
         let pre = List.init offset (fun _ -> Ldd.Copy) in
         List.concat_map
           (fun sm ->
             if cof sm = k
             then
               List.filter_map
                 (fun r ->
                   if r.label = l
                   then Some (Ldd.rel_meta (pre @ levels r.rcodes), r.rel)
                   else None)
                 sm.rels
             else [])
           x.summands
       in
       let pre rs s =
         List.fold_left
           (fun acc (meta, rel) ->
             Ldd.union acc (Ldd.relprev s ~rel ~meta ~within:uni))
           (Ldd.empty ())
           rs
       in
       let tau_star rs z =
         let z = Ldd.inter z uni in
         let rec go z fresh =
           if Ldd.is_empty fresh
           then z
           else (
             let f = Ldd.minus (pre rs fresh) z in
             go (Ldd.union z f) f)
         in
         go z z
       in
       let labels (x : explored) cof k =
         List.sort_uniq
           compare
           (List.concat_map
              (fun sm ->
                if cof sm = k then List.map (fun r -> r.label) sm.rels else [])
              x.summands)
       in
       let bad mover mcof moff answ acof aoff pairs z =
         List.fold_left
           (fun acc (k, k') ->
             let tz = tau_star (rels answ acof ~offset:aoff k' t) z in
             List.fold_left
               (fun acc l ->
                 let ok =
                   if l = t
                   then tz
                   else
                     tau_star
                       (rels answ acof ~offset:aoff k' t)
                       (pre (rels answ acof ~offset:aoff k' l) tz)
                 in
                 Ldd.union
                   acc
                   (pre (rels mover mcof ~offset:moff k l) (Ldd.minus uni ok)))
               acc
               (labels mover mcof k))
           (Ldd.empty ())
           pairs
       in
       let iters = ref 0 in
       let rec gfp z =
         incr iters;
         let bd = bad ea ca 0 eb cb wa pairs z in
         let bd =
           match kind with
           | Sim -> bd
           | Bisim ->
             Ldd.union
               bd
               (bad eb cb wa ea ca 0 (List.map (fun (x, y) -> y, x) pairs) z)
         in
         let z' = Ldd.minus z bd in
         if Ldd.equal z z' then z else gfp z'
       in
       let z = gfp uni in
       Printf.eprintf
         "[compose] local answers: the start pair is %s; %.0f pairs (of %.0f), \
          %d iterations (%.1fs)\n\
          %!"
         (if Ldd.mem z (Array.append ea.start_v eb.start_v)
          then "IN the relation"
          else "NOT in the relation")
         (Ldd.count z)
         (Ldd.count uni)
         !iters
         (Unix.gettimeofday () -. t0)));
  let wa = ea.vwidth
  and wb = eb.vwidth in
  (* the pairs' vectors: the answering side's positions first (for
     [Bisim], where both answer, the narrower side's), so that its
     relations, which the game and its certificate apply most, start at
     the first position with no copied prefix; the other side's relations
     copy that prefix *)
  let b_first = match kind with Sim -> true | Bisim -> wb <= wa in
  let off_a = if b_first then wb else 0
  and off_b = if b_first then 0 else wa in
  let uni =
    if b_first
    then Ldd.product eb.reach ~width:wb ea.reach
    else Ldd.product ea.reach ~width:wa eb.reach
  in
  (* every relation of a side, per label, with its meta for pairs; made
     once *)
  let rels_tbl = Hashtbl.create 16 in
  let rels_of (x : explored) ~(offset : int) l : (Ldd.t * Ldd.t) list =
    match Hashtbl.find_opt rels_tbl (offset, l) with
    | Some r -> r
    | None ->
      let r =
        List.concat_map
          (fun sm ->
            List.filter_map
              (fun r ->
                if r.label = l
                then Some (Ldd.rel_meta (meta_levels ~offset r.rcodes), r.rel)
                else None)
              sm.rels)
          x.summands
      in
      Hashtbl.add rels_tbl (offset, l) r;
      r
  in
  (* The sets below are released as soon as they are dead: the game makes
     many large intermediate sets, and Sylvan's table only frees the nodes
     no live value refers to. [x <- f] stands for "replace and release". *)
  let union_into (acc : Ldd.t) (b : Ldd.t) : Ldd.t =
    let u = Ldd.union acc b in
    Ldd.release acc;
    Ldd.release b;
    u
  in
  (* the pairs with an [l]-step of side [x] into [s] *)
  let pre (x : explored) ~offset l (s : Ldd.t) : Ldd.t =
    List.fold_left
      (fun acc (meta, rel) ->
        union_into acc (Ldd.relprev s ~rel ~meta ~within:uni))
      (Ldd.empty ())
      (rels_of x ~offset l)
  in
  (* the pairs from which side [x] reaches [z] by silent steps (zero or
     more), new pairs only applied each round (the answerer's: their
     soundness is used) *)
  let tau_star (x : explored) ~offset (z : Ldd.t) : Ldd.t =
    let z = Ldd.inter z uni in
    match tau with
    | None -> z
    | Some t ->
      let rec go z fresh =
        if Ldd.is_empty fresh
        then (
          Ldd.release fresh;
          z)
        else (
          let p = pre x ~offset t fresh in
          Ldd.release fresh;
          let fresh = Ldd.minus p z in
          Ldd.release p;
          let z' = Ldd.union z fresh in
          Ldd.release z;
          go z' fresh)
      in
      go z (Ldd.union z (Ldd.empty ()))
  in
  let labels_of (x : explored) =
    List.sort_uniq
      compare
      (List.concat_map
         (fun sm -> List.map (fun r -> r.label) sm.rels)
         x.summands)
  in
  (* the pairs of [z] where [mover] has a step [answerer] cannot weakly
     answer into [z]: silent steps, the label, silent steps (a silent step
     is answered by silent steps alone) *)
  let bad mover ~moff answerer ~aoff z =
    let tz = tau_star answerer ~offset:aoff z in
    let bd =
      List.fold_left
        (fun acc l ->
          let ok =
            if Some l = tau
            then tz
            else (
              let p = pre answerer ~offset:aoff l tz in
              let ok = tau_star answerer ~offset:aoff p in
              Ldd.release p;
              ok)
          in
          let not_ok = Ldd.minus uni ok in
          if ok != tz then Ldd.release ok;
          let b = pre mover ~offset:moff l not_ok in
          Ldd.release not_ok;
          union_into acc b)
        (Ldd.empty ())
        (labels_of mover)
    in
    Ldd.release tz;
    bd
  in
  let iterations = ref 0 in
  (* MEBI_CERT_STATS: each iteration's time and relation, to size the
     extra logged iteration (doc/certificate.md, A) *)
  let stats = Sys.getenv_opt "MEBI_CERT_STATS" <> None in
  let rec gfp z =
    incr iterations;
    let t_it = Unix.gettimeofday () in
    Ldd.reclaim ();
    let bd = bad ea ~moff:off_a eb ~aoff:off_b z in
    let bd =
      match kind with
      | Sim -> bd
      | Bisim -> union_into bd (bad eb ~moff:off_b ea ~aoff:off_a z)
    in
    let z' = Ldd.minus z bd in
    if stats
    then
      Printf.eprintf
        "[game] iteration %d: %.3fs; z %.0f pairs (%d nodes), bad %.0f pairs\n\
         %!"
        !iterations
        (Unix.gettimeofday () -. t_it)
        (Ldd.count z)
        (Ldd.nodes z)
        (Ldd.count bd);
    Ldd.release bd;
    if Ldd.equal z z'
    then (
      Ldd.release z';
      z)
    else (
      Ldd.release z;
      gfp z')
  in
  let z = gfp (Ldd.union uni (Ldd.empty ())) in
  (* [proof_log] (doc/certificate.md, A; set by the plugin, from
     [Set MeBi Log]): once the fixpoint has finished, its check, made again
     as images with Sylvan's proof log on (the cache cleared first, so that
     every result is computed again, and so recorded), into the file
     [proof_log]: for each move of the mover's, the image of [z] by its
     relation (complete: every move from a pair of [z] lands in it) is
     within the pairs it can answer, which are images of [z] backwards by
     the answerer's relations (sound: each is a step), closed under its
     silent steps backwards. No universe, no closure of the exploration:
     the check needs neither. Each image is named in the log with the
     property the game's proof uses ([role]: 1 sound, 2 complete), so that
     only that one is proved. *)
  last_plan := None;
  (match !proof_log with
   | None -> ()
   | Some src ->
     let t0 = Unix.gettimeofday () in
     Gc.full_major ();
     let prefix = Proof_log.source_name src in
     if
       Bool.not
         (match src with
          | Proof_log.File path -> Ldd.log_open path
          | Proof_log.Memory -> Ldd.log_open_mem ())
     then Printf.eprintf "[proof log] cannot open %s\n%!" prefix
     else (
       (* the logged run keeps every set (collection is off while the log
          is open), so nothing is released *)
       let codes_string (cs : code array) =
         String.concat
           ""
           (List.map
              (function RW -> "X" | RO -> "O" | WO -> "W" | Copy -> "C")
              (Array.to_list cs))
       in
       (* each relation of the check by its node: label, codes (its
          system's, untrimmed), diagram *)
       let rinfo
         : (int * int * int, int * string * Ldd.t * int * string list) Hashtbl.t
         =
         Hashtbl.create 64
       in
       let inv_tbl = Hashtbl.create 16 in
       let inv_rels_of (x : explored) ~(offset : int) l =
         match Hashtbl.find_opt inv_tbl (offset, l) with
         | Some r -> r
         | None ->
           let r =
             List.concat_map
               (fun sm ->
                 List.filter_map
                   (fun r ->
                     if r.label = l
                     then (
                       match inverse r.rcodes r.rel with
                       | Some inv ->
                         let meta =
                           Ldd.rel_meta (meta_levels ~offset r.rcodes)
                         in
                         Hashtbl.replace
                           rinfo
                           (l, Ldd.index meta, Ldd.index inv)
                           (l, codes_string r.rcodes, inv, sm.sid, sm.chain);
                         Some (meta, inv)
                       | None -> raise Exit)
                     else None)
                   sm.rels)
               x.summands
           in
           Hashtbl.add inv_tbl (offset, l) r;
           r
       in
       let ix = Ldd.index in
       let img ~role s (meta, rel) =
         let r = Ldd.relprod s ~rel ~meta in
         Ldd.log_top_relprod ~role s ~rel ~meta r;
         r
       in
       (* the images of [src] by [rs] (label [l]) and their union from the
          empty set, left to right, with their plan *)
       let images ~role l src rs =
         let results = List.map (img ~role src) rs in
         let acc = ref (Ldd.empty ())
         and unions = ref [] in
         List.iter
           (fun i ->
             let u = Ldd.union !acc i in
             unions := { Plan.acc = ix !acc; arg = ix i; res = ix u } :: !unions;
             acc := u)
           results;
         ( !acc
         , { Plan.src = ix src
           ; rels =
               List.map
                 (fun (m, r) -> { Plan.label = l; meta = ix m; rel = ix r })
                 rs
           ; imgs = List.map ix results
           ; unions = List.rev !unions
           ; res = ix !acc
           } )
       in
       (* the pairs from which the answerer reaches [z0] by silent steps
          (zero or more), new pairs only stepped back each round *)
       let back_tau (x : explored) ~offset z0 =
         match tau with
         | None -> z0, { Plan.cstart = ix z0; rounds = []; final = ix z0 }
         | Some t ->
           let rs = inv_rels_of x ~offset t in
           let rec go z fresh rounds =
             if Ldd.is_empty fresh
             then
               ( z
               , { Plan.cstart = ix z0; rounds = List.rev rounds; final = ix z }
               )
             else (
               let p, pi = images ~role:1 t fresh rs in
               let fresh' = Ldd.minus p z in
               let z' = Ldd.union z fresh' in
               go
                 z'
                 fresh'
                 ({ Plan.fresh = ix fresh
                  ; p = pi
                  ; tz = ix z
                  ; fresh' = ix fresh'
                  ; tz' = ix z'
                  }
                  :: rounds))
           in
           go z0 z0 []
       in
       (* the coverage of [z] by each summand of the mover: every pair of
          [z] whose mover's state has the summand's shape has its read
          projection among those the summand learned (doc/certificate.md,
          "The theorem from the certificate") *)
       let coverage (mover : explored) ~moff =
         List.map
           (fun (sm : summand) ->
             let sh_rel = Ldd.cube [| sm.sid |] in
             let sh_meta = Ldd.rel_meta (meta_levels ~offset:moff [| RO |]) in
             let kc =
               Array.of_list
                 (List.map (fun k -> if k then RO else Copy) sm.keep)
             in
             let d_meta = Ldd.rel_meta (meta_levels ~offset:moff kc) in
             let f = img ~role:2 z (sh_meta, sh_rel) in
             let g = img ~role:1 z (d_meta, sm.seen) in
             let empty = Ldd.is_empty (Ldd.minus f g) in
             ( empty
             , { Plan.sid = sm.sid
               ; keep = sm.keep
               ; chain = sm.chain
               ; srels =
                   List.map
                     (fun r ->
                       ( r.label
                       , codes_string r.rcodes
                       , r.rel
                       , ix (Ldd.rel_meta (meta_levels ~offset:moff r.rcodes)) ))
                     sm.rels
               ; seen = sm.seen
               ; escaped = Bool.not (Ldd.is_empty sm.gseen)
               ; sh = { Plan.label = 0; meta = ix sh_meta; rel = ix sh_rel }
               ; dsee = { Plan.label = 0; meta = ix d_meta; rel = ix sm.seen }
               ; f = ix f
               ; g = ix g
               } ))
           mover.summands
         @
         (* a shape of the layout no summand has: [z] holds no pair of it
            (its image by the shape's filter is empty), logged as a
            summand that learned nothing *)
         Hashtbl.fold
           (fun sid _ acc ->
             if List.exists (fun (sm : summand) -> sm.sid = sid) mover.summands
             then acc
             else (
               let sh_rel = Ldd.cube [| sid |] in
               let sh_meta = Ldd.rel_meta (meta_levels ~offset:moff [| RO |]) in
               let nothing = Ldd.empty () in
               let f = img ~role:2 z (sh_meta, sh_rel) in
               let g = img ~role:1 z (sh_meta, nothing) in
               ( Ldd.is_empty (Ldd.minus f g)
               , { Plan.sid
                 ; keep = true :: List.init (mover.vwidth - 1) (fun _ -> false)
                 ; chain = []
                 ; srels = []
                 ; seen = nothing
                 ; escaped = false
                 ; sh = { Plan.label = 0; meta = ix sh_meta; rel = ix sh_rel }
                 ; dsee =
                     { Plan.label = 0; meta = ix sh_meta; rel = ix nothing }
                 ; f = ix f
                 ; g = ix g
                 } )
               :: acc))
           mover.enc.lay.spos
           []
       in
       let check ~mover_a mover ~moff answerer ~aoff =
         List.iter
           (fun (sm : summand) ->
             List.iter
               (fun (r : rel) ->
                 Hashtbl.replace
                   rinfo
                   ( r.label
                   , Ldd.index
                       (Ldd.rel_meta (meta_levels ~offset:moff r.rcodes))
                   , Ldd.index r.rel )
                   (r.label, codes_string r.rcodes, r.rel, sm.sid, sm.chain))
               sm.rels)
           (mover : explored).summands;
         let cov = coverage mover ~moff in
         let tz, tzc = back_tau answerer ~offset:aoff z in
         let plabels = ref [] in
         let fine =
           List.fold_left
             (fun acc l ->
               let ok, okfrom =
                 if Some l = tau
                 then tz, None
                 else (
                   let p, pi =
                     images ~role:1 l tz (inv_rels_of answerer ~offset:aoff l)
                   in
                   let ok, c = back_tau answerer ~offset:aoff p in
                   ok, Some (pi, c))
               in
               let moves =
                 List.map
                   (fun (meta, rel) ->
                     let i = img ~role:2 z (meta, rel) in
                     let empty = Ldd.is_empty (Ldd.minus i ok) in
                     ( empty
                     , { Plan.mrel =
                           { Plan.label = l; meta = ix meta; rel = ix rel }
                       ; img = ix i
                       ; ok = ix ok
                       } ))
                   (rels_of mover ~offset:moff l)
               in
               plabels
               := { Plan.lab = l
                  ; okset = ix ok
                  ; okfrom
                  ; moves = List.map snd moves
                  }
                  :: !plabels;
               acc && List.for_all fst moves)
             (List.for_all fst cov)
             (labels_of mover)
         in
         ( fine
         , { Plan.mover_a
           ; tzc
           ; plabels = List.rev !plabels
           ; cover = List.map snd cov
           } )
       in
       let psys (x : explored) (s : system) =
         { Plan.pwidth = x.vwidth
         ; pshapes =
             Hashtbl.fold
               (fun sid pos acc ->
                 (sid, Hashtbl.find tb.shape_of sid, pos) :: acc)
               x.enc.lay.spos
               []
         ; pstart = s.start
         ; plts = s.lts
         }
       in
       let fixed =
         match
           let ok1, d1 = check ~mover_a:true ea ~moff:off_a eb ~aoff:off_b in
           match kind with
           | Sim -> ok1, [ d1 ]
           | Bisim ->
             let ok2, d2 = check ~mover_a:false eb ~moff:off_b ea ~aoff:off_a in
             ok1 && ok2, [ d1; d2 ]
         with
         | ok, dirs ->
           last_plan
           := Some
                { Plan.z = ix z
                ; start =
                    (if b_first
                     then Array.append eb.start_v ea.start_v
                     else Array.append ea.start_v eb.start_v)
                ; b_first
                ; widths = (if b_first then wb, wa else wa, wb)
                ; tau
                ; label_terms =
                    Hashtbl.fold (fun i t acc -> (i, t) :: acc) tb.label_of []
                ; dirs
                ; values =
                    Hashtbl.fold (fun i t acc -> (i, t) :: acc) tb.value_of []
                ; sys_a = psys ea a
                ; sys_b = psys eb b
                ; rinfo = Hashtbl.fold (fun i x acc -> (i, x) :: acc) rinfo []
                };
           Some ok
         | exception Exit -> None
       in
       let n =
         match src with
         | Proof_log.File _ -> Ldd.log_close ()
         | Proof_log.Memory -> Ldd.log_close_mem ()
       in
       Printf.eprintf
         "[proof log] %d records; the check as images: %s (%.3fs) (%s)\n%!"
         n
         (match fixed with
          | Some true -> "passes"
          | Some false -> "FAILS"
          | None -> "not made (an answering relation only writes a position)")
         (Unix.gettimeofday () -. t0)
         prefix));
  let start =
    if b_first
    then Array.append eb.start_v ea.start_v
    else Array.append ea.start_v eb.start_v
  in
  let verdict = Ldd.mem z start in
  { verdict
  ; states = ea.states, eb.states
  ; pairs = Ldd.count z
  ; labels = CTbl.length tb.labels
  ; interpreted = c.interpreted
  ; projections = ea.projections, eb.projections
  ; escapes = ea.escapes, eb.escapes
  ; summands = List.length ea.summands, List.length eb.summands
  ; width = wa, wb
  ; rounds = ea.rounds, eb.rounds
  ; iterations = !iterations
  ; nodes = Ldd.nodes z
  ; seconds = Unix.gettimeofday () -. t0
  }
;;

let pr ?(strong = false) (kind : kind) (r : report) : Pp.t =
  let w = if strong then "strongly" else "weakly" in
  let open Pp in
  let a, b = r.states in
  let pa, pb = r.projections in
  let ea, eb = r.escapes in
  let sa, sb = r.summands in
  let wa, wb = r.width in
  let ra, rb = r.rounds in
  v
    0
    (str
       (Printf.sprintf
          "symbolic (learned per summand, Sylvan): %s."
          (match kind, r.verdict with
           | Bisim, true -> w ^ " bisimilar"
           | Bisim, false -> "NOT " ^ w ^ " bisimilar"
           | Sim, true -> w ^ " simulated"
           | Sim, false -> "NOT " ^ w ^ " simulated"))
     ++ fnl ()
     ++ str
          (Printf.sprintf
             "  reachable states: %.0f and %.0f; pairs in the relation: %.0f \
              (%d nodes); labels: %d"
             a
             b
             r.pairs
             r.nodes
             r.labels)
     ++ fnl ()
     ++ str
          (Printf.sprintf
             "  summands: %d and %d; projections learned: %d and %d (escaped: \
              %d and %d); steps interpreted: %d"
             sa
             sb
             pa
             pb
             ea
             eb
             r.interpreted)
     ++ fnl ()
     ++ str
          (Printf.sprintf
             "  vector widths %d and %d; exploration rounds: %d and %d; game \
              iterations: %d; %.2fs"
             wa
             wb
             ra
             rb
             r.iterations
             r.seconds))
;;
