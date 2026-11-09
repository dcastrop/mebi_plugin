(* See the [.mli]. doc/skeleton.md. *)

open Names

(* the shapes are the containers' (container.ml): this module refines
   them by the rules *)
type shape = Container.shape =
  | Slot of int
  | Node of node

and node = Container.node =
  { cons : constructor
  ; univ : UVars.Instance.t
  ; params : Constr.t list
  ; kids : shape list
  }

type origin =
  | Moved of int
  | Computed of int list

type target =
  | TSlot of origin
  | TNode of node_t

and node_t =
  { tcons : constructor
  ; tuniv : UVars.Instance.t
  ; tparams : Constr.t list
  ; tkids : target list
  }

type summand =
  { chain : string list
  ; reads : int list
  ; target : target
  ; lhead : constructor option
  ; dynamic : bool
  }

type shape_info =
  { shape : shape
  ; summands : summand list
  }

type t =
  { lts : inductive
  ; structural : constructor list
  ; never_kept : constructor list
  ; start : shape
  ; values : (int * Constr.t) list
  ; shapes : shape_info list
  ; complete : bool
  }

(* {1 Constructors that some rule keeps in place} *)

let nparams = Container.nparams
let fields = Container.view

let rec at env (t : Constr.t) (path : int list) : Constr.t option =
  match path with
  | [] -> Some t
  | i :: rest ->
    (match fields env t with
     | Some (_, _, _, fs) when i < List.length fs -> at env (List.nth fs i) rest
     | _ -> None)
;;

let rec occurrences env (t : Constr.t) (path : int list) =
  match fields env t with
  | Some (c, _, _, fs) ->
    (c, List.rev path)
    :: List.concat (List.mapi (fun i f -> occurrences env f (i :: path)) fs)
  | None -> []
;;

let mem_c c l = List.exists (Construct.CanOrd.equal c) l

(** [classify env rules]: the constructors some rule keeps at the same
    place in its target (structural), and those occurring in sources that
    no rule keeps. *)
let classify env (rules : Rules.rule list) : constructor list * constructor list
  =
  let seen = ref []
  and kept = ref [] in
  List.iter
    (fun (r : Rules.rule) ->
      List.iter
        (fun (c, path) ->
          if Bool.not (mem_c c !seen) then seen := c :: !seen;
          let k =
            match at env r.Rules.tgt path with
            | Some t' ->
              (match fields env t' with
               | Some (c', _, _, _) -> Construct.CanOrd.equal c c'
               | None -> false)
            | None -> false
          in
          if k && Bool.not (mem_c c !kept) then kept := c :: !kept)
        (occurrences env r.Rules.src []))
    rules;
  ( List.rev !kept
  , List.rev (List.filter (fun c -> Bool.not (mem_c c !kept)) !seen) )
;;

(* {1 Shapes} *)

let slots = Container.slots
let key = Container.key
let renumber = Container.renumber

let rec shape_of_target = function
  | TSlot _ -> Slot 0
  | TNode n ->
    Node
      { cons = n.tcons
      ; univ = n.tuniv
      ; params = n.tparams
      ; kids = List.map shape_of_target n.tkids
      }
;;

let rec target_of_shape = function
  | Slot p -> TSlot (Moved p)
  | Node n ->
    TNode
      { tcons = n.cons
      ; tuniv = n.univ
      ; tparams = n.params
      ; tkids = List.map target_of_shape n.kids
      }
;;

let rec target_reads = function
  | TSlot (Moved p) -> [ p ]
  | TSlot (Computed ps) -> ps
  | TNode n -> List.concat_map target_reads n.tkids
;;

(** the source slots a target's computed slots are computed from: read,
    since computing needs their values *)
let rec computed_reads = function
  | TSlot (Computed ps) -> ps
  | TSlot (Moved _) -> []
  | TNode n -> List.concat_map computed_reads n.tkids
;;

let union a b = List.sort_uniq compare (a @ b)

(* {1 Matching a rule against a shape} *)

type bind =
  | BTarget of target (** a part of the shape, or a premise's target *)
  | BVal of int list (** a value computed from these slots *)

exception No_match

let rels (t : Constr.t) : int list =
  let acc = ref [] in
  let rec go d t =
    match Constr.kind t with
    | Constr.Rel i -> if i > d then acc := (i - d) :: !acc
    | _ -> Constr.iter_with_binders succ go d t
  in
  go 0 t;
  List.sort_uniq compare !acc
;;

let bind_vals b (p : Constr.t) ps =
  List.iter
    (fun i -> if Bool.not (Hashtbl.mem b i) then Hashtbl.replace b i (BVal ps))
    (rels p)
;;

(** [pmatch env structural b reads p s]: binds [p]'s variables against
    [s]. Slots never start with a structural constructor (terms are split
    canonically), so a structural constructor in [p] matches nodes only;
    any other constructor over a slot reads the slot's value. *)
let rec pmatch env structural b reads (p : Constr.t) (s : target) : unit =
  match Constr.kind p with
  | Constr.Rel i ->
    (match Hashtbl.find_opt b i with
     | None -> Hashtbl.replace b i (BTarget s)
     | Some (BTarget s') ->
       (* the same variable twice: equal values, decided at run time *)
       reads := union !reads (union (target_reads s) (target_reads s'))
     | Some (BVal ps) -> reads := union !reads (union ps (target_reads s)))
  | _ ->
    (match fields env p, s with
     | Some (c, _, _, fs), TNode n ->
       if
         Bool.not (Construct.CanOrd.equal c n.tcons)
         || List.length fs <> List.length n.tkids
       then raise No_match;
       List.iter2 (pmatch env structural b reads) fs n.tkids
     | Some (c, _, _, _), TSlot _ when mem_c c structural -> raise No_match
     | _, _ ->
       let ps = target_reads s in
       reads := union !reads ps;
       bind_vals b p ps)
;;

(** [inst env structural b t]: the rule's term [t] under the bindings, as
    a target: structural constructors become nodes; anything else is a
    slot, moved if it is exactly a slot, computed otherwise. *)
let rec inst env structural b (t : Constr.t) : target =
  match Constr.kind t with
  | Constr.Rel i ->
    (match Hashtbl.find_opt b i with
     | Some (BTarget s) -> s
     | Some (BVal ps) -> TSlot (Computed ps)
     | None -> TSlot (Computed []))
  | _ ->
    (match fields env t with
     | Some (c, u, params, fs) when mem_c c structural ->
       TNode
         { tcons = c
         ; tuniv = u
         ; tparams = params
         ; tkids = List.map (inst env structural b) fs
         }
     | _ ->
       TSlot
         (Computed
            (List.concat_map
               (fun i ->
                 match Hashtbl.find_opt b i with
                 | Some (BTarget s) -> target_reads s
                 | Some (BVal ps) -> ps
                 | None -> [])
               (rels t))))
;;

(** whether a target is made of nodes and moved slots only: a part of the
    source shape, or a premise's target over it *)
let rec structured = function
  | TSlot (Moved _) -> true
  | TSlot (Computed _) -> false
  | TNode n -> List.for_all structured n.tkids
;;

(** [field_types env c u params]: constructor [c]'s field types, its
    type's parameters given, in head normal form *)
let field_types env (c : constructor) u (params : Constr.t list)
  : Constr.types list
  =
  let specif = Inductive.lookup_mind_specif env (fst c) in
  let ty =
    Term.prod_applist (Inductive.type_of_constructor (c, u) specif) params
  in
  let ctx, _ = Term.decompose_prod_decls ty in
  List.map
    (fun d -> Reduction.whd_all env (Context.Rel.Declaration.get_type d))
    (List.rev ctx)
;;

(** [single env structural ty]: [ty]'s constructor, if [ty] is an inductive
    with exactly one constructor and it is structural: a value of [ty]
    always has that structure (a pair, a record) *)
let single env structural (ty : Constr.types) =
  let ty = Reduction.whd_all env ty in
  let h, args = Constr.decompose_app_list ty in
  match Constr.kind h with
  | Constr.Ind (ind, u) ->
    let mib, mip = Inductive.lookup_mind_specif env ind in
    if
      Array.length mip.Declarations.mind_consnames = 1
      && List.length args = mib.Declarations.mind_nparams
      && mem_c (ind, 1) structural
    then Some ((ind, 1), u, args)
    else None
  | _ -> None
;;

(** whether a value of [ty] may start with a structural constructor *)
let carries env structural (ty : Constr.types) : bool =
  match Constr.kind (fst (Constr.decompose_app (Reduction.whd_all env ty))) with
  | Constr.Ind (ind, _) ->
    List.exists
      (fun (c' : constructor) -> Ind.CanOrd.equal (fst c') ind)
      structural
  | _ -> false
;;

(** [normalize env structural ty t]: the target [t] of type [ty], with each
    computed value of a single-constructor structural type expanded into
    its node (its structure is certain), and whether a computed value may
    still start with a structural constructor (its shape is known only at
    run time) *)
let rec normalize env structural ?(fuel = 20) (ty : Constr.types) (t : target)
  : target * bool
  =
  match t with
  | TSlot (Moved _) -> t, false
  | TSlot (Computed ps) ->
    (match single env structural ty with
     | Some (c, u, params) when fuel > 0 ->
       let kids =
         List.map
           (fun fty ->
             normalize env structural ~fuel:(fuel - 1) fty (TSlot (Computed ps)))
           (field_types env c u params)
       in
       ( TNode
           { tcons = c; tuniv = u; tparams = params; tkids = List.map fst kids }
       , List.exists snd kids )
     | _ -> t, carries env structural ty)
  | TNode n ->
    let ftys = field_types env n.tcons n.tuniv n.tparams in
    if List.length ftys <> List.length n.tkids
    then t, false
    else (
      let kids =
        List.map2
          (fun fty k -> normalize env structural ~fuel fty k)
          ftys
          n.tkids
      in
      TNode { n with tkids = List.map fst kids }, List.exists snd kids)
;;

(** [state_type env lts]: the [M] of [lts : M -> L -> M -> Prop] *)
let state_type env (lts : inductive) : Constr.types =
  let _, mip = Inductive.lookup_mind_specif env lts in
  match Term.decompose_prod mip.Declarations.mind_user_arity with
  | [ _; _; (_, m) ], _ -> m
  | _ -> Constr.mkProp
;;

(* {1 Summands} *)

(** [summands env lts rules structural s]: every rule chain of [lts] from
    the shape [s]: its target (shape, and where each slot comes from), the
    slots it reads. A premise over [lts] on a structured part of [s]
    recurses; any other premise is a step the explicit route computes. *)
let summands
      env
      (lts : inductive)
      (rules : Rules.rule list)
      structural
      (s : shape)
  : summand list
  =
  let lts_name =
    "<" ^ Id.to_string (Nametab.basename_of_global (GlobRef.IndRef lts)) ^ ">"
  in
  let progress = ref [] in
  let m = state_type env lts in
  let rec go (s : target) : summand list =
    match s with
    | TSlot o ->
      (* an opaque value: its steps are the explicit route's *)
      let ps = target_reads (TSlot o) in
      let t, d = normalize env structural m (TSlot (Computed ps)) in
      [ { chain = [ lts_name ]
        ; reads = ps
        ; target = t
        ; lhead = None
        ; dynamic = d
        }
      ]
    | TNode _ ->
      let k =
        key (shape_of_target s)
        ^ String.concat "," (List.map string_of_int (target_reads s))
      in
      if List.mem k !progress
      then (
        (* a premise steps from the same shape again: computed whole *)
        let ps = target_reads s in
        [ { chain = [ lts_name ]
          ; reads = ps
          ; target = TSlot (Computed ps)
          ; lhead = None
          ; dynamic = true
          }
        ])
      else (
        progress := k :: !progress;
        let r = List.concat_map (fun r -> rule r s) rules in
        progress := List.filter (( <> ) k) !progress;
        r)
  and rule (r : Rules.rule) (s : target) : summand list =
    let b = Hashtbl.create 8 in
    let reads = ref [] in
    match pmatch env structural b reads r.Rules.src s with
    | exception No_match -> []
    | () ->
      let lvars : (int, constructor option) Hashtbl.t = Hashtbl.create 4 in
      let head (t : Constr.t) =
        match Constr.kind t with
        | Constr.Rel i ->
          (match Hashtbl.find_opt lvars i with Some h -> h | None -> None)
        | _ ->
          (match fields env t with Some (c, _, _, _) -> Some c | None -> None)
      in
      let rec prems ps b (acc : summand) : summand list =
        match ps with
        | [] ->
          let tgt, dyn =
            normalize env structural m (inst env structural b r.Rules.tgt)
          in
          let lbl = target_reads (inst env structural b r.Rules.lbl) in
          [ { acc with
              reads = union acc.reads (union lbl (computed_reads tgt))
            ; target = tgt
            ; lhead = head r.Rules.lbl
            ; dynamic = acc.dynamic || dyn
            }
          ]
        | Rules.Decide { prop; _ } :: rest ->
          prems
            rest
            b
            { acc with
              reads =
                union acc.reads (target_reads (inst env structural b prop))
            }
        | Rules.Opaque prop :: rest ->
          (* the explicit route decides it, and may bind variables: what
             depends on them is computed at run time *)
          prems
            rest
            b
            { acc with
              reads =
                union acc.reads (target_reads (inst env structural b prop))
            ; dynamic = true
            }
        | Rules.Step { lts = l'; src; lbl; tgt } :: rest ->
          let isrc = inst env structural b src in
          if Ind.CanOrd.equal l' lts && structured isrc
          then (
            let want =
              match fields env lbl with
              | Some (c, _, _, _) -> Some c
              | None -> None
            in
            List.concat_map
              (fun (sm : summand) ->
                match want, sm.lhead with
                | Some c, Some c' when Bool.not (Construct.CanOrd.equal c c') ->
                  []
                | _ ->
                  (match Constr.kind lbl with
                   | Constr.Rel i -> Hashtbl.replace lvars i sm.lhead
                   | _ -> ());
                  let b' = Hashtbl.copy b in
                  let rd = ref [] in
                  (match pmatch env structural b' rd tgt sm.target with
                   | exception No_match -> []
                   | () ->
                     prems
                       rest
                       b'
                       { acc with
                         chain = acc.chain @ sm.chain
                       ; reads = union acc.reads (union sm.reads !rd)
                       ; dynamic = acc.dynamic || sm.dynamic
                       }))
              (go isrc))
          else (
            let ps = target_reads isrc in
            let b' = Hashtbl.copy b in
            bind_vals b' tgt ps;
            let name =
              "<"
              ^ Id.to_string (Nametab.basename_of_global (GlobRef.IndRef l'))
              ^ ">"
            in
            prems
              rest
              b'
              { acc with
                chain = acc.chain @ [ name ]
              ; reads = union acc.reads ps
              })
      in
      prems
        r.Rules.premises
        b
        { chain = [ Id.to_string r.Rules.name ]
        ; reads = !reads
        ; target = TSlot (Computed [])
        ; lhead = None
        ; dynamic = false
        }
  in
  go (target_of_shape s)
;;

(* {1 The start term, and the reachable shapes} *)

let build env sigma ?(max_shapes = 200) (lts : inductive) (t0 : Constr.t) : t =
  let ltss = Rules.of_lts env sigma lts in
  let rules =
    match
      List.find_opt
        (fun (l : Rules.lts) -> Ind.CanOrd.equal l.Rules.ind lts)
        ltss
    with
    | Some l -> l.Rules.rules
    | None -> []
  in
  let structural, never_kept = classify env rules in
  let t0 =
    EConstr.to_constr
      sigma
      (Reductionops.nf_all env sigma (EConstr.of_constr t0))
  in
  let values = ref [] in
  let next = ref 0 in
  let rec decompose (t : Constr.t) : shape =
    match fields env t with
    | Some (c, u, params, fs) when mem_c c structural ->
      Node { cons = c; univ = u; params; kids = List.map decompose fs }
    | _ ->
      let p = !next in
      incr next;
      values := (p, t) :: !values;
      Slot p
  in
  let start = decompose t0 in
  let seen : (string, shape_info) Hashtbl.t = Hashtbl.create 16 in
  let order = ref [] in
  let queue = Queue.create () in
  Queue.push start queue;
  let complete = ref true in
  while Bool.not (Queue.is_empty queue) do
    let s = renumber (Queue.pop queue) in
    let k = key s in
    if Bool.not (Hashtbl.mem seen k)
    then
      if Hashtbl.length seen >= max_shapes
      then complete := false
      else (
        let sms = summands env lts rules structural s in
        let info = { shape = s; summands = sms } in
        Hashtbl.add seen k info;
        order := info :: !order;
        List.iter
          (fun (sm : summand) -> Queue.push (shape_of_target sm.target) queue)
          sms)
  done;
  { lts
  ; structural
  ; never_kept
  ; start
  ; values = List.rev !values
  ; shapes = List.rev !order
  ; complete = !complete
  }
;;

(* {1 Classifying summands} *)

type kind =
  | Local
  | Structural
  | Dynamic

let kind (src : shape) (sm : summand) : kind =
  if sm.dynamic
  then Dynamic
  else if key (shape_of_target sm.target) = key src
  then Local
  else Structural
;;

(* {1 Reorderings: structural congruence written as silent rules} *)

let is_none (t : Constr.t) : bool =
  match Constr.kind (fst (Constr.decompose_app_list t)) with
  | Constr.Construct (c, _) ->
    GlobRef.CanOrd.equal
      (GlobRef.ConstructRef c)
      (Rocqlib.lib_ref "core.option.None")
  | _ -> false
;;

(* a pattern of constructors over the rule's variables: its variables in
   order, if every leaf is one *)
let rec linear env (t : Constr.t) : int list option =
  match Constr.kind t with
  | Constr.Rel i -> Some [ i ]
  | _ ->
    (match fields env t with
     | Some (_, _, _, fs) ->
       List.fold_left
         (fun acc f ->
           match acc, linear env f with
           | Some a, Some b -> Some (a @ b)
           | _ -> None)
         (Some [])
         fs
     | None -> None)
;;

(* a source and target, up to the names of the variables (numbered in
   order of first occurrence) *)
let pair_key env (a : Constr.t) (b : Constr.t) : string =
  let tbl = Hashtbl.create 8 in
  let rec go t =
    match Constr.kind t with
    | Constr.Rel i ->
      let k =
        match Hashtbl.find_opt tbl i with
        | Some k -> k
        | None ->
          let k = Hashtbl.length tbl in
          Hashtbl.add tbl i k;
          k
      in
      "v" ^ string_of_int k
    | _ ->
      (match fields env t with
       | Some (c, _, _, fs) ->
         Printf.sprintf
           "%s/%d(%s)"
           (MutInd.to_string (fst (fst c)))
           (snd c)
           (String.concat "," (List.map go fs))
       | None -> "?")
  in
  let ka = go a in
  let kb = go b in
  ka ^ "->" ^ kb
;;

(* a rule that only reorders: no premise, silent, its source a structure
   (not a variable) over variables each once, its target the same
   variables each once, and not the same term *)
let reordering env (r : Rules.rule) : bool =
  r.Rules.premises = []
  && is_none r.Rules.lbl
  && (match Constr.kind r.Rules.src with Constr.Rel _ -> false | _ -> true)
  && Bool.not (Constr.equal r.Rules.src r.Rules.tgt)
  &&
  match linear env r.Rules.src, linear env r.Rules.tgt with
  | Some vs, Some vt ->
    List.sort compare vs = List.sort compare vt
    && List.length (List.sort_uniq compare vs) = List.length vs
  | _ -> false
;;

let reorderings env sigma (lts : inductive) : Id.t list =
  let ltss = Rules.of_lts env sigma lts in
  match
    List.find_opt (fun (l : Rules.lts) -> Ind.CanOrd.equal l.Rules.ind lts) ltss
  with
  | None -> []
  | Some l ->
    let rs = List.filter (reordering env) l.Rules.rules in
    let keys =
      List.map (fun (r : Rules.rule) -> pair_key env r.Rules.src r.Rules.tgt) rs
    in
    List.filter_map
      (fun (r : Rules.rule) ->
        if List.mem (pair_key env r.Rules.tgt r.Rules.src) keys
        then Some r.Rules.name
        else None)
      rs
;;

let by_hand : (string, Id.t list) Hashtbl.t = Hashtbl.create 4
let lts_key ((m, i) : inductive) = MutInd.to_string m ^ "/" ^ string_of_int i

let set_congruence (lts : inductive) (names : Id.t list) =
  Hashtbl.replace by_hand (lts_key lts) names
;;

let congruence env sigma (lts : inductive) : Id.t list * bool =
  match Hashtbl.find_opt by_hand (lts_key lts) with
  | Some l -> l, true
  | None -> reorderings env sigma lts, false
;;

(* {1 Printing} *)

(** a summand's writes: the target's slots that are not the same source
    slot in the same place of a target of the same shape *)
let writes (src : shape) (sm : summand) : int =
  let same_shape = key (shape_of_target sm.target) = key src in
  let rec go i t =
    match t with
    | TSlot (Moved p) -> if same_shape && p = i then 0 else 1
    | TSlot (Computed _) -> 1
    | TNode n ->
      fst
        (List.fold_left
           (fun (acc, j) k ->
             acc + go j k, j + List.length (slots (shape_of_target k)))
           (0, i)
           n.tkids)
  in
  go 0 sm.target
;;

let pr env sigma (s : t) : Pp.t =
  let open Pp in
  let pr_c c = Printer.pr_global (GlobRef.ConstructRef c) in
  let rec pr_shape = function
    | Slot p -> str ("P" ^ string_of_int p)
    | Node n ->
      hov
        2
        (pr_c n.cons
         ++
         if n.kids = []
         then mt ()
         else
           str " "
           ++ prlist_with_sep
                spc
                (fun k ->
                  match k with
                  | Node _ -> str "(" ++ pr_shape k ++ str ")"
                  | Slot _ -> pr_shape k)
                n.kids)
  in
  let all =
    List.concat_map
      (fun i -> List.map (fun sm -> i.shape, sm) i.summands)
      s.shapes
  in
  let n_sum = List.length all in
  let same, change =
    List.partition
      (fun (sh, sm) -> key (shape_of_target sm.target) = key sh)
      all
  in
  let count k =
    List.length (List.filter (fun (sh, sm) -> kind sh sm = k) all)
  in
  let dyn = count Dynamic in
  let avg_w l =
    if l = []
    then 0.
    else
      float_of_int (List.fold_left (fun a (sh, sm) -> a + writes sh sm) 0 l)
      /. float_of_int (List.length l)
  in
  let max_slots =
    List.fold_left (fun a i -> max a (List.length (slots i.shape))) 0 s.shapes
  in
  let pr_info (i : shape_info) =
    let k = key i.shape in
    let stay =
      List.filter (fun sm -> key (shape_of_target sm.target) = k) i.summands
    in
    fnl ()
    ++ str "    "
    ++ pr_shape i.shape
    ++ str
         (Printf.sprintf
            "   (%d slots; %d summands, %d keep the shape)"
            (List.length (slots i.shape))
            (List.length i.summands)
            (List.length stay))
  in
  v
    0
    (str "Shapes of the start term, for "
     ++ Printer.pr_global (GlobRef.IndRef s.lts)
     ++ str ":"
     ++ fnl ()
     ++ str "  structural constructors (kept by some rule): "
     ++ prlist_with_sep (fun () -> str ", ") pr_c s.structural
     ++ fnl ()
     ++ str "  never kept: "
     ++ prlist_with_sep (fun () -> str ", ") pr_c s.never_kept
     ++ fnl ()
     ++ str "  start: "
     ++ pr_shape s.start
     ++ prlist
          (fun (p, t) ->
            fnl ()
            ++ str ("    P" ^ string_of_int p ^ " = ")
            ++ Printer.pr_constr_env env sigma t)
          (List.filteri (fun i _ -> i < 12) s.values)
     ++ (if List.length s.values > 12 then fnl () ++ str "    ..." else mt ())
     ++ fnl ()
     ++ str
          (Printf.sprintf
             "  reachable shapes: %d%s"
             (List.length s.shapes)
             (if s.complete then "" else " (bound reached: more exist)"))
     ++ prlist pr_info (List.filteri (fun i _ -> i < 15) s.shapes)
     ++ (if List.length s.shapes > 15 then fnl () ++ str "    ..." else mt ())
     ++ fnl ()
     ++ str
          (Printf.sprintf
             "  summands: %d; %d keep their shape (writing %.1f slots on \
              average), %d change it; %d may create structure at run time \
              (local %d, structural %d, dynamic %d)"
             n_sum
             (List.length same)
             (avg_w same)
             (List.length change)
             dyn
             (count Local)
             (count Structural)
             dyn)
     ++ fnl ()
     ++ str (Printf.sprintf "  largest shape: %d slots" max_slots)
     ++
     let names, hand = congruence env sigma s.lts in
     fnl ()
     ++ str
          (if names = []
           then "  reorderings (structural congruence): none found"
           else
             Printf.sprintf
               "  reorderings (structural congruence%s): %s"
               (if hand then ", given by hand" else "")
               (String.concat ", " (List.map Id.to_string names))))
;;

(* {1 Splitting terms, for the symbolic route} *)

(* See the [.mli]. *)
let structural_of env sigma (lts : inductive) : constructor list =
  let ltss = Rules.of_lts env sigma lts in
  match
    List.find_opt (fun (l : Rules.lts) -> Ind.CanOrd.equal l.Rules.ind lts) ltss
  with
  | Some l -> fst (classify env l.Rules.rules)
  | None -> []
;;

let split = Container.split
let rebuild = Container.rebuild

(* See the [.mli]. *)
let summands_of env sigma (lts : inductive) (s : shape) : summand list =
  let ltss = Rules.of_lts env sigma lts in
  match
    List.find_opt (fun (l : Rules.lts) -> Ind.CanOrd.equal l.Rules.ind lts) ltss
  with
  | Some l ->
    let structural = fst (classify env l.Rules.rules) in
    summands env lts l.Rules.rules structural (renumber s)
  | None -> []
;;

(* See the [.mli]. *)
let rec target_slots (t : target) : origin list =
  match t with
  | TSlot o -> [ o ]
  | TNode n -> List.concat_map target_slots n.tkids
;;

let target_shape = shape_of_target
