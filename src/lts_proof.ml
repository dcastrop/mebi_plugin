(* The proof about a goal's own LTSs (doc/certificate.md, "The theorem
   from the certificate"): each system's encoding of its states as
   vectors, the soundness of the answerer's relations (each tuple a step,
   by its derivation) and the completeness of the mover's (every step a
   tuple, by case analysis on the rules). Written as Gallina text over
   named local definitions and elaborated: the engine's terms are never
   printed; a rule's terms are written structurally, every global by its
   qualified name, every closed term by the name of a local definition.

   What Rocq computes, beyond typing: the conversions a step's type needs
   (a value's number to its term through a trie; the user's own functions
   in a rule's label or target; a decidable side condition's boolean). *)

module P = Learn.Plan

let failf fmt = Printf.ksprintf failwith fmt
let lib s = Rocqlib.lib_ref s

(* a registered global, made once (each use shares it) *)
let c_tbl : (string, Constr.t) Hashtbl.t = Hashtbl.create 64

let c s =
  match Hashtbl.find_opt c_tbl s with
  | Some x -> x
  | None ->
    let x = UnivGen.constr_of_monomorphic_global (Global.env ()) (lib s) in
    Hashtbl.add c_tbl s x;
    x
;;

let app f a = Constr.mkApp (f, a)

(* {1 Names: qualified, as the current scope resolves them} *)

let qual_of (gr : Names.GlobRef.t) : string =
  Libnames.string_of_qualid
    (Nametab.shortest_qualid_of_global Names.Id.Set.empty gr)
;;

let qtbl : (string, string) Hashtbl.t = Hashtbl.create 64

(* [g s]: "@q" for the registered reference [s] *)
let g (s : string) : string =
  match Hashtbl.find_opt qtbl s with
  | Some x -> x
  | None ->
    let x = "@" ^ qual_of (lib s) in
    Hashtbl.add qtbl s x;
    x
;;

let gcons (cn : Names.constructor) =
  "@" ^ qual_of (Names.GlobRef.ConstructRef cn)
;;

let gind (i : Names.inductive) = "@" ^ qual_of (Names.GlobRef.IndRef i)

(* {1 Literals} *)

(* the numerals, made once each (each use shares it) *)
let pos_tbl : (int, Constr.t) Hashtbl.t = Hashtbl.create 256
let n_tbl : (int, Constr.t) Hashtbl.t = Hashtbl.create 256

let rec pos_constr k =
  match Hashtbl.find_opt pos_tbl k with
  | Some x -> x
  | None ->
    let x =
      if k = 1
      then c "num.pos.xH"
      else
        app
          (c (if k land 1 = 1 then "num.pos.xI" else "num.pos.xO"))
          [| pos_constr (k lsr 1) |]
    in
    Hashtbl.add pos_tbl k x;
    x
;;

let n_constr k =
  match Hashtbl.find_opt n_tbl k with
  | Some x -> x
  | None ->
    let x =
      if k = 0 then c "num.N.N0" else app (c "num.N.Npos") [| pos_constr k |]
    in
    Hashtbl.add n_tbl k x;
    x
;;

let rec nat_constr k =
  if k = 0 then c "num.nat.O" else app (c "num.nat.S") [| nat_constr (k - 1) |]
;;

let n_ty () = c "num.N.type"

let list_constr ty xs =
  List.fold_right
    (fun x acc -> app (c "core.list.cons") [| ty; x; acc |])
    xs
    (app (c "core.list.nil") [| ty |])
;;

(* lists of numbers by their contents, each on its tail's term (shared
   suffixes) *)
let nlist_tbl : (int list, Constr.t) Hashtbl.t = Hashtbl.create 1024

let rec nlist_constr (xs : int list) =
  match xs with
  | [] -> app (c "core.list.nil") [| n_ty () |]
  | x :: rest ->
    (match Hashtbl.find_opt nlist_tbl xs with
     | Some t -> t
     | None ->
       let t =
         app (c "core.list.cons") [| n_ty (); n_constr x; nlist_constr rest |]
       in
       Hashtbl.add nlist_tbl xs t;
       t)
;;

let code_constr = function
  | 'X' -> c "mebi.dt.RW"
  | 'O' -> c "mebi.dt.RO"
  | 'W' -> c "mebi.dt.WO"
  | _ -> c "mebi.dt.Copy"
;;

(* a list of codes, made once (each use shares it) *)
let codes_tbl : (string, Constr.t) Hashtbl.t = Hashtbl.create 64

let codes_constr (cs : string) =
  match Hashtbl.find_opt codes_tbl cs with
  | Some x -> x
  | None ->
    let x =
      list_constr
        (c "mebi.dt.code")
        (List.init (String.length cs) (fun i -> code_constr cs.[i]))
    in
    Hashtbl.add codes_tbl cs x;
    x
;;

let lc (xs : Constr.t list) = list_constr (c "mebi.dt.code") xs

let codes_list (cs : string) =
  List.init (String.length cs) (fun i -> code_constr cs.[i])
;;

(* [Shifted n cs (repeat Copy n ++ cs)], by its constructors *)
let shifted_constr n (cs : string) =
  let csl = codes_list cs in
  let rec go k =
    if k = 0
    then app (c "mebi.de.sh_0") [| lc csl |]
    else
      app
        (c "mebi.de.sh_S")
        [| nat_constr (k - 1)
         ; lc csl
         ; lc (List.init (k - 1) (fun _ -> c "mebi.dt.Copy") @ csl)
         ; go (k - 1)
        |]
  in
  go n
;;

(* [Within cs n] *)
let within_constr n (cs : string) =
  let rec go l n =
    match l with
    | [] -> app (c "mebi.de.wi_nil") [| nat_constr n |]
    | x :: rest ->
      if n = 0 then failf "lts proof: codes longer than the system";
      app
        (c "mebi.de.wi_cons")
        [| x; lc rest; nat_constr (n - 1); go rest (n - 1) |]
  in
  go (codes_list cs) n
;;

(* [NoWO cs], [RoOnly cs] *)
let nowo_constr (cs : string) =
  let rec go = function
    | [] -> c "mebi.de.nw_nil"
    | x :: rest ->
      let k =
        match x with
        | 'C' -> "mebi.de.nw_copy"
        | 'O' -> "mebi.de.nw_ro"
        | 'X' -> "mebi.de.nw_rw"
        | _ -> failf "lts proof: an answerer's relation that only writes"
      in
      app (c k) [| lc (List.map code_constr rest); go rest |]
  in
  go (List.init (String.length cs) (fun i -> cs.[i]))
;;

let roonly_constr (cs : string) =
  let rec go = function
    | [] -> c "mebi.de.ro_nil"
    | x :: rest ->
      let k =
        match x with
        | 'C' -> "mebi.de.ro_copy"
        | 'O' -> "mebi.de.ro_ro"
        | _ -> failf "lts proof: a projection that writes"
      in
      app (c k) [| lc (List.map code_constr rest); go rest |]
  in
  go (List.init (String.length cs) (fun i -> cs.[i]))
;;

(* a relation's codes without their trailing copies *)
let trim (cs : string) =
  let n = ref (String.length cs) in
  while !n > 0 && cs.[!n - 1] = 'C' do
    decr n
  done;
  String.sub cs 0 !n
;;

(* {1 The elaboration context} *)

type st =
  { mutable env : Environ.env
  ; mutable named : (string * Constr.t * Constr.types) list (** last first *)
  ; mutable fdefs : (string * Constr.t * Constr.types) list
    (** definitions over the certificate's facts, last first *)
  ; mutable assumed : (string * Constr.types) list
    (** the certificate's facts, as variables *)
  ; terms : (int, (Constr.t * string) list) Hashtbl.t
  ; nums : (int, string) Hashtbl.t
  ; mutable cnt : int
  ; decls : (string, int * Constr.named_declaration * string list) Hashtbl.t
    (** each local name: its order, its declaration, the names it uses *)
  ; mutable effs : Evd.side_effects option
    (** the constants declared so far (as [abstract] declares its
        sub-proofs: a tactic's global declarations do not last); [None]:
        the definitions are [let]s of the term *)
  ; consts : (Names.Id.t, Constr.t) Hashtbl.t
    (** each local name declared: its term (its constant, applied to the
        facts it uses; or, a small one, its body) *)
  ; assumed_at : (Names.Id.t, int * Constr.types) Hashtbl.t
    (** each fact: its order and its type *)
  ; mutable pending : (string * Constr.t * Constr.types * bool) list
    (** the definitions made before the universe constraints are known,
        last first *)
  ; mutable ready : bool (** the universe constraints known *)
  ; mutable upushed : Univ.ContextSet.t
    (** the universe constraints declared with the constants *)
  ; prefix : string (** the lemma's name: the constants' prefix *)
  ; mutable nconst : int
  ; global : bool
    (** the constants global, as a command declares them (else side
        effects of the proof) *)
  ; used_facts : (Names.Id.t, unit) Hashtbl.t
    (** the facts the constants declared are over *)
  }

let new_st ?(constants = true) ?(global = false) (prefix : string) =
  Hashtbl.reset qtbl;
  { env = Global.env ()
  ; named = []
  ; fdefs = []
  ; assumed = []
  ; terms = Hashtbl.create 256
  ; nums = Hashtbl.create 64
  ; cnt = 0
  ; decls = Hashtbl.create 1024
  ; effs = (if constants then Some Evd.empty_side_effects else None)
  ; consts = Hashtbl.create 1024
  ; assumed_at = Hashtbl.create 256
  ; pending = []
  ; ready = false
  ; upushed = Univ.ContextSet.empty
  ; prefix
  ; nconst = 0
  ; global
  ; used_facts = Hashtbl.create 1024
  }
;;

(* the local names a term uses *)
let names_in (ts : Constr.t list) : string list =
  let seen = Hashtbl.create 16 in
  let rec go t =
    match Constr.kind t with
    | Constr.Var id -> Hashtbl.replace seen (Names.Id.to_string id) ()
    | _ -> Constr.iter go t
  in
  List.iter go ts;
  Hashtbl.fold (fun k () acc -> k :: acc) seen []
;;

let record st n (d : Constr.named_declaration) (uses : Constr.t list) =
  Hashtbl.replace st.decls n (Hashtbl.length st.decls, d, names_in uses)
;;

(* the environment for a text: the local definitions it names, and those
   they use, in their order (elaborating in all of them slows every hole
   the elaborator makes) *)
let env_for st (text : string) : Environ.env =
  let found = Hashtbl.create 64 in
  let rec add n =
    if Bool.not (Hashtbl.mem found n)
    then (
      match Hashtbl.find_opt st.decls n with
      | Some (i, d, uses) ->
        Hashtbl.add found n (i, d);
        List.iter add uses
      | None -> ())
  in
  let len = String.length text in
  let is_id c =
    (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9')
    || c = '_'
    || c = '\''
  in
  let i = ref 0 in
  while !i < len - 3 do
    if
      text.[!i] = 'm'
      && text.[!i + 1] = 'b'
      && text.[!i + 2] = '_'
      && (!i = 0 || Bool.not (is_id text.[!i - 1]))
    then (
      let j = ref (!i + 3) in
      while !j < len && is_id text.[!j] do
        incr j
      done;
      add (String.sub text !i (!j - !i));
      i := !j)
    else incr i
  done;
  let ds =
    List.sort
      compare
      (Hashtbl.fold (fun _ (i, d) acc -> (i, d) :: acc) found [])
  in
  List.fold_left
    (fun env (_, d) -> Environ.push_named Environ.ProofVar d env)
    (Global.env ())
    ds
;;

let fresh st base =
  st.cnt <- st.cnt + 1;
  Printf.sprintf "mb_%s%d" base st.cnt
;;

let var n = Constr.mkVar (Names.Id.of_string n)
let arrow a b = Constr.mkProd (Context.anonR, a, Vars.lift 1 b)
let annot n = Context.annotR (Names.Id.of_string n)

(* the universe constraints the proof needs: the elaborations', and a
   probe's (the constants applied to the goal's types) *)
let univs = ref Univ.ContextSet.empty

(* {2 The definitions as constants}

   Each definition is a constant, declared as the tactic [abstract]
   declares its sub-proofs (a side effect of the proof, exported with it
   at [Qed]): a proof is opaque (never unfolded: the elaboration sees only
   its statement), the rest transparent. The kernel checks each once, when
   it is declared; the proof's term only names them. A definition over the
   certificate's facts (variables until the facts are read) is abstracted
   over the facts it uses, and named applied to them. *)

(* [t] with the local names declared replaced by their terms *)
let to_consts st (t : Constr.t) : Constr.t =
  if Hashtbl.length st.consts = 0
  then t
  else (
    let rec go t =
      match Constr.kind t with
      | Constr.Var id ->
        (match Hashtbl.find_opt st.consts id with Some x -> x | None -> t)
      | _ -> Constr.map go t
    in
    go t)
;;

(* the facts the terms use, and those their types use, in their order *)
let facts_in st (ts : Constr.t list) : (Names.Id.t * Constr.types) list =
  let found = Hashtbl.create 16 in
  let rec go t =
    match Constr.kind t with
    | Constr.Var id ->
      if Bool.not (Hashtbl.mem found id)
      then (
        match Hashtbl.find_opt st.assumed_at id with
        | Some (i, ty) ->
          let ty = to_consts st ty in
          Hashtbl.add found id (i, ty);
          go ty
        | None -> ())
    | _ -> Constr.iter go t
  in
  List.iter go ts;
  List.map
    (fun (_, id, ty) -> id, ty)
    (List.sort
       compare
       (Hashtbl.fold (fun id (i, ty) acc -> (i, id, ty) :: acc) found []))
;;

let stats = Sys.getenv_opt "MEBI_CERT_STATS" <> None

(* MEBI_CERT_STATS: the time of a coverage's parts *)
let tm_label = ref 0.
let tm_search = ref 0.
let tm_witness = ref 0.
let tm_group = ref 0.

(* MEBI_CERT_STATS: the kernel's time per kind of constant *)
let kernel_time : (string, int * float) Hashtbl.t = Hashtbl.create 16

(* [budget] less the size of [t], or 0 *)
let rec constr_size (t : Constr.t) (budget : int) : int =
  if budget <= 0
  then 0
  else Constr.fold (fun b t -> constr_size t b) (budget - 1) t
;;

(* [fun x1 .. xk => body] and [forall x1 .. xk, ty] for the facts [fs]
   (in order, each with its type), in one pass over each term *)
let abstract_facts (fs : (Names.Id.t * Constr.types) list) body ty =
  let k = List.length fs in
  if k = 0
  then body, ty
  else (
    let idx = Hashtbl.create (2 * k) in
    List.iteri (fun j (x, _) -> Hashtbl.replace idx x j) fs;
    (* under [j0] of the binders and [depth] more, [Var xj] (j < j0) is
       [Rel (depth + j0 - j)] *)
    let abs j0 t =
      let rec go depth t =
        match Constr.kind t with
        | Constr.Var x ->
          (match Hashtbl.find_opt idx x with
           | Some j when j < j0 -> Constr.mkRel (depth + j0 - j)
           | _ -> t)
        | Constr.Rel i -> if i > depth then Constr.mkRel (i + j0) else t
        | _ -> Constr.map_with_binders succ go depth t
      in
      go 0 t
    in
    let tys = List.mapi (fun j (_, t) -> abs j t) fs in
    let names = List.map (fun (x, _) -> Context.annotR (Names.Name x)) fs in
    let wrap mk inner =
      List.fold_right2 (fun n t acc -> mk (n, t, acc)) names tys inner
    in
    wrap Constr.mkLambda (abs k body), wrap Constr.mkProd (abs k ty))
;;

(* a body this small is written in place of its name *)
let inline_size = 12

let fresh_label st (n : string) : Names.Id.t =
  let ok id =
    Bool.not
      (Global.exists_objlabel id
       || match st.effs with Some e -> Evd.seff_mem_label id e | None -> false)
  in
  let base = st.prefix ^ "_" ^ n in
  let rec find k =
    let id =
      Names.Id.of_string (if k = 0 then base else Printf.sprintf "%s_%d" base k)
    in
    if ok id then id else find (k + 1)
  in
  find 0
;;

let mark fmt =
  Printf.ksprintf (fun m -> if stats then Dd_cert.log_mark ("[qed] " ^ m)) fmt
;;

let declare_now st (n, body, ty, opaque) =
  match st.effs with
  | None -> ()
  | Some effs ->
    let id = Names.Id.of_string n in
    let t0 = Unix.gettimeofday () in
    let body = to_consts st body
    and ty = to_consts st ty in
    let t1 = Unix.gettimeofday () in
    if Bool.not opaque && constr_size body inline_size > 0
    then Hashtbl.replace st.consts id body
    else (
      let fs = facts_in st [ body; ty ] in
      List.iter (fun (x, _) -> Hashtbl.replace st.used_facts x ()) fs;
      let t2 = Unix.gettimeofday () in
      let body, ty = abstract_facts fs body ty in
      let t3 = Unix.gettimeofday () in
      if t3 -. t0 > 1.
      then
        mark
          "%s: named %.1fs, %d facts found %.1fs, abstracted %.1fs"
          n
          (t1 -. t0)
          (List.length fs)
          (t2 -. t1)
          (t3 -. t2);
      let env = Global.env () in
      let secctx =
        if List.is_empty (Environ.named_context env)
        then Names.Id.Set.empty
        else
          Environ.really_needed
            env
            (Names.Id.Set.union
               (Environ.global_vars_set env ty)
               (Environ.global_vars_set env body))
      in
      let decl =
        if opaque
        then
          Safe_typing.OpaqueEff
            { Entries.opaque_entry_body = body
            ; opaque_entry_secctx = secctx
            ; opaque_entry_type = ty
            ; opaque_entry_universes = Entries.Monomorphic_entry
            }
        else
          Safe_typing.DefinitionEff
            { Entries.definition_entry_body = body
            ; definition_entry_secctx = Some secctx
            ; definition_entry_type = Some ty
            ; definition_entry_universes = Entries.Monomorphic_entry
            ; definition_entry_inline_code = false
            }
      in
      let ctx = Univ.ContextSet.diff !univs st.upushed in
      st.upushed <- Univ.ContextSet.union st.upushed ctx;
      let t0 = Unix.gettimeofday () in
      let refused e =
        failf
          "lts proof: the kernel refuses %s: %s"
          n
          (Pp.string_of_ppcmds (CErrors.print e))
      in
      let kn =
        if st.global
        then (
          (* a global constant, as a command declares one: checked now,
             kept once (its universes declared with it) *)
          try
            Declare.declare_constant
              ~name:(fresh_label st n)
              ~kind:
                (if opaque
                 then Decls.(IsProof Lemma)
                 else Decls.(IsDefinition Definition))
              (Declare.DefinitionEntry
                 (Declare.definition_entry
                    ~opaque
                    ~types:ty
                    ~univs:
                      (UState.Monomorphic_entry ctx, UnivNames.empty_binders)
                    body))
          with
          | e when CErrors.noncritical e -> refused e)
        else (
          try
            let kn, effs =
              Evd.push_side_effects (fresh_label st n) decl ctx effs
            in
            st.effs <- Some effs;
            kn
          with
          | e when CErrors.noncritical e -> refused e)
      in
      st.nconst <- st.nconst + 1;
      if Unix.gettimeofday () -. t0 > 1.
      then mark "%s: the kernel's check %.1fs" n (Unix.gettimeofday () -. t0);
      if stats
      then (
        (* the kind: the name without its [mb_] and its number *)
        let k =
          let b = String.sub n 3 (String.length n - 3) in
          let j = ref (String.length b) in
          while !j > 0 && b.[!j - 1] >= '0' && b.[!j - 1] <= '9' do
            decr j
          done;
          String.sub b 0 !j
        in
        let c, t =
          Stdlib.Option.value (Hashtbl.find_opt kernel_time k) ~default:(0, 0.)
        in
        Hashtbl.replace kernel_time k (c + 1, t +. (Unix.gettimeofday () -. t0)));
      Hashtbl.replace
        st.consts
        id
        (Constr.mkApp
           ( Constr.mkConstU (kn, UVars.Instance.empty)
           , Array.of_list (List.map (fun (x, _) -> Constr.mkVar x) fs) )))
;;

(* declared now, or once the universe constraints are known *)
let declare st d =
  if st.ready then declare_now st d else st.pending <- d :: st.pending
;;

(* the universe constraints known: the definitions made so far declared *)
let set_ready st =
  st.ready <- true;
  let ds = List.rev st.pending in
  st.pending <- [];
  List.iter (declare_now st) ds
;;

let push_def st ?(opaque = false) n body ty =
  (* a proof is elaborated over its statement alone *)
  let d =
    if opaque
    then Context.Named.Declaration.LocalAssum (annot n, ty)
    else Context.Named.Declaration.LocalDef (annot n, body, ty)
  in
  st.env <- Environ.push_named Environ.ProofVar d st.env;
  record st n d (if opaque then [ ty ] else [ body; ty ]);
  declare st (n, body, ty, opaque)
;;

let def_named st ?opaque n body ty =
  push_def st ?opaque n body ty;
  if st.effs = None then st.named <- (n, body, ty) :: st.named;
  n
;;

let def st ?opaque base body ty = def_named st ?opaque (fresh st base) body ty

let fdef st ?opaque base body ty =
  let n = fresh st base in
  push_def st ?opaque n body ty;
  if st.effs = None then st.fdefs <- (n, body, ty) :: st.fdefs;
  n
;;

let assume st base ty =
  let n = fresh st base in
  let d = Context.Named.Declaration.LocalAssum (annot n, ty) in
  st.env <- Environ.push_named Environ.ProofVar d st.env;
  record st n d [ ty ];
  st.assumed <- (n, ty) :: st.assumed;
  Hashtbl.replace
    st.assumed_at
    (Names.Id.of_string n)
    (Hashtbl.length st.assumed_at, ty);
  n
;;

let parse (text : string) =
  try Procq.parse_string Procq.Constr.lconstr text with
  | e ->
    failf
      "lts proof: cannot parse %s (%s)"
      (String.sub text 0 (min 600 (String.length text)))
      (Printexc.to_string e)
;;

let report text e =
  let msg = Pp.string_of_ppcmds (CErrors.print e) in
  failf
    "lts proof: %s\nin: %s"
    msg
    (String.sub text 0 (min 3000 (String.length text)))
;;

(* the sort variables elaboration left, settled (each to the least sort) *)
let settle (sigma : Evd.evar_map) (c : EConstr.t) : Constr.t =
  let sigma = Evd.collapse_sort_variables ~only_above_prop:false sigma in
  let sigma = Evd.minimize_universes sigma in
  univs := Univ.ContextSet.union !univs (Evd.universe_context_set sigma);
  if Evarutil.has_undefined_evars sigma c
  then failf "lts proof: a term with holes left";
  EConstr.to_constr sigma c
;;

(* MEBI_CERT_STATS: elaborations, their text and time *)
let as_count = ref 0
let el_n = ref 0
let el_chars = ref 0
let el_time = ref 0.

(* [elab st ?expected text]: [text] elaborated in the context *)
let rec elab st ?expected (text : string) : Constr.t =
  let t0 = Unix.gettimeofday () in
  let r = elab_ st ?expected text in
  incr el_n;
  (match Sys.getenv_opt "MEBI_QED_TEXT" with
   | Some f when String.length text > 0 ->
     let oc = open_out_gen [ Open_append; Open_creat ] 0o644 f in
     output_string oc (text ^ "\n\n");
     close_out oc
   | _ -> ());
  el_chars := !el_chars + String.length text;
  el_time := !el_time +. (Unix.gettimeofday () -. t0);
  r

and elab_ st ?expected (text : string) : Constr.t =
  let env =
    env_for
      st
      (match expected with
       | None -> text
       | Some ty -> text ^ " " ^ String.concat " " (names_in [ ty ]))
  in
  let sigma = Evd.from_env env in
  let expr = parse text in
  let sigma, c =
    try
      match expected with
      | None -> Constrintern.interp_constr_evars env sigma expr
      | Some ty ->
        Constrintern.interp_casted_constr_evars
          env
          sigma
          expr
          (EConstr.of_constr ty)
    with
    | e when CErrors.noncritical e -> report text e
  in
  let sigma =
    try
      Pretyping.solve_remaining_evars
        (Pretyping.default_inference_flags true)
        env
        sigma
    with
    | e when CErrors.noncritical e -> report text e
  in
  settle sigma c
;;

let elab_type st (text : string) : Constr.types =
  let env = env_for st text in
  let sigma = Evd.from_env env in
  let sigma, c =
    try Constrintern.interp_type_evars env sigma (parse text) with
    | e when CErrors.noncritical e -> report text e
  in
  settle sigma c
;;

let type_of st (t : Constr.t) : Constr.types =
  let sigma = Evd.from_env st.env in
  EConstr.Unsafe.to_constr
    (Retyping.get_type_of st.env sigma (EConstr.of_constr t))
;;

(* [term st t]: the name of a local definition of the closed term [t] *)
let term st (t : Constr.t) : string =
  let h = Hashtbl.hash_param 40 200 t in
  let l = Stdlib.Option.value (Hashtbl.find_opt st.terms h) ~default:[] in
  match List.find_opt (fun (u, _) -> Constr.equal u t) l with
  | Some (_, n) -> n
  | None ->
    let n = def st "k" t (type_of st t) in
    Hashtbl.replace st.terms h ((t, n) :: l);
    n
;;

let num st (k : int) : string =
  match Hashtbl.find_opt st.nums k with
  | Some n -> n
  | None ->
    let n = def st "n" (n_constr k) (n_ty ()) in
    Hashtbl.add st.nums k n;
    n
;;

(* {1 Text} *)

let paren s = "(" ^ s ^ ")"
let arg a = if String.length a > 0 && a.[0] = '@' then paren a else a

let apps f args =
  if args = [] then f else paren (String.concat " " (f :: List.map arg args))
;;

let teq a b = apps (g "core.eq.type") [ "_"; a; b ]
let trefl = apps (g "core.eq.refl") [ "_"; "_" ]
let tconj a b = apps (g "core.and.conj") [ "_"; "_"; a; b ]
let tex w p = apps (g "core.ex.intro") [ "_"; "_"; w; p ]
let ttrue () = paren (g "core.True.type")
let tI () = paren (g "core.True.I")
let tfalse () = paren (g "core.False.type")

let tlist (xs : string list) =
  List.fold_right
    (fun x acc -> apps (g "core.list.cons") [ "_"; x; acc ])
    xs
    (apps (g "core.list.nil") [ "_" ])
;;

(* the list pattern of [xs] (each a pattern) *)
let plist (xs : string list) = tlist xs
let tlistN () = apps (g "mebi.de.list") [ g "num.N.type" ]

(* the pattern of a number *)
let npat (k : int) : string =
  let rec pos k =
    if k = 1
    then g "num.pos.xH"
    else
      apps
        (g (if k land 1 = 1 then "num.pos.xI" else "num.pos.xO"))
        [ pos (k lsr 1) ]
  in
  if k = 0 then paren (g "num.N.N0") else apps (g "num.N.Npos") [ pos k ]
;;

(* [refute h pat]: [h : X = Y], [pat] matches [X] but not [Y] *)
let refute h pat =
  apps
    (g "mebi.de.False_ind")
    [ "_"
    ; Printf.sprintf
        "(match %s in %s return (match mb_y with %s => %s | _ => %s end) with \
         %s => %s end)"
        h
        (apps (g "core.eq.type") [ "_"; "_"; "mb_y" ])
        pat
        (ttrue ())
        (tfalse ())
        (paren (g "core.eq.refl" ^ " _ _"))
        (tI ())
    ]
;;

(* {1 A system's encoding} *)

(* the type of each slot of a shape, from its constructors' fields *)
let slot_types env ?(top : Constr.t option) (sh : Skeleton.shape)
  : (int * Constr.t) list
  =
  let acc = ref [] in
  let rec go (sh : Skeleton.shape) (expected : Constr.t option) =
    match sh with
    | Container.Slot i ->
      (match expected with
       | Some ty -> acc := (i, ty) :: !acc
       | None -> failf "lts proof: a slot of unknown type")
    | Container.Node n ->
      let ty =
        EConstr.Unsafe.to_constr
          (Inductiveops.type_of_constructor
             env
             (n.Container.cons, EConstr.EInstance.make n.Container.univ))
      in
      let ty = Reduction.hnf_prod_applist env ty n.Container.params in
      let decls, _ = Term.decompose_prod_decls ty in
      let fields = List.rev decls in
      if List.length fields <> List.length n.Container.kids
      then failf "lts proof: a shape's fields do not match its constructor";
      List.iteri
        (fun k (d, kid) ->
          let fty = Context.Rel.Declaration.get_type d in
          if Bool.not (Vars.noccur_between 1 k fty)
          then failf "lts proof: a shape with dependent fields";
          go kid (Some (Vars.lift (-k) fty)))
        (List.combine fields n.Container.kids)
  in
  go sh top;
  !acc
;;

type trie =
  | Leaf
  | Node of Constr.t option * trie * trie

let rec insert (t : trie) (k : int) (x : Constr.t) : trie =
  let v, l, r =
    match t with Leaf -> None, Leaf, Leaf | Node (v, l, r) -> v, l, r
  in
  if k = 1
  then Node (Some x, l, r)
  else if k land 1 = 0
  then Node (v, insert l (k lsr 1) x, r)
  else Node (v, l, insert r (k lsr 1) x)
;;

let rec trie_constr (ty : Constr.t) (t : trie) : Constr.t =
  match t with
  | Leaf -> app (c "mebi.de.tleaf") [| ty |]
  | Node (v, l, r) ->
    let o =
      match v with
      | Some x -> app (c "core.option.Some") [| ty; x |]
      | None -> app (c "core.option.None") [| ty |]
    in
    app (c "mebi.de.tnode") [| ty; o; trie_constr ty l; trie_constr ty r |]
;;

(* slot types, shared by the systems: (type, name), names [T], [T_d] (a
   default value), [T_tr] (the values' trie) *)
type gen =
  { st : st
  ; plan : P.t
  ; value_of : (int, Constr.t) Hashtbl.t
  ; mutable stypes : (Constr.t * string) list
  ; nl : int (** the left system's width *)
  ; nl_name : string
  ; mutable svals : (string * (int * Constr.t) list) list
    (** each slot type's values, by number *)
  ; mutable nsd : ((string * Names.constructor) * string) list
    (** per slot type and constructor: no value of the type starts with it *)
  }

let tval (key : string) (u : string) = apps (key ^ "_v") [ u ]

let stype (gn : gen) (ty : Constr.t) : string =
  let st = gn.st in
  let sigma = Evd.from_env st.env in
  match
    List.find_opt
      (fun (t, _) ->
        Reductionops.is_conv
          st.env
          sigma
          (EConstr.of_constr t)
          (EConstr.of_constr ty))
      gn.stypes
  with
  | Some (_, k) -> k
  | None ->
    let key = fresh st "T" in
    let vs =
      List.filter
        (fun (_, v) ->
          match Retyping.get_type_of st.env sigma (EConstr.of_constr v) with
          | vty -> Reductionops.is_conv st.env sigma vty (EConstr.of_constr ty)
          | exception _ -> false)
        (List.sort compare gn.plan.P.values)
    in
    let d =
      match vs with
      | (_, v) :: _ -> v
      | [] -> failf "lts proof: no value of a slot's type"
    in
    ignore (def_named st key ty (type_of st ty));
    ignore (def_named st (key ^ "_d") (var (term st d)) ty);
    let tr =
      List.fold_left (fun t (i, v) -> insert t i (var (term st v))) Leaf vs
    in
    ignore
      (def_named
         st
         (key ^ "_tr")
         (trie_constr (var key) tr)
         (app (c "mebi.de.trie") [| var key |]));
    ignore
      (def_named
         st
         (key ^ "_v")
         (app
            (c "mebi.de.val")
            [| var key; var (key ^ "_d"); var (key ^ "_tr") |])
         (Constr.mkProd (Context.anonR, n_ty (), var key)));
    gn.stypes <- gn.stypes @ [ ty, key ];
    gn.svals <- gn.svals @ [ key, vs ];
    key
;;

type shp =
  { sid : int
  ; shape : Skeleton.shape
  ; pos : int array (** each slot's position *)
  ; keys : string array (** each slot's type *)
  ; fill : string
  ; vec : string (** the shape's vector, a function of its slots' numbers *)
  }

type enc =
  { tag : string
  ; lts : Names.inductive
  ; m : string (** the state type *)
  ; m_c : Constr.t
  ; lty : string (** the label type, [option A] *)
  ; a_c : Constr.t (** [A] *)
  ; e : string (** the encoding [enc Pt] *)
  ; pt : string
  ; width : int
  ; shapes : shp list
  ; ptrie : trie
  ; pnames : (int * string) list (** each shape's predicate *)
  }

let lts_types (ind : Names.inductive) : Constr.t * Constr.t * Constr.t =
  let ty, _ =
    Typeops.type_of_global_in_context (Global.env ()) (Names.GlobRef.IndRef ind)
  in
  match Constr.kind ty with
  | Constr.Prod (_, m, rest) ->
    (match Constr.kind rest with
     | Constr.Prod (_, lt, _) ->
       (match Constr.kind lt with
        | Constr.App (_, [| a |]) -> m, lt, a
        | _ -> failf "lts proof: a label type that is not [option A]")
     | _ ->
       failf "lts proof: an LTS whose type is not M -> option A -> M -> Prop")
  | _ -> failf "lts proof: an LTS whose type is not M -> option A -> M -> Prop"
;;

(* the template of a shape's vector after position 0: [slot j] at its
   position, [zero] elsewhere *)
let tmpl (sh : shp) ~width ~(slot : int -> string) ~(zero : string)
  : string list
  =
  List.init (width - 1) (fun i ->
    let p = i + 1 in
    let j = ref (-1) in
    Array.iteri (fun k q -> if q = p then j := k) sh.pos;
    if !j >= 0 then slot !j else zero)
;;

let encoding (gn : gen) ~(tag : string) (sys : P.psys) : enc =
  let st = gn.st in
  let env = st.env in
  let m_c, lt_c, a_c = lts_types sys.P.plts in
  let m = term st m_c
  and lty = term st lt_c in
  let shapes =
    List.map
      (fun (sid, shape, pos) ->
        let sl = List.sort compare (slot_types env ~top:m_c shape) in
        let keys = Array.of_list (List.map (fun (_, ty) -> stype gn ty) sl) in
        let k = Array.length keys in
        let slot_ty i = snd (List.nth sl i) in
        let body =
          Skeleton.rebuild shape (Array.init k (fun i -> Constr.mkRel (k - i)))
        in
        let f =
          List.fold_left
            (fun acc i -> Constr.mkLambda (Context.anonR, slot_ty i, acc))
            body
            (List.rev (List.init k Fun.id))
        in
        let fty =
          List.fold_left
            (fun acc i -> Constr.mkProd (Context.anonR, slot_ty i, acc))
            m_c
            (List.rev (List.init k Fun.id))
        in
        let fill = def st (tag ^ "fill") f fty in
        (* its vector: [sid], each slot's number at its position, 0 elsewhere *)
        let width = sys.P.pwidth in
        let lN = app (c "core.list.type") [| n_ty () |] in
        let elems =
          List.init width (fun p ->
            if p = 0
            then n_constr sid
            else (
              let j = ref (-1) in
              Array.iteri (fun i q -> if q = p then j := i) pos;
              if !j >= 0 then Constr.mkRel (k - !j) else n_constr 0))
        in
        let vbody = list_constr (n_ty ()) elems in
        let vf =
          List.fold_left
            (fun acc _ -> Constr.mkLambda (Context.anonR, n_ty (), acc))
            vbody
            (List.init k Fun.id)
        in
        let vty =
          List.fold_left
            (fun acc _ -> Constr.mkProd (Context.anonR, n_ty (), acc))
            lN
            (List.init k Fun.id)
        in
        let vec = def st (tag ^ "V") vf vty in
        { sid; shape; pos; keys; fill; vec })
      (List.sort compare sys.P.pshapes)
  in
  let width = sys.P.pwidth in
  let pty =
    arrow (var m) (arrow (app (c "core.list.type") [| n_ty () |]) Constr.mkProp)
  in
  let pnames =
    List.map
      (fun sh ->
        let k = Array.length sh.keys in
        let us = List.init k (fun j -> Printf.sprintf "mb_u%d" j) in
        let t = tmpl sh ~width ~slot:(List.nth us) ~zero:(num st 0) in
        let fillx =
          apps sh.fill (List.mapi (fun j u -> tval sh.keys.(j) u) us)
        in
        let body =
          Printf.sprintf
            "%s /\\ %s"
            (teq "mb_r" (tlist (num st sh.sid :: t |> List.tl)))
            (teq "mb_x" fillx)
        in
        let body =
          if k = 0
          then body
          else
            Printf.sprintf
              "exists %s : %s, %s"
              (String.concat " " us)
              (g "num.N.type")
              body
        in
        let text =
          Printf.sprintf "fun (mb_x : %s) (mb_r : %s) => %s" m (tlistN ()) body
        in
        sh.sid, def st (tag ^ "P") (elab st ~expected:pty text) pty)
      shapes
  in
  let ptrie =
    List.fold_left (fun t (sid, n) -> insert t sid (var n)) Leaf pnames
  in
  let pt =
    def
      st
      (tag ^ "Pt")
      (trie_constr pty ptrie)
      (app (c "mebi.de.trie") [| pty |])
  in
  let e = def st (tag ^ "E") (app (c "mebi.de.enc") [| var m; var pt |]) pty in
  { tag
  ; lts = sys.P.plts
  ; m
  ; m_c
  ; lty
  ; a_c
  ; e
  ; pt
  ; width
  ; shapes
  ; ptrie
  ; pnames
  }
;;

let shp_of (en : enc) sid =
  match List.find_opt (fun s -> s.sid = sid) en.shapes with
  | Some s -> s
  | None -> failf "lts proof: shape %d is not the system's" sid
;;

(* the proof of [TAll Q Pt], [Q p P] = [forall x rest, P x rest -> R x (Npos p :: rest)]: per shape, its entry elaborated against its own
   type; and [Q] *)
let tall_proof (st : st) (en : enc) ~(rname : string) ~(entry : int -> string)
  : Constr.t * Constr.t
  =
  let lN = app (c "core.list.type") [| n_ty () |] in
  let pty = arrow (var en.m) (arrow lN Constr.mkProp) in
  let posty = c "num.pos.type" in
  let qty = arrow posty (arrow pty Constr.mkProp) in
  let qtext =
    Printf.sprintf
      "fun (mb_p : %s) (mb_P : %s -> %s -> Prop) => forall (mb_x : %s) (mb_r : \
       %s), mb_P mb_x mb_r -> %s"
      (paren (g "num.pos.type"))
      en.m
      (tlistN ())
      en.m
      (tlistN ())
      (apps
         rname
         [ "mb_x"
         ; apps
             (g "core.list.cons")
             [ "_"; apps (g "num.N.Npos") [ "mb_p" ]; "mb_r" ]
         ])
  in
  let q0 = elab st ~expected:qty qtext in
  let sigma = Evd.from_env st.env in
  let beta t =
    EConstr.Unsafe.to_constr
      (Reductionops.whd_beta st.env sigma (EConstr.of_constr t))
  in
  let tall q t = app (c "mebi.de.TAll") [| pty; q; trie_constr pty t |] in
  let rec go q t acc d =
    match t with
    | Leaf -> c "core.True.I"
    | Node (v, l, r) ->
      let key = acc + (1 lsl d) in
      let a_ty, a_pf =
        match v with
        | Some x ->
          let ty = beta (app q [| c "num.pos.xH"; x |]) in
          let text = entry key in
          mark "shape %d: its proof's text, %d chars" key (String.length text);
          let pf = elab st ~expected:ty text in
          mark "shape %d: elaborated" key;
          (* each shape's proof a constant of its own *)
          ty, var (def st ~opaque:true "shp" pf ty)
        | None -> c "core.True.type", c "core.True.I"
      in
      let shift k =
        Constr.mkLambda
          ( Context.anonR
          , posty
          , app (Vars.lift 1 q) [| app (c k) [| Constr.mkRel 1 |] |] )
      in
      let ql = shift "num.pos.xO"
      and qr = shift "num.pos.xI" in
      let b_ty = app (c "core.and.type") [| tall ql l; tall qr r |] in
      app
        (c "core.and.conj")
        [| a_ty
         ; b_ty
         ; a_pf
         ; app
             (c "core.and.conj")
             [| tall ql l
              ; tall qr r
              ; go ql l acc (d + 1)
              ; go qr r (acc + (1 lsl d)) (d + 1)
             |]
        |]
  in
  go q0 en.ptrie 0 0, q0
;;

(* [LenL E nL]'s proof *)
let lenl (gn : gen) (en : enc) : string =
  let st = gn.st in
  let entry sid =
    let sh = shp_of en sid in
    let k = Array.length sh.keys in
    let inner =
      apps
        (g "mebi.de.f_equal")
        [ "_"
        ; "_"
        ; Printf.sprintf
            "(fun mb_l => %s)"
            (apps
               (g "mebi.de.length")
               [ "_"; apps (g "core.list.cons") [ "_"; num st sid; "mb_l" ] ])
        ; "_"
        ; "_"
        ; "mb_e"
        ]
    in
    let pat =
      List.fold_right
        (fun j acc ->
          apps (g "core.ex.intro") [ "_"; "_"; Printf.sprintf "mb_u%d" j; acc ])
        (List.init k Fun.id)
        (apps (g "core.and.conj") [ "_"; "_"; "mb_e"; "_" ])
    in
    Printf.sprintf
      "(fun mb_x mb_r mb_h => match mb_h with %s => %s end)"
      pat
      inner
  in
  let lN = app (c "core.list.type") [| n_ty () |] in
  let pty = arrow (var en.m) (arrow lN Constr.mkProp) in
  let rname =
    def
      st
      (en.tag ^ "lenR")
      (elab
         st
         ~expected:pty
         (Printf.sprintf
            "(fun mb_x mb_v => %s)"
            (teq (apps (g "mebi.de.length") [ "_"; "mb_v" ]) gn.nl_name)))
      pty
  in
  let tall, _ = tall_proof st en ~rname ~entry in
  let ty = app (c "mebi.dg.LenL") [| var en.m; var en.e; var gn.nl_name |] in
  def
    st
    ~opaque:true
    (en.tag ^ "len")
    (app (c "mebi.de.enc_ind") [| var en.m; var en.pt; var rname; tall |])
    ty
;;

(* [E x v]'s proof, [x] the start, [v] its vector *)
let start_proof (gn : gen) (en : enc) (x : Constr.t) (v : int array)
  : string * Constr.t
  =
  let st = gn.st in
  let sh = shp_of en v.(0) in
  let k = Array.length sh.keys in
  let body =
    List.fold_right
      (fun j acc -> tex (num st v.(sh.pos.(j))) acc)
      (List.init k Fun.id)
      (tconj trefl trefl)
  in
  let vec = nlist_constr (Array.to_list v) in
  let ty = app (var en.e) [| var (term st x); vec |] in
  def st ~opaque:true (en.tag ^ "start") (elab st ~expected:ty body) ty, vec
;;

(* {1 Rules, per LTS} *)

type rinfo =
  { rule : Rules.rule
  ; ridx : int (** its constructor's index, from 1 *)
  ; n : int (** binders *)
  ; prem : bool array (** each binder (outermost first): a premise? *)
  ; vars : int array (** the variables' binders, in order *)
  }

let rules_tbl : (Names.inductive, rinfo list) Hashtbl.t = Hashtbl.create 8

let rules_of (ind : Names.inductive) : rinfo list =
  match Hashtbl.find_opt rules_tbl ind with
  | Some r -> r
  | None ->
    let env = Global.env () in
    let sigma = Evd.from_env env in
    let all = Rules.of_lts env sigma ind in
    List.iter
      (fun (l : Rules.lts) ->
        let mib, oib = Inductive.lookup_mind_specif env l.Rules.ind in
        ignore mib;
        let names = oib.Declarations.mind_consnames in
        let infos =
          List.map
            (fun (r : Rules.rule) ->
              let ridx =
                let k = ref (-1) in
                Array.iteri
                  (fun i n -> if Names.Id.equal n r.Rules.name then k := i + 1)
                  names;
                if !k < 0
                then
                  failf
                    "lts proof: rule %s not found"
                    (Names.Id.to_string r.Rules.name);
                !k
              in
              let binders = List.rev r.Rules.ctx in
              let n = List.length binders in
              let prem = Array.make n false in
              let _ =
                List.fold_left
                  (fun (i, env_i) d ->
                    let t = Context.Rel.Declaration.get_type d in
                    prem.(i)
                    <- Rocq_utils.is_prop env_i sigma (EConstr.of_constr t);
                    i + 1, Environ.push_rel d env_i)
                  (0, env)
                  binders
              in
              let vars =
                Array.of_list
                  (List.filter
                     (fun i -> Bool.not prem.(i))
                     (List.init n Fun.id))
              in
              { rule = r; ridx; n; prem; vars })
            l.Rules.rules
        in
        Hashtbl.replace rules_tbl l.Rules.ind infos)
      all;
    (match Hashtbl.find_opt rules_tbl ind with
     | Some r -> r
     | None -> failf "lts proof: no rules")
;;

(* {1 Terms with unknowns}

   An unknown is a variable, by number: a rule's variable, a premise's
   label or target, a goal's label or target, a slot's number. *)

module IM = Map.Make (Int)

type pt =
  | K of Constr.t (** a closed term, in normal form *)
  | Num of int (** a number *)
  | U of int * string (** a slot: its number's variable, its type *)
  | Pat of pat (** a rule's term, its variables by number *)
  | Var of int
  | Cn of Names.constructor * Constr.t list * pt list
  (** a constructor: its parameters (closed), its arguments *)
  | Tm of Constr.t * pt array
  (** a term with holes ([Meta (hole_base + j)]), the [j]th hole [args.(j)]:
      a step of a step lemma over holes, at its arguments *)

and pat =
  { body : Constr.t (** over the rule's binders *)
  ; inst : int array (** each binder's variable (premises: -1) *)
  }

type sigma = pt IM.t

let hole_base = 900_000

(* [t]'s holes replaced *)
let fill_holes (f : int -> Constr.t) (t : Constr.t) : Constr.t =
  let rec go t =
    match Constr.kind t with
    | Constr.Meta j when j >= hole_base -> f (j - hole_base)
    | _ -> Constr.map go t
  in
  go t
;;

let has_holes (t : Constr.t) : bool =
  let r = ref false in
  let rec go t =
    match Constr.kind t with
    | Constr.Meta j when j >= hole_base -> r := true
    | _ -> Constr.iter go t
  in
  go t;
  !r
;;

type vars =
  { names : (int, string) Hashtbl.t
  ; types : (int, string) Hashtbl.t
  ; mutable next : int
  }

let vars = { names = Hashtbl.create 256; types = Hashtbl.create 256; next = 0 }

let new_var ?(name = "mb_v") (ty : string) : int =
  vars.next <- vars.next + 1;
  let id = vars.next in
  Hashtbl.replace vars.names id (Printf.sprintf "%s%d" name id);
  Hashtbl.replace vars.types id ty;
  id
;;

let vname id = Hashtbl.find vars.names id

(* normal forms of the labels met so far (they recur across projections),
   for the engine's own comparisons: no proof uses them *)
module CTbl = Hashtbl.Make (struct
    type t = Constr.t

    let equal = Constr.equal
    let hash t = Hashtbl.hash_param 40 200 t
  end)

let nf_tbl : Constr.t CTbl.t = CTbl.create 1024

let nf_all (t : Constr.t) : Constr.t =
  match CTbl.find_opt nf_tbl t with
  | Some x -> x
  | None ->
    let env = Global.env () in
    let x =
      EConstr.Unsafe.to_constr
        (Reductionops.nf_all env Evd.empty (EConstr.of_constr t))
    in
    CTbl.add nf_tbl t x;
    x
;;

let nf (t : Constr.t) : Constr.t = nf_all t

let fields (t : Constr.t)
  : (Names.constructor * Constr.t list * Constr.t list) option
  =
  let h, args = Constr.decompose_app_list t in
  match Constr.kind h with
  | Constr.Construct (((ind, _) as cn), _) ->
    let mib, _ = Inductive.lookup_mind_specif (Global.env ()) ind in
    let np = mib.Declarations.mind_nparams in
    if List.length args >= np
    then
      Some
        ( cn
        , List.filteri (fun i _ -> i < np) args
        , List.filteri (fun i _ -> i >= np) args )
    else None
  | _ -> None
;;

let value_of (gn : gen) k =
  match Hashtbl.find_opt gn.value_of k with
  | Some v -> v
  | None -> failf "lts proof: value %d is not the engine's" k
;;

(* normal form, placeholders kept *)
let nf_meta (t : Constr.t) : Constr.t =
  EConstr.Unsafe.to_constr
    (Reductionops.nf_all (Global.env ()) Evd.empty (EConstr.of_constr t))
;;

(* [a] and [b] convertible (placeholders rigid): the engine's comparison
   of a target with a tuple's, lazily (no normal form is built) *)
let conv (a : Constr.t) (b : Constr.t) : bool =
  Constr.equal a b
  ||
  try
    Reductionops.is_conv
      (Global.env ())
      Evd.empty
      (EConstr.of_constr a)
      (EConstr.of_constr b)
  with
  | e when CErrors.noncritical e -> false
;;

(* the closed term of [x], if it is closed *)
let rec closed_of (gn : gen) (s : sigma) (x : pt) : Constr.t option =
  match x with
  | K t -> Some t
  | Num k -> Some (n_constr k)
  | U (u, _) ->
    (match IM.find_opt u s with
     | Some (Num k) -> Some (value_of gn k)
     | _ -> None)
  | Var v ->
    (match IM.find_opt v s with Some y -> closed_of gn s y | None -> None)
  | Cn (cn, ps, args) ->
    (try
       Some
         (app
            (Constr.mkConstructU (cn, UVars.Instance.empty))
            (Array.of_list
               (ps
                @ List.map
                    (fun a ->
                      match closed_of gn s a with
                      | Some t -> t
                      | None -> raise Exit)
                    args)))
     with
     | Exit -> None)
  | Tm (t, args) ->
    (try
       Some
         (nf
            (fill_holes
               (fun j ->
                 match closed_of gn s args.(j) with
                 | Some c -> c
                 | None -> raise Exit)
               t))
     with
     | Exit -> None)
  | Pat p ->
    let n = Array.length p.inst in
    (try
       let sub =
         List.init n (fun d0 ->
           let i = n - 1 - d0 in
           if p.inst.(i) < 0 || Vars.noccurn (d0 + 1) p.body
           then Constr.mkProp
           else (
             match closed_of gn s (Var p.inst.(i)) with
             | Some t -> t
             | None -> raise Exit))
       in
       let t = Vars.substl sub p.body in
       Some (nf t)
     with
     | Exit -> None)
;;

type view =
  | VC of Names.constructor * Constr.t list * pt list
  | VClosed of Constr.t
  | VSym

let view_closed (t : Constr.t) : view =
  match fields t with
  | Some (cn, ps, args) -> VC (cn, ps, List.map (fun a -> K a) args)
  | None -> VClosed t
;;

let rec view (gn : gen) (s : sigma) (x : pt) : view =
  match x with
  | K t -> view_closed t
  | Num k -> view_closed (n_constr k)
  | U (u, _) ->
    (match IM.find_opt u s with
     | Some (Num k) -> view_closed (value_of gn k)
     | _ -> VSym)
  | Var v -> (match IM.find_opt v s with Some y -> view gn s y | None -> VSym)
  | Cn (cn, ps, args) -> VC (cn, ps, args)
  | Tm (t, args) ->
    (match Constr.kind t with
     | Constr.Meta j when j >= hole_base -> view gn s args.(j - hole_base)
     | _ ->
       (match fields t with
        | Some (cn, ps, xs)
          when List.for_all (fun p -> Bool.not (has_holes p)) ps ->
          VC (cn, ps, List.map (fun a -> Tm (a, args)) xs)
        | _ ->
          (match closed_of gn s x with Some c -> view_closed c | None -> VSym)))
  | Pat p ->
    (match Constr.kind p.body with
     | Constr.Rel d ->
       let i = Array.length p.inst - d in
       view gn s (Var p.inst.(i))
     | _ ->
       (match fields p.body with
        | Some (cn, ps, args) when List.for_all Vars.closed0 ps ->
          VC (cn, ps, List.map (fun a -> Pat { p with body = a }) args)
        | _ ->
          (match closed_of gn s x with Some t -> view_closed t | None -> VSym)))
;;

(* {2 Text of terms with unknowns} *)

let rec ptext (gn : gen) (s : sigma) (x : pt) : string =
  match x with
  | K t -> term gn.st t
  | Num k -> num gn.st k
  | U (u, key) -> tval key (vtext gn s u)
  | Var v -> vtext gn s v
  | Cn (cn, ps, args) ->
    apps (gcons cn) (List.map (term gn.st) ps @ List.map (ptext gn s) args)
  | Pat p -> ttext gn s p p.body
  | Tm (t, args) -> ctext gn s args t

(* a term with holes, structurally *)
and ctext gn s (args : pt array) (t : Constr.t) : string =
  if Bool.not (has_holes t)
  then term gn.st t
  else (
    match Constr.kind t with
    | Constr.Meta j -> ptext gn s args.(j - hole_base)
    | Constr.App (h, xs) ->
      let hs =
        match Constr.kind h with
        | Constr.Construct (cn, _) -> gcons cn
        | Constr.Ind (i, _) -> gind i
        | Constr.Const (k, _) -> "@" ^ qual_of (Names.GlobRef.ConstRef k)
        | _ -> failf "lts proof: a step with a head that is not a global"
      in
      apps hs (List.map (ctext gn s args) (Array.to_list xs))
    | _ -> failf "lts proof: a step the proof cannot write")

and vtext gn s v =
  match IM.find_opt v s with Some x -> ptext gn s x | None -> vname v

(* a rule's term, structurally: globals qualified, closed parts named *)
and ttext gn s (p : pat) (t : Constr.t) : string =
  let n = Array.length p.inst in
  if Vars.closed0 t
  then term gn.st t
  else (
    match Constr.kind t with
    | Constr.Rel d ->
      let i = n - d in
      if p.inst.(i) < 0 then failf "lts proof: a premise used as a term";
      vtext gn s p.inst.(i)
    | Constr.App (h, args) ->
      let hs =
        match Constr.kind h with
        | Constr.Construct (cn, _) -> gcons cn
        | Constr.Ind (i, _) -> gind i
        | Constr.Const (k, _) -> "@" ^ qual_of (Names.GlobRef.ConstRef k)
        | _ -> failf "lts proof: a rule's term with a head that is not a global"
      in
      apps hs (List.map (ttext gn s p) (Array.to_list args))
    | _ -> failf "lts proof: a rule's term the proof cannot write")
;;

(* {2 Matching a rule's term against a known term, statically} *)

type mres =
  | MOk
  | MClash
  | MStuck

let both a b =
  match a, b with
  | MClash, _ | _, MClash -> MClash
  | MStuck, _ | _, MStuck -> MStuck
  | MOk, MOk -> MOk
;;

let eq_closed a b = Constr.equal (nf a) (nf b)

(* [pmatch gn s p t x binds]: the rule's term [t] (of [p]) against [x];
   [binds] gets the unbound variables of [t] (first occurrences), each
   with the part of [x] it stands for *)
(* [Some leaves] if [t] is pairs all the way down with distinct unbound
   variables at the leaves: each variable with its projection of a hole *)
let pair_leaves (p : pat) (s : sigma) (binds : (int * pt) list) (t : Constr.t)
  : (int * Constr.t) list option
  =
  let n = Array.length p.inst in
  let pair =
    Names.GlobRef.ConstructRef
      (fst (Constr.destConstruct (c "core.prod.intro")))
  in
  let is_pair cn =
    Names.GlobRef.CanOrd.equal (Names.GlobRef.ConstructRef cn) pair
  in
  let rec go t (proj : Constr.t -> Constr.t) acc =
    match Constr.kind t with
    | Constr.Rel d ->
      let v = p.inst.(n - d) in
      if v < 0 || IM.mem v s || List.mem_assoc v binds || List.mem_assoc v acc
      then raise Exit
      else (v, proj (Constr.mkMeta hole_base)) :: acc
    | _ ->
      (match fields t with
       | Some (cn, [ a; b ], [ l; r ])
         when is_pair cn && Vars.closed0 a && Vars.closed0 b ->
         let acc =
           go l (fun h -> proj (app (c "core.prod.proj1") [| a; b; h |])) acc
         in
         go r (fun h -> proj (app (c "core.prod.proj2") [| a; b; h |])) acc
       | _ -> raise Exit)
  in
  match Constr.kind t with
  | Constr.Rel _ -> None
  | _ -> (try Some (go t (fun h -> h) []) with Exit -> None)
;;

let rec resolve_slot gn (s : sigma) (x : pt) : (int * string) option =
  match x with
  | U (u, key) -> if IM.mem u s then None else Some (u, key)
  | Var v ->
    (match IM.find_opt v s with Some y -> resolve_slot gn s y | None -> None)
  | Pat { body; inst } when Constr.isRel body ->
    resolve_slot gn s (Var inst.(Array.length inst - Constr.destRel body))
  | Tm (t, args) when Constr.isMeta t ->
    resolve_slot gn s args.(Constr.destMeta t - hole_base)
  | _ -> None

and pmatch
      ?stk
      ?sites
      gn
      (s : sigma)
      (p : pat)
      (t : Constr.t)
      (x : pt)
      (binds : (int * pt) list ref)
  : mres
  =
  let n = Array.length p.inst in
  match Constr.kind t with
  | Constr.Rel d ->
    let vid = p.inst.(n - d) in
    (match IM.find_opt vid s, List.assoc_opt vid !binds with
     | None, None ->
       binds := (vid, x) :: !binds;
       MOk
     | Some y, _ | None, Some y ->
       (match closed_of gn s y, closed_of gn s x with
        | Some a, Some b -> if eq_closed a b then MOk else MClash
        | _ -> MStuck))
  | _ ->
    (match fields t with
     | Some (cn, _, args) ->
       (match view gn s x with
        | VC (cn', _, xs) ->
          if Bool.not (Names.Construct.CanOrd.equal cn cn')
          then MClash
          else if List.length args <> List.length xs
          then MStuck
          else
            List.fold_left2
              (fun acc a y -> both acc (pmatch ?stk ?sites gn s p a y binds))
              MOk
              args
              xs
        | VSym ->
          (match pair_leaves p s !binds t with
           | Some leaves ->
             (* pairs all the way down, unbound variables at the leaves:
                each variable is a projection of [x] *)
             List.iter
               (fun (v, path) -> binds := (v, Tm (path, [| x |])) :: !binds)
               leaves;
             (match sites with
              | Some r -> r := (t, x, leaves) :: !r
              | None -> ());
             MOk
           | None ->
             (match stk, resolve_slot gn s x with
              | Some r, Some (u, key) -> r := (u, key, cn) :: !r
              | _ -> ());
             MStuck)
        | VClosed _ -> MStuck)
     | None ->
       (match closed_of gn s (Pat { p with body = t }), closed_of gn s x with
        | Some a, Some b -> if eq_closed a b then MOk else MClash
        | _ -> MStuck))
;;

(* a term holding a part of the state's shape (a container): its steps
   by the rules; a term a rule builds from slots alone is a step the
   explicit route takes, once the slots are known *)
let rec shape_part gn (s : sigma) (x : pt) : bool =
  match x with
  | Cn _ -> true
  | Var v ->
    (match IM.find_opt v s with Some y -> shape_part gn s y | None -> false)
  | Pat p ->
    let n = Array.length p.inst in
    let rec go t =
      match Constr.kind t with
      | Constr.Rel d ->
        p.inst.(n - d) >= 0 && shape_part gn s (Var p.inst.(n - d))
      | _ ->
        (match fields t with
         | Some (_, _, args) -> List.exists go args
         | None -> false)
    in
    go p.body
  | Tm (t, args) ->
    let rec go t =
      match Constr.kind t with
      | Constr.Meta j when j >= hole_base ->
        shape_part gn s args.(j - hole_base)
      | _ ->
        (match fields t with
         | Some (_, _, xs) -> List.exists go xs
         | None -> false)
    in
    go t
  | _ -> false
;;

(* {2 Slots never start with a structural constructor} *)

let head_cons t =
  match fields t with Some (cn, _, _) -> Some cn | None -> None
;;

(* no value of slot type [key] starts with [cn] *)
let no_head gn key cn =
  match List.assoc_opt key gn.svals with
  | Some vs ->
    List.for_all
      (fun (_, v) ->
        match head_cons v with
        | Some c' -> Bool.not (Names.Construct.CanOrd.equal c' cn)
        | None -> false)
      vs
  | None -> false
;;

(* [forall n, Q (key_v n)], [Q z] = [z] does not start with [cn] *)
let ns_def (gn : gen) key cn : string =
  match
    List.find_opt
      (fun ((k, c'), _) -> k = key && Names.Construct.CanOrd.equal c' cn)
      gn.nsd
  with
  | Some (_, n) -> n
  | None ->
    let st = gn.st in
    let nargs = Inductiveops.constructor_nallargs (Global.env ()) cn in
    let qtext =
      Printf.sprintf
        "fun (mb_z : %s) => match mb_z with %s => %s | _ => %s end"
        key
        (apps (gcons cn) (List.init nargs (fun _ -> "_")))
        (tfalse ())
        (ttrue ())
    in
    let q = elab st ~expected:(arrow (var key) Constr.mkProp) qtext in
    let vs = List.assoc key gn.svals in
    let tr =
      List.fold_left (fun t (i, v) -> insert t i (var (term st v))) Leaf vs
    in
    let q' = Constr.mkLambda (Context.anonR, c "num.pos.type", q) in
    let tallty t =
      app (c "mebi.de.TAll") [| var key; q'; trie_constr (var key) t |]
    in
    let rec tallv t =
      match t with
      | Leaf -> c "core.True.I"
      | Node (v, l, r) ->
        let a_ty =
          match v with Some x -> app q [| x |] | None -> c "core.True.type"
        in
        let b_ty = app (c "core.and.type") [| tallty l; tallty r |] in
        app
          (c "core.and.conj")
          [| a_ty
           ; b_ty
           ; c "core.True.I"
           ; app (c "core.and.conj") [| tallty l; tallty r; tallv l; tallv r |]
          |]
    in
    let body =
      app
        (c "mebi.de.val_all")
        [| var key
         ; var (key ^ "_d")
         ; var (key ^ "_tr")
         ; q
         ; c "core.True.I"
         ; tallv tr
        |]
    in
    let ty =
      Constr.mkProd
        ( Context.anonR
        , n_ty ()
        , app q [| app (var (key ^ "_v")) [| Constr.mkRel 1 |] |] )
    in
    let n = def st ~opaque:true "ns" body ty in
    gn.nsd <- ((key, cn), n) :: gn.nsd;
    n
;;

(* a pattern that needs a slot to start with a constructor no value of
   its type starts with *)
let refutable gn (s : sigma) (p : pat) (x : pt)
  : (int * string * Names.constructor) option
  =
  let stk = ref [] in
  match pmatch ~stk gn s p p.body x (ref []) with
  | MStuck -> List.find_opt (fun (_, key, cn) -> no_head gn key cn) !stk
  | _ -> None
;;

(* the pattern of [t] down to that slot, with its constructor there *)
let refpat gn (s : sigma) (p : pat) (u : int) (cn : Names.constructor) (x : pt)
  : string
  =
  let rec go t x =
    match fields t with
    | Some (c1, ps, args) ->
      (match view gn s x with
       | VC (c2, _, xs)
         when Names.Construct.CanOrd.equal c1 c2
              && List.length args = List.length xs ->
         apps (gcons c1) (List.map (fun _ -> "_") ps @ List.map2 go args xs)
       | VSym ->
         (match resolve_slot gn s x with
          | Some (u', _) when u' = u && Names.Construct.CanOrd.equal c1 cn ->
            apps (gcons c1) (List.map (fun _ -> "_") (ps @ args))
          | _ -> "_")
       | _ -> "_")
    | None -> "_"
  in
  go p.body x
;;

(* a pattern no term of its type fails: one constructor all the way *)
let rec irrefutable (t : Constr.t) : bool =
  match Constr.kind t with
  | Constr.Rel _ -> true
  | _ ->
    (match fields t with
     | Some (((ind, _) as _cn), _, args) ->
       let _, oib = Inductive.lookup_mind_specif (Global.env ()) ind in
       Array.length oib.Declarations.mind_consnames = 1
       && List.for_all irrefutable args
     | None -> true)
;;

(* the pattern of [t] for a small inversion: [pv vid] the pattern
   variable of each variable [binds] holds, [_] elsewhere *)
let pattext
      ?(site = fun _ -> None)
      (p : pat)
      (pv : int -> string option)
      (t : Constr.t)
  : string
  =
  let n = Array.length p.inst in
  let used = Hashtbl.create 8 in
  let rec go t =
    match site t with
    | Some w -> w
    | None ->
      (match Constr.kind t with
       | Constr.Rel d ->
         let v = p.inst.(n - d) in
         (match pv v with
          | Some x when Bool.not (Hashtbl.mem used v) ->
            Hashtbl.add used v ();
            x
          | _ -> "_")
       | _ ->
         (match fields t with
          | Some (cn, ps, args) ->
            apps (gcons cn) (List.map (fun _ -> "_") ps @ List.map go args)
          | None -> "_"))
  in
  go t
;;

(* the pattern of [x]'s structure where [t] has a constructor, down to
   where they differ ([MClash]) *)
let rec clashtext gn (s : sigma) (p : pat) (t : Constr.t) (x : pt) : string =
  let n = Array.length p.inst in
  let of_views (y : pt) =
    (* both known: [x]'s structure where it differs from [y] *)
    let rec go (a : view) (b : view) =
      match a, b with
      | VC (ca, psa, xa), VC (cb, _, xb) ->
        if Bool.not (Names.Construct.CanOrd.equal ca cb)
        then
          apps
            (gcons ca)
            (List.map (fun _ -> "_") psa @ List.map (fun _ -> "_") xa)
        else
          apps
            (gcons ca)
            (List.map (fun _ -> "_") psa
             @ List.map2 (fun u v -> go (view gn s u) (view gn s v)) xa xb)
      | _ -> "_"
    in
    go (view gn s x) (view gn s y)
  in
  match Constr.kind t with
  | Constr.Rel d ->
    let vid = p.inst.(n - d) in
    (match IM.find_opt vid s with Some y -> of_views y | None -> "_")
  | _ ->
    (match fields t, view gn s x with
     | Some (cn, _, args), VC (cn', ps', xs) ->
       if Bool.not (Names.Construct.CanOrd.equal cn cn')
       then
         apps
           (gcons cn')
           (List.map (fun _ -> "_") ps' @ List.map (fun _ -> "_") xs)
       else
         apps
           (gcons cn')
           (List.map (fun _ -> "_") ps'
            @ List.map2 (fun a y -> clashtext gn s p a y) args xs)
     | None, _ ->
       (match closed_of gn s (Pat { p with body = t }) with
        | Some y -> of_views (K y)
        | None -> "_")
     | _ -> "_")
;;

(* {1 The engine: a proof of a goal from hypotheses, by small inversion}

   Hypotheses in scope by name; each step either uses one (and drops it)
   or rewrites the others (they are abstracted again, under the same
   names). *)

type hkind =
  | HState of pt * pat (** [x = pattern] *)
  | HEq of int * pat (** [v = pattern] *)
  | HStep of Names.inductive * pt * int * int (** [L src a b] *)
  | HDec of pat * pat (** a decidable premise: its proposition, its witness *)

type hyp =
  { hn : string
  ; hk : hkind
  }

let htype gn (s : sigma) (h : hyp) : string =
  match h.hk with
  | HState (x, p) -> teq (ptext gn s x) (ttext gn s p p.body)
  | HEq (v, p) -> teq (vtext gn s v) (ttext gn s p p.body)
  | HStep (ind, x, a, b) ->
    apps (gind ind) [ ptext gn s x; vtext gn s a; vtext gn s b ]
  | HDec (p, _) -> ttext gn s p p.body
;;

type rc =
  { gn : gen
  ; goal : sigma -> string
  ; finish : sigma -> string list -> string
  ; stuck :
      (rc -> sigma -> string list -> int list -> hyp list -> string) option
  ; scope : int list (** the variables in scope, for abstracting *)
  ; extends : string list -> bool
    (** some summand's rules go on after this path: a premise holding part
        of the shape is to be inverted, not stepped as a whole *)
  }

let forall_text gn (s : sigma) (hyps : hyp list) (goal : string) : string =
  List.fold_right
    (fun h acc -> Printf.sprintf "forall (%s : %s), %s" h.hn (htype gn s h) acc)
    hyps
    goal
;;

let names hyps = List.map (fun h -> h.hn) hyps

(* the case lemma's disjunct for rule [k] of [n]: its pattern *)
let case_pattern (k : int) (nr : int) (inner : string) : string =
  let rec wrap i =
    if nr = 1
    then inner
    else if i = k
    then
      if k = nr - 1
      then inner
      else apps (g "mebi.de.or_introl") [ "_"; "_"; inner ]
    else apps (g "mebi.de.or_intror") [ "_"; "_"; wrap (i + 1) ]
  in
  wrap 0
;;

(* {2 Case lemmas: per LTS, its steps by rule} *)

type case =
  { cname : string
  ; rules : rinfo list
  }

let cases : (Names.inductive, case) Hashtbl.t = Hashtbl.create 8

(* a rule's existential variables: its own, then a label and a target per
   step premise *)
let exvars (ri : rinfo) =
  let prems = ri.rule.Rules.premises in
  Array.length ri.vars
  + List.fold_left
      (fun a p -> match p with Rules.Step _ -> a + 2 | _ -> a)
      0
      prems
;;

let rec case_lemma (gn : gen) (ind : Names.inductive) : case =
  match Hashtbl.find_opt cases ind with
  | Some cs -> cs
  | None ->
    let st = gn.st in
    let rs = rules_of ind in
    let m_c, lt_c, _ = lts_types ind in
    let m = term st m_c
    and lty = term st lt_c in
    let nr = List.length rs in
    (* a rule's disjunct, over variable names [vn] (binder -> name) *)
    let disjunct (ri : rinfo) =
      let r = ri.rule in
      let inst =
        Array.init ri.n (fun i -> if ri.prem.(i) then -1 else new_var "_")
      in
      let s = IM.empty in
      let p body = { body; inst } in
      let prem_items, prem_vars =
        List.fold_left
          (fun (items, pv) pr ->
            match pr with
            | Rules.Step { lts; src; lbl; tgt } ->
              let a = new_var "_"
              and b = new_var "_" in
              ( items
                @ [ apps (gind lts) [ ttext gn s (p src) src; vname a; vname b ]
                  ; teq (vname a) (ttext gn s (p lbl) lbl)
                  ; teq (vname b) (ttext gn s (p tgt) tgt)
                  ]
              , pv @ [ a; b ] )
            | Rules.Decide { prop; _ } | Rules.Opaque prop ->
              items @ [ ttext gn s (p prop) prop ], pv)
          ([], [])
          r.Rules.premises
      in
      let items =
        prem_items
        @ [ teq "mb_s" (ttext gn s (p r.Rules.src) r.Rules.src)
          ; teq "mb_l" (ttext gn s (p r.Rules.lbl) r.Rules.lbl)
          ; teq "mb_t" (ttext gn s (p r.Rules.tgt) r.Rules.tgt)
          ]
      in
      let body = String.concat " /\\ " items in
      let evs =
        List.map (fun i -> vname inst.(i)) (Array.to_list ri.vars)
        @ List.map vname prem_vars
      in
      if evs = []
      then paren body
      else Printf.sprintf "(exists %s, %s)" (String.concat " " evs) body
    in
    let ortext =
      if nr = 0 then tfalse () else String.concat " \\/ " (List.map disjunct rs)
    in
    let orty = arrow m_c (arrow lt_c (arrow m_c Constr.mkProp)) in
    let ortext =
      Printf.sprintf
        "fun (mb_s : %s) (mb_l : %s) (mb_t : %s) => %s"
        m
        lty
        m
        ortext
    in
    let orn = def st "or" (elab st ~expected:orty ortext) orty in
    (* the proof: a match on the step *)
    let branch k (ri : rinfo) =
      let r = ri.rule in
      let bn i = Printf.sprintf "mb_c%d" i in
      let pat = apps (gcons (ind, ri.ridx)) (List.init ri.n bn) in
      (* write terms over the binders' names *)
      let bvars =
        Array.init ri.n (fun i -> if ri.prem.(i) then -1 else new_var "_")
      in
      Array.iteri
        (fun i v -> if v >= 0 then Hashtbl.replace vars.names v (bn i))
        bvars;
      let p body = { body; inst = bvars } in
      let s = IM.empty in
      let prem_binders =
        List.filter (fun i -> ri.prem.(i)) (List.init ri.n Fun.id)
      in
      let wits = ref []
      and items = ref [] in
      List.iteri
        (fun j pr ->
          let b = List.nth prem_binders j in
          match pr with
          | Rules.Step { lbl; tgt; _ } ->
            wits := !wits @ [ ttext gn s (p lbl) lbl; ttext gn s (p tgt) tgt ];
            items := !items @ [ bn b; trefl; trefl ]
          | Rules.Decide _ | Rules.Opaque _ -> items := !items @ [ bn b ])
        r.Rules.premises;
      let items = !items @ [ trefl; trefl; trefl ] in
      let rec conjs = function
        | [ x ] -> x
        | x :: rest -> tconj x (conjs rest)
        | [] -> tI ()
      in
      let ws = List.map (fun i -> bn i) (Array.to_list ri.vars) @ !wits in
      let inner = List.fold_right (fun w acc -> tex w acc) ws (conjs items) in
      let rec intro i =
        if nr = 1
        then inner
        else if i = k
        then
          if k = nr - 1
          then inner
          else apps (g "mebi.de.or_introl") [ "_"; "_"; inner ]
        else apps (g "mebi.de.or_intror") [ "_"; "_"; intro (i + 1) ]
      in
      Printf.sprintf "| %s => %s" pat (intro 0)
    in
    let text =
      Printf.sprintf
        "fun (mb_s : %s) (mb_l : %s) (mb_t : %s) (mb_H : %s) => match mb_H in \
         %s return %s with %s end"
        m
        lty
        m
        (apps (gind ind) [ "mb_s"; "mb_l"; "mb_t" ])
        (apps (gind ind) [ "mb_s"; "mb_l"; "mb_t" ])
        (apps orn [ "mb_s"; "mb_l"; "mb_t" ])
        (String.concat " " (List.mapi branch rs))
    in
    let ty =
      elab_type
        st
        (Printf.sprintf
           "forall (mb_s : %s) (mb_l : %s) (mb_t : %s), %s -> %s"
           m
           lty
           m
           (apps (gind ind) [ "mb_s"; "mb_l"; "mb_t" ])
           (apps orn [ "mb_s"; "mb_l"; "mb_t" ]))
    in
    let cname = def st ~opaque:true "case" (elab st ~expected:ty text) ty in
    let cs = { cname; rules = rs } in
    Hashtbl.add cases ind cs;
    cs

(* {2 The engine} *)

and run (rc : rc) (s : sigma) (path : string list) (hyps : hyp list) : string =
  let gn = rc.gn in
  match hyps with
  | [] -> rc.finish s path
  | _ ->
    (* the first hypothesis that can be used without a slot's value; a
       premise's label and target are defined by its step only *)
    let outputs =
      List.concat_map
        (fun h -> match h.hk with HStep (_, _, a, b) -> [ a; b ] | _ -> [])
        hyps
    in
    let matches x p =
      pmatch gn s p p.body x (ref []) <> MStuck || refutable gn s p x <> None
    in
    let ready h =
      match h.hk with
      | HState (x, p) -> matches x p
      | HEq (v, p) ->
        (match IM.find_opt v s with
         | None -> Bool.not (List.mem v outputs)
         | Some x -> matches x p)
      | HStep (ind, x, _, _) ->
        generalize gn s ind x <> None || (shape_part gn s x && rc.extends path)
      | HDec (_, w) -> closed_of gn s (Pat w) <> None
    in
    let rec pick acc = function
      | [] -> None
      | h :: tl ->
        if ready h then Some (h, List.rev_append acc tl) else pick (h :: acc) tl
    in
    (match pick [] hyps with
     | None ->
       (match rc.stuck with
        | Some f -> f rc s path [] hyps
        | None ->
          failf
            "lts proof: hypotheses that need an unknown slot along [%s]: %s"
            (String.concat ";" path)
            (String.concat "; " (List.map (htype gn s) hyps)))
     | Some (h, rest) -> use rc s path h rest)

and use rc s path h rest =
  let gn = rc.gn in
  match h.hk with
  | HState (x, p) -> small_inv rc s path h rest x p
  | HEq (v, p) ->
    (match IM.find_opt v s with
     | None -> define rc s path h rest v p
     | Some x -> small_inv rc s path h rest x p)
  | HStep (ind, x, a, b) ->
    (match generalize gn s ind x with
     | Some _ when Bool.not (shape_part gn s x && rc.extends path) ->
       by_steps rc s path h rest ind x a b
     | _ -> invert rc s path h rest ind x a b)
  | HDec (prop, w) ->
    (match closed_of gn s (Pat w) with
     | Some b when Constr.equal b (c "core.bool.true") -> run rc s path rest
     | Some b when Constr.equal b (c "core.bool.false") ->
       let pc =
         match closed_of gn s (Pat prop) with
         | Some x -> x
         | None -> failf "lts proof: a side condition not closed"
       in
       let env = Global.env () in
       let sigma = Evd.from_env env in
       let goal = EConstr.of_constr (app (c "mebi.de.Decidable") [| pc |]) in
       let sigma', inst = Class_tactics.resolve_one_typeclass env sigma goal in
       let inst = EConstr.to_constr sigma' (Evarutil.nf_evar sigma' inst) in
       apps
         (g "mebi.de.False_ind")
         [ "_"
         ; apps
             (g "mebi.de.Decidable_complete_alt")
             [ term gn.st pc
             ; term gn.st inst
             ; apps (g "core.eq.refl") [ "_"; g "core.bool.false" ]
             ; h.hn
             ]
         ]
     | _ -> failf "lts proof: a side condition the proof cannot decide")

(* the hypotheses whose types a rewriting changes ([s] to [s']): only
   those are abstracted again *)
and dep gn (s : sigma) (s' : sigma) (rest : hyp list) : hyp list =
  List.filter (fun h -> htype gn s h <> htype gn s' h) rest

and lam (hs : hyp list) (body : string) =
  if hs = []
  then body
  else Printf.sprintf "(fun %s => %s)" (String.concat " " (names hs)) body

(* [h : x = pattern], [x] known *)
and small_inv rc s path h rest x p =
  let gn = rc.gn in
  let binds = ref []
  and sites = ref [] in
  match pmatch ~sites gn s p p.body x binds with
  | MClash -> refute h.hn (clashtext gn s p p.body x)
  | MStuck ->
    (match refutable gn s p x with
     | Some (u, key, cn) ->
       apps
         (g "mebi.de.False_ind")
         [ "_"
         ; Printf.sprintf
             "(match %s in %s return (match mb_y with %s => %s | _ => %s end) \
              with %s => %s end)"
             h.hn
             (apps (g "core.eq.type") [ "_"; "_"; "mb_y" ])
             (refpat gn s p u cn x)
             (tfalse ())
             (ttrue ())
             (paren (g "core.eq.refl" ^ " _ _"))
             (apps (ns_def gn key cn) [ vtext gn s u ])
         ]
     | None ->
       failf
         "lts proof: a pattern over an unknown term along [%s]"
         (String.concat ";" path))
  | MOk when !binds = [] -> run rc s path rest
  | MOk when Constr.isRel p.body ->
    (* the pattern is a variable: it is [x] from now on *)
    let v, y = List.hd !binds in
    let yv = new_var ~name:"mb_y" "_" in
    let s_pat = IM.add v (Var yv) s in
    let s_br = IM.add v y s in
    let ds = dep gn s s_pat rest in
    apps
      (Printf.sprintf
         "(match %s in %s return %s with %s => %s end)"
         h.hn
         (apps (g "core.eq.type") [ "_"; "_"; vname yv ])
         (paren (forall_text gn s_pat ds (rc.goal s_pat)))
         (paren (g "core.eq.refl" ^ " _ _"))
         (lam ds (run rc s_br path rest)))
      (names ds)
  | MOk ->
    let binds = List.rev !binds in
    (* a part of [x] matched by pairs only: a pattern variable, the
       pattern's variables its projections *)
    let ws =
      List.map
        (fun (t, _, leaves) -> t, new_var ~name:"mb_w" "_", leaves)
        !sites
    in
    let in_site v =
      List.exists (fun (_, _, leaves) -> List.mem_assoc v leaves) ws
    in
    let direct = List.filter (fun (v, _) -> Bool.not (in_site v)) binds in
    let pvs = List.map (fun (v, _) -> v, new_var ~name:"mb_p" "_") direct in
    let s_pat = List.fold_left (fun s (v, pv) -> IM.add v (Var pv) s) s pvs in
    let s_pat =
      List.fold_left
        (fun s (_, w, leaves) ->
          List.fold_left
            (fun s (v, path) -> IM.add v (Tm (path, [| Var w |])) s)
            s
            leaves)
        s_pat
        ws
    in
    let s_br = List.fold_left (fun s (v, y) -> IM.add v y s) s binds in
    let pv vid = Stdlib.Option.map vname (List.assoc_opt vid pvs) in
    let site t =
      Stdlib.Option.map
        (fun (_, w, _) -> vname w)
        (List.find_opt (fun (t', _, _) -> t' == t) ws)
    in
    let ds = dep gn s s_pat rest in
    apps
      (Printf.sprintf
         "(match %s in %s return (match mb_y with %s => %s%s end) with %s => \
          %s end)"
         h.hn
         (apps (g "core.eq.type") [ "_"; "_"; "mb_y" ])
         (pattext ~site p pv p.body)
         (forall_text gn s_pat ds (rc.goal s_pat))
         (if irrefutable p.body then "" else " | _ => " ^ ttrue ())
         (paren (g "core.eq.refl" ^ " _ _"))
         (lam ds (run rc s_br path rest)))
      (names ds)

(* [h : v = pattern], [v] unknown: [v] is the pattern from now on *)
and define rc s path h rest v p =
  let gn = rc.gn in
  let y = new_var ~name:"mb_d" "_" in
  let s_pat = IM.add v (Var y) s in
  let s_br = IM.add v (Pat p) s in
  let sym = apps (g "mebi.de.eq_sym") [ "_"; "_"; "_"; h.hn ] in
  let ds = dep gn s s_pat rest in
  apps
    (Printf.sprintf
       "(match %s in %s return %s with %s => %s end)"
       sym
       (apps (g "core.eq.type") [ "_"; "_"; vname y ])
       (paren (forall_text gn s_pat ds (rc.goal s_pat)))
       (paren (g "core.eq.refl" ^ " _ _"))
       (lam ds (run rc s_br path rest)))
    (names ds)

(* [h : L src a b], [src] closed: by the steps of [src] *)
and by_steps rc s path h rest ind x a b =
  let gn = rc.gn in
  let skel, args =
    match generalize gn s ind x with
    | Some g -> g
    | None -> failf "lts proof: a premise's source not known"
  in
  (* over holes if it can be built so, else for the term itself *)
  let skel, args =
    if Array.length args = 0 || Bool.not (failed_skel ind skel)
    then skel, args
    else (
      match closed_of gn s (Tm (skel, args)) with
      | Some c -> c, [||]
      | None -> failf "lts proof: a step lemma over holes that cannot be built")
  in
  let asn, steps =
    try as_lemma gn ind skel with
    | Failure _ when Array.length args > 0 ->
      as_failed := (ind, skel) :: !as_failed;
      (match closed_of gn s (Tm (skel, args)) with
       | Some c -> as_lemma gn ind c
       | None -> failf "lts proof: a step lemma over holes that cannot be built")
  in
  let args =
    if has_holes skel && Bool.not (failed_skel ind skel) then args else [||]
  in
  let qa = new_var ~name:"mb_qa" "_"
  and qb = new_var ~name:"mb_qb" "_" in
  let s_q = IM.add a (Var qa) (IM.add b (Var qb) s) in
  let ds = dep gn s s_q rest in
  let q =
    Printf.sprintf
      "(fun %s %s => %s)"
      (vname qa)
      (vname qb)
      (forall_text gn s_q ds (rc.goal s_q))
  in
  let at t = if has_holes t then Tm (t, args) else K (nf t) in
  let cases =
    List.map
      (fun (l, t) ->
        let s' = IM.add a (at l) (IM.add b (at t) s) in
        lam ds (run rc s' path rest))
      steps
  in
  apps
    asn
    (Array.to_list (Array.map (ptext gn s) args)
     @ (q :: cases)
     @ [ vtext gn s a; vtext gn s b; h.hn ]
     @ names ds)

(* [h : L x a b], [x] partly known: by the rules *)
and invert rc s path h rest ind x a b =
  let gn = rc.gn in
  let cs = case_lemma gn ind in
  let nr = List.length cs.rules in
  let branches =
    List.mapi
      (fun k (ri : rinfo) ->
        let r = ri.rule in
        let inst =
          Array.init ri.n (fun i ->
            if ri.prem.(i) then -1 else new_var "mb_x" ~name:"mb_x")
        in
        Array.iteri
          (fun i v ->
            if v >= 0
            then (
              let d = List.nth (List.rev r.Rules.ctx) i in
              let ty = Context.Rel.Declaration.get_type d in
              Hashtbl.replace
                vars.types
                v
                (if Vars.closed0 ty then term gn.st ty else "_")))
          inst;
        let p body = { body; inst } in
        let hyps = ref []
        and evs = ref [] in
        List.iter
          (fun pr ->
            match pr with
            | Rules.Step { lts; src; lbl; tgt } ->
              let _, plt, _ = lts_types lts in
              let pm, _, _ = lts_types lts in
              let a' = new_var ~name:"mb_a" (term gn.st plt)
              and b' = new_var ~name:"mb_b" (term gn.st pm) in
              evs := !evs @ [ a'; b' ];
              let hs = fresh gn.st "h"
              and ea = fresh gn.st "h"
              and eb = fresh gn.st "h" in
              hyps
              := !hyps
                 @ [ { hn = hs; hk = HStep (lts, Pat (p src), a', b') }
                   ; { hn = ea; hk = HEq (a', p lbl) }
                   ; { hn = eb; hk = HEq (b', p tgt) }
                   ]
            | Rules.Decide { prop; witness } ->
              hyps
              := !hyps
                 @ [ { hn = fresh gn.st "h"; hk = HDec (p prop, p witness) } ]
            | Rules.Opaque _ ->
              failf "lts proof: a premise the rules cannot evaluate")
          r.Rules.premises;
        let es = fresh gn.st "h"
        and el = fresh gn.st "h"
        and et = fresh gn.st "h" in
        let hs_state = { hn = es; hk = HState (x, p r.Rules.src) } in
        let hl = { hn = el; hk = HEq (a, p r.Rules.lbl) }
        and ht = { hn = et; hk = HEq (b, p r.Rules.tgt) } in
        let prem_names = List.map (fun h -> h.hn) !hyps in
        let rec conjs = function
          | [ x ] -> x
          | x :: rest -> apps (g "core.and.conj") [ "_"; "_"; x; conjs rest ]
          | [] -> "_"
        in
        let inner = conjs (prem_names @ [ es; el; et ]) in
        let evnames =
          List.map (fun i -> vname inst.(i)) (Array.to_list ri.vars)
          @ List.map vname !evs
        in
        let inner =
          List.fold_right
            (fun w acc -> apps (g "core.ex.intro") [ "_"; "_"; w; acc ])
            evnames
            inner
        in
        let pat = case_pattern k nr inner in
        let rc' =
          { rc with
            scope =
              rc.scope
              @ Array.to_list
                  (Array.of_list
                     (List.filter (fun v -> v >= 0) (Array.to_list inst)))
              @ !evs
          }
        in
        let body =
          run
            rc'
            s
            (path @ [ Names.Id.to_string r.Rules.name ])
            ((hs_state :: !hyps) @ [ hl; ht ] @ rest)
        in
        Printf.sprintf "| %s => %s" pat body)
      cs.rules
  in
  let m_c, lt_c, _ = lts_types ind in
  ignore m_c;
  ignore lt_c;
  let scrut = apps cs.cname [ "_"; "_"; "_"; h.hn ] in
  if nr = 0
  then apps (g "mebi.de.False_ind") [ "_"; scrut ]
  else
    Printf.sprintf "(match %s with %s end)" scrut (String.concat " " branches)

(* {2 The steps of a closed term, as an eliminator}

   [forall Q, Q l1 t1 -> ... -> forall l t, L v l t -> Q l t] *)

and as_tbl
  : ( int
      , (Names.inductive * Constr.t * (string * (Constr.t * Constr.t) list))
          list )
      Hashtbl.t
  =
  Hashtbl.create 1024

and as_busy : (Names.inductive * Constr.t) list ref = ref []

(* the skeletons over holes whose step lemma could not be built (a
   function inspects a hole) *)
and as_failed : (Names.inductive * Constr.t) list ref = ref []

and failed_skel ind skel =
  List.exists
    (fun (i, t) -> Names.Ind.CanOrd.equal i ind && Constr.equal t skel)
    !as_failed

(* [as_lemma gn ind skel]: the steps of the source [skel] (its holes, the
   positions no rule inspects, as variables), and the lemma
   [forall holes Q, Q l1 t1 -> .. -> forall l t, L skel l t -> Q l t],
   each step [(li, ti)] over the holes, collected from the proof itself *)
and as_lemma (gn : gen) (ind : Names.inductive) (skel : Constr.t) =
  let hv = Hashtbl.hash_param 40 200 skel in
  match
    List.find_opt
      (fun (i, t, _) -> Names.Ind.CanOrd.equal i ind && Constr.equal t skel)
      (Stdlib.Option.value (Hashtbl.find_opt as_tbl hv) ~default:[])
  with
  | Some (_, _, x) -> x
  | None ->
    if
      List.exists
        (fun (i, t) -> Names.Ind.CanOrd.equal i ind && Constr.equal t skel)
        !as_busy
    then failf "lts proof: a term whose steps depend on its own";
    as_busy := (ind, skel) :: !as_busy;
    let st = gn.st in
    let m_c, lt_c, _ = lts_types ind in
    let m = term st m_c
    and lty = term st lt_c in
    let htys = hole_types skel in
    let holes = Array.map (fun ty -> new_var ~name:"mb_z" (term st ty)) htys in
    let x = Tm (skel, Array.map (fun v -> Var v) holes) in
    let l = new_var ~name:"mb_l" lty
    and t = new_var ~name:"mb_t" m in
    let steps = ref [] in
    let rc =
      { gn
      ; goal =
          (fun s -> Printf.sprintf "mb_Q %s %s" (vtext gn s l) (vtext gn s t))
      ; finish =
          (fun s _ ->
            let lc = pconstr gn s holes (Var l)
            and tc = pconstr gn s holes (Var t) in
            let rec find i = function
              | [] ->
                steps := !steps @ [ lc, tc, vtext gn s l, vtext gn s t ];
                i
              | (l', t', _, _) :: rest ->
                if Constr.equal l' lc && Constr.equal t' tc
                then i
                else find (i + 1) rest
            in
            Printf.sprintf "mb_h%d" (find 0 !steps))
      ; stuck = None
      ; scope = []
      ; extends = (fun _ -> true)
      }
    in
    let h = { hn = "mb_H"; hk = HStep (ind, x, l, t) } in
    let body =
      try invert rc IM.empty [] h [] ind x l t with
      | e ->
        as_busy := List.tl !as_busy;
        raise e
    in
    let hs = List.mapi (fun i _ -> Printf.sprintf "mb_h%d" i) !steps in
    let zs = Array.to_list (Array.map vname holes) in
    let ty =
      elab_type
        st
        (Printf.sprintf
           "forall %s(mb_Q : %s -> %s -> Prop), %s forall (mb_l : %s) (mb_t : \
            %s), %s -> mb_Q mb_l mb_t"
           (String.concat
              ""
              (List.map2
                 (fun z ty -> Printf.sprintf "(%s : %s) " z (term st ty))
                 zs
                 (Array.to_list htys)))
           lty
           m
           (String.concat
              ""
              (List.map
                 (fun (_, _, lt, tt) -> Printf.sprintf "mb_Q %s %s -> " lt tt)
                 !steps))
           lty
           m
           (apps (gind ind) [ ptext gn IM.empty x; "mb_l"; "mb_t" ]))
    in
    let text =
      Printf.sprintf
        "fun %s mb_Q %s %s %s mb_H => %s"
        (String.concat " " zs)
        (String.concat " " hs)
        (vname l)
        (vname t)
        body
    in
    let n = def st ~opaque:true "as" (elab st ~expected:ty text) ty in
    as_busy := List.tl !as_busy;
    incr as_count;
    if !as_count mod 200 = 0
    then
      mark "%d step lemmas, %d elaborations, %d chars" !as_count !el_n !el_chars;
    let res = n, List.map (fun (l, t, _, _) -> l, t) !steps in
    Hashtbl.replace
      as_tbl
      hv
      ((ind, skel, res)
       :: Stdlib.Option.value (Hashtbl.find_opt as_tbl hv) ~default:[]);
    res

(* a step's source with its positions no rule inspects as holes, and the
   holes' terms; [None] if a position some rule inspects is not known *)
and generalize gn (s : sigma) (ind : Names.inductive) (x : pt)
  : (Constr.t * pt array) option
  =
  let abs = abstractable ind in
  let split cn ps (args : pt list) =
    let holes = ref [] in
    try
      let args' =
        List.mapi
          (fun k a ->
            if
              List.exists
                (fun (c', k') -> k' = k && Names.Construct.CanOrd.equal c' cn)
                abs
            then (
              holes := a :: !holes;
              Constr.mkMeta (hole_base + List.length !holes - 1))
            else (
              match closed_of gn s a with Some c -> c | None -> raise Exit))
          args
      in
      Some
        ( app
            (Constr.mkConstructU (cn, UVars.Instance.empty))
            (Array.of_list (ps @ args'))
        , Array.of_list (List.rev !holes) )
    with
    | Exit -> None
  in
  match view gn s x with
  | VC (cn, ps, args) -> split cn ps args
  | VClosed c -> Some (c, [||])
  | VSym -> None

(* the types of a skeleton's holes: its constructor's fields *)
and hole_types (skel : Constr.t) : Constr.t array =
  match fields skel with
  | Some (cn, ps, args) ->
    let env = Global.env () in
    let ty =
      EConstr.Unsafe.to_constr
        (Inductiveops.type_of_constructor env (cn, EConstr.EInstance.empty))
    in
    let ty = Reduction.hnf_prod_applist env ty ps in
    let decls, _ = Term.decompose_prod_decls ty in
    let fields = List.rev decls in
    let tys =
      List.filter_map
        (fun (k, (a, d)) ->
          match Constr.kind a with
          | Constr.Meta j when j >= hole_base ->
            let fty = Context.Rel.Declaration.get_type d in
            if Bool.not (Vars.noccur_between 1 k fty)
            then failf "lts proof: a hole of a dependent type";
            Some (j - hole_base, Vars.lift (-k) fty)
          | _ -> None)
        (List.mapi (fun k x -> k, x) (List.combine args fields))
    in
    Array.of_list (List.map snd (List.sort compare tys))
  | None -> [||]

(* the term of [x], the step lemma's holes as [Meta]s *)
and pconstr gn (s : sigma) (holes : int array) (x : pt) : Constr.t =
  match x with
  | K t -> t
  | Num k -> n_constr k
  | U (u, _) ->
    (match IM.find_opt u s with
     | Some (Num k) -> value_of gn k
     | _ -> failf "lts proof: a step lemma over a slot")
  | Var v ->
    (match IM.find_opt v s with
     | Some y -> pconstr gn s holes y
     | None ->
       let j = ref (-1) in
       Array.iteri (fun i h -> if h = v then j := i) holes;
       if !j < 0 then failf "lts proof: a step with an unknown part";
       Constr.mkMeta (hole_base + !j))
  | Cn (cn, ps, args) ->
    app
      (Constr.mkConstructU (cn, UVars.Instance.empty))
      (Array.of_list (ps @ List.map (pconstr gn s holes) args))
  | Pat p ->
    let n = Array.length p.inst in
    let sub =
      List.init n (fun d0 ->
        let i = n - 1 - d0 in
        if p.inst.(i) < 0 || Vars.noccurn (d0 + 1) p.body
        then Constr.mkProp
        else pconstr gn s holes (Var p.inst.(i)))
    in
    Vars.substl sub p.body
  | Tm (t, args) -> fill_holes (fun j -> pconstr gn s holes args.(j)) t

(* the positions of an LTS's sources no rule inspects, per constructor:
   every rule with that head has a variable there, used once, and a
   premise's source holds it only at such a position of its own *)
and abs_tbl : (Names.inductive, (Names.constructor * int) list) Hashtbl.t =
  Hashtbl.create 8

and abstractable (ind : Names.inductive) : (Names.constructor * int) list =
  match Hashtbl.find_opt abs_tbl ind with
  | Some x -> x
  | None ->
    ignore (rules_of ind);
    let ltss = Hashtbl.fold (fun i _ acc -> i :: acc) rules_tbl [] in
    let head (t : Constr.t) =
      match fields t with Some (cn, _, args) -> Some (cn, args) | None -> None
    in
    let cand = Hashtbl.create 16 in
    List.iter
      (fun l ->
        let rs = Hashtbl.find rules_tbl l in
        if List.for_all (fun ri -> head ri.rule.Rules.src <> None) rs
        then
          List.iter
            (fun ri ->
              match head ri.rule.Rules.src with
              | Some (cn, args) ->
                List.iteri
                  (fun k _ ->
                    let ok =
                      List.for_all
                        (fun rj ->
                          match head rj.rule.Rules.src with
                          | Some (cn', args')
                            when Names.Construct.CanOrd.equal cn cn' ->
                            let a = List.nth args' k in
                            Constr.isRel a
                            && List.length
                                 (List.filter (fun b -> Constr.equal b a) args')
                               = 1
                            && List.for_all
                                 (fun (j, b) ->
                                   j = k || Vars.noccurn (Constr.destRel a) b)
                                 (List.mapi (fun j b -> j, b) args')
                          | _ -> true)
                        rs
                    in
                    if ok then Hashtbl.replace cand (l, cn, k) ())
                  args
              | None -> ())
            rs)
      ltss;
    let is_cand l cn k =
      Hashtbl.fold
        (fun (l', cn', k') () acc ->
          acc
          || (Names.Ind.CanOrd.equal l l'
              && Names.Construct.CanOrd.equal cn cn'
              && k = k'))
        cand
        false
    in
    let justified lts (src : Constr.t) d =
      Vars.noccurn d src
      ||
      match head src with
      | Some (cn', args) ->
        List.for_all
          (fun (k', a) ->
            if Constr.isRel a && Constr.destRel a = d
            then is_cand lts cn' k'
            else Vars.noccurn d a)
          (List.mapi (fun k' a -> k', a) args)
      | None -> false
    in
    let changed = ref true in
    while !changed do
      changed := false;
      List.iter
        (fun (l, cn, k) ->
          let rs = Hashtbl.find rules_tbl l in
          let bad =
            List.exists
              (fun ri ->
                match head ri.rule.Rules.src with
                | Some (cn', args) when Names.Construct.CanOrd.equal cn cn' ->
                  let d = Constr.destRel (List.nth args k) in
                  List.exists
                    (fun pr ->
                      match pr with
                      | Rules.Step { lts; src; _ } ->
                        Bool.not (justified lts src d)
                      | Rules.Decide { prop; witness } ->
                        Bool.not (Vars.noccurn d prop && Vars.noccurn d witness)
                      | Rules.Opaque t -> Bool.not (Vars.noccurn d t))
                    ri.rule.Rules.premises
                | _ -> false)
              rs
          in
          if bad
          then (
            Hashtbl.remove cand (l, cn, k);
            changed := true))
        (Hashtbl.fold (fun key () acc -> key :: acc) cand [])
    done;
    List.iter
      (fun l ->
        Hashtbl.replace
          abs_tbl
          l
          (Hashtbl.fold
             (fun (l', cn, k) () acc ->
               if Names.Ind.CanOrd.equal l l' then (cn, k) :: acc else acc)
             cand
             []))
      ltss;
    Stdlib.Option.value (Hashtbl.find_opt abs_tbl ind) ~default:[]
;;

(* {1 Dump, for development (MEBI_PLAN_DUMP=1)} *)

let dump env sigma (plan : P.t) =
  let pr t = Pp.string_of_ppcmds (Printer.pr_constr_env env sigma t) in
  let e = Printf.eprintf in
  e
    "[plan] z=%d b_first=%b widths=(%d,%d) tau=%s start=[%s]\n%!"
    plan.P.z
    plan.P.b_first
    (fst plan.P.widths)
    (snd plan.P.widths)
    (match plan.P.tau with Some t -> string_of_int t | None -> "-")
    (String.concat ";" (Array.to_list (Array.map string_of_int plan.P.start)));
  List.iter (fun (i, t) -> e "  label %d = %s\n" i (pr t)) plan.P.label_terms;
  List.iter
    (fun (i, t) -> e "  value %d = %s\n" i (pr t))
    (List.sort compare plan.P.values);
  let sys name (s : P.psys) =
    e "  system %s: width %d, start %s\n" name s.P.pwidth (pr s.P.pstart);
    List.iter
      (fun (sid, sh, pos) ->
        e
          "    shape %d = %s at [%s]\n"
          sid
          (Skeleton.key sh)
          (String.concat ";" (Array.to_list (Array.map string_of_int pos))))
      (List.sort compare s.P.pshapes)
  in
  sys "a" plan.P.sys_a;
  sys "b" plan.P.sys_b;
  List.iter
    (fun ((_, m, i), (l, cs, r, sid, chain)) ->
      e
        "  rel %d (meta %d): label %d codes %s sid %d chain [%s] tuples %s\n"
        i
        m
        l
        cs
        sid
        (String.concat ";" chain)
        (String.concat
           " "
           (List.map
              (fun t ->
                "["
                ^ String.concat ";" (Array.to_list (Array.map string_of_int t))
                ^ "]")
              (Ldd.elements ~limit:50 r))))
    (List.sort compare plan.P.rinfo);
  List.iter
    (fun (d : P.pdir) ->
      e "  dir mover_a=%b\n" d.P.mover_a;
      List.iter
        (fun (s : P.psum) ->
          e
            "    summand sid %d keep [%s] chain [%s] escaped %b seen %s\n"
            s.P.sid
            (String.concat ";" (List.map string_of_bool s.P.keep))
            (String.concat ";" s.P.chain)
            s.P.escaped
            (String.concat
               " "
               (List.map
                  (fun t ->
                    "["
                    ^ String.concat
                        ";"
                        (Array.to_list (Array.map string_of_int t))
                    ^ "]")
                  (Ldd.elements ~limit:50 s.P.seen)));
          List.iter
            (fun (l, cs, r, _) ->
              e "      rel %d label %d codes %s\n" (Ldd.index r) l cs)
            s.P.srels)
        d.P.cover;
      List.iter
        (fun (pl : P.plabel) ->
          e
            "    label %d: moves [%s] answers [%s]\n"
            pl.P.lab
            (String.concat
               ";"
               (List.map
                  (fun (m : P.pmove) -> string_of_int m.P.mrel.P.rel)
                  pl.P.moves))
            (match pl.P.okfrom with
             | Some (pi, _) ->
               String.concat
                 ";"
                 (List.map
                    (fun (r : P.prel) -> string_of_int r.P.rel)
                    pi.P.rels)
             | None -> "-"))
        d.P.plabels)
    plan.P.dirs;
  flush stderr
;;

(* the term of [x], the given number variables' slots as holes (their
   values), unread slots as [Meta u] *)
let rec pconstr_g gn (s : sigma) (cvs : int list) (x : pt) : Constr.t =
  match x with
  | K t -> t
  | Num k -> n_constr k
  | U (u, _) ->
    (match IM.find_opt u s with
     | Some (Num k) -> value_of gn k
     | Some (Var v) when List.mem v cvs ->
       let rec idx i = function
         | [] -> assert false
         | y :: rest -> if y = v then i else idx (i + 1) rest
       in
       Constr.mkMeta (hole_base + idx 0 cvs)
     | None -> Constr.mkMeta u
     | Some y -> pconstr_g gn s cvs y)
  | Var v ->
    (match IM.find_opt v s with
     | Some y -> pconstr_g gn s cvs y
     | None -> failf "lts proof: a step with an unknown part")
  | Cn (cn, ps, args) ->
    app
      (Constr.mkConstructU (cn, UVars.Instance.empty))
      (Array.of_list (ps @ List.map (pconstr_g gn s cvs) args))
  | Pat p ->
    let n = Array.length p.inst in
    let sub =
      List.init n (fun d0 ->
        let i = n - 1 - d0 in
        if p.inst.(i) < 0 || Vars.noccurn (d0 + 1) p.body
        then Constr.mkProp
        else pconstr_g gn s cvs (Var p.inst.(i)))
    in
    Vars.substl sub p.body
  | Tm (t, args) -> fill_holes (fun j -> pconstr_g gn s cvs args.(j)) t
;;

(* {1 Completeness: every step of the mover's is a tuple of its relations} *)

type mover =
  { men : enc
  ; right : bool (** the mover's positions second *)
  ; dir : P.pdir
  ; f_rs : string (** the mover's relations *)
  ; f_z : string
  ; mutable used_rels : ((int * int * int) * (string * string)) list
    (** relation: its diagram, its membership in the list *)
  ; mutable used_mem : (((int * int * int) * int list) * string) list
    (** a tuple's membership in a relation, last first *)
  ; mem_tbl : ((int * int * int) * int list, string) Hashtbl.t
    (** the same, by relation and tuple *)
  ; mutable packs : (string * string * (int * string * Constr.t) list) list
    (** a coverage's memberships as one fact: its name, its trie's, and
        each membership's key, fact and statement *)
  ; mutable covs : (P.psum * string * string) list (** summand, D, coverage *)
  ; tidx : (int, (int list, int array list) Hashtbl.t) Hashtbl.t
  }

let rel_ty (a_c : Constr.t) = app (c "mebi.dg.rel") [| a_c |]

let entry_constr
      (a_c : Constr.t)
      (lab : Constr.t)
      (cs : Constr.t)
      (r : Constr.t)
  : Constr.t
  =
  let code_list = app (c "core.list.type") [| c "mebi.dt.code" |] in
  let opt_a = app (c "core.option.type") [| a_c |] in
  let inner_ty = app (c "core.prod.type") [| opt_a; code_list |] in
  app
    (c "core.prod.intro")
    [| inner_ty
     ; c "mebi.dt.dd"
     ; app (c "core.prod.intro") [| opt_a; code_list; lab; cs |]
     ; r
    |]
;;

let label_id (gn : gen) (l : Constr.t) : int =
  match List.find_opt (fun (_, t) -> eq_closed t l) gn.plan.P.label_terms with
  | Some (i, _) -> i
  | None -> failf "lts proof: a label the engine did not number"
;;

let rinfo_of (gn : gen) ((_, _, r) as k) =
  match List.assoc_opt k gn.plan.P.rinfo with
  | Some x -> x
  | None -> failf "lts proof: relation %d has no information" r
;;

(* the partial term of [x]: placeholders [Meta u] for unknown slots *)
let rec pmeta (gn : gen) (s : sigma) (x : pt) : Constr.t =
  match closed_of gn s x with
  | Some t -> t
  | None ->
    (match x with
     | U (u, _) -> Constr.mkMeta u
     | Var v ->
       (match IM.find_opt v s with
        | Some y -> pmeta gn s y
        | None -> failf "lts proof: a target with an unknown part")
     | Cn (cn, ps, args) ->
       app
         (Constr.mkConstructU (cn, UVars.Instance.empty))
         (Array.of_list (ps @ List.map (pmeta gn s) args))
     | Pat { body; inst } when Constr.isRel body ->
       pmeta gn s (Var inst.(Array.length inst - Constr.destRel body))
     | Tm (t, args) -> fill_holes (fun j -> pmeta gn s args.(j)) t
     | Pat p ->
       (match view gn s x with
        | VC (cn, ps, args) ->
          app
            (Constr.mkConstructU (cn, UVars.Instance.empty))
            (Array.of_list (ps @ List.map (pmeta gn s) args))
        | _ -> failf "lts proof: a target computed from an unread slot")
     | K t -> t
     | Num k -> n_constr k)
;;

(* the summand of shape [sid] whose chain goes on from [path] with a
   closed step *)
let summand_of (mv : mover) sid (path : string list) : P.psum =
  let rec prefix a b =
    match a, b with
    | [], x :: _ -> String.length x > 0 && x.[0] = '<'
    | x :: a', y :: b' -> x = y && prefix a' b'
    | _ -> false
  in
  let rules (ch : string list) =
    List.filter (fun x -> Bool.not (String.length x > 0 && x.[0] = '<')) ch
  in
  ignore prefix;
  match
    List.find_opt
      (fun (sm : P.psum) -> sm.P.sid = sid && rules sm.P.chain = path)
      mv.dir.P.cover
  with
  | Some sm ->
    if sm.P.escaped then failf "lts proof: a summand with escaped projections";
    sm
  | None ->
    failf
      "lts proof: no summand of shape %d along [%s]"
      sid
      (String.concat ";" path)
;;

let keep_codes (sm : P.psum) =
  trim (String.concat "" (List.map (fun k -> if k then "O" else "C") sm.P.keep))
;;

(* [close_defs defs body]: [let]s of [defs] (outermost first) around
   [body], their names abstracted in one pass each; [body]'s free [Rel]s
   lifted over them *)
let close_defs
      (defs : (string * Constr.t * Constr.types) list)
      (body : Constr.t)
  : Constr.t
  =
  let defs = Array.of_list defs in
  let n = Array.length defs in
  let tbl = Hashtbl.create ((2 * n) + 1) in
  Array.iteri
    (fun j (id, _, _) -> Hashtbl.replace tbl (Names.Id.of_string id) j)
    defs;
  let abs k t =
    let rec go depth t =
      match Constr.kind t with
      | Constr.Rel i -> if i > depth then Constr.mkRel (i + k) else t
      | Constr.Var id ->
        (match Hashtbl.find_opt tbl id with
         | Some j when j < k -> Constr.mkRel (depth + k - j)
         | _ -> t)
      | _ -> Constr.map_with_binders succ go depth t
    in
    if k = 0 then t else go 0 t
  in
  let rec wrap j =
    if j = n
    then abs n body
    else (
      let id, b, ty = defs.(j) in
      Constr.mkLetIn
        (Context.nameR (Names.Id.of_string id), abs j b, abs j ty, wrap (j + 1)))
  in
  wrap 0
;;

let allt_build
      ~(top : Constr.t)
      (q : Constr.t)
      (ts : (int array * Constr.t) list)
  : Constr.t * Constr.t
  =
  let lN = app (c "core.list.type") [| n_ty () |] in
  let shift q v =
    Constr.mkLambda
      ( Context.anonR
      , lN
      , app
          (Vars.lift 1 q)
          [| app (c "core.list.cons") [| n_ty (); n_constr v; Constr.mkRel 1 |]
          |] )
  in
  (* each subtree a [let] (children first), so that the proof names it
     instead of repeating it in each ancestor's arguments *)
  let defs = ref []
  and cnt = ref 0 in
  let name_of t =
    incr cnt;
    let n = Printf.sprintf "mb__dd%d" !cnt in
    defs := (n, t, c "mebi.dt.dd") :: !defs;
    var n
  in
  (* the subtree, its name, its proof *)
  let rec build q ts depth =
    match ts with
    | [] -> c "mebi.dt.D0", c "mebi.dt.D0", app (c "mebi.de.allt_0") [| q |]
    | [ (t, pf) ] when depth = Array.length t ->
      c "mebi.dt.D1", c "mebi.dt.D1", app (c "mebi.de.allt_1") [| q; pf |]
    | (t, _) :: _ ->
      if depth >= Array.length t
      then failf "lts proof: a diagram with a repeated tuple";
      let v = t.(depth) in
      let same = List.filter (fun (u, _) -> u.(depth) = v) ts
      and rest = List.filter (fun (u, _) -> u.(depth) <> v) ts in
      let qd = shift q v in
      let dd, nd, pd = build qd same (depth + 1) in
      let dr, nr, pr = build q rest depth in
      ( app (c "mebi.dt.Nd") [| n_constr v; dd; dr |]
      , name_of (app (c "mebi.dt.Nd") [| n_constr v; nd; nr |])
      , app (c "mebi.de.allt_nd") [| n_constr v; nd; nr; q; pd; pr |] )
  in
  let dd, _, pf =
    build
      q
      (List.sort
         (fun (a, _) (b, _) -> compare (Array.to_list a) (Array.to_list b))
         ts)
      0
  in
  (* the proof given the type [top] (the diagram by its name) inside the
     [let]s: typing each [let] substitutes into that type, which names no
     subtree (into the proof's own type, it would make the whole tree,
     subtree by subtree) *)
  let pf =
    app (Constr.mkLambda (Context.anonR, top, Constr.mkRel 1)) [| pf |]
  in
  dd, close_defs (List.rev !defs) pf
;;

(* the diagram of the tuples [ts] (sorted, the way Sylvan orders them) *)
let dd_of (ts : int array list) : Constr.t =
  let rec build ts depth =
    match ts with
    | [] -> c "mebi.dt.D0"
    | [ t ] when depth = Array.length t -> c "mebi.dt.D1"
    | t :: _ ->
      if depth >= Array.length t
      then failf "lts proof: a diagram with a repeated tuple";
      let v = t.(depth) in
      let same = List.filter (fun u -> u.(depth) = v) ts
      and rest = List.filter (fun u -> u.(depth) <> v) ts in
      app
        (c "mebi.dt.Nd")
        [| n_constr v; build same (depth + 1); build rest depth |]
  in
  build
    (List.sort (fun a b -> compare (Array.to_list a) (Array.to_list b)) ts)
    0
;;

let slot_of_pos (sh : shp) q =
  let j = ref (-1) in
  Array.iteri (fun i p -> if p = q then j := i) sh.pos;
  !j
;;

(* the shape's slots, as unknowns *)
let shape_pt (sh : shp) (us : int array) : pt =
  let rec go (x : Skeleton.shape) =
    match x with
    | Container.Slot j -> U (us.(j), sh.keys.(j))
    | Container.Node n ->
      Cn (n.Container.cons, n.Container.params, List.map go n.Container.kids)
  in
  go sh.shape
;;

let completeness (gn : gen) (mv : mover) : Constr.t =
  let st = gn.st in
  let enc_intros : (string * int, string) Hashtbl.t = Hashtbl.create 8 in
  let en = mv.men in
  let nl = gn.nl_name in
  let si =
    let ty =
      Constr.mkProd
        ( Context.anonR
        , app (c "core.option.type") [| en.a_c |]
        , Constr.mkProd
            ( Context.anonR
            , app (c "core.list.type") [| n_ty () |]
            , Constr.mkProd
                (Context.anonR, Vars.lift 2 (var en.m), Constr.mkProp) ) )
    in
    fdef
      st
      "SI"
      (app
         (c (if mv.right then "mebi.de.StepInR" else "mebi.de.StepInL"))
         [| var en.m; en.a_c; var en.e; var gn.nl_name; var mv.f_rs |])
      ty
  in
  let goal_of l t (q : sigma -> string) s =
    apps si [ vtext gn s l; q s; vtext gn s t ]
  in
  let rpred =
    let text =
      if mv.right
      then
        Printf.sprintf
          "fun (mb_r : %s) (mb_q : %s) => forall (mb_p : %s) (mb_l : %s) (mb_t \
           : %s), %s -> %s -> %s -> %s"
          en.m
          (tlistN ())
          (tlistN ())
          en.lty
          en.m
          (teq (apps (g "mebi.de.length") [ "_"; "mb_p" ]) nl)
          (apps
             (g "mebi.dt.In_t")
             [ mv.f_z; apps (g "mebi.de.app") [ "_"; "mb_p"; "mb_q" ] ])
          (apps (gind en.lts) [ "mb_r"; "mb_l"; "mb_t" ])
          (apps
             (g "mebi.de.StepInR")
             [ "_"; "_"; en.e; nl; mv.f_rs; "mb_l"; "mb_q"; "mb_t" ])
      else
        Printf.sprintf
          "fun (mb_r : %s) (mb_p : %s) => forall (mb_q : %s) (mb_l : %s) (mb_t \
           : %s), %s -> %s -> %s"
          en.m
          (tlistN ())
          (tlistN ())
          en.lty
          en.m
          (apps
             (g "mebi.dt.In_t")
             [ mv.f_z; apps (g "mebi.de.app") [ "_"; "mb_p"; "mb_q" ] ])
          (apps (gind en.lts) [ "mb_r"; "mb_l"; "mb_t" ])
          (apps
             (g "mebi.de.StepInL")
             [ "_"; "_"; en.e; nl; mv.f_rs; "mb_l"; "mb_p"; "mb_t" ])
    in
    let ty =
      arrow
        (var en.m)
        (arrow (app (c "core.list.type") [| n_ty () |]) Constr.mkProp)
    in
    fdef st "R" (elab st ~expected:ty text) ty
  in
  let shape_proof sid =
    let sh = shp_of en sid in
    let k = Array.length sh.keys in
    let us = Array.init k (fun _ -> new_var ~name:"mb_u" (g "num.N.type")) in
    let l = new_var ~name:"mb_l" en.lty
    and t = new_var ~name:"mb_t" en.m in
    let tmpl_s s =
      tmpl
        sh
        ~width:en.width
        ~slot:(fun j -> vtext gn s us.(j))
        ~zero:(num st 0)
    in
    let q s =
      apps sh.vec (Array.to_list (Array.map (fun u -> vtext gn s u) us))
    in
    let goal = goal_of l t q in
    (* the tuple of one of the summand's relations for a step: label
       [lid], target [target] (unread slots as [Meta u]), the vector's
       values [value_at] *)
    let search (sm : P.psum) lid target value_at =
      List.find_map
        (fun (lab, codes, rel, meta) ->
          if lab <> lid
          then None
          else (
            let k = Ldd.index rel in
            let rkey = lab, meta, k in
            let idx =
              match Hashtbl.find_opt mv.tidx k with
              | Some x -> x
              | None ->
                let h = Hashtbl.create 64 in
                List.iter
                  (fun (tu : int array) ->
                    let rd = ref []
                    and i = ref 0 in
                    String.iter
                      (fun cd ->
                        match cd with
                        | 'O' ->
                          rd := tu.(!i) :: !rd;
                          incr i
                        | 'X' ->
                          rd := tu.(!i) :: !rd;
                          i := !i + 2
                        | 'W' -> incr i
                        | _ -> ())
                      codes;
                    let key = List.rev !rd in
                    Hashtbl.replace
                      h
                      key
                      (tu
                       :: Stdlib.Option.value
                            (Hashtbl.find_opt h key)
                            ~default:[]))
                  (Ldd.elements ~limit:max_int rel);
                Hashtbl.add mv.tidx k h;
                h
            in
            let reads =
              List.filter_map
                (fun (p, cd) ->
                  if cd = 'O' || cd = 'X' then Some (value_at p) else None)
                (List.init (String.length codes) (fun p -> p, codes.[p]))
            in
            if List.exists (fun x -> x = None) reads
            then None
            else (
              let key = List.map Stdlib.Option.get reads in
              let cands =
                Stdlib.Option.value (Hashtbl.find_opt idx key) ~default:[]
              in
              List.find_map
                (fun (tu : int array) ->
                  (* the vector after the tuple: each position's new
                     value, a number or a slot's unknown *)
                  let i = ref 0 in
                  let post =
                    Array.init (String.length codes) (fun p ->
                      match codes.[p] with
                      | 'O' ->
                        incr i;
                        `N tu.(!i - 1)
                      | 'X' ->
                        i := !i + 2;
                        `N tu.(!i - 1)
                      | 'W' ->
                        incr i;
                        `N tu.(!i - 1)
                      | _ ->
                        (match value_at p with Some v -> `N v | None -> `U p))
                  in
                  let at p =
                    if p < Array.length post
                    then post.(p)
                    else (match value_at p with Some v -> `N v | None -> `U p)
                  in
                  match at 0 with
                  | `N sid' ->
                    (match List.find_opt (fun x -> x.sid = sid') en.shapes with
                     | None -> None
                     | Some sh' ->
                       let slot_val j' =
                         match at sh'.pos.(j') with
                         | `N 0 -> None
                         | `N v -> Some (value_of gn v)
                         | `U p ->
                           let j = ref (-1) in
                           Array.iteri (fun i q -> if q = p then j := i) sh.pos;
                           Some (Constr.mkMeta us.(!j))
                       in
                       let vals = Array.init (Array.length sh'.keys) slot_val in
                       if Array.exists (fun x -> x = None) vals
                       then None
                       else (
                         let tm =
                           Skeleton.rebuild
                             sh'.shape
                             (Array.map Stdlib.Option.get vals)
                         in
                         if conv tm target
                         then Some (rkey, codes, tu, sh', at)
                         else None))
                  | `U _ -> None)
                cands)))
        sm.P.srels
    in
    (* the facts naming a relation (its diagram, its place in the list)
       and a tuple's membership *)
    let rel_facts ((_, _, _) as key) lid cs =
      match List.assoc_opt key mv.used_rels with
      | Some x -> x
      | None ->
        let rr = assume st "rr" (c "mebi.dt.dd") in
        let lab = List.assoc lid gn.plan.P.label_terms in
        let csp = if mv.right then String.make gn.nl 'C' ^ cs else cs in
        let inty =
          app
            (c "mebi.dg.In")
            [| rel_ty en.a_c
             ; entry_constr en.a_c lab (codes_constr csp) (var rr)
             ; var mv.f_rs
            |]
        in
        let inn = assume st "in" inty in
        mv.used_rels <- mv.used_rels @ [ key, (rr, inn) ];
        rr, inn
    in
    let mem_fact key rr (tul : int list) =
      match Hashtbl.find_opt mv.mem_tbl (key, tul) with
      | Some x -> x
      | None ->
        let x =
          assume
            st
            "mem"
            (app (c "mebi.dt.In_t") [| var rr; nlist_constr tul |])
        in
        Hashtbl.add mv.mem_tbl (key, tul) x;
        mv.used_mem <- ((key, tul), x) :: mv.used_mem;
        x
    in
    (* [forall v.., E (fill (val v)..) (vec v..)], per shape *)
    let enc_intro (sh' : shp) =
      match Hashtbl.find_opt enc_intros (en.tag, sh'.sid) with
      | Some n -> n
      | None ->
        let k' = Array.length sh'.keys in
        let vs = List.init k' (fun j -> Printf.sprintf "mb_v%d" j) in
        let ty =
          elab_type
            st
            (Printf.sprintf
               "%s%s"
               (if k' = 0
                then ""
                else
                  Printf.sprintf
                    "forall %s : %s, "
                    (String.concat " " vs)
                    (g "num.N.type"))
               (apps
                  en.e
                  [ apps
                      sh'.fill
                      (List.mapi (fun j v -> tval sh'.keys.(j) v) vs)
                  ; apps sh'.vec vs
                  ]))
        in
        let body = List.fold_right tex vs (tconj trefl trefl) in
        let text =
          if k' = 0
          then body
          else Printf.sprintf "fun %s => %s" (String.concat " " vs) body
        in
        let n = def st ~opaque:true "ei" (elab st ~expected:ty text) ty in
        Hashtbl.add enc_intros (en.tag, sh'.sid) n;
        n
    in
    (* the proofs of [Reads] by their values, the names of [Shifted]'s by
       their codes: made once each *)
    let reads_tbl : (int list, Constr.t * Constr.t) Hashtbl.t =
      Hashtbl.create 256
    in
    let shifted_tbl : (string, string) Hashtbl.t = Hashtbl.create 16 in
    let shifted_name cs =
      match Hashtbl.find_opt shifted_tbl cs with
      | Some n -> n
      | None ->
        let n = term st (shifted_constr gn.nl cs) in
        Hashtbl.add shifted_tbl cs n;
        n
    in
    (* the memberships a coverage's proof names, as one fact: a trie of
       their statements (a datum over the relations' diagrams) and
       [TAll] of it, each membership its entry ([tall_get], the entry
       found as a [val] is). A constant's facts are its arguments, and the
       kernel types an application in time quadratic in its arguments:
       thousands of memberships, one argument each, took minutes *)
    let pack_mems (pf : Constr.t) : Constr.t =
      let mem_names = Hashtbl.create 64 in
      Hashtbl.iter
        (fun _ x -> Hashtbl.replace mem_names (Names.Id.of_string x) ())
        mv.mem_tbl;
      let found = Hashtbl.create 64
      and order = ref [] in
      let rec scan t =
        match Constr.kind t with
        | Constr.Var id ->
          if Hashtbl.mem mem_names id && Bool.not (Hashtbl.mem found id)
          then (
            Hashtbl.add found id ();
            order := id :: !order)
        | _ -> Constr.iter scan t
      in
      scan pf;
      let ids = List.rev !order in
      if List.length ids < 16 || st.effs = None
      then pf
      else (
        let entries =
          List.mapi
            (fun i id -> i + 1, id, snd (Hashtbl.find st.assumed_at id))
            ids
        in
        let prop = Constr.mkProp in
        let tr =
          List.fold_left (fun t (k, _, ty) -> insert t k ty) Leaf entries
        in
        let mt =
          def st "MT" (trie_constr prop tr) (app (c "mebi.de.trie") [| prop |])
        in
        let q =
          Constr.mkLambda
            ( Context.anonR
            , c "num.pos.type"
            , Constr.mkLambda (Context.anonR, prop, Constr.mkRel 1) )
        in
        let mems =
          assume st "mems" (app (c "mebi.de.TAll") [| prop; q; var mt |])
        in
        let by = Hashtbl.create (List.length entries) in
        List.iter
          (fun (k, id, _) ->
            Hashtbl.replace
              by
              id
              (app
                 (c "mebi.de.tall_get")
                 [| prop; var mt; q; pos_constr k; var mems |]))
          entries;
        mv.packs
        <- ( mems
           , mt
           , List.map (fun (k, id, ty) -> k, Names.Id.to_string id, ty) entries
           )
           :: mv.packs;
        let rec sub t =
          match Constr.kind t with
          | Constr.Var id ->
            (match Hashtbl.find_opt by id with Some x -> x | None -> t)
          | _ -> Constr.map sub t
        in
        sub pf)
    in
    (* a step's witness as a term: its tuple's relation, membership, reads
       and the target's encoding; [num_at p] the source vector's value at
       [p] (a number, or the [Rel] of an unread slot) *)
    let witness_c
          lid
          (key, codes, tu, sh', at)
          (num_at : int -> Constr.t)
          (slot_rel : int -> Constr.t)
      =
      let cs = trim codes in
      let rr, inn = rel_facts key lid cs in
      let tul = Array.to_list tu in
      let mem = mem_fact key rr tul in
      let lab = List.assoc lid gn.plan.P.label_terms in
      let reads_vals =
        let i = ref 0
        and acc = ref [] in
        String.iter
          (fun cd ->
            match cd with
            | 'O' ->
              acc := tu.(!i) :: !acc;
              incr i
            | 'X' ->
              acc := tu.(!i) :: !acc;
              i := !i + 2
            | 'W' -> incr i
            | _ -> ())
          cs;
        List.rev !acc
      in
      let rec reads = function
        | [] -> c "core.True.type", c "core.True.I"
        | v :: rest as l ->
          (match Hashtbl.find_opt reads_tbl l with
           | Some x -> x
           | None ->
             let ty_r, pf_r = reads rest in
             let e =
               app (c "core.eq.type") [| n_ty (); n_constr v; n_constr v |]
             in
             let x =
               ( app (c "core.and.type") [| e; ty_r |]
               , app
                   (c "core.and.conj")
                   [| e
                    ; ty_r
                    ; app (c "core.eq.refl") [| n_ty (); n_constr v |]
                    ; pf_r
                   |] )
             in
             Hashtbl.add reads_tbl l x;
             x)
      in
      let _, readsp = reads reads_vals in
      let wv j' =
        match at sh'.pos.(j') with `N v -> n_constr v | `U p -> slot_rel p
      in
      let k' = Array.length sh'.keys in
      let target =
        app
          (var sh'.fill)
          (Array.init k' (fun j' ->
             app (var (sh'.keys.(j') ^ "_v")) [| wv j' |]))
      in
      let encw = app (var (enc_intro sh')) (Array.init k' wv) in
      let q =
        app
          (var sh.vec)
          (Array.init (Array.length sh.keys) (fun j -> num_at sh.pos.(j)))
      in
      if mv.right
      then
        app
          (c "mebi.de.stepinR_of")
          [| var en.m
           ; en.a_c
           ; var en.e
           ; var gn.nl_name
           ; var mv.f_rs
           ; var (term st lab)
           ; codes_constr cs
           ; codes_constr (String.make gn.nl 'C' ^ cs)
           ; var rr
           ; nlist_constr tul
           ; q
           ; target
           ; var (shifted_name cs)
           ; var inn
           ; var mem
           ; readsp
           ; encw
          |]
      else
        app
          (c "mebi.de.stepinL_of")
          [| var en.m
           ; en.a_c
           ; var en.e
           ; var gn.nl_name
           ; var mv.f_rs
           ; var (term st lab)
           ; codes_constr cs
           ; var rr
           ; nlist_constr tul
           ; q
           ; target
           ; var (term st (within_constr gn.nl cs))
           ; var inn
           ; var mem
           ; readsp
           ; encw
          |]
    in
    let stuck rc s path _ hyps =
      let sm = summand_of mv sid path in
      let kc = keep_codes sm in
      let kcn = term st (codes_constr kc) in
      let free = List.filter (fun v -> Bool.not (IM.mem v s)) rc.scope in
      let binder v =
        match Hashtbl.find_opt vars.types v with
        | Some ty when ty <> "_" -> Printf.sprintf "(%s : %s)" (vname v) ty
        | _ -> vname v
      in
      let vec s =
        if mv.right
        then apps (g "core.list.cons") [ "_"; num st sid; tlist (tmpl_s s) ]
        else
          apps
            (g "mebi.de.app")
            [ "_"
            ; apps (g "core.list.cons") [ "_"; num st sid; tlist (tmpl_s s) ]
            ; "mb_q"
            ]
      in
      let proj s = apps (g "mebi.de.projv") [ kcn; vec s ] in
      let qtext =
        Printf.sprintf
          "fun (mb_w : %s) => forall %s, %s -> %s"
          (tlistN ())
          (String.concat
             " "
             (List.map binder free
              @
              if mv.right
              then []
              else [ Printf.sprintf "(mb_q : %s)" (tlistN ()) ]))
          (teq "mb_w" (proj s))
          (forall_text gn s hyps (rc.goal s))
      in
      let qty = arrow (app (c "core.list.type") [| n_ty () |]) Constr.mkProp in
      let qn = fdef st "Q" (elab st ~expected:qty qtext) qty in
      (* the projections' diagram, as Sylvan made it (its node of the log
         is the same tree) *)
      let tuples = Ldd.elements ~limit:max_int sm.P.seen in
      let fd = def st "D" (dd_of tuples) (c "mebi.dt.dd") in
      let covty =
        if mv.right
        then
          Printf.sprintf
            "forall (mb_p mb_r : %s), %s -> %s -> %s"
            (tlistN ())
            (teq (apps (g "mebi.de.length") [ "_"; "mb_p" ]) nl)
            (apps
               (g "mebi.dt.In_t")
               [ mv.f_z
               ; apps
                   (g "mebi.de.app")
                   [ "_"
                   ; "mb_p"
                   ; apps (g "core.list.cons") [ "_"; num st sid; "mb_r" ]
                   ]
               ])
            (apps
               (g "mebi.dt.In_t")
               [ fd
               ; apps
                   (g "mebi.de.projv")
                   [ kcn
                   ; apps (g "core.list.cons") [ "_"; num st sid; "mb_r" ]
                   ]
               ])
        else
          Printf.sprintf
            "forall (mb_r mb_q : %s), %s -> %s"
            (tlistN ())
            (apps
               (g "mebi.dt.In_t")
               [ mv.f_z
               ; apps
                   (g "mebi.de.app")
                   [ "_"
                   ; apps (g "core.list.cons") [ "_"; num st sid; "mb_r" ]
                   ; "mb_q"
                   ]
               ])
            (apps
               (g "mebi.dt.In_t")
               [ fd
               ; apps
                   (g "mebi.de.projv")
                   [ kcn
                   ; apps
                       (g "mebi.de.app")
                       [ "_"
                       ; apps (g "core.list.cons") [ "_"; num st sid; "mb_r" ]
                       ; "mb_q"
                       ]
                   ]
               ])
      in
      let fcov = assume st "cov" (elab_type st covty) in
      mark
        "coverage of shape %d along [%s]: %.0f projections, %d elaborations so \
         far"
        sid
        (String.concat ";" sm.P.chain)
        (Ldd.count sm.P.seen)
        !el_n;
      mv.covs <- mv.covs @ [ sm, fd, fcov ];
      (* the leaves: one per learned projection *)
      let keep = Array.of_list sm.P.keep in
      let kept =
        List.filter (fun p -> keep.(p)) (List.init (Array.length keep) Fun.id)
      in
      let slot_at p =
        let j = ref (-1) in
        Array.iteri (fun i q -> if q = p then j := i) sh.pos;
        !j
      in
      let rc_leaf = { rc with stuck = None } in
      let leaf_of (tup : int array) =
        if tup.(0) <> sid
        then (
          (* a projection of another shape: impossible here *)
          let text =
            Printf.sprintf
              "fun %s mb_ew %s => %s"
              (String.concat
                 " "
                 (List.map vname free @ if mv.right then [] else [ "mb_q" ]))
              (String.concat " " (names hyps))
              (refute
                 "mb_ew"
                 (apps (g "core.list.cons") [ "_"; npat tup.(0); "_" ]))
          in
          let ty = app (var qn) [| nlist_constr (Array.to_list tup) |] in
          tup, elab st ~expected:ty text)
        else (
          let pvs =
            List.map
              (fun p ->
                if p = 0
                then None
                else (
                  let j = slot_at p in
                  if j < 0 then None else Some (us.(j), new_var ~name:"mb_p" "_")))
              kept
          in
          let s_pat =
            List.fold_left
              (fun s x ->
                match x with Some (u, pv) -> IM.add u (Var pv) s | None -> s)
              s
              pvs
          in
          let s_br =
            List.fold_left2
              (fun s x v ->
                match x with Some (u, _) -> IM.add u (Num v) s | None -> s)
              s
              pvs
              (Array.to_list tup)
          in
          let lpat =
            plist
              (List.map (function Some (_, pv) -> vname pv | None -> "_") pvs)
          in
          let ret = forall_text gn s_pat hyps (rc.goal s_pat) in
          let body = run rc_leaf s_br path hyps in
          let ns = names hyps in
          let text =
            Printf.sprintf
              "fun %s mb_ew %s => %s"
              (String.concat
                 " "
                 (List.map vname free @ if mv.right then [] else [ "mb_q" ]))
              (String.concat " " ns)
              (apps
                 (Printf.sprintf
                    "(match mb_ew in %s return (match mb_y with %s => %s | _ \
                     => %s end) with %s => %s end)"
                    (apps (g "core.eq.type") [ "_"; "_"; "mb_y" ])
                    lpat
                    ret
                    (ttrue ())
                    (paren (g "core.eq.refl" ^ " _ _"))
                    (if ns = []
                     then body
                     else
                       Printf.sprintf
                         "(fun %s => %s)"
                         (String.concat " " ns)
                         body))
                 ns)
          in
          let ty = app (var qn) [| nlist_constr (Array.to_list tup) |] in
          tup, elab st ~expected:ty text)
      in
      (* the slots the remaining hypotheses inspect: their values fixed per
         group of projections; the others stay numbers in a lemma for the
         group *)
      let inspected = Hashtbl.create 8 in
      let rec slots_in (x : pt) =
        match x with
        | U (u, _) ->
          if Bool.not (IM.mem u s) then Hashtbl.replace inspected u ()
        | Var v ->
          (match IM.find_opt v s with Some y -> slots_in y | None -> ())
        | Cn (_, _, args) -> List.iter slots_in args
        | Tm (_, args) -> Array.iter slots_in args
        | Pat p ->
          let n = Array.length p.inst in
          Array.iteri
            (fun i v ->
              if v >= 0 && Bool.not (Vars.noccurn (n - i) p.body)
              then slots_in (Var v))
            p.inst
        | K _ | Num _ -> ()
      in
      let rec inspected_in ind (x : pt) =
        match view gn s x with
        | VC (cn, _, args) ->
          let abs = abstractable ind in
          List.iteri
            (fun k a ->
              if
                List.exists
                  (fun (c', k') -> k' = k && Names.Construct.CanOrd.equal c' cn)
                  abs
              then ()
              else inspected_in_arg ind a)
            args
        | _ -> slots_in x
      and inspected_in_arg _ a = slots_in a in
      List.iter
        (fun h ->
          match h.hk with
          | HStep (ind, x, _, _) -> inspected_in ind x
          | HDec (p, _) -> slots_in (Pat p)
          | HState (x, _) -> slots_in x
          | HEq _ -> ())
        hyps;
      let keep = Array.of_list sm.P.keep in
      let kept_pos =
        List.filter (fun p -> keep.(p)) (List.init (Array.length keep) Fun.id)
      in
      let symb =
        List.filter_map
          (fun (i, p) ->
            let j = slot_at p in
            if p > 0 && j >= 0 && Bool.not (Hashtbl.mem inspected us.(j))
            then Some (i, p, j)
            else None)
          (List.mapi (fun i p -> i, p) kept_pos)
      in
      let generic () =
        let groups = Hashtbl.create 64 in
        let order = ref [] in
        List.iter
          (fun (tup : int array) ->
            if tup.(0) = sid
            then (
              let key =
                List.filteri
                  (fun i _ ->
                    Bool.not (List.exists (fun (i', _, _) -> i' = i) symb))
                  (Array.to_list tup)
              in
              if Bool.not (Hashtbl.mem groups key) then order := key :: !order;
              Hashtbl.replace
                groups
                key
                (tup
                 :: Stdlib.Option.value
                      (Hashtbl.find_opt groups key)
                      ~default:[])))
          tuples;
        let others =
          List.filter (fun (tup : int array) -> tup.(0) <> sid) tuples
        in
        List.map leaf_of others
        @ List.concat_map
            (fun key ->
              let tups = List.rev (Hashtbl.find groups key) in
              let rep = List.hd tups in
              let cvs =
                List.map
                  (fun (_, _, j) -> j, new_var ~name:"mb_c" (g "num.N.type"))
                  symb
              in
              (* the branch: inspected slots their values, the others
                 numbers *)
              let pvs =
                List.map
                  (fun p ->
                    if p = 0
                    then None
                    else (
                      let j = slot_at p in
                      if j < 0
                      then None
                      else Some (us.(j), new_var ~name:"mb_p" "_")))
                  kept_pos
              in
              let s_pat =
                List.fold_left
                  (fun s x ->
                    match x with
                    | Some (u, pv) -> IM.add u (Var pv) s
                    | None -> s)
                  s
                  pvs
              in
              let s_br =
                List.fold_left2
                  (fun s x v ->
                    match x with
                    | Some (u, _) ->
                      let j = ref (-1) in
                      Array.iteri (fun i u' -> if u' = u then j := i) us;
                      (match List.assoc_opt !j cvs with
                       | Some cv -> IM.add u (Var cv) s
                       | None -> IM.add u (Num v) s)
                    | None -> s)
                  s
                  pvs
                  (Array.to_list rep)
              in
              let records = ref [] in
              let cv_ids = List.map snd cvs in
              let rc_gen =
                { rc with
                  stuck = None
                ; finish =
                    (fun s _ ->
                      let unread =
                        List.filter
                          (fun u -> Bool.not (IM.mem u s))
                          (Array.to_list us)
                      in
                      let lc = pconstr_g gn s cv_ids (Var l)
                      and tc = pconstr_g gn s cv_ids (Var t) in
                      let i = List.length !records in
                      records := !records @ [ lc, tc, unread, rc.goal s ];
                      apps (Printf.sprintf "mb_H%d" i) (List.map vname unread))
                }
              in
              let tg = Unix.gettimeofday () in
              let body = run rc_gen s_br path hyps in
              tm_group := !tm_group +. (Unix.gettimeofday () -. tg);
              let ns = names hyps in
              let lpat =
                plist
                  (List.map
                     (function Some (_, pv) -> vname pv | None -> "_")
                     pvs)
              in
              let ret = forall_text gn s_pat hyps (rc.goal s_pat) in
              let hs =
                List.mapi (fun i _ -> Printf.sprintf "mb_H%d" i) !records
              in
              let text =
                Printf.sprintf
                  "fun %s %s %s mb_ew %s => %s"
                  (String.concat " " (List.map (fun (_, cv) -> vname cv) cvs))
                  (String.concat " " hs)
                  (String.concat
                     " "
                     (List.map vname free @ if mv.right then [] else [ "mb_q" ]))
                  (String.concat " " ns)
                  (apps
                     (Printf.sprintf
                        "(match mb_ew in %s return (match mb_y with %s => %s | \
                         _ => %s end) with %s => %s end)"
                        (apps (g "core.eq.type") [ "_"; "_"; "mb_y" ])
                        lpat
                        ret
                        (ttrue ())
                        (paren (g "core.eq.refl" ^ " _ _"))
                        (if ns = []
                         then body
                         else
                           Printf.sprintf
                             "(fun %s => %s)"
                             (String.concat " " ns)
                             body))
                     ns)
              in
              let proj =
                tlist
                  (List.mapi
                     (fun i p ->
                       match List.find_opt (fun (i', _, _) -> i' = i) symb with
                       | Some (_, _, j) -> vname (List.assoc j cvs)
                       | None ->
                         ignore p;
                         num st rep.(i))
                     kept_pos)
              in
              let ty =
                elab_type
                  st
                  (Printf.sprintf
                     "%s%s%s"
                     (if cvs = []
                      then ""
                      else
                        Printf.sprintf
                          "forall %s : %s, "
                          (String.concat
                             " "
                             (List.map (fun (_, cv) -> vname cv) cvs))
                          (g "num.N.type"))
                     (String.concat
                        ""
                        (List.map
                           (fun (_, _, unread, gl) ->
                             if unread = []
                             then Printf.sprintf "%s -> " gl
                             else
                               Printf.sprintf
                                 "(forall %s : %s, %s) -> "
                                 (String.concat " " (List.map vname unread))
                                 (g "num.N.type")
                                 gl)
                           !records))
                     (apps qn [ proj ]))
              in
              let gl =
                fdef st ~opaque:true "lf" (elab st ~expected:ty text) ty
              in
              (* each projection of the group: the lemma, its numbers, and
                 each step's witness *)
              List.map
                (fun (tup : int array) ->
                  let cvals = List.map (fun (i, _, j) -> j, tup.(i)) symb in
                  let value_at p =
                    if p = 0
                    then Some sid
                    else (
                      let j = slot_at p in
                      if j < 0
                      then Some 0
                      else (
                        match
                          List.find_opt
                            (fun (_, p', _) -> p' = p)
                            (List.mapi (fun i p -> i, p, ()) kept_pos)
                        with
                        | Some (i, _, ()) -> Some tup.(i)
                        | None -> None))
                  in
                  let wits =
                    List.map
                      (fun (lc, tc, unread, _) ->
                        let fill t =
                          fill_holes
                            (fun h ->
                              let cvj = List.nth cv_ids h in
                              let j =
                                fst (List.find (fun (_, cv) -> cv = cvj) cvs)
                              in
                              value_of gn (List.assoc j cvals))
                            t
                        in
                        let t0 = Unix.gettimeofday () in
                        let lc' = nf (fill lc)
                        and target = fill tc in
                        let lid = label_id gn lc' in
                        let t1 = Unix.gettimeofday () in
                        tm_label := !tm_label +. (t1 -. t0);
                        let m = List.length unread in
                        let rel_of u =
                          let rec idx i = function
                            | [] ->
                              failf "lts proof: an unread slot not abstracted"
                            | x :: rest -> if x = u then i else idx (i + 1) rest
                          in
                          Constr.mkRel (m - idx 0 unread)
                        in
                        let slot_rel p = rel_of us.(slot_at p) in
                        let num_at p =
                          match value_at p with
                          | Some v -> n_constr v
                          | None -> slot_rel p
                        in
                        let found = search sm lid target value_at in
                        let t2 = Unix.gettimeofday () in
                        tm_search := !tm_search +. (t2 -. t1);
                        match found with
                        | None ->
                          failf
                            "lts proof: a projection's step not among the \
                             tuples"
                        | Some f ->
                          let w = witness_c lid f num_at slot_rel in
                          tm_witness
                          := !tm_witness +. (Unix.gettimeofday () -. t2);
                          List.fold_left
                            (fun acc _ ->
                              Constr.mkLambda (Context.anonR, n_ty (), acc))
                            w
                            unread)
                      !records
                  in
                  ( tup
                  , app
                      (var gl)
                      (Array.of_list
                         (List.map (fun (_, v) -> n_constr v) cvals @ wits)) ))
                tups)
            (List.rev !order)
      in
      let leaves =
        if symb = [] || Sys.getenv_opt "MEBI_QED_LEAVES" = Some "each"
        then List.map leaf_of tuples
        else (
          try generic () with
          | Failure msg ->
            mark
              "generic cases of shape %d along [%s] failed: %s"
              sid
              (String.concat ";" sm.P.chain)
              msg;
            List.map leaf_of tuples)
      in
      mark
        "coverage: %d leaves built (labels %.1fs, search %.1fs, witnesses \
         %.1fs, group cases %.1fs)"
        (List.length leaves)
        !tm_label
        !tm_search
        !tm_witness
        !tm_group;
      let allty = app (c "mebi.de.AllT") [| var fd; var qn |] in
      let dd, pf = allt_build ~top:allty (var qn) leaves in
      let pf = pack_mems pf in
      ignore dd;
      let fall = fdef st ~opaque:true "all" pf allty in
      (* a large coverage's garbage (its leaves, its proof, now the
         kernel's) given back before the next (a compaction costs a
         fraction of a second: not for small ones) *)
      if List.length leaves > 5000 then Gc.compact ();
      mark "coverage: declared";
      if mv.right
      then
        apps
          fall
          ([ "_"; apps fcov [ "mb_p"; tlist (tmpl_s s); "mb_hl"; "mb_hz" ] ]
           @ List.map vname free
           @ [ trefl ]
           @ names hyps)
      else
        apps
          fall
          ([ "_"; apps fcov [ tlist (tmpl_s s); "mb_q"; "mb_hz" ] ]
           @ List.map vname free
           @ [ "mb_q"; trefl ]
           @ names hyps)
    in
    let rc_ref = ref None in
    let rec finish s path =
      let sm = summand_of mv sid path in
      (* a summand that reads slots no premise needed: their values first *)
      let unread =
        List.exists
          (fun (p, k) ->
            k
            && p > 0
            &&
            let j = slot_of_pos sh p in
            j >= 0 && Bool.not (IM.mem us.(j) s))
          (List.mapi (fun p k -> p, k) sm.P.keep)
      in
      match unread, !rc_ref with
      | true, Some rc when rc.stuck <> None -> stuck rc s path [] []
      | _ -> finish_ s path sm
    and finish_ s path sm =
      let lc =
        match closed_of gn s (Var l) with
        | Some x -> x
        | None -> failf "lts proof: a step's label not closed"
      in
      let lid = label_id gn lc in
      let target = nf_meta (pmeta gn s (Var t)) in
      let value_at p =
        if p = 0
        then Some sid
        else (
          let j = ref (-1) in
          Array.iteri (fun i q -> if q = p then j := i) sh.pos;
          if !j < 0
          then Some 0
          else (
            match IM.find_opt us.(!j) s with
            | Some (Num v) -> Some v
            | _ -> None))
      in
      let found = search sm lid target value_at in
      match found with
      | None ->
        let env = Global.env () in
        let pr t =
          Pp.string_of_ppcmds (Printer.pr_constr_env env (Evd.from_env env) t)
        in
        failf
          "lts proof: a step of the mover's not among its tuples: shape %d, \
           path [%s], label %s, target %s, slots [%s], relations %s"
          sid
          (String.concat ";" path)
          (pr lc)
          (pr target)
          (String.concat
             ";"
             (Array.to_list
                (Array.map
                   (fun u ->
                     match IM.find_opt u s with
                     | Some (Num v) -> string_of_int v
                     | _ -> "?")
                   us)))
          (String.concat
             " "
             (List.map
                (fun (l, cs, r, _) ->
                  Printf.sprintf "%d:%s:%d" l cs (Ldd.index r))
                sm.P.srels))
      | Some (((_, _, k) as key), codes, tu, sh', at) ->
        let cs = trim codes in
        let rr, inn =
          match List.assoc_opt key mv.used_rels with
          | Some x -> x
          | None ->
            let rr = assume st "rr" (c "mebi.dt.dd") in
            let lab = List.assoc lid gn.plan.P.label_terms in
            let csp = if mv.right then String.make gn.nl 'C' ^ cs else cs in
            let inty =
              app
                (c "mebi.dg.In")
                [| rel_ty en.a_c
                 ; entry_constr en.a_c lab (codes_constr csp) (var rr)
                 ; var mv.f_rs
                |]
            in
            let inn = assume st "in" inty in
            mv.used_rels <- mv.used_rels @ [ key, (rr, inn) ];
            rr, inn
        in
        let tul = Array.to_list tu in
        let mem =
          match Hashtbl.find_opt mv.mem_tbl (key, tul) with
          | Some x -> x
          | None ->
            let x =
              assume
                st
                "mem"
                (app (c "mebi.dt.In_t") [| var rr; nlist_constr tul |])
            in
            Hashtbl.add mv.mem_tbl (key, tul) x;
            mv.used_mem <- ((key, tul), x) :: mv.used_mem;
            x
        in
        let csn = term st (codes_constr cs) in
        let reads =
          List.fold_right
            (fun cd acc ->
              if cd = 'O' || cd = 'X' then tconj trefl acc else acc)
            (List.init (String.length cs) (fun i -> cs.[i]))
            (tI ())
        in
        let encw =
          List.fold_right
            (fun j' acc ->
              let w =
                match at sh'.pos.(j') with
                | `N v -> num st v
                | `U p ->
                  let j = ref (-1) in
                  Array.iteri (fun i q -> if q = p then j := i) sh.pos;
                  vtext gn s us.(!j)
              in
              tex w acc)
            (List.init (Array.length sh'.keys) Fun.id)
            (tconj trefl trefl)
        in
        let tupn = term st (nlist_constr tul) in
        if mv.right
        then (
          let csp = term st (codes_constr (String.make gn.nl 'C' ^ cs)) in
          let shn = term st (shifted_constr gn.nl cs) in
          apps
            (g "mebi.de.stepinR_of")
            [ "_"
            ; "_"
            ; en.e
            ; nl
            ; mv.f_rs
            ; vtext gn s l
            ; csn
            ; csp
            ; rr
            ; tupn
            ; q s
            ; vtext gn s t
            ; shn
            ; inn
            ; mem
            ; reads
            ; encw
            ])
        else (
          let win = term st (within_constr gn.nl cs) in
          apps
            (g "mebi.de.stepinL_of")
            [ "_"
            ; "_"
            ; en.e
            ; nl
            ; mv.f_rs
            ; vtext gn s l
            ; csn
            ; rr
            ; tupn
            ; q s
            ; vtext gn s t
            ; win
            ; inn
            ; mem
            ; reads
            ; encw
            ])
    in
    let rc =
      { gn
      ; goal
      ; finish
      ; stuck = Some stuck
      ; scope = Array.to_list us @ [ l; t ]
      ; extends =
          (fun path ->
            let rules ch =
              List.filter
                (fun x -> Bool.not (String.length x > 0 && x.[0] = '<'))
                ch
            in
            let rec strict a b =
              match a, b with
              | [], _ :: _ -> true
              | x :: a', y :: b' -> x = y && strict a' b'
              | _ -> false
            in
            List.exists
              (fun (sm : P.psum) ->
                sm.P.sid = sid && strict path (rules sm.P.chain))
              mv.dir.P.cover)
      }
    in
    rc_ref := Some rc;
    let hT = { hn = "mb_T"; hk = HStep (en.lts, shape_pt sh us, l, t) } in
    let absent =
      List.for_all
        (fun (sm : P.psum) -> sm.P.sid <> sid || sm.P.chain = [])
        mv.dir.P.cover
    in
    let body =
      if absent
      then stuck rc IM.empty [] [] [ hT ]
      else run rc IM.empty [] [ hT ]
    in
    let tm =
      tlist
        (tmpl sh ~width:en.width ~slot:(fun j -> vname us.(j)) ~zero:(num st 0))
    in
    let expat =
      List.fold_right
        (fun j acc -> apps (g "core.ex.intro") [ "_"; "_"; vname us.(j); acc ])
        (List.init k Fun.id)
        (apps (g "core.and.conj") [ "_"; "_"; "mb_er"; "mb_ex" ])
    in
    let inner =
      if mv.right
      then
        Printf.sprintf
          "fun mb_p %s %s mb_hl mb_hz mb_T => %s"
          (vname l)
          (vname t)
          body
      else
        Printf.sprintf
          "fun mb_q %s %s mb_hz mb_T => %s"
          (vname l)
          (vname t)
          body
    in
    Printf.sprintf
      "(fun mb_r mb_rest mb_hp => match mb_hp with %s => match %s in %s return \
       %s with %s => match %s in %s return %s with %s => %s end end end)"
      expat
      (apps (g "mebi.de.eq_sym") [ "_"; "_"; "_"; "mb_er" ])
      (apps (g "core.eq.type") [ "_"; "_"; "mb_y" ])
      (apps
         rpred
         [ "mb_r"; apps (g "core.list.cons") [ "_"; num st sid; "mb_y" ] ])
      (paren (g "core.eq.refl" ^ " _ _"))
      (apps (g "mebi.de.eq_sym") [ "_"; "_"; "_"; "mb_ex" ])
      (apps (g "core.eq.type") [ "_"; "_"; "mb_y" ])
      (apps rpred [ "mb_y"; apps (g "core.list.cons") [ "_"; num st sid; tm ] ])
      (paren (g "core.eq.refl" ^ " _ _"))
      inner
  in
  let sid_of x =
    match List.find_opt (fun (_, n) -> Constr.equal (var n) x) en.pnames with
    | Some (s, _) -> s
    | None -> failf "lts proof: a shape predicate not found"
  in
  ignore sid_of;
  let tall_pf, q0 = tall_proof st en ~rname:rpred ~entry:shape_proof in
  let pty =
    arrow
      (var en.m)
      (arrow (app (c "core.list.type") [| n_ty () |]) Constr.mkProp)
  in
  let tall =
    fdef
      st
      ~opaque:true
      "tall"
      tall_pf
      (app (c "mebi.de.TAll") [| pty; q0; var en.pt |])
  in
  mark "the shapes' proofs declared";
  let text =
    if mv.right
    then
      Printf.sprintf
        "fun mb_r mb_q mb_p mb_l mb_t mb_hr mb_hl mb_hz mb_T => %s"
        (apps
           (g "mebi.de.enc_ind")
           [ en.m
           ; en.pt
           ; rpred
           ; tall
           ; "mb_r"
           ; "mb_q"
           ; "mb_hr"
           ; "mb_p"
           ; "mb_l"
           ; "mb_t"
           ; "mb_hl"
           ; "mb_hz"
           ; "mb_T"
           ])
    else
      Printf.sprintf
        "fun mb_x mb_p mb_q mb_l mb_t mb_he mb_hz mb_T => %s"
        (apps
           (g "mebi.de.enc_ind")
           [ en.m
           ; en.pt
           ; rpred
           ; tall
           ; "mb_x"
           ; "mb_p"
           ; "mb_he"
           ; "mb_q"
           ; "mb_l"
           ; "mb_t"
           ; "mb_hz"
           ; "mb_T"
           ])
  in
  let ty =
    app
      (c (if mv.right then "mebi.de.CompleteTabR" else "mebi.de.CompleteTabL"))
      [| var en.m
       ; en.a_c
       ; Constr.mkIndU (en.lts, UVars.Instance.empty)
       ; var en.e
       ; var gn.nl_name
       ; var mv.f_z
       ; var mv.f_rs
      |]
  in
  elab st ~expected:ty text
;;

(* {1 Soundness: each tuple of the answerer's relations is a step} *)

let map_meta (f : int -> Constr.t) (t : Constr.t) : Constr.t =
  let rec go t =
    match Constr.kind t with Constr.Meta j -> f j | _ -> Constr.map go t
  in
  go t
;;

let decide_proof (prop : Constr.t) : Constr.t =
  let env = Global.env () in
  let sigma = Evd.from_env env in
  let goal = EConstr.of_constr (app (c "mebi.de.Decidable") [| prop |]) in
  let sigma', inst = Class_tactics.resolve_one_typeclass env sigma goal in
  let inst = EConstr.to_constr sigma' (Evarutil.nf_evar sigma' inst) in
  app
    (c "mebi.de.Decidable_sound")
    [| prop
     ; inst
     ; app (c "core.eq.refl") [| c "core.bool.type"; c "core.bool.true" |]
    |]
;;

(* a derivation as a term: the rule's constructor applied to its
   variables' values and its premises' derivations *)
let rec deriv_term (d : Learn.deriv) (meta : int -> Constr.t) : Constr.t =
  let ri =
    match
      List.find_opt
        (fun ri -> Names.Id.equal ri.rule.Rules.name d.Learn.drule.Rules.name)
        (rules_of d.Learn.dlts)
    with
    | Some x -> x
    | None -> failf "lts proof: a derivation's rule not found"
  in
  let prems = ref d.Learn.dprems in
  let sub t =
    map_meta
      (fun j ->
        if j = 1_000_000
        then failf "lts proof: a derivation with an unbound variable"
        else meta j)
      t
  in
  let args =
    Array.init ri.n (fun i ->
      if ri.prem.(i)
      then (
        match !prems with
        | p :: rest ->
          prems := rest;
          (match p with
           | Learn.DStep d' -> deriv_term d' meta
           | Learn.DDecide prop -> decide_proof (sub prop))
        | [] -> failf "lts proof: a derivation with too few premises")
      else sub d.Learn.dvals.(i))
  in
  app (Constr.mkConstructU ((d.Learn.dlts, ri.ridx), UVars.Instance.empty)) args
;;

let slot_of (sh : shp) q =
  let j = ref (-1) in
  Array.iteri (fun i p -> if p = q then j := i) sh.pos;
  !j
;;

let meta_base = 500_000

(* the proof of [AllT r (TSound L E l cs)] for the answerer's relation
   [k] (logged backwards), its diagram written out; and [cs] *)
(* [norm] (up to reordering): the relation stated instead of the LTS (its
   text and term), and each tuple's proof from its shapes and the slots'
   numbers before and after, instead of a derivation *)
let soundness_rel
      ?(norm :
         (string
         * Constr.t
         * (shp -> shp -> int array -> int array -> Constr.t))
           option)
      (gn : gen)
      (en : enc)
      (k : int * int * int)
  : Constr.t * string * Constr.t
  =
  let st = gn.st in
  let lid, codes, inv, _, chain = rinfo_of gn k in
  let cs = trim codes in
  let n = String.length cs in
  if n = 0 || (cs.[0] <> 'O' && cs.[0] <> 'X')
  then failf "lts proof: a relation that does not read the shape";
  let lab =
    if lid < 0
    then (
      match gn.plan.P.tau with
      | Some t -> List.assoc t gn.plan.P.label_terms
      | None -> failf "lts proof: a normalisation with no silent label")
    else List.assoc lid gn.plan.P.label_terms
  in
  let lbn = term st lab
  and csn = term st (codes_constr cs) in
  let code q = if q < n then cs.[q] else 'C' in
  (* each position's old value and new one, from a tuple read backwards *)
  let decode (tu : int array) =
    let i = ref 0 in
    Array.init n (fun q ->
      match cs.[q] with
      | 'O' ->
        incr i;
        tu.(!i - 1), tu.(!i - 1)
      | 'X' ->
        i := !i + 2;
        tu.(!i - 1), tu.(!i - 2)
      | _ -> -1, -1)
  in
  let tuples = Ldd.elements ~limit:max_int inv in
  mark
    "relation %d: %d tuples, codes %s"
    (let _, _, r = k in
     r)
    (List.length tuples)
    cs;
  let groups = Hashtbl.create 4 in
  List.iter
    (fun tu ->
      let d = decode tu in
      let key = fst d.(0), snd d.(0) in
      Hashtbl.replace
        groups
        key
        ((tu, d)
         :: Stdlib.Option.value (Hashtbl.find_opt groups key) ~default:[]))
    tuples;
  let csrest = term st (codes_constr (String.sub cs 1 (n - 1))) in
  let leaves = ref [] in
  Hashtbl.iter
    (fun (sid, sid') group ->
      let sh = shp_of en sid
      and sh' = shp_of en sid' in
      let reads =
        List.filter
          (fun q -> (code q = 'O' || code q = 'X') && slot_of sh q >= 0)
          (List.init (n - 1) (fun i -> i + 1))
      in
      let writes =
        List.filter
          (fun q -> code q = 'X' && slot_of sh' q >= 0)
          (List.init (n - 1) (fun i -> i + 1))
      in
      let cname q = Printf.sprintf "mb_c%d" q
      and wname q = Printf.sprintf "mb_w%d" q
      and uname j = Printf.sprintf "mb_u%d" j in
      let zero = num st 0 in
      let rd q = if List.mem q reads then cname q else zero
      and wr q = if List.mem q writes then wname q else zero in
      let tv q =
        match code q with 'O' -> [ rd q ] | 'X' -> [ wr q; rd q ] | _ -> []
      in
      let t0 =
        if cs.[0] = 'O' then [ num st sid ] else [ num st sid'; num st sid ]
      in
      let trest = List.concat_map tv (List.init (n - 1) (fun i -> i + 1)) in
      let tuple = tlist (t0 @ trest)
      and trestt = tlist trest in
      let frees =
        List.filter
          (fun j -> code sh.pos.(j) = 'C')
          (List.init (Array.length sh.keys) Fun.id)
      in
      (* a slot's value: read ones by [choice] *)
      let src choice =
        apps
          sh.fill
          (List.init (Array.length sh.keys) (fun j ->
             let q = sh.pos.(j) in
             tval sh.keys.(j) (if List.mem q reads then choice j q else uname j)))
      in
      let tgt choice =
        apps
          sh'.fill
          (List.init (Array.length sh'.keys) (fun j' ->
             let q' = sh'.pos.(j') in
             let v =
               match code q' with
               | 'X' -> wr q'
               | _ ->
                 let j = slot_of sh q' in
                 if j < 0
                 then zero
                 else if List.mem q' reads
                 then choice j q'
                 else uname j
             in
             tval sh'.keys.(j') v))
      in
      let allc _ q = cname q
      and allu j _ = uname j in
      let lts = match norm with Some (t, _, _) -> t | None -> gind en.lts in
      let dty =
        let body = apps lts [ src allc; lbn; tgt allc ] in
        if frees = []
        then body
        else
          Printf.sprintf
            "forall %s : %s, %s"
            (String.concat " " (List.map uname frees))
            (g "num.N.type")
            body
      in
      let params = List.map cname reads @ List.map wname writes in
      let stmt =
        Printf.sprintf
          "%s(%s) -> %s"
          (if params = []
           then ""
           else
             Printf.sprintf
               "forall %s : %s, "
               (String.concat " " params)
               (g "num.N.type"))
          dty
          (apps
             (g "mebi.de.TSound")
             [ en.m; term st en.a_c; lts; en.e; lbn; csn; tuple ])
      in
      let ty = elab_type st stmt in
      let k_slots = Array.length sh.keys in
      let expat =
        List.fold_right
          (fun j acc -> apps (g "core.ex.intro") [ "_"; "_"; uname j; acc ])
          (List.init k_slots Fun.id)
          (apps (g "core.and.conj") [ "_"; "_"; "mb_er"; "mb_ex" ])
      in
      let rpos =
        List.filter
          (fun q -> code q = 'O' || code q = 'X')
          (List.init (n - 1) (fun i -> i + 1))
      in
      let readpat =
        List.fold_right
          (fun q acc ->
            apps
              (g "core.and.conj")
              [ "_"
              ; "_"
              ; (if List.mem q reads then Printf.sprintf "mb_e%d" q else "_")
              ; acc
              ])
          rpos
          "_"
      in
      let encw =
        List.fold_right
          (fun j' acc ->
            let q' = sh'.pos.(j') in
            let v =
              match code q' with
              | 'X' -> wr q'
              | _ ->
                let j = slot_of sh q' in
                if j < 0 then zero else uname j
            in
            tex v acc)
          (List.init (Array.length sh'.keys) Fun.id)
          (tconj trefl trefl)
      in
      let rslots = List.map (fun q -> slot_of sh q, q) reads in
      let der =
        let start = apps "mb_D" (List.map uname frees) in
        let rec go acc done_ = function
          | [] -> acc
          | (j, q) :: rest ->
            let choice j'' q'' =
              if j'' = j
              then "mb_y"
              else if List.mem j'' done_
              then uname j''
              else cname q''
            in
            let motive =
              Printf.sprintf
                "(fun mb_y : %s => %s)"
                (g "num.N.type")
                (apps lts [ src choice; lbn; tgt choice ])
            in
            go
              (apps
                 (g "core.eq.ind_r")
                 [ "_"
                 ; cname q
                 ; motive
                 ; acc
                 ; uname j
                 ; Printf.sprintf "mb_e%d" q
                 ])
              (j :: done_)
              rest
        in
        go start [] (List.rev rslots)
      in
      let x' = tgt allu in
      let gpred =
        Printf.sprintf
          "(fun mb_x0 mb_r0 => exists mb_x1, %s /\\ %s)"
          (apps
             (g "mebi.de.enc")
             [ en.m
             ; en.pt
             ; "mb_x1"
             ; apps (g "core.list.cons") [ "_"; num st sid'; "mb_r0" ]
             ])
          (apps lts [ "mb_x0"; lbn; "mb_x1" ])
      in
      let inner =
        Printf.sprintf
          "(fun mb_x mb_r mb_r2 mb_hp mb_hs => match mb_hp with %s => %s end)"
          expat
          (apps
             (g "mebi.de.tsound_tmpl")
             [ en.m
             ; "mb_x"
             ; "_"
             ; "mb_r"
             ; "mb_r2"
             ; "_"
             ; csrest
             ; apps (g "mebi.de.swapt") [ csrest; trestt ]
             ; gpred
             ; "mb_er"
             ; "mb_ex"
             ; "mb_hs"
             ; Printf.sprintf
                 "(fun mb_hr => match mb_hr with %s => %s end)"
                 readpat
                 (tex x' (tconj encw der))
             ])
      in
      let head =
        if cs.[0] = 'O'
        then
          apps
            (g "mebi.de.tsound_ro")
            [ en.m
            ; term st en.a_c
            ; lts
            ; en.pt
            ; lbn
            ; csrest
            ; trestt
            ; num st sid
            ; inner
            ]
        else
          apps
            (g "mebi.de.tsound_rw")
            [ en.m
            ; term st en.a_c
            ; lts
            ; en.pt
            ; lbn
            ; csrest
            ; trestt
            ; num st sid
            ; num st sid'
            ; inner
            ]
      in
      let text =
        Printf.sprintf "fun %s mb_D => %s" (String.concat " " params) head
      in
      let sg = def st ~opaque:true "sg" (elab st ~expected:ty text) ty in
      (* each tuple: the lemma applied to its values and its derivation *)
      let env = Global.env () in
      let sigma = Evd.from_env env in
      List.iter
        (fun (tu, d) ->
          let vof q = snd d.(q)
          and oof q = fst d.(q) in
          for q = 1 to n - 1 do
            if (code q = 'O' || code q = 'X') && slot_of sh q < 0 && oof q <> 0
            then failf "lts proof: a tuple reading outside its shape";
            if code q = 'X' && slot_of sh' q < 0 && vof q <> 0
            then failf "lts proof: a tuple writing outside its shape"
          done;
          let value v = value_of gn v in
          let src =
            Skeleton.rebuild
              sh.shape
              (Array.init (Array.length sh.keys) (fun j ->
                 let q = sh.pos.(j) in
                 if List.mem q reads
                 then value (oof q)
                 else Constr.mkMeta (meta_base + j)))
          in
          let expected =
            Skeleton.rebuild
              sh'.shape
              (Array.init (Array.length sh'.keys) (fun j' ->
                 let q' = sh'.pos.(j') in
                 match code q' with
                 | 'X' -> value (vof q')
                 | _ ->
                   let j = slot_of sh q' in
                   if j < 0
                   then failf "lts proof: a target slot from nowhere"
                   else if List.mem q' reads
                   then value (oof q')
                   else Constr.mkMeta (meta_base + j)))
          in
          let body_norm =
            match norm with
            | None -> None
            | Some (_, _, pf) ->
              if frees <> []
              then failf "lts proof: a normalisation that copies a slot";
              let srcn = Array.map (fun q -> oof q) sh.pos in
              let tgtn =
                Array.map
                  (fun q' -> if code q' = 'X' then vof q' else oof q')
                  sh'.pos
              in
              Some (pf sh sh' srcn tgtn)
          in
          ignore src;
          ignore expected;
          let derivs =
            if body_norm <> None
            then []
            else Learn.derive_chain env sigma en.lts src chain
          in
          let d =
            if body_norm <> None
            then None
            else
              Some
                (match
                   List.find_opt
                     (fun (dv : Learn.deriv) ->
                       eq_closed dv.Learn.dlbl lab
                       && Constr.equal dv.Learn.dtgt expected)
                     derivs
                 with
                 | Some x -> x
                 | None ->
                   let pr t =
                     Pp.string_of_ppcmds (Printer.pr_constr_env env sigma t)
                   in
                   failf
                     "lts proof: a tuple with no derivation: chain [%s], from \
                      %s, label %s, expected %s; derived: %s"
                     (String.concat ";" chain)
                     (pr src)
                     (pr lab)
                     (pr expected)
                     (String.concat
                        " | "
                        (List.map
                           (fun (dv : Learn.deriv) ->
                             pr dv.Learn.dlbl ^ " -> " ^ pr dv.Learn.dtgt)
                           derivs)))
          in
          let m = List.length frees in
          let meta mt =
            let j = mt - meta_base in
            let rec idx i = function
              | [] -> failf "lts proof: a placeholder for a read slot"
              | x :: rest -> if x = j then i else idx (i + 1) rest
            in
            let i = idx 0 frees in
            let key = sh.keys.(j) in
            app
              (c "mebi.de.val")
              [| var key
               ; var (key ^ "_d")
               ; var (key ^ "_tr")
               ; Constr.mkRel (m - i)
              |]
          in
          let body =
            match body_norm, d with
            | Some b, _ -> b
            | None, Some d -> deriv_term d meta
            | None, None -> assert false
          in
          let dfun =
            List.fold_left
              (fun acc _ -> Constr.mkLambda (Context.anonR, n_ty (), acc))
              body
              frees
          in
          let args =
            List.map (fun q -> n_constr (oof q)) reads
            @ List.map (fun q -> n_constr (vof q)) writes
            @ [ dfun ]
          in
          leaves := (tu, app (var sg) (Array.of_list args)) :: !leaves)
        group)
    groups;
  let q =
    app
      (c "mebi.de.TSound")
      [| var en.m
       ; en.a_c
       ; (match norm with
          | Some (_, r, _) -> r
          | None -> Constr.mkIndU (en.lts, UVars.Instance.empty))
       ; var en.e
       ; var lbn
       ; var csn
      |]
  in
  (* the relation's diagram (its node of the log is the same tree), and
     the proof, opaque *)
  let rd = def st "Rd" (dd_of (List.map fst !leaves)) (c "mebi.dt.dd") in
  let ty = app (c "mebi.de.AllT") [| var rd; q |] in
  let _, pf = allt_build ~top:ty q !leaves in
  let sn = def st ~opaque:true "snd" pf ty in
  var sn, cs, lab
;;

(* {1 Up to reordering (doc/upto.md, route B)}

   For a system the engine explored up to reordering: the one-step
   reorderings [Ro] (a disjunction, one disjunct per reordering summand
   of a shape of the encoding, over the slots' numbers: a slot holds a
   value of its type's table, [T_v u]); its eliminator, its inverse paths,
   and its commutation with the steps of the LTS (by small inversion: a
   rule that needs a slot to start with a constructor no value starts
   with is refuted; a slot's own step is kept as a hypothesis, its target
   a value of the table by the closure lemma [Vn]), so that [Cong Ro] is
   a strong bisimulation ([cong_equiv], theories/DdUpto.v). *)

let du s = g ("mebi.du." ^ s)
let tor_ty a b = apps (g "core.or.type") [ a; b ]
let tand_ty a b = apps (g "core.and.type") [ a; b ]

let tex_ty x ty body =
  apps
    (g "core.ex.type")
    [ ty; Printf.sprintf "(fun (%s : %s) => %s)" x ty body ]
;;

let rec ors = function
  | [] -> tfalse ()
  | [ x ] -> x
  | x :: rest -> tor_ty x (ors rest)
;;

let rec ands = function
  | [] -> ttrue ()
  | [ x ] -> x
  | x :: rest -> tand_ty x (ands rest)
;;

let rec conjs = function
  | [] -> tI ()
  | [ x ] -> x
  | x :: rest -> tconj x (conjs rest)
;;

let ucount = ref 0

let ufresh base =
  incr ucount;
  Printf.sprintf "%s%d" base !ucount
;;

(* [match h with or_introl a => case 0 a | or_intror b => ...], [n]
   disjuncts, right-nested *)
let destr_or (n : int) (h : string) (case : int -> string -> string) : string =
  let rec go k h =
    if k = n - 1
    then case k h
    else (
      let a = ufresh "mb_o"
      and b = ufresh "mb_o" in
      Printf.sprintf
        "(match %s with | %s => %s | %s => %s end)"
        h
        (apps (g "mebi.de.or_introl") [ "_"; "_"; a ])
        (case k a)
        (apps (g "mebi.de.or_intror") [ "_"; "_"; b ])
        (go (k + 1) b))
  in
  if n = 0 then apps (g "mebi.de.False_ind") [ "_"; h ] else go 0 h
;;

(* the number of a value in its type's table *)
let value_num (gn : gen) (key : string) (c : Constr.t) : int =
  let vs =
    match List.assoc_opt key gn.svals with
    | Some l -> l
    | None -> failf "up to: no values of %s" key
  in
  match List.find_opt (fun (_, v) -> eq_closed v c) vs with
  | Some (i, _) -> i
  | None -> failf "up to: a step to a value no state holds"
;;

(* [Vn]: [forall u a w, L (T_v u) a w -> exists n, w = T_v n], [L] the
   slots' LTS: each value of the table (and the default) by its step
   lemma *)
let vn_tbl : ((string * Names.inductive) * string) list ref = ref []

let vn (gn : gen) (key : string) (ind : Names.inductive) : string =
  match
    List.find_opt
      (fun ((k, i), _) -> k = key && Names.Ind.CanOrd.equal i ind)
      !vn_tbl
  with
  | Some (_, n) -> n
  | None ->
    let st = gn.st in
    let _, lt_c, _ = lts_types ind in
    let lty = term st lt_c in
    let nt = g "num.N.type" in
    let qtext v =
      Printf.sprintf
        "forall (mb_a : %s) (mb_w : %s), %s -> %s"
        lty
        key
        (apps (gind ind) [ v; "mb_a"; "mb_w" ])
        (tex_ty "mb_n" nt (teq "mb_w" (tval key "mb_n")))
    in
    let q = Printf.sprintf "(fun (mb_z : %s) => %s)" key (qtext "mb_z") in
    let qproof (c : Constr.t) =
      let asn, steps = as_lemma gn ind c in
      let q' =
        Printf.sprintf
          "(fun (mb_a : %s) (mb_w : %s) => %s)"
          lty
          key
          (tex_ty "mb_n" nt (teq "mb_w" (tval key "mb_n")))
      in
      let cases =
        List.map
          (fun (_, t) ->
            apps
              (g "core.ex.intro")
              [ "_"; "_"; num st (value_num gn key (nf t)); trefl ])
          steps
      in
      Printf.sprintf
        "(fun mb_a mb_w mb_s => %s)"
        (apps asn ((q' :: cases) @ [ "mb_a"; "mb_w"; "mb_s" ]))
    in
    let vs = List.assoc key gn.svals in
    let tr = List.fold_left (fun t (i, v) -> insert t i v) Leaf vs in
    let tallty t =
      apps
        (g "mebi.de.TAll")
        [ key
        ; Printf.sprintf "(fun (_ : %s) => %s)" (g "num.pos.type") q
        ; term
            st
            (trie_constr
               (var key)
               (let rec conv = function
                  | Leaf -> Leaf
                  | Node (v, l, r) ->
                    Node
                      ( Stdlib.Option.map (fun x -> var (term st x)) v
                      , conv l
                      , conv r )
                in
                conv t))
        ]
    in
    let rec tallv t =
      match t with
      | Leaf -> tI ()
      | Node (v, l, r) ->
        let a_ty, a_pf =
          match v with
          | Some x -> apps q [ term st x ], qproof x
          | None -> ttrue (), tI ()
        in
        apps
          (g "core.and.conj")
          [ a_ty
          ; tand_ty (tallty l) (tallty r)
          ; a_pf
          ; apps (g "core.and.conj") [ tallty l; tallty r; tallv l; tallv r ]
          ]
    in
    let d =
      match vs with
      | (_, v) :: _ -> v
      | [] -> failf "up to: no values of %s" key
    in
    let text =
      apps
        (g "mebi.de.val_all")
        [ key; key ^ "_d"; key ^ "_tr"; q; qproof d; tallv tr ]
    in
    let ty =
      elab_type
        st
        (Printf.sprintf "forall (mb_u : %s), %s" nt (qtext (tval key "mb_u")))
    in
    let n = def st ~opaque:true "Vn" (elab st ~expected:ty text) ty in
    vn_tbl := ((key, ind), n) :: !vn_tbl;
    n
;;

(* a disjunct of [Ro]: a reordering summand of a shape, its target shape,
   its slot map (target slot [j] holds source slot [dmap.(j)]), its rule
   chain *)
type disj =
  { dsrc : shp
  ; dtgt : shp
  ; dmap : int array
  ; dchain : string list
  }

type ucong =
  { ro : string
  ; ro_inv : string
  ; equiv : string
  ; disjs : disj list
  ; inj : int -> string list -> string
    (** [inj k ns]: [Ro (src ns) (tgt (map ns))] by disjunct [k] *)
  ; uen : enc
  }

let rules_only ch =
  List.filter (fun x -> Bool.not (String.length x > 0 && x.[0] = '<')) ch
;;

let find_shp (en : enc) (sh : Skeleton.shape) : shp =
  let k = Skeleton.key sh in
  match List.find_opt (fun s -> Skeleton.key s.shape = k) en.shapes with
  | Some s -> s
  | None -> failf "up to: a reordering's shape the encoding lacks"
;;

let moved_map (tgt : Skeleton.target) : int array =
  Array.of_list
    (List.map
       (function
         | Skeleton.Moved i -> i
         | Skeleton.Computed _ -> failf "up to: a reordering computes a slot")
       (Skeleton.target_slots tgt))
;;

(* the summands of a shape, cached *)
let usms_tbl : (string, Skeleton.summand list) Hashtbl.t = Hashtbl.create 16

let usms (en : enc) (sh : Skeleton.shape) =
  let k = Skeleton.key sh in
  match Hashtbl.find_opt usms_tbl k with
  | Some l -> l
  | None ->
    let env = Global.env () in
    let l = Skeleton.summands_of env (Evd.from_env env) en.lts sh in
    Hashtbl.add usms_tbl k l;
    l
;;

(* a derivation of a rule chain (outermost first) of [ind], the innermost
   rule's premise [inner] (or none) *)
let deriv_text
      (ind : Names.inductive)
      (chain : string list)
      (inner : string option)
  : string
  =
  let ris = rules_of ind in
  let rec go = function
    | [] -> failf "up to: an empty rule chain"
    | [ r ] -> one r inner
    | r :: rest -> one r (Some (go rest))
  and one r prem =
    let ri =
      match
        List.find_opt (fun ri -> Names.Id.to_string ri.rule.Rules.name = r) ris
      with
      | Some ri -> ri
      | None -> failf "up to: rule %s not found" r
    in
    let nprem = Array.fold_left (fun a b -> if b then a + 1 else a) 0 ri.prem in
    if nprem > 1 then failf "up to: a rule with %d premises" nprem;
    let args =
      List.init ri.n (fun i ->
        if ri.prem.(i)
        then (
          match prem with
          | Some p -> p
          | None -> failf "up to: a premise with no derivation")
        else "_")
    in
    apps (gcons (ind, ri.ridx)) args
  in
  go chain
;;

let upto_cong (gn : gen) ?(also : enc option) (en : enc) : ucong =
  (* the other system's shapes too, when it is of the same LTS *)
  let en =
    match also with
    | Some e2 when Names.Ind.CanOrd.equal e2.lts en.lts ->
      { en with
        shapes =
          en.shapes
          @ List.filter
              (fun (s2 : shp) ->
                Bool.not
                  (List.exists
                     (fun (s1 : shp) ->
                       Skeleton.key s1.shape = Skeleton.key s2.shape)
                     en.shapes))
              e2.shapes
      }
    | _ -> en
  in
  let st = gn.st in
  let env = Global.env () in
  let sigma = Evd.from_env env in
  let cong =
    List.map Names.Id.to_string (fst (Skeleton.congruence env sigma en.lts))
  in
  let disjs =
    List.concat_map
      (fun (sh : shp) ->
        List.filter_map
          (fun (sm : Skeleton.summand) ->
            if Learn.is_reordering cong sm
            then (
              let t =
                Container.renumber (Skeleton.target_shape sm.Skeleton.target)
              in
              Some
                { dsrc = sh
                ; dtgt = find_shp en t
                ; dmap = moved_map sm.Skeleton.target
                ; dchain = rules_only sm.Skeleton.chain
                })
            else None)
          (usms en sh.shape))
      en.shapes
  in
  let nd = List.length disjs in
  let m = en.m in
  let lname = gind en.lts in
  let nt = g "num.N.type" in
  let slot_names n base = List.init n (fun j -> Printf.sprintf "%s%d" base j) in
  (* a shape's state from its slots' numbers *)
  let st_of (sh : shp) (ns : string list) =
    apps sh.fill (List.mapi (fun j u -> tval sh.keys.(j) u) ns)
  in
  let mapped (d : disj) (ns : string list) =
    Array.to_list (Array.map (List.nth ns) d.dmap)
  in
  let disj_text (d : disj) x y =
    let n = Array.length d.dsrc.keys in
    let ns = slot_names n "mb_r" in
    let body =
      tand_ty (teq x (st_of d.dsrc ns)) (teq y (st_of d.dtgt (mapped d ns)))
    in
    List.fold_right (fun u acc -> tex_ty u nt acc) ns body
  in
  let ro_ty = elab_type st (Printf.sprintf "%s -> %s -> Prop" m m) in
  let ro =
    def
      st
      "Ro"
      (elab
         st
         ~expected:ro_ty
         (Printf.sprintf
            "fun (mb_x mb_y : %s) => %s"
            m
            (ors (List.map (fun d -> disj_text d "mb_x" "mb_y") disjs))))
      ro_ty
  in
  let inj k (ns : string list) =
    let p =
      List.fold_right
        (fun u acc -> apps (g "core.ex.intro") [ "_"; "_"; u; acc ])
        ns
        (tconj trefl trefl)
    in
    case_pattern k nd p
  in
  (* the eliminator: a property of every pair of [Ro], from one per
     disjunct *)
  let hyp_ty (d : disj) prop =
    let n = Array.length d.dsrc.keys in
    let ns = slot_names n "mb_r" in
    Printf.sprintf
      "forall %s, %s"
      (String.concat
         " "
         (List.map (fun u -> Printf.sprintf "(%s : %s)" u nt) ns))
      (prop (st_of d.dsrc ns) (st_of d.dtgt (mapped d ns)))
  in
  let elim_ty =
    Printf.sprintf
      "forall (mb_P : %s -> %s -> Prop), %s forall (mb_x mb_y : %s), %s -> \
       mb_P mb_x mb_y"
      m
      m
      (String.concat
         ""
         (List.map
            (fun d ->
              paren (hyp_ty d (fun a b -> apps "mb_P" [ a; b ])) ^ " -> ")
            disjs))
      m
      (apps ro [ "mb_x"; "mb_y" ])
  in
  let elim_text =
    let hs = List.mapi (fun k _ -> Printf.sprintf "mb_c%d" k) disjs in
    let case k h =
      let d = List.nth disjs k in
      let n = Array.length d.dsrc.keys in
      let ns = slot_names n "mb_r" in
      let rec ex_d j h =
        if j = n
        then (
          let ex = ufresh "mb_e"
          and ey = ufresh "mb_e" in
          let src = st_of d.dsrc ns in
          Printf.sprintf
            "(match %s with %s => (match %s in %s return %s with %s => (match \
             %s in %s return %s with %s => %s end) end) end)"
            h
            (apps (g "core.and.conj") [ "_"; "_"; ex; ey ])
            (apps (g "mebi.de.eq_sym") [ "_"; "_"; "_"; ex ])
            (apps (g "core.eq.type") [ "_"; "_"; "mb_x0" ])
            (apps "mb_P" [ "mb_x0"; "mb_y" ])
            (paren (g "core.eq.refl" ^ " _ _"))
            (apps (g "mebi.de.eq_sym") [ "_"; "_"; "_"; ey ])
            (apps (g "core.eq.type") [ "_"; "_"; "mb_y0" ])
            (apps "mb_P" [ src; "mb_y0" ])
            (paren (g "core.eq.refl" ^ " _ _"))
            (apps (List.nth hs k) ns))
        else (
          let h' = ufresh "mb_e" in
          Printf.sprintf
            "(match %s with %s => %s end)"
            h
            (apps (g "core.ex.intro") [ "_"; "_"; List.nth ns j; h' ])
            (ex_d (j + 1) h'))
      in
      ex_d 0 h
    in
    Printf.sprintf
      "fun mb_P %s mb_x mb_y mb_H => %s"
      (String.concat " " hs)
      (destr_or nd "mb_H" case)
  in
  let elim_t = elab_type st elim_ty in
  let elim =
    def st ~opaque:true "roe" (elab st ~expected:elim_t elim_text) elim_t
  in
  (* inverse paths: each disjunct's inverse disjunct, one step back *)
  let inv_case (d : disj) =
    let n = Array.length d.dsrc.keys in
    let ns = slot_names n "mb_r" in
    let k' =
      let rec find i = function
        | [] -> failf "up to: a reordering with no inverse"
        | d' :: rest ->
          if
            d'.dsrc.sid = d.dtgt.sid
            && d'.dtgt.sid = d.dsrc.sid
            && Array.length d'.dmap = n
            && List.for_all
                 (fun i -> d.dmap.(d'.dmap.(i)) = i)
                 (List.init n Fun.id)
          then i
          else find (i + 1) rest
      in
      find 0 disjs
    in
    let ws = mapped d ns in
    let x = st_of d.dsrc ns
    and y = st_of d.dtgt ws in
    Printf.sprintf
      "(fun %s => %s)"
      (String.concat " " ns)
      (apps
         (du "cong_step")
         [ m; ro; y; x; x; inj k' ws; apps (du "cong_refl") [ m; ro; x ] ])
  in
  let inv_ty =
    Printf.sprintf
      "forall (mb_x mb_y : %s), %s -> %s"
      m
      (apps ro [ "mb_x"; "mb_y" ])
      (apps (du "Cong") [ m; ro; "mb_y"; "mb_x" ])
  in
  let inv_t = elab_type st inv_ty in
  let ro_inv =
    def
      st
      ~opaque:true
      "roi"
      (elab
         st
         ~expected:inv_t
         (apps
            elim
            (Printf.sprintf
               "(fun (mb_x mb_y : %s) => %s)"
               m
               (apps (du "Cong") [ m; ro; "mb_y"; "mb_x" ])
             :: List.map inv_case disjs)))
      inv_t
  in
  (* commutation: each disjunct's lemma, by small inversion *)
  let lty = en.lty in
  let sim_prop x y =
    Printf.sprintf
      "forall (mb_a : %s) (mb_t : %s), %s -> %s"
      lty
      m
      (apps lname [ x; "mb_a"; "mb_t" ])
      (tex_ty
         "mb_yy"
         m
         (tand_ty
            (apps lname [ y; "mb_a"; "mb_yy" ])
            (apps (du "Cong") [ m; ro; "mb_t"; "mb_yy" ])))
  in
  let commute (k : int) (d : disj) : string =
    let src = d.dsrc
    and tgt = d.dtgt in
    let n = Array.length src.keys in
    let holes = Array.init n (fun _ -> new_var ~name:"mb_u" nt) in
    let hole_of v =
      let r = ref (-1) in
      Array.iteri (fun j h -> if h = v then r := j) holes;
      !r
    in
    let a = new_var ~name:"mb_a" lty
    and t = new_var ~name:"mb_t" m in
    let rec xs_of (sh : Skeleton.shape) =
      match sh with
      | Container.Slot j -> U (holes.(j), src.keys.(j))
      | Container.Node nd ->
        Cn
          ( nd.Container.cons
          , nd.Container.params
          , List.map xs_of nd.Container.kids )
    in
    let xs = xs_of src.shape in
    let hole_text s j = vtext gn s holes.(j) in
    let goal_with s tt =
      tex_ty
        "mb_yy"
        m
        (tand_ty
           (apps
              lname
              [ st_of tgt (mapped d (List.init n (hole_text s)))
              ; vtext gn s a
              ; "mb_yy"
              ])
           (apps (du "Cong") [ m; ro; tt; "mb_yy" ]))
    in
    let goal s = goal_with s (vtext gn s t) in
    let rec res s x =
      match x with
      | Var v -> (match IM.find_opt v s with Some y -> res s y | None -> x)
      | Pat p when Constr.isRel p.body ->
        let i = Array.length p.inst - Constr.destRel p.body in
        if p.inst.(i) >= 0 then res s (Var p.inst.(i)) else x
      | _ -> x
    in
    (* the slots of [x], of shape [sh] *)
    let rec slots_of s (sh : Skeleton.shape) (x : pt) acc =
      match sh with
      | Container.Slot j -> (j, x) :: acc
      | Container.Node nd ->
        (match view gn s x with
         | VC (cn, _, args)
           when Names.Construct.CanOrd.equal cn nd.Container.cons ->
           List.fold_left2
             (fun acc kid x -> slots_of s kid x acc)
             acc
             nd.Container.kids
             args
         | _ -> failf "up to: a target that does not have its summand's shape")
    in
    let finish s path =
      (* a reordering of the source: answered by the inverse reordering
         back to it *)
      match List.rev path with
      | r :: _ when List.mem r cong ->
        let dsig =
          let rec find i = function
            | [] ->
              failf
                "up to: a reordering of no disjunct: [%s]"
                (String.concat ";" path)
            | (d' : disj) :: rest ->
              if d'.dsrc.sid = src.sid && d'.dchain = path
              then i
              else find (i + 1) rest
          in
          find 0 disjs
        in
        let back =
          match
            List.find_opt
              (fun (sm : Skeleton.summand) ->
                Learn.is_reordering cong sm
                && Skeleton.key
                     (Container.renumber
                        (Skeleton.target_shape sm.Skeleton.target))
                   = Skeleton.key src.shape
                &&
                let mm = moved_map sm.Skeleton.target in
                List.for_all (fun i -> d.dmap.(mm.(i)) = i) (List.init n Fun.id))
              (usms en tgt.shape)
          with
          | Some sm -> sm
          | None -> failf "up to: no reordering back from a disjunct's target"
        in
        let ws = List.init n (hole_text s) in
        let x = st_of src ws in
        let dv = deriv_text en.lts (rules_only back.Skeleton.chain) None in
        apps
          (g "core.ex.intro")
          [ "_"
          ; "_"
          ; x
          ; tconj dv (apps ro_inv [ x; vtext gn s t; inj dsig ws ])
          ]
      | _ ->
        failf
          "up to: rules [%s] do not commute with reordering [%s]"
          (String.concat ";" path)
          (String.concat ";" d.dchain)
    in
    let rec stuck rc s path _ (hyps : hyp list) =
      match
        List.find_opt
          (fun h ->
            match h.hk with HEq (v, _) -> Bool.not (IM.mem v s) | _ -> false)
          hyps
      with
      | Some ({ hk = HEq (v, p); _ } as h) ->
        define rc s path h (List.filter (fun h' -> h' != h) hyps) v p
      | _ -> local s path hyps
    and local s path (hyps : hyp list) =
      (* a slot's own step: the same rule at the slot's place in the
         reordered state, the same premise; its target a value of the
         table ([Vn]) *)
      let h, ind, x0, a0, b0 =
        match hyps with
        | [ ({ hk = HStep (ind, x0, a0, b0); _ } as h) ] -> h, ind, x0, a0, b0
        | _ ->
          failf
            "up to: a case the proof cannot follow, along [%s]: %s"
            (String.concat ";" path)
            (String.concat "; " (List.map (htype gn s) hyps))
      in
      let i =
        match x0 with
        | U (v, _) when hole_of v >= 0 -> hole_of v
        | _ ->
          (match res s x0 with
           | U (v, _) when hole_of v >= 0 -> hole_of v
           | _ -> failf "up to: a step from no slot")
      in
      let key = src.keys.(i) in
      let j =
        let r = ref (-1) in
        Array.iteri (fun j' i' -> if i' = i then r := j') d.dmap;
        !r
      in
      let leaf_depth (sh : Skeleton.shape) slot =
        let rec go sh depth =
          match sh with
          | Container.Slot q -> if q = slot then Some depth else None
          | Container.Node nd ->
            List.find_map (fun k -> go k (depth + 1)) nd.Container.kids
        in
        match go sh 0 with
        | Some dd -> dd
        | None -> failf "up to: a slot not in its shape"
      in
      let inner =
        List.filteri (fun q _ -> q >= leaf_depth src.shape i - 1) path
      in
      let dp = leaf_depth tgt.shape j - 1 in
      let sm' =
        match
          List.find_opt
            (fun (sm : Skeleton.summand) ->
              let ch = rules_only sm.Skeleton.chain in
              sm.Skeleton.reads = [ j ]
              && List.length ch = dp + List.length inner
              && List.filteri (fun q _ -> q >= dp) ch = inner)
            (usms en tgt.shape)
        with
        | Some sm -> sm
        | None ->
          failf
            "up to: no summand at a slot's new place, along [%s]"
            (String.concat ";" path)
      in
      (* the step's target, a free variable of the case: rewritten to the
         table's value [T_v n] *)
      let bv =
        match res s (Var b0) with
        | Var v -> v
        | _ -> failf "up to: a step's target already known"
      in
      let nv = new_var ~name:"mb_n" nt in
      let zv = new_var ~name:"mb_z" key in
      let s_z = IM.add bv (Var zv) s in
      (* the target, of the source's shape: slot [i] the new value *)
      let xv = slots_of s src.shape (Var t) [] in
      List.iter
        (fun (q, x) ->
          if q <> i
          then (
            match res s x with
            | U (v, _) when v = holes.(q) -> ()
            | _ -> failf "up to: a step that changes more than its slot"))
        xv;
      let ns =
        List.init n (fun q -> if q = i then vname nv else hole_text s q)
      in
      let x' = st_of src ns in
      let y' = st_of tgt (mapped d ns) in
      let h' = ufresh "mb_s" in
      let dv = deriv_text en.lts (rules_only sm'.Skeleton.chain) (Some h') in
      let body =
        apps
          (g "core.ex.intro")
          [ "_"
          ; "_"
          ; y'
          ; tconj
              dv
              (apps
                 (du "cong_step")
                 [ m
                 ; ro
                 ; x'
                 ; y'
                 ; y'
                 ; inj k ns
                 ; apps (du "cong_refl") [ m; ro; y' ]
                 ])
          ]
      in
      let e = ufresh "mb_e" in
      let step_z = apps (gind ind) [ ptext gn s x0; vtext gn s a0; vname zv ] in
      Printf.sprintf
        "(match %s with %s => (match %s in %s return (%s -> %s) with %s => \
         (fun %s => %s) end) %s end)"
        (apps
           (vn gn key ind)
           [ hole_text s i; vtext gn s a0; vtext gn s b0; h.hn ])
        (apps (g "core.ex.intro") [ "_"; "_"; vname nv; e ])
        (apps (g "mebi.de.eq_sym") [ "_"; "_"; "_"; e ])
        (apps (g "core.eq.type") [ "_"; "_"; vname zv ])
        step_z
        (goal_with s_z (vtext gn s_z t))
        (paren (g "core.eq.refl" ^ " _ _"))
        h'
        body
        h.hn
    in
    let rc =
      { gn
      ; goal
      ; finish
      ; stuck = Some stuck
      ; scope = Array.to_list holes @ [ a; t ]
      ; extends = (fun _ -> true)
      }
    in
    let hT = { hn = "mb_H"; hk = HStep (en.lts, xs, a, t) } in
    let t0 = Unix.gettimeofday () in
    let body = run rc IM.empty [] [ hT ] in
    let ty = elab_type st (hyp_ty d sim_prop) in
    let text =
      Printf.sprintf
        "fun %s %s %s mb_H => %s"
        (String.concat " " (Array.to_list (Array.map vname holes)))
        (vname a)
        (vname t)
        body
    in
    let r = def st ~opaque:true "rc" (elab st ~expected:ty text) ty in
    mark
      "up to: commutation %d (%s): %d chars, %.2fs"
      k
      (String.concat ";" d.dchain)
      (String.length text)
      (Unix.gettimeofday () -. t0);
    r
  in
  let cs = List.mapi commute disjs in
  let sim_ty =
    Printf.sprintf
      "forall (mb_x mb_y : %s), %s -> %s"
      m
      (apps ro [ "mb_x"; "mb_y" ])
      (sim_prop "mb_x" "mb_y")
  in
  let sim_t = elab_type st sim_ty in
  let ro_sim =
    def
      st
      ~opaque:true
      "ros"
      (elab
         st
         ~expected:sim_t
         (apps
            elim
            (Printf.sprintf
               "(fun (mb_x mb_y : %s) => %s)"
               m
               (sim_prop "mb_x" "mb_y")
             :: cs)))
      sim_t
  in
  let eq_ty =
    elab_type
      st
      (apps (du "Equiv") [ m; "_"; lname; apps (du "Cong") [ m; ro ] ])
  in
  let equiv =
    def
      st
      ~opaque:true
      "req"
      (elab
         st
         ~expected:eq_ty
         (apps (du "cong_equiv") [ m; "_"; lname; ro; ro_inv; ro_sim ]))
      eq_ty
  in
  { ro; ro_inv; equiv; disjs; inj; uen = en }
;;

(* {2 Items: a state and its representative, by the item's path} *)

let items_tbl : ((int * string * int array) * string) list ref = ref []

let st_of_shp (sh : shp) (ns : string list) =
  apps sh.fill (List.mapi (fun j u -> tval sh.keys.(j) u) ns)
;;

(* [forall us, Cong Ro (src us) (item (map us))], by the item's path:
   each step a disjunct of [Ro] *)
let item_lemma (gn : gen) (uc : ucong) (src : shp) (it : P.pnorm) : string =
  let key = src.sid, Skeleton.key it.P.nshape, it.P.nmap in
  match List.assoc_opt key !items_tbl with
  | Some n -> n
  | None ->
    let st = gn.st in
    let en = uc.uen in
    let m = en.m in
    let n = Array.length src.keys in
    let ns = List.init n (fun j -> Printf.sprintf "mb_r%d" j) in
    let at (pi : int array) = Array.to_list (Array.map (List.nth ns) pi) in
    let fin = find_shp en it.P.nshape in
    let w = st_of_shp fin (at it.P.nmap) in
    let rec go (cur : shp) (pi : int array) steps =
      match steps with
      | [] ->
        if Skeleton.key cur.shape <> Skeleton.key it.P.nshape || pi <> it.P.nmap
        then failf "up to: an item's path that does not reach it";
        apps (du "cong_refl") [ m; uc.ro; w ]
      | (shk, idx) :: rest ->
        if Skeleton.key shk <> Skeleton.key cur.shape
        then failf "up to: an item's path out of its shapes";
        let sm = List.nth (usms en shk) idx in
        let ch = rules_only sm.Skeleton.chain in
        let dk =
          let rec find i = function
            | [] -> failf "up to: a path step of no disjunct"
            | (d : disj) :: r ->
              if
                Skeleton.key d.dsrc.shape = Skeleton.key cur.shape
                && d.dchain = ch
              then i
              else find (i + 1) r
          in
          find 0 uc.disjs
        in
        let d = List.nth uc.disjs dk in
        let pi' = Array.map (fun j -> pi.(j)) d.dmap in
        apps
          (du "cong_step")
          [ m
          ; uc.ro
          ; st_of_shp cur (at pi)
          ; st_of_shp d.dtgt (at pi')
          ; w
          ; uc.inj dk (at pi)
          ; go d.dtgt pi' rest
          ]
    in
    let body = go src (Array.init n Fun.id) it.P.npath in
    let ty =
      elab_type
        st
        (Printf.sprintf
           "forall %s, %s"
           (String.concat
              " "
              (List.map
                 (fun u -> Printf.sprintf "(%s : %s)" u (g "num.N.type"))
                 ns))
           (apps (du "Cong") [ m; uc.ro; st_of_shp src ns; w ]))
    in
    let text = Printf.sprintf "fun %s => %s" (String.concat " " ns) body in
    let nm = def st ~opaque:true "item" (elab st ~expected:ty text) ty in
    items_tbl := (key, nm) :: !items_tbl;
    nm
;;

(* the relation [fun x _ y => Cong Ro x y], its text and term *)
let cong_lts (gn : gen) (uc : ucong) : string * Constr.t =
  let en = uc.uen in
  let text =
    Printf.sprintf
      "(fun (mb_x : %s) (_ : %s) (mb_y : %s) => %s)"
      en.m
      en.lty
      en.m
      (apps (du "Cong") [ en.m; uc.ro; "mb_x"; "mb_y" ])
  in
  text, elab gn.st text
;;

(* a normalisation tuple's proof: its item's lemma at the source's
   numbers *)
let norm_pf (gn : gen) (uc : ucong) (pnorm : P.pnorm list)
  : shp -> shp -> int array -> int array -> Constr.t
  =
  let tbl = Hashtbl.create 256 in
  List.iter (fun (it : P.pnorm) -> Hashtbl.replace tbl it.P.nfrom it) pnorm;
  fun sh sh' srcn tgtn ->
    let found =
      Hashtbl.fold
        (fun v (it : P.pnorm) acc ->
          match acc with
          | Some _ -> acc
          | None ->
            if
              v.(0) = sh.sid
              && it.P.nto.(0) = sh'.sid
              && Array.for_all2 (fun q u -> v.(q) = u) sh.pos srcn
              && Array.for_all2 (fun q u -> it.P.nto.(q) = u) sh'.pos tgtn
            then Some it
            else None)
        tbl
        None
    in
    match found with
    | None -> failf "up to: a normalisation tuple of no item"
    | Some it ->
      app (var (item_lemma gn uc sh it)) (Array.map (fun u -> n_constr u) srcn)
;;

(* {1 The proof of a goal} *)

type pdir =
  { d : P.pdir
  ; rels : Game_proof.b
  ; inv : Game_proof.b
  ; game : Game_proof.b
  ; mkeys : (int * int * int) list
  ; mentries : Game_proof.b list
  ; ikeys : (int * int * int) list
  ; ientries : Game_proof.b list
  ; mv : mover
  ; ctab : Constr.t
  ; snd_ : (Constr.t * string * Constr.t) list
  ; aen : enc
  }

(* [Forall InvOk inv]: per entry of the answerer's list, its relation's
   codes and its tuples' proof *)
(* [Forall InvOk entries]: each entry's soundness, [lts_c] in the place of
   the LTS (the answerer's, or a normalisation's [fun x _ y => Cong Ro x
   y]) *)
let forall_ok
      (gn : gen)
      ~(right_answers : bool)
      ~(en : enc)
      ~(lts_c : Constr.t)
      (entries : Constr.t list)
      (snds : (Constr.t * string * Constr.t) list)
  : Constr.t
  =
  let a_c = en.a_c in
  let rty = rel_ty a_c in
  let pred =
    app
      (c (if right_answers then "mebi.de.InvOkR" else "mebi.de.InvOkL"))
      [| var en.m; a_c; lts_c; var en.e; var gn.nl_name |]
  in
  let proof (e : Constr.t) (allt, cs, lab) =
    let ent_cs, ent_r =
      match Constr.kind e with
      | Constr.App (_, [| _; _; inner; r |]) ->
        (match Constr.kind inner with
         | Constr.App (_, [| _; _; _; csc |]) -> csc, r
         | _ -> failf "lts proof: an entry not a triple")
      | _ -> failf "lts proof: an entry not a triple"
    in
    let csl = codes_constr cs in
    let tsound csx =
      app (c "mebi.de.TSound") [| var en.m; a_c; lts_c; var en.e; lab; csx |]
    in
    let nw = nowo_constr cs in
    if Bool.not right_answers
    then (
      let wi = within_constr gn.nl cs in
      let a_t = app (c "mebi.de.NoWO") [| csl |] in
      let w_t = app (c "mebi.de.Within") [| csl; var gn.nl_name |] in
      let al_t = app (c "mebi.de.AllT") [| ent_r; tsound csl |] in
      app
        (c "core.and.conj")
        [| a_t
         ; app (c "core.and.type") [| w_t; al_t |]
         ; nw
         ; app (c "core.and.conj") [| w_t; al_t; wi; allt |]
        |])
    else (
      let lcode = app (c "core.list.type") [| c "mebi.dt.code" |] in
      let body x r csp =
        let sh = app (c "mebi.de.Shifted") [| var gn.nl_name; x; csp |] in
        let nwt = app (c "mebi.de.NoWO") [| x |] in
        let al = app (c "mebi.de.AllT") [| r; tsound x |] in
        sh, nwt, al
      in
      let sh1, nw1, al1 =
        body (Constr.mkRel 1) (Vars.lift 1 ent_r) (Vars.lift 1 ent_cs)
      in
      let p =
        Constr.mkLambda
          ( Context.anonR
          , lcode
          , app
              (c "core.and.type")
              [| sh1; app (c "core.and.type") [| nw1; al1 |] |] )
      in
      let sh0, nw0, al0 = body csl ent_r ent_cs in
      app
        (c "core.ex.intro")
        [| lcode
         ; p
         ; csl
         ; app
             (c "core.and.conj")
             [| sh0
              ; app (c "core.and.type") [| nw0; al0 |]
              ; shifted_constr gn.nl cs
              ; app (c "core.and.conj") [| nw0; al0; nw; allt |]
             |]
        |])
  in
  let rec go = function
    | [] -> app (c "mebi.dg.Forall_nil") [| rty; pred |]
    | (e, pf) :: rest ->
      app
        (c "mebi.dg.Forall_cons")
        [| rty
         ; pred
         ; e
         ; list_constr rty (List.map fst rest)
         ; proof e pf
         ; go rest
        |]
  in
  go (List.combine entries snds)
;;

let forall_inv (gn : gen) (pd : pdir) (get : int -> Constr.t) : Constr.t =
  forall_ok
    gn
    ~right_answers:(pd.mv.right = false)
    ~en:pd.aen
    ~lts_c:(Constr.mkIndU (pd.aen.lts, UVars.Instance.empty))
    (List.map (fun e -> e get) pd.ientries)
    pd.snd_
;;

(* the coverage fact of a summand *)
let cov_term
      (gn : gen)
      (sm : P.psum)
      (right : bool)
      get
      (zt : Constr.t)
      (dn : Constr.t)
      kreq
  : Constr.t
  =
  let k_sh, k_csh, k_ckc, k_f, k_g, k_sz, k_ssh, k_hf, k_hg, k_hm = kreq in
  let kc = keep_codes sm in
  let sh = get k_sh in
  let eq = app (c "core.eq.refl") [| c "mebi.dt.dd"; sh |] in
  if right
  then
    app
      (c "mebi.de.cov_of")
      [| var gn.nl_name
       ; zt
       ; n_constr sm.P.sid
       ; sh
       ; get k_csh
       ; codes_constr kc
       ; get k_ckc
       ; dn
       ; get k_f
       ; get k_g
       ; get k_sz
       ; get k_ssh
       ; eq
       ; shifted_constr gn.nl "O"
       ; shifted_constr gn.nl kc
       ; get k_hf
       ; get k_hg
       ; get k_hm
       ; roonly_constr kc
      |]
  else
    app
      (c "mebi.de.cov_of_l")
      [| zt
       ; n_constr sm.P.sid
       ; sh
       ; codes_constr kc
       ; dn
       ; get k_f
       ; get k_g
       ; get k_sz
       ; get k_ssh
       ; eq
       ; get k_hf
       ; get k_hg
       ; get k_hm
       ; roonly_constr kc
      |]
;;

(* some type of the goal's lies above [Set]: the proof's universe
   constraints must be inferred (the tactic types the term once) *)
let above_set = ref false

let qed_term
      env
      ~(name : string)
      ~(log : Proof_log.source)
      ?(global = false)
      (plan : P.t)
      ~(strong : bool)
  : Constr.t * Evd.side_effects option
  =
  let st =
    new_st
      ~constants:(global || Sys.getenv_opt "MEBI_QED_LETS" = None)
      ~global
      name
  in
  univs := Univ.ContextSet.empty;
  el_n := 0;
  as_count := 0;
  el_chars := 0;
  el_time := 0.;
  Hashtbl.reset cases;
  Hashtbl.reset as_tbl;
  CTbl.reset nf_tbl;
  Hashtbl.reset kernel_time;
  as_busy := [];
  as_failed := [];
  Hashtbl.reset vars.names;
  Hashtbl.reset vars.types;
  vn_tbl := [];
  items_tbl := [];
  Hashtbl.reset usms_tbl;
  let value_of = Hashtbl.create 64 in
  List.iter (fun (i, t) -> Hashtbl.replace value_of i t) plan.P.values;
  let nl, nr = plan.P.widths in
  let nl_name = def st "nL" (nat_constr nl) (c "num.nat.type") in
  let gn =
    { st; plan; value_of; stypes = []; nl; nl_name; svals = []; nsd = [] }
  in
  let sys_l, sys_r =
    if plan.P.b_first
    then plan.P.sys_b, plan.P.sys_a
    else plan.P.sys_a, plan.P.sys_b
  in
  let enl = encoding gn ~tag:"L" sys_l
  and enr = encoding gn ~tag:"R" sys_r in
  above_set
  := List.exists
       (fun ty ->
         let sigma = Evd.from_env env in
         match
           EConstr.ESorts.kind
             sigma
             (Retyping.get_sort_of env sigma (EConstr.of_constr ty))
         with
         | Sorts.Set | Sorts.Prop | Sorts.SProp -> false
         | _ -> true)
       ([ enl.m_c; enr.m_c; enl.a_c ] @ List.map fst gn.stypes);
  if !above_set
  then (
    (* the constants built in OCaml, applied to the goal's types *)
    let lN = app (c "core.list.type") [| n_ty () |] in
    let pty m = arrow m (arrow lN Constr.mkProp) in
    let ml = enl.m_c
    and mr = enr.m_c
    and a = enl.a_c in
    let ra = rel_ty a in
    let per_t =
      [ "mebi.de.trie"
      ; "mebi.de.tleaf"
      ; "mebi.de.tnode"
      ; "mebi.de.val"
      ; "mebi.de.TAll"
      ; "mebi.de.val_all"
      ; "mebi.de.eq_rew"
      ]
    and per_m =
      [ "mebi.de.enc"
      ; "mebi.de.enc_ind"
      ; "mebi.dg.LenL"
      ; "mebi.de.tsound_tmpl"
      ]
    and per_ma =
      [ "mebi.de.TSound"
      ; "mebi.de.tsound_ro"
      ; "mebi.de.tsound_rw"
      ; "mebi.de.InvOkL"
      ; "mebi.de.InvOkR"
      ; "mebi.de.soundL_of"
      ; "mebi.de.soundR_of"
      ; "mebi.de.StepInR"
      ; "mebi.de.StepInL"
      ; "mebi.de.stepinR_of"
      ; "mebi.de.stepinL_of"
      ; "mebi.de.CompleteTabR"
      ; "mebi.de.CompleteTabL"
      ; "mebi.de.completeR_of"
      ; "mebi.de.completeL_of"
      ]
    and per_mma =
      [ "mebi.dg.cert_strong_sim"
      ; "mebi.dg.cert_weak_sim"
      ; "mebi.dg.cert_strong_bisim"
      ; "mebi.dg.cert_weak_bisim"
      ; "mebi.dg.strong_bisimilar_sym"
      ; "mebi.dg.weak_bisimilar_sym"
      ]
    and per_a =
      [ "mebi.dg.rel"
      ; "mebi.dg.VWSim"
      ; "mebi.dg.VSSim"
      ; "mebi.dg.MoveW"
      ; "mebi.dg.MoveS"
      ; "mebi.dg.Reach"
      ; "mebi.dg.OkL"
      ; "mebi.dg.OkS"
      ; "mebi.dg.all_back_reach"
      ; "mebi.dg.all_back_okl"
      ; "mebi.dg.all_back_okl_tau"
      ; "mebi.dg.all_back_strong"
      ; "mebi.dg.all_reach_self"
      ; "mebi.dg.movew_l"
      ; "mebi.dg.movew_tau"
      ; "mebi.dg.moves_of"
      ; "mebi.dg.vwsim_of"
      ; "mebi.dg.vssim_of"
      ]
    and per_ra =
      [ "mebi.dg.Forall"
      ; "mebi.dg.Forall_nil"
      ; "mebi.dg.Forall_cons"
      ; "mebi.dg.In"
      ]
    in
    let ts = List.map fst gn.stypes @ [ pty ml; pty mr; lN; ml; mr ] in
    let probes =
      List.concat_map (fun k -> List.map (fun t -> c k, [| t |]) ts) per_t
      @ List.concat_map (fun k -> [ c k, [| ml |]; c k, [| mr |] ]) per_m
      @ List.concat_map (fun k -> [ c k, [| ml; a |]; c k, [| mr; a |] ]) per_ma
      @ List.concat_map
          (fun k -> [ c k, [| ml; mr; a |]; c k, [| mr; ml; a |] ])
          per_mma
      @ List.map (fun k -> c k, [| a |]) per_a
      @ List.map (fun k -> c k, [| ra |]) per_ra
    in
    let sigma =
      List.fold_left
        (fun sigma (h, args) ->
          try
            fst (Typing.type_of env sigma (EConstr.of_constr (app h args)))
          with
          | e when CErrors.noncritical e -> sigma)
        (Evd.from_env env)
        probes
    in
    univs := Univ.ContextSet.union !univs (Evd.universe_context_set sigma));
  set_ready st;
  (* up to reordering (doc/upto.md, route B): each side's congruence *)
  let upto_on = sys_l.P.pnorm <> [] || sys_r.P.pnorm <> [] in
  let ucl, ucr =
    if upto_on
    then (
      if strong then failf "lts proof: up to reordering for a strong goal";
      let l = upto_cong gn ~also:enr enl in
      mark "up to: the left congruence, %d reorderings" (List.length l.disjs);
      let r =
        if Names.Ind.CanOrd.equal enl.lts enr.lts then l else upto_cong gn enr
      in
      mark "up to: the right congruence, %d reorderings" (List.length r.disjs);
      Some l, Some r)
    else None, None
  in
  let lenl = lenl gn enl in
  let start = plan.P.start in
  (* up to reordering: the start's own vectors, normalised by the
     certificate into [z] *)
  let start_v =
    if upto_on then Array.append sys_l.P.pstart_v sys_r.P.pstart_v else start
  in
  let hl, pvec = start_proof gn enl sys_l.P.pstart (Array.sub start_v 0 nl)
  and hr, qvec = start_proof gn enr sys_r.P.pstart (Array.sub start_v nl nr) in
  let a_c = enl.a_c in
  let rq = Game_proof.new_reqs () in
  Game_proof.lets := [];
  let ask = Game_proof.ask rq in
  let t = { Game_proof.a_ty = a_c; rel_ty = rel_ty a_c } in
  let label_term l =
    match List.assoc_opt l plan.P.label_terms with
    | Some x -> x
    | None -> failf "lts proof: label %d has no term" l
  in
  let f_z = assume st "z" (c "mebi.dt.dd") in
  let kz = ask (Dd_cert.Node plan.P.z) in
  let dirs =
    List.map
      (fun (d : P.pdir) ->
        let rels, inv, game =
          Game_proof.direction
            ~upto:upto_on
            rq
            t
            ~strong
            ~label_term
            ~tau:plan.P.tau
            plan
            d
        in
        let mkeys = !Game_proof.move_keys
        and mentries = !Game_proof.move_entries
        and ikeys = !Game_proof.inv_keys
        and ientries = !Game_proof.inv_entries_out in
        let right = d.P.mover_a = plan.P.b_first in
        let men, aen = if right then enr, enl else enl, enr in
        let f_rs = assume st "rs" (app (c "core.list.type") [| rel_ty a_c |]) in
        let mv =
          { men
          ; right
          ; dir = d
          ; f_rs
          ; f_z
          ; used_rels = []
          ; used_mem = []
          ; mem_tbl = Hashtbl.create 1024
          ; packs = []
          ; covs = []
          ; tidx = Hashtbl.create 8
          }
        in
        mark
          "direction: mover %s, %d answerer relations"
          (if right then "right" else "left")
          (List.length ikeys);
        let ctab = completeness gn mv in
        mark "completeness: %d elaborations, %d chars" !el_n !el_chars;
        let snd_ = List.map (fun key -> soundness_rel gn aen key) ikeys in
        mark "soundness: %d elaborations, %d chars" !el_n !el_chars;
        { d
        ; rels
        ; inv
        ; game
        ; mkeys
        ; mentries
        ; ikeys
        ; ientries
        ; mv
        ; ctab
        ; snd_
        ; aen
        })
      plan.P.dirs
  in
  (* up to reordering: the normalisation relations' soundness, by item;
     the start's normalisation steps, their tuples' memberships *)
  let nkl, nkr = if upto_on then Game_proof.norm_keys plan else [], [] in
  let nsl_entries = !Game_proof.nsl_entries_out
  and nsr_entries = !Game_proof.nsr_entries_out in
  let norm_snd (uc : ucong option) en (pn : P.pnorm list) keys =
    match uc with
    | None -> [], Constr.mkProp
    | Some uc ->
      let tx, tc = cong_lts gn uc in
      ( List.map
          (fun key -> soundness_rel ~norm:(tx, tc, norm_pf gn uc pn) gn en key)
          keys
      , tc )
  in
  let snl, lnl = norm_snd ucl enl sys_l.P.pnorm nkl in
  let snr, lnr = norm_snd ucr enr sys_r.P.pnorm nkr in
  if upto_on
  then
    mark
      "up to: normalisation sound, %d and %d relations"
      (List.length snl)
      (List.length snr);
  let interl (a : int array) (b : int array) =
    Array.concat (Array.to_list (Array.mapi (fun i x -> [| x; b.(i) |]) a))
  in
  let rep_l = Array.sub start 0 nl
  and rep_r = Array.sub start nl nr
  and ex_l = Array.sub start_v 0 nl
  and ex_r = Array.sub start_v nl nr in
  let kstart keys rep ex =
    if (not upto_on) || rep = ex
    then None
    else (
      match keys with
      | [ (_, _, r) ] -> Some (ask (Dd_cert.Mem (r, interl rep ex)))
      | _ -> failf "lts proof: a start normalised by no relation")
  in
  let kstart_l = kstart nkl rep_l ex_l
  and kstart_r = kstart nkr rep_r ex_r in
  (* the facts the completeness proofs name, requested *)
  let freqs =
    List.map
      (fun pd ->
        let mv = pd.mv in
        ( pd
        , List.map
            (fun (((_, _, r) as k), (rr, inn)) ->
              k, rr, inn, ask (Dd_cert.Node r))
            mv.used_rels
        , List.map
            (fun (((_, _, k), tu), x) ->
              x, ask (Dd_cert.Mem (k, Array.of_list tu)))
            (List.filter
               (fun (_, x) ->
                 st.effs = None
                 || Hashtbl.mem st.used_facts (Names.Id.of_string x))
               (List.rev mv.used_mem))
        , List.map
            (fun ((sm : P.psum), fd, fcov) ->
              ( sm
              , fd
              , fcov
              , ask (Dd_cert.Node sm.P.dsee.P.rel)
              , ( ask (Dd_cert.Node sm.P.sh.P.rel)
                , ask (Dd_cert.Codes sm.P.sh.P.meta)
                , ask (Dd_cert.Codes sm.P.dsee.P.meta)
                , ask (Dd_cert.Node sm.P.f)
                , ask (Dd_cert.Node sm.P.g)
                , ask (Dd_cert.Srt plan.P.z)
                , ask (Dd_cert.Srt sm.P.sh.P.rel)
                , ask
                    (Dd_cert.Img_complete
                       (plan.P.z, sm.P.sh.P.rel, sm.P.sh.P.meta))
                , ask
                    (Dd_cert.Img_sound
                       (plan.P.z, sm.P.dsee.P.rel, sm.P.dsee.P.meta))
                , ask (Dd_cert.Minus (sm.P.f, sm.P.g)) ) ))
            mv.covs ))
      dirs
  in
  let kmem = ask (Dd_cert.Mem (plan.P.z, plan.P.start)) in
  (* the coverages' memberships, each pack checked as a constant of its
     own, out of the proof's term: a chunk per relation the packs name
     (its diagram: nodes only), then a chunk per pack (its memberships,
     and its relations' nodes, its own) *)
  let packs = List.concat_map (fun pd -> List.rev pd.mv.packs) dirs in
  let rr_node = Hashtbl.create 64
  and mem_req = Hashtbl.create 1024 in
  List.iter
    (fun pd ->
      List.iter
        (fun ((_, _, k), (rr, _)) -> Hashtbl.replace rr_node rr k)
        pd.mv.used_rels;
      List.iter
        (fun (((_, _, k), tu), x) ->
          Hashtbl.replace mem_req x (k, Array.of_list tu))
        pd.mv.used_mem)
    dirs;
  (* a pack's trie: its constant, and the relations it is applied to *)
  let trie_of mt =
    match Constr.kind (Hashtbl.find st.consts (Names.Id.of_string mt)) with
    | Constr.App (h, args) ->
      ( h
      , Array.to_list
          (Array.map
             (fun a ->
               match Constr.kind a with
               | Constr.Var id -> Names.Id.to_string id
               | _ -> failf "lts proof: a pack's trie over a non-fact")
             args) )
    | _ -> failf "lts proof: a pack's trie not over its relations"
  in
  let pack_rels = List.map (fun (_, mt, _) -> snd (trie_of mt)) packs in
  let rels_used =
    List.sort_uniq
      compare
      (List.concat_map (List.map (Hashtbl.find rr_node)) pack_rels)
  in
  let groups =
    List.map (fun k -> [ Dd_cert.Node k ]) rels_used
    @ List.map2
        (fun (_, _, entries) rrs ->
          List.map
            (fun (_, id, _) ->
              let k, tu = Hashtbl.find mem_req id in
              Dd_cert.Mem (k, tu))
            entries
          @ List.map (fun rr -> Dd_cert.Node (Hashtbl.find rr_node rr)) rrs)
        packs
        pack_rels
  in
  (* the facts, from the log, handed a line at a time to their chunk's
     reader *)
  let readers =
    Array.init (1 + List.length groups) (fun _ -> Dd_read.proof_reader env)
  in
  let cur = ref (-1) in
  let feed l =
    if String.length l > 0 && l.[0] = 'Q' && !cur < 0
    then Array.iter (fun r -> r.Dd_read.feed l) readers
    else (
      if String.length l > 0 && l.[0] = 'C' then incr cur;
      readers.(!cur).Dd_read.feed l)
  in
  let wst =
    Dd_cert.write_log
      ~requests:(List.rev rq.Game_proof.rev)
      ~groups
      ~log
      (Dd_cert.To_lines feed)
  in
  (* the facts written, nothing reads Sylvan's nodes any more: for a large
     certificate, its log and tables given back now, before the kernel
     checks the pieces (collection back on; its table shrunk to fit) *)
  if wst.Dd_cert.facts > 100_000
  then (
    (match log with
     | Proof_log.Memory -> Ldd.log_free ()
     | Proof_log.File _ -> ());
    Gc.full_major ();
    Ldd.gc ();
    mark "Sylvan's tables given back");
  (* the main chunk in pieces, each a constant the kernel checks now (its
     nodes by number, its facts by segment): the term names them *)
  let ctx =
    (* in pieces for a command only: a tactic's pieces are side effects,
       which Rocq keeps in their typed form until [Qed] (more memory than
       one term) *)
    if Bool.not global
    then readers.(0).Dd_read.finish ()
    else
      readers.(0).Dd_read.finish_pieces
        ~declare:(fun n b ty opaque -> declare st (n, b, ty, opaque))
        ~seg:
          (match Sys.getenv_opt "MEBI_CHUNK_FACTS" with
           | Some k -> (try int_of_string k with _ -> 20_000)
           | None -> 20_000)
        ()
  in
  mark
    "facts: %d, %d lets in the term"
    wst.Dd_cert.facts
    (List.length ctx.Dd_read.lets);
  let lets_around (lets : (Constr.t * Constr.types) list) (body : Constr.t) =
    List.fold_left
      (fun acc (b, ty) -> Constr.mkLetIn (Context.anonR, b, ty, acc))
      body
      (List.rev lets)
  in
  (* each relation's diagram, a constant: its nodes as [let]s *)
  let rr_const = Hashtbl.create 16 in
  List.iteri
    (fun i k ->
      let cg = readers.(1 + i).Dd_read.finish () in
      let body =
        lets_around cg.Dd_read.lets (cg.Dd_read.req 0 cg.Dd_read.depth)
      in
      Hashtbl.replace rr_const k (def st "RR" body (c "mebi.dt.dd")))
    rels_used;
  (* each pack, a constant: its memberships' facts as [let]s, then [TAll]
     of the trie of their statements over its own nodes, given the
     statement over the relations' constants (the two compared once, by
     the trie's arguments) *)
  let prop = Constr.mkProp
  and posty = c "num.pos.type" in
  let q0 =
    Constr.mkLambda
      ( Context.anonR
      , posty
      , Constr.mkLambda (Context.anonR, prop, Constr.mkRel 1) )
  in
  let shift k q =
    Constr.mkLambda
      ( Context.anonR
      , posty
      , app (Vars.lift 1 q) [| app (c k) [| Constr.mkRel 1 |] |] )
  in
  let first_pack = 1 + List.length rels_used in
  let pack_consts =
    List.mapi
      (fun i ((mems, mt, entries), rrs) ->
        let cg = readers.(first_pack + i).Dd_read.finish () in
        let d = cg.Dd_read.depth in
        let nmem = List.length entries in
        let local =
          List.mapi (fun j rr -> rr, cg.Dd_read.req (nmem + j) d) rrs
        in
        let stmt t =
          let rec go t =
            match Constr.kind t with
            | Constr.Var id ->
              (match List.assoc_opt (Names.Id.to_string id) local with
               | Some x -> x
               | None -> t)
            | _ -> Constr.map go t
          in
          go t
        in
        let entries = List.mapi (fun j (k, _, ty) -> k, j, stmt ty) entries in
        let tr =
          List.fold_left (fun t (k, _, ty) -> insert t k ty) Leaf entries
        in
        let pf_of = Hashtbl.create 64 in
        List.iter
          (fun (k, j, _) -> Hashtbl.replace pf_of k (cg.Dd_read.req j d))
          entries;
        (* each subtrie: its term (made once) and the proof of its [TAll] *)
        let rec go q t acc dp : Constr.t * Constr.t =
          match t with
          | Leaf -> trie_constr prop Leaf, c "core.True.I"
          | Node (v, l, r) ->
            let key = acc + (1 lsl dp) in
            let a_ty, a_pf =
              match v with
              | Some x -> x, Hashtbl.find pf_of key
              | None -> c "core.True.type", c "core.True.I"
            in
            let ql = shift "num.pos.xO" q
            and qr = shift "num.pos.xI" q in
            let lt, lp = go ql l acc (dp + 1)
            and rt, rp = go qr r (acc + (1 lsl dp)) (dp + 1) in
            let tl = app (c "mebi.de.TAll") [| prop; ql; lt |]
            and tr' = app (c "mebi.de.TAll") [| prop; qr; rt |] in
            ( app
                (c "mebi.de.tnode")
                [| prop
                 ; (match v with
                    | Some x -> app (c "core.option.Some") [| prop; x |]
                    | None -> app (c "core.option.None") [| prop |])
                 ; lt
                 ; rt
                |]
            , app
                (c "core.and.conj")
                [| a_ty
                 ; app (c "core.and.type") [| tl; tr' |]
                 ; a_pf
                 ; app (c "core.and.conj") [| tl; tr'; lp; rp |]
                |] )
        in
        let pf = snd (go q0 tr 0 0) in
        let head, _ = trie_of mt in
        let tall_over args =
          app (c "mebi.de.TAll") [| prop; q0; app head (Array.of_list args) |]
        in
        let ty_local = tall_over (List.map snd local)
        and ty =
          tall_over
            (List.map
               (fun rr -> var (Hashtbl.find rr_const (Hashtbl.find rr_node rr)))
               rrs)
        in
        let ascribe t x =
          app (Constr.mkLambda (Context.anonR, t, Constr.mkRel 1)) [| x |]
        in
        let body =
          lets_around cg.Dd_read.lets (ascribe ty (ascribe ty_local pf))
        in
        mems, def st ~opaque:true "pack" body ty)
      (List.combine packs pack_rels)
  in
  mark "facts read";
  let depth = ctx.Dd_read.depth in
  let get k = ctx.Dd_read.req k depth in
  let zt = get kz in
  (* the facts, for their names *)
  let subst =
    (f_z, zt)
    :: List.concat_map
         (fun (pd, rels, mems, covs) ->
           let mv = pd.mv in
           let entries = List.map (fun e -> e get) pd.mentries in
           let index k =
             let rec find i = function
               | [] -> failf "lts proof: a relation not among the mover's"
               | x :: rest -> if x = k then i else find (i + 1) rest
             in
             find 0 pd.mkeys
           in
           ((mv.f_rs, pd.rels get)
            :: List.concat_map
                 (fun (k, rr, inn, kn) ->
                   [ rr, get kn; inn, Game_proof.in_proof t entries (index k) ])
                 rels)
           @ List.map (fun (x, km) -> x, get km) mems
           @ List.concat_map
               (fun (sm, fd, fcov, kd, kreq) ->
                 let dn = get kd in
                 let cov = cov_term gn sm mv.right get zt dn kreq in
                 ignore fd;
                 [ fcov, cov ])
               covs)
         freqs
  in
  (* each coverage's memberships as one fact: [TAll] of the trie of their
     statements, each entry its fact from the log (a node: its statement,
     and [conj] of its two subtries' [TAll]) *)
  (* each pack named by its constant *)
  let subst = subst @ List.map (fun (mems, n) -> mems, var n) pack_consts in
  (* the theorem, applied *)
  let ml = var enl.m
  and mr = var enr.m in
  let lts_l = Constr.mkIndU (enl.lts, UVars.Instance.empty)
  and lts_r = Constr.mkIndU (enr.lts, UVars.Instance.empty) in
  let x0 = var (term st sys_l.P.pstart)
  and r0 = var (term st sys_r.P.pstart) in
  let common = [| var hl; var hr; get kmem |] in
  let of_dir pd =
    let game = pd.game get
    and inv = pd.inv get
    and rels = var pd.mv.f_rs in
    let fa = forall_inv gn pd get in
    if pd.mv.right
    then
      ( rels
      , inv
      , game
      , app
          (c "mebi.de.completeR_of")
          [| mr; a_c; lts_r; var enr.e; var nl_name; var f_z; rels; pd.ctab |]
      , app
          (c "mebi.de.soundL_of")
          [| ml; a_c; lts_l; var enl.e; var nl_name; inv; var lenl; fa |] )
    else
      ( rels
      , inv
      , game
      , app
          (c "mebi.de.completeL_of")
          [| ml
           ; a_c
           ; lts_l
           ; var enl.e
           ; var nl_name
           ; var f_z
           ; rels
           ; var lenl
           ; pd.ctab
          |]
      , app
          (c "mebi.de.soundR_of")
          [| mr; a_c; lts_r; var enr.e; var nl_name; inv; fa |] )
  in
  (* up to reordering: the congruences, the normalisation lists and
     their soundness, the start normalised into [z] *)
  let upto_args () =
    let ucl = Stdlib.Option.get ucl
    and ucr = Stdlib.Option.get ucr in
    let rty = rel_ty a_c in
    let nsl = List.map (fun e -> e get) nsl_entries
    and nsr = List.map (fun e -> e get) nsr_entries in
    let nsl_c = list_constr rty nsl
    and nsr_c = list_constr rty nsr in
    let eql = app (c "mebi.du.Cong") [| ml; var ucl.ro |]
    and eqr = app (c "mebi.du.Cong") [| mr; var ucr.ro |] in
    let fal = forall_ok gn ~right_answers:false ~en:enl ~lts_c:lnl nsl snl
    and far = forall_ok gn ~right_answers:true ~en:enr ~lts_c:lnr nsr snr in
    let snl_pf =
      app
        (c "mebi.du.soundNL_of")
        [| ml
         ; a_c
         ; var enl.e
         ; eql
         ; nsl_c
         ; app
             (c "mebi.de.soundL_of")
             [| ml; a_c; lnl; var enl.e; var nl_name; nsl_c; var lenl; fal |]
        |]
    and snr_pf =
      app
        (c "mebi.du.soundNR_of")
        [| mr
         ; a_c
         ; var enr.e
         ; var nl_name
         ; eqr
         ; nsr_c
         ; app
             (c "mebi.de.soundR_of")
             [| mr; a_c; lnr; var enr.e; var nl_name; nsr_c; far |]
        |]
    in
    let ent_parts (e : Constr.t) =
      match Constr.kind e with
      | Constr.App (_, [| _; _; inner; r |]) ->
        (match Constr.kind inner with
         | Constr.App (_, [| _; _; _; csc |]) -> csc, r
         | _ -> failf "lts proof: an entry not a triple")
      | _ -> failf "lts proof: an entry not a triple"
    in
    let lst a = nlist_constr (Array.to_list a) in
    let napp a b = app (c "mebi.de.app") [| n_ty (); a; b |] in
    let lcode = app (c "core.list.type") [| c "mebi.dt.code" |] in
    let nat = c "num.nat.type" in
    let len a = app (c "mebi.de.length") [| n_ty (); a |] in
    let normz =
      app
        (c "mebi.du.normz_refl")
        [| a_c; nsl_c; nsr_c; zt; napp (lst rep_l) (lst rep_r); get kmem |]
    in
    let normz =
      match kstart_r with
      | None -> normz
      | Some km ->
        let e = List.hd nsr in
        let cs, r = ent_parts e in
        app
          (c "mebi.du.normz_r")
          [| a_c
           ; nsl_c
           ; nsr_c
           ; zt
           ; cs
           ; r
           ; lst rep_l
           ; lst rep_r
           ; lst ex_r
           ; Game_proof.in_proof t nsr 0
           ; get km
           ; app (c "core.eq.refl") [| lcode; cs |]
           ; app (c "core.eq.refl") [| nat; len (lst rep_r) |]
           ; normz
          |]
    in
    let normz =
      match kstart_l with
      | None -> normz
      | Some km ->
        let e = List.hd nsl in
        let cs, r = ent_parts e in
        app
          (c "mebi.du.normz_l")
          [| a_c
           ; nsl_c
           ; nsr_c
           ; zt
           ; cs
           ; r
           ; lst rep_l
           ; lst ex_l
           ; lst ex_r
           ; Game_proof.in_proof t nsl 0
           ; get km
           ; app (c "core.eq.refl") [| lcode; cs |]
           ; app (c "core.eq.refl") [| nat; len (lst rep_l) |]
           ; normz
          |]
    in
    ( [| ml
       ; mr
       ; a_c
       ; lts_l
       ; lts_r
       ; var enl.e
       ; var enr.e
       ; var f_z
       ; var nl_name
       ; eql
       ; eqr
       ; nsl_c
       ; nsr_c
       ; var ucl.equiv
       ; var ucr.equiv
       ; var lenl
       ; snl_pf
       ; snr_pf
      |]
    , [| x0; r0; pvec; qvec; var hl; var hr; normz |] )
  in
  let body =
    match dirs with
    | [ pd ] when upto_on ->
      if Bool.not pd.mv.right
      then failf "lts proof: a simulation whose mover comes first";
      let relr, invl, g1, c1, s1 = of_dir pd in
      let pre, post = upto_args () in
      app
        (c "mebi.du.cert_weak_sim_upto")
        (Array.concat [ pre; [| relr; invl; g1; c1; s1 |]; post ])
    | [ pa; pb ] when upto_on ->
      let pr, pl = if pa.mv.right then pa, pb else pb, pa in
      let relr, invl, g1, c1, s1 = of_dir pr
      and rell, invr, g2, c2, s2 = of_dir pl in
      let pre, post = upto_args () in
      app
        (c "mebi.du.cert_weak_bisim_upto")
        (Array.concat
           [ pre; [| relr; invl; rell; invr; g1; c1; s1; g2; c2; s2 |]; post ])
    | [ pd ] ->
      if Bool.not pd.mv.right
      then failf "lts proof: a simulation whose mover comes first";
      let relr, invl, g1, c1, s1 = of_dir pd in
      app
        (c
           (if strong
            then "mebi.dg.cert_strong_sim"
            else "mebi.dg.cert_weak_sim"))
        (Array.append
           [| ml
            ; mr
            ; a_c
            ; lts_l
            ; lts_r
            ; var enl.e
            ; var enr.e
            ; var f_z
            ; var nl_name
            ; relr
            ; invl
            ; g1
            ; var lenl
            ; c1
            ; s1
            ; x0
            ; r0
            ; pvec
            ; qvec
           |]
           common)
    | [ pa; pb ] ->
      let pr, pl = if pa.mv.right then pa, pb else pb, pa in
      let relr, invl, g1, c1, s1 = of_dir pr
      and rell, invr, g2, c2, s2 = of_dir pl in
      app
        (c
           (if strong
            then "mebi.dg.cert_strong_bisim"
            else "mebi.dg.cert_weak_bisim"))
        (Array.append
           [| ml
            ; mr
            ; a_c
            ; lts_l
            ; lts_r
            ; var enl.e
            ; var enr.e
            ; var f_z
            ; var nl_name
            ; relr
            ; invl
            ; rell
            ; invr
            ; var lenl
            ; g1
            ; c1
            ; s1
            ; g2
            ; c2
            ; s2
            ; x0
            ; r0
            ; pvec
            ; qvec
           |]
           common)
    | _ -> failf "lts proof: a plan with %d directions" (List.length dirs)
  in
  let body =
    if List.length dirs = 2 && plan.P.b_first
    then
      app
        (c
           (if strong
            then "mebi.dg.strong_bisimilar_sym"
            else "mebi.dg.weak_bisimilar_sym"))
        [| ml; mr; a_c; lts_l; lts_r; x0; r0; body |]
    else body
  in
  mark "theorem applied";
  let body = close_defs (List.rev !Game_proof.lets) body in
  (* the definitions over the facts, as [let]s; then the facts *)
  let body = close_defs (List.rev st.fdefs) body in
  (* the constants, named *)
  let body = to_consts st body in
  mark "definitions over the facts closed";
  let body =
    Vars.replace_vars
      (List.map (fun (n, x) -> Names.Id.of_string n, to_consts st x) subst)
      body
  in
  mark "facts substituted";
  let term =
    List.fold_left
      (fun acc (b, ty) -> Constr.mkLetIn (Context.anonR, b, ty, acc))
      body
      (List.rev ctx.Dd_read.lets)
  in
  (* the named definitions, outermost *)
  let r = close_defs (List.rev st.named) term in
  mark
    "%d elaborations, %d chars, %.2fs; %d constants, %d named, %d over facts, \
     %d facts lets"
    !el_n
    !el_chars
    !el_time
    st.nconst
    (List.length st.named)
    (List.length st.fdefs)
    (List.length ctx.Dd_read.lets);
  mark
    "kernel, per kind of constant: %s"
    (String.concat
       ", "
       (List.map
          (fun (k, (n, t)) -> Printf.sprintf "%s %d %.2fs" k n t)
          (List.sort
             (fun (_, (_, a)) (_, (_, b)) -> compare b a)
             (Hashtbl.fold (fun k v acc -> (k, v) :: acc) kernel_time []))));
  (* the constraints the constants declared are the proof's already *)
  univs := Univ.ContextSet.diff !univs st.upushed;
  r, if global then None else st.effs
;;
