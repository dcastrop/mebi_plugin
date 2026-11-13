(* The game on vectors for a goal (doc/certificate.md, "The theorem from
   the certificate"): from the engine's plan of its logged check
   ([Learn.Plan]) and the facts the log gives ([Dd_cert] with requests,
   [Dd_read.proof_reader]), a term of [VWSim rs inv z] (or [VSSim]) built
   from theories/DdGame.v's lemmas, each applied to nodes and facts: no
   lambda, nothing computed. *)

module P = Learn.Plan

let c s =
  UnivGen.constr_of_monomorphic_global (Global.env ()) (Rocqlib.lib_ref s)
;;

let app f a = Constr.mkApp (f, a)
let failf fmt = Printf.ksprintf failwith fmt

(* {1 Requests} *)

type reqs =
  { tbl : (Dd_cert.request, int) Hashtbl.t
  ; mutable rev : Dd_cert.request list
  ; mutable n : int
  }

let new_reqs () = { tbl = Hashtbl.create 256; rev = []; n = 0 }

let ask (rq : reqs) (r : Dd_cert.request) : int =
  match Hashtbl.find_opt rq.tbl r with
  | Some k -> k
  | None ->
    let k = rq.n in
    Hashtbl.add rq.tbl r k;
    rq.rev <- r :: rq.rev;
    rq.n <- k + 1;
    k
;;

(* a term once the requests are answered: [get k] is request [k]'s *)
type b = (int -> Constr.t) -> Constr.t

(* {1 Shared sub-proofs}

   A sub-proof used more than once (an image's [All] fact, a closure
   round's) is built once, as a [let] the caller places around the game's
   proof: [lets], last first, each (name, body, type). *)
let lets : (string * Constr.t * Constr.t) list ref = ref []
let let_count = ref 0

let share (ty : b) (f : b) : b =
  let cache = ref None in
  fun get ->
    match !cache with
    | Some v -> v
    | None ->
      let body = f get in
      let typ = ty get in
      incr let_count;
      let n = Printf.sprintf "mb_g%d" !let_count in
      lets := (n, body, typ) :: !lets;
      let v = Constr.mkVar (Names.Id.of_string n) in
      cache := Some v;
      v
;;

let all_ty (x : b) (pred : b) : b =
  fun get -> app (c "mebi.dg.All") [| x get; pred get |]
;;

(* {1 The terms} *)

type terms =
  { a_ty : Constr.t (** the labels' [A] *)
  ; rel_ty : Constr.t (** [rel A] *)
  }

let entry (t : terms) (lab : Constr.t) (cs : Constr.t) (r : Constr.t) : Constr.t
  =
  let code_list = app (c "core.list.type") [| c "mebi.dt.code" |] in
  let opt_a = app (c "core.option.type") [| t.a_ty |] in
  let inner_ty = app (c "core.prod.type") [| opt_a; code_list |] in
  app
    (c "core.prod.intro")
    [| inner_ty
     ; c "mebi.dt.dd"
     ; app (c "core.prod.intro") [| opt_a; code_list; lab; cs |]
     ; r
    |]
;;

let list_of (t : terms) (xs : Constr.t list) : Constr.t =
  List.fold_right
    (fun x acc -> app (c "core.list.cons") [| t.rel_ty; x; acc |])
    xs
    (app (c "core.list.nil") [| t.rel_ty |])
;;

(* [In x l] for the [i]th element [x] of [xs] ([l] the list of [xs]) *)
let in_proof (t : terms) (xs : Constr.t list) (i : int) : Constr.t =
  let rec go xs i =
    match xs with
    | [] -> failf "game: an element not in its list"
    | y :: rest ->
      let x = List.nth xs i in
      let tl = list_of t rest in
      let eq_ty = app (c "core.eq.type") [| t.rel_ty; y; x |] in
      let in_ty = app (c "mebi.dg.In") [| t.rel_ty; x; tl |] in
      if i = 0
      then
        app
          (c "mebi.dg.or_introl")
          [| eq_ty; in_ty; app (c "core.eq.refl") [| t.rel_ty; x |] |]
      else app (c "mebi.dg.or_intror") [| eq_ty; in_ty; go rest (i - 1) |]
  in
  go xs i
;;

(* {1 One direction of the check} *)

(* the last direction's relations, by key (label, meta, relation): the
   mover's in the order of its list, the answerer's backwards *)
let move_keys : (int * int * int) list ref = ref []
let inv_keys : (int * int * int) list ref = ref []
let move_entries : b list ref = ref []
let inv_entries_out : b list ref = ref []

(* up to reordering (doc/upto.md, route B): the normalisation relations
   backwards, of the vector's first part (label -1) and its second (-2),
   from every direction's closures, in order: the same for both
   directions *)
let norm_keys (plan : P.t) : (int * int * int) list * (int * int * int) list =
  let l = ref []
  and r = ref [] in
  let add (x : P.prel) =
    let k = x.P.label, x.P.meta, x.P.rel in
    if x.P.label = -1 && Bool.not (List.mem k !l) then l := !l @ [ k ];
    if x.P.label = -2 && Bool.not (List.mem k !r) then r := !r @ [ k ]
  in
  let of_closure (cl : P.pclosure) =
    List.iter (fun (rd : P.pround) -> List.iter add rd.P.p.P.rels) cl.P.rounds
  in
  List.iter
    (fun (d : P.pdir) ->
      of_closure d.P.tzc;
      List.iter
        (fun (pl : P.plabel) ->
          match pl.P.okfrom with
          | Some (pi, cl) ->
            List.iter add pi.P.rels;
            of_closure cl
          | None -> ())
        d.P.plabels)
    plan.P.dirs;
  !l, !r
;;

(* the normalisation lists' entries, for the last direction *)
let nsl_entries_out : b list ref = ref []
let nsr_entries_out : b list ref = ref []

let direction
      ?(upto = false)
      (rq : reqs)
      (t : terms)
      ~(strong : bool)
      ~(label_term : int -> Constr.t)
      ~(tau : int option)
      (plan : P.t)
      (d : P.pdir)
  : b * b * b
  =
  let label_term l =
    if l < 0
    then (
      match tau with
      | Some t -> label_term t
      | None -> failf "game: a normalisation with no silent label")
    else label_term l
  in
  let node x = ask rq (Dd_cert.Node x) in
  let z = plan.P.z in
  let kz = node z
  and ksz = ask rq (Dd_cert.Srt z) in
  (* the answerer's relations backwards, each once, in order *)
  let invs = ref [] in
  let add_inv (r : P.prel) =
    if
      r.P.label >= 0 && Bool.not (List.mem (r.P.label, r.P.meta, r.P.rel) !invs)
    then invs := !invs @ [ r.P.label, r.P.meta, r.P.rel ]
  in
  let rels_of_closure (cl : P.pclosure) =
    List.iter
      (fun (rd : P.pround) -> List.iter add_inv rd.P.p.P.rels)
      cl.P.rounds
  in
  rels_of_closure d.P.tzc;
  List.iter
    (fun (pl : P.plabel) ->
      match pl.P.okfrom with
      | Some (pi, cl) ->
        List.iter add_inv pi.P.rels;
        rels_of_closure cl
      | None -> ())
    d.P.plabels;
  let entry_of (l, meta, rel) =
    let kc = ask rq (Dd_cert.Codes meta)
    and kr = node rel in
    fun get -> entry t (label_term l) (get kc) (get kr)
  in
  let inv_entries = List.map entry_of !invs in
  let inv_list get = list_of t (List.map (fun e -> e get) inv_entries) in
  let nkl, nkr = if upto then norm_keys plan else [], [] in
  let nsl_entries = List.map entry_of nkl
  and nsr_entries = List.map entry_of nkr in
  let nsl get = list_of t (List.map (fun e -> e get) nsl_entries)
  and nsr get = list_of t (List.map (fun e -> e get) nsr_entries) in
  let in_keys keys entries (r : P.prel) get =
    let rec find i = function
      | [] -> failf "game: a normalisation relation not in its list"
      | x :: rest ->
        if x = (r.P.label, r.P.meta, r.P.rel) then i else find (i + 1) rest
    in
    in_proof t (List.map (fun e -> e get) entries) (find 0 keys)
  in
  (* the toolkit's lemma, and its arguments before its own: up to
     reordering, the normalisation lists after the answerer's *)
  let lem name get =
    if upto
    then (
      let nname =
        match name with
        | "all_reach_self" -> "all_reachN_self"
        | "movew_tau" -> "movewN_tau"
        | "movew_l" -> "movewN_l"
        | "OkL" -> "OkLN"
        | "all_back_okl" -> "all_back_oklN"
        | "MoveW" -> "MoveWN"
        | x -> failf "game: no up-to version of %s" x
      in
      c ("mebi.du." ^ nname), [| t.a_ty; inv_list get; nsl get; nsr get |])
    else c ("mebi.dg." ^ name), [| t.a_ty; inv_list get |]
  in
  let lapp name get args =
    let f, pre = lem name get in
    app f (Array.append pre args)
  in
  let inv_index (r : P.prel) =
    let rec find i = function
      | [] -> failf "game: a relation not among the answerer's"
      | x :: rest ->
        if x = (r.P.label, r.P.meta, r.P.rel) then i else find (i + 1) rest
    in
    find 0 !invs
  in
  let in_inv (r : P.prel) get =
    in_proof t (List.map (fun e -> e get) inv_entries) (inv_index r)
  in
  let cs_of (r : P.prel) =
    let k = ask rq (Dd_cert.Codes r.P.meta) in
    fun get -> get k
  in
  let nd x =
    let k = node x in
    fun get -> get k
  in
  (* [All x P] along the images of [src] (one per relation), their union
     from the empty set: [img_all r img] is the fact for one image *)
  let union_all (pi : P.pimgs) (pred : b) (img_all : P.prel -> int -> b) : b =
    let alls = List.map2 (fun r i -> i, img_all r i) pi.P.rels pi.P.imgs in
    let all_of x =
      match List.assoc_opt x alls with
      | Some a -> a
      | None -> failf "game: a union's argument is no image"
    in
    let acc0 : b = fun get -> app (c "mebi.dg.all_0") [| pred get |] in
    let res, all =
      List.fold_left
        (fun (_, acc_all) (u : P.punion) ->
          let ku = ask rq (Dd_cert.Union (u.P.acc, u.P.arg)) in
          let na = nd u.P.acc
          and nb = nd u.P.arg
          and nr = nd u.P.res
          and arg_all = all_of u.P.arg in
          ( u.P.res
          , share (all_ty nr pred) (fun get ->
              app
                (c "mebi.dg.all_u")
                [| na get
                 ; nb get
                 ; nr get
                 ; pred get
                 ; get ku
                 ; acc_all get
                 ; arg_all get
                |]) ))
        (0, acc0)
        pi.P.unions
    in
    if res <> pi.P.res then failf "game: a union's result is not the plan's";
    all
  in
  (* [All final P] along a silent closure from [All start P] *)
  let closure
        (cl : P.pclosure)
        (pred : b)
        (start_all : b)
        (img_all : P.prel -> int -> int -> b -> b)
    : b
    =
    let rec go rounds (tz_all : b) (fresh_all : b) =
      match rounds with
      | [] -> tz_all
      | (rd : P.pround) :: rest ->
        let p_all =
          union_all rd.P.p pred (fun r i -> img_all r rd.P.fresh i fresh_all)
        in
        let km = ask rq (Dd_cert.Minus (rd.P.p.P.res, rd.P.tz))
        and ku = ask rq (Dd_cert.Union (rd.P.tz, rd.P.fresh')) in
        let np = nd rd.P.p.P.res
        and ntz = nd rd.P.tz
        and nf' = nd rd.P.fresh'
        and ntz' = nd rd.P.tz' in
        let fresh'_all =
          share (all_ty nf' pred) (fun get ->
            app
              (c "mebi.dg.all_mi")
              [| np get; ntz get; nf' get; pred get; get km; p_all get |])
        in
        let tz'_all =
          share (all_ty ntz' pred) (fun get ->
            app
              (c "mebi.dg.all_u")
              [| ntz get
               ; nf' get
               ; ntz' get
               ; pred get
               ; get ku
               ; tz_all get
               ; fresh'_all get
              |])
        in
        go rest tz'_all fresh'_all
    in
    let _ = cl.P.cstart in
    go cl.P.rounds start_all start_all
  in
  let a get =
    ignore get;
    t.a_ty
  in
  let inv get = inv_list get in
  let zt get = get kz in
  let reach_z get =
    if upto
    then
      app
        (c "mebi.du.ReachN")
        [| t.a_ty
         ; inv get
         ; nsl get
         ; nsr get
         ; app (c "mebi.dg.InZ") [| zt get |]
        |]
    else
      app
        (c "mebi.dg.Reach")
        [| t.a_ty; inv get; app (c "mebi.dg.InZ") [| zt get |] |]
  in
  (* a closure's lemma for relation [r]: the answerer's, or a
     normalisation of either part *)
  let back_lem (r : P.prel) (base : string) get =
    if r.P.label = -1
    then
      ( c ("mebi.du." ^ base ^ "N_nl")
      , [| t.a_ty; inv get; nsl get; nsr get |]
      , in_keys nkl nsl_entries r get )
    else if r.P.label = -2
    then
      ( c ("mebi.du." ^ base ^ "N_nr")
      , [| t.a_ty; inv get; nsl get; nsr get |]
      , in_keys nkr nsr_entries r get )
    else (
      let f, pre =
        if upto
        then
          ( c
              ("mebi.du." ^ base ^ if base = "all_back_okl" then "N_tau" else "N"
              )
          , [| t.a_ty; inv get; nsl get; nsr get |] )
        else
          ( c ("mebi.dg." ^ base ^ if base = "all_back_okl" then "_tau" else "")
          , [| t.a_ty; inv get |] )
      in
      f, pre, in_inv r get)
  in
  (* a sound image backwards: its fact *)
  let posts (r : P.prel) src img =
    let k = ask rq (Dd_cert.Img_sound (src, r.P.rel, r.P.meta)) in
    ignore img;
    fun get -> get k
  in
  let tz_all =
    if strong
    then fun get -> app (c "mebi.dg.all_self") [| zt get |]
    else
      closure
        d.P.tzc
        reach_z
        (fun get -> lapp "all_reach_self" get [| zt get |])
        (fun r src img fresh_all ->
          let ps = posts r src img in
          let ncs = cs_of r
          and nr = nd r.P.rel
          and nsrc = nd src
          and nimg = nd img in
          fun get ->
            let f, pre, inr = back_lem r "all_back_reach" get in
            app
              f
              (Array.append
                 pre
                 [| ncs get
                  ; nr get
                  ; nsrc get
                  ; nimg get
                  ; app (c "mebi.dg.InZ") [| zt get |]
                  ; inr
                  ; ps get
                  ; fresh_all get
                 |]))
  in
  let tz = d.P.tzc.P.final in
  (* each label: the pairs that can answer it, then its moves *)
  let moves = ref [] in
  List.iter
    (fun (pl : P.plabel) ->
      let lab = label_term pl.P.lab in
      let okl get = lapp "OkL" get [| zt get; lab |] in
      let oks get = app (c "mebi.dg.OkS") [| t.a_ty; inv get; zt get; lab |] in
      let ok_all : b =
        match pl.P.okfrom with
        | None ->
          if strong || Some pl.P.lab <> tau
          then failf "game: a label with no answers";
          tz_all
        | Some (pi, cl) ->
          if strong
          then (
            if cl.P.rounds <> [] then failf "game: a strong closure with rounds";
            union_all pi oks (fun r img ->
              let ps = posts r z img in
              let ncs = cs_of r
              and nr = nd r.P.rel
              and nimg = nd img in
              fun get ->
                app
                  (c "mebi.dg.all_back_strong")
                  [| t.a_ty
                   ; inv get
                   ; lab
                   ; ncs get
                   ; nr get
                   ; nimg get
                   ; zt get
                   ; in_inv r get
                   ; ps get
                  |]))
          else (
            let p_all =
              union_all pi okl (fun r img ->
                let ps = posts r tz img in
                let ncs = cs_of r
                and nr = nd r.P.rel
                and ntz = nd tz
                and nimg = nd img in
                fun get ->
                  lapp
                    "all_back_okl"
                    get
                    [| lab
                     ; ncs get
                     ; nr get
                     ; ntz get
                     ; nimg get
                     ; zt get
                     ; in_inv r get
                     ; ps get
                     ; tz_all get
                    |])
            in
            closure cl okl p_all (fun r src img fresh_all ->
              let ps = posts r src img in
              let ncs = cs_of r
              and nr = nd r.P.rel
              and nsrc = nd src
              and nimg = nd img in
              fun get ->
                let f, pre, inr = back_lem r "all_back_okl" get in
                app
                  f
                  (Array.append
                     pre
                     [| lab
                      ; ncs get
                      ; nr get
                      ; nsrc get
                      ; nimg get
                      ; zt get
                      ; inr
                      ; ps get
                      ; fresh_all get
                     |])))
      in
      List.iter
        (fun (mv : P.pmove) ->
          let r = mv.P.mrel in
          let e = entry_of (r.P.label, r.P.meta, r.P.rel) in
          let ncs = cs_of r
          and nr = nd r.P.rel
          and ni = nd mv.P.img
          and nok = nd mv.P.ok
          and ksr = ask rq (Dd_cert.Srt r.P.rel)
          and kc = ask rq (Dd_cert.Img_complete (z, r.P.rel, r.P.meta))
          and km = ask rq (Dd_cert.Minus (mv.P.img, mv.P.ok)) in
          let proof get =
            let common =
              [| ncs get
               ; nr get
               ; ni get
               ; nok get
               ; get ksz
               ; get ksr
               ; get kc
               ; get km
               ; ok_all get
              |]
            in
            if strong
            then
              app
                (c "mebi.dg.moves_of")
                (Array.append [| t.a_ty; inv get; zt get; lab |] common)
            else if Some pl.P.lab = tau
            then lapp "movew_tau" get (Array.append [| zt get |] common)
            else (
              let arg =
                match Constr.kind lab with
                | Constr.App (_, args) when Array.length args = 2 -> args.(1)
                | _ -> failf "game: a visible label that is not [Some _]"
              in
              lapp "movew_l" get (Array.append [| zt get; arg |] common))
          in
          moves := (e, proof) :: !moves)
        pl.P.moves)
    d.P.plabels;
  let moves = List.rev !moves in
  move_keys
  := List.concat_map
       (fun (pl : P.plabel) ->
         List.map
           (fun (mv : P.pmove) ->
             mv.P.mrel.P.label, mv.P.mrel.P.meta, mv.P.mrel.P.rel)
           pl.P.moves)
       d.P.plabels;
  inv_keys := !invs;
  move_entries := List.map fst moves;
  inv_entries_out := inv_entries;
  nsl_entries_out := nsl_entries;
  nsr_entries_out := nsr_entries;
  let rel_list get = list_of t (List.map (fun (e, _) -> e get) moves) in
  let game get =
    let pred =
      if strong
      then app (c "mebi.dg.MoveS") [| t.a_ty; inv get; zt get |]
      else lapp "MoveW" get [| zt get |]
    in
    let rec forall = function
      | [] -> app (c "mebi.dg.Forall_nil") [| t.rel_ty; pred |]
      | (e, proof) :: rest ->
        app
          (c "mebi.dg.Forall_cons")
          [| t.rel_ty
           ; pred
           ; e get
           ; list_of t (List.map (fun (e, _) -> e get) rest)
           ; proof get
           ; forall rest
          |]
    in
    if strong
    then
      app
        (c "mebi.dg.vssim_of")
        [| t.a_ty; rel_list get; inv get; zt get; forall moves |]
    else if upto
    then
      app
        (c "mebi.du.vwsimN_of")
        [| t.a_ty
         ; inv get
         ; nsl get
         ; nsr get
         ; rel_list get
         ; zt get
         ; forall moves
        |]
    else
      app
        (c "mebi.dg.vwsim_of")
        [| t.a_ty; rel_list get; inv get; zt get; forall moves |]
  in
  ignore a;
  rel_list, inv, game
;;
