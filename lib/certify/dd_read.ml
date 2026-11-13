(* See the [.mli]. The file's format: dd_cert.ml. *)

let app f args = Constr.mkApp (f, args)
let failf fmt = Printf.ksprintf failwith fmt

type stats =
  { chunks : int
  ; facts : int
  ; nodes : int
  ; seconds : float
  }

let rss_mb () =
  try
    let ic = open_in "/proc/self/status" in
    let rec go () =
      match input_line ic with
      | l when String.length l > 6 && String.sub l 0 6 = "VmRSS:" ->
        close_in ic;
        Scanf.sscanf
          (String.sub l 6 (String.length l - 6))
          " %d"
          (fun kb -> kb / 1024)
      | _ -> go ()
      | exception End_of_file ->
        close_in ic;
        0
    in
    go ()
  with
  | _ -> 0
;;

(* {1 The terms used} *)

type ctx =
  { env : Environ.env
  ; c : string -> Constr.t
  ; dd : Constr.t
  ; d0 : Constr.t
  ; d1 : Constr.t
  ; nd : Constr.t
  ; nums : (int, Constr.t) Hashtbl.t
  ; poss : (int, Constr.t) Hashtbl.t
  ; lemmas : (string, Constr.t * int * Constr.t) Hashtbl.t
  }

let make_ctx env =
  let c s = UnivGen.constr_of_monomorphic_global env (Rocqlib.lib_ref s) in
  { env
  ; c
  ; dd = c "mebi.dt.dd"
  ; d0 = c "mebi.dt.D0"
  ; d1 = c "mebi.dt.D1"
  ; nd = c "mebi.dt.Nd"
  ; nums = Hashtbl.create 256
  ; poss = Hashtbl.create 64
  ; lemmas = Hashtbl.create 128
  }
;;

(* a positive number, from its binary digits *)
let rec pos ctx k =
  if k < 1 then failf "certificate: positive %d" k;
  match Hashtbl.find_opt ctx.poss k with
  | Some t -> t
  | None ->
    let t =
      if k = 1
      then ctx.c "num.pos.xH"
      else
        app
          (ctx.c (if k land 1 = 1 then "num.pos.xI" else "num.pos.xO"))
          [| pos ctx (k lsr 1) |]
    in
    Hashtbl.add ctx.poss k t;
    t
;;

(* a value of [N] *)
let num ctx k =
  match Hashtbl.find_opt ctx.nums k with
  | Some t -> t
  | None ->
    let t =
      if k = 0
      then ctx.c "num.N.N0"
      else app (ctx.c "num.N.Npos") [| pos ctx k |]
    in
    Hashtbl.add ctx.nums k t;
    t
;;

(* a lemma: its constant, its number of arguments, its conclusion *)
let lemma ctx name =
  match Hashtbl.find_opt ctx.lemmas name with
  | Some x -> x
  | None ->
    let g =
      try Rocqlib.lib_ref ("mebi.dt." ^ name) with
      | _ -> failf "certificate: unknown lemma %s" name
    in
    let c = UnivGen.constr_of_monomorphic_global ctx.env g in
    let ty, _ = Typeops.type_of_global_in_context ctx.env g in
    let decls, concl = Term.decompose_prod_decls ty in
    let x = c, Context.Rel.length decls, concl in
    Hashtbl.add ctx.lemmas name x;
    x
;;

(* {1 Reading} *)

(* a chunk's lines, as integers: a node [| -1; id; v; d; r |], a fact
   [| lemma; id; arg... |], each argument packed with its kind in the low
   three bits (no string kept: a chunk can hold a million facts) *)
let k_node = 0
let k_fact = 1
let k_val = 2
let k_pos = 3
let k_codes = 4
let k_list = 5

(* the lists of numbers of [l] arguments, by index *)
let lists : int array array ref = ref [||]
let nlists = ref 0

let add_list (a : int array) : int =
  if !nlists = Array.length !lists
  then lists := Array.append !lists (Array.make (max 64 !nlists) [||]);
  !lists.(!nlists) <- a;
  incr nlists;
  !nlists - 1
;;

let pack k x = (x lsl 3) lor k
let kind a = a land 7
let payload a = a lsr 3

(* a fact's line, [f id lemma arg...]: [| lemma; id; arg... |], the lemma
   numbered by [lemma_id]; an argument's kind from its letter *)
let scan_fact (lemma_id : string -> int) (line : string) : int array =
  let n = String.length line in
  let i = ref 2 in
  let num () =
    let x = ref 0 in
    while !i < n && line.[!i] >= '0' && line.[!i] <= '9' do
      x := (!x * 10) + Char.code line.[!i] - 48;
      incr i
    done;
    !x
  in
  let skip () =
    while !i < n && line.[!i] = ' ' do
      incr i
    done
  in
  let id = num () in
  skip ();
  let start = !i in
  while !i < n && line.[!i] <> ' ' do
    incr i
  done;
  let lemma = lemma_id (String.sub line start (!i - start)) in
  let args = ref [] in
  skip ();
  while !i < n do
    let c = line.[!i] in
    incr i;
    let a =
      match c with
      | 'n' -> pack k_node (num ())
      | 'f' -> pack k_fact (num ())
      | 'v' -> pack k_val (num ())
      | 'p' -> pack k_pos (num ())
      | 'q' -> pack k_codes (num ())
      | 'l' ->
        let xs = ref [] in
        if !i < n && line.[!i] >= '0' && line.[!i] <= '9'
        then (
          xs := [ num () ];
          while !i < n && line.[!i] = '.' do
            incr i;
            xs := num () :: !xs
          done);
        pack k_list (add_list (Array.of_list (List.rev !xs)))
      | _ -> failf "certificate: bad argument in %s" line
    in
    args := a :: !args;
    skip ()
  done;
  Array.of_list (lemma :: id :: List.rev !args)
;;

let ints line = List.map int_of_string (List.tl (String.split_on_char ' ' line))

let read env ~(declare : int -> Constr.types -> Constr.t -> unit) path : stats =
  let t0 = Unix.gettimeofday () in
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let ctx = make_ctx env in
      let code_ty = ctx.c "mebi.dt.code" in
      let code_of = function
        | 'C' -> ctx.c "mebi.dt.Copy"
        | 'O' -> ctx.c "mebi.dt.RO"
        | 'W' -> ctx.c "mebi.dt.WO"
        | 'X' -> ctx.c "mebi.dt.RW"
        | c -> failf "certificate: bad code %c" c
      in
      (* the header: the lists of codes, [Q i codes] *)
      let qcodes : (int, Constr.t) Hashtbl.t = Hashtbl.create 64 in
      let line = ref (try input_line ic with End_of_file -> "") in
      while String.length !line > 0 && !line.[0] = 'Q' do
        let i, cs =
          match String.split_on_char ' ' !line with
          | [ _; i ] -> int_of_string i, ""
          | _ :: i :: cs :: _ -> int_of_string i, cs
          | _ -> failf "certificate: bad line %s" !line
        in
        let t = ref (app (ctx.c "core.list.nil") [| code_ty |]) in
        for j = String.length cs - 1 downto 0 do
          t := app (ctx.c "core.list.cons") [| code_ty; code_of cs.[j]; !t |]
        done;
        Hashtbl.replace qcodes i !t;
        line := try input_line ic with End_of_file -> ""
      done;
      let codes i =
        match Hashtbl.find_opt qcodes i with
        | Some t -> t
        | None -> failf "certificate: unknown codes %d" i
      in
      let lemma_names = Hashtbl.create 128 in
      let lemma_tbl = ref [||] in
      let lemma_id name =
        match Hashtbl.find_opt lemma_names name with
        | Some k -> k
        | None ->
          let k = Hashtbl.length lemma_names in
          Hashtbl.add lemma_names name k;
          lemma_tbl := Array.append !lemma_tbl [| name, lemma ctx name |];
          k
      in
      let true_ty = ctx.c "core.True.type" in
      let nchunks = ref 0
      and nfacts = ref 0
      and nnodes = ref 0 in
      (* the chunk from [!line] (its [C] line) to its [E] line, declared *)
      let read_chunk () =
        let header = Array.of_list (String.split_on_char ' ' !line) in
        if Array.length header < 3 || header.(2) <> "check"
        then failf "certificate: unknown chunk %s" !line;
        let cid = int_of_string header.(1) in
        let items = ref []
        and final = ref (-1) in
        line := input_line ic;
        while !final < 0 do
          (match !line.[0] with
           | 'n' ->
             (match ints !line with
              | [ i; v; d; r ] -> items := [| -1; i; v; d; r |] :: !items
              | _ -> failf "certificate: bad line %s" !line)
           | 'f' -> items := scan_fact lemma_id !line :: !items
           | 'E' ->
             final
             := int_of_string (String.sub !line 2 (String.length !line - 2))
           | _ -> failf "certificate: bad line %s" !line);
          line := try input_line ic with End_of_file -> ""
        done;
        let items = Array.of_list (List.rev !items) in
        (* uses of each fact: twice or more, a [let] *)
        let nf =
          Array.fold_left
            (fun m it -> if it.(0) >= 0 then max m (it.(1) + 1) else m)
            0
            items
        in
        let uses = Array.make nf 0 in
        let fact_item = Array.make nf [||] in
        Array.iter
          (fun it ->
            if it.(0) >= 0
            then (
              fact_item.(it.(1)) <- it;
              for k = 2 to Array.length it - 1 do
                if kind it.(k) = k_fact
                then (
                  let g = payload it.(k) in
                  uses.(g) <- uses.(g) + 1)
              done))
          items;
        uses.(!final) <- uses.(!final) + 1;
        nfacts := !nfacts + nf;
        (* the chunk's [let]s: its nodes, and the facts used twice or more;
           under [dep] of them, the one at depth [k] is [Rel (dep - k)] *)
        let node_let : (int, int) Hashtbl.t = Hashtbl.create 1024 in
        let fact_let = Array.make nf (-1) in
        let lets = ref [] in
        let ldepth = ref 0 in
        (* one [Rel n] per [n] in the chunk (most facts are at one depth,
           under all the [let]s, and name the same nodes) *)
        let rel_cache = Array.make (Array.length items + 2) Constr.mkProp in
        let rel n =
          if n < Array.length rel_cache
          then (
            let r = rel_cache.(n) in
            if r == Constr.mkProp
            then (
              let r = Constr.mkRel n in
              rel_cache.(n) <- r;
              r)
            else r)
          else Constr.mkRel n
        in
        let node_t dep (i : int) =
          if i = 0
          then ctx.d0
          else if i = 1
          then ctx.d1
          else (
            match Hashtbl.find_opt node_let i with
            | Some k -> rel (dep - k)
            | None -> failf "certificate: node %d not made in chunk %d" i cid)
        in
        let rec arg_t dep (a : int) =
          let x = payload a in
          match kind a with
          | 0 -> node_t dep x
          | 1 -> fact_t dep x
          | 2 -> num ctx x
          | 3 -> pos ctx x
          | 4 -> codes x
          | _ -> failf "certificate: bad argument"
        and fact_t dep (i : int) =
          if fact_let.(i) >= 0
          then rel (dep - fact_let.(i))
          else (
            let it = fact_item.(i) in
            let name, (c, n, _) = !lemma_tbl.(it.(0)) in
            if Array.length it - 2 <> n
            then
              failf
                "certificate: %s takes %d arguments, not %d"
                name
                n
                (Array.length it - 2);
            app c (Array.init n (fun k -> arg_t dep it.(k + 2))))
        in
        Array.iter
          (fun it ->
            if it.(0) < 0
            then (
              let body =
                app
                  ctx.nd
                  [| num ctx it.(2)
                   ; node_t !ldepth it.(3)
                   ; node_t !ldepth it.(4)
                  |]
              in
              lets := (body, ctx.dd) :: !lets;
              Hashtbl.add node_let it.(1) !ldepth;
              incr nnodes;
              incr ldepth)
            else (
              let i = it.(1) in
              if uses.(i) >= 2 && i <> !final
              then (
                let name, (c, n, concl) = !lemma_tbl.(it.(0)) in
                if Array.length it - 2 <> n
                then
                  failf
                    "certificate: %s takes %d arguments, not %d"
                    name
                    n
                    (Array.length it - 2);
                let a = Array.init n (fun k -> arg_t !ldepth it.(k + 2)) in
                let ty = Vars.substl (List.rev (Array.to_list a)) concl in
                lets := (app c a, ty) :: !lets;
                fact_let.(i) <- !ldepth;
                incr ldepth)))
          items;
        let body = ref (fact_t !ldepth !final) in
        List.iter
          (fun (b, ty) -> body := Constr.mkLetIn (Context.anonR, b, ty, !body))
          !lets;
        incr nchunks;
        declare !nchunks true_ty !body;
        (* a large chunk's lines are garbage now: give the memory back
           before the next *)
        if nf > 50_000 then Gc.compact ();
        if Stdlib.Option.is_some (Sys.getenv_opt "MEBI_CERT_STATS")
        then
          Printf.eprintf
            "[certificate %7.2fs %5dMB] chunk %d: %d facts, %d nodes\n%!"
            (Unix.gettimeofday () -. t0)
            (rss_mb ())
            cid
            nf
            (Hashtbl.length node_let)
      in
      (* the chunks need none other: the largest first, so that the
         kernel's memory for checking it (about twice the chunk's term: it
         hash-conses the term, and keeps the types of shared parts) meets
         the least memory held by the chunks declared before *)
      if String.length !line > 0
      then (
        let first = pos_in ic - String.length !line - 1 in
        let places = ref [] in
        let cur = ref (-1)
        and nf = ref 0 in
        let close () = if !cur >= 0 then places := (!nf, !cur) :: !places in
        seek_in ic first;
        (try
           while true do
             let here = pos_in ic in
             let l = input_line ic in
             if String.length l > 0
             then (
               match l.[0] with
               | 'C' ->
                 close ();
                 cur := here;
                 nf := 0
               | 'f' -> incr nf
               | _ -> ())
           done
         with
         | End_of_file -> ());
        close ();
        List.iter
          (fun (_, here) ->
            seek_in ic here;
            line := input_line ic;
            read_chunk ())
          (List.sort (fun (a, p) (b, q) -> compare (b, p) (a, q)) !places));
      { chunks = !nchunks
      ; facts = !nfacts
      ; nodes = !nnodes
      ; seconds = Unix.gettimeofday () -. t0
      })
;;

(* {1 A proof's chunk (doc/certificate.md, "The theorem from the
   certificate")} *)

type proof_ctx =
  { lets : (Constr.t * Constr.types) list (** outermost first *)
  ; depth : int (** the number of [lets] *)
  ; req : int -> int -> Constr.t
    (** [req k d]: request [k]'s fact, node or list of codes, under [d]
        [let]s ([d >= depth]) *)
  }

(* the lines of a proof's chunk ([Dd_cert.write_log ~requests]), taken one
   at a time; the facts, once the chunk has ended *)
type proof_reader =
  { feed : string -> unit
  ; finish : unit -> proof_ctx
  ; finish_pieces :
      declare:(string -> Constr.t -> Constr.types -> bool -> unit)
      -> seg:int
      -> unit
      -> proof_ctx
  }

let proof_reader env : proof_reader =
  lists := [||];
  nlists := 0;
  let ctx = make_ctx env in
  let code_ty = ctx.c "mebi.dt.code" in
  let code_of = function
    | 'C' -> ctx.c "mebi.dt.Copy"
    | 'O' -> ctx.c "mebi.dt.RO"
    | 'W' -> ctx.c "mebi.dt.WO"
    | 'X' -> ctx.c "mebi.dt.RW"
    | c -> failf "certificate: bad code %c" c
  in
  let qcodes : (int, Constr.t) Hashtbl.t = Hashtbl.create 64 in
  let lemma_names = Hashtbl.create 128 in
  let lemma_tbl = ref [||] in
  let lemma_id name =
    match Hashtbl.find_opt lemma_names name with
    | Some k -> k
    | None ->
      let k = Hashtbl.length lemma_names in
      Hashtbl.add lemma_names name k;
      lemma_tbl := Array.append !lemma_tbl [| name, lemma ctx name |];
      k
  in
  let started = ref false in
  let items = ref []
  and reqs = ref []
  and final = ref (-1) in
  let feed (line : string) =
    if String.length line = 0
    then ()
    else if !final >= 0
    then failf "certificate: a line after the chunk: %s" line
    else (
      match line.[0] with
      | 'Q' when Bool.not !started ->
        let i, cs =
          match String.split_on_char ' ' line with
          | [ _; i ] -> int_of_string i, ""
          | _ :: i :: cs :: _ -> int_of_string i, cs
          | _ -> failf "certificate: bad line %s" line
        in
        let t = ref (app (ctx.c "core.list.nil") [| code_ty |]) in
        for j = String.length cs - 1 downto 0 do
          t := app (ctx.c "core.list.cons") [| code_ty; code_of cs.[j]; !t |]
        done;
        Hashtbl.replace qcodes i !t
      | 'C' when Bool.not !started -> started := true
      | _ when Bool.not !started -> failf "certificate: no chunk (%s)" line
      | 'n' ->
        (match ints line with
         | [ i; v; d; r ] -> items := [| -1; i; v; d; r |] :: !items
         | _ -> failf "certificate: bad line %s" line)
      | 'f' -> items := scan_fact lemma_id line :: !items
      | 'R' ->
        (* [R k arg]: like a fact line's arguments *)
        let rest = String.sub line 2 (String.length line - 2) in
        let sp = String.index rest ' ' in
        let a =
          scan_fact
            (fun _ -> 0)
            ("f "
             ^ String.sub rest 0 sp
             ^ " _ "
             ^ String.sub rest (sp + 1) (String.length rest - sp - 1))
        in
        reqs := a :: !reqs
      | 'E' ->
        final := int_of_string (String.sub line 2 (String.length line - 2))
      | _ -> failf "certificate: bad line %s" line)
  in
  let finish () =
    if !final < 0 then failf "certificate: the chunk did not end";
    (* an [R] line scanned as a fact: [| lemma (the request's number read as a lemma name); k; arg |]: its number is at index 1 *)
    let items = Array.of_list (List.rev !items) in
    let nf =
      Array.fold_left
        (fun m it -> if it.(0) >= 0 then max m (it.(1) + 1) else m)
        0
        items
    in
    let uses = Array.make (max nf 1) 0 in
    let fact_item = Array.make (max nf 1) [||] in
    Array.iter
      (fun it ->
        if it.(0) >= 0
        then (
          fact_item.(it.(1)) <- it;
          for k = 2 to Array.length it - 1 do
            if kind it.(k) = k_fact
            then (
              let g = payload it.(k) in
              uses.(g) <- uses.(g) + 1)
          done))
      items;
    (* a requested fact is a [let] *)
    List.iter
      (fun r -> if kind r.(2) = k_fact then uses.(payload r.(2)) <- 2)
      !reqs;
    let node_let : (int, int) Hashtbl.t = Hashtbl.create 1024 in
    let node_def : (int, int * int * int) Hashtbl.t = Hashtbl.create 1024 in
    let fact_let = Array.make (max nf 1) (-1) in
    let lets = ref [] in
    let ldepth = ref 0 in
    (* lists by their contents, each built on its tail's term: the
       suffixes of a vector, one per level of its membership, share
       their cells *)
    let list_terms : (int list, Constr.t) Hashtbl.t = Hashtbl.create 4096 in
    let n_ty = ctx.c "num.N.type" in
    let nil_t = app (ctx.c "core.list.nil") [| n_ty |] in
    let rec list_of = function
      | [] -> nil_t
      | x :: rest as l ->
        (match Hashtbl.find_opt list_terms l with
         | Some t -> t
         | None ->
           let t =
             app (ctx.c "core.list.cons") [| n_ty; num ctx x; list_of rest |]
           in
           Hashtbl.add list_terms l t;
           t)
    in
    let list_t k = list_of (Array.to_list !lists.(k)) in
    (* one term per de Bruijn index *)
    let rels = ref [||] in
    let rel i =
      if i >= Array.length !rels
      then
        rels
        := Array.init
             (max (2 * i) 1024)
             (fun j ->
               if j < Array.length !rels then !rels.(j) else Constr.mkRel j);
      !rels.(i)
    in
    let node_t dep (i : int) =
      if i = 0
      then ctx.d0
      else if i = 1
      then ctx.d1
      else (
        match Hashtbl.find_opt node_let i with
        | Some k -> rel (dep - k)
        | None -> failf "certificate: node %d not made" i)
    in
    let codes i =
      match Hashtbl.find_opt qcodes i with
      | Some t -> t
      | None -> failf "certificate: unknown codes %d" i
    in
    let rec arg_t dep (a : int) =
      let x = payload a in
      match kind a with
      | 0 -> node_t dep x
      | 1 -> fact_t dep x
      | 2 -> num ctx x
      | 3 -> pos ctx x
      | 4 -> codes x
      | 5 -> list_t x
      | _ -> failf "certificate: bad argument"
    and fact_t dep (i : int) =
      if fact_let.(i) >= 0
      then rel (dep - fact_let.(i))
      else (
        let it = fact_item.(i) in
        let name, (c, n, _) = !lemma_tbl.(it.(0)) in
        if Array.length it - 2 <> n
        then
          failf
            "certificate: %s takes %d arguments, not %d"
            name
            n
            (Array.length it - 2);
        app c (Array.init n (fun k -> arg_t dep it.(k + 2))))
    in
    Array.iter
      (fun it ->
        if it.(0) < 0
        then (
          Hashtbl.replace node_def it.(1) (it.(2), it.(3), it.(4));
          let body =
            app
              ctx.nd
              [| num ctx it.(2); node_t !ldepth it.(3); node_t !ldepth it.(4) |]
          in
          lets := (body, ctx.dd) :: !lets;
          Hashtbl.add node_let it.(1) !ldepth;
          incr ldepth)
        else (
          let i = it.(1) in
          if uses.(i) >= 2
          then (
            let name, (c, n, concl) = !lemma_tbl.(it.(0)) in
            if Array.length it - 2 <> n
            then
              failf
                "certificate: %s takes %d arguments, not %d"
                name
                n
                (Array.length it - 2);
            let a = Array.init n (fun k -> arg_t !ldepth it.(k + 2)) in
            let ty = Vars.substl (List.rev (Array.to_list a)) concl in
            lets := (app c a, ty) :: !lets;
            fact_let.(i) <- !ldepth;
            incr ldepth)))
      items;
    let req_tbl = Hashtbl.create 64 in
    List.iter (fun r -> Hashtbl.replace req_tbl r.(1) r.(2)) !reqs;
    let depth = !ldepth in
    { lets = List.rev !lets
    ; depth
    ; req =
        (fun k d ->
          match Hashtbl.find_opt req_tbl k with
          | Some a -> arg_t d a
          | None -> failf "certificate: request %d not answered" k)
    }
  in
  (* the chunk in pieces (doc/certificate.md, "Large goals"): its
     nodes by height, each height a trie of its nodes, a node named by
     its number there (looked up as [val] looks a value up); its facts in
     segments of [seg], each a trie of the statements of the facts others
     need and [TAll] of it, its facts [let]s of its own; each piece handed
     to [declare] as it is made (name, body, type, opaque), in an order
     where a piece names only pieces before it. A request is then a
     closed term over the pieces' names. *)
  let finish_pieces
        ~(declare : string -> Constr.t -> Constr.types -> bool -> unit)
        ~(seg : int)
        ()
    : proof_ctx
    =
    if !final < 0 then failf "certificate: the chunk did not end";
    let items = Array.of_list (List.rev !items) in
    let seg = max 1 seg in
    let c = ctx.c in
    let n_ty = c "num.N.type" in
    let posty = c "num.pos.type" in
    (* a binary trie of terms by key (1, then 2 and 3, ...) *)
    let module T = struct
      type t =
        | Leaf
        | Node of Constr.t option * t * t
    end
    in
    let rec insert (t : T.t) (k : int) (x : Constr.t) : T.t =
      let v, l, r =
        match t with
        | T.Leaf -> None, T.Leaf, T.Leaf
        | T.Node (v, l, r) -> v, l, r
      in
      if k = 1
      then T.Node (Some x, l, r)
      else if k land 1 = 0
      then T.Node (v, insert l (k lsr 1) x, r)
      else T.Node (v, l, insert r (k lsr 1) x)
    in
    let rec trie_t ty (t : T.t) =
      match t with
      | T.Leaf -> app (c "mebi.de.tleaf") [| ty |]
      | T.Node (v, l, r) ->
        let o =
          match v with
          | Some x -> app (c "core.option.Some") [| ty; x |]
          | None -> app (c "core.option.None") [| ty |]
        in
        app (c "mebi.de.tnode") [| ty; o; trie_t ty l; trie_t ty r |]
    in
    (* {2 Nodes} *)
    let height : (int, int) Hashtbl.t = Hashtbl.create 4096 in
    let place : (int, int * int) Hashtbl.t = Hashtbl.create 4096 in
    let groups : (int, (int * int * int * int) list ref) Hashtbl.t =
      Hashtbl.create 256
    in
    let h_of i = if i <= 1 then 0 else Hashtbl.find height i in
    Array.iter
      (fun it ->
        if it.(0) < 0
        then (
          let id, v, d, r = it.(1), it.(2), it.(3), it.(4) in
          let h = 1 + max (h_of d) (h_of r) in
          Hashtbl.replace height id h;
          let g =
            match Hashtbl.find_opt groups h with
            | Some g -> g
            | None ->
              let g = ref [] in
              Hashtbl.add groups h g;
              g
          in
          let p = List.length !g + 1 in
          g := (p, v, d, r) :: !g;
          Hashtbl.replace place id (h, p)))
      items;
    let gname h = Printf.sprintf "mb_dn%d" h in
    let var n = Constr.mkVar (Names.Id.of_string n) in
    let node_refs : (int, Constr.t) Hashtbl.t = Hashtbl.create 4096 in
    let node_ref (i : int) =
      if i = 0
      then ctx.d0
      else if i = 1
      then ctx.d1
      else (
        match Hashtbl.find_opt node_refs i with
        | Some t -> t
        | None ->
          let h, p =
            match Hashtbl.find_opt place i with
            | Some x -> x
            | None -> failf "certificate: node %d not made" i
          in
          let t =
            app (c "mebi.de.val") [| ctx.dd; ctx.d0; var (gname h); num ctx p |]
          in
          Hashtbl.add node_refs i t;
          t)
    in
    let heights =
      List.sort compare (Hashtbl.fold (fun h _ acc -> h :: acc) groups [])
    in
    List.iter
      (fun h ->
        let entries = !(Hashtbl.find groups h) in
        let tr =
          List.fold_left
            (fun t (p, v, d, r) ->
              insert t p (app ctx.nd [| num ctx v; node_ref d; node_ref r |]))
            T.Leaf
            entries
        in
        declare
          (gname h)
          (trie_t ctx.dd tr)
          (app (c "mebi.de.trie") [| ctx.dd |])
          false)
      heights;
    (* {2 Facts} *)
    let nf =
      Array.fold_left
        (fun m it -> if it.(0) >= 0 then max m (it.(1) + 1) else m)
        0
        items
    in
    let fact_item = Array.make (max nf 1) [||] in
    Array.iter (fun it -> if it.(0) >= 0 then fact_item.(it.(1)) <- it) items;
    let seg_of i = i / seg in
    (* a fact a later segment or the proof names: its segment exports it *)
    let exported = Array.make (max nf 1) false
    and uses = Array.make (max nf 1) 0 in
    Array.iter
      (fun it ->
        if it.(0) >= 0
        then
          for k = 2 to Array.length it - 1 do
            if kind it.(k) = k_fact
            then (
              let g = payload it.(k) in
              if seg_of g < seg_of it.(1)
              then exported.(g) <- true
              else uses.(g) <- uses.(g) + 1)
          done)
      items;
    List.iter
      (fun r -> if kind r.(2) = k_fact then exported.(payload r.(2)) <- true)
      !reqs;
    let codes i =
      match Hashtbl.find_opt qcodes i with
      | Some t -> t
      | None -> failf "certificate: unknown codes %d" i
    in
    let list_terms : (int list, Constr.t) Hashtbl.t = Hashtbl.create 4096 in
    let nil_t = app (c "core.list.nil") [| n_ty |] in
    let rec list_of = function
      | [] -> nil_t
      | x :: rest as l ->
        (match Hashtbl.find_opt list_terms l with
         | Some t -> t
         | None ->
           let t =
             app (c "core.list.cons") [| n_ty; num ctx x; list_of rest |]
           in
           Hashtbl.add list_terms l t;
           t)
    in
    let prop = Constr.mkProp in
    let q0 =
      Constr.mkLambda
        ( Context.anonR
        , posty
        , Constr.mkLambda (Context.anonR, prop, Constr.mkRel 1) )
    in
    let sname s = Printf.sprintf "mb_ds%d" s
    and pname s = Printf.sprintf "mb_dp%d" s in
    (* an exported fact: its segment and key *)
    let key_of : (int, int * int) Hashtbl.t = Hashtbl.create 4096 in
    let import (g : int) =
      match Hashtbl.find_opt key_of g with
      | Some (s, k) ->
        app
          (c "mebi.de.tall_get")
          [| prop; var (sname s); q0; pos ctx k; var (pname s) |]
      | None -> failf "certificate: fact %d not exported" g
    in
    let lemma_of it =
      let name, (cst, n, concl) = !lemma_tbl.(it.(0)) in
      if Array.length it - 2 <> n
      then
        failf
          "certificate: %s takes %d arguments, not %d"
          name
          n
          (Array.length it - 2);
      cst, n, concl
    in
    let nseg = if nf = 0 then 0 else seg_of (nf - 1) + 1 in
    for s = 0 to nseg - 1 do
      let lo = s * seg
      and hi = min nf ((s + 1) * seg) in
      let fact_let = Hashtbl.create 1024 in
      let lets = ref []
      and depth = ref 0 in
      let rels = ref [||] in
      let rel i =
        if i >= Array.length !rels
        then
          rels
          := Array.init
               (max (2 * i) 1024)
               (fun j ->
                 if j < Array.length !rels then !rels.(j) else Constr.mkRel j);
        !rels.(i)
      in
      let rec arg_t dep (a : int) =
        let x = payload a in
        match kind a with
        | 0 -> node_ref x
        | 1 -> fact_t dep x
        | 2 -> num ctx x
        | 3 -> pos ctx x
        | 4 -> codes x
        | 5 -> list_of (Array.to_list !lists.(x))
        | _ -> failf "certificate: bad argument"
      and fact_t dep (g : int) =
        if g < lo
        then import g
        else (
          match Hashtbl.find_opt fact_let g with
          | Some k -> rel (dep - k)
          | None ->
            let it = fact_item.(g) in
            let cst, n, _ = lemma_of it in
            app cst (Array.init n (fun k -> arg_t dep it.(k + 2))))
      in
      let exp = ref [] in
      for i = lo to hi - 1 do
        let it = fact_item.(i) in
        if Array.length it > 0 && (uses.(i) >= 2 || exported.(i))
        then (
          let cst, n, concl = lemma_of it in
          let a = Array.init n (fun k -> arg_t !depth it.(k + 2)) in
          let ty = Vars.substl (List.rev (Array.to_list a)) concl in
          lets := (app cst a, ty) :: !lets;
          Hashtbl.replace fact_let i !depth;
          incr depth;
          if exported.(i) then exp := (i, ty) :: !exp)
      done;
      let exp = List.rev !exp in
      if exp <> []
      then (
        (* the statements, keyed 1, 2, ...: closed (nodes by number) *)
        let keyed = List.mapi (fun j (i, ty) -> j + 1, i, ty) exp in
        List.iter
          (fun (k, i, ty) ->
            if Bool.not (Vars.closed0 ty)
            then failf "certificate: an exported statement not closed";
            Hashtbl.replace key_of i (s, k))
          keyed;
        let tr =
          List.fold_left (fun t (k, _, ty) -> insert t k ty) T.Leaf keyed
        in
        let pf_of = Hashtbl.create 1024 in
        List.iter (fun (k, i, _) -> Hashtbl.replace pf_of k i) keyed;
        let d = !depth in
        let shift k q =
          Constr.mkLambda
            ( Context.anonR
            , posty
            , app (Vars.lift 1 q) [| app (c k) [| Constr.mkRel 1 |] |] )
        in
        (* each subtrie: its term (made once) and the proof of its [TAll] *)
        let rec go q (t : T.t) acc dp : Constr.t * Constr.t =
          match t with
          | T.Leaf -> trie_t prop T.Leaf, c "core.True.I"
          | T.Node (v, l, r) ->
            let key = acc + (1 lsl dp) in
            let a_ty, a_pf =
              match v with
              | Some x ->
                x, rel (d - Hashtbl.find fact_let (Hashtbl.find pf_of key))
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
        let stries, pf = go q0 tr 0 0 in
        declare (sname s) stries (app (c "mebi.de.trie") [| prop |]) false;
        let ty = app (c "mebi.de.TAll") [| prop; q0; var (sname s) |] in
        (* the proof given its statement inside the [let]s (typing a [let]
           substitutes into the type of what follows it) *)
        let body =
          List.fold_left
            (fun acc (b, t) -> Constr.mkLetIn (Context.anonR, b, t, acc))
            (app (Constr.mkLambda (Context.anonR, ty, Constr.mkRel 1)) [| pf |])
            !lets
        in
        declare (pname s) body ty true)
    done;
    let req_tbl = Hashtbl.create 64 in
    List.iter (fun r -> Hashtbl.replace req_tbl r.(1) r.(2)) !reqs;
    let closed_arg (a : int) =
      let x = payload a in
      match kind a with
      | 0 -> node_ref x
      | 1 -> import x
      | 2 -> num ctx x
      | 3 -> pos ctx x
      | 4 -> codes x
      | 5 -> list_of (Array.to_list !lists.(x))
      | _ -> failf "certificate: bad argument"
    in
    { lets = []
    ; depth = 0
    ; req =
        (fun k _ ->
          match Hashtbl.find_opt req_tbl k with
          | Some a -> closed_arg a
          | None -> failf "certificate: request %d not answered" k)
    }
  in
  { feed; finish; finish_pieces }
;;
