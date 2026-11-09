(* See the [.mli]. doc/certificate.md, A; theories/DdTree.v.

   The facts file, one record per line ([Dd_read] parses it):
   - [Q i codes]: a list of codes, by number (C copy, O only-read, W
     only-write, X read and write), named [q<i>] by facts;
   - [C id check]: a chunk, then its lines, then [E fact]: the fact that
     closes it (its roots' facts in a chain);
   - [n id v d r]: a node of the chunk; [f id lemma args]: a fact, the
     lemma of theories/DdTree.v applied to [args]: [n_] node, [f_] fact,
     [v_] number, [p_] positive number, [q_] list of codes. *)

type stats =
  { chunks : int
  ; facts : int
  ; locals : int
  ; seconds : float
  }

exception Fails of string

let fails fmt = Printf.ksprintf (fun s -> raise (Fails s)) fmt

type arg =
  | N of int (* node *)
  | F of int (* fact of the chunk *)
  | V of int (* number *)
  | P of int (* a positive number (the digit lemmas) *)
  | Q of int (* a list of codes, by its index in the [Q] lines *)
  | L of int array (* a list of numbers *)

(** What a proof asks the translation for (doc/certificate.md, "The
    theorem from the certificate"), by Sylvan's node indices. *)
type request =
  | Img_sound of int * int * int (** [PostS cs s r R] for [relprod s r m] *)
  | Img_complete of int * int * int (** [PostC cs s r R] *)
  | Union of int * int (** [U a b c] *)
  | Minus of int * int (** [Mi a b c] *)
  | Srt of int (** [Srt x] *)
  | Mem of int * int array (** [In_t x w] *)
  | Node of int (** the node itself *)
  | Codes of int (** a meta's list of codes *)

(* {1 The writer} *)

type st =
  { buf : Buffer.t (** the line being written *)
  ; out : string -> unit (** a line written *)
  ; mutable tv : int array
  ; mutable td : int array
  ; mutable tr : int array
  ; mutable next : int
  ; luniq : (int * int * int, int) Hashtbl.t
  ; mutable nfact : int
  ; mutable nchunk : int
  ; mutable total_facts : int
  ; mutable total_locals : int
  }

let grow st =
  let c = Array.length st.tv in
  let ext a = Array.append a (Array.make c 0) in
  st.tv <- ext st.tv;
  st.td <- ext st.td;
  st.tr <- ext st.tr
;;

let new_node st key =
  let v0, d0, r0 = key in
  if st.next >= Array.length st.tv then grow st;
  let i = st.next in
  st.next <- i + 1;
  st.tv.(i) <- v0;
  st.td.(i) <- d0;
  st.tr.(i) <- r0;
  i
;;

(* the chunk's node [(v, d, r)], [r] if [d] is empty: made once per
   chunk, and written as an [n] line *)
(* the line written, handed on *)
let line st =
  st.out (Buffer.contents st.buf);
  Buffer.clear st.buf
;;

let mk st v0 d0 r0 =
  if d0 = 0
  then r0
  else (
    let key = v0, d0, r0 in
    match Hashtbl.find_opt st.luniq key with
    | Some i -> i
    | None ->
      let i = new_node st key in
      Printf.bprintf st.buf "n %d %d %d %d" i v0 d0 r0;
      line st;
      st.total_locals <- st.total_locals + 1;
      Hashtbl.add st.luniq key i;
      i)
;;

let write_arg b = function
  | N i -> Printf.bprintf b " n%d" i
  | F i -> Printf.bprintf b " f%d" i
  | V x -> Printf.bprintf b " v%d" x
  | P k -> Printf.bprintf b " p%d" k
  | Q i -> Printf.bprintf b " q%d" i
  | L a ->
    Buffer.add_string b " l";
    Array.iteri
      (fun i x -> Printf.bprintf b (if i = 0 then "%d" else ".%d") x)
      a
;;

(* MEBI_CERT_STATS: the facts per lemma *)
let per_lemma : (string, int ref) Hashtbl.t = Hashtbl.create 64

let fact st (lemma : string) (args : arg list) : int =
  (match Hashtbl.find_opt per_lemma lemma with
   | Some r -> incr r
   | None -> Hashtbl.add per_lemma lemma (ref 1));
  let i = st.nfact in
  st.nfact <- i + 1;
  st.total_facts <- st.total_facts + 1;
  Printf.bprintf st.buf "f %d %s" i lemma;
  List.iter (write_arg st.buf) args;
  line st;
  i
;;

let chunk st (header : string) (body : unit -> int) : int =
  let c = st.nchunk in
  st.nchunk <- c + 1;
  Printf.bprintf st.buf "C %d %s" c header;
  line st;
  st.nfact <- 0;
  let f = body () in
  Printf.bprintf st.buf "E %d" f;
  line st;
  (* the chunk's nodes are gone with it *)
  st.next <- 2;
  Hashtbl.reset st.luniq;
  c
;;

(* {1 Facts from Sylvan's proof log (doc/certificate.md, A)}

   The records of a log, as facts of the lemmas of theories/DdTree.v:
   unions and differences ([U], [Mi]), images complete ([PostC]) or sound
   ([PostS]): Sylvan's own computation, case by case, not computed again.
   A record's children are found from the nodes' contents (the case
   Sylvan took) and the records of the children's calls; a terminal case
   (equal arguments, an empty one, a meta's end) is a lemma of its own.
   The roots (the caller's top-level images, each the way its use needs;
   the unions and differences no other record called) are taken a chunk
   at a time, up to 20,000 facts each, with nodes of the chunk's own (no
   inputs: the nodes a chunk needs are made again in the next), and the
   chunk's statement [True], from its roots' facts in a chain ([chk_u],
   [chk_mi], [chk_post], [chk_posts], [chk_nil]): the kernel checks every
   fact, and no node is global. *)

(* a record's key: the operation and its arguments as Sylvan's cache sees
   them (the meta, for an image) *)
type log_key = Proof_log.op * int * int * int * int

(* A table from two ints to an int, by open addressing (linear probing,
   at most three quarters full): the log's records, a million or more for
   three-process CADP, where [Hashtbl]'s boxed keys and records took about
   150 bytes each and this takes about 32. *)
module Itab = struct
  type t =
    { mutable k1 : int array (* -1: an empty slot *)
    ; mutable k2 : int array
    ; mutable vs : int array
    ; mutable size : int
    }

  let create n =
    let c = ref 16 in
    while !c * 3 < n * 4 do
      c := 2 * !c
    done;
    { k1 = Array.make !c (-1)
    ; k2 = Array.make !c 0
    ; vs = Array.make !c 0
    ; size = 0
    }
  ;;

  let hash a b =
    let h = a * 0x2545F4914F6CDD1D lxor b in
    let h = h * 0x9E3779B97F4A7C1 in
    h lxor (h lsr 29)
  ;;

  let slot t a b =
    let m = Array.length t.k1 - 1 in
    let i = ref (hash a b land m) in
    while t.k1.(!i) <> -1 && (t.k1.(!i) <> a || t.k2.(!i) <> b) do
      i := (!i + 1) land m
    done;
    !i
  ;;

  (* grows only for a new key: [iter] may replace values as it goes *)
  let rec replace t a b x =
    let i = slot t a b in
    if t.k1.(i) <> -1
    then t.vs.(i) <- x
    else if (t.size + 1) * 4 > Array.length t.k1 * 3
    then (
      grow t;
      replace t a b x)
    else (
      t.size <- t.size + 1;
      t.k1.(i) <- a;
      t.k2.(i) <- b;
      t.vs.(i) <- x)

  and grow t =
    let k1 = t.k1
    and k2 = t.k2
    and vs = t.vs in
    let c = 2 * Array.length k1 in
    t.k1 <- Array.make c (-1);
    t.k2 <- Array.make c 0;
    t.vs <- Array.make c 0;
    t.size <- 0;
    Array.iteri (fun i a -> if a <> -1 then replace t a k2.(i) vs.(i)) k1
  ;;

  let find_opt t a b =
    let i = slot t a b in
    if t.k1.(i) = -1 then None else Some t.vs.(i)
  ;;

  let iter f t =
    Array.iteri (fun i a -> if a <> -1 then f a t.k2.(i) t.vs.(i)) t.k1
  ;;

  let length t = t.size
end

(* a record's key in an [Itab]: the operation and the first two arguments,
   the other two (node indices below 2^29: Sylvan's table is far smaller) *)
let op_num = function
  | Proof_log.Union -> 0
  | Proof_log.Minus -> 1
  | Proof_log.Intersect -> 2
  | Proof_log.Relprod -> 3
  | Proof_log.Relprev -> 4
  | Proof_log.Top -> 5
;;

let op_of_num = function
  | 0 -> Proof_log.Union
  | 1 -> Proof_log.Minus
  | 2 -> Proof_log.Intersect
  | 3 -> Proof_log.Relprod
  | 4 -> Proof_log.Relprev
  | _ -> Proof_log.Top
;;

let pack ((op, a, b, c, d) : log_key) : int * int =
  op_num op lor (a lsl 3) lor (b lsl 32), c lor (d lsl 30)
;;

let unpack k1 k2 : log_key =
  let m = (1 lsl 29) - 1 in
  op_of_num (k1 land 7), (k1 lsr 3) land m, k1 lsr 32, k2 land m, k2 lsr 30
;;

(* MEBI_CERT_STATS: the translator's phases, with the process's memory *)
let log_mark =
  let on = Sys.getenv_opt "MEBI_CERT_STATS" <> None in
  let t0 = Unix.gettimeofday () in
  (* the process's memory now and at most so far ([VmRSS], [VmHWM]), in MB *)
  let field l name =
    let k = String.length name in
    if String.length l > k && String.sub l 0 k = name
    then
      Some
        (Scanf.sscanf
           (String.sub l k (String.length l - k))
           " %d"
           (fun kb -> kb / 1024))
    else None
  in
  fun (what : string) ->
    if on
    then (
      let rss = ref 0
      and hwm = ref 0 in
      (try
         let ic = open_in "/proc/self/status" in
         (try
            while true do
              let l = input_line ic in
              (match field l "VmRSS:" with Some x -> rss := x | None -> ());
              match field l "VmHWM:" with Some x -> hwm := x | None -> ()
            done
          with
          | End_of_file -> ());
         close_in ic
       with
       | _ -> ());
      Printf.eprintf
        "[log facts %7.2fs %5dMB, at most %5dMB] %s\n%!"
        (Unix.gettimeofday () -. t0)
        !rss
        !hwm
        what)
;;

type output =
  | To_file of string
  | To_lines of (string -> unit)

let write_log
      ?(requests : request list = [])
      ?(groups : request list list = [])
      ~(log : Proof_log.source)
      (output : output)
  : stats
  =
  let proof_mode = requests <> [] in
  let t0 = Unix.gettimeofday () in
  log_mark "reading the log";
  (* each record's result, by its key (the arguments as Sylvan's cache saw
     them), and a bit: whether another record calls it *)
  let recs = Itab.create 65536 in
  let nrec = ref 0 in
  (* the caller's top-level images, as roots: [case] the directions their
     use needs (1 sound, 2 complete) *)
  let tops = ref [] in
  let node_of =
    Proof_log.iter log (fun (r : Proof_log.record) ->
      if r.a lor r.b lor r.c lor r.d lor r.r >= 1 lsl 29
      then fails "proof log: a node index above 2^29";
      incr nrec;
      match r.op with
      | Proof_log.Top ->
        if Proof_log.top_op r = Proof_log.Relprod
        then
          tops
          := { r with op = Proof_log.Relprod; case = Proof_log.top_role r }
             :: !tops
      | Proof_log.Relprev ->
        fails
          "proof log: a preimage, from an earlier version of the engine \
           (decide the goal again)"
      | _ ->
        let k1, k2 =
          pack
            (match r.op with
             | Proof_log.Union | Proof_log.Minus | Proof_log.Intersect
             | Proof_log.Top ->
               r.op, r.a, r.b, 0, 0
             | Proof_log.Relprod -> r.op, r.a, r.b, r.c, 0
             | Proof_log.Relprev -> r.op, r.a, r.b, r.c, r.d)
        in
        Itab.replace recs k1 k2 (r.r lsl 1))
  in
  (* an image used both ways is one root *)
  let tops =
    let by : (int * int * int * int, Proof_log.record) Hashtbl.t =
      Hashtbl.create 64
    in
    List.iter
      (fun (t : Proof_log.record) ->
        let k = t.a, t.b, t.c, t.d in
        match Hashtbl.find_opt by k with
        | Some t0 -> Hashtbl.replace by k { t0 with case = t0.case lor t.case }
        | None -> Hashtbl.add by k t)
      !tops;
    Hashtbl.fold (fun _ t acc -> t :: acc) by []
  in
  log_mark
    (Printf.sprintf
       "read from %s: %d records (%d distinct)"
       (Proof_log.source_name log)
       !nrec
       (Itab.length recs));
  let node i =
    match node_of i with
    | n ->
      if n.Proof_log.copy then fails "proof log: a copy node (not expected)";
      n
    | exception Not_found -> fails "proof log: node %d not dumped" i
  in
  let result k =
    let k1, k2 = pack k in
    match Itab.find_opt recs k1 k2 with
    | Some x -> x lsr 1
    | None ->
      let op, a, b, c, d = k in
      fails
        "proof log: no record of a call (%s %d %d %d %d)"
        (match op with
         | Proof_log.Union -> "union"
         | Proof_log.Minus -> "minus"
         | Proof_log.Relprod -> "image"
         | Proof_log.Relprev -> "preimage"
         | Proof_log.Intersect -> "intersect"
         | Proof_log.Top -> "top")
        a
        b
        c
        d
  in
  (* Sylvan's union orders its arguments by index first, for its cache *)
  let ukey a b = Proof_log.Union, max a b, min a b, 0, 0 in
  (* a meta cube (Sylvan's per-level codes: 0 copy, 1 read then 2 write,
     3 only-read, 4 only-write, -1 the end) as the list of codes of
     theories/DdTree.v, one per position: C copy, O only-read, W only-write,
     X read and write *)
  let is_end m = m = 1 || (m > 1 && (node m).Proof_log.value = 0xFFFFFFFF) in
  let codes_memo : (int, string) Hashtbl.t = Hashtbl.create 64 in
  let rec codes_of m =
    match Hashtbl.find_opt codes_memo m with
    | Some c -> c
    | None ->
      let c =
        if is_end m
        then ""
        else (
          let n = node m in
          match n.Proof_log.value with
          | 0 -> "C" ^ codes_of n.Proof_log.down
          | 3 -> "O" ^ codes_of n.Proof_log.down
          | 1 ->
            let n2 = node n.Proof_log.down in
            if n2.Proof_log.value <> 2
            then fails "proof log: a read level without its write";
            "X" ^ codes_of n2.Proof_log.down
          | 2 ->
            (* a write level: the codes after its position (its read
               level's) *)
            codes_of n.Proof_log.down
          | v -> fails "proof log: meta code %d not supported yet" v)
      in
      Hashtbl.add codes_memo m c;
      c
  in
  let qidx : (string, int) Hashtbl.t = Hashtbl.create 64 in
  let q m =
    let c = codes_of m in
    match Hashtbl.find_opt qidx c with
    | Some i -> i
    | None ->
      let i = Hashtbl.length qidx in
      Hashtbl.add qidx c i;
      i
  in
  (* every list of codes a fact may name: those of each image's meta and
     of every meta below it *)
  Itab.iter
    (fun k1 k2 _ ->
      let op, _, _, m, _ = unpack k1 k2 in
      if op = Proof_log.Relprod
      then (
        let rec reg m =
          ignore (q m);
          if Bool.not (is_end m) then reg (node m).Proof_log.down
        in
        reg m))
    recs;
  (* the call [relprod s r m] as Sylvan's cache sees it: after skipping to
     equal first values (read and only-read levels); [None] at a terminal
     case *)
  let rec rp_norm s r m =
    if s = 0 || r = 0 || is_end m
    then None
    else (
      let mv = (node m).Proof_log.value in
      if mv = 1 || mv = 3
      then (
        let ns = node s
        and nr = node r in
        if ns.Proof_log.value < nr.Proof_log.value
        then rp_norm ns.Proof_log.right r m
        else if ns.Proof_log.value > nr.Proof_log.value
        then rp_norm s nr.Proof_log.right m
        else Some (Proof_log.Relprod, s, r, m, 0))
      else Some (Proof_log.Relprod, s, r, m, 0))
  in
  (* the calls a record makes, by its case *)
  let children ((op, a, b, c, d) : log_key) : log_key list =
    match op with
    | Proof_log.Union | Proof_log.Minus ->
      let na = node a
      and nb = node b in
      let va = na.Proof_log.value
      and vb = nb.Proof_log.value in
      let call x y =
        if x = y || x = 0 || y = 0
        then []
        else if op = Proof_log.Union
        then [ ukey x y ]
        else [ Proof_log.Minus, x, y, 0, 0 ]
      in
      if va < vb
      then call na.Proof_log.right b
      else if va > vb
      then call a nb.Proof_log.right
      else
        call na.Proof_log.down nb.Proof_log.down
        @ call na.Proof_log.right nb.Proof_log.right
    | Proof_log.Relprod ->
      let ns = node a
      and nm = node c in
      (* the relation's node, only where a case reads it (at a copy level
         the relation may be [D1], consumed) *)
      let nr () = node b in
      let opt = function Some k -> [ k ] | None -> [] in
      (match nm.Proof_log.value with
       | 0 ->
         opt (rp_norm ns.Proof_log.down b nm.Proof_log.down)
         @ opt (rp_norm ns.Proof_log.right b c)
       | 3 ->
         let nr = nr () in
         opt (rp_norm ns.Proof_log.down nr.Proof_log.down nm.Proof_log.down)
         @ opt (rp_norm ns.Proof_log.right nr.Proof_log.right c)
       | 1 ->
         let nr = nr () in
         let wk =
           Proof_log.Relprod, a, nr.Proof_log.down, nm.Proof_log.down, 0
         in
         let right = rp_norm a nr.Proof_log.right c in
         let w = result wk in
         let r2 = match right with Some k -> result k | None -> 0 in
         (wk :: opt right)
         @ if w = 0 || r2 = 0 || w = r2 then [] else [ ukey w r2 ]
       | 2 ->
         let rec chain r =
           if r = 0
           then []
           else (
             let nr = node r in
             opt (rp_norm ns.Proof_log.down nr.Proof_log.down nm.Proof_log.down)
             @ chain nr.Proof_log.right)
         in
         chain b
       | _ -> [])
    | _ -> []
  in
  (* mark every called record (a record calls only recorded ones: the
     keys come from the table, the bits change in place) *)
  Itab.iter
    (fun k1 k2 _ ->
      List.iter
        (fun k' ->
          let a, b = pack k' in
          match Itab.find_opt recs a b with
          | Some x -> Itab.replace recs a b (x lor 1)
          | None -> ())
        (children (unpack k1 k2)))
    recs;
  let roots =
    let acc = ref [] in
    Itab.iter
      (fun k1 k2 x ->
        let op, a, b, c, d = unpack k1 k2 in
        (* an image's root is the caller's top-level record, with its role
           (a write level is only ever called by its read level); the
           check makes no intersection *)
        if x land 1 = 0 && (op = Proof_log.Union || op = Proof_log.Minus)
        then acc := { Proof_log.op; case = 0; a; b; c; d; r = x lsr 1 } :: !acc)
      recs;
    if tops = []
    then
      fails
        "proof log: no top-level image (the check was not logged: an answering \
         relation that only writes a position?)";
    List.sort compare (tops @ !acc)
  in
  log_mark (Printf.sprintf "tables built: %d roots" (List.length roots));
  let oc =
    match output with
    | To_file path -> Some (open_out_bin path)
    | To_lines _ -> None
  in
  let out =
    match output, oc with
    | To_lines f, _ -> f
    | To_file _, Some oc ->
      fun l ->
        output_string oc l;
        output_char oc '\n'
    | To_file _, None -> assert false
  in
  let st =
    { buf = Buffer.create 256
    ; out
    ; tv = Array.make 1024 0
    ; td = Array.make 1024 0
    ; tr = Array.make 1024 0
    ; next = 2
    ; luniq = Hashtbl.create 4096
    ; nfact = 0
    ; nchunk = 0
    ; total_facts = 0
    ; total_locals = 0
    }
  in
  Fun.protect
    ~finally:(fun () -> Stdlib.Option.iter close_out_noerr oc)
    (fun () ->
      (* the lists of codes the facts name; every chunk makes the nodes it
         needs, as its own *)
      Hashtbl.iter
        (fun c i ->
          Printf.bprintf st.buf "Q %d %s" i c;
          line st)
        qidx;
      (* roots per chunk until this many facts (MEBI_CHUNK_FACTS): the
         nodes a chunk needs are made again in the next, so fewer chunks
         make fewer nodes, and each needs more memory *)
      let max_facts =
        match Sys.getenv_opt "MEBI_CHUNK_FACTS" with
        | Some k -> (try int_of_string k with _ -> 20_000)
        | None -> 20_000
      in
      (* a proof's requests: the first chunk's, then one chunk per group *)
      let pending = ref (if proof_mode then requests :: groups else []) in
      let rec chunks_of (rest : Proof_log.record list) =
        if rest <> [] || !pending <> []
        then (
          let left = ref rest in
          ignore
            (chunk st "check" (fun () ->
               (* Sylvan's nodes as the chunk's, children first (an
                  explicit stack: a right chain can be long) *)
               let ids : (int, int) Hashtbl.t = Hashtbl.create 4096 in
               let conv (x : int) : int =
                 let stack = ref [ x ] in
                 while !stack <> [] do
                   match !stack with
                   | [] -> ()
                   | y :: tl ->
                     if y <= 1 || Hashtbl.mem ids y
                     then stack := tl
                     else (
                       let nd = node y in
                       let pending =
                         List.filter
                           (fun c -> c > 1 && Bool.not (Hashtbl.mem ids c))
                           [ nd.Proof_log.down; nd.Proof_log.right ]
                       in
                       if pending = []
                       then (
                         let g c = if c <= 1 then c else Hashtbl.find ids c in
                         Hashtbl.replace
                           ids
                           y
                           (mk
                              st
                              nd.Proof_log.value
                              (g nd.Proof_log.down)
                              (g nd.Proof_log.right));
                         stack := tl)
                       else stack := pending @ !stack)
                 done;
                 if x <= 1 then x else Hashtbl.find ids x
               in
               let n = conv in
               (* [a < b], from the numbers' binary digits *)
               let lt_memo : (int * int, int) Hashtbl.t = Hashtbl.create 64 in
               let rec plt p q' =
                 match Hashtbl.find_opt lt_memo (p, q') with
                 | Some f -> f
                 | None ->
                   if p >= q' || p < 1
                   then fails "proof log: %d < %d does not hold" p q';
                   let p1 = p lsr 1
                   and q1 = q' lsr 1 in
                   let f =
                     if p = 1
                     then
                       fact
                         st
                         (if q' land 1 = 0 then "plt_1_xO" else "plt_1_xI")
                         [ P q1 ]
                     else if p land 1 = 0
                     then
                       if q' land 1 = 0
                       then fact st "plt_xO_xO" [ P p1; P q1; F (plt p1 q1) ]
                       else if p1 = q1
                       then fact st "plt_xO_xI_eq" [ P p1 ]
                       else fact st "plt_xO_xI" [ P p1; P q1; F (plt p1 q1) ]
                     else if q' land 1 = 0
                     then fact st "plt_xI_xO" [ P p1; P q1; F (plt p1 q1) ]
                     else fact st "plt_xI_xI" [ P p1; P q1; F (plt p1 q1) ]
                   in
                   Hashtbl.add lt_memo (p, q') f;
                   f
               in
               (* once per pair in a chunk: the same values meet often *)
               let nlt_memo : (int * int, int) Hashtbl.t = Hashtbl.create 64 in
               let nlt a b =
                 match Hashtbl.find_opt nlt_memo (a, b) with
                 | Some f -> f
                 | None ->
                   if a >= b then fails "proof log: %d < %d does not hold" a b;
                   let f =
                     if a = 0
                     then fact st "nlt_0" [ P b ]
                     else fact st "nlt_pos" [ P a; P b; F (plt a b) ]
                   in
                   Hashtbl.add nlt_memo (a, b) f;
                   f
               in
               let memo : (log_key, int * int) Hashtbl.t =
                 Hashtbl.create 4096
               in
               (* the sound images' facts (an image may be used both ways) *)
               let memo_s : (int * int * int, int * int) Hashtbl.t =
                 Hashtbl.create 4096
               in
               (* union and minus: the fact that [op a b] (Sylvan's
                  indices) is its result, and that result *)
               let rec go op a b : int * int =
                 match op with
                 | Proof_log.Union when a = b -> a, fact st "u_same" [ N (n a) ]
                 | Proof_log.Union when a = 0 -> b, fact st "u_a0" [ N (n b) ]
                 | Proof_log.Union when b = 0 -> a, fact st "u_b0" [ N (n a) ]
                 | Proof_log.Minus when a = b ->
                   0, fact st "mi_same" [ N (n a) ]
                 | Proof_log.Minus when a = 0 -> 0, fact st "mi_a0" [ N (n b) ]
                 | Proof_log.Minus when b = 0 ->
                   a, fact st "mi_id" [ N (n a); N (n b) ]
                 | _ ->
                   (match Hashtbl.find_opt memo (op, a, b, 0, 0) with
                    | Some x -> x
                    | None ->
                      (* a union is recorded under Sylvan's order of its
                         arguments; its facts follow the order asked for
                         (each case has its mirror image) *)
                      let res =
                        result
                          (if op = Proof_log.Union
                           then ukey a b
                           else op, a, b, 0, 0)
                      in
                      let na = node a
                      and nb = node b in
                      let va = na.Proof_log.value
                      and vb = nb.Proof_log.value in
                      let pre = if op = Proof_log.Union then "u_" else "mi_" in
                      let x =
                        if
                          va < vb
                          && op = Proof_log.Union
                          && na.Proof_log.right = 0
                        then (
                          expect res va na.Proof_log.down b;
                          ( res
                          , fact
                              st
                              "u_lt0"
                              [ V va; N (n na.Proof_log.down); N (n b) ] ))
                        else if
                          va > vb
                          && op = Proof_log.Union
                          && nb.Proof_log.right = 0
                        then (
                          expect res vb nb.Proof_log.down a;
                          ( res
                          , fact
                              st
                              "u_gt0"
                              [ N (n a); V vb; N (n nb.Proof_log.down) ] ))
                        else if va = vb && na.Proof_log.down = nb.Proof_log.down
                        then (
                          (* the same child: the right siblings' only *)
                          let rr, fr =
                            go op na.Proof_log.right nb.Proof_log.right
                          in
                          let args =
                            [ V va
                            ; N (n na.Proof_log.down)
                            ; N (n na.Proof_log.right)
                            ; N (n nb.Proof_log.right)
                            ; N (n rr)
                            ; F fr
                            ]
                          in
                          if op = Proof_log.Union
                          then (
                            expect res va na.Proof_log.down rr;
                            res, fact st "u_eq_d" args)
                          else (
                            if res <> rr
                            then
                              fails
                                "proof log: a result does not match its case";
                            res, fact st "mi_eq0d" args))
                        else if
                          va = vb
                          && op = Proof_log.Minus
                          && na.Proof_log.right = nb.Proof_log.right
                          && res = 0
                        then (
                          (* the same right sibling, the difference empty:
                             the children's difference is empty *)
                          let _, fd =
                            go op na.Proof_log.down nb.Proof_log.down
                          in
                          ( res
                          , fact
                              st
                              "mi_eq0r"
                              [ V va
                              ; N (n na.Proof_log.down)
                              ; N (n nb.Proof_log.down)
                              ; N (n na.Proof_log.right)
                              ; F fd
                              ] ))
                        else if
                          va = vb
                          && op = Proof_log.Union
                          && na.Proof_log.right = nb.Proof_log.right
                        then (
                          (* the same right sibling: the children's only *)
                          let dd, fd =
                            go op na.Proof_log.down nb.Proof_log.down
                          in
                          expect res va dd na.Proof_log.right;
                          ( res
                          , fact
                              st
                              "u_eq_r"
                              [ V va
                              ; N (n na.Proof_log.down)
                              ; N (n nb.Proof_log.down)
                              ; N (n na.Proof_log.right)
                              ; N (n dd)
                              ; F fd
                              ] ))
                        else if va < vb
                        then (
                          let rr, f = go op na.Proof_log.right b in
                          expect res va na.Proof_log.down rr;
                          ( res
                          , fact
                              st
                              (pre ^ "lt")
                              [ V va
                              ; N (n na.Proof_log.down)
                              ; N (n na.Proof_log.right)
                              ; N (n b)
                              ; N (n rr)
                              ; F f
                              ] ))
                        else if va > vb
                        then (
                          let rr, f = go op a nb.Proof_log.right in
                          if op = Proof_log.Union
                          then expect res vb nb.Proof_log.down rr
                          else if res <> rr
                          then
                            fails "proof log: a result does not match its case";
                          ( res
                          , fact
                              st
                              (pre ^ "gt")
                              [ N (n a)
                              ; V vb
                              ; N (n nb.Proof_log.down)
                              ; N (n nb.Proof_log.right)
                              ; N (n rr)
                              ; F f
                              ] ))
                        else (
                          let dd, fd =
                            go op na.Proof_log.down nb.Proof_log.down
                          in
                          let rr, fr =
                            go op na.Proof_log.right nb.Proof_log.right
                          in
                          expect res va dd rr;
                          let args0 =
                            [ V va
                            ; N (n na.Proof_log.down)
                            ; N (n na.Proof_log.right)
                            ; N (n nb.Proof_log.down)
                            ; N (n nb.Proof_log.right)
                            ]
                          in
                          if dd = 0
                          then
                            ( res
                            , fact
                                st
                                (pre ^ "eq0")
                                (args0 @ [ N (n rr); F fd; F fr ]) )
                          else
                            ( res
                            , fact
                                st
                                (pre ^ "eq")
                                (args0 @ [ N (n dd); N (n rr); F fd; F fr ]) ))
                      in
                      Hashtbl.add memo (op, a, b, 0, 0) x;
                      x)
               (* the node [res] is [mk v d rr] (Sylvan's [makenode]: [rr] if
                  [d] is empty) *)
               and expect res v d rr =
                 let ok =
                   if d = 0
                   then res = rr
                   else (
                     let nr = node res in
                     nr.Proof_log.value = v
                     && nr.Proof_log.down = d
                     && nr.Proof_log.right = rr)
                 in
                 if Bool.not ok
                 then fails "proof log: a result does not match its case"
               (* the image [relprod s r m]: the fact [PostC cs s r R]
                  (cs: [m]'s codes), and [R] *)
               and img s r m : int * int =
                 if s = 0
                 then 0, fact st "post_x0" [ Q (q m); N (n r); N 0 ]
                 else if r = 0
                 then 0, fact st "post_r0" [ Q (q m); N (n s); N 0 ]
                 else if is_end m
                 then s, fact st "post_end" [ N (n s); N (n r) ]
                 else (
                   match
                     Hashtbl.find_opt memo (Proof_log.Relprod, s, r, m, 0)
                   with
                   | Some x -> x
                   | None ->
                     let nm = node m in
                     let ns = node s in
                     let vx = ns.Proof_log.value
                     and dx = ns.Proof_log.down
                     and xr = ns.Proof_log.right in
                     let x =
                       match nm.Proof_log.value with
                       | 0 when xr = 0 ->
                         let res = result (Proof_log.Relprod, s, r, m, 0) in
                         let r1, f1 = img dx r nm.Proof_log.down in
                         expect res vx r1 0;
                         let cs = Q (q nm.Proof_log.down) in
                         if r1 = 0
                         then
                           ( res
                           , fact
                               st
                               "post_copy0_last"
                               [ cs; V vx; N (n dx); N (n r); F f1 ] )
                         else
                           ( res
                           , fact
                               st
                               "post_copy_last"
                               [ cs; V vx; N (n dx); N (n r); N (n r1); F f1 ] )
                       | 0 ->
                         let res = result (Proof_log.Relprod, s, r, m, 0) in
                         let r1, f1 = img dx r nm.Proof_log.down in
                         let r2, f2 = img xr r m in
                         expect res vx r1 r2;
                         let cs = Q (q nm.Proof_log.down) in
                         if r1 = 0
                         then
                           ( res
                           , fact
                               st
                               "post_copy0"
                               [ cs
                               ; V vx
                               ; N (n dx)
                               ; N (n xr)
                               ; N (n r)
                               ; N (n r2)
                               ; F f1
                               ; F f2
                               ] )
                         else
                           ( res
                           , fact
                               st
                               "post_copy"
                               [ cs
                               ; V vx
                               ; N (n dx)
                               ; N (n xr)
                               ; N (n r)
                               ; N (n r1)
                               ; N (n r2)
                               ; F f1
                               ; F f2
                               ] )
                       | (1 | 3) as mv ->
                         let nr = node r in
                         let vr = nr.Proof_log.value
                         and dr = nr.Proof_log.down
                         and rr = nr.Proof_log.right in
                         let tail =
                           if mv = 3
                           then nm.Proof_log.down
                           else (node nm.Proof_log.down).Proof_log.down
                         in
                         let kind = if mv = 3 then "ro" else "rw" in
                         let cs = Q (q tail) in
                         if vx < vr
                         then (
                           let res, f = img xr r m in
                           ( res
                           , fact
                               st
                               ("post_" ^ kind ^ "_lt")
                               [ cs
                               ; V vx
                               ; N (n dx)
                               ; N (n xr)
                               ; V vr
                               ; N (n dr)
                               ; N (n rr)
                               ; N (n res)
                               ; F (nlt vx vr)
                               ; F f
                               ] ))
                         else if vx > vr
                         then (
                           let res, f = img s rr m in
                           ( res
                           , fact
                               st
                               ("post_" ^ kind ^ "_gt")
                               [ cs
                               ; V vx
                               ; N (n dx)
                               ; N (n xr)
                               ; V vr
                               ; N (n dr)
                               ; N (n rr)
                               ; N (n res)
                               ; F (nlt vr vx)
                               ; F f
                               ] ))
                         else (
                           let res = result (Proof_log.Relprod, s, r, m, 0) in
                           if mv = 3
                           then (
                             let r1, f1 = img dx dr nm.Proof_log.down in
                             let r2, f2 = img xr rr m in
                             expect res vx r1 r2;
                             if r1 = 0
                             then
                               ( res
                               , fact
                                   st
                                   "post_ro_eq0"
                                   [ cs
                                   ; V vx
                                   ; N (n dx)
                                   ; N (n xr)
                                   ; N (n dr)
                                   ; N (n rr)
                                   ; N (n r2)
                                   ; F f1
                                   ; F f2
                                   ] )
                             else
                               ( res
                               , fact
                                   st
                                   "post_ro_eq"
                                   [ cs
                                   ; V vx
                                   ; N (n dx)
                                   ; N (n xr)
                                   ; N (n dr)
                                   ; N (n rr)
                                   ; N (n r1)
                                   ; N (n r2)
                                   ; F f1
                                   ; F f2
                                   ] ))
                           else if rr = 0
                           then (
                             (* read and write, the relation's last read
                                value: the write level's image *)
                             let w, fw = wimg s dr nm.Proof_log.down in
                             if w <> res
                             then
                               fails
                                 "proof log: a result does not match its case";
                             ( res
                             , fact
                                 st
                                 "post_rw_last"
                                 [ cs
                                 ; V vx
                                 ; N (n dx)
                                 ; N (n xr)
                                 ; N (n dr)
                                 ; N (n w)
                                 ; F fw
                                 ] ))
                           else (
                             (* read and write: the write level, the
                                relation's right with the same set, their
                                union *)
                             let w, fw = wimg s dr nm.Proof_log.down in
                             let r2, f2 = img s rr m in
                             let u, fu = go Proof_log.Union w r2 in
                             if u <> res
                             then
                               fails
                                 "proof log: a result does not match its case";
                             ( res
                             , fact
                                 st
                                 "post_rw_s"
                                 [ cs
                                 ; V vx
                                 ; N (n dx)
                                 ; N (n xr)
                                 ; N (n dr)
                                 ; N (n rr)
                                 ; N (n w)
                                 ; N (n r2)
                                 ; N (n u)
                                 ; F fw
                                 ; F f2
                                 ; F fu
                                 ] )))
                       | v ->
                         fails "proof log: meta code %d not supported yet" v
                     in
                     Hashtbl.add memo (Proof_log.Relprod, s, r, m, 0) x;
                     x)
               (* the write level [relprod s rw m2] ([m2] a write node): the
                  fact [WlC cs dx rw W] (dx: [s]'s down; cs: the codes after
                  the position), along the structure of Sylvan's result: its
                  nodes are the written values whose image is not empty *)
               and wimg s rw m2 : int * int =
                 let res = result (Proof_log.Relprod, s, rw, m2, 0) in
                 let dx = (node s).Proof_log.down in
                 let md = (node m2).Proof_log.down in
                 let cs = Q (q md) in
                 let rec chain rw w =
                   if rw = 0
                   then (
                     if w <> 0
                     then fails "proof log: a result does not match its case";
                     fact st "wl_r0" [ cs; N (n dx); N 0 ])
                   else (
                     let nrw = node rw in
                     let vb = nrw.Proof_log.value
                     and db = nrw.Proof_log.down
                     and rb = nrw.Proof_log.right in
                     let r1, f1 = img dx db md in
                     if rb = 0
                     then
                       if r1 = 0
                       then (
                         if w <> 0
                         then
                           fails "proof log: a result does not match its case";
                         fact
                           st
                           "wl_last0"
                           [ cs; N (n dx); V vb; N (n db); F f1 ])
                       else (
                         let nw = node w in
                         if
                           nw.Proof_log.value <> vb
                           || nw.Proof_log.down <> r1
                           || nw.Proof_log.right <> 0
                         then
                           fails "proof log: a result does not match its case";
                         fact
                           st
                           "wl_last"
                           [ cs; N (n dx); V vb; N (n db); N (n r1); F f1 ])
                     else if r1 = 0
                     then (
                       let f = chain rb w in
                       fact
                         st
                         "wl_nd0"
                         [ cs
                         ; N (n dx)
                         ; V vb
                         ; N (n db)
                         ; N (n rb)
                         ; N (n w)
                         ; F f1
                         ; F f
                         ])
                     else (
                       let nw = node w in
                       if nw.Proof_log.value <> vb || nw.Proof_log.down <> r1
                       then fails "proof log: a result does not match its case";
                       let f = chain rb nw.Proof_log.right in
                       fact
                         st
                         "wl_nd"
                         [ cs
                         ; N (n dx)
                         ; V vb
                         ; N (n db)
                         ; N (n rb)
                         ; N (n r1)
                         ; N (n nw.Proof_log.right)
                         ; F f1
                         ; F f
                         ]))
                 in
                 res, chain rw res
               (* the result of the image [relprod s r m], from the log
                  (no fact) *)
               and img_res s r m =
                 if s = 0 || r = 0
                 then 0
                 else if is_end m
                 then s
                 else (
                   match rp_norm s r m with Some k -> result k | None -> 0)
               (* the image [relprod s r m], sound: the fact [PostS cs s r R]
                  (cs: [m]'s codes), and [R]. A branch Sylvan skipped, or
                  whose image is empty, needs no fact: fewer candidates,
                  fewer images *)
               and imgs s r m : int * int =
                 if s = 0 || r = 0
                 then 0, fact st "posts_0" [ Q (q m); N (n s); N (n r) ]
                 else if is_end m
                 then (
                   if r <> 1
                   then fails "proof log: a relation left at its meta's end";
                   s, fact st "posts_end" [ N (n s) ])
                 else (
                   match Hashtbl.find_opt memo_s (s, r, m) with
                   | Some x -> x
                   | None ->
                     let nm = node m in
                     let ns = node s in
                     let vx = ns.Proof_log.value
                     and dx = ns.Proof_log.down
                     and xr = ns.Proof_log.right in
                     let x =
                       match nm.Proof_log.value with
                       | 0 ->
                         let res = result (Proof_log.Relprod, s, r, m, 0) in
                         let r2, f2 = imgs xr r m in
                         if img_res dx r nm.Proof_log.down = 0
                         then (
                           expect res vx 0 r2;
                           ( res
                           , fact
                               st
                               "posts_x"
                               [ Q (q m)
                               ; V vx
                               ; N (n dx)
                               ; N (n xr)
                               ; N (n r)
                               ; N (n r2)
                               ; F f2
                               ] ))
                         else (
                           let r1, f1 = imgs dx r nm.Proof_log.down in
                           expect res vx r1 r2;
                           ( res
                           , fact
                               st
                               "posts_copy"
                               [ Q (q nm.Proof_log.down)
                               ; V vx
                               ; N (n dx)
                               ; N (n xr)
                               ; N (n r)
                               ; N (n r1)
                               ; N (n r2)
                               ; F f1
                               ; F f2
                               ] ))
                       | (1 | 3) as mv ->
                         let nr = node r in
                         let vr = nr.Proof_log.value
                         and dr = nr.Proof_log.down
                         and rr = nr.Proof_log.right in
                         if vx < vr
                         then (
                           let res, f = imgs xr r m in
                           ( res
                           , fact
                               st
                               "posts_x"
                               [ Q (q m)
                               ; V vx
                               ; N (n dx)
                               ; N (n xr)
                               ; N (n r)
                               ; N (n res)
                               ; F f
                               ] ))
                         else if vx > vr
                         then (
                           let res, f = imgs s rr m in
                           ( res
                           , fact
                               st
                               "posts_r"
                               [ Q (q m)
                               ; N (n s)
                               ; V vr
                               ; N (n dr)
                               ; N (n rr)
                               ; N (n res)
                               ; F f
                               ] ))
                         else if mv = 3
                         then (
                           let res = result (Proof_log.Relprod, s, r, m, 0) in
                           let r2, f2 = imgs xr rr m in
                           if img_res dx dr nm.Proof_log.down = 0
                           then (
                             expect res vx 0 r2;
                             ( res
                             , fact
                                 st
                                 "posts_xr"
                                 [ Q (q m)
                                 ; V vx
                                 ; N (n dx)
                                 ; N (n xr)
                                 ; V vr
                                 ; N (n dr)
                                 ; N (n rr)
                                 ; N (n r2)
                                 ; F f2
                                 ] ))
                           else (
                             let r1, f1 = imgs dx dr nm.Proof_log.down in
                             expect res vx r1 r2;
                             ( res
                             , fact
                                 st
                                 "posts_ro"
                                 [ Q (q nm.Proof_log.down)
                                 ; V vx
                                 ; N (n dx)
                                 ; N (n xr)
                                 ; N (n dr)
                                 ; N (n rr)
                                 ; N (n r1)
                                 ; N (n r2)
                                 ; F f1
                                 ; F f2
                                 ] )))
                         else (
                           (* read and write: the write level, the
                              relation's right with the same set, their
                              union *)
                           let res = result (Proof_log.Relprod, s, r, m, 0) in
                           let w, fw = wimgs s dr nm.Proof_log.down in
                           let r2, f2 = imgs s rr m in
                           let u, fu = go Proof_log.Union w r2 in
                           if u <> res
                           then
                             fails "proof log: a result does not match its case";
                           ( res
                           , fact
                               st
                               "posts_rw"
                               [ Q (q (node nm.Proof_log.down).Proof_log.down)
                               ; V vx
                               ; N (n dx)
                               ; N (n xr)
                               ; N (n dr)
                               ; N (n rr)
                               ; N (n w)
                               ; N (n r2)
                               ; N (n u)
                               ; F fw
                               ; F f2
                               ; F fu
                               ] ))
                       | v ->
                         fails "proof log: meta code %d not supported yet" v
                     in
                     Hashtbl.add memo_s (s, r, m) x;
                     x)
               (* the write level [relprod s rw m2], sound: the fact
                  [WlS cs dx rw W], along Sylvan's result: its nodes are the
                  written values whose image is not empty *)
               and wimgs s rw m2 : int * int =
                 let res = result (Proof_log.Relprod, s, rw, m2, 0) in
                 let dx = (node s).Proof_log.down in
                 let md = (node m2).Proof_log.down in
                 let cs = Q (q md) in
                 let rec chain rw w =
                   if rw = 0
                   then (
                     if w <> 0
                     then fails "proof log: a result does not match its case";
                     fact st "wls_0" [ cs; N (n dx); N 0 ])
                   else (
                     let nrw = node rw in
                     let vb = nrw.Proof_log.value
                     and db = nrw.Proof_log.down
                     and rb = nrw.Proof_log.right in
                     if img_res dx db md = 0
                     then (
                       let f = chain rb w in
                       fact
                         st
                         "wls_nd0"
                         [ cs
                         ; N (n dx)
                         ; V vb
                         ; N (n db)
                         ; N (n rb)
                         ; N (n w)
                         ; F f
                         ])
                     else (
                       let r1, f1 = imgs dx db md in
                       let nw = node w in
                       if nw.Proof_log.value <> vb || nw.Proof_log.down <> r1
                       then fails "proof log: a result does not match its case";
                       let f = chain rb nw.Proof_log.right in
                       fact
                         st
                         "wls_nd"
                         [ cs
                         ; N (n dx)
                         ; V vb
                         ; N (n db)
                         ; N (n rb)
                         ; N (n r1)
                         ; N (n nw.Proof_log.right)
                         ; F f1
                         ; F f
                         ]))
                 in
                 res, chain rw res
               in
               (* a proof's requests (a chunk per group, the first chunk's
                  first): each fact, node or list of codes, named on an
                  [R] line *)
               if !pending <> []
               then (
                 let requests = List.hd !pending in
                 pending := List.tl !pending;
                 let srt_memo : (int, int) Hashtbl.t = Hashtbl.create 1024 in
                 let rec srt x =
                   if x = 0
                   then fact st "srt_0" []
                   else if x = 1
                   then fact st "srt_1" []
                   else (
                     match Hashtbl.find_opt srt_memo x with
                     | Some f -> f
                     | None ->
                       let nx = node x in
                       let v = nx.Proof_log.value
                       and d = nx.Proof_log.down
                       and r = nx.Proof_log.right in
                       let fd = srt d in
                       let f =
                         if r = 0
                         then fact st "srt_nd0" [ V v; N (n d); F fd ]
                         else (
                           let nr = node r in
                           let fr = srt r in
                           fact
                             st
                             "srt_ndn"
                             [ V v
                             ; N (n d)
                             ; V nr.Proof_log.value
                             ; N (n nr.Proof_log.down)
                             ; N (n nr.Proof_log.right)
                             ; F (nlt v nr.Proof_log.value)
                             ; F fd
                             ; F fr
                             ])
                       in
                       Hashtbl.add srt_memo x f;
                       f)
                 in
                 (* a membership's steps depend on the node and the
                    rest of the vector only: shared (Sylvan shares
                    subtrees, so many vectors meet at a node) *)
                 let mem_memo : (int * int list, int) Hashtbl.t =
                   Hashtbl.create 4096
                 and sib_memo : (int * int, int) Hashtbl.t =
                   Hashtbl.create 4096
                 in
                 let rec mem x (w : int array) i =
                   let key =
                     x, Array.to_list (Array.sub w i (Array.length w - i))
                   in
                   match Hashtbl.find_opt mem_memo key with
                   | Some f -> f
                   | None ->
                     let f = mem_ x w i in
                     Hashtbl.add mem_memo key f;
                     f
                 (* [Sib x y]: [y] along [x]'s right children, each
                    step's fact shared by every vector scanning from [x] *)
                 and sib x y =
                   let rec go c prev =
                     let f =
                       match Hashtbl.find_opt sib_memo (x, c) with
                       | Some f -> f
                       | None ->
                         let f =
                           match prev with
                           | None -> fact st "sib_refl" [ N (n x) ]
                           | Some (p, pf) ->
                             let np = node p in
                             fact
                               st
                               "sib_step"
                               [ N (n x)
                               ; V np.Proof_log.value
                               ; N (n np.Proof_log.down)
                               ; N (n np.Proof_log.right)
                               ; F pf
                               ]
                         in
                         Hashtbl.add sib_memo (x, c) f;
                         f
                     in
                     if c = y
                     then f
                     else if c <= 1
                     then fails "proof log: a sibling not along its list"
                     else go (node c).Proof_log.right (Some (c, f))
                   in
                   go x None
                 and mem_ x (w : int array) i =
                   if x = 1 && i = Array.length w
                   then fact st "in1" []
                   else if x <= 1 || i >= Array.length w
                   then fails "proof log: a vector not in its set"
                   else (
                     (* the sibling holding the vector's next value *)
                     let rec find y =
                       if y <= 1
                       then fails "proof log: a vector not in its set"
                       else (
                         let ny = node y in
                         if ny.Proof_log.value = w.(i)
                         then y
                         else if ny.Proof_log.value > w.(i)
                         then fails "proof log: a vector not in its set"
                         else find ny.Proof_log.right)
                     in
                     let y = find x in
                     let ny = node y in
                     fact
                       st
                       "in_sib_d"
                       [ N (n x)
                       ; V ny.Proof_log.value
                       ; N (n ny.Proof_log.down)
                       ; N (n ny.Proof_log.right)
                       ; L (Array.sub w (i + 1) (Array.length w - i - 1))
                       ; F (sib x y)
                       ; F (mem ny.Proof_log.down w (i + 1))
                       ])
                 in
                 List.iteri
                   (fun k rq ->
                     let arg =
                       match rq with
                       | Img_sound (s, r, m) -> F (snd (imgs s r m))
                       | Img_complete (s, r, m) -> F (snd (img s r m))
                       | Union (a, b) -> F (snd (go Proof_log.Union a b))
                       | Minus (a, b) -> F (snd (go Proof_log.Minus a b))
                       | Srt x -> F (srt x)
                       | Mem (x, w) -> F (mem x w 0)
                       | Node x -> N (n x)
                       | Codes m -> Q (q m)
                     in
                     Printf.bprintf st.buf "R %d" k;
                     write_arg st.buf arg;
                     line st)
                   requests);
               (* roots until the budget, then their facts in a chain *)
               let done_ = ref [] in
               while !left <> [] && (st.nfact < max_facts || !done_ = []) do
                 match !left with
                 | [] -> ()
                 | (r : Proof_log.record) :: tl ->
                   (match r.op with
                    | Proof_log.Relprod ->
                      (* the directions its use needs ([case]: 1 sound, 2
                         complete, 3 both) *)
                      let same res =
                        if res <> r.r
                        then
                          fails
                            "proof log: an image's facts give %d, its record %d"
                            res
                            r.r
                      in
                      if r.case land 2 <> 0
                      then (
                        let res, f = img r.a r.b r.c in
                        same res;
                        done_ := (`C r, f) :: !done_);
                      if r.case land 1 <> 0
                      then (
                        let res, f = imgs r.a r.b r.c in
                        same res;
                        done_ := (`S r, f) :: !done_)
                    | op -> done_ := (`R r, snd (go op r.a r.b)) :: !done_);
                   left := tl
               done;
               List.fold_left
                 (fun acc (e, f) ->
                   match e with
                   | `C (r : Proof_log.record) ->
                     fact
                       st
                       "chk_post"
                       [ Q (q r.c)
                       ; N (n r.a)
                       ; N (n r.b)
                       ; N (n r.r)
                       ; F f
                       ; F acc
                       ]
                   | `S (r : Proof_log.record) ->
                     fact
                       st
                       "chk_posts"
                       [ Q (q r.c)
                       ; N (n r.a)
                       ; N (n r.b)
                       ; N (n r.r)
                       ; F f
                       ; F acc
                       ]
                   | `R (r : Proof_log.record) ->
                     (match r.op with
                      | op ->
                        fact
                          st
                          (if op = Proof_log.Union then "chk_u" else "chk_mi")
                          [ N (n r.a); N (n r.b); N (n r.r); F f; F acc ]))
                 (fact st "chk_nil" [])
                 !done_));
          log_mark
            (Printf.sprintf
               "chunk %d written: %d facts in all (%s)"
               st.nchunk
               st.total_facts
               (String.concat
                  ", "
                  (List.filteri
                     (fun i _ -> i < 12)
                     (List.map
                        (fun (l, n) -> Printf.sprintf "%s %d" l n)
                        (List.sort
                           (fun (_, a) (_, b) -> compare b a)
                           (Hashtbl.fold
                              (fun l r acc -> (l, !r) :: acc)
                              per_lemma
                              []))))));
          Hashtbl.reset per_lemma;
          chunks_of !left)
      in
      chunks_of (if proof_mode then [] else roots);
      { chunks = st.nchunk
      ; facts = st.total_facts
      ; locals = st.total_locals
      ; seconds = Unix.gettimeofday () -. t0
      })
;;
