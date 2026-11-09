(* See the [.mli]. The formats are the fork's ([vendor/sylvan],
   [sylvan_ldd.c], [lddmc_log_close], [ldd_log_rec], [ldd_log_node]),
   native byte order: a record is 48 bytes (op, case, 6 bytes of padding,
   then a, b, c, d, r as 64-bit words); a node 32 bytes (index, down,
   right as 64-bit words, value as a 32-bit word, the copy flag as a byte,
   3 bytes of padding). *)

type op =
  | Union
  | Minus
  | Intersect
  | Relprod
  | Relprev
  | Top

type record =
  { op : op
  ; case : int
  ; a : int
  ; b : int
  ; c : int
  ; d : int
  ; r : int
  }

type node =
  { value : int
  ; down : int
  ; right : int
  ; copy : bool
  }

let op_of = function
  | 1 -> Union
  | 2 -> Minus
  | 3 -> Intersect
  | 4 -> Relprod
  | 5 -> Relprev
  | 6 -> Top
  | n -> failwith (Printf.sprintf "proof log: unknown operation %d" n)
;;

let top_role (r : record) = r.case lsr 4
let top_op (r : record) = op_of (r.case land 15)

let word (b : Bytes.t) (o : int) : int =
  if Sys.big_endian
  then Int64.to_int (Bytes.get_int64_be b o)
  else Int64.to_int (Bytes.get_int64_le b o)
;;

let word32 (b : Bytes.t) (o : int) : int =
  let x =
    if Sys.big_endian then Bytes.get_int32_be b o else Bytes.get_int32_le b o
  in
  Int32.to_int x land 0xFFFFFFFF
;;

(* the record at [o] of [b] *)
let record_at (b : Bytes.t) (o : int) : record =
  { op = op_of (Bytes.get_uint8 b o)
  ; case = Bytes.get_uint8 b (o + 1)
  ; a = word b (o + 8)
  ; b = word b (o + 16)
  ; c = word b (o + 24)
  ; d = word b (o + 32)
  ; r = word b (o + 40)
  }
;;

(* the records to [f], one at a time; the nodes in a table *)
let read_iter (path : string) (f : record -> unit) : (int, node) Hashtbl.t =
  In_channel.with_open_bin path (fun ic ->
    let get n =
      let b = Bytes.create n in
      match In_channel.really_input ic b 0 n with
      | Some () -> b
      | None -> failwith (Printf.sprintf "proof log: %s is truncated" path)
    in
    if Bytes.to_string (get 8) <> "MEBILOG1"
    then failwith (Printf.sprintf "proof log: %s is not a proof log" path);
    let nrec = word (get 8) 0 in
    let b = Bytes.create 48 in
    for _ = 1 to nrec do
      (match In_channel.really_input ic b 0 48 with
       | Some () -> ()
       | None -> failwith (Printf.sprintf "proof log: %s is truncated" path));
      f (record_at b 0)
    done;
    let nnodes = word (get 8) 0 in
    let t = Hashtbl.create (max 16 nnodes) in
    let nb = Bytes.create 32 in
    for _ = 1 to nnodes do
      (match In_channel.really_input ic nb 0 32 with
       | Some () -> ()
       | None -> failwith (Printf.sprintf "proof log: %s is truncated" path));
      Hashtbl.replace
        t
        (word nb 0)
        { down = word nb 8
        ; right = word nb 16
        ; value = word32 nb 24
        ; copy = Bytes.get_uint8 nb 28 <> 0
        }
    done;
    t)
;;

type source =
  | File of string
  | Memory

let source_name = function File path -> path | Memory -> "memory"

(* a log in memory: its records to [f], taken from Sylvan's buffers a
   block at a time (each freed once taken); the nodes, Sylvan's *)
let iter (src : source) (f : record -> unit) : int -> node =
  match src with
  | File path ->
    let t = read_iter path f in
    fun i -> Hashtbl.find t i
  | Memory ->
    let n = 4096 in
    let b = Bytes.create (48 * n) in
    let rec take () =
      let k = Ldd.log_take b in
      if k > 0
      then (
        for j = 0 to k - 1 do
          f (record_at b (48 * j))
        done;
        take ())
    in
    take ();
    fun i ->
      if i <= 1 then raise Not_found;
      let value, down, right, copy = Ldd.node_fields i in
      { value; down; right; copy }
;;

let read (path : string) : record list * (int, node) Hashtbl.t =
  In_channel.with_open_bin path (fun ic ->
    let get n =
      let b = Bytes.create n in
      match In_channel.really_input ic b 0 n with
      | Some () -> b
      | None -> failwith (Printf.sprintf "proof log: %s is truncated" path)
    in
    if Bytes.to_string (get 8) <> "MEBILOG1"
    then failwith (Printf.sprintf "proof log: %s is not a proof log" path);
    let nrec = word (get 8) 0 in
    let recs =
      List.init nrec (fun _ ->
        let b = get 48 in
        { op = op_of (Bytes.get_uint8 b 0)
        ; case = Bytes.get_uint8 b 1
        ; a = word b 8
        ; b = word b 16
        ; c = word b 24
        ; d = word b 32
        ; r = word b 40
        })
    in
    let nnodes = word (get 8) 0 in
    let t = Hashtbl.create (max 16 nnodes) in
    for _ = 1 to nnodes do
      let b = get 32 in
      Hashtbl.replace
        t
        (word b 0)
        { down = word b 8
        ; right = word b 16
        ; value = word32 b 24
        ; copy = Bytes.get_uint8 b 28 <> 0
        }
    done;
    recs, t)
;;
