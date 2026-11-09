(* See the [.mli]. *)

type t

external init_c : int -> int -> unit = "ml_ldd_init"
external empty : unit -> t = "ml_ldd_false"
external epsilon : unit -> t = "ml_ldd_true"
external cube_c : int array -> t = "ml_ldd_cube"
external union_cubes : t -> int array array -> t = "ml_ldd_union_cubes"
external union : t -> t -> t = "ml_ldd_union"
external minus : t -> t -> t = "ml_ldd_minus"
external inter : t -> t -> t = "ml_ldd_intersect"
external relprod_c : t -> t -> t -> t = "ml_ldd_relprod"
external relprev_c : t -> t -> t -> t -> t = "ml_ldd_relprev"
external index : t -> int = "ml_ldd_index"
external project_c : t -> t -> t = "ml_ldd_project"
external count : t -> float = "ml_ldd_count"
external nodes : t -> int = "ml_ldd_nodes"
external equal : t -> t -> bool = "ml_ldd_equal"
external mem : t -> int array -> bool = "ml_ldd_mem"
external release : t -> unit = "ml_ldd_release"
external table_usage : unit -> int * int = "ml_ldd_table_usage"
external gc : unit -> unit = "ml_ldd_gc"
external log_open : string -> bool = "ml_ldd_log_open"
external log_close : unit -> int = "ml_ldd_log_close"
external log_open_mem : unit -> bool = "ml_ldd_log_open_mem"
external log_close_mem : unit -> int = "ml_ldd_log_close_mem"
external log_take : Bytes.t -> int = "ml_ldd_log_take"
external log_free : unit -> unit = "ml_ldd_log_free"
external node_fields : int -> int * int * int * bool = "ml_ldd_node"
external log_top_c : int -> t array -> unit = "ml_ldd_log_top"
external join_c : t -> t -> t -> t -> t = "ml_ldd_join"
external elements_c : t -> int -> int array list = "ml_ldd_elements"
external export : t array -> int array * int array = "ml_ldd_export"

external elements_flat
  :  t
  -> int * (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
  = "ml_ldd_elements_flat"

let init ?(workers = 1) ?(memory_mb = 1024) () = init_c workers memory_mb
let max_value = 0xFFFFFFFE

let check (v : int array) : unit =
  Array.iter
    (fun x ->
      if x < 0 || x > max_value
      then invalid_arg "Ldd: a value outside [0, 2^32 - 2]")
    v
;;

let cube (v : int array) : t =
  check v;
  cube_c v
;;

let of_list (vs : int array list) : t =
  let a = Array.of_list vs in
  Array.iter check a;
  let rec build lo hi =
    if hi - lo <= 64
    then union_cubes (empty ()) (Array.sub a lo (hi - lo))
    else (
      let mid = (lo + hi) / 2 in
      let l = build lo mid in
      let r = build mid hi in
      let u = union l r in
      release l;
      release r;
      u)
  in
  if Array.length a = 0 then empty () else build 0 (Array.length a)
;;

let is_empty (s : t) : bool = equal s (empty ())
let elements ?(limit = 1000) (s : t) = elements_c s limit

type level =
  | Copy
  | Read
  | Write
  | Only_read
  | Only_write
  | Label

(* Sylvan's codes (sylvan_ldd.c, lddmc_relprod): 0 copy, 1 read, 2 write,
   3 only-read, 4 only-write, 5 action label, -1 the end *)
let code = function
  | Copy -> 0
  | Read -> 1
  | Write -> 2
  | Only_read -> 3
  | Only_write -> 4
  | Label -> 5
;;

let rel_meta (levels : level list) : t =
  cube_c (Array.of_list (List.map code levels @ [ 0xFFFFFFFF ]))
;;

let relprod s ~rel ~meta = relprod_c s rel meta
let relprev s ~rel ~meta ~within = relprev_c s rel meta within

(* lddmc_project's codes: 1 keep, 0 quantify, -2 the end (quantify the
   rest) *)
let project (s : t) (keep : bool list) : t =
  let m =
    cube_c
      (Array.of_list
         (List.map (fun k -> if k then 1 else 0) keep @ [ 0xFFFFFFFE ]))
  in
  let r = project_c s m in
  release m;
  r
;;

(* lddmc_join's projections: 1 the set has this level, 0 it has not, -1
   the end (it has the rest), -2 the end (it has nothing more) *)
let product (a : t) ~(width : int) (b : t) : t =
  let pa =
    cube_c (Array.of_list (List.init width (fun _ -> 1) @ [ 0xFFFFFFFE ]))
  in
  let pb =
    cube_c (Array.of_list (List.init width (fun _ -> 0) @ [ 0xFFFFFFFF ]))
  in
  let r = join_c a b pa pb in
  release pa;
  release pb;
  r
;;

let reclaim () =
  let filled, max = table_usage () in
  if filled > max / 2
  then (
    Gc.full_major ();
    gc ())
;;

(* op 4: Sylvan's code for relprod in the log *)
let log_top_relprod ~role s ~rel ~meta r =
  log_top_c ((role lsl 4) lor 4) [| s; rel; meta; empty (); r |]
;;
