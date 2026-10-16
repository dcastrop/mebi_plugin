(* See the [.mli]. *)
exception
  Exceeded of
    { phase : string
    ; used : int
    ; limit : int
    }

(* See the [.mli]. Reading [/proc/self/status] takes some microseconds, so
   a loop calling {!check} per unit of work costs nothing noticeable. *)
let interval : float = 0.02

(** [vm_rss ()] is the [VmRSS] line of [/proc/self/status] in bytes, or
    [None] if there is no such file or line. Raises nothing. *)
let vm_rss () : int option =
  match In_channel.with_open_text "/proc/self/status" In_channel.input_all with
  | exception Sys_error _ -> None
  | status ->
    String.split_on_char '\n' status
    |> List.find_map (fun (line : string) ->
      try Scanf.sscanf line "VmRSS: %d kB" (fun kb -> Some (kb * 1024)) with
      | Scanf.Scan_failure _ | Failure _ | End_of_file -> None)
;;

(* See the [.mli]. *)
let used () : int option =
  match vm_rss () with
  | Some _ as x -> x
  | None ->
    (match (Gc.quick_stat ()).heap_words with
     | 0 -> None
     | w -> Some (w * (Sys.word_size / 8)))
;;

(** [mb b] is [b] bytes in whole megabytes, rounded up, so a reading just
    over the limit never prints as equal to it. Raises nothing. *)
let mb (b : int) : string = Printf.sprintf "%iMB" ((b + 999_999) / 1_000_000)

(** [meminfo_total ()] is [MemTotal] in [/proc/meminfo] in bytes, or
    [None]. Raises nothing. *)
let meminfo_total () : int option =
  match In_channel.with_open_text "/proc/meminfo" In_channel.input_all with
  | exception Sys_error _ -> None
  | info ->
    String.split_on_char '\n' info
    |> List.find_map (fun (line : string) ->
      try Scanf.sscanf line "MemTotal: %d kB" (fun kb -> Some (kb * 1024)) with
      | Scanf.Scan_failure _ | Failure _ | End_of_file -> None)
;;

(** [cgroup_max ()] is the smallest [memory.max] of this process's cgroup
    (v2, from [/proc/self/cgroup]) and its ancestors, in bytes: the cap
    [systemd-run -p MemoryMax=...] sets. [None] if there is none, or no
    cgroup v2. Raises nothing. *)
let cgroup_max () : int option =
  let read (file : string) : string option =
    try
      Some (String.trim (In_channel.with_open_text file In_channel.input_all))
    with
    | Sys_error _ -> None
  in
  match read "/proc/self/cgroup" with
  | None -> None
  | Some lines ->
    (match
       String.split_on_char '\n' lines
       |> List.find_map (fun (line : string) ->
         if String.starts_with ~prefix:"0::" line
         then Some (String.sub line 3 (String.length line - 3))
         else None)
     with
     | None -> None
     | Some path ->
       (* [path] and each of its ancestors, the root excluded *)
       let parts : string list =
         String.split_on_char '/' path |> List.filter (fun x -> x <> "")
       in
       let rec prefixes (acc : string list) (dir : string) = function
         | [] -> acc
         | p :: rest ->
           let dir = dir ^ "/" ^ p in
           prefixes (dir :: acc) dir rest
       in
       prefixes [] "/sys/fs/cgroup" parts
       |> List.filter_map (fun (dir : string) ->
         Stdlib.Option.bind (read (dir ^ "/memory.max")) int_of_string_opt)
       |> List.fold_left
            (fun (acc : int option) (x : int) ->
              match acc with Some y when y <= x -> acc | _ -> Some x)
            None)
;;

(** [gb b] is [b] bytes in GB, to one place. Raises nothing. *)
let gb (b : int) : string = Printf.sprintf "%.1fGB" (Float.of_int b /. 1e9)

(* See the [.mli]. *)
let default : (int * string) option =
  let of_ram : (int * string) option =
    Stdlib.Option.map
      (fun (t : int) -> t / 4 * 3, Printf.sprintf "75%% of %s RAM" (gb t))
      (meminfo_total ())
  in
  let of_cgroup : (int * string) option =
    Stdlib.Option.map
      (fun (c : int) ->
        c / 10 * 9, Printf.sprintf "90%% of this process's %s cap" (gb c))
      (cgroup_max ())
  in
  match of_ram, of_cgroup with
  | Some (r, _), Some (c, _) when c < r -> of_cgroup
  | Some _, _ -> of_ram
  | None, _ -> of_cgroup
;;

(* See the [.mli]. *)
let limit : int option ref = ref (Stdlib.Option.map fst default)

(** When {!check} last read memory; [neg_infinity] forces the next read. *)
let last_read : float ref = ref Float.neg_infinity

(* See the [.mli]. *)
let set_limit (x : int option) : unit =
  limit := x;
  last_read := Float.neg_infinity
;;

(* See the [.mli]. *)
let reset () : unit = set_limit (Stdlib.Option.map fst default)

(** [describe_bound l] is [l] bytes in MB, saying so if it is the
    default. Raises nothing. *)
let describe_bound (l : int) : string =
  match default with
  | Some (d, why) when l = d -> Printf.sprintf "%s (the default, %s)" (mb l) why
  | _ -> mb l
;;

(* See the [.mli]. *)
let describe () : string =
  match !limit with None -> "none" | Some l -> describe_bound l
;;

(* See the [.mli]. *)
let over_bound (bytes : int) : string option =
  match !limit with
  | Some l when bytes > l ->
    Some
      (Printf.sprintf
         "That is more than the memory bound, %s, which would stop it first \
          ([MeBi Config Bounds Memory <mb>])."
         (describe ()))
  | _ -> None
;;

(** [hint phase] is what to try when [phase] passes the bound. Raises
    nothing. *)
let hint : string -> string = function
  | "extraction" ->
    "Extraction holds every state it finds: a lower [MeBi Config Bounds As Num \
     States <n>] stops it sooner."
  | "saturation" ->
    "Saturating whole holds every weak action: [MeBi Config Saturation \
     OnDemand True] saturates each state only when it is needed."
  | "the saturation estimate" ->
    "The estimate needs memory quadratic in the FSM's silent SCCs \
     (doc/limitations.md, \"Many silent SCCs\")."
  | "a game walk" ->
    "The walk visits every pair of states reachable from the start; on an FSM \
     saturated on demand, a lower [MeBi Config Bounds Game <n>] bounds it."
  | "planning answers" ->
    "[MeBi Config Solver Answers Default] answers move by move instead of \
     planning every answer up front."
  | "the cofix estimate" ->
    "[MeBi Config Solver MutualCofix True] (or [False]) skips the estimate."
  | "Solve" ->
    "A proof costs far more memory than a verdict: [MeBi Run Bisim] decides \
     the same question without proving it (doc/decide-vs-prove.md)."
  | _ -> ""
;;

(* See the [.mli]. *)
let message (phase : string) (used : int) (limit : int) : string =
  Printf.sprintf
    "Memory_Bound_Exceeded: the process uses %s, over the memory bound of %s, \
     during %s. The command was stopped. %s If the machine has the memory, \
     raise the bound with [MeBi Config Bounds Memory <mb>] (0 turns it off)."
    (mb used)
    (describe_bound limit)
    phase
    (hint phase)
;;

(** What {!check} last read, [0] before any read. *)
let last_used : int ref = ref 0

(* See the [.mli]. A check skips the read only while the last one was
   under the limit: otherwise a command that starts within {!interval} of
   one stopped by the limit would skip its read and run on. *)
let check (phase : string) : unit =
  match !limit with
  | None -> ()
  | Some limit ->
    let now : float = Unix.gettimeofday () in
    if now -. !last_read >= interval || !last_used > limit
    then (
      last_read := now;
      match used () with
      | None -> ()
      | Some used ->
        last_used := used;
        if used > limit then raise (Exceeded { phase; used; limit }))
;;
