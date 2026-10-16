(* See the [.mli]. *)
exception
  Exceeded of
    { phase : string
    ; used : int
    ; limit : int
    }

(* See the [.mli]. *)
let limit : int option ref = ref None

(** When {!check} last read memory; [neg_infinity] forces the next read. *)
let last_read : float ref = ref Float.neg_infinity

(* See the [.mli]. *)
let set_limit (x : int option) : unit =
  limit := x;
  last_read := Float.neg_infinity
;;

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

(* See the [.mli]. *)
let message (phase : string) (used : int) (limit : int) : string =
  Printf.sprintf
    "Memory_Bound_Exceeded: the process uses %s, over the bound of %s set by \
     MeBi Config Bounds Memory, during %s. The command was stopped. Raise the \
     bound, or remove it with MeBi Config Reset Bounds."
    (mb used)
    (mb limit)
    phase
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
