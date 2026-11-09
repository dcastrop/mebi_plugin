(** Reading Sylvan's proof log (doc/certificate.md, A; written by the
    fork's [lddmc_log_open]/[lddmc_log_close], driven by {!Ldd.log_open}
    and {!Ldd.log_close}). No Rocq.

    The file: the magic ["MEBILOG1"], the number of records, the records,
    the number of nodes, the nodes (64-bit counts, native byte order). *)

(** The operations the log records. *)
type op =
  | Union
  | Minus
  | Intersect
  | Relprod
  | Relprev
  | Top
  (** a call the caller made at its top level ({!Ldd.log_top_relprod}):
      the case is its role times 16 plus its operation's code (4
      [Relprod], 5 [Relprev]); the arguments as passed *)

(** One result: the operation, the case Sylvan took (for [Union] and
    [Minus], 1, 2 or 3 as the first value of [a] is below, equal to or
    above that of [b], plus 4 and 8 for copy nodes of [b] and [a]; for
    [Intersect] 2; for [Relprod] and [Relprev] the meta code), the
    arguments as the cache sees them (after Sylvan's skipping; [c] is the
    meta of [Relprod] and [Relprev], [d] the universe of [Relprev], unused
    ones 0) and the result. Nodes are indices of Sylvan's table: 0 is the
    empty set, 1 the set of the empty vector. *)
type record =
  { op : op
  ; case : int
  ; a : int
  ; b : int
  ; c : int
  ; d : int
  ; r : int
  }

(** A node of the table: its value, its child below (the rest of the
    vectors with this value) and to the right (the vectors with a larger
    value here), and whether it is a copy node. *)
type node =
  { value : int
  ; down : int
  ; right : int
  ; copy : bool
  }

(** [top_role r]: the role of a [Top] record (1 soundness, 2
    completeness); [top_op r]: its operation *)
val top_role : record -> int

val top_op : record -> op

(** Where a log is: a file, or Sylvan's memory ({!Ldd.log_open_mem}). *)
type source =
  | File of string
  | Memory

(** [source_name src]: the file's path, or ["memory"] *)
val source_name : source -> string

(** [iter src f]: the records to [f], in the order the workers wrote them
    (from memory: each of Sylvan's buffers freed once read); then the
    nodes, by index ([Not_found] for one the log does not name; from
    memory, Sylvan's table, which collection must not have changed:
    {!Ldd.log_close_mem}). Raises [Failure] on a malformed file. *)
val iter : source -> (record -> unit) -> int -> node

(** [read path]: the records, in the order the workers wrote them, and the
    nodes by index. Raises [Failure] on a malformed file. *)
val read : string -> record list * (int, node) Hashtbl.t

(** [read_iter path f]: the records to [f], in order, without keeping
    them; then the nodes by index. Raises [Failure] on a malformed file. *)
val read_iter : string -> (record -> unit) -> (int, node) Hashtbl.t
