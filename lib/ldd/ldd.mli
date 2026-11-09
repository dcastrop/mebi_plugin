(** Sets of integer vectors, and relations between them, as Sylvan's List
    Decision Diagrams (LDDs) ([lib/ldd/README.md]).

    An LDD is a set of vectors of naturals below [2^32 - 1], each value a
    label of the diagram, never bits: domains need no bound and no width.
    Diagrams are canonical: two sets are equal exactly when their LDDs are.

    {b Memory.} A value keeps its nodes alive until OCaml finalises it.
    Loops that make many short-lived sets should {!release} them as soon as
    they are no longer needed: otherwise Sylvan's node table can fill up
    before OCaml's GC runs.

    {b Relations} are LDDs too, read through a meta vector ({!rel_meta})
    that says what each of the relation's levels is. *)

type t

(** [init ?workers ?memory_mb ()] starts Sylvan, with [workers] Lace worker
    threads (default 1) and at most [memory_mb] MB for its tables (default
    1024). Only the first call has an effect. Every other function needs it
    first. *)
val init : ?workers:int -> ?memory_mb:int -> unit -> unit

(** the empty set *)
val empty : unit -> t

(** the set with one vector of length 0 *)
val epsilon : unit -> t

(** [cube v]: the set [{v}]. Values must lie in [0, 2^32 - 2].
    @raise Invalid_argument if [v] is longer than 4096 *)
val cube : int array -> t

(** [of_list vs]: the set of the vectors [vs], all of one length, merged by
    halves (unions of sets are linear merges, so this is [n log n]). *)
val of_list : int array list -> t

val union : t -> t -> t
val minus : t -> t -> t
val inter : t -> t -> t
val equal : t -> t -> bool
val is_empty : t -> bool
val mem : t -> int array -> bool

(** the number of vectors *)
val count : t -> float

(** the number of nodes *)
val nodes : t -> int

(** [elements ?limit s]: at most [limit] (default 1000) of [s]'s vectors *)
val elements : ?limit:int -> t -> int array list

(** [export roots]: the nodes of [roots], shared, as [(nodes, ids)]:
    [nodes] holds a [(value, down, right)] triple per node, the node
    numbered [k + 2] at positions [3k .. 3k+2] (0 is the empty set, 1 the
    set holding the empty vector), each node after its children; [ids.(r)]
    is [roots.(r)]'s number. For writing diagrams out. *)
val export : t array -> int array * int array

(** [elements_flat s]: [(w, a)], every vector of [s], each [w] values long,
    one after another in [a], in lexicographic order. For sets too large to
    list as OCaml arrays. *)
val elements_flat
  :  t
  -> int * (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t

(** What a relation's level is, for {!rel_meta}. *)
type level =
  | Copy (** a position the relation does not touch: carried unchanged *)
  | Read (** a position read and written: this level is the old value ... *)
  | Write (** ... and the next level the new one *)
  | Only_read (** a position read, and kept *)
  | Only_write (** a position written, whatever it held *)
  | Label (** an action label stored in the relation, not in the state *)

(** [rel_meta levels]: the meta vector of a relation whose levels are
    [levels]; positions after the last are carried unchanged. *)
val rel_meta : level list -> t

(** [relprod s ~rel ~meta]: the image of [s] through [rel] *)
val relprod : t -> rel:t -> meta:t -> t

(** [index s]: Sylvan's index of [s]'s node (0 the empty set, 1 the set
    of the empty vector): the name the proof log gives it, stable while
    the log is open *)
val index : t -> int

(** [relprev s ~rel ~meta ~within]: the vectors of [within] with a step of
    [rel] into [s] *)
val relprev : t -> rel:t -> meta:t -> within:t -> t

(** [project s keep]: [s] with the positions where [keep] is false
    quantified away (positions after [keep]'s end are dropped too) *)
val project : t -> bool list -> t

(** [product a ~width b]: the vectors [u @ v] for [u] in [a] (all of
    length [width]) and [v] in [b] *)
val product : t -> width:int -> t -> t

(** [release s]: give [s]'s nodes back now; [s] is then the empty set *)
val release : t -> unit

(** [table_usage ()]: the nodes in Sylvan's table (live, or dead and not
    yet collected) and the table's maximum size. *)
val table_usage : unit -> int * int

(** [gc ()]: collect the nodes no live value refers to. A value OCaml has
    not finalised yet still counts as live: run OCaml's GC first. *)
val gc : unit -> unit

(** [log_open path]: start Sylvan's proof log (doc/certificate.md,
    A): grow the node table and cache to their maximum, through a
    collection (run OCaml's GC first), which clears the operation cache;
    turn collection off; then union, minus, intersection, [relprod] and
    [relprev] record every result they compute (one temporary file per
    worker, [path.w<i>]). [false] if a file cannot be opened. *)
val log_open : string -> bool

(** [log_close ()]: stop the log; write the one file [path] (the records
    and every node they reach: {!Proof_log}); turn collection back on. The
    number of records. *)
val log_close : unit -> int

(** [log_open_mem ()]: {!log_open}, the log kept in memory (one list of
    buffers per worker), nothing written. *)
val log_open_mem : unit -> bool

(** [log_close_mem ()]: stop a log in memory; collection stays off, so
    that the nodes its records name stay the table's (read them with
    {!node_fields}) until {!log_free}. The number of records. *)
val log_close_mem : unit -> int

(** [log_take buf]: the next records of a log in memory closed, as many
    as fit in [buf] (48 bytes each, as {!Proof_log} reads them from a
    file), each buffer freed once taken: how many (0 at the end). *)
val log_take : Bytes.t -> int

(** [log_free ()]: what a log in memory holds freed; collection back on. *)
val log_free : unit -> unit

(** [node_fields i]: the node of index [i] of the table (not 0 or 1):
    its value, its child below and to the right, whether it is a copy
    node. *)
val node_fields : int -> int * int * int * bool

(** [log_top_relprod ~role s ~rel ~meta r]: while the log is open,
    record that the caller called [relprod s ~rel ~meta] at its top level,
    with result [r], and which of the result's properties its proof uses:
    [role] 1 soundness (every vector of [r] is the image of one of [s]), 2
    completeness (the image of every vector of [s] is in [r]). The record
    names the arguments as passed (before Sylvan skips any node); its
    nodes are written at close (op 6 of {!Proof_log}). *)
val log_top_relprod : role:int -> t -> rel:t -> meta:t -> t -> unit

(** [reclaim ()]: if Sylvan's table is over half full, OCaml's major GC
    (finalising dead values) and then Sylvan's. For loops that make large
    sets. *)
val reclaim : unit -> unit
