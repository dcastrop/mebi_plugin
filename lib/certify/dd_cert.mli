(** Sylvan's proof log as facts (doc/certificate.md, A): each record of the
    log ({!Proof_log}), the result of one LDD operation in the case Sylvan
    took, becomes the application of a lemma of theories/DdTree.v to its
    nodes. No Rocq: the facts go to a file that {!Dd_read} reads back. *)

type stats =
  { chunks : int
  ; facts : int
  ; locals : int (** the chunks' nodes, counted once per chunk *)
  ; seconds : float
  }

exception Fails of string

(** What a proof asks the translation for, by Sylvan's node indices. *)
type request =
  | Img_sound of int * int * int (** [PostS cs s r R] for [relprod s r m] *)
  | Img_complete of int * int * int (** [PostC cs s r R] *)
  | Union of int * int (** [U a b c] *)
  | Minus of int * int (** [Mi a b c] *)
  | Srt of int (** [Srt x] *)
  | Mem of int * int array (** [In_t x w] *)
  | Node of int (** the node itself *)
  | Codes of int (** a meta's list of codes *)

(** Where the facts go: a file, or each line (without its newline) to a
    reader ({!Dd_read.proof_reader}), nothing written. *)
type output =
  | To_file of string
  | To_lines of (string -> unit)

(** [write_log ~log out]: the records of Sylvan's proof log [log] (a
    file, or Sylvan's memory), as facts of theories/DdTree.v, into [out]:
    chunks of kind [check] (statement [True]), each the facts of some roots
    in a chain, with nodes of its own. The roots: the caller's top-level
    images ([Top] records), each proved the way its use needs (sound or
    complete), and the unions and differences no other record calls. With
    [requests] (a proof's), one chunk holding what each request asks for,
    each answered on an [R] line ([Dd_read.proof_reader]); then, per list
    of [groups], one chunk more, holding what its requests ask for, with
    nodes of its own.
    @raise Fails when a record is missing or does not match its case *)
val write_log
  :  ?requests:request list
  -> ?groups:request list list
  -> log:Proof_log.source
  -> output
  -> stats

(** [log_mark what]: under [MEBI_CERT_STATS], a timestamped line with the
    process's memory, now and at most so far, on stderr *)
val log_mark : string -> unit
