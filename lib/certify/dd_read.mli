(** A facts file ({!Dd_cert.write_log}) read back as Rocq terms
    (doc/certificate.md, A): each chunk one closed term, its nodes and the
    facts it uses twice or more as [let]s, each fact the application of a
    lemma of theories/DdTree.v (registered as [mebi.dt.<name>]) to its
    arguments; nothing is evaluated. *)

type stats =
  { chunks : int
  ; facts : int
  ; nodes : int (** the chunks' nodes, counted once per chunk *)
  ; seconds : float
  }

(** [read env ~declare path]: each chunk of the facts file [path], the
    largest first, given to [declare i statement proof] ([i] from 1; the
    statement [True], the proof the chunk's facts in a chain), which
    declares it, and so has the kernel check it, before the next is read.
    Raises [Failure] on a malformed file or an unknown lemma. *)
val read
  :  Environ.env
  -> declare:(int -> Constr.types -> Constr.t -> unit)
  -> string
  -> stats

(** A proof's chunk, read back (doc/certificate.md, "The theorem from the
    certificate"): its [let]s (nodes and facts, outermost first), their
    number, and [req k d], the [k]th request's fact, node or list of codes
    as a term under [d] [let]s. *)
type proof_ctx =
  { lets : (Constr.t * Constr.types) list
  ; depth : int
  ; req : int -> int -> Constr.t
  }

(** A proof's chunk, read a line at a time as {!Dd_cert.write_log}
    writes it with requests ([Dd_cert.To_lines feed]): [finish ()], once
    it has ended, its facts. *)
type proof_reader =
  { feed : string -> unit
  ; finish : unit -> proof_ctx
  }

(** [proof_reader env]: a reader for one proof's chunk *)
val proof_reader : Environ.env -> proof_reader
