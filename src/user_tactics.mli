(** The tactics [sim], [bisim], [weak_sim] and [weak_bisim], on a
    [strong_sim], [strong_bisimilar], [weak_sim] or [weak_bisimilar] goal
    between closed terms, and [MeBi Check Log].
    {[
      Example pq : weak_bisimilar compLTS compLTS p q.
      Proof. weak_bisim. Qed.
    ]}
    Each tactic reads from its goal the two LTSs and the two terms (weak:
    [None] is the silent step, as in [theories/Bisimilarity.v]; strong: no
    label is silent), and the engine (lib/learn, doc/learn.md) decides it on
    Sylvan's diagrams. Sylvan logs the decision's last checks, the
    certificate (doc/certificate.md), in [.File.v.mebi/<lemma>.log] next
    to the file, and the tactic closes the goal with the proof built from
    it ([Lts_proof.qed_term]), which the kernel checks at [Qed]. The log
    is then deleted, unless [Set MeBi Log Mode "keep"]. [debug] before any
    of them: the engine's report is printed too. *)

type kind =
  | Bisim (** a bisimilarity goal *)
  | Sim (** a simulation goal *)

(** [prove ?debug ~weak kind] is the tactic [bisim] or [sim], or with
    [weak] [weak_bisim] or [weak_sim] ([debug ...] with [debug]).
    @raise CErrors.UserError
      when run, if the goal is not of its kind (naming the other tactic if
      it is of that one), not between closed terms, or over an LTS that is
      not an inductive [M -> option A -> M -> Prop] without parameters; if a
      step cannot be followed from the rules; if the verdict is negative;
      or if the proof cannot be built from the certificate. *)
val prove : ?debug:bool -> weak:bool -> kind -> unit Proofview.tactic

(** [check_log file] ([MeBi Check Log]): the records of Sylvan's proof log
    [file] (left by [Set MeBi Log Mode "keep"]; relative to the checked
    file's directory) as facts of theories/DdTree.v, each chunk its own
    constant [mebi_log<n>_c<i>], checked by the kernel when declared.
    @raise CErrors.UserError if the log does not translate *)
val check_log : string -> unit
