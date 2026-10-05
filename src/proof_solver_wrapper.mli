module type S = sig
  (** [gl ()] is the goal in focus. Raises nothing. *)
  val gl : unit -> Proofview.Goal.t

  (** [get_concl ()] is the conclusion of the goal in focus. Raises nothing. *)
  val get_concl : unit -> Evd.econstr

  (** [get_hyps ()] is the hypotheses of the goal in focus. Raises nothing. *)
  val get_hyps : unit -> Rocq_utils.hyp list

  (** [get_hyp_name h] is the name of the hypothesis [h]. Raises nothing. *)
  val get_hyp_name : Rocq_utils.hyp -> Names.Id.t

  (** [get_hyp_names ()] is the names of the goal's hypotheses. Raises
      nothing. *)
  val get_hyp_names : unit -> Names.Id.Set.t

  (** [next_name_of names x] is [x], or [x] renumbered, so it is none of
      [names]. Raises nothing. *)
  val next_name_of : Names.Id.Set.t -> Names.Id.t -> Names.Id.t

  (** [new_name_of_string s] is a name from [s] that no hypothesis has.
      Raises nothing. *)
  val new_name_of_string : string -> Names.Id.t

  (** [new_cofix_name ()] is a fresh name for a coinduction hypothesis
      ([Cofix0], [Cofix1], ...). Raises nothing. *)
  val new_cofix_name : unit -> Names.Id.t

  val new_H_name : unit -> Names.Id.t

  (** [get_all_cofix_hyp_names ()] is the names of the coinduction
      hypotheses ([Cofix...]). Raises nothing. *)
  val get_all_cofix_hyp_names : unit -> Names.Id.Set.t

  (** [get_all_non_cofix_hyp_names ()] is the names of the other
      hypotheses. Raises nothing. *)
  val get_all_non_cofix_hyp_names : unit -> Names.Id.Set.t

  include Rocq_monad_utils.S

  (** [log_concl ()] logs the conclusion. Raises nothing. *)
  val log_concl : unit -> unit

  (** [log_hyps ()] logs the hypotheses. Raises nothing. *)
  val log_hyps : unit -> unit

  (** Sets of terms, compared within one proof step only. *)
  module EConstrSet : sig
    include Set.S with type elt = EConstr.t
  end
end

module type Args = sig
  (** The goal in focus, updated as the proof advances. *)
  val gl : Proofview.Goal.t ref
end

(** Builds a proof step its own monad/encoding stack, reading [X.gl]'s
    [env]/[sigma] rather than the global environment. Kept separate from the
    command-time stack because a [Bi_encoding] table is only consistent under
    one context; see the note on [I] in the implementation. *)
module Make (Enc : Encoding.S) (X : Args) :
  S with type enc = Enc.t and type tree = Enc.Tree.t
