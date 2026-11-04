(** The extraction machinery over {!Rocq_monad}: term utilities, the
    plugin's errors, inductive LTS definitions ([Ind]), and the unification
    that finds a term's transitions ([Unification]). Remember that monadic
    values raise when run, not when built ({!Rocq_monad}). *)
module type S = Rocq_monad_utils_intf.S

(** What made the LTS being extracted approximate (undecided premises,
    incomplete premise searches, undetermined transitions), noted by the
    extraction's warnings every time they apply. [reset] before an
    extraction, [get] after it. *)
module Approximations : sig
  (** [reset ()] forgets every note. Raises nothing. *)
  val reset : unit -> unit

  (** [note s] records [s]. Raises nothing. *)
  val note : string -> unit

  (** [get ()] is the notes recorded since {!reset}, oldest first. Raises nothing.
  *)
  val get : unit -> string list
end

module Make (Enc : Encoding.S) :
  S with type enc = Enc.t and type tree = Enc.Tree.t
