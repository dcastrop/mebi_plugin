module type S = sig
  type weak
  type 'a mm

  (** The two systems' silent labels, encoded. *)
  type weak_args =
    { a : weak option
    ; b : weak option
    }

  (** The silent labels loaded by {!load_weak_args}, if any. *)
  val the_weak_args : weak_args ref option ref

  (** [load_weak_args ()] sets {!the_weak_args} from {!Api.the_weak_args},
      encoded. Raises as {!load_weak_arg}, when run. *)
  val load_weak_args : unit -> unit mm

  (** [get_the_weak_arg1 ()] is the first system's silent label, if loaded.
      Raises nothing. *)
  val get_the_weak_arg1 : unit -> weak option

  (** [get_the_weak_arg2 ()] is the second system's silent label, if
      loaded. Raises nothing. *)
  val get_the_weak_arg2 : unit -> weak option

  (** [get_weak w] is [w], or, if it is [None], the first system's silent
      label ({!get_the_weak_arg1}). Raises nothing. *)
  val get_weak : weak option -> weak option

  (** The exploration bounds loaded by {!load_the_bounds_args}. *)
  val the_bounds_args : Api.bounds_args ref

  (** [load_the_bounds_args ()] sets {!the_bounds_args} from
      {!Api.the_bounds_args}. Raises nothing. *)
  val load_the_bounds_args : unit -> unit
end

module Make
    (Enc : Encoding.S)
    (M : Rocq_monad_utils.S with type enc = Enc.t)
    (Weak : Weak.S with type enc = Enc.t) :
  S with type weak = Weak.t and type 'a mm = 'a M.mm
