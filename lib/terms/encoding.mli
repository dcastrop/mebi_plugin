(** Encodings: terms that stand for Rocq terms in the model, handed out
    in order by a counter (the table between the two is
    {!Bi_encoding}). *)
module type S = sig
  include Base_term.S

  include Json.S with type k = t (** @closed *)

  (** The first encoding. *)
  val init : t

  (** [next e] is the encoding after [e]. *)
  val next : t -> t

  (** [reset ()] restarts the counter at {!init}. Raises nothing. *)
  val reset : unit -> unit

  (** [incr ()] is a fresh encoding: the counter's value (so {!init}
      first), the counter moving to the next. Raises nothing. *)
  val incr : unit -> t
end

(** What {!Make} needs: the first encoding and the one after each. *)
module type Args = sig
  type t

  val init : t
  val next : t -> t
end

(** [Make (Base) (X)] is {!S} over [Base], counting from [X.init] by
    [X.next], with one counter for the module. *)
module Make (Base : Base_term.S) (X : Args with type t = Base.t) :
  S with type t = Base.t

(** Encodings built from one module of arguments. *)
module Packed : sig
  (** The arguments of both {!Base_term.Make} and {!Make}, for one type. *)
  module type PackedS = sig
    type t

    module BaseArgs : Base_term.Args with type t = t
    module EncodingArgs : Args with type t = t
  end

  (** Integer encodings, from 0. *)
  module Int : PackedS with type t = Int.t

  (** [Unpack (Args)] is {!S} for [Args.t]: {!Base_term.Make} of its
      arguments, then {!Make}. *)
  module Unpack (Args : PackedS) : S with type t = Args.t
end
