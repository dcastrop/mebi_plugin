module type S = sig
  type state
  type label
  type trees

  type t =
    { from : state
    ; label : label
    ; using : trees
    ; goto : state
    }

  include Json.S with type k = t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val is_silent : t -> bool
  val has_label : label -> t -> bool
end

module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type base = Base.t)
    (Label : Label.S with type base = Base.t) :
  S
  with type state = State.t
   and type label = Label.t
   and type trees = Base.Trees.t
