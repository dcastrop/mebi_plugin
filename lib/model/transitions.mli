module Make : (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type t = Base.t)
    (Label : Label.S with type t = Base.t Label.t')
    (Labels : sig
       include Set.S with type elt = Label.t
     end)
    (Annotation : sig
       type t
     end)
    (Transition : Transition.S
                  with type t =
                    (State.t, Label.t, Base.Tree.t, Annotation.t) Transition.t')
    -> sig
  include Set.S with type elt = Transition.t

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val labels : t -> Labels.t
end
