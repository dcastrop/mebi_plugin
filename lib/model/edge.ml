module type S = sig
  type state
  type label
  type action

  type t =
    { from : state
    ; goto : state
    ; action : action
    }

  include Json.S with type k = t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val is_silent : t -> bool
  val is_labelled : label -> t -> bool
end

module Make
    (Log : Logger.S)
    (State : State.S)
    (Label : Label.S)
    (Action : Action.S with type label = Label.t) :
  S
  with type state = State.t
   and type label = Label.t
   and type action = Action.t = struct
  type state = State.t
  type label = Label.t
  type action = Action.t

  type t =
    { from : state
    ; goto : state
    ; action : action
    }

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "Edge"

        let json ?(as_elt : bool = false) (x : t) : Yojson.t =
          `Assoc
            [ "from", State.json x.from
            ; "goto", State.json x.goto
            ; "action", Action.json x.action
            ]
        ;;
      end)

  let equal (a : t) (b : t) : bool =
    State.equal a.from b.from
    && State.equal a.goto b.goto
    && Action.equal a.action b.action
  ;;

  let compare (a : t) (b : t) : int =
    Utils.compare_chain
      [ State.compare a.from b.from
      ; State.compare a.goto b.goto
      ; Action.compare a.action b.action
      ]
  ;;

  let is_silent (x : t) : bool = Action.is_silent x.action
  let is_labelled (x : Label.t) (y : t) : bool = Action.is_labelled x y.action
end
