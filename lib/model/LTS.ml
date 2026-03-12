module type S = sig
  type state
  type states
  type labels
  type transitions
  type info

  type t =
    { init : state option
    ; terminals : states
    ; alphabet : labels
    ; states : states
    ; transitions : transitions
    ; info : info
    }

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
end

module Make
    (Log : Logger.S)
    (State : State.S)
    (States : States.S with type elt = State.t)
    (Labels : Labels.S)
    (Transitions : Transitions.S with type labels = Labels.t)
    (Info : Info.S with type base = State.base and type labels = Labels.t) :
  S
  with type state = State.t
   and type states = States.t
   and type labels = Labels.t
   and type transitions = Transitions.t
   and type info = Info.t = struct
  type state = State.t
  type states = States.t
  type labels = Labels.t
  type transitions = Transitions.t
  type info = Info.t

  type t =
    { init : state option
    ; terminals : states
    ; alphabet : labels
    ; states : states
    ; transitions : transitions
    ; info : info
    }

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "LTS"

        let json ?as_elt (x : t) : Yojson.t =
          `Assoc
            [ "init", Json.option ~as_elt:true State.json x.init
            ; "info", Info.json ~as_elt:true x.info
            ; "terminals", States.json ~as_elt:true x.terminals
            ; "alphabet", Labels.json ~as_elt:true x.alphabet
            ; "states", States.json ~as_elt:true x.states
            ; "transitions", Transitions.json ~as_elt:true x.transitions
            ]
        ;;
      end)
end
