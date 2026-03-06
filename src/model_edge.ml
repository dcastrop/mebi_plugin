module Make
    (Log : Logger.S)
    (State : sig
       type t

       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
       val equal : t -> t -> bool
       val compare : t -> t -> int
       val hash : t -> int
     end)
    (Label : sig
       type t

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val hash : t -> int
       val is_silent : t -> bool
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (Action : sig
       type t

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val hash : t -> int
       val wk_equal : t -> t -> bool
       val is_silent : t -> bool
       val is_labelled : Label.t -> t -> bool
       val shorter_annotation : t -> t -> t
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end) : sig
  type t =
    { from : State.t
    ; goto : State.t
    ; action : Action.t
    }

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val is_silent : t -> bool
  val is_labelled : Label.t -> t -> bool
  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
end = struct
  type t =
    { from : State.t
    ; goto : State.t
    ; action : Action.t
    }

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
end
