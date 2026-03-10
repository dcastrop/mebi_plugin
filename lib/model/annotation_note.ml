module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type t = Base.t)
    (Label : Label.S with type t = Base.t Label.t') : sig
  type t =
    { from : State.t
    ; label : Label.t
    ; using : Base.Trees.t
    ; goto : State.t
    }

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val is_silent : t -> bool
  val has_label : Label.t -> t -> bool
end = struct
  type t =
    { from : State.t
    ; label : Label.t
    ; using : Base.Trees.t
    ; goto : State.t
    }

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "Note"

        let json ?(as_elt : bool = false) (x : t) : Yojson.t =
          `Assoc
            [ "from", State.json ~as_elt:true x.from
            ; "label", Label.json ~as_elt:true x.label
            ; "goto", State.json ~as_elt:true x.goto
            ; "using", Base.Trees.json ~as_elt:true x.using
            ]
        ;;
      end)

  let equal (a : t) (b : t) : bool =
    State.equal a.from b.from
    && State.equal a.goto b.goto
    && Label.equal a.label b.label
    && Base.Trees.equal a.using b.using
  ;;

  let compare (a : t) (b : t) : int =
    Utils.compare_chain
      [ State.compare a.from b.from
      ; State.compare a.goto b.goto
      ; Label.compare a.label b.label
      ; Base.Trees.compare a.using b.using
      ]
  ;;

  let is_silent (x : t) : bool = Label.is_silent x.label
  let has_label (x : Label.t) (y : t) : bool = Label.equal x y.label
end
