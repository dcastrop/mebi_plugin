module type S = sig
  type state
  type label
  type tree
  type annotation

  type t =
    { from : state
    ; goto : state
    ; label : label
    ; tree : tree option
    ; annotation : annotation option
    }

  include Json.S with type k = t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val is_silent : t -> bool
end

module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type base = Base.t)
    (Label : Label.S with type base = Base.t)
    (Annotation : Annotation.S with type label = Label.t) :
  S
  with type state = State.t
   and type label = Label.t
   and type tree = Base.Tree.t
   and type annotation = Annotation.t = struct
  type state = State.t
  type label = Label.t
  type tree = Base.Tree.t
  type annotation = Annotation.t

  type t =
    { from : state
    ; goto : state
    ; label : label
    ; tree : tree option
    ; annotation : annotation option
    }

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "Transition"

        let json ?(as_elt : bool = false) (x : t) : Yojson.t =
          `Assoc
            [ "from", State.json ~as_elt:true x.from
            ; "goto", State.json ~as_elt:true x.goto
            ; "label", Label.json ~as_elt:true x.label
            ; ( "annotation"
              , Json.option ~as_elt:true Annotation.json x.annotation )
            ; "tree", Json.option ~as_elt:true Base.Tree.json x.tree
            ]
        ;;
      end)

  let equal (a : t) (b : t) : bool =
    State.equal a.from b.from
    && State.equal a.goto b.goto
    && Label.equal a.label b.label
    && Option.equal Annotation.equal a.annotation b.annotation
    && Option.equal Base.Tree.equal a.tree b.tree
  ;;

  let compare (a : t) (b : t) : int =
    Utils.compare_chain
      [ State.compare a.from b.from
      ; State.compare a.goto b.goto
      ; Label.compare a.label b.label
      ; Option.compare Annotation.compare a.annotation b.annotation
      ; Option.compare Base.Tree.compare a.tree b.tree
      ]
  ;;

  let is_silent (x : t) : bool = Label.is_silent x.label
end
