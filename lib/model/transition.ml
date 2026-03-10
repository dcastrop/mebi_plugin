module type S = sig
  type t

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val is_silent : t -> bool
end

type ('a, 'b, 'c, 'd) t' =
  { from : 'a
  ; goto : 'a
  ; label : 'b
  ; tree : 'c option
  ; annotation : 'd option
  }

module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type t = Base.t)
    (Label : Label.S with type t = Base.t Label.t')
    (Note : sig
       type t =
         { from : State.t
         ; label : Label.t
         ; using : Base.Trees.t
         ; goto : State.t
         }
     end)
    (Annotation : sig
       type t =
         { this : Note.t
         ; next : t option
         }

       val json : ?as_elt:bool -> t -> Yojson.t
       val equal : t -> t -> bool
       val compare : t -> t -> int
     end) : S with type t = (State.t, Label.t, Base.Tree.t, Annotation.t) t' =
struct
  type t = (State.t, Label.t, Base.Tree.t, Annotation.t) t'

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
