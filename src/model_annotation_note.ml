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
    (Tree : sig
       module Node : sig
         type t

         val compare : t -> t -> int
         val equal : t -> t -> bool
         val json : ?as_elt:bool -> t -> Yojson.t
         val to_string : ?pretty:bool -> t -> string
         val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
       end

       type 'a tree = N of 'a * 'a tree list
       type t = Node.t tree

       val add : t -> t -> t
       val add_list : t -> t list -> t list
       val equal : t -> t -> bool
       val compare : t -> t -> int
       val minimize : t -> Node.t list

       exception CannotMinimizeEmptyList of unit

       val min : t list -> Node.t list
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (Trees : sig
       include Set.S with type elt = Tree.t

       exception EmptyHasNoMin

       val min : t -> Tree.t
       val min_opt : t -> Tree.t option
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end) : sig
  type t =
    { from : State.t
    ; label : Label.t
    ; using : Trees.t
    ; goto : State.t
    }

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val is_silent : t -> bool
  val has_label : Label.t -> t -> bool
  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
end = struct
  type t =
    { from : State.t
    ; label : Label.t
    ; using : Trees.t
    ; goto : State.t
    }

  let equal (a : t) (b : t) : bool =
    State.equal a.from b.from
    && State.equal a.goto b.goto
    && Label.equal a.label b.label
    && Trees.equal a.using b.using
  ;;

  let compare (a : t) (b : t) : int =
    Utils.compare_chain
      [ State.compare a.from b.from
      ; State.compare a.goto b.goto
      ; Label.compare a.label b.label
      ; Trees.compare a.using b.using
      ]
  ;;

  let is_silent (x : t) : bool = Label.is_silent x.label
  let has_label (x : Label.t) (y : t) : bool = Label.equal x y.label

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
            ; "using", Trees.json ~as_elt:true x.using
            ]
        ;;
      end)
end
