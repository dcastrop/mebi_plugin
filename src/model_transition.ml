module Make
    (Log : Logger.S)
    (State : sig
       type t

       val json : ?as_elt:bool -> t -> Yojson.t

       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       val equal : t -> t -> bool
       val compare : t -> t -> int
       (* val hash : t -> int *)
     end)
    (Label : sig
       type t

       val json : ?as_elt:bool -> t -> Yojson.t
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)

       val equal : t -> t -> bool
       val compare : t -> t -> int

       (* val hash : t -> int *)
       val is_silent : t -> bool
     end)
    (Tree : sig
       module Node : sig
         type t
         (* val json : ?as_elt:bool -> t -> Yojson.t *)
         (* val to_string : ?pretty:bool -> t -> string *)
         (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)

         (* val compare : t -> t -> int *)
         (* val equal : t -> t -> bool *)
       end

       type 'a tree = N of 'a * 'a tree list
       type t = Node.t tree

       val json : ?as_elt:bool -> t -> Yojson.t

       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)

       (* val add : t -> t -> t *)
       (* val add_list : t -> t list -> t list *)
       val equal : t -> t -> bool
       val compare : t -> t -> int
       (* val minimize : t -> Node.t list *)

       exception CannotMinimizeEmptyList of unit

       (* val min : t list -> Node.t list *)
     end)
    (Note : sig
       type t
       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)

       (* val equal : t -> t -> bool *)
       (* val compare : t -> t -> int *)
       (* val is_silent : t -> bool *)
       (* val has_label : Label.t -> t -> bool *)
     end)
    (Annotation : sig
       type t =
         { this : Note.t
         ; next : t option
         }

       val json : ?as_elt:bool -> t -> Yojson.t
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)

       val equal : t -> t -> bool
       val compare : t -> t -> int

       (* val is_empty : t -> bool *)
       (* val opt_is_empty : ?fail_if_none:bool -> t option -> bool *)
       (* val length : t -> int *)
       (* val opt_length : ?fail_if_none:bool -> t option -> int *)
       (* val shorter : t -> t -> t *)
       (* val exists : Note.t -> t -> bool *)
       (* val exists_label : Label.t -> t -> bool *)
       (* val append : Note.t -> t -> t *)
       (* val last : t -> Note.t *)

       exception CannotDropLastOfSingleton of t

       (* val drop_last : t -> t *)
     end) : sig
  type t =
    { from : State.t
    ; goto : State.t
    ; label : Label.t
    ; tree : Tree.t option
    ; annotation : Annotation.t option
    }

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val is_silent : t -> bool
end = struct
  type t =
    { from : State.t
    ; goto : State.t
    ; label : Label.t
    ; tree : Tree.t option
    ; annotation : Annotation.t option
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
            ; "tree", Json.option ~as_elt:true Tree.json x.tree
            ]
        ;;
      end)

  let equal (a : t) (b : t) : bool =
    State.equal a.from b.from
    && State.equal a.goto b.goto
    && Label.equal a.label b.label
    && Option.equal Annotation.equal a.annotation b.annotation
    && Option.equal Tree.equal a.tree b.tree
  ;;

  let compare (a : t) (b : t) : int =
    Utils.compare_chain
      [ State.compare a.from b.from
      ; State.compare a.goto b.goto
      ; Label.compare a.label b.label
      ; Option.compare Annotation.compare a.annotation b.annotation
      ; Option.compare Tree.compare a.tree b.tree
      ]
  ;;

  let is_silent (x : t) : bool = Label.is_silent x.label
end
