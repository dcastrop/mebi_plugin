module Make
    (Log : Logger.S)
    (Enc : Encoding.S)
    (Tree : sig
              module Node : sig
                  type t = Enc.t * int

                  val compare : t -> t -> int
                  val equal : t -> t -> bool
                  val json : ?as_elt:bool -> t -> Yojson.t
                  val to_string : ?pretty:bool -> t -> string
                  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
                end
                with type t = Enc.t * int

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
            end
            with type Node.t = Enc.t * int) : sig
    type t = Enc.t * Enc.t * Tree.t

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end
  with type t = Enc.t * Enc.t * Tree.t = struct
  type t = Enc.t * Enc.t * Tree.t

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "Constructor"

        let json ?as_elt ((action, goto, tree) : t) : Yojson.t =
          `Assoc
            [ "action", `String (Enc.to_string action)
            ; "goto", `String (Enc.to_string goto)
            ; "tree", Tree.json tree
            ]
        ;;
      end)
end
