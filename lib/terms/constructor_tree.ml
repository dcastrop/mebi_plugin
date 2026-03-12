module Make
    (Log : Logger.S)
    (Base : sig
       type t

       val json : ?as_elt:bool -> t -> Yojson.t
       val equal : t -> t -> bool
       val compare : t -> t -> int
     end)
    (Tree : sig
              module Node : sig
                  type t = Base.t * int

                  (* val json : ?as_elt:bool -> t -> Yojson.t *)
                  (* val to_string : ?pretty:bool -> t -> string *)
                  (* val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit *)
                  (* val compare : t -> t -> int *)
                  (* val equal : t -> t -> bool *)
                end
                with type t = Base.t * int

              type 'a tree = N of 'a * 'a tree list
              type t = Node.t tree

              val json : ?as_elt:bool -> t -> Yojson.t

              (* val to_string : ?pretty:bool -> t -> string *)
              (* val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit *)

              (* val add : t -> t -> t *)
              (* val add_list : t -> t list -> t list *)
              val equal : t -> t -> bool
              val compare : t -> t -> int
              (* val minimize : t -> Node.t list *)

              exception CannotMinimizeEmptyList of unit

              (* val min : t list -> Node.t list *)
            end
            with type Node.t = Base.t * int) : sig
    type t = Base.t * Base.t * Tree.t

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
    val equal : t -> t -> bool
    val compare : t -> t -> int
  end
  with type t = Base.t * Base.t * Tree.t = struct
  type t = Base.t * Base.t * Tree.t

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "Constructor"

        let json ?as_elt ((action, goto, tree) : t) : Yojson.t =
          `Assoc
            [ "action", Base.json ~as_elt:true action
            ; "goto", Base.json ~as_elt:true goto
            ; "tree", Tree.json ~as_elt:true tree
            ]
        ;;
      end)

  let equal ((a, b, c) : t) ((x, y, z) : t) : bool =
    Base.equal a x && Base.equal b y && Tree.equal c z
  ;;

  let compare ((a, b, c) : t) ((x, y, z) : t) : int =
    Utils.compare_chain [ Base.compare a x; Base.compare b y; Tree.compare c z ]
  ;;
end
