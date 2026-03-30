module type S = sig
  type base
  type tree
  type t = base * base * tree

  include Json.S with type k = t

  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Make
    (Log : Logger.S)
    (Base : Base_.S)
    (Tree : Tree.S with type base = Base.t) :
  S with type base = Base.t and type tree = Tree.t = struct
  type base = Base.t
  type tree = Tree.t
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
