module Make (Log : Logger.S) (Enc : Encoding.SEncoding) = struct
  module Tree_ = Enc_tree.Make (Log) (Enc)
  module Tree = Tree_.Tree

  module type S = sig
    type t = Enc.t * Enc.t * Tree.t

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module Constructor : S = struct
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

  module Constructors (Constructor : S) = struct
    type t = Constructor.t list

    (* *)
    include
      Json.List.Make
        (Log)
        (struct
          include Constructor

          let name = "Constructors"
        end)
  end
end
