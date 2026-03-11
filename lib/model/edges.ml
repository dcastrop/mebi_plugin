module type S = sig
  type label

  include Set.S

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val labelled : t -> label -> t
end

module Make (Log : Logger.S) (Edge : Edge.S) :
  S with type elt = Edge.t and type label = Edge.label = struct
  type label = Edge.label

  module Set_ : Set.S with type elt = Edge.t = Set.Make (Edge)
  include Set_

  include
    Json.Set.Make
      (Log)
      (struct
        module Set = Set_

        let name = "Edge"
        let json = Edge.json
      end)

  let labelled (xs : t) (y : label) : t =
    Log.trace __FUNCTION__;
    filter (Edge.is_labelled y) xs
  ;;
end
