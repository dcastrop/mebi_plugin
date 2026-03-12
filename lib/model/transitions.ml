module type S = sig
  type labels

  include Set.S

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
  val labels : t -> labels
end

module Make
    (Log : Logger.S)
    (Labels : Labels.S)
    (Transition : Transition.S with type label = Labels.elt) :
  S with type elt = Transition.t and type labels = Labels.t = struct
  type labels = Labels.t

  module Set_ : Set.S with type elt = Transition.t = Set.Make (Transition)
  include Set_

  include
    Json.Set.Make
      (Log)
      (struct
        module Set = Set_

        let name = "Transitions"
        let json = Transition.json
      end)

  let labels (xs : t) : Labels.t =
    Log.trace __FUNCTION__;
    fold
      (fun ({ label; _ } : Transition.t) : (Labels.t -> Labels.t) ->
        Labels.add label)
      xs
      Labels.empty
  ;;
end
