module type S = sig
  type label
  type labels

  include Set.S
  include Json.S with type k = t

  val labelled : t -> label -> t
  val labels : t -> labels
end

module Make
    (Log : Logger.S)
    (Label : Label.S)
    (Labels : Labels.S with type elt = Label.t)
    (Action : Action.S with type label = Label.t) :
  S with type elt = Action.t and type label = Label.t and type labels = Labels.t =
struct
  type label = Label.t
  type labels = Labels.t

  module Set_ : Set.S with type elt = Action.t = Set.Make (Action)
  include Set_

  include
    Json.Set.Make
      (Log)
      (struct
        module Set = Set_

        let name = "Actions"
        let json = Action.json
      end)

  let labelled (xs : t) (y : label) : t =
    Log.trace __FUNCTION__;
    filter (fun ({ label; _ } : Action.t) -> Label.equal label y) xs
  ;;

  let labels (xs : t) : Labels.t =
    Log.trace __FUNCTION__;
    fold
      (fun ({ label; _ } : Action.t) : (Labels.t -> Labels.t) ->
        Labels.add label)
      xs
      Labels.empty
  ;;
end
