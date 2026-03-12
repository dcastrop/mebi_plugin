module type S = sig
  type states

  include Set.S

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
  val destinations : t -> states

  exception IsEmpty

  val shortest_annotation : t -> elt
  val merge_list : t -> elt list -> t
end

module Make
    (Log : Logger.S)
    (States : States.S)
    (Action : Action.S)
    (ActionPair :
       Actionpair.S with type action = Action.t and type states = States.t) :
  S
  with type states = States.t
   and type states = ActionPair.states
   and type elt = ActionPair.t
   and type elt = Action.t * States.t = struct
  type states = States.t

  module Set_ : Set.S with type elt = ActionPair.t = Set.Make (ActionPair)
  include Set_

  include
    Json.Set.Make
      (Log)
      (struct
        module Set = Set_

        let name = "ActionPairs"
        let json = ActionPair.json
      end)

  let destinations (x : t) : States.t =
    to_list x
    |> List.fold_left
         (fun (acc : States.t) ((a, b) : ActionPair.t) -> States.union acc b)
         States.empty
  ;;

  exception IsEmpty

  (** returns the action in [x] that has the {e shortest} annotation (where [None] is treated as 0).
  *)
  let shortest_annotation (x : t) : ActionPair.t =
    match to_list x with
    | [] -> raise IsEmpty
    | h :: tl -> List.fold_left ActionPair.shorter_annotation h tl
  ;;

  let merge_list : t -> ActionPair.t list -> t =
    List.fold_left (fun (acc : t) ((a, s) : ActionPair.t) ->
      (* NOTE: merge destinations of equal actions *)
      let matching =
        filter (fun ((b, t) : ActionPair.t) -> Action.equal a b) acc
      in
      if is_empty matching
      then add (a, s) acc
      else (
        (* NOTE: update states of each matching *)
        let acc = diff acc matching in
        matching
        |> to_list
        |> List.map (fun (_, t) -> a, States.union s t)
        |> of_list
        |> union acc))
  ;;
end
