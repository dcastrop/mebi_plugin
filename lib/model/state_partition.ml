module type S = sig
  type state
  type label
  type edgemap

  include Set.S

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val get_bisimilar : state -> t -> elt
  val filter_reachable : elt -> t -> t
  val reachable : state -> edgemap -> t -> t
  val reachable_by_label : state -> label -> edgemap -> t -> t
end

module Make
    (Log : Logger.S)
    (State : State.S)
    (States : States.S with type elt = State.t)
    (ActionMap : Actionmap.S with type states = States.t)
    (EdgeMap :
       Edgemap.S
       with type state = State.t
        and type states = States.t
        and type label = ActionMap.label
        and type action = ActionMap.action
        and type actions = ActionMap.actions
        and type actionmap = ActionMap.t') :
  S
  with type elt = States.t
   and type state = State.t
   and type label = ActionMap.label
   and type label = EdgeMap.label
   and type edgemap = EdgeMap.t' = struct
  type state = State.t
  type label = ActionMap.label
  type edgemap = EdgeMap.t'

  module Set_ : Set.S with type elt = States.t = Set.Make (States)
  include Set_

  include
    Json.Set.Make
      (Log)
      (struct
        module Set = Set_

        let name = "Partitions"
        let json = States.json
      end)

  let get_bisimilar (x : State.t) : t -> States.t =
    find_first (fun (ys : States.t) -> States.mem x ys)
  ;;

  let filter_reachable (xs : States.t) : t -> t =
    filter (fun (y : States.t) ->
      Bool.not (States.is_empty (States.inter y xs)))
  ;;

  let reachable (from : State.t) (edges : EdgeMap.t') : t -> t =
    Log.trace __FUNCTION__;
    filter_reachable (EdgeMap.destinations edges from)
  ;;

  let reachable_by_label (from : State.t) (label : label) (edges : EdgeMap.t')
    : t -> t
    =
    Log.trace __FUNCTION__;
    let actions = ActionMap.reduce_by_label (EdgeMap.find edges from) label in
    filter_reachable (ActionMap.destinations actions)
  ;;
end
