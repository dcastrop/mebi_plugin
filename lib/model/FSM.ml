module type S = sig
  type state
  type states
  type labels
  type edgemap
  type info
  type lts

  type t =
    { init : state option
    ; alphabet : labels
    ; states : states
    ; edges : edgemap
    ; terminals : states
    ; info : info
    }

  include Json.S with type k = t

  val of_lts : lts -> t
  val merge : t -> t -> t
  val is_weak_mode : t -> bool
  val saturate : ?only_if_weak:bool -> t -> t
end

module Make
    (Log : Logger.S)
    (State : State.S)
    (States : States.S with type elt = State.t)
    (Labels : Labels.S)
    (EdgeMap : Edgemap.S with type state = State.t and type label = Labels.elt)
    (Info : Info.S with type base = State.base and type labels = Labels.t)
    (LTS :
       LTS.S
       with type state = State.t
        and type states = States.t
        and type labels = Labels.t
        and type transitions = EdgeMap.transitions
        and type info = Info.t)
    (Saturation :
       Saturation.S
       with type state = State.t
        and type states = States.t
        and type labels = Labels.t
        and type edgemap = EdgeMap.t') :
  S
  with type state = State.t
   and type states = States.t
   and type labels = Labels.t
   and type edgemap = EdgeMap.t'
   and type info = Info.t
   and type lts = LTS.t = struct
  type state = State.t
  type states = States.t
  type labels = Labels.t
  type edgemap = EdgeMap.t'
  type info = Info.t
  type lts = LTS.t

  type t =
    { init : state option
    ; alphabet : labels
    ; states : states
    ; edges : edgemap
    ; terminals : states
    ; info : info
    }

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "FSM"

        let json ?as_elt (x : t) : Yojson.t =
          `Assoc
            [ "init", Json.option ~as_elt:true State.json x.init
            ; "info", Info.json ~as_elt:true x.info
            ; "terminals", States.json ~as_elt:true x.terminals
            ; "alphabet", Labels.json ~as_elt:true x.alphabet
            ; "states", States.json ~as_elt:true x.states
            ; "edges", EdgeMap.json ~as_elt:true x.edges
            ]
        ;;
      end)

  let of_lts (x : LTS.t) : t =
    Log.trace __FUNCTION__;
    { init = x.init
    ; terminals = x.terminals
    ; alphabet = x.alphabet
    ; states = x.states
    ; edges = EdgeMap.of_transitions x.transitions
    ; info = x.info
    }
  ;;

  let merge (a : t) (b : t) : t =
    let init : State.t option = None in
    let terminals : States.t = States.union a.terminals b.terminals in
    let alphabet : Labels.t = Labels.union a.alphabet b.alphabet in
    let states : States.t = States.union a.states b.states in
    let edges : EdgeMap.t' = EdgeMap.merge a.edges b.edges in
    let nums : Info.nums =
      { states = States.cardinal states
      ; labels = Labels.cardinal alphabet
      ; edges = EdgeMap.size edges
      }
    in
    let info : Info.t = Info.merge ~nums:(Some nums) a.info b.info in
    { init; terminals; alphabet; states; edges; info }
  ;;

  let is_weak_mode (x : t) : bool =
    Bool.not (Labels.is_empty x.info.weak_labels)
  ;;

  let saturate ?(only_if_weak : bool = true) (x : t) : t =
    Log.trace __FUNCTION__;
    if only_if_weak && Bool.not (is_weak_mode x)
    then (
      Log.debug ~__FUNCTION__ "Not weak, returning unchanged";
      x)
    else (
      let edges, terminals' =
        Saturation.edges x.alphabet x.states (EdgeMap.copy x.edges)
      in
      { x with edges; terminals = States.union x.terminals terminals' })
  ;;
end
