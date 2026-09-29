module type S = sig
  type state
  type states
  type label
  type transition
  type fsm

  exception
    NoBisimilarResponse of
      { from : state
      ; label : label
      }

  val respond : fsm -> state -> label -> states -> transition
end

module Make
    (Base : Base_term.S)
    (C : Components.S with type tree = Base.Tree.t and type trees = Base.Trees.t)
    (FSM :
       FSM.S
       with type state = C.State.t
        and type states = C.State.Set.t
        and type labels = C.Label.Set.t
        and type edgemap = C.EdgeMap.t'
        and type info = C.Info.t) =
struct
  type state = C.State.t
  type states = C.State.Set.t
  type label = C.Label.t
  type transition = C.Transition.t
  type fsm = FSM.t

  exception
    NoBisimilarResponse of
      { from : state
      ; label : label
      }

  (* Lifted verbatim out of [Proof_solver_step.try_get_visible_transition].
     The two lines that used to precede it there resolved [from] and [label]
     out of the Rocq goal; everything below is, and always was, pure model
     code. Keep it that way -- the caller resolves, this decides. *)
  let respond
        (m : FSM.t)
        (from : C.State.t)
        (label : C.Label.t)
        (bisimilar : C.State.Set.t)
    : C.Transition.t
    =
    Logger.trace __FUNCTION__;
    try
      let ({ annotation; trees; _ }, destinations) : C.Action.Pair.t =
        (* NOTE: get actions [from] with [label] *)
        C.Action.Map.reduce_by_label (C.EdgeMap.find m.edges from) label
        |> C.Action.Map.to_actionpairs
        (* NOTE: keep only those that are [bisimilar] *)
        |> C.Action.Pair.Set.filter_map (fun ((x, y) : C.Action.Pair.t) ->
          if C.State.Set.disjoint bisimilar y
          then None
          else Some (x, C.State.Set.inter bisimilar y))
        (* NOTE: get the pair with the shortest annotation (less steps to do) *)
        |> C.Action.Pair.Set.shortest_annotation
      in
      let tree : Base.Tree.t option = Base.Trees.min_opt trees in
      let goto : C.State.t = C.State.Set.min_elt destinations in
      { from; goto; label; annotation; tree }
    with
    | C.Action.Pair.Set.IsEmpty -> raise (NoBisimilarResponse { from; label })
  ;;
end
