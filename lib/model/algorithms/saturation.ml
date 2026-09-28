(** {i See {!Model.S.Saturation}.} *)
module type S = sig
  type state
  type states
  type labels
  type edgemap

  (** [edges labels states old_edges] returns a saturated [edgemap], paired
      with the states that now have no outgoing actions. *)
  val edges : labels -> states -> edgemap -> edgemap * states
end

module Make
    (Base : Base_term.S)
    (C : Components.S with type trees = Base.Trees.t) :
  S
  with type state = C.State.t
   and type states = C.State.Set.t
   and type labels = C.Label.Set.t
   and type edgemap = C.EdgeMap.t' = struct
  module State = C.State
  module States = C.State.Set
  module Labels = C.Label.Set
  module Annotation = C.Annotation
  module Action = C.Action
  module ActionPair = C.Action.Pair
  module ActionPairs = C.Action.Pair.Set
  module ActionMap = C.Action.Map
  module EdgeMap = C.EdgeMap
  module Note = C.Note

  type state = State.t
  type states = States.t
  type labels = Labels.t
  type edgemap = EdgeMap.t'
  (* Closure-based saturation. Replaces the depth-first path enumeration
     above, which cost time exponential in the path count rather than the
     state count -- 101 states took 437 seconds on a grid (see
     [test/satscale.ml]). The specification it computes is unchanged, and is
     derived in [notes/5-saturation-rewrite.md]:

     for each [from], each visible label [a], and each [goto] such that
     [from -tau*-> s -a-> t -tau*-> goto], emit one weak action labelled
     [a] with destination [{goto}], annotated with the SHORTEST witness.

     That "shortest" is not a new choice: [ActionPair.try_update] merges
     [wk_equal] actions with equal destination sets by keeping
     [Annotation.shorter], so of the exponentially many paths the old
     traversal explored, only the shortest per destination ever survived. A
     breadth-first closure produces exactly those survivors directly. *)

  (** Silent steps leaving [s], each silent action paired with one of its
      destinations. *)
  let silent_steps (old_edges : EdgeMap.t') (s : State.t)
    : (Action.t * State.t) list
    =
    match EdgeMap.find_opt old_edges s with
    | None -> []
    | Some actions ->
      ActionMap.fold
        (fun (a : Action.t) (ds : States.t) (acc : (Action.t * State.t) list) ->
          if Action.is_silent a
          then States.fold (fun (d : State.t) acc -> (a, d) :: acc) ds acc
          else acc)
        actions
        []
  ;;

  let note_of (from : State.t) (a : Action.t) (goto : State.t) : Note.t =
    { from; label = a.label; using = a.trees; goto }
  ;;

  (** Reflexive-transitive silent closure of [src], breadth-first, pairing
      each reachable state with a shortest silent path to it. Paths are
      accumulated most-recent-first and reversed at the point of use. *)
  let silent_closure (old_edges : EdgeMap.t') (src : State.t)
    : (State.t * Note.t list) list
    =
    let rec bfs
              (frontier : (State.t * Note.t list) list)
              (seen : States.t)
              (acc : (State.t * Note.t list) list)
      : (State.t * Note.t list) list
      =
      match frontier with
      | [] -> acc
      | _ ->
        let next, seen =
          List.fold_left
            (fun ((next, seen) : (State.t * Note.t list) list * States.t)
              ((s, path) : State.t * Note.t list) ->
              List.fold_left
                (fun ((next, seen) : (State.t * Note.t list) list * States.t)
                  ((a, d) : Action.t * State.t) ->
                  if States.mem d seen
                  then next, seen
                  else (d, note_of s a d :: path) :: next, States.add d seen)
                (next, seen)
                (silent_steps old_edges s))
            ([], seen)
            frontier
        in
        bfs next seen (List.rev_append next acc)
    in
    let start : (State.t * Note.t list) list = [ src, [] ] in
    bfs start (States.singleton src) start
  ;;

  let rec annotation_of_notes : Note.t list -> Annotation.t option = function
    | [] -> None
    | x :: tl -> Some { this = x; next = annotation_of_notes tl }
  ;;

  (** The closure-based counterpart of [edge]. *)
  let edge_closure
        (new_actions : ActionMap.t')
        (from : State.t)
        (old_edges : EdgeMap.t')
    : unit
    =
    Logger.trace __FUNCTION__;
    let pairs : ActionPair.t list ref = ref [] in
    List.iter
      (fun ((s, pre_rev) : State.t * Note.t list) ->
        match EdgeMap.find_opt old_edges s with
        | None -> ()
        | Some actions ->
          ActionMap.fold
            (fun (a : Action.t) (ds : States.t) () ->
              if Action.is_silent a
              then ()
              else
                States.iter
                  (fun (t : State.t) ->
                    let mid : Note.t = note_of s a t in
                    List.iter
                      (fun ((goto, post_rev) : State.t * Note.t list) ->
                        let notes : Note.t list =
                          List.rev pre_rev @ (mid :: List.rev post_rev)
                        in
                        match annotation_of_notes notes with
                        | None -> ()
                        | Some ann ->
                          let act : Action.t =
                            { label = a.label
                            ; annotation = Some ann
                            ; trees = Base.Trees.empty
                            }
                          in
                          pairs := (act, States.singleton goto) :: !pairs)
                      (silent_closure old_edges t))
                  ds)
            actions
            ())
      (silent_closure old_edges from);
    ActionPair.merge_lists [] !pairs
    |> ActionPairs.of_list
    |> ActionPairs.iter
         (fun ((saturated_action, destinations) : Action.t * States.t) ->
         ActionMap.update new_actions saturated_action destinations)
  ;;

  (****************************************************************************)

  (** [] returns a saturated [EdgeMap.t'] paired with a set of terminals states {i (i.e., states that now have no outgoing actions, and if reached)}.*)
  let edges (labels : Labels.t) (states : States.t) (old_edges : EdgeMap.t')
    : EdgeMap.t' * States.t
    =
    Logger.trace __FUNCTION__;
    let new_edges : EdgeMap.t' = EdgeMap.create 0 in
    let terminals : States.t =
      EdgeMap.fold
        (fun (from : State.t) (_old_actions : ActionMap.t') (acc : States.t) ->
          (* [edge_closure] reads [from]'s actions out of [old_edges] itself,
             along with those of every state in its silent closure, so the
             fold's own [_old_actions] is redundant here. *)
          let new_actions : ActionMap.t' = ActionMap.create 0 in
          let () = edge_closure new_actions from old_edges in
          if ActionMap.length new_actions > 0
          then (
            EdgeMap.replace new_edges from new_actions;
            acc)
          else States.add from acc)
        old_edges
        States.empty
    in
    new_edges, terminals
  ;;
end
