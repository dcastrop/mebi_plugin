(** {i See {!Model.S.Saturation}.} *)
module type S = sig
  type state
  type states
  type labels
  type edgemap
  type annotation

  (** [edges labels states old_edges] returns a saturated [edgemap], paired
      with the states that now have no outgoing actions. *)
  val edges : labels -> states -> edgemap -> edgemap * states

  (** [silent_paths edges s] is every state [s] reaches by zero or more
      silent steps of [edges], each with the length and the annotation of a
      shortest such path ([None] for [s] itself). *)
  val silent_paths : edgemap -> state -> (state * annotation option * int) list
end

module Make
    (Base : Base_term.S)
    (C : Components.S with type trees = Base.Trees.t) :
  S
  with type state = C.State.t
   and type states = C.State.Set.t
   and type labels = C.Label.Set.t
   and type edgemap = C.EdgeMap.t'
   and type annotation = C.Annotation.t = struct
  module State = C.State
  module States = C.State.Set
  module Labels = C.Label.Set
  module Annotation = C.Annotation
  module Action = C.Action
  module ActionPairs = C.Action.Pair.Set
  module ActionMap = C.Action.Map
  module EdgeMap = C.EdgeMap
  module Note = C.Note
  module StateTbl = Hashtbl.Make (C.State)

  type state = State.t
  type states = States.t
  type labels = Labels.t
  type annotation = Annotation.t
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

  let with_lengths
    : (State.t * Note.t list) list -> (State.t * Note.t list * int) list
    =
    List.map (fun ((s, path) : State.t * Note.t list) ->
      s, path, List.length path)
  ;;

  let silent_paths (old_edges : EdgeMap.t') (src : State.t)
    : (State.t * Annotation.t option * int) list
    =
    List.map
      (fun ((s, path_rev, len) : State.t * Note.t list * int) ->
        s, annotation_of_notes (List.rev path_rev), len)
      (with_lengths (silent_closure old_edges src))
  ;;

  (** Weak actions from one state, keyed by [(label, goto)]: the
      deduplication [ActionPair.merge_lists] used to do by scanning a list,
      which cost (witnesses) x (distinct weak actions) per state -- ~10x per
      doubling on [Proc/Test4]'s shape, see [test/satscale.ml] and backlog
      item 3b. *)
  module Key = Hashtbl.Make (struct
      type t = C.Label.t * State.t

      let equal ((l, s) : t) ((l', s') : t) : bool =
        C.Label.equal l l' && State.equal s s'
      ;;

      let hash ((l, s) : t) : int = Hashtbl.hash (C.Label.hash l, State.hash s)
    end)

  (** The closure-based counterpart of [edge].

      Survivors are exactly those [merge_lists] picked. It was fed the
      witnesses newest-first and kept [Annotation.shorter existing incoming],
      which returns [incoming] on a tie: so the shortest witness wins, ties go
      to the {e earliest} generated, and the label to the {e latest}
      ([Label.equal] ignores [is_silent], so which one is not quite moot).
      Checked against [test/satdiff.expected]. *)
  let edge_closure
        (closure_of : State.t -> (State.t * Note.t list * int) list)
        (new_actions : ActionMap.t')
        (from : State.t)
        (old_edges : EdgeMap.t')
    : unit
    =
    Logger.trace __FUNCTION__;
    let found : (C.Label.t * int * Annotation.t) Key.t = Key.create 16 in
    List.iter
      (fun ((s, pre_rev, pre_len) : State.t * Note.t list * int) ->
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
                      (fun ((goto, post_rev, post_len) :
                             State.t * Note.t list * int) ->
                        (* Lengths first: the witness is only built if it
                           replaces the one held, and on this shape almost
                           none do. *)
                        let len : int = pre_len + 1 + post_len in
                        let key : Key.key = a.label, goto in
                        match Key.find_opt found key with
                        | Some (_, len', ann') when len' <= len ->
                          Key.replace found key (a.label, len', ann')
                        | _ ->
                          (match
                             annotation_of_notes
                               (List.rev_append
                                  pre_rev
                                  (mid :: List.rev post_rev))
                           with
                           | None -> ()
                           | Some ann ->
                             Key.replace found key (a.label, len, ann)))
                      (closure_of t))
                  ds)
            actions
            ())
      (with_lengths (silent_closure old_edges from));
    Key.fold
      (fun ((_, goto) : Key.key)
        ((label, _, ann) : C.Label.t * int * Annotation.t)
        (acc : (Action.t * States.t) list) ->
        ( { label; annotation = Some ann; trees = Base.Trees.empty }
        , States.singleton goto )
        :: acc)
      found
      []
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
    (* A state's silent closure does not depend on where the weak step
       started, so each is computed once per saturation rather than once per
       visible move into it (backlog item 3b). *)
    let closures : (State.t * Note.t list * int) list StateTbl.t =
      StateTbl.create 64
    in
    let closure_of (t : State.t) : (State.t * Note.t list * int) list =
      match StateTbl.find_opt closures t with
      | Some c -> c
      | None ->
        let c = with_lengths (silent_closure old_edges t) in
        StateTbl.add closures t c;
        c
    in
    let terminals : States.t =
      EdgeMap.fold
        (fun (from : State.t) (_old_actions : ActionMap.t') (acc : States.t) ->
          (* [edge_closure] reads [from]'s actions out of [old_edges] itself,
             along with those of every state in its silent closure, so the
             fold's own [_old_actions] is redundant here. *)
          let new_actions : ActionMap.t' = ActionMap.create 0 in
          let () = edge_closure closure_of new_actions from old_edges in
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
