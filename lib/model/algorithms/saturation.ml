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

  type actionmap

  (** [on_demand old_edges] saturates one state at a time: applied to [s], it
      returns [s]'s weak actions exactly as [edges] would ([None] if it has
      none). Silent closures are shared between calls, in a cache cleared once
      it holds [closure_cap] states (default 4096). *)
  val on_demand : ?closure_cap:int -> edgemap -> state -> actionmap option
end

module Make
    (Base : Base_term.S)
    (C : Components.S with type trees = Base.Trees.t) :
  S
  with type state = C.State.t
   and type states = C.State.Set.t
   and type labels = C.Label.Set.t
   and type edgemap = C.EdgeMap.t'
   and type annotation = C.Annotation.t
   and type actionmap = C.Action.Map.t' = struct
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
  type actionmap = ActionMap.t'
  (* Closure-based saturation. It replaced (2026-09-28) a depth-first path
     enumeration, which cost time exponential in the path count rather than
     the state count -- 101 states took 437 seconds on a grid (see
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

  (** [note_of from a goto]: one step of a witness path, [from -a-> goto],
      recording [a]'s label and its derivation trees. *)
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

  (** [annotation_of_notes notes]: the witness path [notes] (first step
      first) as an annotation, linked step to step; [None] for the empty
      path. *)
  let rec annotation_of_notes : Note.t list -> Annotation.t option = function
    | [] -> None
    | x :: tl -> Some { this = x; next = annotation_of_notes tl }
  ;;

  (** [with_lengths closure]: each [(state, path)] of a silent closure with
      its path's length added. *)
  let with_lengths
    : (State.t * Note.t list) list -> (State.t * Note.t list * int) list
    =
    List.map (fun ((s, path) : State.t * Note.t list) ->
      s, path, List.length path)
  ;;

  (* See the [.mli]. {!silent_closure}'s paths, reversed into first-step-first
     order and turned into annotations. *)
  let silent_paths (old_edges : EdgeMap.t') (src : State.t)
    : (State.t * Annotation.t option * int) list
    =
    List.map
      (fun ((s, path_rev, len) : State.t * Note.t list * int) ->
        s, annotation_of_notes (List.rev path_rev), len)
      (with_lengths (silent_closure old_edges src))
  ;;

  (** A silent-closure lookup over [old_edges], memoised. A state's silent
      closure does not depend on where the weak step started, so each is
      computed once rather than once per visible move into it (backlog item
      3b). With [cap], the memo is emptied whenever it reaches [cap] states,
      bounding its memory for on-demand use. *)
  let closures_of ?(cap : int option) (old_edges : EdgeMap.t')
    : State.t -> (State.t * Note.t list * int) list
    =
    let closures : (State.t * Note.t list * int) list StateTbl.t =
      StateTbl.create 64
    in
    fun (t : State.t) ->
      match StateTbl.find_opt closures t with
      | Some c -> c
      | None ->
        let c = with_lengths (silent_closure old_edges t) in
        (match cap with
         | Some n when StateTbl.length closures >= n -> StateTbl.reset closures
         | _ -> ());
        StateTbl.add closures t c;
        c
  ;;

  (** [visible_sources closure_of from old_edges]: for each visible label
      [a] of a step [from -tau*-> s -a-> t], in the order first met, the
      states [t] so reached, each with its shortest path (most recent step
      first), that path's length, and the order in which it was first
      reached (its arrival index, which breaks ties between equally short
      paths later). [closure_of] gives [from]'s silent closure. *)
  let visible_sources
        (closure_of : State.t -> (State.t * Note.t list * int) list)
        (from : State.t)
        (old_edges : EdgeMap.t')
    : (C.Label.t * (Note.t list * int * int) StateTbl.t) list
    =
    let labels : (C.Label.t * (Note.t list * int * int) StateTbl.t) list ref =
      ref []
    in
    (* [sources_of l]: [l]'s table, added (at the end) if new *)
    let sources_of (l : C.Label.t) : (Note.t list * int * int) StateTbl.t =
      match List.find_opt (fun (l', _) -> C.Label.equal l l') !labels with
      | Some (_, tbl) -> tbl
      | None ->
        let tbl = StateTbl.create 64 in
        labels := !labels @ [ l, tbl ];
        tbl
    in
    List.iter
      (fun ((s, pre_rev, pre_len) : State.t * Note.t list * int) ->
        match EdgeMap.find_opt old_edges s with
        | None -> ()
        | Some actions ->
          ActionMap.fold
            (fun (a : Action.t) (ds : States.t) () ->
              if Bool.not (Action.is_silent a)
              then (
                let tbl = sources_of a.label in
                States.iter
                  (fun (t : State.t) ->
                    let len = pre_len + 1 in
                    match StateTbl.find_opt tbl t with
                    | Some (_, len', _) when len' <= len -> ()
                    | Some (_, _, k) ->
                      StateTbl.replace tbl t (note_of s a t :: pre_rev, len, k)
                    | None ->
                      StateTbl.replace
                        tbl
                        t
                        (note_of s a t :: pre_rev, len, StateTbl.length tbl))
                  ds))
            actions
            ())
      (closure_of from);
    !labels
  ;;

  (** [silent_bfs old_edges sources]: every state reachable by silent steps
      from the [sources] of one label ({!visible_sources}), each with its
      shortest distance and the path that first reached it (most recent step
      first). The search goes distance by distance; within a distance,
      sources in order of length then arrival index, so ties go to the path
      met first. *)
  let silent_bfs
        (old_edges : EdgeMap.t')
        (sources : (Note.t list * int * int) StateTbl.t)
    : (Note.t list * int) StateTbl.t
    =
    let best : (Note.t list * int) StateTbl.t = StateTbl.create 256 in
    (* frontier by distance; within a distance, in arrival order *)
    let buckets : (int, (State.t * Note.t list) Queue.t) Hashtbl.t =
      Hashtbl.create 16
    in
    (* [push d x]: queue [x] at distance [d] *)
    let push (d : int) (x : State.t * Note.t list) : unit =
      match Hashtbl.find_opt buckets d with
      | Some q -> Queue.push x q
      | None ->
        let q = Queue.create () in
        Queue.push x q;
        Hashtbl.replace buckets d q
    in
    StateTbl.fold
      (fun (t : State.t) ((path, len, k) : Note.t list * int * int) acc ->
        (len, k, t, path) :: acc)
      sources
      []
    |> List.sort (fun (l, k, _, _) (l', k', _, _) ->
      match Int.compare l l' with 0 -> Int.compare k k' | n -> n)
    |> List.iter (fun ((len, _, t, path) : int * int * State.t * Note.t list) ->
      StateTbl.replace best t (path, len);
      push len (t, path));
    let d = ref (Hashtbl.fold (fun k _ acc -> min k acc) buckets max_int) in
    while Hashtbl.length buckets > 0 do
      (match Hashtbl.find_opt buckets !d with
       | None -> ()
       | Some q ->
         Hashtbl.remove buckets !d;
         Queue.iter
           (fun ((u, path) : State.t * Note.t list) ->
             (* settled at a shorter distance by now: skip *)
             match StateTbl.find_opt best u with
             | Some (_, du) when du < !d -> ()
             | _ ->
               List.iter
                 (fun ((a, v) : Action.t * State.t) ->
                   match StateTbl.find_opt best v with
                   | Some (_, dv) when dv <= !d + 1 -> ()
                   | _ ->
                     let p = note_of u a v :: path in
                     StateTbl.replace best v (p, !d + 1);
                     push (!d + 1) (v, p))
                 (silent_steps old_edges u))
           q);
      incr d
    done;
    best
  ;;

  (** [emit_weak_actions new_actions label best]: add to [new_actions] one
      weak action under [label] per state of [best] ({!silent_bfs}), with
      that single state as its destination and the state's path as its
      annotation. *)
  let emit_weak_actions
        (new_actions : ActionMap.t')
        (label : C.Label.t)
        (best : (Note.t list * int) StateTbl.t)
    : unit
    =
    StateTbl.fold
      (fun (goto : State.t) ((path, _) : Note.t list * int) acc ->
        match annotation_of_notes (List.rev path) with
        | None -> acc
        | Some ann ->
          ( ({ label; annotation = Some ann; trees = Base.Trees.empty }
             : Action.t)
          , States.singleton goto )
          :: acc)
      best
      []
    |> ActionPairs.of_list
    |> ActionPairs.iter (fun ((x, ds) : Action.t * States.t) ->
      ActionMap.update new_actions x ds)
  ;;

  (** [edge_bfs closure_of new_actions from old_edges] computes [from]'s
      weak actions, in time linear in its output rather than in the number
      of witnesses (backlog item 3b, 2026-10-03; it replaces [edge_closure],
      see git history). For
      each visible label [a]: the states [t] with [from -tau*-> s -a-> t],
      each at its shortest distance (ties: the first met, in the order
      [edge_closure] met them), are the sources of one breadth-first search
      over silent steps, and every state it reaches is a weak [a]-target, at
      its shortest distance, with the path that reached it first as its
      witness. [edge_closure] instead proposed one witness per (s, visible
      step, state reachable from t) -- 2.3M-4.7M per state on [Proc/Test4],
      for 5,280-7,680 weak actions: 2.9s a state, now 35ms.

      Same targets and lengths as [edge_closure]; among witnesses of equal
      length it may keep a different one (2 of the 2426 in [satdiff]; no
      proof count changed).

      In three steps: {!visible_sources}, then per label {!silent_bfs} and
      {!emit_weak_actions}. *)
  let edge_bfs
        (closure_of : State.t -> (State.t * Note.t list * int) list)
        (new_actions : ActionMap.t')
        (from : State.t)
        (old_edges : EdgeMap.t')
    : unit
    =
    Logger.trace __FUNCTION__;
    List.iter
      (fun ((label, sources) : C.Label.t * (Note.t list * int * int) StateTbl.t) ->
        emit_weak_actions new_actions label (silent_bfs old_edges sources))
      (visible_sources closure_of from old_edges)
  ;;

  (** [from]'s weak actions, or [None] if it has none. The one place a
      state is saturated: [edges] and [on_demand] both use it, so they agree
      by construction. *)
  let state_actions
        (closure_of : State.t -> (State.t * Note.t list * int) list)
        (old_edges : EdgeMap.t')
        (from : State.t)
    : ActionMap.t' option
    =
    let new_actions : ActionMap.t' = ActionMap.create 0 in
    edge_bfs closure_of new_actions from old_edges;
    if ActionMap.length new_actions > 0 then Some new_actions else None
  ;;

  (* See the [.mli]. Every state with outgoing edges is saturated by
     {!state_actions}, sharing one closure memo (uncapped: the whole FSM is
     saturated anyway); a state left with no weak action is a terminal. *)
  let edges (labels : Labels.t) (states : States.t) (old_edges : EdgeMap.t')
    : EdgeMap.t' * States.t
    =
    Logger.trace __FUNCTION__;
    let new_edges : EdgeMap.t' = EdgeMap.create 0 in
    let closure_of = closures_of old_edges in
    let terminals : States.t =
      EdgeMap.fold
        (fun (from : State.t) (_old_actions : ActionMap.t') (acc : States.t) ->
          (* [state_actions] reads [from]'s actions out of [old_edges] itself,
             along with those of every state in its silent closure, so the
             fold's own [_old_actions] is redundant here. *)
          match state_actions closure_of old_edges from with
          | Some new_actions ->
            EdgeMap.replace new_edges from new_actions;
            acc
          | None -> States.add from acc)
        old_edges
        States.empty
    in
    new_edges, terminals
  ;;

  (* See the [.mli]. {!state_actions} with a closure memo capped at
     [closure_cap] states. *)
  let on_demand ?(closure_cap : int = 4096) (old_edges : EdgeMap.t')
    : State.t -> ActionMap.t' option
    =
    state_actions (closures_of ~cap:closure_cap old_edges) old_edges
  ;;
end
