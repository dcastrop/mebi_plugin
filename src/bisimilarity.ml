(* modules in this file follow the sections in Chapter 3 of
   [`Advanced Topics in Bisimulation and Coinduction`].
   (See https://doi.org/10.1017/CBO9780511792588.) *)

open Fsm

(** [RCP] contains algorithms for solving the
    `Relational Coarsest Partitioning` problem. *)
module RCP = struct
  (** [Examples] contains examples to be used by either [KS90] or [PT87]. *)
  module Examples = struct
    (** [exa_1] is `Example 3.2.5` on page 106. *)
    let exa_1 : fsm * fsm =
      (* s *)
      let (s : fsm) =
        let init = { id = 0; pp = "s0" } in
        let states =
          States.of_list
            [ { id = 0; pp = "s0" }
            ; { id = 1; pp = "s1" }
            ; { id = 2; pp = "s2" }
            ]
        in
        let alphabet =
          Alphabet.of_list [ { id = 1; label = "a" }; { id = 2; label = "b" } ]
        in
        let edges = Edges.create 4 in
        (* s0 *)
        Edges.add
          edges
          (get_state_by_id states 0)
          (Actions.of_seq
             (List.to_seq
                [ ( get_action_by_label alphabet "a"
                  , States.of_list
                      [ get_state_by_id states 1; get_state_by_id states 2 ] )
                ]));
        (* s1 *)
        Edges.add
          edges
          (get_state_by_id states 1)
          (Actions.of_seq
             (List.to_seq
                [ ( get_action_by_label alphabet "b"
                  , States.of_list [ get_state_by_id states 2 ] )
                ]));
        (* s2 *)
        Edges.add
          edges
          (get_state_by_id states 2)
          (Actions.of_seq
             (List.to_seq
                [ ( get_action_by_label alphabet "b"
                  , States.of_list [ get_state_by_id states 2 ] )
                ]));
        { init; states; alphabet; edges }
      in
      (* t *)
      let (t : fsm) =
        let init = { id = 0; pp = "t0" } in
        let states =
          States.of_list [ { id = 0; pp = "t0" }; { id = 1; pp = "t1" } ]
        in
        let alphabet =
          Alphabet.of_list [ { id = 1; label = "a" }; { id = 2; label = "b" } ]
        in
        let edges = Edges.create 4 in
        (* t0 *)
        Edges.add
          edges
          (get_state_by_id states 0)
          (Actions.of_seq
             (List.to_seq
                [ ( get_action_by_label alphabet "a"
                  , States.of_list [ get_state_by_id states 1 ] )
                ]));
        (* t1 *)
        Edges.add
          edges
          (get_state_by_id states 1)
          (Actions.of_seq
             (List.to_seq
                [ ( get_action_by_label alphabet "b"
                  , States.of_list [ get_state_by_id states 1 ] )
                ]));
        { init; states; alphabet; edges }
      in
      s, t
    ;;

    (** [exa_2] is `Example 3.2.6` on page 107. *)
    let exa_2 : fsm * fsm =
      (* s *)
      let (s : fsm) =
        let init = { id = 0; pp = "s0" } in
        let states =
          States.of_list
            [ { id = 0; pp = "s0" }
            ; { id = 1; pp = "s1" }
            ; { id = 2; pp = "s2" }
            ; { id = 3; pp = "s3" }
            ; { id = 4; pp = "s4" }
            ]
        in
        let alphabet =
          Alphabet.of_list [ { id = 1; label = "a" }; { id = 2; label = "b" } ]
        in
        let edges = Edges.create 4 in
        (* s0 *)
        Edges.add
          edges
          (get_state_by_id states 0)
          (Actions.of_seq
             (List.to_seq
                [ ( get_action_by_label alphabet "a"
                  , States.of_list
                      [ get_state_by_id states 1; get_state_by_id states 2 ] )
                ]));
        (* s1 *)
        Edges.add
          edges
          (get_state_by_id states 1)
          (Actions.of_seq
             (List.to_seq
                [ ( get_action_by_label alphabet "a"
                  , States.of_list [ get_state_by_id states 3 ] )
                ; ( get_action_by_label alphabet "b"
                  , States.of_list [ get_state_by_id states 4 ] )
                ]));
        (* s2 *)
        Edges.add
          edges
          (get_state_by_id states 2)
          (Actions.of_seq
             (List.to_seq
                [ ( get_action_by_label alphabet "a"
                  , States.of_list [ get_state_by_id states 4 ] )
                ]));
        (* s3 *)
        Edges.add
          edges
          (get_state_by_id states 3)
          (Actions.of_seq
             (List.to_seq
                [ ( get_action_by_label alphabet "a"
                  , States.of_list [ get_state_by_id states 0 ] )
                ]));
        (* s4 *)
        Edges.add
          edges
          (get_state_by_id states 4)
          (Actions.of_seq
             (List.to_seq
                [ ( get_action_by_label alphabet "a"
                  , States.of_list [ get_state_by_id states 0 ] )
                ]));
        { init; states; alphabet; edges }
      in
      (* t *)
      let (t : fsm) =
        let init = { id = 0; pp = "t0" } in
        let states =
          States.of_list
            [ { id = 0; pp = "t0" }
            ; { id = 1; pp = "t1" }
            ; { id = 2; pp = "t2" }
            ; { id = 3; pp = "t3" }
            ; { id = 4; pp = "t4" }
            ; { id = 5; pp = "t5" }
            ]
        in
        let alphabet =
          Alphabet.of_list [ { id = 1; label = "a" }; { id = 2; label = "b" } ]
        in
        let edges = Edges.create 4 in
        (* t0 *)
        Edges.add
          edges
          (get_state_by_id states 0)
          (Actions.of_seq
             (List.to_seq
                [ ( get_action_by_label alphabet "a"
                  , States.of_list
                      [ get_state_by_id states 1; get_state_by_id states 3 ] )
                ]));
        (* t1 *)
        Edges.add
          edges
          (get_state_by_id states 1)
          (Actions.of_seq
             (List.to_seq
                [ ( get_action_by_label alphabet "b"
                  , States.of_list [ get_state_by_id states 2 ] )
                ; ( get_action_by_label alphabet "a"
                  , States.of_list [ get_state_by_id states 5 ] )
                ; ( get_action_by_label alphabet "b"
                  , States.of_list [ get_state_by_id states 5 ] )
                ]));
        (* t2 *)
        Edges.add
          edges
          (get_state_by_id states 2)
          (Actions.of_seq
             (List.to_seq
                [ ( get_action_by_label alphabet "a"
                  , States.of_list [ get_state_by_id states 0 ] )
                ]));
        (* t3 *)
        Edges.add
          edges
          (get_state_by_id states 3)
          (Actions.of_seq
             (List.to_seq
                [ ( get_action_by_label alphabet "a"
                  , States.of_list [ get_state_by_id states 4 ] )
                ]));
        (* t4 *)
        Edges.add
          edges
          (get_state_by_id states 4)
          (Actions.of_seq
             (List.to_seq
                [ ( get_action_by_label alphabet "a"
                  , States.of_list [ get_state_by_id states 0 ] )
                ]));
        (* t5 *)
        Edges.add
          edges
          (get_state_by_id states 5)
          (Actions.of_seq
             (List.to_seq
                [ ( get_action_by_label alphabet "a"
                  , States.of_list
                      [ get_state_by_id states 0; get_state_by_id states 4 ] )
                ]));
        { init; states; alphabet; edges }
      in
      s, t
    ;;
  end

  (** [KS90] follows algorithm by Kanellakis and Smolka. *)
  module KS90 = struct
    (* Error when trying to split on empty block. *)
    exception EmptyBlock of Block.t

    (* Error if same state found in multiple blocks of a partition. *)
    exception PartitionsNotDisjoint of Partition.t

    (** [reachable_partition outgoing_edges pi] is the subset of states in [pi] reachable via [outgoing_edges]. *)
    let reachable_partition
      (outgoing_edges : States.t Actions.t)
      (pi : Partition.t)
      : Partition.t
      =
      let all_destinations =
        Actions.fold
          (fun (_a : action) (destinations : States.t) (acc : States.t) ->
            (* [a]'s should already be filtered to be all of the same kind. *)
            States.union acc destinations)
          outgoing_edges
          States.empty
      in
      (* return subset of [pi] that contains any of the destinations of [outgoing_edges] *)
      Partition.filter
        (fun (b : Block.t) ->
          (* get *)
          Bool.not (Block.is_empty (Block.inter b all_destinations)))
        pi
    ;;

    (** [split block action pi f] is ...
        (Definition follows `Fig. 3.1` on page 108.)
        [block] is ...
        [action] is ...
        [pi] is ...
        [edges] is the (sorted) list of outgoing edges. *)
    let split
      (block : Block.t)
      (a : action)
      (pi : Partition.t)
      (edges : States.t Actions.t Edges.t)
      : Partition.t
      =
      (* choose some state [s] in [block] *)
      let block_list = Block.to_list block in
      match block_list with
      | [] -> raise (EmptyBlock block)
      | s_state :: _block' ->
        (* get edges corresponding to [action] *)
        let s_edges = get_outgoing_actions edges s_state a in
        (* cache which partitions in [pi] [s_edges] can reach *)
        let s_reachable_partition = reachable_partition s_edges pi in
        (* for each state in [block] *)
        let b1, b2 =
          List.fold_left
            (fun ((b1', b2') : Block.t * Block.t) (t_state : state) ->
              let t_edges = get_outgoing_actions edges t_state a in
              if Actions.length t_edges > 0
              then (
                (* check if edges of s and t can reach the same blocks in [pi]. *)
                let t_reachable_partition = reachable_partition t_edges pi in
                (* if s and t can reach the same blocks *)
                if Partition.equal
                     (Partition.inter
                        s_reachable_partition
                        t_reachable_partition)
                     (Partition.union
                        s_reachable_partition
                        t_reachable_partition)
                then Block.add t_state b1', b2'
                else b1', Block.add t_state b2')
              else if (* no edges for [t_state] via [a] *)
                      Actions.length s_edges > 0
              then b1', Block.add t_state b2'
              else Block.add t_state b1', b2')
            (Block.empty, Block.empty)
            block_list
        in
        if Block.is_empty b2
        then Partition.of_list [ b1 ]
        else Partition.of_list [ b1; b2 ]
    ;;

    exception SplitEmpty of Partition.t
    exception SplitTooMany of Partition.t

    (** Error when duplication actions found. *)
    exception MultipleActionsSameLabel of States.t Actions.t Edges.t

    exception OldStateHasNoNewState of (state * (state, state) Hashtbl.t)

    (*  *)
    let run (s : fsm) (t : fsm) : bool * Partition.t =
      Printf.printf "[DEBUG] (a) entered run";
      (* initially [pi] is a partition with a single block containing all states. *)
      let pi, map_of_states =
        let init_block, map_of_states' =
          (* cant just merge States.t since their ids would conflict *)
          States.fold
            (fun (state : state)
              ((acc, map) : Block.t * (state, state) Hashtbl.t) ->
              let state' = make_state ~pp:state.pp (Block.cardinal acc) in
              (* save mapping from old to new state *)
              Hashtbl.add map state state';
              (* continue *)
              Block.add state' acc, map)
            t.states
            ( s.states
            , (* map states of [s] to themselves *)
              Hashtbl.of_seq
                (List.to_seq
                   (States.fold
                      (fun (state : state) (acc : (state * state) list) ->
                        List.append acc [ state, state ])
                      s.states
                      [])) )
        in
        ref (Partition.of_list [ init_block ]), map_of_states'
      in
      Printf.printf
        "[DEBUG] (b.1) initial pi: %s.\n(b.2) state map: [%s]."
        (Fsm.pstr (pp_wrap_as_supported (Fsm.Partition !pi)))
        (Hashtbl.fold
           (fun (old_state : state) (new_state : state) (acc : string) ->
             Printf.sprintf
               "%s\n  (old: %s -> new: %s)"
               acc
               (pstr (pp_wrap_as_supported (State old_state)))
               (pstr (pp_wrap_as_supported (State new_state))))
           map_of_states
           "");
      (* merge actions *)
      let s_alphabet = s.alphabet in
      let t_alphabet = t.alphabet in
      let alphabet, map_t_alphabet =
        Alphabet.fold
          (fun (t_action : action)
            ((alphabet, map) : Alphabet.t * (action, action) Hashtbl.t) ->
            let s_alphas =
              Alphabet.filter
                (fun (s_action : action) -> s_action.label == t_action.label)
                alphabet
            in
            if Alphabet.is_empty s_alphas
            then (
              (* create new action in alphabet and add to map *)
              let new_action =
                make_action ~label:t_action.label (Alphabet.cardinal alphabet)
              in
              let alphabet' = Alphabet.add new_action alphabet in
              alphabet', map)
            else (
              (* double check there is only one with matching label *)
              if Alphabet.cardinal s_alphas > 1
              then raise (MultipleActionsSameLabel s.edges);
              (* just update map to use existing in s *)
              Hashtbl.add map t_action (List.nth (Alphabet.elements s_alphas) 0);
              alphabet, map))
          t_alphabet
          (s_alphabet, Alphabet.cardinal t_alphabet |> Hashtbl.create)
      in
      Printf.printf
        "[DEBUG] (c.1) merged alphabet: %s.\n(c.2) alphabet map: [%s]."
        (pstr (pp_wrap_as_supported (Alphabet alphabet)))
        (Hashtbl.fold
           (fun (old_alpha : action) (new_alpha : action) (acc : string) ->
             Printf.sprintf
               "%s\n  (old: (%s) -> new: (%s))"
               acc
               (pstr (pp_wrap_as_supported (Action old_alpha)))
               (pstr (pp_wrap_as_supported (Action new_alpha))))
           map_t_alphabet
           "");
      (* merge edge tables *)
      let edges = s.edges in
      Edges.iter
        (fun (from_state : state) (outgoing_edges : States.t Actions.t) ->
          (* need to update states in edge *)
          Edges.add
            edges
            (Hashtbl.find map_of_states from_state)
            (Actions.of_seq
               (List.to_seq
                  (Actions.fold
                     (fun (action : action)
                       (destinations : States.t)
                       (acc : (action * States.t) list) ->
                       List.append
                         acc
                         [ ( Hashtbl.find map_t_alphabet action
                           , States.map
                               (fun (old_state : state) ->
                                 match
                                   Hashtbl.find_opt map_of_states old_state
                                 with
                                 | None ->
                                   raise
                                     (OldStateHasNoNewState
                                        (old_state, map_of_states))
                                 | Some new_state -> new_state)
                               destinations )
                         ])
                     outgoing_edges
                     []))))
        t.edges;
      Printf.printf
        "[DEBUG] (d) merged edges: %s"
        (pstr (pp_wrap_as_supported (Edges edges)));
      (* prepare main alg loop *)
      let changed = ref true in
      while !changed do
        changed := false;
        (* for each block in [pi] *)
        Partition.iter
          (fun (_b : Block.t) ->
            let b = ref _b in
            (* for each action *)
            Alphabet.iter
              (fun (a : action) ->
                (* filter [edges] that are of [a]. *)
                (* (start by assuming an equal number of each action.) *)
                let edges_of_a =
                  Edges.length edges
                  / Alphabet.cardinal (get_action_alphabet_from_edges edges)
                  |> Edges.create
                in
                Edges.iter
                  (fun (from_state : state)
                    (outgoing_edges : States.t Actions.t)
                    : unit ->
                    match Actions.find_opt outgoing_edges a with
                    | None -> ()
                    | Some destinations ->
                      Edges.add
                        edges_of_a
                        from_state
                        (Actions.of_seq (List.to_seq [ a, destinations ])))
                  edges;
                (* split *)
                match Partition.elements (split !b a !pi edges_of_a) with
                | [] -> Printf.printf "[DEBUG] split returned empty list."
                | b1 :: [] ->
                  (* check if same as b *)
                  if Bool.not (Block.equal b1 !b)
                  then (
                    pi := Partition.remove !b !pi;
                    pi := Partition.union !pi (Partition.of_list [ b1 ]);
                    (* b := b1; *)
                    changed := true)
                | [ b1; b2 ] ->
                  (* check if union is not the same as b *)
                  if Bool.not (Block.equal b1 !b)
                     && Bool.not (Block.equal b2 !b)
                  then (
                    Printf.printf "[DEBUG] b1 and b2 are not equal to b";
                    pi := Partition.remove !b !pi;
                    pi
                    := Partition.union
                         !pi
                         (Partition.of_list
                            (if Block.is_empty b1 then [ b2 ] else [ b1; b2 ]));
                    (* FIXME: how could [b := b1 and b2] as the book suggests ? *)
                    if Block.is_empty b1 then b := b2 else b := b1;
                    changed := true)
                | _pi ->
                  Printf.printf
                    "Warning: split returned more than 2 items: %s."
                    (pstr
                       (pp_wrap_as_supported
                          (Partition (Partition.of_list _pi)))))
              alphabet;
            ())
          !pi
      done;
      (* [s] and [t] are bisimilar if there are no singleton blocks in [pi] *)
      let non_bisimilar_partition =
        Partition.filter
          (fun (b : Block.t) ->
            (* either is alone *)
            let b_is_singleton = Block.cardinal b == 1 in
            (* or does not have another state in block from the other fsm *)
            let b_not_diverse =
              Bool.not
                (* i.e., for all states in block b *)
                (Block.for_all
                   (fun (state : state) ->
                     (* there must exist another state *)
                     Block.exists
                       (fun (state' : state) ->
                         (* that is different *)
                         Bool.not (Int.equal state.id state'.id)
                         &&
                         (* and originates from different fsms *)
                         let original_state =
                           get_reverse_map_state map_of_states state
                         in
                         let original_state' =
                           get_reverse_map_state map_of_states state'
                         in
                         (States.mem original_state s.states
                          && States.mem original_state' t.states)
                         || (States.mem original_state' s.states
                             && States.mem original_state t.states))
                       b)
                   b)
            in
            (* return *)
            b_is_singleton || b_not_diverse)
          !pi
      in
      if Partition.is_empty non_bisimilar_partition
      then (* bisimilar *)
        true, !pi
      else (* not bisimilar *)
        false, non_bisimilar_partition
    ;;
  end

  (** [PT87] follows algorithm by Paige and Tarjan,
      improving upon [KS90]. *)
  module PT87 = struct
    (* TODO: *)
  end
end

let bisim_foo : int = 0
