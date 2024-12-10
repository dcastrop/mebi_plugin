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
        let init = { id = 0; hash = -1; pp = "s0" } in
        let states =
          States.of_list
            [ { id = 0; hash = -1; pp = "s0" }
            ; { id = 1; hash = -1; pp = "s1" }
            ; { id = 2; hash = -1; pp = "s2" }
            ]
        in
        let edges = Edges.create 4 in
        (* s0 *)
        Edges.add
          edges
          (get_state_by_id states 0)
          (Actions.of_seq
             (List.to_seq
                [ ( { id = 1; label = "a" }
                  , States.of_list
                      [ get_state_by_id states 1; get_state_by_id states 2 ] )
                ]));
        (* s1 *)
        Edges.add
          edges
          (get_state_by_id states 1)
          (Actions.of_seq
             (List.to_seq
                [ ( { id = 2; label = "b" }
                  , States.of_list [ get_state_by_id states 2 ] )
                ]));
        (* s2 *)
        Edges.add
          edges
          (get_state_by_id states 2)
          (Actions.of_seq
             (List.to_seq
                [ ( { id = 2; label = "b" }
                  , States.of_list [ get_state_by_id states 2 ] )
                ]));
        { init; states; edges }
      in
      (* t *)
      let (t : fsm) =
        let init = { id = 0; hash = -1; pp = "t0" } in
        let states =
          States.of_list
            [ { id = 0; hash = -1; pp = "t0" }
            ; { id = 1; hash = -1; pp = "t1" }
            ]
        in
        let edges = Edges.create 4 in
        (* t0 *)
        Edges.add
          edges
          (get_state_by_id states 0)
          (Actions.of_seq
             (List.to_seq
                [ ( { id = 1; label = "a" }
                  , States.of_list [ get_state_by_id states 1 ] )
                ]));
        (* t1 *)
        Edges.add
          edges
          (get_state_by_id states 1)
          (Actions.of_seq
             (List.to_seq
                [ ( { id = 2; label = "b" }
                  , States.of_list [ get_state_by_id states 1 ] )
                ]));
        { init; states; edges }
      in
      s, t
    ;;

    (** [exa_2] is `Example 3.2.6` on page 107. *)
    let exa_2 : fsm * fsm =
      (* s *)
      let (s : fsm) =
        let init = { id = 0; hash = -1; pp = "s0" } in
        let states =
          States.of_list
            [ { id = 0; hash = -1; pp = "s0" }
            ; { id = 1; hash = -1; pp = "s1" }
            ; { id = 2; hash = -1; pp = "s2" }
            ; { id = 3; hash = -1; pp = "s3" }
            ; { id = 4; hash = -1; pp = "s4" }
            ]
        in
        let edges = Edges.create 4 in
        (* s0 *)
        Edges.add
          edges
          (get_state_by_id states 0)
          (Actions.of_seq
             (List.to_seq
                [ ( { id = 1; label = "a" }
                  , States.of_list
                      [ get_state_by_id states 1; get_state_by_id states 2 ] )
                ]));
        (* s1 *)
        Edges.add
          edges
          (get_state_by_id states 1)
          (Actions.of_seq
             (List.to_seq
                [ ( { id = 1; label = "a" }
                  , States.of_list [ get_state_by_id states 3 ] )
                ; ( { id = 2; label = "b" }
                  , States.of_list [ get_state_by_id states 4 ] )
                ]));
        (* s2 *)
        Edges.add
          edges
          (get_state_by_id states 2)
          (Actions.of_seq
             (List.to_seq
                [ ( { id = 1; label = "a" }
                  , States.of_list [ get_state_by_id states 4 ] )
                ]));
        (* s3 *)
        Edges.add
          edges
          (get_state_by_id states 3)
          (Actions.of_seq
             (List.to_seq
                [ ( { id = 1; label = "a" }
                  , States.of_list [ get_state_by_id states 0 ] )
                ]));
        (* s4 *)
        Edges.add
          edges
          (get_state_by_id states 4)
          (Actions.of_seq
             (List.to_seq
                [ ( { id = 1; label = "a" }
                  , States.of_list [ get_state_by_id states 0 ] )
                ]));
        { init; states; edges }
      in
      (* t *)
      let (t : fsm) =
        let init = { id = 0; hash = -1; pp = "t0" } in
        let states =
          States.of_list
            [ { id = 0; hash = -1; pp = "t0" }
            ; { id = 1; hash = -1; pp = "t1" }
            ; { id = 2; hash = -1; pp = "t2" }
            ; { id = 3; hash = -1; pp = "t3" }
            ; { id = 4; hash = -1; pp = "t4" }
            ; { id = 5; hash = -1; pp = "t5" }
            ]
        in
        let edges = Edges.create 4 in
        (* t0 *)
        Edges.add
          edges
          (get_state_by_id states 0)
          (Actions.of_seq
             (List.to_seq
                [ ( { id = 1; label = "a" }
                  , States.of_list
                      [ get_state_by_id states 1; get_state_by_id states 3 ] )
                ]));
        (* t1 *)
        Edges.add
          edges
          (get_state_by_id states 1)
          (Actions.of_seq
             (List.to_seq
                [ ( { id = 2; label = "b" }
                  , States.of_list [ get_state_by_id states 2 ] )
                ; ( { id = 1; label = "a" }
                  , States.of_list [ get_state_by_id states 5 ] )
                ; ( { id = 2; label = "b" }
                  , States.of_list [ get_state_by_id states 5 ] )
                ]));
        (* t2 *)
        Edges.add
          edges
          (get_state_by_id states 2)
          (Actions.of_seq
             (List.to_seq
                [ ( { id = 1; label = "a" }
                  , States.of_list [ get_state_by_id states 0 ] )
                ]));
        (* t3 *)
        Edges.add
          edges
          (get_state_by_id states 3)
          (Actions.of_seq
             (List.to_seq
                [ ( { id = 1; label = "a" }
                  , States.of_list [ get_state_by_id states 4 ] )
                ]));
        (* t4 *)
        Edges.add
          edges
          (get_state_by_id states 4)
          (Actions.of_seq
             (List.to_seq
                [ ( { id = 1; label = "a" }
                  , States.of_list [ get_state_by_id states 0 ] )
                ]));
        (* t5 *)
        Edges.add
          edges
          (get_state_by_id states 5)
          (Actions.of_seq
             (List.to_seq
                [ ( { id = 1; label = "a" }
                  , States.of_list
                      [ get_state_by_id states 0; get_state_by_id states 4 ] )
                ]));
        { init; states; edges }
      in
      s, t
    ;;
  end

  (** [KS90] follows algorithm by Kanellakis and Smolka. *)
  module KS90 = struct
    (** [Block] is a set of states.
        This is necessary since [KS90] requires the actions of states to be sortable. *)
    module Block = Fsm.States

    module Partition = Set.Make (Block)

    (**  *)
    let pstr_partition
      ?(ids : unit option)
      ?(pp : unit option)
      ?(indent : int = 1)
      (p : Partition.t)
      : string
      =
      if Partition.is_empty p
      then "[ ] (empty)"
      else
        Printf.sprintf
          "[%s]"
          (Partition.fold
             (fun (b : Block.t) (acc : string) ->
               Printf.sprintf
                 "%s%s%s\n"
                 acc
                 (Mebi_utils.str_tabs indent)
                 (* figure out which units to pass on (bit messy) *)
                 (handle_states_pstr ~indent:(indent + 1) ids pp None b))
             p
             "\n")
    ;;

    (* Error when trying to split on empty block. *)
    exception EmptyBlock of Block.t

    (* Error if same state found in multiple blocks of a partition. *)
    exception PartitionsNotDisjoint of Partition.t

    (** [reachable_blocks outgoing_edges pi] is the subset of states in [pi] reachable via [outgoing_edges]. *)
    let reachable_blocks
      (outgoing_edges : States.t Actions.t)
      (pi : Partition.t)
      : Block.t
      =
      Actions.fold
        (fun (action : action) (destinations : States.t) (acc : Block.t) ->
          (* filter [pi] to find block containing one of the destination states. *)
          let filtered_pi =
            Partition.filter
              (fun (b : Block.t) -> Block.is_empty (Block.inter destinations b))
              pi
          in
          (* throw error if state found in more than one partition *)
          if Partition.cardinal filtered_pi > 1
          then raise (PartitionsNotDisjoint pi)
          else (
            (* get single block in partition *)
            let filtered' = List.nth (Partition.elements filtered_pi) 0 in
            if Block.is_empty filtered'
            then acc
            else Block.add_seq (Block.to_seq filtered') acc))
        outgoing_edges
        Block.empty
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
      Feedback.msg_warning
        (Pp.str
           (Printf.sprintf
              "#### split...\nblock: %s.\na: %s\npi: %s.\nedges: %s.\n"
              (Fsm.pstr_states ~pp:() block)
              (Fsm.pstr_action a)
              (pstr_partition ~pp:() pi)
              (Fsm.pstr_edges ~pp:() edges)));
      (* choose some state [s] in [block] *)
      match Block.to_list block with
      | [] -> raise (EmptyBlock block)
      | s :: block' ->
        (* get edges corresponding to [action] *)
        let s_edges = get_edges_with_action edges s a in
        (* cache which partitions in [pi] [s_edges] can reach *)
        let s_reachable = reachable_blocks s_edges pi in
        (* for each state in [block] *)
        let b1, b2 =
          List.fold_left
            (fun ((b1', b2') : Block.t * Block.t) t ->
              let t_edges = get_edges_with_action edges t a in
              (* check if edges of s and t can reach the same blocks in [pi]. *)
              let t_reachable = reachable_blocks t_edges pi in
              (* TODO: this seems to be broken

                 !!! continue from here

                 ! no, go back and filter block [b] to only contain those that can perform the action [a]
              *)
              Feedback.msg_warning
                (Pp.str
                   (Printf.sprintf
                      "via (%s)...\n\
                       s [%s] reachable: %s.\n\
                       t_reachable [%s]: %s."
                      (Fsm.pstr_action a)
                      (Fsm.pstr_state ~pp:() s)
                      (Fsm.pstr_states ~pp:() s_reachable)
                      (Fsm.pstr_state ~pp:() t)
                      (Fsm.pstr_states ~pp:() t_reachable)));
              if Block.is_empty (Block.inter s_reachable t_reachable)
              then (
                Feedback.msg_warning
                  (Pp.str
                     (Printf.sprintf
                        "s and t cannot reach the same block via label %s."
                        (Fsm.pstr_action a)));
                Block.union b1' (Block.of_list [ t ]), b2')
              else b1', Block.union b2' (Block.of_list [ t ]))
            (Block.empty, Block.empty)
            block'
        in
        Feedback.msg_warning
          (Pp.str
             (Printf.sprintf
                "\nb1: %s.\nb2: %s.\n#### split #### (end)\n"
                (Fsm.pstr_states ~pp:() b1)
                (Fsm.pstr_states ~pp:() b2)));
        if Block.is_empty b2
        then Partition.of_list [ b1 ]
        else Partition.of_list [ b1; b2 ]
    ;;

    (* let changed = ref true in
       let rec loop (pi : Partition.t) : Partition.t =
       if !changed
       then (
       changed := false;
       Partition.fold (fun (b:Block.t) (acc:Partition.t) -> acc) pi Partition.empty *)

    exception SplitEmpty of Partition.t
    exception SplitTooMany of Partition.t

    (** Error when duplication actions found. *)
    exception MultipleActionsSameLabel of States.t Actions.t Edges.t

    exception OldStateHasNoNewState of (state * (state, state) Hashtbl.t)

    (*  *)
    let run (s : fsm) (t : fsm) : Partition.t =
      Feedback.msg_warning (Pp.str "(a) entered run");
      (* initially [pi] is a partition with a single block containing all states. *)
      let pi, map_t_states =
        let init_block, map_t_states' =
          (* cant just merge States.t since their ids would conflict *)
          States.fold
            (fun (state : Fsm.state)
              ((acc, map) : Block.t * (Fsm.state, Fsm.state) Hashtbl.t) ->
              let state' = Fsm.state ~pp:state.pp (Block.cardinal acc) in
              (* save mapping from old to new state *)
              (* (match Hashtbl.find_opt map state with
                 | None -> Hashtbl.add map state (States.of_list [ state' ])
                 | Some states -> *)
              Hashtbl.add map state state';
              (* ); *)
              (* continue *)
              Block.add state' acc, map)
            s.states
            (t.states, States.cardinal t.states |> Hashtbl.create)
        in
        ref (Partition.of_list [ init_block ]), map_t_states'
      in
      Feedback.msg_warning
        (Pp.str
           (Printf.sprintf
              "(b.1) initial pi: %s.\n(b.2) state map: [%s]."
              (Partition.fold
                 (fun (b : Block.t) (acc : string) ->
                   Printf.sprintf "%s%s" acc (pstr_states ~long:() b))
                 !pi
                 "")
              (Hashtbl.fold
                 (fun (old_state : state) (new_state : state) (acc : string) ->
                   Printf.sprintf
                     "%s\n  (old: %s -> new: %s)"
                     acc
                     (pstr_state ~ids:() ~pp:() old_state)
                     (pstr_state ~ids:() ~pp:() new_state))
                 map_t_states
                 "")));
      (* merge actions *)
      let s_alphabet = get_action_alphabet_from_edges s.edges in
      let t_alphabet = get_action_alphabet_from_edges t.edges in
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
                action ~label:t_action.label (Alphabet.cardinal alphabet)
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
      Feedback.msg_warning
        (Pp.str
           (Printf.sprintf
              "(c.1) merged alphabet: %s.\n(c.2) alphabet map: [%s]."
              (pstr_action_alphabet ~long:() alphabet)
              (Hashtbl.fold
                 (fun (old_alpha : action) (new_alpha : action) (acc : string) ->
                   Printf.sprintf
                     "%s\n  (old: (%s) -> new: (%s))"
                     acc
                     (pstr_action ~long:() old_alpha)
                     (pstr_action ~long:() new_alpha))
                 map_t_alphabet
                 "")));
      (* merge edge tables *)
      let edges = s.edges in
      Edges.iter
        (fun (from_state : state) (outgoing_edges : States.t Actions.t) ->
          (* need to update states in edge *)
          Edges.add
            edges
            (Hashtbl.find map_t_states from_state)
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
                                   Hashtbl.find_opt map_t_states old_state
                                 with
                                 | None ->
                                   Feedback.msg_warning
                                     (Pp.str
                                        (Printf.sprintf
                                           "old_state: %s is not in \
                                            map_t_states"
                                           (pstr_state ~ids:() ~pp:() old_state)));
                                   raise
                                     (OldStateHasNoNewState
                                        (old_state, map_t_states))
                                 | Some new_state -> new_state)
                               destinations )
                         ])
                     outgoing_edges
                     []))))
        t.edges;
      Feedback.msg_warning
        (Pp.str (Printf.sprintf "(d) merged edges: %s" (pstr_edges edges)));
      (* prepare main alg loop *)
      let changed = ref true in
      while !changed do
        changed := false;
        (* for each block in [pi] *)
        Partition.iter
          (fun (b : Block.t) ->
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
                    let relevant_edges =
                      Actions.fold
                        (fun (action : action)
                          (destinations : States.t)
                          (acc : (action * States.t) list) ->
                          if action.label == a.label
                          then List.append acc [ action, destinations ]
                          else acc)
                        outgoing_edges
                        []
                    in
                    if List.is_empty relevant_edges
                    then ()
                    else Edges.add edges_of_a from_state outgoing_edges)
                  edges;
                (* List.filter (fun e -> e.action.label==a) edges in *)
                let pi' = split b a !pi edges_of_a in
                (* check no greater than 2 blocks were returned *)
                if Partition.cardinal pi' > 2
                then raise (SplitTooMany pi')
                else if Partition.cardinal pi' == 2
                then
                  (* if [pi'] has two blocks that are not just [b] again, *)
                  if Bool.not
                       (Block.equal
                          b
                          (Partition.fold
                             (fun (b' : Block.t) (acc : Block.t) ->
                               Block.add_seq (Block.to_seq b') acc)
                             pi'
                             Block.empty))
                  then (
                    (* refine further using [pi'] *)
                    pi := pi';
                    changed := true))
              alphabet;
            ())
          !pi
      done;
      (* return partitions *)
      !pi
    ;;
  end

  (** [PT87] follows algorithm by Paige and Tarjan,
      improving upon [KS90]. *)
  module PT87 = struct
    (* TODO: *)
  end
end

let bisim_foo : int = 0
