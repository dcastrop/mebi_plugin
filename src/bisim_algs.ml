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
        let init = { id = 0; pp = "t0" } in
        let states =
          States.of_list [ { id = 0; pp = "t0" }; { id = 1; pp = "t1" } ]
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

    (** [reachable_partition outgoing_edges pi] is the subset of states in [pi] reachable via [outgoing_edges]. *)
    let reachable_partition
      (outgoing_edges : States.t Actions.t)
      (pi : Partition.t)
      : Partition.t
      =
      (* !

         error in here, somehow thinks partitions are not disjoint?

         filter function must be wrong
      *)
      (* Feedback.msg_warning
         (Pp.str
         (Printf.sprintf
         "checking for reachability of: %s;\nof states in: %s."
         (pstr_action_alphabet
         (get_action_alphabet_from_actions outgoing_edges))
         (pstr_partition ~pp:() pi))); *)
      (* get the destinations of all outgoing_edges *)
      let all_destinations =
        Actions.fold
          (fun (a : action) (destinations : States.t) (acc : States.t) ->
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

    (* Actions.fold
       (fun (action : action) (destinations : States.t) (acc : Block.t) ->
       (* filter [pi] to find block containing one of the destination states. *)
       let filtered_pi =
       Partition.filter
       (fun (b : Block.t) ->
       Feedback.msg_warning
       (Pp.str
       (Printf.sprintf
       "b: %s.\n\
       destinations: %s.\n\
       is reachable: %b.\n\
       intersection: %s."
       (pstr_states ~pp:() b)
       (pstr_states ~pp:() destinations)
       (Bool.not (Block.is_empty (Block.inter destinations b)))
       (pstr_states ~pp:() (Block.inter destinations b))));
       Bool.not (Block.is_empty (Block.inter destinations b)))
       pi
       in
       Feedback.msg_warning
       (Pp.str
       (Printf.sprintf
       "filtered pi: %s."
       (pstr_partition ~pp:() filtered_pi)));
       (* throw error if state found in more than one partition *)
       if Partition.is_empty filtered_pi
       then Block.empty
       else if Partition.cardinal filtered_pi > 1
       then raise (PartitionsNotDisjoint pi)
       else (
       (* get single block in partition *)
       let filtered' = List.nth (Partition.elements filtered_pi) 0 in
       if Block.is_empty filtered'
       then acc
       else Block.add_seq (Block.to_seq filtered') acc))
       outgoing_edges
       Block.empty *)

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
      (* Feedback.msg_warning
         (Pp.str
         (Printf.sprintf
         "#### split...\nblock: %s.\na: %s\npi: %s.\nedges: %s.\n"
         (Fsm.pstr_states ~pp:() block)
         (Fsm.pstr_action a)
         (pstr_partition ~pp:() pi)
         (Fsm.pstr_edges ~pp:() edges))); *)
      (* choose some state [s] in [block] *)
      let block_list = Block.to_list block in
      match block_list with
      | [] -> raise (EmptyBlock block)
      | s_state :: _block' ->
        (* get edges corresponding to [action] *)
        let s_edges = get_edges_with_action edges s_state a in
        (* cache which partitions in [pi] [s_edges] can reach *)
        let s_reachable_partition = reachable_partition s_edges pi in
        (* Feedback.msg_info
           (Pp.str
           (Printf.sprintf
           "\nvia (%s) [%s] reachable_blocks: %s.\n"
           (Fsm.pstr_action a)
           (Fsm.pstr_state ~pp:() s_state)
           (pstr_partition ~pp:() s_reachable_partition))); *)
        (* for each state in [block] *)
        let b1, b2 =
          List.fold_left
            (fun ((b1', b2') : Block.t * Block.t) (t_state : state) ->
              let t_edges = get_edges_with_action edges t_state a in
              (* Feedback.msg_info
                 (Pp.str
                 (Printf.sprintf
                 "\nvia (%s) %s has (%d) t_edges: [%s].\n"
                 (Fsm.pstr_action a)
                 (Fsm.pstr_state ~pp:() t_state)
                 (Actions.length t_edges)
                 (Actions.fold
                 (fun (_a : action) (_dest : States.t) (_acc : string) ->
                 Printf.sprintf
                 "%s  (%s) --( %s )-> (%s)\n"
                 _acc
                 (pstr_state ~pp:() t_state)
                 (pstr_action _a)
                 (pstr_states ~pp:() _dest))
                 t_edges
                 "\n"))); *)
              if Actions.length t_edges > 0
              then (
                (* check if edges of s and t can reach the same blocks in [pi]. *)
                let t_reachable_partition = reachable_partition t_edges pi in
                (* Feedback.msg_warning
                   (Pp.str
                   (Printf.sprintf
                   "via (%s)...\n\
                   s [%s] reachable: %s.\n\
                   t_reachable [%s]: %s."
                   (Fsm.pstr_action a)
                   (Fsm.pstr_state ~pp:() s_state)
                   (pstr_partition ~pp:() s_reachable_partition)
                   (Fsm.pstr_state ~pp:() t_state)
                   (pstr_partition ~pp:() t_reachable_partition))); *)
                (* if s and t can reach the same blocks *)
                if Partition.equal
                     (Partition.inter
                        s_reachable_partition
                        t_reachable_partition)
                     (Partition.union
                        s_reachable_partition
                        t_reachable_partition)
                then Block.add t_state b1', b2'
                else
                  ( (* Feedback.msg_info
                       (Pp.str
                       (Printf.sprintf
                       "\n\
                       %s and %s cannot reach the same block via label %s.\n"
                       (pstr_state ~pp:() s_state)
                       (pstr_state ~pp:() t_state)
                       (Fsm.pstr_action a))); *)
                    b1'
                  , Block.add t_state b2' ))
              else if (* no edges for [t_state] via [a] *)
                      (* Feedback.msg_info
                         (Pp.str
                         (Printf.sprintf
                         "\n%s has no transitions via label %s.\n"
                         (pstr_state ~pp:() t_state)
                         (Fsm.pstr_action a))); *)
                      (* if [s_state] can use [a], then put [t] in [b2] *)
                      Actions.length s_edges > 0
              then b1', Block.add t_state b2'
              else Block.add t_state b1', b2')
            (Block.empty, Block.empty)
            block_list
        in
        (* Feedback.msg_warning
           (Pp.str
           (Printf.sprintf
           "\nb1: %s.\nb2: %s.\n#### split #### (end)\n"
           (Fsm.pstr_states ~pp:() b1)
           (Fsm.pstr_states ~pp:() b2))); *)
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
    let run (s : fsm) (t : fsm) : bool * Partition.t =
      Feedback.msg_debug (Pp.str "(a) entered run");
      (* initially [pi] is a partition with a single block containing all states. *)
      let pi, map_t_states =
        let init_block, map_t_states' =
          (* cant just merge States.t since their ids would conflict *)
          States.fold
            (fun (state : Fsm.state)
              ((acc, map) : Block.t * (Fsm.state, Fsm.state) Hashtbl.t) ->
              let state' = Fsm.state ~pp:state.pp (Block.cardinal acc) in
              (* save mapping from old to new state *)
              Hashtbl.add map state state';
              (* continue *)
              Block.add state' acc, map)
            t.states
            (s.states, States.cardinal t.states |> Hashtbl.create)
        in
        ref (Partition.of_list [ init_block ]), map_t_states'
      in
      Feedback.msg_debug
        (Pp.str
           (Printf.sprintf
              "(b.1) initial pi: %s.\n(b.2) state map: [%s]."
              (pstr_partition !pi)
              (Hashtbl.fold
                 (fun (old_state : state) (new_state : state) (acc : string) ->
                   Printf.sprintf
                     "%s\n  (old: %s -> new: %s)"
                     acc
                     (pstr_state ~pp:() old_state)
                     (pstr_state ~pp:() new_state))
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
      Feedback.msg_debug
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
                                   (* Feedback.msg_warning
                                      (Pp.str
                                      (Printf.sprintf
                                      "old_state: %s is not in \
                                      map_t_states: [%s]"
                                      (pstr_state ~ids:() ~pp:() old_state)
                                      (Hashtbl.fold
                                      (fun (old : state)
                                      (_new : state)
                                      (_acc : string) ->
                                      Printf.sprintf
                                      "%s  (old: %s) -> (new: %s)\n"
                                      _acc
                                      (pstr_state
                                      ~ids:()
                                      ~pp:()
                                      old)
                                      (pstr_state
                                      ~ids:()
                                      ~pp:()
                                      _new))
                                      map_t_states
                                      "\n"))); *)
                                   raise
                                     (OldStateHasNoNewState
                                        (old_state, map_t_states))
                                 | Some new_state -> new_state)
                               destinations )
                         ])
                     outgoing_edges
                     []))))
        t.edges;
      Feedback.msg_debug
        (Pp.str
           (Printf.sprintf "(d) merged edges: %s" (pstr_edges ~pp:() edges)));
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
                        (Actions.of_seq (List.to_seq [ a, destinations ]))
                    (* Actions.fold
                       (fun (action : action)
                       (destinations : States.t)
                       (acc : (action * States.t) list) ->
                       if action.label == a.label
                       then List.append acc [ action, destinations ]
                       else acc)
                       outgoing_edges
                       [] *)
                    (* if List.is_empty relevant_edges
                       then () *)
                    (* else Edges.add edges_of_a from_state outgoing_edges) *))
                  edges;
                (* split *)
                match Partition.elements (split !b a !pi edges_of_a) with
                | [] ->
                  Feedback.msg_debug
                    (Pp.str (Printf.sprintf "split returned empty list."))
                | b1 :: [] ->
                  (* check if same as b *)
                  Feedback.msg_debug
                    (Pp.str
                       (Printf.sprintf
                          "split returned only b1: %s."
                          "[...]" (* (pstr_states ~pp:() b1) *)));
                  if Bool.not (Block.equal b1 !b)
                  then (
                    pi := Partition.remove !b !pi;
                    pi := Partition.union !pi (Partition.of_list [ b1 ]);
                    (* b := b1; *)
                    changed := true)
                | [ b1; b2 ] ->
                  Feedback.msg_debug
                    (Pp.str
                       (Printf.sprintf
                          "split returned b1: %s.\nand b2: %s"
                          "[...]" (* (pstr_states ~pp:() b1) *)
                          "[...]" (* (pstr_states ~pp:() b2) *)));
                  (* check if union is not the same as b *)
                  if Bool.not (Block.equal b1 !b)
                     && Bool.not (Block.equal b2 !b)
                  then (
                    Feedback.msg_debug
                      (Pp.str (Printf.sprintf "b1 and b2 are not equal to b"));
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
                  Feedback.msg_debug
                    (Pp.str
                       (Printf.sprintf
                          "split returned more than 2 items: %s."
                          (pstr_partition (Partition.of_list _pi)))))
              alphabet;
            ())
          !pi
      done;
      (* [s] and [t] are bisimilar if there are no singleton blocks in [pi] *)
      let non_bisimilar_partition =
        Partition.filter (fun (b : Block.t) -> Block.cardinal b == 1) !pi
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
