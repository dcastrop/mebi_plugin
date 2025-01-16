(* modules in this file follow the sections in Chapter 3 of
   [`Advanced Topics in Bisimulation and Coinduction`].
   (See https://doi.org/10.1017/CBO9780511792588.) *)

open Fsm
open Pp_ext

(** [bisim_result] is returned by the algorithms that check for bisimilarity. *)
type bisim_result =
  { are_bisimilar : bool
  ; bisimilar_states : Partition.t
  ; non_bisimilar_states : Partition.t
  }

(** [RCP] contains algorithms for solving the
    `Relational Coarsest Partitioning` problem. *)
module RCP = struct
  (** [Examples] contains examples to be used by either [KS90] or [PT87]. *)
  module Examples = struct
    type example =
      { name : string
      ; s : fsm
      ; t : fsm
      }

    let exa (name : string) (s : fsm) (t : fsm) : example = { name; s; t }

    (** [exa_1] is `Example 3.2.5` on page 106. *)
    let exa_1 : example =
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
      exa "exa1" s t
    ;;

    (** [exa_2] is `Example 3.2.6` on page 107. *)
    let exa_2 : example =
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
      exa "exa2" s t
    ;;
  end

  (** [KS90] implements the algorithm by Kanellakis and Smolka (1990).
  *)
  module KS90' = struct
    exception EmptyBlock of Block.t
    exception PartitionsNotDisjoint of Partition.t

    (** [] *)
    let _ = ()

    (** [] *)
    let _ = ()

    (** [] *)
    let reachable_partitions
          ?(coq : bool = false)
          ?(show : bool = false)
          ?(debug : bool = false)
          (edges : States.t Actions.t)
          (pi : Partition.t)
      : Partition.t
      =
      let destinations =
        Actions.fold
          (fun (_a : action) (destinations : States.t) (acc : States.t) ->
             States.union acc destinations)
          edges
          States.empty
      in
      Partition.filter
        (fun (block : Block.t) ->
           Bool.not (Block.is_empty (Block.inter block destinations)))
        pi
    ;;

    (** []
        @return a partition that contains the split [block].
        @raise EmptyBlock if [block] is empty. *)
    let split
          ?(coq : bool = false)
          ?(show : bool = false)
          ?(debug : bool = false)
          (block : Block.t)
          (a : action)
          (pi : Partition.t)
          (edges : States.t Actions.t Edges.t)
      : Block.t * Block.t
      =
      (* *)
      handle_pp
        ~coq
        ~show
        ~debug:true
        (Printf.sprintf
           "%s\n\n\
           \  action: %s.\n\
           \  edges: %s.\n\n\
           \  block: %s.\n\n\
           \  pi: %s.\n\n\
            %s\n\n"
           "/\\/\\/\\ KS90.split /\\/\\/\\"
           (pstr (pp_wrap_as_supported (Action a)))
           (pstr ~tabs:2 (pp_wrap_as_supported (Edges edges)))
           (pstr ~tabs:2 (pp_wrap_as_supported (Block block)))
           (pstr ~tabs:2 (pp_wrap_as_supported (Partition pi)))
           "/\\/\\/\\/\\/\\/\\/\\/\\/\\");
      (* *)
      let _block (* list for pattern matching *) = Block.to_list block in
      match _block with
      | [] -> raise (EmptyBlock block)
      | (s : state) :: _block' ->
        let s_edges = get_outgoing_actions edges s a in
        let s_reachable_partitions =
          reachable_partitions ~coq ~show ~debug s_edges pi
        in
        (* *)
        List.fold_left
          (fun ((b1, b2) : Block.t * Block.t) (t : state) ->
             let t_edges = get_outgoing_actions edges t a in
             match Actions.length s_edges > 0, Actions.length t_edges > 0 with
             | false, false ->
               (* neither [s] or [t] have action [a] *) Block.add t b1, b2
             | true, true ->
               (* both [s] and [t] have action [a] *)
               let t_reachable_partitions =
                 reachable_partitions ~coq ~show ~debug t_edges pi
               in
               let p_inter =
                 Partition.inter s_reachable_partitions t_reachable_partitions
               and p_union =
                 Partition.union s_reachable_partitions t_reachable_partitions
               in
               (match Partition.equal p_inter p_union with
                | true ->
                  (* both [s] and [t] can reach the same blocks via action [a] *)
                  Block.add t b1, b2
                | false ->
                  (* must split since [s] and [t] can reach different blocks via action [a] *)
                  b1, Block.add t b2)
             | _, _ ->
               (* only one of [s] or [t] has action [a] *) b1, Block.add t b2)
          (Block.empty, Block.empty)
          _block
    ;;

    (** [run ?coq s t] algorithmically checks if [s] and [t] are bisimilar, returning a [bisim_result] with further details.
      @param ?coq determines whether [Pp.Feedback.msg_info] or [Printf.printf] is used for output.
      @param s is an [fsm] to check.
      @param t is an [fsm] to check. *)
    let run
          ?(coq : bool = false)
          ?(show : bool = false)
          ?(debug : bool = true)
          (s : fsm)
          (t : fsm)
      : bisim_result
      =
      (* *)
      handle_pp ~coq ~show ~debug:true "\n\n=/=/=/= KS90.run =/=/=/=\n\n";
      (* get initial partition [pi] by merging states from [s] and [t] into single set. *)
      let merged_fsm, map_of_alphabet, map_of_states = Fsm.merge_fsm s t in
      match merged_fsm with
      | { alphabet; states; edges; _ } ->
        let pi (* working partition *) = ref (Partition.of_list [ states ]) in
        (* *)
        let changed = ref true in
        while !changed do
          changed := false;
          (* *)
          Partition.iter
            (fun (_b : Block.t) : unit ->
               let b = ref _b in
               Alphabet.iter
                 (fun (a : action) : unit ->
                    (* *)
                    handle_pp
                      ~coq
                      ~show:false
                      ~debug:true
                      (Printf.sprintf
                         "- - - next iteration - - -\n\n\
                         \  action: %s.\n\
                         \  block: %s.\n\n\
                         \  pi: %s.\n\n\
                          - - - - - - - - -\n\n"
                         (pstr (pp_wrap_as_supported (Action a)))
                         (pstr ~tabs:1 (pp_wrap_as_supported (Block _b)))
                         (pstr ~tabs:1 (pp_wrap_as_supported (Partition !pi))));
                    (* *)
                    let edges_of_a = Edges.create 0 in
                    Edges.iter
                      (fun (from_state : state)
                        (outgoing_edges : States.t Actions.t)
                        : unit ->
                         match Actions.find_opt outgoing_edges a with
                         | None -> (* skip edge without action [a] *) ()
                         | Some destinations ->
                           Edges.add
                             edges_of_a
                             from_state
                             (Actions.of_seq (List.to_seq [ a, destinations ])))
                      edges;
                    (* *)
                    let b1, b2 = split ~coq ~show ~debug !b a !pi edges_of_a in
                    match Block.is_empty b1, Block.is_empty b2 with
                    | true, true ->
                      (* both are empty, this is not supposed to happen *)
                      handle_pp
                        ~coq
                        ~show
                        ~debug:true
                        (Printf.sprintf "split returned two empty blocks.\n\n");
                      ()
                    | false, true ->
                      (* empty [b2] means that split did not occur *)
                      assert (Block.equal b1 !b);
                      handle_pp
                        ~coq
                        ~show
                        ~debug:true
                        (Printf.sprintf
                           "split returned empty b2.\nb1: %s.\n\n"
                           (pstr (pp_wrap_as_supported (Block b1))));
                      ()
                    | _, _ ->
                      (* split did occur, so replace [b] with [b1] and [b2] and refine *)
                      assert (Bool.not (Block.is_empty b1));
                      handle_pp
                        ~coq
                        ~show
                        ~debug:true
                        (Printf.sprintf
                           "split returned two blocks.\nb1: %s.\nb2: %s.\n\n"
                           (pstr (pp_wrap_as_supported (Block b1)))
                           (pstr (pp_wrap_as_supported (Block b2))));
                      pi := Partition.remove !b !pi;
                      pi := Partition.union !pi (Partition.of_list [ b1; b2 ]);
                      changed := true;
                      ())
                 alphabet)
            !pi
        done;
        (* *)
        handle_pp ~coq ~show ~debug "=/= KS90.run, exited main loop =/=\n\n";
        handle_pp
          ~coq
          ~show:false
          ~debug:true
          (Printf.sprintf
             "=/= KS90.run, map_of_states: {%s}.\n\
              s.states: %s.\n\
              t.states: %s.\n"
             (Hashtbl.fold
                (fun (state : state) (state' : state) (acc : string) ->
                   Printf.sprintf
                     "%s  original:%s -> merged:%s\n"
                     acc
                     (pstr
                        ~options:(if debug then Debug () else Default ())
                        (pp_wrap_as_supported (State state)))
                     (pstr
                        ~options:(if debug then Debug () else Default ())
                        (pp_wrap_as_supported (State state'))))
                map_of_states
                "\n")
             (pstr
                ~options:(if debug then Debug () else Default ())
                (pp_wrap_as_supported (States s.states)))
             (pstr
                ~options:(if debug then Debug () else Default ())
                (pp_wrap_as_supported (States t.states))));
        (* split [!pi] based on whether if states are bisimilar or not *)
        let (bisimilar_states, non_bisimilar_states) : Partition.t * Partition.t
          =
          Partition.fold
            (fun (block : Block.t)
              ((bisimilar_states', non_bisimilar_states') :
                Partition.t * Partition.t) ->
               (* check that another state in block is from another fsm. *)
               let block_contains_states_from_both : bool =
                 Block.for_all
                   (fun (state : state) ->
                      Block.exists
                        (fun (state' : state) ->
                           match Int.equal state.id state'.id with
                           | true -> false
                           | false ->
                             let original_state =
                               get_reverse_map_state map_of_states state
                             and original_state' =
                               get_reverse_map_state map_of_states state'
                             in
                             (* *)
                             handle_pp
                               ~coq
                               ~show:true
                               ~debug:true
                               (Printf.sprintf
                                  "%s\n%s\n"
                                  (Printf.sprintf
                                     "%s -> %s originates from s ?= %b."
                                     (pstr (pp_wrap_as_supported (State state)))
                                     (pstr
                                        (pp_wrap_as_supported
                                           (State original_state)))
                                     (States.mem original_state s.states))
                                  (Printf.sprintf
                                     "%s -> %s originates from t ?= %b."
                                     (pstr
                                        (pp_wrap_as_supported (State state')))
                                     (pstr
                                        (pp_wrap_as_supported
                                           (State original_state)))
                                     (States.mem original_state t.states)));
                             (* *)
                             (* [state] and [state'] must originate from different fsm. *)
                             (States.mem original_state s.states
                              && States.mem original_state' t.states)
                             || (States.mem original_state' s.states
                                 && States.mem original_state t.states))
                        block)
                   block
               in
               handle_pp
                 ~coq
                 ~show:true
                 ~debug:true
                 (Printf.sprintf
                    "=/= KS90.run, block_contains_states_from_both: %b.\n\
                     block: %s.\n"
                    block_contains_states_from_both
                    (pstr
                       ~options:(if debug then Debug () else Default ())
                       (pp_wrap_as_supported (Block block))));
               (* block is bisimilar if it contains states from both fsms. *)
               match block_contains_states_from_both with
               | true ->
                 Partition.add block bisimilar_states', non_bisimilar_states'
               | false ->
                 bisimilar_states', Partition.add block non_bisimilar_states')
            !pi
            (Partition.empty, Partition.empty)
        in
        let are_bisimilar = Partition.is_empty non_bisimilar_states in
        handle_pp
          ~coq
          ~show
          ~debug:true
          (Printf.sprintf
             "=/=/=/=/=/=/=\n\nKS90.run, are_bisimilar: %b\n\n=/=/=/=/=/=/=\n\n"
             are_bisimilar);
        { are_bisimilar; bisimilar_states; non_bisimilar_states }
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
                 if
                   Partition.equal
                     (Partition.inter
                        s_reachable_partition
                        t_reachable_partition)
                     (Partition.union
                        s_reachable_partition
                        t_reachable_partition)
                 then Block.add t_state b1', b2'
                 else b1', Block.add t_state b2')
               else if
                 (* no edges for [t_state] via [a] *)
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

    (* *)
    let run (s : fsm) (t : fsm) : bool * Partition.t =
      (* Printf.printf "\n[DEBUG] (a) entered run\n"; *)
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
      (* Printf.printf
         "\n[DEBUG] (b.1) initial pi: %s.\n(b.2) state map: [%s].\n"
         (Fsm.pstr (pp_wrap_as_supported (Fsm.Partition !pi)))
         (Hashtbl.fold
         (fun (old_state : state) (new_state : state) (acc : string) ->
         Printf.sprintf
         "%s\n  (old: %s -> new: %s)"
         acc
         (pstr (pp_wrap_as_supported (State old_state)))
         (pstr (pp_wrap_as_supported (State new_state))))
         map_of_states
         ""); *)
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
               Hashtbl.add
                 map
                 t_action
                 (List.nth (Alphabet.elements s_alphas) 0);
               alphabet, map))
          t_alphabet
          (s_alphabet, Alphabet.cardinal t_alphabet |> Hashtbl.create)
      in
      (* Printf.printf
         "\n[DEBUG] (c.1) merged alphabet: %s.\n(c.2) alphabet map: [%s].\n"
         (pstr (pp_wrap_as_supported (Alphabet alphabet)))
         (Hashtbl.fold
         (fun (old_alpha : action) (new_alpha : action) (acc : string) ->
         Printf.sprintf
         "%s\n  (old: (%s) -> new: (%s))"
         acc
         (pstr (pp_wrap_as_supported (Action old_alpha)))
         (pstr (pp_wrap_as_supported (Action new_alpha))))
         map_t_alphabet
         ""); *)
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
      (* Printf.printf
         "\n[DEBUG] (d) merged edges: %s.\n"
         (pstr (pp_wrap_as_supported (Edges edges))); *)
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
                  | [] ->
                    (* Printf.printf "\n[DEBUG] split returned empty list.\n"; *)
                    ()
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
                    if
                      Bool.not (Block.equal b1 !b)
                      && Bool.not (Block.equal b2 !b)
                    then (
                      (* Printf.printf "\n[DEBUG] b1 and b2 are not equal to b.\n"; *)
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
                    (* Printf.printf
                     "\nWarning: split returned more than 2 items: %s.\n"
                     (pstr
                     (pp_wrap_as_supported
                     (Partition (Partition.of_list _pi)))); *)
                    ())
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
