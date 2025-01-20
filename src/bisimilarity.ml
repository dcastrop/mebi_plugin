(* modules in this file follow the sections in Chapter 3 of
   [`Advanced Topics in Bisimulation and Coinduction`].
   (See https://doi.org/10.1017/CBO9780511792588.) *)

open Fsm
open Utils

(** [bisim_result] is returned by the algorithms that check for bisimilarity. *)
type bisim_result =
  { are_bisimilar : bool
  ; bisimilar_states : Partition.t
  ; non_bisimilar_states : Partition.t
  }

(** [RCP] contains algorithms for solving the
    `Relational Coarsest Partitioning` problem. *)
module RCP = struct
  (** [KS90] implements the algorithm by Kanellakis and Smolka (1990). *)
  module KS90 = struct
    exception EmptyBlock of Block.t
    exception PartitionsNotDisjoint of Partition.t

    (** [] *)
    let reachable_partitions
      ?(show : bool = false)
      ?(debug : bool = false)
      (edges : States.t Actions.t)
      (pi : Partition.t)
      : Partition.t
      =
      let destinations : States.t =
        Actions.fold
          (fun (_a : action) (destinations : States.t) (acc : States.t) ->
            States.union acc destinations)
          edges
          States.empty
      in
      print
        ~show
        (Printf.sprintf
           "/\\/\\/\\ KS90.reachable_partitions /\\/\\/\\\n\n\
           \  destinations: %s.\n\
            /\\/\\/\\/\\/\\/\\/\\/\\/\\\n\n"
           (pstr
              ~options:(pstr_options debug)
              (pp_wrap_as_supported (States destinations))));
      Partition.filter
        (fun (block : Block.t) ->
          Bool.not (Block.is_empty (Block.inter block destinations)))
        pi
    ;;

    (** []
        @return a partition that contains the split [block].
        @raise EmptyBlock if [block] is empty. *)
    let split
      ?(show : bool = false)
      ?(debug : bool = false)
      (block : Block.t)
      (a : action)
      (pi : Partition.t)
      (edges : States.t Actions.t Edges.t)
      : Block.t * Block.t
      =
      (* *)
      print
        ~show:false
        (Printf.sprintf
           "/\\/\\/\\ KS90.split /\\/\\/\\\n\n\
           \  action: %s.\n\
           \  edges: %s.\n\n\
           \  block: %s.\n\n\
           \  pi: %s.\n\n\
            /\\/\\/\\/\\/\\/\\/\\/\\/\\\n\n"
           (pstr
              ~options:(pstr_options debug)
              (pp_wrap_as_supported (Action a)))
           (pstr
              ~options:(pstr_options debug)
              ~tabs:2
              (pp_wrap_as_supported (Edges edges)))
           (pstr
              ~options:(pstr_options debug)
              ~tabs:2
              (pp_wrap_as_supported (Block block)))
           (pstr
              ~options:(pstr_options debug)
              ~tabs:2
              (pp_wrap_as_supported (Partition pi))));
      (* *)
      let _block (* list for pattern matching *) = Block.to_list block in
      match _block with
      | [] -> raise (EmptyBlock block)
      | (s : state) :: _block' ->
        let s_edges = get_outgoing_actions edges s a in
        let s_reachable_partitions =
          reachable_partitions ~show ~debug s_edges pi
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
                reachable_partitions ~show ~debug t_edges pi
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
        @param ?coq
          determines whether [Pp.Feedback.msg_info] or [Printf.printf] is used for output.
        @param s is an [fsm] to check.
        @param t is an [fsm] to check. *)
    let run ?(show : bool = false) ?(debug : bool = true) (s : fsm) (t : fsm)
      : bisim_result
      =
      (* *)
      print ~show "\n\n=/=/=/= KS90.run =/=/=/=\n\n";
      (* get initial partition [pi] by merging states from [s] and [t] into single set. *)
      let merged_fsm, _map_of_alphabet, map_of_states = Fsm.merge_fsm s t in
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
                  print
                    ~show
                    (Printf.sprintf
                       "- - - next iteration - - -\n\n\
                       \  action: %s.\n\
                       \  block: %s.\n\n\
                       \  pi: %s.\n\n\
                        - - - - - - - - -\n\n"
                       (pstr
                          ~options:(pstr_options debug)
                          (pp_wrap_as_supported (Action a)))
                       (pstr
                          ~options:(pstr_options debug)
                          ~tabs:1
                          (pp_wrap_as_supported (Block _b)))
                       (pstr
                          ~options:(pstr_options debug)
                          ~tabs:1
                          (pp_wrap_as_supported (Partition !pi))));
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
                  let b1, b2 = split ~show ~debug !b a !pi edges_of_a in
                  match Block.is_empty b1, Block.is_empty b2 with
                  | true, true ->
                    (* both are empty, this is not supposed to happen *)
                    print
                      ~show:false
                      (Printf.sprintf "split returned two empty blocks.\n\n");
                    ()
                  | false, true ->
                    (* empty [b2] means that split did not occur *)
                    assert (Block.equal b1 !b);
                    print
                      ~show:false
                      (Printf.sprintf
                         "split returned empty b2.\nb1: %s.\n\n"
                         (pstr (pp_wrap_as_supported (Block b1))));
                    ()
                  | _, _ ->
                    (* split did occur, so replace [b] with [b1] and [b2] and refine *)
                    assert (Bool.not (Block.is_empty b1));
                    print
                      ~show:false
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
        print ~show "=/= KS90.run, exited main loop =/=\n\n";
        print
          ~show:false
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
                       ~options:(pstr_options debug)
                       (pp_wrap_as_supported (State state)))
                    (pstr
                       ~options:(pstr_options debug)
                       (pp_wrap_as_supported (State state'))))
                map_of_states
                "\n")
             (pstr
                ~options:(pstr_options debug)
                (pp_wrap_as_supported (States s.states)))
             (pstr
                ~options:(pstr_options debug)
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
                          print
                            ~show:false
                            (Printf.sprintf
                               "%s\n%s\n"
                               (Printf.sprintf
                                  "%s -> %s originates from s ?= %b."
                                  (pstr
                                     ~options:(pstr_options debug)
                                     (pp_wrap_as_supported (State state)))
                                  (pstr
                                     ~options:(pstr_options debug)
                                     (pp_wrap_as_supported
                                        (State original_state)))
                                  (States.mem original_state s.states))
                               (Printf.sprintf
                                  "%s -> %s originates from t ?= %b."
                                  (pstr
                                     ~options:(pstr_options debug)
                                     (pp_wrap_as_supported (State state')))
                                  (pstr
                                     ~options:(pstr_options debug)
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
              print
                ~show:false
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
        print
          ~show:false
          (Printf.sprintf
             "=/=/=/=/=/=/=\n\nKS90.run, are_bisimilar: %b\n\n=/=/=/=/=/=/=\n\n"
             are_bisimilar);
        { are_bisimilar; bisimilar_states; non_bisimilar_states }
    ;;
  end

  (** [PT87] follows algorithm by Paige and Tarjan,
      improving upon [KS90]. *)
  module PT87 = struct
    (* TODO: *)
  end
end
