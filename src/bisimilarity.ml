(* modules in this file follow the sections in Chapter 3 of
   [`Advanced Topics in Bisimulation and Coinduction`].
   (See https://doi.org/10.1017/CBO9780511792588.) *)

open Fsm
open Utils.Logging
open Utils.Formatting
open Utils

(** [bisim_result] is returned by the algorithms that check for bisimilarity. *)
type bisim_result =
  { are_bisimilar : bool
  ; merged_fsm : fsm
  ; bisimilar_states : Partition.t
  ; non_bisimilar_states : Partition.t
  }

let default_params : Params.log = Params.Default.log ~mode:(Coq ()) ()

(** [RCP] contains algorithms for solving the
    `Relational Coarsest Partitioning` problem. *)
module RCP = struct
  (** [KS90] implements the algorithm by Kanellakis and Smolka (1990). *)
  module KS90 = struct
    exception EmptyBlock of Block.t
    exception PartitionsNotDisjoint of Partition.t

    (** [reachable_blocks edges pi] is the subset of partition [pi] containing blocks that contain the destination states of the [actions].
        @return
          a partition that contains all of the blocks within [pi] that contain a destination state of [actions].
        @param ?params contains parameters for logging and formatting.
        @param edges is the map from actions to destinations states.
        @param pi
          is the partition of blocks from which we extract and return a subset from. *)
    let reachable_blocks
      ?(params : Params.log = default_params)
      (actions : States.t Actions.t)
      (pi : Partition.t)
      : Partition.t
      =
      let destinations : States.t = get_all_destinations (Actions actions) in
      match States.is_empty destinations with
      | true -> Partition.empty
      | false ->
        (* remove any empty blocks *)
        Partition.filter
          (fun (block : Block.t) ->
            Bool.not (Block.is_empty (Block.inter block destinations)))
          pi
    ;;

    let reach_same_blocks
      (s_reachable_blocks : Partition.t)
      (t_reachable_blocks : Partition.t)
      : bool
      =
      Partition.equal
        (Partition.inter s_reachable_blocks t_reachable_blocks)
        (Partition.union s_reachable_blocks t_reachable_blocks)
    ;;

    let lengths_gtr_0
      (s_actions : States.t Actions.t)
      (t_actions : States.t Actions.t)
      : bool * bool
      =
      Actions.length s_actions > 0, Actions.length t_actions > 0
    ;;

    (** [split block a pi edges] splits [block] if the states within can reach different blocks within [pi] via action [a].
        @return a partition containing a (potentially) split [block].
        @param ?params contains parameters for logging and formatting.
        @param block is the block to split.
        @param a is the action to check for splitting.
        @param pi is the partition of all blocks.
        @param edges is the map from states to actions to destination states.
        @raise EmptyBlock if [block] is empty. *)
    let split
      ?(params : Params.log = default_params)
      (block : Block.t)
      (a : action)
      (pi : Partition.t)
      (edges : States.t Actions.t Edges.t)
      : Block.t * Block.t
      =
      if Block.is_empty block
      then raise (EmptyBlock block)
      else (
        (* select some state [s] from block *)
        let s : state = Block.min_elt block in
        let s_actions : States.t Actions.t = get_actions_from s edges in
        let s_reachable_blocks : Partition.t =
          reachable_blocks ~params s_actions pi
        in
        Block.fold
          (fun (t : state) ((b1, b2) : Block.t * Block.t) ->
            let t_actions : States.t Actions.t = get_actions_from t edges in
            (* check if to add to [b1] or [b2]. *)
            match lengths_gtr_0 s_actions t_actions with
            (* neither [s] or [t] have action [a]. *)
            | false, false -> Block.add t b1, b2
            (* both [s] and [t] have action [a]. *)
            | true, true ->
              let t_reachable_blocks = reachable_blocks ~params t_actions pi in
              if reach_same_blocks s_reachable_blocks t_reachable_blocks
              then Block.add t b1, b2
              else b1, Block.add t b2
            (* only one of [s] or [t] has action [a]. *)
            | _, _ -> b1, Block.add t b2)
          block
          (Block.empty, Block.empty))
    ;;

    (** [main_loop (alphabet,edges) pi changed] is the main loop of the [KS90] algorithm.
        @param ?params contains parameters for logging and formatting.
        @param (alphabet,edges)
          are the merged alphabets and edges of the systems being checked.
        @param pi is the partition contianing all states.
        @param changed
          is used to denote whether a refinement has occured at all in the current iteration. *)
    let main_loop
      ?(params : Params.log = default_params)
      ((alphabet, edges) : Alphabet.t * States.t Actions.t Edges.t)
      (pi : Partition.t ref)
      (changed : bool ref)
      : unit
      =
      Partition.iter
        (fun (_b : Block.t) : unit ->
          let b = ref _b in
          Alphabet.iter
            (fun (a : action) : unit ->
              (* *)
              let edges_of_a = get_edges_of a edges in
              (* *)
              let b1, b2 = split ~params !b a !pi edges_of_a in
              match Block.is_empty b1, Block.is_empty b2 with
              | true, true ->
                (* both are empty, this is not supposed to happen *)
                params.kind <- Debug ();
                log ~params "split returned two empty blocks.\n\n";
                ()
              | false, true ->
                (* empty [b2] means that split did not occur *)
                assert (Block.equal b1 !b);
                params.kind <- Debug ();
                log
                  ~params
                  (Printf.sprintf
                     "split returned empty b2.\nb1: %s.\n\n"
                     (PStr.states ~params:(Log params) b1));
                ()
              | _, _ ->
                (* split did occur, so replace [b] with [b1] and [b2] and refine *)
                assert (Bool.not (Block.is_empty b1));
                params.kind <- Debug ();
                log
                  ~params
                  (Printf.sprintf
                     "split returned two blocks.\nb1: %s.\nb2: %s.\n\n"
                     (PStr.states ~params:(Log params) b1)
                     (PStr.states ~params:(Log params) b2));
                pi := Partition.remove !b !pi;
                pi := Partition.union !pi (Partition.of_list [ b1; b2 ]);
                changed := true;
                ())
            alphabet)
        !pi
    ;;

    type state_origins =
      { s : bool
      ; t : bool
      }

    let origins_of
      ?(params : Params.log = default_params)
      (block : Block.t)
      ((s_states, t_states) : States.t * States.t)
      (map_of_states : (state, state) Hashtbl.t)
      : state_origins
      =
      Block.fold
        (fun (s : state) (origins : state_origins) ->
          match Hashtbl.find_opt map_of_states s with
          | None -> { s = States.mem s s_states || origins.s; t = origins.t }
          | Some s' ->
            { s = origins.s; t = States.mem s' t_states || origins.t })
        block
        { s = false; t = false }
    ;;

    (** [split_bisimilar map_of_states (s_states,t_states) pi] is ...
        @return
          a pair of partitions containing the bisimilar and non-bisimilar states.
        @param ?params contains parameters for logging and formatting.
        @param (s_states,t_states)
          are the states of the (pre-merged) fsms being checked.
        @param map_of_states
          maps the original (pre-merged) states to the post-merged states.
        @param pi is the partition containing all blocks of states. *)
    let split_bisimilar
      ?(params : Params.log = default_params)
      ((s_states, t_states) : States.t * States.t)
      (map_of_states : (state, state) Hashtbl.t)
      (pi : Partition.t)
      : Partition.t * Partition.t
      =
      Partition.fold
        (fun (block : Block.t)
          ((bisimilar_states', non_bisimilar_states') :
            Partition.t * Partition.t) ->
          (* check that another state in block is from another fsm. *)
          let origins : state_origins =
            origins_of ~params block (s_states, t_states) map_of_states
          in
          (* block is bisimilar if it contains states from both fsms. *)
          if origins.s && origins.t
          then Partition.add block bisimilar_states', non_bisimilar_states'
          else bisimilar_states', Partition.add block non_bisimilar_states')
        pi
        (Partition.empty, Partition.empty)
    ;;

    (** [run ?coq s t] algorithmically checks if [s] and [t] are bisimilar, returning a [bisim_result] with further details.
        @return
          [bisim_result] containing the bisimilar and non-bisimilar states.
        @param ?params contains parameters for logging and formatting.
        @param s is an [fsm] to check.
        @param t is an [fsm] to check. *)
    let run ?(params : Params.log = default_params) (s : fsm) (t : fsm)
      : bisim_result
      =
      (* *)
      params.kind <- Debug ();
      params.options.show_debug_output <- true;
      log ~params "=/=/=/= KS90.run =/=/=/=\n\n";
      (* get initial partition [pi] by merging states from [s] and [t] into single set. *)
      let merged_fsm, map_of_states = Merge.fsms s t in
      (* *)
      match merged_fsm with
      | { alphabet; states; edges; _ } ->
        (* working partition is initially all the states *)
        let pi = ref (Partition.of_list [ states ])
        and changed = ref true in
        while !changed do
          changed := false;
          (* [main_loop] checks whether each block in partition [pi] can be further refined,
             depending on whether each state within the block can reach a different set of
             other blocks within [pi] via any of the available actions in [alphabet].
             [main_loop] continues so long as refinement occurs.
             If there is no change then the loop ends. *)
          main_loop ~params (alphabet, edges) pi changed
        done;
        (* *)
        log ~params "=/= KS90.run, exited main loop =/=\n\n";
        (* split [!pi] based on whether if states are bisimilar or not *)
        let (bisimilar_states, non_bisimilar_states) : Partition.t * Partition.t
          =
          split_bisimilar ~params (s.states, t.states) map_of_states !pi
        in
        let are_bisimilar = Partition.is_empty non_bisimilar_states in
        log
          ~params
          (Printf.sprintf
             "=/=/=/=/=/=/=\n\nKS90.run, are_bisimilar: %b\n\n=/=/=/=/=/=/=\n\n"
             are_bisimilar);
        { are_bisimilar; merged_fsm; bisimilar_states; non_bisimilar_states }
    ;;
  end

  (** [PT87] follows algorithm by Paige and Tarjan,
      improving upon [KS90]. *)
  module PT87 = struct
    (* TODO: *)
  end
end
