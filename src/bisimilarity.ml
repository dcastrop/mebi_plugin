(* modules in this file follow the sections in Chapter 3 of
   [`Advanced Topics in Bisimulation and Coinduction`].
   (See https://doi.org/10.1017/CBO9780511792588.) *)

open Fsm
open Utils.Logging
open Utils.Formatting
open Utils

(* TODO: weak bisimilarity.
   NOTES on extending for weak bisimilarity:
   - define [Fsm.Tau] as the silent transition
   - redefine [Fsm.action] to be either a silent transition, or a labelled transition
   - do not add [Fsm.Tau] to [Fsm.Alphabet]
   - update [Fsm.Alphabet] to be of [Fsm.label] for labelled transitions
   - extend [Fsm.get_edges_of] to ignore silent transitions (configurable)
   - 'closure of tau actions'
   - will be useful when proving in coq
   - to be created during bisim
   - 'if S does a then T does tau tau tau a'
   - where we need to annotate the tau actions,
     to keep track of the path through states
*)

(** [bisim_result] is returned by the algorithms that check for bisimilarity. *)
type bisim_result =
  { are_bisimilar : bool
  ; merged_fsm : fsm
  ; bisimilar_states : Partition.t
  ; non_bisimilar_states : Partition.t
  }

type minim_result = Partition.t

type of_bisim_result =
  | OfMerged of
      ((fsm * fsm) * (fsm * (state, state) Hashtbl.t) * Partition.t ref)
  | OfMinimized of minim_result ref

type result =
  | BisimResult of bisim_result
  | MinimResult of minim_result

let default_params : Params.log = Params.Default.log ~mode:(Coq ()) ()

module PStr = struct
  let bisim_result
    ?(params : Params.log = default_params)
    ?(merged_from : (fsm * fsm) option)
    (to_pstr : bisim_result)
    : string
    =
    match to_pstr with
    | { are_bisimilar; merged_fsm; bisimilar_states; non_bisimilar_states; _ }
      ->
      Printf.sprintf
        "Are Bisimilar: %b.\n\n\
         Bisimilar states: %s.\n\n\
         Non-bisimilar states: %s.\n\n\
         Using merged fsm: %s.\n\
         %s\n"
        are_bisimilar
        (Fsm.PStr.partition ~params:(Log params) bisimilar_states)
        (Fsm.PStr.partition ~params:(Log params) non_bisimilar_states)
        (Fsm.PStr.fsm ~params:(Log params) merged_fsm)
        (match merged_from with
         | None -> ""
         | Some (s, t) ->
           Printf.sprintf
             "\nObtained from FSM s: %s\n\nand FSM t: %s.\n"
             (Fsm.PStr.fsm ~params:(Log params) s)
             (Fsm.PStr.fsm ~params:(Log params) t))
  ;;
end

(** [RCP] contains algorithms for solving the
    `Relational Coarsest Partitioning` problem. *)
module RCP = struct
  (** [KS90] implements the algorithm by Kanellakis and Smolka (1990). *)
  module KS90 = struct
    exception EmptyBlock of Block.t
    exception PartitionsNotDisjoint of Partition.t

    (** [reachable_blocks ?params actions pi] is the subset of partition [pi] containing blocks that contain the destination states of the [actions].
        @param ?params contains parameters for logging and formatting.
        @param actions is the map from actions to destinations states.
        @param pi
          is the partition of blocks from which we extract and return a subset from.
        @return
          a partition that contains all of the blocks within [pi] that contain a destination state of [actions]. *)
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

    (** [reach_same_blocks s t] *)
    let reach_same_blocks
      (s_reachable_blocks : Partition.t)
      (t_reachable_blocks : Partition.t)
      : bool
      =
      Partition.equal
        (Partition.inter s_reachable_blocks t_reachable_blocks)
        (Partition.union s_reachable_blocks t_reachable_blocks)
    ;;

    (** [lengths_gtr_0 s t] is used to compare if the actions of two states share any actions. *)
    let lengths_gtr_0
      (s_actions : States.t Actions.t)
      (t_actions : States.t Actions.t)
      : bool * bool
      =
      Actions.length s_actions > 0, Actions.length t_actions > 0
    ;;

    (** [split block a pi edges] splits [block] if the states within can reach different blocks within [pi] via action [a].
        @param ?params contains parameters for logging and formatting.
        @param block is the block to split.
        @param a is the action to check for splitting.
        @param pi is the partition of all blocks.
        @param edges is the map from states to actions to destination states.
        @return a partition containing a (potentially) split [block].
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

    (** [inner_loop ?params b1 b2 b pi changed] is the main inner loop of the [KS90] algorithm. It handles the result of [split], and determines if the split produced a refinement of [pi].
        @param ?params contains parameters for logging and formatting.
        @param b1
        @param b2
        @param b is the current block within [pi] being sorted (via [split]).
        @param pi
          is the partition containing all states to be grouped by the algorithm with those they are bisimilar with..
        @param changed
          is a referenced used to signify if [pi] has changed, and if the algorithm should terminate. *)
    let inner_loop
      ?(params : Params.log = default_params)
      (b1 : Block.t)
      (b2 : Block.t)
      (b : Block.t ref)
      (pi : Partition.t ref)
      (changed : bool ref)
      : unit
      =
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
             (Fsm.PStr.states ~params:(Log params) b1));
        ()
      | _, _ ->
        (* split did occur, so replace [b] with [b1] and [b2] and refine *)
        assert (Bool.not (Block.is_empty b1));
        params.kind <- Debug ();
        log
          ~params
          (Printf.sprintf
             "split returned two blocks.\nb1: %s.\nb2: %s.\n\n"
             (Fsm.PStr.states ~params:(Log params) b1)
             (Fsm.PStr.states ~params:(Log params) b2));
        pi := Partition.remove !b !pi;
        pi := Partition.union !pi (Partition.of_list [ b1; b2 ]);
        changed := true;
        ()
    ;;

    (** [main_loop_body ?params (alphabet,edges) pi changed] is the main outer loop of the [KS90] algorithm. It handles iterating through each block in [pi], and trying to split the block by finding an action not shared by all (via [split]), the result of which is handled by [inner_loop].
        @param ?params contains parameters for logging and formatting.
        @param (alphabet,edges)
          are the (merged) alphabet and edges of the (merged) system being checked.
        @param pi is the partition contianing all states.
        @param changed
          is used to denote whether a refinement has occured at all in the current iteration.
        @return
          unit (nothing), since [pi] (and [changed]) are references that are updated. *)
    let main_loop_body
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
              let edges_of_a = get_edges_of a edges in
              let b1, b2 = split ~params !b a !pi edges_of_a in
              inner_loop b1 b2 b pi changed)
            alphabet)
        !pi
    ;;

    (** [run_main_loop ?params alphabet edges pi] calls [main_loop_body] until [pi] is unchanged.
        @parm ?params contains parameters for logging and formatting.
        @param alphabet is the (merged) alphabet of the (merged) fsm.
        @param edges are the (merged) edge sof the (merged) fsm.
        @param pi is the partition of states (a set of set of states).
        @return unit (nothing), since [pi] is a reference that is updated. *)
    let run_main_loop
      ?(params : Params.log = default_params)
      (alphabet : Alphabet.t)
      (edges : States.t Actions.t Edges.t)
      (pi : Partition.t ref)
      : unit
      =
      let changed = ref true in
      while !changed do
        changed := false;
        main_loop_body ~params (alphabet, edges) pi changed
      done
    ;;

    type of_weak_bisim_input =
      | ()
      | Weak

    type of_bisim_input =
      | ToMerge of (fsm * fsm)
      | Merged of (fsm * fsm * fsm * (state, state) Hashtbl.t)
      | Minimize of fsm

    exception RunInputNotExpected of of_bisim_input

    (** [run ?params input] handles running the alogrithm for either term Minimization or checking Bisimilarity.
        @param ?params contains parameters for logging and formatting.
        @param input
          is [of_bisim_input], which facilitates Minimization (via [Minimize]) or Bisimilarity (via [ToMerge] or [Merged]).
        @return
          [of_bisim_result], either for [Minimize] or [Bisimilarity], which must be futher processed via [result].*)
    let run
      ?(params : Params.log = default_params)
      (input : of_bisim_input)
      (weak : of_weak_bisim_input)
      : of_bisim_result
      =
      (* run the algorithm for different usecases *)
      match input with
      | Minimize the_fsm ->
        (match the_fsm with
         | { alphabet; states; edges; _ } ->
           let pi = ref (Partition.of_list [ states ]) in
           run_main_loop ~params alphabet edges pi;
           OfMinimized pi)
      | _ ->
        (* check for bisimilarity *)
        let s, t, merged_fsm, map_of_states =
          match input with
          | ToMerge (s, t) ->
            let merged_fsm, map_of_states = Merge.fsms s t in
            s, t, merged_fsm, map_of_states
          | Merged (s, t, merged_fsm, map_of_states) ->
            s, t, merged_fsm, map_of_states
          | _ -> raise (RunInputNotExpected input)
        in
        (* check if weak-bisimilarity *)
        (* (match input with
           | _ _ Weak ->""
           | _ _ () -> ""); *)
        (match merged_fsm with
         | { alphabet; states; edges; _ } ->
           let pi = ref (Partition.of_list [ states ]) in
           run_main_loop ~params alphabet edges pi;
           OfMerged ((s, t), (merged_fsm, map_of_states), pi))
    ;;

    type state_origins =
      { s : bool
      ; t : bool
      }

    (** [origins_of ?params block (s_states,t_states) map_of_states] ... *)
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

    (** [split_bisimilar ?params (s_states,t_states) map_of_states pi] is ...
        @param ?params contains parameters for logging and formatting.
        @param (s_states,t_states)
          are the states of the (pre-merged) fsms being checked.
        @param map_of_states
          maps the original (pre-merged) states to the post-merged states.
        @param pi is the partition containing all blocks of states.
        @return
          a pair of partitions containing the bisimilar and non-bisimilar states. *)
    let split_bisimilar
      ?(params : Params.log = default_params)
      ((s_states, t_states) : States.t * States.t)
      (map_of_states : (state, state) Hashtbl.t)
      (pi : Partition.t ref)
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
        !pi
        (Partition.empty, Partition.empty)
    ;;

    (** [result ?params res] ... *)
    let result ?(params : Params.log = default_params) (res : of_bisim_result)
      : result
      =
      match res with
      | OfMerged ((s, t), (merged_fsm, map_of_states), pi) ->
        let (bisimilar_states, non_bisimilar_states) : Partition.t * Partition.t
          =
          split_bisimilar ~params (s.states, t.states) map_of_states pi
        in
        let are_bisimilar = Partition.is_empty non_bisimilar_states in
        BisimResult
          { are_bisimilar; merged_fsm; bisimilar_states; non_bisimilar_states }
      | OfMinimized pi -> MinimResult !pi
    ;;
  end

  (** [PT87] follows algorithm by Paige and Tarjan,
      improving upon [KS90]. *)
  module PT87 = struct
    (* TODO: *)
  end
end
