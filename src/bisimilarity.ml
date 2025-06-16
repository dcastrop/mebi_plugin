(* modules in this file follow the sections in Chapter 3 of
   [`Advanced Topics in Bisimulation and Coinduction`].
   (See https://doi.org/10.1017/CBO9780511792588.) *)
open Model

type fsm_pair = Fsm.t * Fsm.t
type run_params = fsm_pair * Fsm.t option
type result_kind = fsm_pair * Fsm.t * (Partition.t * Partition.t)

let get_pair_from_result (p, _, _) : fsm_pair = p

let resolve ((_, _, (_, non_bisim_states)) : result_kind) : bool =
  Partition.is_empty non_bisim_states
;;

exception CouldNotFindOriginOfState of Model_state.t

(** [block_has_shared_origin block the_states_1 the_states_2 map_of_states] is [true] if [block] contains states that originated in both [the_states_1] and [the_states_2].
    @param map_of_states
      maps states from the merged fsm to those of [the_states_2], since the merged fsm begins with [the_states_1].
    @raise CouldNotFindOriginOfState
      if a state in [block] cannot be found in either of [the_states_1] or [the_states_2].
*)
let block_has_shared_origin
      (block : States.t)
      (the_states_1 : States.t)
      (the_states_2 : States.t) (* (map_of_states : Merge.merged_state_map) *)
  : bool
  =
  let is_1_origin = ref false in
  let is_2_origin = ref false in
  States.iter
    (fun (s : Model_state.t) ->
      match Hashtbl.find_opt map_of_states s with
      | None ->
        (* no translation for [2], must be [1] *)
        if States.mem s the_states_1
        then is_1_origin := true
        else raise (CouldNotFindOriginOfState s)
      | Some s' ->
        (* found translation for [2] *)
        if States.mem s' the_states_2
        then is_2_origin := true
        else raise (CouldNotFindOriginOfState s))
    block;
  !is_1_origin && !is_2_origin
;;

(** [split_bisimilar pi the_states_1 the_states_2] separates the blocks in [pi] into two distinct partitions of bisimilar and non-bisimilar blocks.
*)
let split_bisimilar
      (pi : Partition.t)
      (the_states_1 : States.t)
      (the_states_2 : States.t) (* (map_of_states : Merge.merged_state_map) *)
  : Partition.t * Partition.t
  =
  Partition.fold
    (fun (block : States.t)
      ((bisim_states, non_bisim_states) : Partition.t * Partition.t) ->
      (* check that another state in block is from another fsm. *)
      if block_has_shared_origin block the_states_1 the_states_2 map_of_states
      then Partition.add block bisim_states, non_bisim_states
      else bisim_states, Partition.add block non_bisim_states)
    pi
    (Partition.empty, Partition.empty)
;;

let saturate_fsms (the_fsm_1 : fsm) (the_fsm_2 : fsm) : fsm_pair =
  Fsm.Saturate.fsm the_fsm_1, Fsm.Saturate.fsm the_fsm_2
;;

let handle_run_params
      (((the_fsm_1, the_fsm_2), pre_merged_fsm) : run_params)
      (weak : bool)
  : Fsm.fsm
  =
  match weak, pre_merged_fsm with
  (* strong *)
  | false, None -> Fsm.merge the_fsm_1 the_fsm_2
  | false, Some merged_fsm -> merged_fsm
  (* weak *)
  | true, None ->
    let to_merge_1, to_merge_2 = saturate_fsms the_fsm_1 the_fsm_2 in
    Fsm.merge to_merge_1 to_merge_2
  | true, Some (merged_fsm, state_translation) ->
    Utils.Logging.Log.warning
      "Checking weak bisimilarity, will re-merge FSMs after saturating them. \
       (Ignoring provided merged FSM.)";
    let the_sat_1, the_sat_2 = saturate_fsms the_fsm_1 the_fsm_2 in
    Fsm.merge the_sat_1 the_sat_2
;;

let reachable_blocks (actions : States.t Actions.t) (pi : Partition.t)
  : Partition.t option
  =
  let destinations = Fsm.Get.destinations (Actions actions) in
  if States.is_empty destinations
  then None
  else
    Some
      (Partition.filter
         (fun (block : States.t) ->
           Bool.not (States.is_empty (States.inter block destinations)))
         pi)
;;

let add_to_block_option (s : state) (block : States.t option) : States.t option =
  Some
    (match block with
     | None -> States.add s States.empty
     | Some b -> States.add s b)
;;

let split_block
      (block : States.t)
      (a : Model_action.t)
      (edges_of_a : States.t Actions.t Edges.t)
      (pi : Partition.t)
  : States.t * States.t option
  =
  assert (Bool.not (States.is_empty block));
  (* selects some state [s] from [block] *)
  let s = States.min_elt block in
  let s_actions = Fsm.Get.actions_from s edges_of_a in
  let s_reachable_blocks = reachable_blocks s_actions pi in
  (* check rest of [block] *)
  States.fold
    (fun (t : Model_state.t) ((b1, b2) : States.t * States.t option) ->
      let t_actions = Fsm.Get.actions_from t edges_of_a in
      let t_reachable_blocks = reachable_blocks t_actions pi in
      match s_reachable_blocks, t_reachable_blocks with
      | None, None -> States.add t b1, b2
      | Some s_blocks, Some t_blocks ->
        if
          Partition.equal
            (Partition.inter s_blocks t_blocks)
            (Partition.union s_blocks t_blocks)
        then States.add t b1, b2
        else b1, add_to_block_option t b2
      | _, _ -> b1, add_to_block_option t b2)
    block
    (States.empty, None)
;;

let update_if_changed : unit = ()

let iterate
      (alphabet : Alphabet.t)
      (edges : States.t Actions.t Edges.t)
      (pi : Partition.t ref)
      (changed : bool ref)
      (weak : bool)
  : unit
  =
  Partition.iter
    (fun (b : States.t) ->
      let b = ref b in
      Alphabet.iter
        (fun (a : Model_action.t) ->
          let edges_of_a = Fsm.Get.edges_of ~weak a edges in
          (* TODO: [Fsm.Saturate.fsm] doesn't clean up unreachable states/edges yet. *)
          if Edges.length edges_of_a > 0
          then (
            match split_block !b a edges_of_a !pi with
            | b1, None ->
              assert (Int.equal (States.cardinal b1) (States.cardinal !b))
            | b1, Some b2 ->
              pi := Partition.remove !b !pi;
              pi := Partition.add b2 (Partition.add b1 !pi);
              (* TODO: is find necessary? just use [b1] ? *)
              b := Partition.find b1 !pi;
              changed := true))
        alphabet)
    !pi
;;

let run ?(weak : bool = false) (r : run_params) : result_kind =
  let the_fsm_1, the_fsm_2 = fst r in
  let the_merged_fsm = handle_run_params r weak in
  match the_merged_fsm with
  | { alphabet; states; edges; _ } ->
    let pi = ref (Partition.of_list [ states ]) in
    let changed = ref true in
    while !changed do
      (* the main loop *)
      changed := false;
      iterate alphabet edges pi changed weak
    done;
    let bisim_states, non_bisim_states =
      split_bisimilar !pi the_fsm_1.states the_fsm_2.states
      (* the_state_translation *)
    in
    (the_fsm_1, the_fsm_2), the_merged_fsm, (bisim_states, non_bisim_states)
;;
