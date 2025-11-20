open Logging
open Model

module Minimize = struct
  open Model

  type result = Fsm.t * Partition.t

  let add_to_block_option (s : State.t) (block : States.t option)
    : States.t option
    =
    Some
      (match block with
       | None -> States.add s States.empty
       | Some b -> States.add s b)
  ;;

  let split_block
        (block : States.t)
        (l : Label.t)
        (edges : States.t Actions.t Edges.t)
        (pi : Partition.t)
    : States.t * States.t option
    =
    assert (Bool.not (States.is_empty block));
    let edges_of_a = Model.get_edges_with_label l edges in
    Log.debug
      (Printf.sprintf
         "algorithms.Minimize.split_block, (%i) edges of (%s)."
         (Edges.length edges_of_a)
         (Label.to_string l));
    (* selects some state [s] from [block] *)
    let s = States.min_elt block in
    let s_actions = Model.get_actions_from s edges_of_a in
    let s_reachable_blocks = Model.get_reachable_blocks_opt s_actions pi in
    (* check rest of [block] *)
    States.fold
      (fun (t : State.t) ((b1, b2) : States.t * States.t option) ->
        if State.equal s t
        then States.add s b1, b2
        else (
          let t_actions = Model.get_actions_from t edges_of_a in
          let t_reachable_blocks =
            Model.get_reachable_blocks_opt t_actions pi
          in
          match s_reachable_blocks, t_reachable_blocks with
          | None, None ->
            Log.debug
              (Printf.sprintf
                 "algorithms.Minimize.split_block (%s): (%s, None), (%s, None)"
                 (Label.to_string l)
                 (State.to_string s)
                 (State.to_string t));
            States.add t b1, b2
          | Some s_blocks, Some t_blocks ->
            Log.debug
              (Printf.sprintf
                 "algorithms.Minimize.split_block (%s): (%s, Some), (%s, Some)"
                 (Label.to_string l)
                 (State.to_string s)
                 (State.to_string t));
            if
              Partition.equal
                (Partition.inter s_blocks t_blocks)
                (Partition.union s_blocks t_blocks)
            then States.add t b1, b2
            else b1, add_to_block_option t b2
          | _, _ ->
            Log.debug
              (Printf.sprintf
                 "algorithms.Minimize.split_block (%s): (%s, _), (%s, _)"
                 (Label.to_string l)
                 (State.to_string s)
                 (State.to_string t));
            b1, add_to_block_option t b2))
      block
      (States.empty, None)
  ;;

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
          (fun (l : Label.t) ->
            (* let edges_of_a = Model.get_edges_with_label l edges in
               (* TODO: [Fsm.Saturate.fsm] doesn't clean up unreachable states/edges yet. *)
               if Edges.length edges_of_a > 0
               then ( *)
            match split_block !b (*edges_of_a*) l edges !pi with
            | b1, None ->
              assert (Int.equal (States.cardinal b1) (States.cardinal !b))
            | b1, Some b2 ->
              pi := Partition.remove !b !pi;
              pi := Partition.add b2 (Partition.add b1 !pi);
              (* TODO: is find necessary? just use [b1] ? *)
              b := Partition.find b1 !pi;
              changed := true)
          alphabet)
      !pi
  ;;

  let run ?(weak : bool = false) (the_fsm : Fsm.t) : result =
    let the_fsm : Fsm.t = if weak then Saturate.fsm the_fsm else the_fsm in
    match the_fsm with
    | { alphabet; states; edges; _ } ->
      let pi = ref (Partition.of_list [ states ]) in
      let changed = ref true in
      while !changed do
        (* the main loop *)
        changed := false;
        iterate alphabet edges pi changed weak
      done;
      the_fsm, !pi
  ;;

  let to_string : result -> string = function
    | the_fsm, the_partition ->
      Printf.sprintf
        "\nMinimized FSM: %s\nMinimized Partition: %s"
        (Fsm.to_string the_fsm)
        (Model.partition_to_string the_partition)
  ;;
end

module Bisimilar = struct
  open Model

  type result =
    { the_fsm_1 : Fsm.t
    ; the_fsm_2 : Fsm.t
    ; merged_fsm : Fsm.t
    ; bisim_states : Partition.t
    ; non_bisim_states : Partition.t
    }

  (** is [true] if [block] has states that originate from both [the_fsm_1] and [the_fsm_2]
  *)
  let block_has_shared_origin
        (block : States.t)
        (the_fsm_1 : Fsm.t)
        (the_fsm_2 : Fsm.t)
    : bool
    =
    let ref_1, ref_2 = ref false, ref false in
    States.iter
      (fun (s : State.t) ->
        match Fsm.origin_of_state s the_fsm_1 the_fsm_2 with
        | 0 ->
          ref_1 := true;
          ref_2 := true
        | n -> if n < 0 then ref_1 := true else ref_2 := true)
      block;
    !ref_1 && !ref_2
  ;;

  (** [split_bisimilar pi the_states_1 the_states_2] separates the blocks in [pi] into two distinct partitions of bisimilar and non-bisimilar blocks.
  *)
  let split_bisimilar (pi : Partition.t) (the_fsm_1 : Fsm.t) (the_fsm_2 : Fsm.t)
    : Partition.t * Partition.t
    =
    Partition.fold
      (fun (block : States.t)
        ((bisim_states, non_bisim_states) : Partition.t * Partition.t) ->
        (* check that another state in block is from another fsm. *)
        if block_has_shared_origin block the_fsm_1 the_fsm_2
        then Partition.add block bisim_states, non_bisim_states
        else bisim_states, Partition.add block non_bisim_states)
      pi
      (Partition.empty, Partition.empty)
  ;;

  let run ?(weak : bool = false) (the_fsm_pair : Fsm.pair) : result =
    let the_sat_1 : Fsm.t = Saturate.fsm (fst the_fsm_pair) in
    let the_sat_2 : Fsm.t = Saturate.fsm (snd the_fsm_pair) in
    let merged_fsm : Fsm.t = Fsm.merge the_sat_1 the_sat_2 in
    let pi : Partition.t = snd (Minimize.run merged_fsm) in
    let (bisim_states, non_bisim_states) : Partition.t * Partition.t =
      split_bisimilar pi (fst the_fsm_pair) (snd the_fsm_pair)
    in
    { the_fsm_1 = fst the_fsm_pair
    ; the_fsm_2 = snd the_fsm_pair
    ; merged_fsm
    ; bisim_states
    ; non_bisim_states
    }
  ;;

  let result_to_bool (r : result) : bool =
    Model.Partition.is_empty r.non_bisim_states
  ;;

  let to_string (r : result) : string =
    Printf.sprintf
      "\nBisimilar: %b\nBisimilar states: %s\nNon-bisimilar states: %s\n"
      (Partition.is_empty r.non_bisim_states)
      (Model.partition_to_string r.bisim_states)
      (Model.partition_to_string r.non_bisim_states)
  ;;
end
