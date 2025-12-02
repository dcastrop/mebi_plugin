open Model

module Minimize = struct
  type result = Fsm.t * Partition.t

  let add_to_block_option (s : State.t) (block : States.t option)
    : States.t option
    =
    Logging.Log.trace "Algorithms.Minimize.add_to_block_option";
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
    Logging.Log.trace "Algorithms.Minimize.split_block";
    assert (Bool.not (States.is_empty block));
    let edges_of_a : States.t Actions.t Edges.t =
      Model.get_edges_labelled l edges
    in
    (* NOTE: selects some state [s] from [block] *)
    let s : State.t = States.min_elt block in
    let s_reachable_blocks : Partition.t option =
      Model.get_reachable_blocks_opt pi edges_of_a s
    in
    (* NOTE: check rest of [block] *)
    States.fold
      (fun (t : State.t) ((b1, b2) : States.t * States.t option) ->
        if State.equal s t
        then States.add s b1, b2
        else (
          let t_reachable_blocks : Partition.t option =
            Model.get_reachable_blocks_opt pi edges_of_a t
          in
          match s_reachable_blocks, t_reachable_blocks with
          | None, None -> States.add t b1, b2
          | Some s_blocks, Some t_blocks ->
            if
              Partition.equal
                (Partition.inter s_blocks t_blocks)
                (Partition.union s_blocks t_blocks)
            then States.add t b1, b2
            else b1, add_to_block_option t b2
          | _, _ -> b1, add_to_block_option t b2))
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
    Logging.Log.trace "Algorithms.Minimize.iterate";
    Partition.iter
      (fun (b : States.t) ->
        let b = ref b in
        Alphabet.iter
          (fun (l : Label.t) ->
            match split_block !b l edges !pi with
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
    Logging.Log.trace "Algorithms.Minimize.run";
    let the_fsm : Fsm.t = if weak then Saturate.fsm the_fsm else the_fsm in
    match the_fsm with
    | { alphabet; states; edges; _ } ->
      let pi = ref (Partition.of_list [ states ]) in
      let changed = ref true in
      while !changed do
        (* NOTE: the main loop *)
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
  type result =
    { the_fsm_1 : fsm_pair
    ; the_fsm_2 : fsm_pair
    ; merged_fsm : Fsm.t
    ; bisim_states : Partition.t
    ; non_bisim_states : Partition.t
    }

  and fsm_pair =
    { original : Fsm.t
    ; saturated : Fsm.t
    }

  let the_cached_result : result option ref = ref None

  let set_the_result (new_result : result) : unit =
    the_cached_result := Some new_result
  ;;

  exception MeBi_Bisim_ResultNotCached of unit

  let get_the_result () : result =
    match !the_cached_result with
    | None -> raise (MeBi_Bisim_ResultNotCached ())
    | Some result -> result
  ;;

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
        (* NOTE: check that another state in block is from another fsm. *)
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
    { the_fsm_1 = { original = fst the_fsm_pair; saturated = the_sat_1 }
    ; the_fsm_2 = { original = snd the_fsm_pair; saturated = the_sat_2 }
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
      "Bisimilar:\n%b\nBisimilar states:\n%s\nNon-bisimilar states:\n%s\n"
      (Partition.is_empty r.non_bisim_states)
      (Model.partition_to_string r.bisim_states)
      (Model.partition_to_string r.non_bisim_states)
  ;;
end
