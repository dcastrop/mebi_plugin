open Model

(***********************************************************************)
module Log : Logger.LOGGER_TYPE = Logger.MkDefault ()

let () = Log.Config.configure_output Debug false
let () = Log.Config.configure_output Trace false
(***********************************************************************)

let prepare_fsm (weak : bool) (fsm : Fsm.t) : Fsm.t =
  Log.trace __FUNCTION__;
  if weak then Saturate_model.fsm fsm else fsm
;;

module Minimize = struct
  type result = Fsm.t * Partition.t

  let add_to_block_option (s : State.t) (block : States.t option)
    : States.t option
    =
    Log.trace __FUNCTION__;
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
    Log.trace __FUNCTION__;
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

  let _handle_split_block (b : States.t ref) (pi : Partition.t ref)
    : States.t * States.t option -> unit
    =
    Log.trace __FUNCTION__;
    function
    | b1, None -> assert (Int.equal (States.cardinal b1) (States.cardinal !b))
    | b1, Some b2 ->
      pi := Partition.remove !b !pi;
      pi := Partition.add b2 (Partition.add b1 !pi);
      (* TODO: is find necessary? just use [b1] ? *)
      b := Partition.find b1 !pi
  ;;

  let iterate
        (alphabet : Alphabet.t)
        (edges : States.t Actions.t Edges.t)
        (pi : Partition.t ref)
        (changed : bool ref)
        (weak : bool)
    : unit
    =
    Log.trace __FUNCTION__;
    Partition.iter
      (fun (b : States.t) ->
        let b : States.t ref = ref b in
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
    Log.trace __FUNCTION__;
    Log.thing ~__FUNCTION__ Debug "weak" weak (Args Utils.Strfy.bool);
    (* NOTE: pre-saturated if weak mode *)
    let the_fsm : Fsm.t = prepare_fsm weak the_fsm in
    Log.trace ~__FUNCTION__ "finished preparing fsms (saturated if weak)";
    let { alphabet; states; edges; _ } : Fsm.t = the_fsm in
    let pi : Partition.t ref = ref (Partition.of_list [ states ]) in
    (* NOTE: main loop *)
    Log.trace ~__FUNCTION__ "entering main loop";
    let changed : bool ref = ref true in
    while !changed do
      changed := false;
      iterate alphabet edges pi changed weak
    done;
    Log.trace ~__FUNCTION__ "exited main loop";
    the_fsm, !pi
  ;;

  let to_string ((the_fsm, the_partition) : result) : string =
    Log.trace __FUNCTION__;
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

  let the_cached_result : result option ref =
    Log.trace __FUNCTION__;
    ref None
  ;;

  let set_the_result (new_result : result) : unit =
    Log.trace __FUNCTION__;
    the_cached_result := Some new_result
  ;;

  exception MeBi_Bisim_ResultNotCached of unit

  let get_the_result () : result =
    Log.trace __FUNCTION__;
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
    Log.trace __FUNCTION__;
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
    Log.trace __FUNCTION__;
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
    Log.trace __FUNCTION__;
    let the_sat_1 : Fsm.t = prepare_fsm weak (fst the_fsm_pair) in
    let the_sat_2 : Fsm.t = prepare_fsm weak (snd the_fsm_pair) in
    Log.trace ~__FUNCTION__ "finished preparing fsms (saturated if weak)";
    let merged_fsm : Fsm.t = Fsm.merge the_sat_1 the_sat_2 in
    Log.trace ~__FUNCTION__ "finished merging fsms";
    let pi : Partition.t = snd (Minimize.run merged_fsm) in
    Log.trace ~__FUNCTION__ "finished minimizing (checking for bisimilarity)";
    let (bisim_states, non_bisim_states) : Partition.t * Partition.t =
      split_bisimilar pi (fst the_fsm_pair) (snd the_fsm_pair)
    in
    Log.trace ~__FUNCTION__ "finished resolving bisimilarity result";
    { the_fsm_1 = { original = fst the_fsm_pair; saturated = the_sat_1 }
    ; the_fsm_2 = { original = snd the_fsm_pair; saturated = the_sat_2 }
    ; merged_fsm
    ; bisim_states
    ; non_bisim_states
    }
  ;;

  let result_to_bool (r : result) : bool =
    Log.trace __FUNCTION__;
    Model.Partition.is_empty r.non_bisim_states
  ;;

  let to_string (r : result) : string =
    Log.trace __FUNCTION__;
    Printf.sprintf
      "Bisimilar:\n%b\nBisimilar states:\n%s\nNon-bisimilar states:\n%s\n"
      (Partition.is_empty r.non_bisim_states)
      (Model.partition_to_string r.bisim_states)
      (Model.partition_to_string r.non_bisim_states)
  ;;
end
