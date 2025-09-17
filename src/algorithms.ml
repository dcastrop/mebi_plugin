module Minimize = struct
  open Model

  type t = Fsm.t
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
        (l : Action.Label.t)
        (edges : States.t Actions.t Edges.t)
        (* (edges_of_a : States.t Actions.t Edges.t) *)
          (pi : Partition.t)
    : States.t * States.t option
    =
    assert (Bool.not (States.is_empty block));
    let edges_of_a = Model.get_edges_with_label l edges in
    if Int.equal 0 (Edges.length edges_of_a)
    then
      Utils.Logging.Log.warning
        (Printf.sprintf "No edges of (%s)." (Action.Label.to_string l));
    (* else
      Utils.Logging.Log.warning
        (Printf.sprintf
           "Edges of (%s): %s"
           (Action.Label.to_string l)
           (pstr_edges edges_of_a)); *)
    (* selects some state [s] from [block] *)
    let s = States.min_elt block in
    let s_actions = Model.get_actions_from s edges_of_a in
    let s_reachable_blocks = Model.get_reachable_blocks_opt s_actions pi in
    (* check rest of [block] *)
    States.fold
      (fun (t : State.t) ((b1, b2) : States.t * States.t option) ->
        if State.eq s t
        then States.add s b1, b2
        else (
          let t_actions = Model.get_actions_from t edges_of_a in
          let t_reachable_blocks =
            Model.get_reachable_blocks_opt t_actions pi
          in
          match s_reachable_blocks, t_reachable_blocks with
          | None, None ->
            (* Utils.Logging.Log.warning
               (Printf.sprintf
               "split_block (%s): (%s, None), (%s, None)"
               (Action.Label.to_string l)
               (State.to_string s)
               (State.to_string t)); *)
            States.add t b1, b2
          | Some s_blocks, Some t_blocks ->
            (* Utils.Logging.Log.warning
               (Printf.sprintf
               "split_block (%s): (%s, Some), (%s, Some)"
               (Action.Label.to_string l)
               (State.to_string s)
               (State.to_string t)); *)
            if
              Partition.equal
                (Partition.inter s_blocks t_blocks)
                (Partition.union s_blocks t_blocks)
            then States.add t b1, b2
            else b1, add_to_block_option t b2
          | _, _ ->
            (* Utils.Logging.Log.warning
               (Printf.sprintf
               "split_block (%s): (%s, _), (%s, _)"
               (Action.Label.to_string l)
               (State.to_string s)
               (State.to_string t)); *)
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
          (fun (l : Action.Label.t) ->
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

  let run ?(weak : bool = false) (the_fsm : t) : result =
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
end

module Bisimilar = struct
  open Model

  type t = Fsm.pair * Fsm.t * Partition.t
  type result = Fsm.pair * Fsm.t * (Partition.t * Partition.t)

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
        match Fsm.state_origin (the_fsm_1, the_fsm_2) s with
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

  let run (((the_fsm_1, the_fsm_2), merged_fsm, pi) : t) : result =
    (the_fsm_1, the_fsm_2), merged_fsm, split_bisimilar pi the_fsm_1 the_fsm_2
  ;;
end

(**************************************************************************)
(** Algorithms ************************************************************)
(**************************************************************************)

type t =
  | Satur of Fsm.t
  | Minim of (bool * Minimize.t)
  | Bisim of (bool * Fsm.pair)

type result =
  | Satur of Fsm.t
  | Minim of Minimize.result
  | Bisim of Bisimilar.result

let run (args : t) : result =
  match args with
  | Satur the_fsm -> Satur (Fsm.saturate the_fsm)
  | Minim params ->
    Minim
      (Minimize.run
         (match params with
          | true, the_fsm -> Fsm.saturate the_fsm
          | _, the_fsm -> the_fsm))
  | Bisim params ->
    Bisim
      (Bisimilar.run
         (match params with
          | true, (the_fsm_1, the_fsm_2) ->
            let the_saturated_pair : Fsm.pair =
              Fsm.saturate the_fsm_1, Fsm.saturate the_fsm_2
            in
            let merged_fsm : Fsm.t = Fsm.merge the_saturated_pair in
            the_saturated_pair, merged_fsm, snd (Minimize.run merged_fsm)
          | _, the_fsm_pair ->
            let merged_fsm : Fsm.t = Fsm.merge the_fsm_pair in
            the_fsm_pair, merged_fsm, snd (Minimize.run merged_fsm)))
;;

let bisim_result_to_bool (r : Bisimilar.result) =
  match r with
  | _, _, (_bisim_states, non_bisim_states) ->
    Model.Partition.is_empty non_bisim_states
;;
