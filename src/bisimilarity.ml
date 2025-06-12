(* modules in this file follow the sections in Chapter 3 of
   [`Advanced Topics in Bisimulation and Coinduction`].
   (See https://doi.org/10.1017/CBO9780511792588.) *)

open Fsm



type fsm_pair = fsm * fsm

type run_params = fsm_pair * (Merge.merged_fsm_result_kind option)

type result_params = fsm_pair * Merge.merged_fsm_result_kind * (Partition.t * Partition.t)

type result_kind = | Pass of result_params | Fail of result_params





    type state_origins =
      { s : bool
      ; t : bool
      }

    (** [origins_of ?params block (s_states,t_states) map_of_states] ... *)
    let origins_of
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
            origins_of block (s_states, t_states) map_of_states
          in
          (* block is bisimilar if it contains states from both fsms. *)
          if origins.s && origins.t
          then Partition.add block bisimilar_states', non_bisimilar_states'
          else bisimilar_states', Partition.add block non_bisimilar_states')
        !pi
        (Partition.empty, Partition.empty)
    ;;


let main_loop 
(alphabet : Alphabet.t)
      (edges : States.t Actions.t Edges.t)
      (pi : Partition.t ref)
      (weak : bool)
      : unit =
      let changed = ref true in
      while !changed do changed := false; 
      (* TODO: *)
        (* main_loop_body (alphabet, edges) pi changed weak *)
  done 
;;
      

let saturate_fsms (the_fsm_1:fsm) (the_fsm_2:fsm) : fsm_pair =
  (Fsm.Saturate.fsm the_fsm_1, Fsm.Saturate.fsm the_fsm_2)

let run ?(weak:bool=false) (((the_fsm_1, the_fsm_2), pre_merged_fsm):run_params) : result_kind =
  let the_merged_fsm, the_state_translation = match pre_merged_fsm with | None ->
    let (to_merge_1, to_merge_2) = if weak then (saturate_fsms the_fsm_1 the_fsm_2) else ( the_fsm_1, the_fsm_2) in 
    Fsm.Merge.fsms (to_merge_1) to_merge_2 
    
    | Some (merged_fsm, state_translation) -> 
    if weak then (
      Utils.Logging.Log.warning "Checking weak bisimilarity, will re-merge FSMs after saturating them. (Ignoring provided merged FSM.)";
      let (the_sat_1, the_sat_2) = saturate_fsms the_fsm_1 the_fsm_2 in
      Fsm.Merge.fsms (the_sat_1) the_sat_2
    )
    else merged_fsm, state_translation in
    match the_merged_fsm with
    | {alphabet; states; edges; _} ->
      let pi = ref (Partition.of_list [states]) in main_loop alphabet edges pi weak;
      let bisim_states, non_bisim_states = split_bisimilar (the_fsm_1.states, the_fsm_2.states) the_state_translation pi in
      let the_result = ((the_fsm_1, the_fsm_2), (the_merged_fsm, the_state_translation), (bisim_states, non_bisim_states))in
      match Partition.is_empty non_bisim_states with
      | true -> Pass the_result
      | _ -> Fail the_result