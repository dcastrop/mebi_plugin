(* modules in this file follow the sections in Chapter 3 of
   [`Advanced Topics in Bisimulation and Coinduction`].
   (See https://doi.org/10.1017/CBO9780511792588.) *)

open Fsm

(** [RCP] contains algorithms for solving the
    `Relational Coarsest Partitioning` problem. *)
module RCP = struct
  (** [Examples] contains examples to be used by either [KS90] or [PT87]. *)
  module Examples = struct
    (** [exa_1] is `Example 3.2.5` on page 106. *)
    let exa_1 : fsm * fsm =
      let (s : fsm) =
        { init = { id = 0; hash = -1; pp = "s0" }
        ; states =
            States.of_list
              [ { id = 0; hash = -1; pp = "s0" }
              ; { id = 1; hash = -1; pp = "s1" }
              ; { id = 2; hash = -1; pp = "s2" }
              ]
        ; actions =
            Actions.of_list [ { id = 1; label = "a" }; { id = 2; label = "b" } ]
        ; edges =
            List.to_seq
              [ (* s0 *)
                ( { id = 0; hash = -1; pp = "s0" }
                , { action = { id = 1; label = "a" }
                  ; to_state = { id = 1; hash = -1; pp = "s1" }
                  } )
              ; ( { id = 0; hash = -1; pp = "s0" }
                , { action = { id = 1; label = "a" }
                  ; to_state = { id = 2; hash = -1; pp = "s2" }
                  } )
                (* s1 *)
              ; ( { id = 1; hash = -1; pp = "s1" }
                , { action = { id = 2; label = "b" }
                  ; to_state = { id = 2; hash = -1; pp = "s2" }
                  } )
                (* s2 *)
              ; ( { id = 2; hash = -1; pp = "s2" }
                , { action = { id = 2; label = "b" }
                  ; to_state = { id = 2; hash = -1; pp = "s2" }
                  } )
              ]
            |> Hashtbl.of_seq
        }
      in
      let (t : fsm) =
        { init = { id = 0; hash = -1; pp = "t0" }
        ; states =
            States.of_list
              [ { id = 0; hash = -1; pp = "t0" }
              ; { id = 1; hash = -1; pp = "t1" }
              ]
        ; actions =
            Actions.of_list [ { id = 1; label = "a" }; { id = 2; label = "b" } ]
        ; edges =
            List.to_seq
              [ (* t0 *)
                ( { id = 0; hash = -1; pp = "t0" }
                , { action = { id = 1; label = "a" }
                  ; to_state = { id = 1; hash = -1; pp = "t1" }
                  } )
                (* t1 *)
              ; ( { id = 1; hash = -1; pp = "t1" }
                , { action = { id = 2; label = "b" }
                  ; to_state = { id = 1; hash = -1; pp = "t1" }
                  } )
              ]
            |> Hashtbl.of_seq
        }
      in
      s, t
    ;;

    (** [exa_2] is `Example 3.2.6` on page 107. *)
    let exa_2 : fsm * fsm =
      let (s : fsm) =
        { init = { id = 0; hash = -1; pp = "s0" }
        ; states =
            States.of_list
              [ { id = 0; hash = -1; pp = "s0" }
              ; { id = 1; hash = -1; pp = "s1" }
              ; { id = 2; hash = -1; pp = "s2" }
              ; { id = 3; hash = -1; pp = "s3" }
              ; { id = 4; hash = -1; pp = "s4" }
              ]
        ; actions =
            Actions.of_list [ { id = 1; label = "a" }; { id = 2; label = "b" } ]
        ; edges =
            List.to_seq
              [ (* s0 *)
                ( { id = 0; hash = -1; pp = "s0" }
                , { action = { id = 1; label = "a" }
                  ; to_state = { id = 1; hash = -1; pp = "s1" }
                  } )
              ; ( { id = 0; hash = -1; pp = "s0" }
                , { action = { id = 1; label = "a" }
                  ; to_state = { id = 2; hash = -1; pp = "s2" }
                  } )
                (* s1 *)
              ; ( { id = 1; hash = -1; pp = "s1" }
                , { action = { id = 1; label = "a" }
                  ; to_state = { id = 3; hash = -1; pp = "s3" }
                  } )
              ; ( { id = 1; hash = -1; pp = "s1" }
                , { action = { id = 2; label = "b" }
                  ; to_state = { id = 4; hash = -1; pp = "s4" }
                  } )
                (* s2 *)
              ; ( { id = 2; hash = -1; pp = "s2" }
                , { action = { id = 1; label = "a" }
                  ; to_state = { id = 4; hash = -1; pp = "s4" }
                  } )
                (* s3 *)
              ; ( { id = 3; hash = -1; pp = "s3" }
                , { action = { id = 1; label = "a" }
                  ; to_state = { id = 0; hash = -1; pp = "s0" }
                  } )
                (* s4 *)
              ; ( { id = 4; hash = -1; pp = "s4" }
                , { action = { id = 1; label = "a" }
                  ; to_state = { id = 0; hash = -1; pp = "s0" }
                  } )
              ]
            |> Hashtbl.of_seq
        }
      in
      let (t : fsm) =
        { init = { id = 0; hash = -1; pp = "t0" }
        ; states =
            States.of_list
              [ { id = 0; hash = -1; pp = "t0" }
              ; { id = 1; hash = -1; pp = "t1" }
              ; { id = 2; hash = -1; pp = "t2" }
              ; { id = 3; hash = -1; pp = "t3" }
              ; { id = 4; hash = -1; pp = "t4" }
              ; { id = 5; hash = -1; pp = "t5" }
              ]
        ; actions =
            Actions.of_list [ { id = 1; label = "a" }; { id = 2; label = "b" } ]
        ; edges =
            List.to_seq
              [ (* t0 *)
                ( { id = 0; hash = -1; pp = "t0" }
                , { action = { id = 1; label = "a" }
                  ; to_state = { id = 1; hash = -1; pp = "t1" }
                  } )
              ; ( { id = 0; hash = -1; pp = "t0" }
                , { action = { id = 1; label = "a" }
                  ; to_state = { id = 3; hash = -1; pp = "t3" }
                  } )
                (* t1 *)
              ; ( { id = 1; hash = -1; pp = "t1" }
                , { action = { id = 2; label = "b" }
                  ; to_state = { id = 2; hash = -1; pp = "t2" }
                  } )
              ; ( { id = 1; hash = -1; pp = "t1" }
                , { action = { id = 1; label = "a" }
                  ; to_state = { id = 5; hash = -1; pp = "t5" }
                  } )
              ; ( { id = 1; hash = -1; pp = "t1" }
                , { action = { id = 2; label = "b" }
                  ; to_state = { id = 5; hash = -1; pp = "t5" }
                  } )
                (* t2 *)
              ; ( { id = 2; hash = -1; pp = "t2" }
                , { action = { id = 1; label = "a" }
                  ; to_state = { id = 0; hash = -1; pp = "t0" }
                  } )
                (* t3 *)
              ; ( { id = 3; hash = -1; pp = "t3" }
                , { action = { id = 1; label = "a" }
                  ; to_state = { id = 4; hash = -1; pp = "t4" }
                  } )
                (* t4 *)
              ; ( { id = 4; hash = -1; pp = "t4" }
                , { action = { id = 1; label = "a" }
                  ; to_state = { id = 0; hash = -1; pp = "t0" }
                  } )
                (* t5 *)
              ; ( { id = 5; hash = -1; pp = "t5" }
                , { action = { id = 1; label = "a" }
                  ; to_state = { id = 0; hash = -1; pp = "t0" }
                  } )
              ; ( { id = 5; hash = -1; pp = "t5" }
                , { action = { id = 1; label = "a" }
                  ; to_state = { id = 4; hash = -1; pp = "t4" }
                  } )
              ]
            |> Hashtbl.of_seq
        }
      in
      s, t
    ;;
  end

  (** [KS90] follows algorithm by Kanellakis and Smolka. *)
  module KS90 = struct
    (** [Block] is a set of states.
        This is necessary since [KS90] requires the actions of states to be sortable. *)
    module Block = Fsm.States

    module Partition = Set.Make (Block)

    (**  *)
    let pstr_partition
      ?(ids : unit option)
      ?(pp : unit option)
      ?(indent : int = 1)
      (p : Partition.t)
      : string
      =
      if Partition.is_empty p
      then "[ ] (empty)"
      else
        Printf.sprintf
          "[%s]"
          (Partition.fold
             (fun (b : Block.t) (acc : string) ->
               Printf.sprintf
                 "%s%s%s\n"
                 acc
                 (Mebi_utils.str_tabs indent)
                 (* figure out which units to pass on (bit messy) *)
                 (handle_states_pstr ~indent:(indent + 1) ids pp None b))
             p
             "\n")
    ;;

    (* Error when trying to split on empty block. *)
    exception EmptyBlock of Block.t

    (* Error if same state found in multiple blocks of a partition. *)
    exception PartitionsNotDisjoint of Partition.t

    (** [reachable_blocks outgoing_edges pi] is the subset of states in [pi] reachable via [outgoing_edges]. *)
    let reachable_blocks
      (outgoing_edges : fsm_transition list)
      (pi : Partition.t)
      : Block.t
      =
      List.fold_left
        (fun (acc : Block.t) (edge : fsm_transition) ->
          (* filter [pi] to find block containing destination state. *)
          let filtered_pi =
            Partition.filter (fun (b : Block.t) -> Block.mem edge.to_state b) pi
          in
          (* throw error if state found in more than one partition *)
          if Partition.cardinal filtered_pi > 1
          then raise (PartitionsNotDisjoint pi)
          else (
            (* get single block in partition *)
            let filtered' = List.nth (Partition.elements filtered_pi) 0 in
            if Block.is_empty filtered'
            then acc
            else Block.add_seq (Block.to_seq filtered') acc))
        Block.empty
        outgoing_edges
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
      (edges : Fsm.edges)
      : Partition.t
      =
      Feedback.msg_warning
        (Pp.str
           (Printf.sprintf
              "#### split...\nblock: %s.\na: %s\npi: %s.\nedges: %s.\n"
              (Fsm.pstr_states ~pp:() block)
              (Fsm.pstr_action a)
              (pstr_partition ~pp:() pi)
              (Fsm.pstr_edges ~pp:() edges)));
      (* choose some state [s] in [block] *)
      match Block.to_list block with
      | [] -> raise (EmptyBlock block)
      | s :: block' ->
        (* get edges corresponding to [action] *)
        let s_edges =
          List.filter
            (fun (e : fsm_transition) -> e.action == a)
            (Hashtbl.find_all edges s)
        in
        (* cache which partitions in [pi] [s_edges] can reach *)
        let s_reachable = reachable_blocks s_edges pi in
        (* for each state in [block] *)
        let b1, b2 =
          List.fold_left
            (fun ((b1', b2') : Block.t * Block.t) t ->
              let t_edges =
                List.filter
                  (fun (e : fsm_transition) -> e.action == a)
                  (Hashtbl.find_all edges t)
              in
              (* check if edges of s and t can reach the same blocks in [pi]. *)
              let t_reachable = reachable_blocks t_edges pi in
              (* TODO: this seems to be broken

                 !!! continue from here
              *)
              if Block.is_empty (Block.inter s_reachable t_reachable)
              then (
                Feedback.msg_warning
                  (Pp.str
                     (Printf.sprintf
                        "s and t cannot reach the same block via label %s."
                        (Fsm.pstr_action a)));
                Block.union b1' (Block.of_list [ t ]), b2')
              else b1', Block.union b2' (Block.of_list [ t ]))
            (Block.empty, Block.empty)
            block'
        in
        Feedback.msg_warning
          (Pp.str
             (Printf.sprintf
                "\nb1: %s.\nb2: %s.\n#### split #### (end)\n"
                (Fsm.pstr_states ~pp:() b1)
                (Fsm.pstr_states ~pp:() b2)));
        if Block.is_empty b2
        then Partition.of_list [ b1 ]
        else Partition.of_list [ b1; b2 ]
    ;;

    (* let changed = ref true in
       let rec loop (pi : Partition.t) : Partition.t =
       if !changed
       then (
       changed := false;
       Partition.fold (fun (b:Block.t) (acc:Partition.t) -> acc) pi Partition.empty *)

    exception SplitEmpty of Partition.t
    exception SplitTooMany of Partition.t

    (*  *)
    let run (s : fsm) (t : fsm) : Partition.t =
      (* initially [pi] is a partition with a single block containing all states. *)
      let pi, map_t_states =
        let init_block, map_t_states' =
          (* cant just merge States.t since their ids would conflict *)
          States.fold
            (fun (state : Fsm.state)
              ((acc, map) : Block.t * (Fsm.state, Fsm.state) Hashtbl.t) ->
              let state' = Fsm.state (Block.cardinal acc) in
              (* save mapping from old to new state *)
              Hashtbl.add map state state';
              Block.add state' acc, map)
            s.states
            (t.states, States.cardinal t.states |> Hashtbl.create)
        in
        ref (Partition.of_list [ init_block ]), map_t_states'
      in
      (* merge actions *)
      let actions, map_t_actions =
        Actions.fold
          (fun (action : Fsm.action)
            ((acc, map) : Fsm.Actions.t * (Fsm.action, Fsm.action) Hashtbl.t) ->
            let action' =
              Fsm.action ~label:action.label (Actions.cardinal acc)
            in
            (* save mapping from old to new action *)
            Hashtbl.add map action action';
            Actions.add action' acc, map)
          s.actions
          (t.actions, Actions.cardinal t.actions |> Hashtbl.create)
      in
      (* merge edge tables *)
      let edges =
        Hashtbl.fold
          (fun (from_state : Fsm.state)
            (outgoing_edge : Fsm.fsm_transition)
            (acc : Fsm.edges) ->
            (* need to update states in edge *)
            Hashtbl.add
              acc
              (Hashtbl.find map_t_states from_state)
              { action = Hashtbl.find map_t_actions outgoing_edge.action
              ; to_state = Hashtbl.find map_t_states outgoing_edge.to_state
              };
            acc)
          s.edges
          t.edges
        (* (Seq.append (Hashtbl.to_seq s.edges) (Hashtbl.to_seq t.edges)) *)
      in
      (* prepare main alg loop *)
      let changed = ref true in
      while !changed do
        changed := false;
        (* for each block in [pi] *)
        Partition.iter
          (fun (b : Block.t) ->
            (* for each action *)
            Fsm.Actions.iter
              (fun (a : Fsm.action) ->
                (* sort outgoing edges with [a.label] by  [b] by the states with outgoing edges with [a.label] *)
                (* let edges = Hashtbl.length f.edges |> Hashtbl.create in *)
                let edges_of_a = edges in
                let pi' = split b a !pi edges_of_a in
                (* check no greater than 2 blocks were returned *)
                if Partition.cardinal pi' > 2
                then raise (SplitTooMany pi')
                else if Partition.cardinal pi' == 2
                then
                  (* if [pi'] has two blocks that are not just [b] again, *)
                  if Bool.not
                       (Block.equal
                          b
                          (Partition.fold
                             (fun (b' : Block.t) (acc : Block.t) ->
                               Block.add_seq (Block.to_seq b') acc)
                             pi'
                             Block.empty))
                  then (
                    (* refine further using [pi'] *)
                    pi := pi';
                    changed := true))
              actions;
            ())
          !pi
      done;
      (* return partitions *)
      !pi
    ;;
  end

  (** [PT87] follows algorithm by Paige and Tarjan,
      improving upon [KS90]. *)
  module PT87 = struct
    (* TODO: *)
  end
end

let bisim_foo : int = 0
