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
        { init = { id = 0; pp = "s0" }
        ; edges =
            List.to_seq
              [ (* s0 *)
                ( { id = 0; pp = "s0" }
                , { label = 1; to_state = { id = 1; pp = "s1" } } )
              ; ( { id = 0; pp = "s0" }
                , { label = 1; to_state = { id = 2; pp = "s2" } } )
                (* s1 *)
              ; ( { id = 1; pp = "s1" }
                , { label = 2; to_state = { id = 2; pp = "s2" } } )
                (* s2 *)
              ; ( { id = 2; pp = "s2" }
                , { label = 2; to_state = { id = 2; pp = "s2" } } )
              ]
            |> Hashtbl.of_seq
        }
      in
      let (t : fsm) =
        { init = { id = 0; pp = "t0" }
        ; edges =
            List.to_seq
              [ (* t0 *)
                ( { id = 0; pp = "t0" }
                , { label = 1; to_state = { id = 1; pp = "t1" } } )
                (* t1 *)
              ; ( { id = 1; pp = "t1" }
                , { label = 2; to_state = { id = 1; pp = "t1" } } )
              ]
            |> Hashtbl.of_seq
        }
      in
      s, t
    ;;

    (** [exa_2] is `Example 3.2.6` on page 107. *)
    let exa_2 : fsm * fsm =
      let (s : fsm) =
        { init = { id = 0; pp = "s0" }
        ; edges =
            List.to_seq
              [ (* s0 *)
                ( { id = 0; pp = "s0" }
                , { label = 1; to_state = { id = 1; pp = "s1" } } )
              ; ( { id = 0; pp = "s0" }
                , { label = 1; to_state = { id = 2; pp = "s2" } } )
                (* s1 *)
              ; ( { id = 1; pp = "s1" }
                , { label = 1; to_state = { id = 3; pp = "s3" } } )
              ; ( { id = 1; pp = "s1" }
                , { label = 2; to_state = { id = 4; pp = "s4" } } )
                (* s2 *)
              ; ( { id = 2; pp = "s2" }
                , { label = 1; to_state = { id = 4; pp = "s4" } } )
                (* s3 *)
              ; ( { id = 3; pp = "s3" }
                , { label = 1; to_state = { id = 0; pp = "s0" } } )
                (* s4 *)
              ; ( { id = 4; pp = "s4" }
                , { label = 1; to_state = { id = 0; pp = "s0" } } )
              ]
            |> Hashtbl.of_seq
        }
      in
      let (t : fsm) =
        { init = { id = 0; pp = "t0" }
        ; edges =
            List.to_seq
              [ (* t0 *)
                ( { id = 0; pp = "t0" }
                , { label = 1; to_state = { id = 1; pp = "t1" } } )
              ; ( { id = 0; pp = "t0" }
                , { label = 1; to_state = { id = 3; pp = "t3" } } )
                (* t1 *)
              ; ( { id = 1; pp = "t1" }
                , { label = 2; to_state = { id = 2; pp = "t2" } } )
              ; ( { id = 1; pp = "t1" }
                , { label = 1; to_state = { id = 5; pp = "t5" } } )
              ; ( { id = 1; pp = "t1" }
                , { label = 2; to_state = { id = 5; pp = "t5" } } )
                (* t2 *)
              ; ( { id = 2; pp = "t2" }
                , { label = 1; to_state = { id = 0; pp = "t0" } } )
                (* t3 *)
              ; ( { id = 3; pp = "t3" }
                , { label = 1; to_state = { id = 4; pp = "t4" } } )
                (* t4 *)
              ; ( { id = 4; pp = "t4" }
                , { label = 1; to_state = { id = 0; pp = "t0" } } )
                (* t5 *)
              ; ( { id = 5; pp = "t5" }
                , { label = 1; to_state = { id = 0; pp = "t0" } } )
              ; ( { id = 5; pp = "t5" }
                , { label = 1; to_state = { id = 4; pp = "t4" } } )
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
        [f] is the corresponding FSM (needed to obtain the edges). *)
    let split (block : Block.t) (action : label) (pi : Partition.t) (f : fsm)
      : Partition.t
      =
      (* choose some state [s] in [block] *)
      match Block.to_list block with
      | [] -> raise (EmptyBlock block)
      | s :: block' ->
        (* get edges corresponding to [action] *)
        let s_edges =
          List.filter
            (fun (e : fsm_transition) -> e.label == action)
            (Hashtbl.find_all f.edges s)
        in
        (* cache which partitions in [pi] [s_edges] can reach *)
        let s_reachable = reachable_blocks s_edges pi in
        (* for each state in [block] *)
        let b1, b2 =
          List.fold_left
            (fun ((b1', b2') : Block.t * Block.t) t ->
              let t_edges =
                List.filter
                  (fun (e : fsm_transition) -> e.label == action)
                  (Hashtbl.find_all f.edges t)
              in
              (* check if edges of s and t can reach the same blocks in [pi]. *)
              let t_reachable = reachable_blocks t_edges pi in
              if Block.is_empty (Block.inter s_reachable t_reachable)
              then Block.union b1' (Block.of_list [ t ]), b2'
              else b1', Block.union b2' (Block.of_list [ t ]))
            (Block.empty, Block.empty)
            block'
        in
        if Block.is_empty b2
        then Partition.of_list [ b1 ]
        else Partition.of_list [ b1; b2 ]
    ;;

    (*  *)
    let run (f : fsm) : unit =
      (* TODO: change this to use [Fsm.States] *)
      (* let pi = Partition.of_list  *)

      (* TODO: *)
      ()
    ;;
  end

  (** [PT87] follows algorithm by Paige and Tarjan,
      improving upon [KS90]. *)
  module PT87 = struct
    (* TODO: *)
  end
end

let bisim_foo : label = 0
