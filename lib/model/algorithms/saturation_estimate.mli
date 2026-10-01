(** The size of an FSM's saturation, computed without saturating it.

    {!FSM.saturate} materialises one weak action per distinct
    [(from, a, goto)] with [from -tau*-> s -a-> t -tau*-> goto] (of the many
    witnessing paths only the shortest is kept), so the size of its output is
    the number of such triples -- which can be orders of magnitude more than
    the strong transitions. [Proc/Test4]'s 9720 states and ~15k transitions
    saturate to ~112M weak actions, and the attempt exhausted 15GB.

    This counts those triples exactly, on the quotient by silent SCCs: states
    in one silent SCC have the same weak successors, so the count is
    [sum over SCC C of |C| * sum over a of |weak_a(C)|], with [weak_a(C)] the
    states reachable from [C] by [tau* a tau*]. [Test4] has 81 such SCCs.

    Cost: O(V + E) for the SCCs, then bitsets over SCCs -- O(K^2) bits for K
    SCCs, recomputed once per visible label. This is negligible next to the
    extraction that produced the FSM: it reaches the size of the extracted
    LTS itself only past about a million SCCs. {i See [ASSISTED-CHANGES.md],
    2026-10-01, backlog item H2.} *)
module type S = sig
  type fsm

  type t =
    { states : int
    ; sccs : int (** silent strongly-connected components *)
    ; largest_scc : int
    ; strong : int (** transitions, silent and visible, before saturating *)
    ; weak : int (** weak (visible) actions {!FSM.saturate} would produce *)
    }

  val fsm : fsm -> t
  val to_string : t -> string
end

module Make
    (C : Components.S)
    (FSM :
       FSM.S
       with type state = C.State.t
        and type states = C.State.Set.t
        and type labels = C.Label.Set.t
        and type edgemap = C.EdgeMap.t'
        and type info = C.Info.t) : S with type fsm = FSM.t
