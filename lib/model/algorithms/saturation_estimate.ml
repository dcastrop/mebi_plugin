(** {i See [saturation_estimate.mli].} *)
module type S = sig
  type fsm

  type t =
    { states : int
    ; sccs : int
    ; largest_scc : int
    ; strong : int
    ; weak : int
    }

  val fsm : fsm -> t
  val to_string : t -> string

  type partition

  val partition : fsm -> partition
end

module Make
    (C : Components.S)
    (FSM :
       FSM.S
       with type state = C.State.t
        and type states = C.State.Set.t
        and type labels = C.Label.Set.t
        and type edgemap = C.EdgeMap.t'
        and type info = C.Info.t) :
  S with type fsm = FSM.t and type partition = C.Partition.t = struct
  module State = C.State
  module States = C.State.Set
  module Label = C.Label
  module Labels = C.Label.Set
  module Action = C.Action
  module ActionMap = C.Action.Map
  module EdgeMap = C.EdgeMap
  module StateTbl = Hashtbl.Make (State)

  type fsm = FSM.t
  type partition = C.Partition.t

  type t =
    { states : int
    ; sccs : int
    ; largest_scc : int
    ; strong : int
    ; weak : int
    }

  (* See the [.mli]. *)
  let to_string (x : t) : string =
    Printf.sprintf
      "%i weak actions from %i states (%i strong transitions, %i silent SCCs, \
       the largest with %i)"
      x.weak
      x.states
      x.strong
      x.sccs
      x.largest_scc
  ;;

  (** Sets of SCC ids. One bit short of [Sys.int_size] per word, so no word
      ever has its sign bit set. *)
  module Bits = struct
    (** The bits used per word. *)
    let w : int = Sys.int_size - 1

    (** [create k] is the empty set over the ids [0 .. k - 1].

        Raises nothing. *)
    let create (k : int) : int array = Array.make ((k + w - 1) / w) 0

    (** [add b i] puts [i] in [b], in place.

        Raises nothing. *)
    let add (b : int array) (i : int) : unit =
      b.(i / w) <- b.(i / w) lor (1 lsl (i mod w))
    ;;

    (** [union_into dst src] adds every member of [src] to [dst], in place
        ([dst] and [src] over the same ids).

        Raises nothing. *)
    let union_into (dst : int array) (src : int array) : unit =
      Array.iteri (fun j x -> if x <> 0 then dst.(j) <- dst.(j) lor x) src
    ;;

    (** [weight size b] is the sum of [size.(i)] over the members [i] of
        [b].

        Raises nothing. *)
    let weight (size : int array) (b : int array) : int =
      let total : int ref = ref 0 in
      Array.iteri
        (fun j x ->
          if x <> 0
          then
            for i = 0 to w - 1 do
              if x land (1 lsl i) <> 0 then total := !total + size.((j * w) + i)
            done)
        b;
      !total
    ;;
  end

  (** [silent_sccs adj] is each node's SCC id, and the number of SCCs, of
      the graph of silent edges [adj] (node [v]'s successors at [adj.(v)]).

      Tarjan's algorithm, run iteratively, as a silent chain can be as long
      as the LTS. SCCs are numbered in the order they complete, which is
      reverse topological: every SCC reachable from [c] has an id below
      [c]'s.

      Raises nothing. *)
  let silent_sccs (adj : int list array) : int array * int =
    let n : int = Array.length adj in
    let index : int array = Array.make n (-1) in
    let low : int array = Array.make n 0 in
    let on_stack : bool array = Array.make n false in
    let comp : int array = Array.make n (-1) in
    let stack : int list ref = ref [] in
    let counter : int ref = ref 0 in
    let nc : int ref = ref 0 in
    (* [visit v] numbers [v] and pushes it on the stack *)
    let visit (v : int) : unit =
      index.(v) <- !counter;
      low.(v) <- !counter;
      incr counter;
      stack := v :: !stack;
      on_stack.(v) <- true
    in
    (* [pop_scc v] pops the stack down to [v], all of it one SCC *)
    let rec pop_scc (v : int) : unit =
      match !stack with
      | [] -> ()
      | x :: tl ->
        stack := tl;
        on_stack.(x) <- false;
        comp.(x) <- !nc;
        if x <> v then pop_scc v
    in
    (* Each work item is a node and the silent successors it has yet to
       look at. *)
    let rec run : (int * int list) list -> unit = function
      | [] -> ()
      | (v, x :: rest) :: tl ->
        if index.(x) < 0
        then (
          visit x;
          run ((x, adj.(x)) :: (v, rest) :: tl))
        else (
          if on_stack.(x) then low.(v) <- min low.(v) index.(x);
          run ((v, rest) :: tl))
      | (v, []) :: tl ->
        if low.(v) = index.(v)
        then (
          pop_scc v;
          incr nc);
        (match tl with
         | (u, _) :: _ -> low.(u) <- min low.(u) low.(v)
         | [] -> ());
        run tl
    in
    for v = 0 to n - 1 do
      if index.(v) < 0
      then (
        visit v;
        run [ v, adj.(v) ])
    done;
    comp, !nc
  ;;

  (** The quotient of an FSM by its silent SCCs: what both the estimate and
      the partition need. *)
  type quotient =
    { ids : int StateTbl.t (** state -> dense id *)
    ; comp : int array (** dense id -> SCC; reverse topological *)
    ; k : int (** number of SCCs *)
    ; size : int array (** SCC -> its number of states *)
    ; succ : int list array (** SCC -> silent successor SCCs, others *)
    ; vout : (Label.t * int) list array (** SCC -> visible moves, to SCCs *)
    ; reach : int array array (** SCC -> SCCs reachable by [tau*] *)
    ; labels : Labels.t (** visible labels used *)
    ; strong : int
    }

  (** [number_states x] is a table giving each of [x]'s states a dense id,
      [0 .. n - 1], in [States] order.

      Raises nothing. *)
  let number_states (x : FSM.t) : int StateTbl.t =
    let ids : int StateTbl.t = StateTbl.create 64 in
    States.iter (fun s -> StateTbl.add ids s (StateTbl.length ids)) x.states;
    ids
  ;;

  (** [transitions x] is every transition of [x], as (source, action,
      destination), in the order [x]'s edges are stored: by source, then
      action, then destination.

      Raises nothing. *)
  let transitions (x : FSM.t) : (State.t * Action.t * State.t) list =
    EdgeMap.fold
      (fun (from : State.t) (actions : ActionMap.t') acc ->
        ActionMap.fold
          (fun (a : Action.t) (ds : States.t) acc ->
            States.fold (fun (d : State.t) acc -> (from, a, d) :: acc) ds acc)
          actions
          acc)
      x.edges
      []
    |> List.rev
  ;;

  (** [split_edges ids x] is [x]'s transitions by id, the silent ones as
      [(from, goto)] and the visible ones as [(from, label, goto)] (each
      list in reverse order of {!transitions}), with the number of
      transitions in all. A state missing from [ids] is given the next id
      on the way.

      Raises nothing. *)
  let split_edges (ids : int StateTbl.t) (x : FSM.t)
    : (int * int) list * (int * Label.t * int) list * int
    =
    (* [id s] is [s]'s id, numbering it if new *)
    let id (s : State.t) : int =
      match StateTbl.find_opt ids s with
      | Some i -> i
      | None ->
        let i : int = StateTbl.length ids in
        StateTbl.add ids s i;
        i
    in
    List.fold_left
      (fun (silent, visible, strong)
        ((from, a, d) : State.t * Action.t * State.t) ->
        let f : int = id from in
        let g : int = id d in
        if Action.is_silent a
        then (f, g) :: silent, visible, strong + 1
        else silent, (f, a.label, g) :: visible, strong + 1)
      ([], [], 0)
      (transitions x)
  ;;

  (** [scc_dag comp k silent visible] is, for each of the [k] SCCs ([comp]
      giving each node's), its silent successor SCCs (other than itself,
      sorted, without duplicates) and its visible moves as (label, target
      SCC).

      Raises nothing. *)
  let scc_dag
        (comp : int array)
        (k : int)
        (silent : (int * int) list)
        (visible : (int * Label.t * int) list)
    : int list array * (Label.t * int) list array
    =
    let succ : int list array = Array.make k [] in
    List.iter
      (fun (f, g) ->
        let cf, cg = comp.(f), comp.(g) in
        if cf <> cg then succ.(cf) <- cg :: succ.(cf))
      silent;
    Array.iteri (fun c l -> succ.(c) <- List.sort_uniq Int.compare l) succ;
    let vout : (Label.t * int) list array = Array.make k [] in
    List.iter
      (fun (f, l, g) -> vout.(comp.(f)) <- (l, comp.(g)) :: vout.(comp.(f)))
      visible;
    succ, vout
  ;;

  (** [tau_reach k succ] is, for each of the [k] SCCs, the set of SCCs it
      reaches by [tau*] (itself included). Successors have lower ids
      ({!silent_sccs}), so each set is built from theirs in one pass.

      Raises nothing. *)
  let tau_reach (k : int) (succ : int list array) : int array array =
    let reach : int array array = Array.make k [||] in
    for c = 0 to k - 1 do
      let r : int array = Bits.create k in
      Bits.add r c;
      List.iter (fun d -> Bits.union_into r reach.(d)) succ.(c);
      reach.(c) <- r
    done;
    reach
  ;;

  (** [quotient x] is [x] quotiented by its silent SCCs ({!type-quotient}):
      {!number_states}, {!split_edges}, the SCCs by {!silent_sccs}, their
      sizes, {!scc_dag} and {!tau_reach}.

      Raises nothing. *)
  let quotient (x : FSM.t) : quotient =
    let ids : int StateTbl.t = number_states x in
    let silent, visible, strong = split_edges ids x in
    let n : int = StateTbl.length ids in
    let adj : int list array = Array.make n [] in
    List.iter (fun (f, g) -> adj.(f) <- g :: adj.(f)) silent;
    let comp, k = silent_sccs adj in
    let size : int array = Array.make k 0 in
    Array.iter (fun c -> size.(c) <- size.(c) + 1) comp;
    let succ, vout = scc_dag comp k silent visible in
    let labels : Labels.t =
      List.fold_left
        (fun acc (_, l, _) -> Labels.add l acc)
        Labels.empty
        visible
    in
    let reach = tau_reach k succ in
    { ids; comp; k; size; succ; vout; reach; labels; strong }
  ;;

  (** [weak_of q a] is, for each SCC [c] of [q], the set of SCCs reachable
      from [c] by [tau* a tau*]: [c]'s own [a]-moves closed under [tau*],
      plus those of every silent successor.

      Raises nothing. *)
  let weak_of (q : quotient) (a : Label.t) : int array array =
    let weak : int array array = Array.make q.k [||] in
    for c = 0 to q.k - 1 do
      let b : int array = Bits.create q.k in
      List.iter
        (fun ((l, e) : Label.t * int) ->
          if Label.equal l a then Bits.union_into b q.reach.(e))
        q.vout.(c);
      List.iter (fun d -> Bits.union_into b weak.(d)) q.succ.(c);
      weak.(c) <- b
    done;
    weak
  ;;

  (* See the [.mli]. For each visible label [a], every SCC [c] contributes
     [|c|] times the number of states in its [weak_a] SCCs. *)
  let fsm (x : FSM.t) : t =
    Logger.trace __FUNCTION__;
    let q : quotient = quotient x in
    (* One label at a time, so only K^2 bits are live on top of [reach]. *)
    let total : int =
      Labels.fold
        (fun (a : Label.t) (acc : int) ->
          let weak = weak_of q a in
          let acc : int ref = ref acc in
          for c = 0 to q.k - 1 do
            acc := !acc + (q.size.(c) * Bits.weight q.size weak.(c))
          done;
          !acc)
        q.labels
        0
    in
    { states = StateTbl.length q.ids
    ; sccs = q.k
    ; largest_scc = Array.fold_left max 0 q.size
    ; strong = q.strong
    ; weak = total
    }
  ;;

  (** [members b] is the members of the bitset [b], ascending.

      Raises nothing. *)
  let members (b : int array) : int list =
    let acc : int list ref = ref [] in
    for j = Array.length b - 1 downto 0 do
      let x = b.(j) in
      if x <> 0
      then
        for i = Bits.w - 1 downto 0 do
          if x land (1 lsl i) <> 0 then acc := ((j * Bits.w) + i) :: !acc
        done
    done;
    !acc
  ;;

  (** [scc_moves q] is, for each SCC of [q], its weak moves as (label index,
      target SCC) and its [=eps=>] targets: all a refinement round reads.
      Label indices are positions in [q]'s visible labels, in order. Kept
      as lists, so the memory is the number of SCC-level moves, not of
      states.

      Raises nothing. *)
  let scc_moves (q : quotient) : (int * int) list array * int list array =
    let labels : Label.t array = Array.of_list (Labels.elements q.labels) in
    let moves : (int * int) list array = Array.make q.k [] in
    Array.iteri
      (fun li a ->
        let weak = weak_of q a in
        for c = 0 to q.k - 1 do
          moves.(c)
          <- List.rev_append
               (List.map (fun d -> li, d) (members weak.(c)))
               moves.(c)
        done)
      labels;
    moves, Array.map members q.reach
  ;;

  (** [refine_blocks k moves eps] is the block of each of the [k] SCCs in
      the coarsest partition in which SCCs of one block reach the same
      blocks by each label of their weak [moves] and by their [=eps=>]
      moves ([eps]).

      Signature refinement: from one block, each SCC's (block, signature)
      is renumbered, the signature being its (label, block reached) pairs
      and ([-1], block) for [=eps=>], until the number of blocks stops
      growing.

      Raises nothing. *)
  let refine_blocks
        (k : int)
        (moves : (int * int) list array)
        (eps : int list array)
    : int array
    =
    let block : int array = Array.make k 0 in
    (* one round: renumber blocks by (block, signature); repeat while that
       makes more blocks *)
    let rec refine (blocks : int) : unit =
      let tbl : (int * (int * int) list, int) Hashtbl.t = Hashtbl.create k in
      let next : int array =
        Array.init k (fun c ->
          let sg =
            List.sort_uniq
              compare
              (List.rev_append
                 (List.map (fun (li, d) -> li, block.(d)) moves.(c))
                 (List.map (fun d -> -1, block.(d)) eps.(c)))
          in
          let key = block.(c), sg in
          match Hashtbl.find_opt tbl key with
          | Some b -> b
          | None ->
            let b = Hashtbl.length tbl in
            Hashtbl.add tbl key b;
            b)
      in
      let blocks' : int = Hashtbl.length tbl in
      Array.blit next 0 block 0 k;
      if blocks' > blocks then refine blocks'
    in
    refine 1;
    block
  ;;

  (** [expand_blocks q block] is the partition of [q]'s states that puts
      each state in its SCC's block.

      Raises nothing. *)
  let expand_blocks (q : quotient) (block : int array) : C.Partition.t =
    let by_block : (int, States.t) Hashtbl.t = Hashtbl.create 16 in
    StateTbl.iter
      (fun (s : State.t) (i : int) ->
        let b = block.(q.comp.(i)) in
        let prev =
          Stdlib.Option.value
            (Hashtbl.find_opt by_block b)
            ~default:States.empty
        in
        Hashtbl.replace by_block b (States.add s prev))
      q.ids;
    Hashtbl.fold
      (fun _ ss acc -> C.Partition.add ss acc)
      by_block
      C.Partition.empty
  ;;

  (* See the [.mli]: {!scc_moves}, {!refine_blocks} over the SCCs, then
     {!expand_blocks}. *)
  let partition (x : FSM.t) : C.Partition.t =
    Logger.trace __FUNCTION__;
    let q : quotient = quotient x in
    let moves, eps = scc_moves q in
    expand_blocks q (refine_blocks q.k moves eps)
  ;;
end
