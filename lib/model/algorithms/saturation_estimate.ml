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
end

module Make
    (C : Components.S)
    (FSM :
       FSM.S
       with type state = C.State.t
        and type states = C.State.Set.t
        and type labels = C.Label.Set.t
        and type edgemap = C.EdgeMap.t'
        and type info = C.Info.t) : S with type fsm = FSM.t = struct
  module State = C.State
  module States = C.State.Set
  module Label = C.Label
  module Labels = C.Label.Set
  module Action = C.Action
  module ActionMap = C.Action.Map
  module EdgeMap = C.EdgeMap
  module StateTbl = Hashtbl.Make (State)

  type fsm = FSM.t

  type t =
    { states : int
    ; sccs : int
    ; largest_scc : int
    ; strong : int
    ; weak : int
    }

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
    let w : int = Sys.int_size - 1
    let create (k : int) : int array = Array.make ((k + w - 1) / w) 0

    let add (b : int array) (i : int) : unit =
      b.(i / w) <- b.(i / w) lor (1 lsl (i mod w))
    ;;

    let union_into (dst : int array) (src : int array) : unit =
      Array.iteri (fun j x -> if x <> 0 then dst.(j) <- dst.(j) lor x) src
    ;;

    (** The sum of [size.(i)] over the members [i] of [b]. *)
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

  (** Tarjan's algorithm over the silent edges [adj], iteratively (a silent
      chain can be as long as the LTS). Returns each state's SCC id and the
      number of SCCs. SCCs are numbered in the order they complete, which is
      reverse topological: every SCC reachable from [c] has an id below
      [c]'s. *)
  let silent_sccs (adj : int list array) : int array * int =
    let n : int = Array.length adj in
    let index : int array = Array.make n (-1) in
    let low : int array = Array.make n 0 in
    let on_stack : bool array = Array.make n false in
    let comp : int array = Array.make n (-1) in
    let stack : int list ref = ref [] in
    let counter : int ref = ref 0 in
    let nc : int ref = ref 0 in
    let visit (v : int) : unit =
      index.(v) <- !counter;
      low.(v) <- !counter;
      incr counter;
      stack := v :: !stack;
      on_stack.(v) <- true
    in
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

  let fsm (x : FSM.t) : t =
    Logger.trace __FUNCTION__;
    let ids : int StateTbl.t = StateTbl.create 64 in
    let id (s : State.t) : int =
      match StateTbl.find_opt ids s with
      | Some i -> i
      | None ->
        let i : int = StateTbl.length ids in
        StateTbl.add ids s i;
        i
    in
    States.iter (fun s -> ignore (id s)) x.states;
    let silent : (int * int) list ref = ref [] in
    let visible : (int * Label.t * int) list ref = ref [] in
    let strong : int ref = ref 0 in
    EdgeMap.fold
      (fun (from : State.t) (actions : ActionMap.t') () ->
        let f : int = id from in
        ActionMap.fold
          (fun (a : Action.t) (ds : States.t) () ->
            States.iter
              (fun (d : State.t) ->
                incr strong;
                let g : int = id d in
                if Action.is_silent a
                then silent := (f, g) :: !silent
                else visible := (f, a.label, g) :: !visible)
              ds)
          actions
          ())
      x.edges
      ();
    let n : int = StateTbl.length ids in
    let adj : int list array = Array.make n [] in
    List.iter (fun (f, g) -> adj.(f) <- g :: adj.(f)) !silent;
    let comp, k = silent_sccs adj in
    let size : int array = Array.make k 0 in
    Array.iter (fun c -> size.(c) <- size.(c) + 1) comp;
    (* The SCC DAG: silent successors, and visible moves, per SCC. *)
    let succ : int list array = Array.make k [] in
    List.iter
      (fun (f, g) ->
        let cf, cg = comp.(f), comp.(g) in
        if cf <> cg then succ.(cf) <- cg :: succ.(cf))
      !silent;
    Array.iteri (fun c l -> succ.(c) <- List.sort_uniq Int.compare l) succ;
    let vout : (Label.t * int) list array = Array.make k [] in
    List.iter
      (fun (f, l, g) -> vout.(comp.(f)) <- (l, comp.(g)) :: vout.(comp.(f)))
      !visible;
    let labels : Labels.t =
      List.fold_left
        (fun acc (_, l, _) -> Labels.add l acc)
        Labels.empty
        !visible
    in
    (* [reach.(c)]: SCCs reachable from [c] by [tau*]. Successors first, as
       their ids are lower. *)
    let reach : int array array = Array.make k [||] in
    for c = 0 to k - 1 do
      let r : int array = Bits.create k in
      Bits.add r c;
      List.iter (fun d -> Bits.union_into r reach.(d)) succ.(c);
      reach.(c) <- r
    done;
    (* Per visible label [a], [weak.(c)]: SCCs reachable from [c] by
       [tau* a tau*] -- [c]'s own [a]-moves closed under [tau*], plus those of
       every silent successor. One label at a time, so only K^2 bits are live
       on top of [reach]. *)
    let total : int =
      Labels.fold
        (fun (a : Label.t) (acc : int) ->
          let weak : int array array = Array.make k [||] in
          let acc : int ref = ref acc in
          for c = 0 to k - 1 do
            let b : int array = Bits.create k in
            List.iter
              (fun ((l, e) : Label.t * int) ->
                if Label.equal l a then Bits.union_into b reach.(e))
              vout.(c);
            List.iter (fun d -> Bits.union_into b weak.(d)) succ.(c);
            weak.(c) <- b;
            acc := !acc + (size.(c) * Bits.weight size b)
          done;
          !acc)
        labels
        0
    in
    { states = n
    ; sccs = k
    ; largest_scc = Array.fold_left max 0 size
    ; strong = !strong
    ; weak = total
    }
  ;;
end
