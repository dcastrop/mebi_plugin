(** The explicit oracle: a network expanded state by state, and checked
    by the plugin's own model ([Model.Bisimilarity.fsm], as [MeBi Run Bisim] calls it), instantiated over [int] as in [bench/ocaml].
*)

module Base : Base_term.S with type t = int = Base_term.Make (struct
    include Int

    let to_string : int -> string = Printf.sprintf "%i"
  end)

module NoBindings : Json.S with type k = unit = Json.Thing.Make (struct
    type k = unit

    let name = "NoBindings"
    let json ?as_elt:_ () : Yojson.t = `Null
  end)

module M = Model.Make (Base) (NoBindings)

type result =
  { bisimilar : bool
  ; states1 : int
  ; states2 : int
  ; seconds : float
  }

(** [fsm ~label ~id net]: [net]'s reachable states as an FSM, global
    states numbered by [id] (shared between the two sides, so that they
    never collide), labels by [label] (0 is silent). *)
let fsm
      ~(label : Network.label -> int)
      ~(id : int * int array -> int)
      side
      (net : Network.t)
  : M.FSM.t * int
  =
  let st i : M.State.t = { base = i } in
  let lbl l : M.Label.t =
    { base = label l; is_silent = Some (l = Network.Tau) }
  in
  let seen = Hashtbl.create 1024 in
  let ts = ref M.Transition.Set.empty in
  let states = ref M.State.Set.empty in
  let sources = ref M.State.Set.empty in
  let alphabet = ref M.Label.Set.empty in
  let q = Queue.create () in
  let root = Network.init net in
  Hashtbl.add seen root ();
  Queue.push root q;
  while Bool.not (Queue.is_empty q) do
    let s = Queue.pop q in
    let i = id (side, s) in
    states := M.State.Set.add (st i) !states;
    List.iter
      (fun (l, t) ->
        sources := M.State.Set.add (st i) !sources;
        alphabet := M.Label.Set.add (lbl l) !alphabet;
        ts
        := M.Transition.Set.add
             { from = st i
             ; goto = st (id (side, t))
             ; label = lbl l
             ; tree = None
             ; annotation = None
             }
             !ts;
        if Bool.not (Hashtbl.mem seen t)
        then (
          Hashtbl.add seen t ();
          Queue.push t q))
      (Network.steps net s)
  done;
  let weak_labels = M.Label.Set.filter (fun l -> l.base = 0) !alphabet in
  ( M.FSM.of_lts
      { init = Some (st (id (side, root)))
      ; alphabet = !alphabet
      ; states = !states
      ; transitions = !ts
      ; terminals = M.State.Set.diff !states !sources
      ; info = { meta = None; weak_labels; nums = None }
      }
  , M.State.Set.cardinal !states )
;;

let check (n1 : Network.t) (n2 : Network.t) : result =
  let t0 = Unix.gettimeofday () in
  let labels = Hashtbl.create 16 in
  let label = function
    | Network.Tau -> 0
    | l ->
      (match Hashtbl.find_opt labels l with
       | Some i -> i
       | None ->
         let i = Hashtbl.length labels + 1 in
         Hashtbl.add labels l i;
         i)
  in
  let ids = Hashtbl.create 1024 in
  let id k =
    match Hashtbl.find_opt ids k with
    | Some i -> i
    | None ->
      let i = Hashtbl.length ids in
      Hashtbl.add ids k i;
      i
  in
  let a, states1 = fsm ~label ~id 0 n1 in
  let b, states2 = fsm ~label ~id 1 n2 in
  let bisimilar =
    M.Bisimilarity.Result.are_bisimilar (M.Bisimilarity.fsm a b).result
  in
  { bisimilar; states1; states2; seconds = Unix.gettimeofday () -. t0 }
;;
