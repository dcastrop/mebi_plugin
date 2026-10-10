(** The model instantiated over [int] (as in [test/tests.ml]), and a
    benchmark input: one FSM, or two to compare, built from edge triples. *)

module Base : Base_term.S with type t = int = Base_term.Make (struct
    include Int

    let to_string : int -> string = Printf.sprintf "%i"
  end)

(** [Model.Make]'s last parameter is only stored and serialised; there is no
    Rocq provenance to record here. *)
module NoBindings : Json.S with type k = unit = Json.Thing.Make (struct
    type k = unit

    let name = "NoBindings"
    let json ?as_elt:_ () : Yojson.t = `Null
  end)

module M = Model.Make (Base) (NoBindings)

(** One side of an input: an initial state, every state (including those
    with no edges), and its edges as [(from, label, goto)] triples. Labels
    in [silent] are silent. *)
type side =
  { init : int
  ; states : int list
  ; edges : (int * int * int) list
  }

(** A benchmark input. [b] and [bisimilar] are [Some] for a pair, whose two
    sides share one encoding (a state both reach is one state, as in the
    plugin), with [bisimilar] the expected verdict on their initial states.
    [expect_states] is each side's state count as the plugin found it. *)
type t =
  { name : string
  ; silent : int list
  ; a : side
  ; b : side option
  ; bisimilar : bool option
  }

let state (i : int) : M.State.t = { base = i }

(** [fsm ~silent s] is side [s] as an FSM, as [FSM.of_lts] makes the
    plugin's: one action per edge, terminals the states with no edges. *)
let fsm ~(silent : int list) (s : side) : M.FSM.t =
  let label l : M.Label.t =
    { base = l; is_silent = Some (List.mem l silent) }
  in
  let transitions =
    List.fold_left
      (fun acc (f, l, g) ->
        M.Transition.Set.add
          { from = state f
          ; goto = state g
          ; label = label l
          ; tree = None
          ; annotation = None
          }
          acc)
      M.Transition.Set.empty
      s.edges
  in
  let states =
    List.fold_left
      (fun acc i -> M.State.Set.add (state i) acc)
      M.State.Set.empty
      (s.init :: s.states)
  in
  let sources =
    List.fold_left
      (fun acc (f, _, _) -> M.State.Set.add (state f) acc)
      M.State.Set.empty
      s.edges
  in
  let alphabet =
    List.fold_left
      (fun acc (_, l, _) -> M.Label.Set.add (label l) acc)
      M.Label.Set.empty
      s.edges
  in
  let weak_labels =
    M.Label.Set.filter (fun l -> List.mem l.base silent) alphabet
  in
  M.FSM.of_lts
    { init = Some (state s.init)
    ; alphabet
    ; states
    ; transitions
    ; terminals = M.State.Set.diff states sources
    ; info = { meta = None; weak_labels; nums = None }
    }
;;

(** [num_states s] and [num_edges s]: the size of side [s]. *)
let num_states (s : side) : int =
  List.length (List.sort_uniq Int.compare (s.init :: s.states))
;;

let num_edges (s : side) : int = List.length (List.sort_uniq compare s.edges)
