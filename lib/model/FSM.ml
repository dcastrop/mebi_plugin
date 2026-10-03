module type S = sig
  type state
  type states
  type labels
  type edgemap
  type info
  type lts

  type t =
    { init : state option
    ; alphabet : labels
    ; states : states
    ; edges : edgemap
    ; terminals : states
    ; info : info
    ; fill : (state -> unit) option
      (** [Some f] for an FSM saturated on demand ({!saturate_on_demand}):
          [edges] holds only the states asked about so far, and [f s] adds
          [s]'s. Read such an FSM's edges for [s] only after {!ensure}. *)
    }

  include Json.S with type k = t

  val of_lts : lts -> t
  val merge : t -> t -> t
  val is_weak_mode : t -> bool
  val saturate : ?only_if_weak:bool -> t -> t

  (** [ensure x s] makes [x.edges] hold [s]'s edges: a no-op unless [x] is
      saturated on demand. Every read of a saturated FSM's edges for one
      state goes through it. *)
  val ensure : t -> state -> unit

  (** [saturate_on_demand ~budget x] is [saturate x] without the up-front
      cost: each state is saturated when first asked about ({!ensure}), from
      the same code as {!saturate}, so with the same weak actions and
      witnesses. At most about [budget] weak actions are held at a time
      (default 1,000,000), the oldest states dropped first and recomputed if
      asked about again. [terminals] stays [x]'s: states that only become
      terminal by saturating are not found without saturating them. For FSMs
      too large to saturate whole ({!Saturation_estimate}); see
      [notes/13]. *)
  val saturate_on_demand : ?budget:int -> t -> t
end

module Make
    (C : Components.S)
    (LTS :
       LTS.S
       with type state = C.State.t
        and type states = C.State.Set.t
        and type labels = C.Label.Set.t
        and type transitions = C.EdgeMap.transitions
        and type info = C.Info.t)
    (Saturation :
       Saturation.S
       with type state = C.State.t
        and type states = C.State.Set.t
        and type labels = C.Label.Set.t
        and type edgemap = C.EdgeMap.t'
        and type actionmap = C.Action.Map.t') :
  S
  with type state = C.State.t
   and type states = C.State.Set.t
   and type labels = C.Label.Set.t
   and type edgemap = C.EdgeMap.t'
   and type info = C.Info.t
   and type lts = LTS.t = struct
  module State = C.State
  module States = C.State.Set
  module Labels = C.Label.Set
  module EdgeMap = C.EdgeMap
  module Info = C.Info

  type state = State.t
  type states = States.t
  type labels = Labels.t
  type edgemap = EdgeMap.t'
  type info = Info.t
  type lts = LTS.t

  type t =
    { init : state option
    ; alphabet : labels
    ; states : states
    ; edges : edgemap
    ; terminals : states
    ; info : info
    ; fill : (state -> unit) option
      (** [Some f] for an FSM saturated on demand ({!saturate_on_demand}):
          [edges] holds only the states asked about so far, and [f s] adds
          [s]'s. Read such an FSM's edges for [s] only after {!ensure}. *)
    }

  include Json.Thing.Make (struct
      type k = t

      let name = "FSM"

      let json ?as_elt (x : t) : Yojson.t =
        `Assoc
          [ "init", Json.option ~as_elt:true State.json x.init
          ; "info", Info.json ~as_elt:true x.info
          ; "terminals", States.json ~as_elt:true x.terminals
          ; "alphabet", Labels.json ~as_elt:true x.alphabet
          ; "states", States.json ~as_elt:true x.states
          ; "edges", EdgeMap.json ~as_elt:true x.edges
          ]
      ;;
    end)

  let of_lts (x : LTS.t) : t =
    Logger.trace __FUNCTION__;
    { init = x.init
    ; terminals = x.terminals
    ; alphabet = x.alphabet
    ; states = x.states
    ; edges = EdgeMap.of_transitions x.transitions
    ; info = x.info
    ; fill = None
    }
  ;;

  let merge (a : t) (b : t) : t =
    let init : State.t option = None in
    let terminals : States.t = States.union a.terminals b.terminals in
    let alphabet : Labels.t = Labels.union a.alphabet b.alphabet in
    let states : States.t = States.union a.states b.states in
    let edges : EdgeMap.t' = EdgeMap.merge a.edges b.edges in
    let nums : Info.nums =
      { states = States.cardinal states
      ; labels = Labels.cardinal alphabet
      ; edges = EdgeMap.size edges
      }
    in
    let info : Info.t = Info.merge ~nums:(Some nums) a.info b.info in
    (* merging copies the edges held now, so a later fill would not reach
       the merged map: merge FSMs saturated on demand only by their originals *)
    let fill : (State.t -> unit) option = None in
    { init; terminals; alphabet; states; edges; info; fill }
  ;;

  let is_weak_mode (x : t) : bool =
    Bool.not (Labels.is_empty x.info.weak_labels)
  ;;

  let saturate ?(only_if_weak : bool = true) (x : t) : t =
    Logger.trace __FUNCTION__;
    if only_if_weak && Bool.not (is_weak_mode x)
    then (
      Logger.debug ~__FUNCTION__ "Not weak, returning unchanged";
      x)
    else (
      let edges, terminals' =
        Saturation.edges x.alphabet x.states (EdgeMap.copy x.edges)
      in
      { x with edges; terminals = States.union x.terminals terminals' })
  ;;

  let ensure (x : t) (s : State.t) : unit =
    match x.fill with None -> () | Some f -> f s
  ;;

  module StateTbl = Hashtbl.Make (C.State)

  let saturate_on_demand ?(budget : int = 1_000_000) (x : t) : t =
    Logger.trace __FUNCTION__;
    if Bool.not (is_weak_mode x)
    then x
    else (
      let compute = Saturation.on_demand (EdgeMap.copy x.edges) in
      let edges : EdgeMap.t' = EdgeMap.create 64 in
      (* states with no weak action: nothing to hold, but not to recompute *)
      let none : unit StateTbl.t = StateTbl.create 64 in
      let held : int ref = ref 0 in
      let order : State.t Queue.t = Queue.create () in
      let fill (s : State.t) : unit =
        if Bool.not (EdgeMap.mem edges s || StateTbl.mem none s)
        then (
          match compute s with
          | None -> StateTbl.replace none s ()
          | Some actions ->
            EdgeMap.replace edges s actions;
            held := !held + C.Action.Map.size actions;
            Queue.push s order;
            (* oldest first, never the state just added *)
            while !held > budget && Queue.length order > 1 do
              let old : State.t = Queue.pop order in
              match EdgeMap.find_opt edges old with
              | Some a ->
                held := !held - C.Action.Map.size a;
                EdgeMap.remove edges old
              | None -> ()
            done)
      in
      { x with edges; fill = Some fill })
  ;;
end
