module type S = sig
  type t
  type 'a mm
  type enc
  type tree
  type action
  type constructor
  type states

  val get_new_constrs : t -> enc -> constructor list mm
  val update_to_visit : t -> enc -> unit
  val update_transitions : t -> enc -> enc * tree -> action -> unit
  val get_action : t -> enc -> tree -> action mm
  val get_new_states : t -> enc -> states mm
  val stop : t -> bool
  val build : ?stop:(t -> bool) -> t -> t mm
end

module Make
    (Log : Logger.S)
    (Enc : Encoding.S)
    (M : Rocq_monad_utils.S with type enc = Enc.t and type tree = Enc.Tree.t)
    (Model :
       Model_.S
       with type base = Enc.t
        and type tree = Enc.Tree.t
        and type trees = Enc.Trees.t)
    (G :
       Graph_type.S
       with type enc = Enc.t
        and type tree = Enc.Tree.t
        and type action = Model.Action.t
        and type ind = M.Ind.t
        and module B = M.B
        and module F = M.F
        and type indmap = M.Ind.t M.B.t
        and type 'a mm = 'a M.mm) :
  S
  with type t = G.t
   and type 'a mm = 'a M.mm
   and type enc = Enc.t
   and type tree = Enc.Tree.t
   and type action = Model.Action.t
   and type constructor = M.Constructor.t
   and type states = G.States.t = struct
  module Action = Model.Action

  type t = G.t
  type 'a mm = 'a M.mm
  type enc = Enc.t
  type tree = Enc.Tree.t
  type action = Action.t
  type constructor = M.Constructor.t
  type states = G.States.t

  open G

  let get_new_constrs (g : t) (from : Enc.t) : M.Constructor.t list M.mm =
    Log.trace __FUNCTION__;
    M.Unification.collect_valid_constructors
      (M.Ind.get_lts_constructor_types g.primarylts)
      (M.decode_map g.ltsmap)
      (M.decode from)
      (M.Ind.get_lts_label_type g.primarylts)
      g.primarylts.enc
  ;;

  (** [update_to_visit g x] adds [x] to [g.to_visit] if [x] has not yet been explored {i (i.e., if [x] is not recorded in [g] as a state or as having a transition)}.
  *)
  let update_to_visit (g : t) (x : Enc.t) : unit =
    if Transitions.mem g.transitions x || States.mem x g.states
    then ()
    else update_to_visit g x
  ;;

  let update_transitions
        (g : t)
        (from : Enc.t)
        (goto : Enc.t * Enc.Tree.t)
        (a : Action.t)
    : unit
    =
    Destinations.singleton goto |> Transitions.update g.transitions from a
  ;;

  let get_action (g : t) (act : Enc.t) (int_tree : Enc.Tree.t) : Action.t mm =
    let act_dec : EConstr.t = M.decode act in
    let open M.Syntax in
    let* is_silent : bool option = is_silent_label act_dec g.weak in
    let label : Model.Label.t = { base = act; is_silent } in
    let trees : Enc.Trees.t = Enc.Trees.singleton int_tree in
    let action : Action.t = { label; trees; annotation = None } in
    M.return action
  ;;

  let get_new_states (g : t) (from : Enc.t) : States.t M.mm =
    Log.trace __FUNCTION__;
    let open M.Syntax in
    let* new_constrs : M.Constructor.t list = get_new_constrs g from in
    let iter_body (i : int) (new_states : States.t) =
      let (act, tgt, int_tree) : M.Constructor.t = List.nth new_constrs i in
      let* action : Action.t = get_action g act int_tree in
      update_transitions g from (tgt, int_tree) action;
      update_to_visit g tgt;
      M.return (States.add tgt new_states)
    in
    M.iterate 0 (List.length new_constrs - 1) (States.singleton from) iter_body
  ;;

  let stop (g : t) : bool =
    Log.trace __FUNCTION__;
    match g.bounds with
    | States n -> States.cardinal g.states > n
    | Transitions n -> Transitions.size g.transitions > n
  ;;

  let rec build ?(stop : t -> bool = stop) (g : t) : t M.mm =
    Log.trace __FUNCTION__;
    if stop g
    then M.return g
    else (
      try
        let enc_to_visit : Enc.t = next_to_visit g in
        let open M.Syntax in
        (* NOTE: [get_new_states] also updates [g.to_visit] *)
        let* new_states : States.t = get_new_states g enc_to_visit in
        update_states g new_states |> build
      with
      | NoMoreToVisit -> M.return g)
  ;;
end
