module type S = sig
  type t
  type lts
  type 'a mm

  val extract : t -> lts mm
end

module Make
    (Enc : Encoding.S)
    (M : Rocq_monad_utils.S with type enc = Enc.t and type tree = Enc.Tree.t)
    (Weak : Weak.S with type enc = Enc.t)
    (Theory :
       Theories_enc.S
       with type enc = Enc.t
        and type 'a mm = 'a M.mm
        and type 'a im = 'a M.mm)
    (ConstructorBindings :
       Constructor_bindings.S with type 'a mm = 'a M.mm and type ind = M.Ind.t)
    (Model :
       Model.S
       with type base = Enc.t
        and type tree = Enc.Tree.t
        and type trees = Enc.Trees.t
        and type constructorbindings = ConstructorBindings.t)
    (X : Graph_type.Args with type enc = Enc.t and type tree = Enc.Tree.t)
    (G :
       Graph_type.S
       with type enc = Enc.t
        and type tree = Enc.Tree.t
        and type action = Model.Action.t
        and type weak = Weak.t
        and type ind = M.Ind.t
        and module B = M.B
        and module F = M.F
        and type indmap = M.Ind.t M.B.t
        and type 'a mm = 'a M.mm) :
  S with type t = G.t and type lts = Model.LTS.t and type 'a mm = 'a M.mm =
struct
  type t = G.t
  type lts = Model.LTS.t
  type 'a mm = 'a M.mm

  open Model

  let state (x : Enc.t) : State.t = { base = x }

  let states (xs : G.States.t) : State.Set.t =
    xs |> G.States.to_list |> List.map state |> State.Set.of_list
  ;;

  let terminals (xs : G.States.t) (ys : G.Transitions.t') : State.Set.t =
    xs
    |> G.States.filter (fun (x : Enc.t) -> Bool.not (G.Transitions.mem ys x))
    |> G.States.to_list
    |> List.map state
    |> State.Set.of_list
  ;;

  let label (x : Action.t) : Label.t = x.label

  let transitions (xs : G.Transitions.t') : Model.Transition.Set.t =
    let goto (from : State.t) (label : Label.t) (goto, tree)
      : Transition.Set.t -> Transition.Set.t
      =
      let goto : State.t = state goto in
      Transition.Set.add { from; goto; label; tree = Some tree; annotation = None }
    in
    let action (from : State.t) (action : Action.t)
      : G.Destinations.t -> Transition.Set.t -> Transition.Set.t
      =
      G.Destinations.fold (goto from (label action))
    in
    let from (from : Enc.t) : G.Actions.t' -> Transition.Set.t -> Transition.Set.t =
      G.Actions.fold (action (state from))
    in
    G.Transitions.fold from xs Transition.Set.empty
  ;;

  let constructor_info (g : G.t) : Model.Info.Meta.RocqLTS.t list M.mm =
    Logger.trace __FUNCTION__;
    let xs = M.B.to_seq g.ltsmap |> List.of_seq in
    let open M.Syntax in
    let f (i : int) (acc : Model.Info.Meta.RocqLTS.t list) =
      let (enc, v) : Enc.t * M.Ind.t = List.nth xs i in
      match v.kind with
      | LTS x ->
        let* constructors = ConstructorBindings.extract_info v in
        let open Model.Info.Meta.RocqLTS in
        { base = enc; constructors } :: acc |> M.return
      | _ -> M.return acc
    in
    M.iterate 0 (List.length xs - 1) [] f
  ;;

  let meta (g : G.t) : Info.Meta.t M.mm =
    Logger.trace __FUNCTION__;
    let open M.Syntax in
    let* lts : Info.Meta.RocqLTS.t list = constructor_info g in
    let x : Info.Meta.t =
      { is_complete = Queue.is_empty g.to_visit
      ; is_merged = false
      ; bounds =
          (match X.bounds with
           | States n -> States n
           | Transitions n -> Transitions n)
      ; lts
      }
    in
    M.return x
  ;;

  let weak_labels (g : G.t) (xs : Label.Set.t) : Label.Set.t M.mm =
    Logger.trace __FUNCTION__;
    match g.weak with
    | None -> Label.Set.empty |> M.return
    | Some weak ->
      let f : Enc.t -> bool M.mm =
        match weak with
        | Weak.Option x -> fun (y : Enc.t) -> M.decode y |> Theory.is_None
        | Weak.Custom (tau_enc, _) ->
          fun (y : Enc.t) -> Enc.equal tau_enc y |> M.return
      in
      let open M.Syntax in
      let xs : Label.t list = Label.Set.to_list xs in
      let g (i : int) (acc : Label.Set.t) =
        let x : Label.t = List.nth xs i in
        let* is_weak : bool = f x.base in
        if is_weak then Label.Set.add x acc |> M.return else M.return acc
      in
      M.iterate 0 (List.length xs - 1) Label.Set.empty g
  ;;

  let extract (g : G.t) : LTS.t M.mm =
    Logger.trace __FUNCTION__;
    let states : State.Set.t = states g.states in
    let terminals : State.Set.t = terminals g.states g.transitions in
    let transitions : Transition.Set.t = transitions g.transitions in
    let alphabet : Label.Set.t = Transition.Set.labels transitions in
    let open M.Syntax in
    let* meta : Info.Meta.t = meta g in
    let* weak_labels : Label.Set.t = weak_labels g alphabet in
    let x : LTS.t =
      { init = Some (state g.init)
      ; terminals
      ; alphabet
      ; states
      ; transitions
      ; info =
          { meta = Some meta
          ; weak_labels
          ; nums =
              Some
                { states = State.Set.cardinal states
                ; labels = Label.Set.cardinal alphabet
                ; edges = Transition.Set.cardinal transitions
                }
          }
      }
    in
    M.return x
  ;;
end
