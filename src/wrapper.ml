module Make
    (Log : Logger.SLogger)
    (C : Rocq_context.SRocq_context)
    (E : Encoding.SEncoding) =
struct
  module M = Rocq_monad_utils.Make (Log) (C) (E)
  module Model = Model.Make (Log) (M.Enc)

  (** *)
  module IsTheory = struct
    (** [is_theory x y] checks if term [x] is equal to theory term [y], catching the exception thrown when [EConstr.kind_of_type x] is not [AtomicType (ty, tys)].
    *)
    let is_theory (x : EConstr.t) (y : EConstr.t) : bool M.mm =
      try
        let open M.Syntax in
        let* sigma = M.get_sigma in
        Rocq_utils.econstr_to_atomic sigma x |> fst |> M.econstr_eq y
      with
      | Rocq_utils.Rocq_utils_EConstrIsNotA_Type _ -> M.return false
    ;;

    (** exists *)
    let is_exists (x : EConstr.t) : bool M.mm = is_theory x (Theories.c_ex ())

    (** weak simulation*)
    let is_weak_sim (x : EConstr.t) : bool M.mm =
      is_theory x (Theories.c_weak_sim ())
    ;;

    (** weak transition *)
    let is_weak (x : EConstr.t) : bool M.mm = is_theory x (Theories.c_weak ())

    let is_tau (x : EConstr.t) : bool M.mm = is_theory x (Theories.c_tau ())

    let is_silent (x : EConstr.t) : bool M.mm =
      is_theory x (Theories.c_silent ())
    ;;

    let is_silent1 (x : EConstr.t) : bool M.mm =
      is_theory x (Theories.c_silent1 ())
    ;;

    let is_LTS (x : EConstr.t) : bool M.mm = is_theory x (Theories.c_LTS ())
    let is_None (x : EConstr.t) : bool M.mm = is_theory x (Theories.c_None ())
    let is_Some (x : EConstr.t) : bool M.mm = is_theory x (Theories.c_Some ())
    (* let is_ (x : EConstr.t) : bool M.mm = is_theory x (Theories.c_ ()) *)

    (** *)
    let get_theory_enc (f : EConstr.t -> bool M.mm) : M.Enc.t M.mm =
      Log.trace __FUNCTION__;
      let open M.Syntax in
      let* fm = M.get_fwdmap in
      let rec find_theory : (EConstr.t * M.Enc.t) list -> M.Enc.t M.mm =
        function
        | [] -> raise Not_found
        | (x, y) :: tl ->
          let is_match : bool = M.run (f x) in
          if is_match then M.return y else find_theory tl
      in
      M.F.to_seq fm |> List.of_seq |> find_theory
    ;;

    exception NoEncodingFoundFor_TheoriesNone of unit

    let get_None_enc () : M.Enc.t M.mm =
      Log.trace __FUNCTION__;
      try get_theory_enc is_None with
      | Not_found -> raise (NoEncodingFoundFor_TheoriesNone ())
    ;;

    exception NoEncodingFoundFor_TheoriesSome of unit

    let get_Some_enc () : M.Enc.t M.mm =
      Log.trace __FUNCTION__;
      try get_theory_enc is_Some with
      | Not_found -> raise (NoEncodingFoundFor_TheoriesSome ())
    ;;

    exception NotEqTheory of unit

    (** *)
    let get_theory_enc_if_eq (x : EConstr.t) (f : EConstr.t -> bool M.mm)
      : M.Enc.t M.mm
      =
      Log.trace __FUNCTION__;
      let is_eq : bool = M.run (f x) in
      try if is_eq then get_theory_enc f else raise Not_found with
      | Not_found -> raise (NotEqTheory ())
    ;;

    let get_None_enc_if_eq (x : EConstr.t) : M.Enc.t M.mm =
      Log.trace __FUNCTION__;
      get_theory_enc_if_eq x is_None
    ;;

    let get_Some_enc_if_eq (x : EConstr.t) : M.Enc.t M.mm =
      Log.trace __FUNCTION__;
      get_theory_enc_if_eq x is_Some
    ;;
  end

  module Weak = struct
    type t =
      | Option of M.Enc.t
      | Custom of M.Enc.t * M.Enc.t

    let eq x y : bool =
      Log.trace __FUNCTION__;
      match x, y with
      | Option x, Option y -> M.Enc.equal x y
      | Custom (x1, x2), Custom (y1, y2) ->
        M.Enc.equal x1 y1 && M.Enc.equal x2 y2
      | _, _ -> false
    ;;

    let to_string : t -> string M.mm =
      Log.trace __FUNCTION__;
      function
      | Option label_enc ->
        let label_dec : EConstr.t = M.decode label_enc in
        let label_enc : string = M.Enc.to_string label_enc in
        let label_dec : string = M.Strfy.econstr label_dec in
        Printf.sprintf "Option (%s) -> %s" label_enc label_dec |> M.return
      | Custom (tau_enc, label_enc) ->
        let tau_dec : EConstr.t = M.decode tau_enc in
        let tau_enc : string = M.Enc.to_string tau_enc in
        let tau_dec : string = M.Strfy.econstr tau_dec in
        let label_dec : EConstr.t = M.decode label_enc in
        let label_enc : string = M.Enc.to_string label_enc in
        let label_dec : string = M.Strfy.econstr label_dec in
        let a : string = Printf.sprintf "- tau (%s) -> %s" tau_enc tau_dec in
        let b : string =
          Printf.sprintf "- label (%s) -> %s" label_enc label_dec
        in
        Printf.sprintf "Custom\n%s\n%s" a b |> M.return
    ;;
  end

  module type X_Args = sig
    val primary_lts : Libnames.qualid
    val grefs : Names.GlobRef.t list
    val weak : Weak.t option
    val bounds : Model.Info.bounds
  end

  module Graph
      (T0 : Hashtbl.S with type key = M.Enc.t)
      (V0 : Set.S with type elt = M.Enc.t)
      (D0 : Set.S with type elt = M.Enc.t * M.Tree.t)
      (X : X_Args) =
  struct
    (** [module S] is a [Graph] alternative to [module Model.States] for tracking the visited states.
    *)
    module V = struct
      module V2 : Set.S with type elt = M.Enc.t = V0
      include V2

      let to_string (xs : t) : string =
        Log.trace __FUNCTION__;
        "TODO: Graph.V.to_string"
      ;;
    end

    (** [module D] is similar to [module S], but each "destination state" is paired with a constructor tree detailing which constructors to take to reach it, which in the context of [module A] and [module T] later illustrates how to get from one state to another via certain constructors.
    *)
    module D = struct
      module D2 : Set.S with type elt = M.Enc.t * M.Tree.t = D0
      include D2

      let to_string (xs : t) : string =
        Log.trace __FUNCTION__;
        "TODO: Graph.D.to_string"
      ;;
    end

    (** [module A] is a [Graph] alternative to [module Model.ActionMap] *)
    module A = struct
      module A2 : Hashtbl.S with type key = Model.Action.t =
        Hashtbl.Make (Model.Action)

      include A2

      type t' = D.t t

      let size (xs : t') : int =
        Log.trace __FUNCTION__;
        fold (fun k v n -> D.cardinal v + n) xs 0
      ;;

      let update (x : t') (action : Model.Action.t) (states : D.t) : unit =
        Log.trace __FUNCTION__;
        if D.is_empty states
        then ()
        else (
          match find_opt x action with
          | None -> add x action states
          | Some old_states -> replace x action (D.union old_states states))
      ;;

      let to_string (xs : t') : string =
        Log.trace __FUNCTION__;
        to_seq xs
        |> List.of_seq
        |> Utils.Strfy.list
             ~args:
               { (Utils.Strfy.style_args ()) with name = Some "Graph.Actions" }
             (Of
                (fun (k, v) ->
                  Utils.Strfy.record
                    [ "action", Model.Action.to_string k; "->", D.to_string v ]))
      ;;
    end

    (** [module T] is a [Graph] alternative to [module Model.EdgeMap], but for transitions.
    *)
    module T = struct
      module T2 : Hashtbl.S with type key = M.Enc.t = T0
      include T2

      type t' = A.t' t

      let update
            (x : t')
            (from : M.Enc.t)
            (action : Model.Action.t)
            (destinations : D.t)
        : unit
        =
        Log.trace __FUNCTION__;
        match find_opt x from with
        | None ->
          [ action, destinations ] |> List.to_seq |> A.of_seq |> add x from
        | Some actions -> A.update actions action destinations
      ;;

      let size (xs : t') : int =
        Log.trace __FUNCTION__;
        fold (fun k v n -> A.size v + n) xs 0
      ;;

      let to_string (xs : t') : string =
        Log.trace __FUNCTION__;
        to_seq xs
        |> List.of_seq
        |> Utils.Strfy.list
             ~args:
               { (Utils.Strfy.style_args ()) with
                 name = Some "Graph.Transitions"
               }
             (Of
                (fun (k, v) ->
                  Utils.Strfy.record
                    [ "from", M.Enc.to_string k; "->", A.to_string v ]))
      ;;
    end

    (** [t] is a record containing a queue of [EConstr.t]s [to_visit], a set of states visited (i.e., [EConstr.t]s), and a hashtbl mapping [EConstr.t] to a map of [constr_transitions], which maps [action]s to [EConstr.t]s and their [Tree.t].
    *)
    type t =
      { to_visit : M.Enc.t Queue.t
      ; init : M.Enc.t
      ; states : V.t
      ; transitions : T.t'
      ; ind_defs : M.Enc.t Rocq_ind.t M.B.t
      ; weak : Weak.t option
      }

    (** [empty init ind_defs weak] is the initial (empty) graph type [t].
        @param init is the initial state.
        @param rocq_defs is a map of rocq inductive definitions of the LTS.
        @param weak is the optional weak arguments. *)
    let empty (init : M.Enc.t) (ind_defs : M.Enc.t Rocq_ind.t M.B.t) : t =
      Log.trace __FUNCTION__;
      { to_visit = Queue.create ()
      ; init
      ; states = V.empty
      ; transitions = T.create 0
      ; ind_defs
      ; weak = X.weak
      }
    ;;

    (*********************************************************)

    let is_silent_transition (x : EConstr.t) : Weak.t option -> bool option M.mm
      =
      Log.trace __FUNCTION__;
      function
      | None -> M.return None
      | Some (Option label_enc) ->
        (* let label_decoding : EConstr.t = M.decode label_enc in *)
        let open M.Syntax in
        let* b : bool = IsTheory.is_None x in
        M.return (Some b)
      | Some (Custom (tau_enc, label_enc)) ->
        (* let tau_decoding : EConstr.t = M.decode tau_enc in *)
        (* let label_decoding : EConstr.t = M.decode label_enc in *)
        let act_enc : M.Enc.t = M.encode x in
        M.return (Some (M.Enc.equal tau_enc act_enc))
    ;;

    (*********************************************************)

    module type Y_Args = sig
      val primary_lts : M.Enc.t Rocq_ind.t
      val rocq_defs : M.Enc.t Rocq_ind.t M.B.t
      val stop : unit -> bool
      val g : t ref
    end

    module type Z_Args = sig
      val pp : bool
      val g : t ref
      val ind_defs : M.Enc.t Rocq_ind.t M.B.t
    end

    module Make (Y : Y_Args) = struct
      let next_to_visit () : M.Enc.t =
        Log.trace __FUNCTION__;
        Queue.pop !Y.g.to_visit
      ;;

      let update_to_visit (xs : V.t) : unit =
        Log.trace __FUNCTION__;
        Y.g := { !Y.g with states = V.union !Y.g.states xs }
      ;;

      let get_new_states (from : M.Enc.t) (ctors : M.Constructor.t list)
        : V.t M.mm
        =
        Log.trace __FUNCTION__;
        let iter_body (i : int) (new_states : V.t) =
          let (act, tgt, int_tree) : M.Constructor.t = List.nth ctors i in
          let tgt_enc : M.Enc.t = M.encode tgt in
          let act_enc : M.Enc.t = M.encode act in
          let open M.Syntax in
          let* is_silent : bool option = is_silent_transition act !Y.g.weak in
          let label : Model.Label.t =
            { term = act_enc; pp = None; is_silent }
          in
          let constructor_trees : Model.Trees.t =
            Model.Trees.singleton int_tree
          in
          let to_add : Model.Action.t =
            { label; constructor_trees; annotation = None }
          in
          D.singleton (tgt_enc, int_tree)
          |> T.update !Y.g.transitions from to_add;
          (* NOTE: if [tgt] has not been explored then add [to_visit] *)
          if T.mem !Y.g.transitions tgt_enc || V.mem tgt_enc !Y.g.states
          then ()
          else Queue.push tgt_enc !Y.g.to_visit;
          (* NOTE: add [tgt] to [new_states] *)
          M.return (V.add tgt_enc new_states)
        in
        M.iterate 0 (List.length ctors - 1) (V.singleton from) iter_body
      ;;

      let get_new_constrs (from : M.Enc.t) : M.Constructor.t list M.mm =
        Log.trace __FUNCTION__;
        let from_term : EConstr.t = M.decode from in
        let label_type : EConstr.t =
          Rocq_ind.get_lts_label_type Y.primary_lts
        in
        let ind_map : M.Enc.t Rocq_ind.t M.F.t = M.decode_map Y.rocq_defs in
        let primary_constr_transitions =
          Rocq_ind.get_lts_constructor_types Y.primary_lts
        in
        M.Unification.collect_valid_constructors
          primary_constr_transitions
          ind_map
          from_term
          label_type
          Y.primary_lts.enc
      ;;

      (** *)
      let rec build () : bool M.mm =
        Log.trace __FUNCTION__;
        if Y.stop ()
        then Queue.is_empty !Y.g.to_visit |> M.return
        else (
          let enc_to_visit : M.Enc.t = Queue.pop !Y.g.to_visit in
          (* status_update enc_to_visit g bound; *)
          let open M.Syntax in
          let* new_constrs : M.Constructor.t list =
            next_to_visit () |> get_new_constrs
          in
          (* NOTE: [get_new_states] also updates [g.to_visit] *)
          let* new_states : V.t = get_new_states enc_to_visit new_constrs in
          update_to_visit new_states;
          build ())
      ;;
    end

    module Extract (Z : Z_Args) = struct
      let pp (x : M.Enc.t) : string option =
        Log.trace __FUNCTION__;
        if Z.pp then Some (M.decode x |> M.Strfy.econstr) else None
      ;;

      let state (x : M.Enc.t) : Model.State.t =
        Log.trace __FUNCTION__;
        { term = x; pp = pp x }
      ;;

      let states () : Model.States.t =
        Log.trace __FUNCTION__;
        !Z.g.states |> V.to_list |> List.map state |> Model.States.of_list
      ;;

      let terminals () : Model.States.t =
        Log.trace __FUNCTION__;
        !Z.g.states
        |> V.filter (fun (x : V.elt) -> Bool.not (T.mem !Z.g.transitions x))
        |> V.to_list
        |> List.map state
        |> Model.States.of_list
      ;;

      let transitions () : Model.Transitions.t =
        Log.trace __FUNCTION__;
        T.fold
          (fun (from : M.Enc.t)
            (vs : A.t')
            : (Model.Transitions.t -> Model.Transitions.t) ->
            let from : Model.State.t = state from in
            A.fold
              (fun (action : Model.Action.t)
                (vs : D.t)
                : (Model.Transitions.t -> Model.Transitions.t) ->
                let label : Model.Label.t = action.label in
                D.fold
                  (fun ((goto, constructor_tree) : M.Enc.t * M.Tree.t)
                    : (Model.Transitions.t -> Model.Transitions.t) ->
                    let goto : Model.State.t = state goto in
                    let open Model.Transition in
                    Model.Transitions.add
                      { from; goto; label; constructor_tree; annotation = None })
                  vs)
              vs)
          !Z.g.transitions
          Model.Transitions.empty
      ;;

      let constructor_info () : Model.Info.lts list M.mm =
        Log.trace __FUNCTION__;
        let xs = M.B.to_seq Z.ind_defs |> List.of_seq in
        let open M.Syntax in
        let f (i : int) (acc : Model.Info.lts list) =
          let (enc, v) : M.Enc.t * M.Enc.t Rocq_ind.t = List.nth xs i in
          match v.kind with
          | LTS x ->
            let* constructors : Rocq_bindings.constructor list =
              M.state (fun env sigma -> Rocq_bindings.extract_info env sigma v)
            in
            let open Model.Info in
            { enc; constructors } :: acc |> M.return
          | _ -> M.return acc
        in
        M.iterate 0 (List.length xs - 1) [] f
      ;;

      let meta () : Model.Info.meta M.mm =
        Log.trace __FUNCTION__;
        let open M.Syntax in
        let* lts : Model.Info.lts list = constructor_info () in
        let open Model.Info in
        { is_complete = Queue.is_empty !Z.g.to_visit
        ; is_merged = false
        ; bounds = X.bounds
        ; lts
        }
        |> M.return
      ;;

      let weak_labels (xs : Model.Labels.t) : Model.Labels.t M.mm =
        Log.trace __FUNCTION__;
        match !Z.g.weak with
        | None -> Model.Labels.empty |> M.return
        | Some weak ->
          let f : M.Enc.t -> bool M.mm =
            match weak with
            | Weak.Option x ->
              fun (y : M.Enc.t) -> M.decode y |> IsTheory.is_None
            | Weak.Custom (tau_enc, _) ->
              fun (y : M.Enc.t) -> M.Enc.equal tau_enc y |> M.return
          in
          let open M.Syntax in
          let xs : Model.Label.t list = Model.Labels.to_list xs in
          let g (i : int) (acc : Model.Labels.t) =
            let x : Model.Label.t = List.nth xs i in
            let* is_weak : bool = f x.term in
            if is_weak then Model.Labels.add x acc |> M.return else M.return acc
          in
          M.iterate 0 (List.length xs - 1) Model.Labels.empty g
      ;;

      let lts () : Model.LTS.t M.mm =
        Log.trace __FUNCTION__;
        let open M.Syntax in
        let transitions : Model.Transitions.t = transitions () in
        let alphabet : Model.Labels.t = Model.Transitions.labels transitions in
        let* meta : Model.Info.meta = meta () in
        let* weak_labels : Model.Labels.t = weak_labels alphabet in
        let open Model.LTS in
        { init = Some (state !Z.g.init)
        ; terminals = terminals ()
        ; alphabet
        ; states = states ()
        ; transitions
        ; info = { meta = Some meta; weak_labels }
        }
        |> M.return
      ;;
    end

    let build_ind_defs () : M.Enc.t Rocq_ind.t M.B.t M.mm =
      Log.trace __FUNCTION__;
      let num : int = List.length X.grefs in
      Log.thing ~__FUNCTION__ Notice "num" num (Of Utils.Strfy.int);
      let ind_defs : M.Enc.t Rocq_ind.t M.B.t = M.B.create num in
      let open M.Syntax in
      let f (i : int) () =
        let gref : Names.GlobRef.t = List.nth X.grefs i in
        let* x : M.Enc.t Rocq_ind.t = M.Ind.lts gref in
        M.B.replace ind_defs x.enc x;
        M.return ()
      in
      let* () = M.iterate 0 (num - 1) () f in
      ind_defs |> M.return
    ;;

    let find_primary_lts (ind_defs : M.Enc.t Rocq_ind.t M.B.t)
      : M.Enc.t Rocq_ind.t M.mm
      =
      Log.trace __FUNCTION__;
      let open M.Syntax in
      let* x : M.Enc.t Rocq_ind.t = Nametab.global X.primary_lts |> M.Ind.lts in
      (* NOTE: catch-all sanity check *)
      M.encode x.ind |> M.B.find ind_defs |> M.return
    ;;

    (** normalize and encode the initial term *)
    let initial_term (init_term : Constrexpr.constr_expr) : M.Enc.t M.mm =
      Log.trace __FUNCTION__;
      let open M.Syntax in
      let* init_term : EConstr.t = M.constrexpr_to_econstr init_term in
      let* init_term : EConstr.t = M.econstr_normalize init_term in
      let init : M.Enc.t = M.encode init_term in
      M.return init
    ;;

    let make_yargs primary_lts ind_defs the_graph : (module Y_Args) =
      Log.trace __FUNCTION__;
      let module Y = struct
        let primary_lts : M.Enc.t Rocq_ind.t = primary_lts
        let rocq_defs : M.Enc.t Rocq_ind.t M.B.t = ind_defs
        let g : t ref = the_graph

        let stop : unit -> bool =
          match X.bounds with
          | States n ->
            fun () -> Queue.is_empty !g.to_visit || V.cardinal !g.states > n
          | Transitions n ->
            fun () -> Queue.is_empty !g.to_visit || T.size !g.transitions > n
        ;;
      end
      in
      (module Y : Y_Args)
    ;;

    let make_zargs ind_defs the_graph : (module Z_Args) =
      Log.trace __FUNCTION__;
      let module Z = struct
        let pp : bool = false
        let ind_defs : M.Enc.t Rocq_ind.t M.B.t = ind_defs
        let g : t ref = the_graph
      end
      in
      (module Z : Z_Args)
    ;;

    let build (init_term : Constrexpr.constr_expr) : Model.LTS.t M.mm =
      Log.trace __FUNCTION__;
      let open M.Syntax in
      let* init : M.Enc.t = initial_term init_term in
      (* NOTE: encode rocq inductive defs *)
      let* ind_defs : M.Enc.t Rocq_ind.t M.B.t = build_ind_defs () in
      Log.things
        ~__FUNCTION__
        Notice
        "ind_defs"
        (M.B.to_seq ind_defs |> List.of_seq)
        (Of
           (fun (k, v) : string ->
             Utils.Strfy.record
               [ "enc", M.Enc.to_string k
               ; "enc'", M.Enc.to_string v.enc
               ; "ind", M.Strfy.econstr v.ind
               ; ("kind", match v.kind with Type _ -> "Type" | LTS _ -> "LTS")
               ]));
      let* primary_lts = find_primary_lts ind_defs in
      (* NOTE: build the graph *)
      Log.trace ~__FUNCTION__ "Build the Graph";
      let the_graph : t ref = ref (empty init ind_defs) in
      let module G = Make ((val make_yargs primary_lts ind_defs the_graph)) in
      let* is_complete : bool = G.build () in
      (* M.return !the_graph *)
      Log.trace ~__FUNCTION__ "Completed Graph, Extracting LTS";
      let module L = Extract ((val make_zargs ind_defs the_graph)) in
      let* the_lts : Model.LTS.t = L.lts () in
      M.return the_lts
    ;;

    (* let extract_lts (init_term : Constrexpr.constr_expr) : Model.LTS.t M.mm =
       let open M.Syntax in
       let* the_graph = build init_term in
       let module L = Extract ((val make_zargs ind_defs the_graph)) in
       let* the_lts : Model.LTS.t = L.lts () in
       M.return the_lts *)
  end

  let make_xargs primary_lts grefs weak bounds : (module X_Args) =
    Log.trace __FUNCTION__;
    let module X = struct
      let primary_lts : Libnames.qualid = primary_lts
      let grefs = grefs
      let weak = weak
      let bounds = bounds
    end
    in
    (module X : X_Args)
  ;;

  let extract_lts
        (primary_lts : Libnames.qualid)
        (init : Constrexpr.constr_expr)
        (names : Libnames.qualid list)
        (weak : Weak.t option)
        (bounds : Model.Info.bounds)
    : Model.LTS.t M.mm
    =
    Log.trace __FUNCTION__;
    let t = M.make_hashtbl in
    let v = M.make_set in
    let d = M.make_state_tree_pair_set in
    Log.thing
      ~__FUNCTION__
      Notice
      "names num"
      (List.length names)
      (Of Utils.Strfy.int);
    let grefs : Names.GlobRef.t list =
      Rocq_utils.libnames_to_globrefs (primary_lts :: names)
    in
    let x = make_xargs primary_lts grefs weak bounds in
    let module G = Graph ((val t)) ((val v)) ((val d)) ((val x)) in
    G.build init
  ;;

  module Command = struct
    let build_lts
          (primary_lts : Libnames.qualid)
          (init : Constrexpr.constr_expr)
          (names : Libnames.qualid list)
          (weak : Weak.t option)
          (bounds : Model.Info.bounds)
      : Model.LTS.t M.mm
      =
      Log.trace __FUNCTION__;
      extract_lts primary_lts init names weak bounds
    ;;

    let build_fsm
          (primary_lts : Libnames.qualid)
          (init : Constrexpr.constr_expr)
          (names : Libnames.qualid list)
          (weak : Weak.t option)
          (bounds : Model.Info.bounds)
      : Model.FSM.t M.mm
      =
      Log.trace __FUNCTION__;
      let open M.Syntax in
      let* the_lts = extract_lts primary_lts init names weak bounds in
      Model.Convert.lts_to_fsm the_lts |> M.return
    ;;

    type t =
      | MakeLTS of rocq_args
      | MakeFSM of rocq_args

    (* | Saturate of rocq_args *)
    (* | Merge of rocq_pair *)
    (* | Minimize of rocq_args *)
    (* | CheckBisim of rocq_pair *)

    (* | Info of unit  *)
    (* | Help of ...  *)
    and rocq_args = Constrexpr.constr_expr * Libnames.qualid

    and rocq_pair =
      { a : rocq_args
      ; b : rocq_args
      }

    let run
          (refs : Libnames.qualid list)
          (weak : Weak.t option)
          (bounds : Model.Info.bounds)
      : t -> 'a M.mm
      =
      Log.trace __FUNCTION__;
      let open M.Syntax in
      function
      | MakeLTS (x, primary_lts) ->
        let* the_lts = build_lts primary_lts x refs weak bounds in
        M.return ()
      | MakeFSM (x, primary_lts) ->
        let* the_fsm = build_fsm primary_lts x refs weak bounds in
        M.return ()
    ;;
    (* | Saturate args -> () *)
    (* | Merge args -> () *)
    (* | Minimize args -> () *)
    (* | CheckBisim args -> () *)
  end
end
