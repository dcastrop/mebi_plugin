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
      (* Log.trace __FUNCTION__; *)
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
      (* Log.trace __FUNCTION__; *)
      try get_theory_enc is_None with
      | Not_found -> raise (NoEncodingFoundFor_TheoriesNone ())
    ;;

    exception NoEncodingFoundFor_TheoriesSome of unit

    let get_Some_enc () : M.Enc.t M.mm =
      (* Log.trace __FUNCTION__; *)
      try get_theory_enc is_Some with
      | Not_found -> raise (NoEncodingFoundFor_TheoriesSome ())
    ;;

    exception NotEqTheory of unit

    (** *)
    let get_theory_enc_if_eq (x : EConstr.t) (f : EConstr.t -> bool M.mm)
      : M.Enc.t M.mm
      =
      (* Log.trace __FUNCTION__; *)
      let is_eq : bool = M.run (f x) in
      try if is_eq then get_theory_enc f else raise Not_found with
      | Not_found -> raise (NotEqTheory ())
    ;;

    let get_None_enc_if_eq (x : EConstr.t) : M.Enc.t M.mm =
      (* Log.trace __FUNCTION__; *)
      get_theory_enc_if_eq x is_None
    ;;

    let get_Some_enc_if_eq (x : EConstr.t) : M.Enc.t M.mm =
      (* Log.trace __FUNCTION__; *)
      get_theory_enc_if_eq x is_Some
    ;;
  end

  module Weak = struct
    type t =
      | Option of M.Enc.t
      | Custom of M.Enc.t * M.Enc.t

    let eq x y : bool =
      (* Log.trace __FUNCTION__; *)
      match x, y with
      | Option x, Option y -> M.Enc.equal x y
      | Custom (x1, x2), Custom (y1, y2) ->
        M.Enc.equal x1 y1 && M.Enc.equal x2 y2
      | _, _ -> false
    ;;

    let to_string : t -> string M.mm =
      (* Log.trace __FUNCTION__; *)
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

  (** Config *)
  module Config = struct
    let load_weak_arg : Api.weak_arg -> Weak.t M.mm =
      let open M.Syntax in
      function
      | Api.Option label_tref ->
        let* label : EConstr.t = M.constrexpr_to_econstr label_tref in
        let label_enc : M.Enc.t = M.encode label in
        (* NOTE: sanity check we can decode these *)
        let _ : EConstr.t = M.decode label_enc in
        Weak.Option label_enc |> M.return
      | Api.Custom (tau_tref, label_ref) ->
        let* tau : EConstr.t = M.constrexpr_to_econstr tau_tref in
        let tau_enc : M.Enc.t = M.encode tau in
        let* ind, (mib, mip) =
          Nametab.global label_ref |> M.Ind.lts_type_mind
        in
        let label : EConstr.t = Rocq_utils.get_ind_ty ind mib in
        let label_enc : M.Enc.t = M.encode label in
        (* NOTE: sanity check we can decode these *)
        let _ : EConstr.t = M.decode tau_enc in
        let _ : EConstr.t = M.decode label_enc in
        Weak.Custom (tau_enc, label_enc) |> M.return
    ;;

    let load_weak_arg_opt : Api.weak_arg option -> Weak.t option M.mm = function
      | None -> M.return None
      | Some x ->
        let open M.Syntax in
        let* y = load_weak_arg x in
        M.return (Some y)
    ;;

    type weak_args =
      { a : Weak.t option
      ; b : Weak.t option
      }

    let the_weak_args : weak_args ref option ref = ref None
    let reset_the_weak_args () : unit = the_weak_args := None

    let load_weak_args () : unit M.mm =
      let open M.Syntax in
      match !Api.the_weak_args with
      | None ->
        the_weak_args := None;
        M.return ()
      | Some x ->
        let* a = load_weak_arg_opt !x.a in
        let* b = load_weak_arg_opt !x.b in
        the_weak_args := Some (ref { a; b });
        M.return ()
    ;;

    let get_the_weak_args : weak_args option =
      match !the_weak_args with None -> None | Some x -> Some !x
    ;;

    let get_the_weak_arg1 () : Weak.t option =
      match get_the_weak_args with None -> None | Some x -> x.a
    ;;

    let get_the_weak_arg2 () : Weak.t option =
      match get_the_weak_args with None -> None | Some x -> x.b
    ;;

    (***********************************************************************)

    let api_bounds_to_model_bounds : Api.bounds_args -> Model.Info.bounds =
      function
      | Api.States x -> Model.Info.States x
      | Api.Transitions x -> Model.Info.Transitions x
    ;;

    let the_bounds_args : Model.Info.bounds ref =
      ref (Api.default_bounds |> api_bounds_to_model_bounds)
    ;;

    let load_the_bounds_args () : unit =
      the_bounds_args := api_bounds_to_model_bounds !Api.the_bounds_args
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
        (* Log.trace __FUNCTION__; *)
        "TODO: Graph.V.to_string"
      ;;
    end

    (** [module D] is similar to [module S], but each "destination state" is paired with a constructor tree detailing which constructors to take to reach it, which in the context of [module A] and [module T] later illustrates how to get from one state to another via certain constructors.
    *)
    module D = struct
      module D2 : Set.S with type elt = M.Enc.t * M.Tree.t = D0
      include D2

      let to_string (xs : t) : string =
        (* Log.trace __FUNCTION__; *)
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
        (* Log.trace __FUNCTION__; *)
        fold (fun k v n -> D.cardinal v + n) xs 0
      ;;

      let update (x : t') (action : Model.Action.t) (states : D.t) : unit =
        (* Log.trace __FUNCTION__; *)
        if D.is_empty states
        then ()
        else (
          match find_opt x action with
          | None -> add x action states
          | Some old_states -> replace x action (D.union old_states states))
      ;;

      let to_string (xs : t') : string =
        (* Log.trace __FUNCTION__; *)
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
        (* Log.trace __FUNCTION__; *)
        match find_opt x from with
        | None ->
          [ action, destinations ] |> List.to_seq |> A.of_seq |> add x from
        | Some actions -> A.update actions action destinations
      ;;

      let size (xs : t') : int =
        (* Log.trace __FUNCTION__; *)
        fold (fun k v n -> A.size v + n) xs 0
      ;;

      let to_string (xs : t') : string =
        (* Log.trace __FUNCTION__; *)
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
      ; ind_defs : M.Ind.t M.B.t
      ; weak : Weak.t option
      }

    (** [empty init ind_defs weak] is the initial (empty) graph type [t].
        @param init is the initial state.
        @param rocq_defs is a map of rocq inductive definitions of the LTS.
        @param weak is the optional weak arguments. *)
    let empty (init : M.Enc.t) (ind_defs : M.Ind.t M.B.t) : t =
      (* Log.trace __FUNCTION__; *)
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
      (* Log.trace __FUNCTION__; *)
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

    (* used to make graph *)
    module type Y_Args = sig
      val primary_lts : M.Ind.t
      val rocq_defs : M.Ind.t M.B.t
      val stop : t -> bool
      (* val g : t ref *)
    end

    (* used for extraction *)
    module type Z_Args = sig
      val pp : bool
      val g : t ref
      val ind_defs : M.Ind.t M.B.t
    end

    module Make (Y : Y_Args) = struct
      let next_to_visit (g : t) : M.Enc.t =
        (* Log.trace __FUNCTION__; *)
        (* Queue.pop !Y.g.to_visit *)
        Queue.pop g.to_visit
      ;;

      let update_to_visit (g : t) (x : M.Enc.t) : unit =
        (* Log.trace __FUNCTION__; *)
        (* Queue.push x !Y.g.to_visit *)
        Queue.push x g.to_visit
      ;;

      let update_states (g : t) (xs : V.t) : t =
        (* Log.trace __FUNCTION__; *)
        (* Y.g := { !Y.g with states = V.union !Y.g.states xs } *)
        { g with states = V.union g.states xs }
      ;;

      let get_new_constrs (from : M.Enc.t) : M.Constructor.t list M.mm =
        (* Log.trace __FUNCTION__; *)
        let from_term : EConstr.t = M.decode from in
        let label_type : EConstr.t = M.Ind.get_lts_label_type Y.primary_lts in
        let ind_map : M.Ind.t M.F.t = M.decode_map Y.rocq_defs in
        let primary_constr_transitions =
          M.Ind.get_lts_constructor_types Y.primary_lts
        in
        M.Unification.collect_valid_constructors
          primary_constr_transitions
          ind_map
          from_term
          label_type
          Y.primary_lts.enc
      ;;

      let get_new_states (g : t) (from : M.Enc.t) : V.t M.mm =
        (* Log.trace __FUNCTION__; *)
        Log.thing ~__FUNCTION__ Debug "from" from (Of M.Enc.to_string);
        let open M.Syntax in
        let* new_constrs : M.Constructor.t list = get_new_constrs from in
        let iter_body (i : int) (new_states : V.t) =
          let (act, tgt, int_tree) : M.Constructor.t = List.nth new_constrs i in
          let tgt_enc : M.Enc.t = M.encode tgt in
          let act_enc : M.Enc.t = M.encode act in
          let* is_silent : bool option = is_silent_transition act g.weak in
          let label : Model.Label.t =
            { term = act_enc; pp = None; is_silent }
          in
          let constructor_trees : Model.Trees.t =
            Model.Trees.singleton int_tree
          in
          let to_add : Model.Action.t =
            { label; constructor_trees; annotation = None }
          in
          D.singleton (tgt_enc, int_tree) |> T.update g.transitions from to_add;
          (* NOTE: if [tgt] has not been explored then add [to_visit] *)
          if T.mem g.transitions tgt_enc || V.mem tgt_enc g.states
          then ()
          else update_to_visit g tgt_enc;
          (* NOTE: add [tgt] to [new_states] *)
          M.return (V.add tgt_enc new_states)
        in
        M.iterate 0 (List.length new_constrs - 1) (V.singleton from) iter_body
      ;;

      (** *)
      let rec build (g : t) : t M.mm =
        (* Log.trace __FUNCTION__; *)
        Log.things
          ~__FUNCTION__
          Debug
          "to visit"
          (* (!Y.g.to_visit |> Queue.to_seq |> List.of_seq) *)
          (g.to_visit |> Queue.to_seq |> List.of_seq)
          (Of
             (fun (x : M.Enc.t) : string ->
               Utils.Strfy.record
                 [ "enc", M.Enc.to_string x
                 ; "dec", M.decode x |> M.Strfy.econstr
                 ]));
        if Y.stop g
        then (
          Log.trace ~__FUNCTION__ "stop condition";
          (* Queue.is_empty !Y.g.to_visit |> M.return) *)
          M.return g
          (* else if Queue.is_empty !Y.g.to_visit *))
        else if Queue.is_empty g.to_visit
        then (
          Log.trace ~__FUNCTION__ "no more to visit";
          M.return g)
        else (
          Log.trace ~__FUNCTION__ "continue";
          let enc_to_visit : M.Enc.t = next_to_visit g in
          (* status_update enc_to_visit g bound; *)
          let open M.Syntax in
          (* NOTE: [get_new_states] also updates [g.to_visit] *)
          let* new_states : V.t = get_new_states g enc_to_visit in
          let g : t = update_states g new_states in
          build g)
      ;;
    end

    module Extract (Z : Z_Args) = struct
      let pp (x : M.Enc.t) : string option =
        (* Log.trace __FUNCTION__; *)
        if Z.pp then Some (M.decode x |> M.Strfy.econstr) else None
      ;;

      let state (x : M.Enc.t) : Model.State.t =
        (* Log.trace __FUNCTION__; *)
        { term = x; pp = pp x }
      ;;

      let states () : Model.States.t =
        (* Log.trace __FUNCTION__; *)
        !Z.g.states |> V.to_list |> List.map state |> Model.States.of_list
      ;;

      let terminals () : Model.States.t =
        (* Log.trace __FUNCTION__; *)
        !Z.g.states
        |> V.filter (fun (x : V.elt) -> Bool.not (T.mem !Z.g.transitions x))
        |> V.to_list
        |> List.map state
        |> Model.States.of_list
      ;;

      let transitions () : Model.Transitions.t =
        (* Log.trace __FUNCTION__; *)
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
        (* Log.trace __FUNCTION__; *)
        let xs = M.B.to_seq Z.ind_defs |> List.of_seq in
        let open M.Syntax in
        let f (i : int) (acc : Model.Info.lts list) =
          let (enc, v) : M.Enc.t * M.Ind.t = List.nth xs i in
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
        (* Log.trace __FUNCTION__; *)
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
        (* Log.trace __FUNCTION__; *)
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
        (* Log.trace __FUNCTION__; *)
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

    let build_ind_defs () : M.Ind.t M.B.t M.mm =
      (* Log.trace __FUNCTION__; *)
      let num : int = List.length X.grefs in
      Log.thing ~__FUNCTION__ Debug "num" num (Of Utils.Strfy.int);
      let ind_defs : M.Ind.t M.B.t = M.B.create num in
      let open M.Syntax in
      let f (i : int) () =
        let gref : Names.GlobRef.t = List.nth X.grefs i in
        (* NOTE: [M.Ind.lts] encodes [x.ind] into the bi-enc maps. *)
        let* x : M.Ind.t = M.Ind.lts gref in
        Log.thing
          ~__FUNCTION__
          Debug
          "x"
          x
          (Of (M.Strfy.rocq_ind M.Enc.to_string));
        (* NOTE: [ind_defs] is a separate map, so add again using same enc. *)
        M.B.replace ind_defs x.enc x;
        let x_enc : M.Enc.t = M.encode x.ind in
        Log.thing ~__FUNCTION__ Debug "x_enc" x_enc (Of M.Enc.to_string);
        let x_dec : EConstr.t = M.decode x.enc in
        Log.thing ~__FUNCTION__ Debug "x_dec" x_dec (Of M.Strfy.econstr);
        M.return ()
      in
      let* () = M.iterate 0 (num - 1) () f in
      ind_defs |> M.return
    ;;

    let find_primary_lts (ind_defs : M.Ind.t M.B.t) : M.Ind.t M.mm =
      (* Log.trace __FUNCTION__; *)
      let open M.Syntax in
      let* x : M.Ind.t = Nametab.global X.primary_lts |> M.Ind.lts in
      Log.thing
        ~__FUNCTION__
        Debug
        "primary lts"
        x
        (Of (M.Strfy.rocq_ind M.Enc.to_string));
      (* NOTE: catch-all sanity check *)
      M.encode x.ind |> M.B.find ind_defs |> M.return
    ;;

    (** normalize and encode the initial term *)
    let initial_term (init_term : Constrexpr.constr_expr) : EConstr.t M.mm =
      (* Log.trace __FUNCTION__; *)
      let open M.Syntax in
      let* init_term : EConstr.t = M.constrexpr_to_econstr init_term in
      let* init_term : EConstr.t = M.econstr_normalize init_term in
      M.return init_term
    ;;

    let make_yargs primary_lts ind_defs the_graph : (module Y_Args) =
      (* Log.trace __FUNCTION__; *)
      let module Y = struct
        let primary_lts : M.Ind.t = primary_lts
        let rocq_defs : M.Ind.t M.B.t = ind_defs

        let stop (g : t) : bool =
          (* Log.trace __FUNCTION__; *)
          match X.bounds with
          | States n -> V.cardinal g.states > n
          | Transitions n -> T.size g.transitions > n
        ;;
      end
      in
      (module Y : Y_Args)
    ;;

    let make_zargs ind_defs the_graph : (module Z_Args) =
      (* Log.trace __FUNCTION__; *)
      let module Z = struct
        let pp : bool = true
        let ind_defs : M.Ind.t M.B.t = ind_defs
        let g : t ref = the_graph
      end
      in
      (module Z : Z_Args)
    ;;

    let build (init_term : Constrexpr.constr_expr) : Model.LTS.t M.mm =
      (* Log.trace __FUNCTION__; *)
      let open M.Syntax in
      (* NOTE: encode rocq inductive defs *)
      let* ind_defs : M.Ind.t M.B.t = build_ind_defs () in
      Log.things
        ~__FUNCTION__
        Debug
        "ind_defs"
        (M.B.to_seq ind_defs |> List.of_seq)
        (Of
           (Utils.Strfy.tuple
              (Of M.Enc.to_string)
              (Of (M.Strfy.rocq_ind M.Enc.to_string))));
      let* primary_lts : M.Ind.t = find_primary_lts ind_defs in
      let* init_term : EConstr.t = initial_term init_term in
      let$* _unit env sigma =
        Rocq_ind.get_lts_term_type primary_lts
        |> Typing.check env sigma init_term
      in
      let init : M.Enc.t = M.encode init_term in
      (* NOTE: build the graph *)
      Log.trace ~__FUNCTION__ "Build the Graph";
      let the_graph : t ref = ref (empty init ind_defs) in
      Queue.push init !the_graph.to_visit;
      Log.things
        ~__FUNCTION__
        Debug
        "to visit"
        (!the_graph.to_visit |> Queue.to_seq |> List.of_seq)
        (Of M.Enc.to_string);
      let module G = Make ((val make_yargs primary_lts ind_defs the_graph)) in
      let* the_graph : t = G.build !the_graph in
      (* M.return !the_graph *)
      Log.thing
        ~__FUNCTION__
        Debug
        "num states (V)"
        (V.cardinal the_graph.states)
        (Of Utils.Strfy.int);
      Log.trace ~__FUNCTION__ "Completed Graph, Extracting LTS";
      let module L = Extract ((val make_zargs ind_defs (ref the_graph))) in
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

  let make_xargs primary_lts grefs weak : (module X_Args) =
    (* Log.trace __FUNCTION__; *)
    let module X = struct
      let primary_lts : Libnames.qualid = primary_lts
      let grefs : Names.GlobRef.t list = Nametab.global primary_lts :: grefs
      let weak : Weak.t option = weak
      let bounds : Model.Info.bounds = !Config.the_bounds_args
    end
    in
    (module X : X_Args)
  ;;

  let fail_if_empty (x : Model.LTS.t) : unit =
    if
      !Api.the_fail_flags.empty
      && (Int.equal (Model.States.cardinal x.states) 1
          || Model.States.is_empty x.states)
      && Model.Transitions.is_empty x.transitions
    then (
      Log.trace ~__FUNCTION__ "LTS Empty";
      M.Err.lts_empty ())
  ;;

  let fail_if_incomplete (x : Model.LTS.t) : unit =
    if !Api.the_fail_flags.incomplete
    then (
      match x with
      | { info = { meta = Some { is_complete = false; _ }; _ }; _ } ->
        Log.trace ~__FUNCTION__ "LTS Incomplete";
        M.Err.lts_incomplete ()
      | _ -> ())
  ;;

  let fail_if_not_bisim (x : Model.Bisimilar.result) : unit =
    if !Api.the_fail_flags.incomplete
    then
      if Bool.not (Model.Bisimilar.are_bisimilar x)
      then (
        Log.trace ~__FUNCTION__ "Not Bisimilar";
        M.Err.not_bisimilar ())
  ;;

  let extract_lts
        (primary_lts : Libnames.qualid)
        (init : Constrexpr.constr_expr)
        (names : Libnames.qualid list)
        (weak : Weak.t option)
    : Model.LTS.t M.mm
    =
    (* Log.trace __FUNCTION__; *)
    let t = M.make_hashtbl () in
    let v = M.make_set () in
    let d = M.make_state_tree_pair_set () in
    Log.thing
      ~__FUNCTION__
      Notice
      "names num"
      (List.length names)
      (Of Utils.Strfy.int);
    let grefs : Names.GlobRef.t list = Rocq_utils.libnames_to_globrefs names in
    let x = make_xargs primary_lts grefs weak in
    let module G = Graph ((val t)) ((val v)) ((val d)) ((val x)) in
    let open M.Syntax in
    let* the_lts = G.build init in
    fail_if_empty the_lts;
    fail_if_incomplete the_lts;
    M.return the_lts
  ;;

  module Command = struct
    let build_lts
          (primary_lts : Libnames.qualid)
          (init : Constrexpr.constr_expr)
          (names : Libnames.qualid list)
          (weak : Weak.t option)
      : Model.LTS.t M.mm
      =
      (* Log.trace __FUNCTION__; *)
      let open M.Syntax in
      let* () = Config.load_weak_args () in
      extract_lts primary_lts init names weak
    ;;

    let build_fsm
          (primary_lts : Libnames.qualid)
          (init : Constrexpr.constr_expr)
          (names : Libnames.qualid list)
          (weak : Weak.t option)
      : Model.FSM.t M.mm
      =
      (* Log.trace __FUNCTION__; *)
      let open M.Syntax in
      let* the_lts = extract_lts primary_lts init names weak in
      Model.Convert.lts_to_fsm the_lts |> M.return
    ;;

    type t =
      | MakeLTS of rocq_args
      | MakeFSM of rocq_args
      | Saturate of rocq_args
      | Minimize of rocq_args
      | Merge of rocq_pair
      | CheckBisim of rocq_pair

    (* | Info of unit  *)
    (* | Help of ...  *)
    and rocq_args = Constrexpr.constr_expr * Libnames.qualid

    and rocq_pair =
      { a : rocq_args
      ; b : rocq_args
      }

    let run (refs : Libnames.qualid list) : t -> Model.Bisimilar.t option M.mm =
      Log.trace __FUNCTION__;
      let open M.Syntax in
      Config.load_the_bounds_args ();
      function
      | MakeLTS (x, primary_lts) ->
        let weak : Weak.t option = Config.get_the_weak_arg1 () in
        let* the_lts = build_lts primary_lts x refs weak in
        Log.thing Notice "the lts" the_lts (Of Model.LTS.to_string);
        M.return None
      | MakeFSM (x, primary_lts) ->
        let weak : Weak.t option = Config.get_the_weak_arg1 () in
        let* the_fsm = build_fsm primary_lts x refs weak in
        Log.thing Notice "the fsm" the_fsm (Of Model.FSM.to_string);
        M.return None
      | Saturate (x, primary_lts) ->
        let weak : Weak.t option = Config.get_the_weak_arg1 () in
        let* the_fsm = build_fsm primary_lts x refs weak in
        Log.thing Notice "the fsm" the_fsm (Of Model.FSM.to_string);
        let the_fsm = Model.Saturate.fsm the_fsm in
        Log.thing Notice "the saturated fsm" the_fsm (Of Model.FSM.to_string);
        M.return None
      | Minimize (x, primary_lts) ->
        let weak : Weak.t option = Config.get_the_weak_arg1 () in
        let* the_fsm = build_fsm primary_lts x refs weak in
        Log.thing Notice "the fsm" the_fsm (Of Model.FSM.to_string);
        let { fsm; pi } : Model.Minimize.t = Model.Minimize.fsm the_fsm in
        Log.thing Notice "the minimized fsm" fsm (Of Model.FSM.to_string);
        M.return None
      | Merge { a; b } ->
        let weak1 : Weak.t option = Config.get_the_weak_arg1 () in
        let* the_fsm_a = build_fsm (snd a) (fst a) refs weak1 in
        Log.thing Notice "the fsm (A)" the_fsm_a (Of Model.FSM.to_string);
        let weak2 : Weak.t option = Config.get_the_weak_arg2 () in
        let* the_fsm_b = build_fsm (snd b) (fst b) refs weak2 in
        Log.thing Notice "the fsm (B)" the_fsm_b (Of Model.FSM.to_string);
        let the_fsm = Model.FSM.merge the_fsm_a the_fsm_b in
        Log.thing Notice "the merged fsm" the_fsm (Of Model.FSM.to_string);
        M.return None
      | CheckBisim { a; b } ->
        let weak1 : Weak.t option = Config.get_the_weak_arg1 () in
        let* the_fsm_a = build_fsm (snd a) (fst a) refs weak1 in
        Log.thing Notice "the fsm (A)" the_fsm_a (Of Model.FSM.to_string);
        let weak2 : Weak.t option = Config.get_the_weak_arg2 () in
        let* the_fsm_b = build_fsm (snd b) (fst b) refs weak2 in
        Log.thing Notice "the fsm (B)" the_fsm_b (Of Model.FSM.to_string);
        let result = Model.Bisimilar.fsm the_fsm_a the_fsm_b in
        fail_if_not_bisim result.result;
        Log.thing Notice "the merged fsm" result.merged (Of Model.FSM.to_string);
        Log.thing
          Notice
          "bisim states"
          result.result.bisim_states
          (Of Model.Partition.to_string);
        Log.thing
          Notice
          "non-bisim states"
          result.result.non_bisim_states
          (Of Model.Partition.to_string);
        M.return (Some result)
    ;;
  end
end
