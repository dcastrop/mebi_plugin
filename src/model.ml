module Make (Log : Logger.S) (Enc : Encoding.S) = struct
  module State = Model_state.Make (Log) (Enc)
  module States = Model_states.Make (Log) (State)
  module Label = Model_label.Make (Log) (Enc)
  module Labels = Model_labels.Make (Log) (Label)
  module Tree = Enc_tree.Make (Log) (Enc)
  module Trees = Enc_trees.Make (Log) (Tree)
  module Note = Model_annotation_note.Make (Log) (State) (Label) (Tree) (Trees)
  module Annotation = Model_annotation.Make (Log) (State) (Label) (Note)
  module Annotations = Model_annotations.Make (Log) (Label) (Note) (Annotation)

  module Transition =
    Model_transition.Make (Log) (State) (Label) (Tree) (Note) (Annotation)

  module Transitions =
    Model_transitions.Make (Log) (State) (Label) (Labels) (Tree) (Note)
      (Annotation)
      (Transition)

  module Action =
    Model_action.Make (Log) (Label) (Tree) (Trees) (Note) (Annotation)

  module Actions =
    Model_actions.Make (Log) (Label) (Labels) (Tree) (Trees) (Note) (Annotation)
      (Action)

  module ActionPair =
    Model_actionpair.Make (Log) (State) (States) (Label) (Tree) (Trees) (Note)
      (Annotation)
      (Action)

  module ActionPairs =
    Model_actionpairs.Make (Log) (State) (States) (Action) (ActionPair)

  module ActionMap =
    Model_actionmap.Make (Log) (State) (States) (Label) (Labels) (Tree) (Trees)
      (Note)
      (Annotation)
      (Action)
      (Actions)
      (ActionPair)
      (ActionPairs)

  module Edge = Model_edge.Make (Log) (State) (Label) (Action)
  module Edges = Model_edges.Make (Log) (State) (Label) (Action) (Edge)

  module EdgeMap =
    Model_edgemap.Make (Log) (State) (States) (Label) (Labels) (Action)
      (Actions)
      (ActionPair)
      (ActionPairs)
      (ActionMap)
      (Edge)
      (Edges)

  module Partition =
    Model_state_partition.Make (Log) (State) (States) (Label) (Labels) (Action)
      (Actions)
      (ActionPair)
      (ActionPairs)
      (ActionMap)
      (Edge)
      (Edges)
      (EdgeMap)

  module Info = struct
    type t =
      { meta : meta option
      ; weak_labels : Labels.t
      }

    and meta =
      { is_complete : bool
      ; is_merged : bool
      ; bounds : bounds
      ; lts : lts list
      }

    and bounds =
      | States of int
      | Transitions of int

    and lts =
      { enc : Enc.t
      ; constructors : Rocq_bindings.constructor list
      }

    (** [merge a b] returns a new [t] with a union of [weak_labels] and [meta=None].
    *)
    let merge (a : t) (b : t) : t =
      { meta = None; weak_labels = Labels.union a.weak_labels b.weak_labels }
    ;;

    (* *)
    let to_string (x : t) : string =
      let f
            ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
            (y : meta)
        : string
        =
        Utils.Strfy.record
          ~args
          [ "is complete", Utils.Strfy.bool y.is_complete
          ; "is merged", Utils.Strfy.bool y.is_merged
          ; ( "bounds"
            , match y.bounds with
              | States i -> Printf.sprintf "States (%i)" i
              | Transitions i -> Printf.sprintf "Transitions (%i)" i )
          ; ( "lts"
            , Utils.Strfy.list
                (Of
                   (fun ({ enc; constructors } : lts) ->
                     Utils.Strfy.record
                       [ "enc", Enc.to_string enc
                       ; ( "constructors"
                         , Utils.Strfy.list
                             (Of Rocq_bindings.constructor_to_string)
                             constructors )
                       ]))
                y.lts )
          ]
      in
      Utils.Strfy.record
        [ "meta", Utils.Strfy.option (Args f) x.meta
        ; "weak labels", Labels.to_string x.weak_labels
        ]
    ;;

    let log ?(__FUNCTION__ : string = "") ?(s : string = "Info") (x : t) : unit =
      Log.thing ~__FUNCTION__ Debug s x (Of to_string)
    ;;
  end

  module LTS = struct
    type t =
      { init : State.t option
      ; terminals : States.t
      ; alphabet : Labels.t
      ; states : States.t
      ; transitions : Transitions.t
      ; info : Info.t
      }

    let to_string (x : t) : string =
      Utils.Strfy.record
        [ "init", Utils.Strfy.option (Of State.to_string) x.init
        ; "info", Info.to_string x.info
        ; "terminals", States.to_string x.terminals
        ; "alphabet", Labels.to_string x.alphabet
        ; "states", States.to_string x.states
        ; "transitions", Transitions.to_string x.transitions
        ]
    ;;
  end

  module FSM = struct
    type t =
      { init : State.t option
      ; terminals : States.t
      ; alphabet : Labels.t
      ; states : States.t
      ; edges : EdgeMap.t'
      ; info : Info.t
      }

    let merge (a : t) (b : t) : t =
      let init : State.t option = None in
      let terminals : States.t = States.union a.terminals b.terminals in
      let alphabet : Labels.t = Labels.union a.alphabet b.alphabet in
      let states : States.t = States.union a.states b.states in
      let edges : EdgeMap.t' = EdgeMap.merge a.edges b.edges in
      let info : Info.t = Info.merge a.info b.info in
      { init; terminals; alphabet; states; edges; info }
    ;;

    let is_weak_mode (x : t) : bool =
      Bool.not (Labels.is_empty x.info.weak_labels)
    ;;

    let to_string (x : t) : string =
      Utils.Strfy.record
        [ "init", Utils.Strfy.option (Of State.to_string) x.init
        ; "\n#### info", Info.to_string x.info
        ; "\n#### terminals", States.to_string x.terminals
        ; "\n#### alphabet", Labels.to_string x.alphabet
        ; "\n#### states", States.to_string x.states
        ; "\n#### edges", EdgeMap.to_string x.edges
        ]
    ;;
  end

  module Convert = struct
    let transitions_to_edgemap (xs : Transitions.t) : EdgeMap.t' =
      Log.trace __FUNCTION__;
      let edges : EdgeMap.t' = EdgeMap.create 0 in
      Transitions.iter
        (fun ({ from; goto; label; annotation; tree } : Transition.t) ->
          (* NOTE: [ActionMap.update] handles merging of [constructor_trees] for [actions] with matching [labels] *)
          EdgeMap.update
            edges
            from
            { label
            ; annotation
            ; constructor_trees = Option.cata Trees.singleton Trees.empty tree
            }
            (States.singleton goto))
        xs;
      edges
    ;;

    let lts_to_fsm (x : LTS.t) : FSM.t =
      Log.trace __FUNCTION__;
      { init = x.init
      ; terminals = x.terminals
      ; alphabet = x.alphabet
      ; states = x.states
      ; edges = transitions_to_edgemap x.transitions
      ; info = x.info
      }
    ;;
  end

  (** [module Saturate ] ...
      (* TODO: the idea of [Traces] needs to be revisited. It does provide optimizations to examples with a lot of silent actions, where the saturated FSM is considerably larger, but i believe that there are areas where this can still be improved. *)
  *)
  module Saturate = struct
    module Log = Log
    (* Logger.Make
       (Output_mode.Default)
       (struct
       let prefix : string option = None
       let level : Output_kind.level -> bool = !Output_kind.default_level

       let special : Output_kind.special -> bool =
       Output_kind.default_special_fun ~trace:false
       ;;
       end) *)

    (** [module WIP] is a lightweight counterpart of [Note.t] that forms some "work-in-progress" [Annotation.t]. Once we stop saturating an action, we check if we are able to yield a new saturated action and convert the [wip list] to an [Annotation.t].
    *)
    module WIP = struct
      type t =
        { from : State.t
        ; via : Label.t
        ; trees : Trees.t
        }

      let to_string (x : t) : string =
        Printf.sprintf
          "<State (%s) Via (%s)>"
          (State.to_string x.from)
          (* (Label.to_string x.label) *)
          (Enc.to_string x.via.term)
      ;;

      let is_silent (x : t) : bool = Label.is_silent x.via
      let is_named (x : t) : bool = is_silent x |> Bool.not

      let equal (a : t) (b : t) : bool =
        State.equal a.from b.from
        && Label.equal a.via b.via
        && Trees.equal a.trees b.trees
      ;;

      let compare (a : t) (b : t) : int =
        Utils.compare_chain
          [ State.compare a.from b.from
          ; Label.compare a.via b.via
          ; Trees.compare a.trees b.trees
          ]
      ;;

      let create (from : State.t) (action : Action.t) : t =
        { from; via = action.label; trees = action.constructor_trees }
      ;;

      exception IsEmptyList

      let list_to_annotation (goto : State.t) (xs : t list) : Annotation.t =
        Log.trace __FUNCTION__;
        let rec f : t list -> Annotation.t = function
          | [] -> raise IsEmptyList
          | { from; via; trees } :: [] ->
            { this = { from; label = via; using = trees; goto }; next = None }
          | { from; via; trees } :: h :: tl ->
            let { from = goto; via = via2; trees = tree2 } : t = h in
            { this = { from; label = via; using = trees; goto }
            ; next = Some (f (h :: tl))
            }
        in
        f (List.rev xs)
      ;;
    end

    (** [module Trace] ... we keep track of the total sum of traces we have already checked. This is useful for checking if, from a state and action, we have already explored the rest of this trace and so can just use what we have already learned, e.g., if we are in some "subtrace".
    *)
    module Trace = struct
      module NT = struct end

      type t =
        { this : WIP.t
        ; next : next option
        }

      and next =
        | Next of t
        | Goto of State.t

      let rec to_string : t -> string = function
        | { this; next = None } ->
          Utils.Strfy.list
            (Of Utils.Strfy.string)
            [ WIP.to_string this; "None" ]
        | { this; next = Some (Next x) } ->
          Utils.Strfy.list
            (Of Utils.Strfy.string)
            [ WIP.to_string this; to_string x ]
        | { this; next = Some (Goto x) } ->
          Utils.Strfy.list
            (Of Utils.Strfy.string)
            [ WIP.to_string this; State.to_string x ]
      ;;

      let create (this : WIP.t) : t = { this; next = None }

      let rec compare (a : t) (b : t) : int =
        Utils.compare_chain
          [ WIP.compare a.this b.this
          ; Option.compare compare_next a.next b.next
          ]

      and compare_next (a : next) (b : next) : int =
        match a, b with
        | Next a, Next b -> compare a b
        | Next a, Goto _ -> -1
        | Goto a, Goto b -> State.compare a b
        | Goto a, Next _ -> 1
      ;;

      exception Invalid

      (** [had_named ?validate x] is [true] if for an element in x has a label that is non-silent.
          @param ?validate
            is a optional flag that if [true] continues the search through the [Some next] that follows a named term, and will raise [Invalid] if another named element is found in x.
          @raise Invalid
            if more than one named element is found in [x] and [~validate:true].
      *)
      let rec has_named ?(validate : bool = false) : t -> bool = function
        | { this; next = Some (Next x) } ->
          if WIP.is_named this
          then (
            if validate then if has_named x then raise Invalid;
            true)
          else has_named ~validate x
        | { this; next = _ } -> WIP.is_named this
      ;;

      let validate (x : t) : unit =
        let _ = has_named ~validate:true x in
        ()
      ;;

      exception CouldNotFindGoto

      let rec get_goto : t -> State.t = function
        | { this; next = Some (Goto x) } -> x
        | { this; next = Some (Next next) } -> get_goto next
        | { this; next = None } -> raise CouldNotFindGoto
      ;;

      exception CouldNotFindNamed

      let rec get_named : t -> Label.t = function
        | { this; next = Some (Next x) } ->
          if WIP.is_named this then this.via else get_named x
        | { this; next = _ } ->
          if WIP.is_named this then this.via else raise CouldNotFindNamed
      ;;

      let get_named_opt (x : t) : Label.t option =
        try Some (get_named x) with CouldNotFindNamed -> None
      ;;

      exception FailAdd_AlreadyNamed
      exception FailAdd_AlreadyHasGoto

      let add (x : WIP.t) (ys : t) : t =
        (* NOTE: cannot have more than one non-silent action *)
        if WIP.is_named x && has_named ys then raise FailAdd_AlreadyHasGoto;
        let rec add : t -> t = function
          | { this; next = None } ->
            { this; next = Some (Next { this = x; next = None }) }
          | { this; next = Some (Next y) } ->
            { this; next = Some (Next (add y)) }
          | { this; next = Some (Goto _) } -> raise FailAdd_AlreadyHasGoto
        in
        add ys
      ;;

      exception FailSetGoto_AlreadyHasGoto

      let set_goto (x : State.t) (ys : t) : t =
        let rec set_goto : t -> t = function
          | { this; next = None } -> { this; next = Some (Goto x) }
          | { this; next = Some (Next y) } ->
            { this; next = Some (Next (set_goto y)) }
          | { this; next = Some (Goto _) } -> raise FailSetGoto_AlreadyHasGoto
        in
        set_goto ys
      ;;

      exception FailSeq_AlreadyNamed
      exception FailSeq_AlreadyHasGoto

      (** [seq a b] appends [b] to the end of [a]. *)
      let seq (a : t) (b : t) : t =
        (* NOTE: cannot have more than one non-silent action *)
        if has_named a && has_named b then raise FailSeq_AlreadyNamed;
        let rec seq : t -> t = function
          | { this; next = None } -> { this; next = Some (Next b) }
          | { this; next = Some (Next x) } ->
            { this; next = Some (Next (seq x)) }
          | { this; next = Some (Goto _) } -> raise FailAdd_AlreadyHasGoto
        in
        seq a
      ;;

      (** [seq_opt a b] is [seq a b] of [Some a] else [b] *)
      let seq_opt (a : t option) (b : t) : t =
        match a with None -> b | Some a -> seq a b
      ;;

      (** [get x ys] ...
          @raise Not_found if [x] does not match any in [ys]. *)
      let rec get (x : WIP.t) : t -> t = function
        | { this; next = Some (Next y) } ->
          if WIP.equal x this then { this; next = Some (Next y) } else get x y
        | { this; next } ->
          if WIP.equal x this then { this; next } else raise Not_found
      ;;

      (** [upto_named x] ...
          @raise Not_found if [x] begins with a named label. *)
      let upto_named (x : t) : t =
        Log.trace __FUNCTION__;
        match x with
        | { this; next } ->
          if WIP.is_named this
          then raise Not_found
          else (
            let rec f : next option -> next option = function
              | None -> None
              | Some (Goto y) -> Some (Goto y)
              | Some (Next { this; next }) ->
                if WIP.is_named this
                then Some (Goto this.from)
                else Some (Next { this; next = f next })
            in
            { this; next = f next })
      ;;

      exception GotoNotSet

      let rec to_annotation : t -> Annotation.t =
        Log.trace __FUNCTION__;
        function
        | { this = { from; via; trees }; next = None } ->
          (* { this = { from; label = via; using = trees; goto }; next = None } *)
          raise GotoNotSet
        | { this = { from; via; trees }; next = Some (Next x) } ->
          let next : Annotation.t = to_annotation x in
          { this = { from; label = via; using = trees; goto = next.this.from }
          ; next = Some next
          }
        | { this = { from; via; trees }; next = Some (Goto goto) } ->
          { this = { from; label = via; using = trees; goto }; next = None }
      ;;
    end

    module Traces = struct
      include Set.Make (Trace)

      (** [get x ys] returns a subset subtraces [ys] that begin with [x]. This includes elements in [ys] that begin with [x], in addition to the trailing-subtraces that begin with [x] for elements in [ys].
          @raise Not_found if the set would return empty. *)
      let get (x : WIP.t) (ys : t) : t =
        let xs : t =
          fold
            (fun (y : Trace.t) (acc : t) ->
              if WIP.equal x y.this
              then add y acc
              else (
                match y.next with
                | Some (Next next) ->
                  (try add (Trace.get x next) acc with Not_found -> acc)
                | _ -> acc))
            ys
            empty
        in
        if is_empty xs then raise Not_found else xs
      ;;

      let to_string (xs : t) : string =
        to_list xs
        |> Utils.Strfy.list
             ~args:{ (Utils.Strfy.style_args ()) with name = Some "Traces" }
             (Of Trace.to_string)
      ;;

      (* let update (x : Trace.t) (xs : t) : t =
         Log.trace __FUNCTION__;
         let xs = add x xs in
         xs
         ;; *)
    end

    (** [data] ...
        @param named is ...
        @param notes is ...
        @param visited
          is the set of states encountered so far in this particular saturation.
        @param traces
          is the set traces of all saturated actions so-far, which enables us to more optimally explore the state-space with minimal repitition.
        @param old_edges is ... *)
    type data =
      { named : Label.t option
      ; current : Trace.t option
      ; visited : States.t
      ; traces : Traces.t ref
      ; can_collect_traces : bool ref
      ; old_edges : EdgeMap.t'
      }

    let initial_data (traces : Traces.t ref) (old_edges : EdgeMap.t') : data =
      { named = None
      ; (* notes = []; *) current = None
      ; visited = States.empty
      ; traces
      ; can_collect_traces = ref true
      ; old_edges
      }
    ;;

    let has_named (d : data) : bool = Option.has_some d.named

    let update_traces (d : data) (x : Trace.t) : unit =
      Log.trace __FUNCTION__;
      d.traces := Traces.add x !(d.traces);
      d.can_collect_traces := true
    ;;

    (****************************************************************************)

    (** returns a copy of [d] with the updated name *)
    let update_named (x : Action.t) (d : data) : data =
      Log.trace __FUNCTION__;
      let named : Label.t option =
        match d.named with
        | None -> if Action.is_silent x then None else Some x.label
        | Some y -> Some y
      in
      { d with named }
    ;;

    (** returns a copy of [d] with the updated notes *)
    (* let update_notes (x : WIP.t) (d : data) : data =
      Log.trace __FUNCTION__;
      { d with notes = x :: d.notes }
    ;; *)

    (** returns a copy of [d] with [x] added to [d.current] *)
    let update_current (x : WIP.t) (d : data) : data =
      Log.trace __FUNCTION__;
      match d.current with
      | None -> { d with current = Some (Trace.create x) }
      | Some current -> { d with current = Some (Trace.add x current) }
    ;;

    (** returns a copy of [d] with the updated visited *)
    let update_visited (x : State.t) (d : data) : data =
      Log.trace __FUNCTION__;
      let f (x : State.t) (d : data) : States.t = States.add x d.visited in
      { d with visited = f x d }
    ;;

    (****************************************************************************)

    let already_visited (x : State.t) (d : data) : bool = States.mem x d.visited

    (** [skip_action x d] is [true] if [x] is non-silent and [d.named] is already [Some _].
    *)
    let skip_action (x : Action.t) (d : data) : bool =
      if Action.is_silent x then false else Option.has_some d.named
    ;;

    let get_old_actions (from : State.t) (d : data) : ActionMap.t' option =
      Log.trace __FUNCTION__;
      EdgeMap.find_opt d.old_edges from
    ;;

    (****************************************************************************)

    (* exception Model_Saturate_WIP_HadNoNamedActions of WIP.t list
    exception Model_Saturate_WIP_HadMultipleNamedActions of WIP.t list

    let validate_wips (xs : WIP.t list) : unit =
      Log.trace __FUNCTION__;
      match
        List.filter
          (fun ({ via; _ } : WIP.t) -> Label.is_silent via |> Bool.not)
          xs
      with
      | [] -> raise (Model_Saturate_WIP_HadNoNamedActions xs)
      | _ :: [] -> ()
      | _ :: _ -> raise (Model_Saturate_WIP_HadMultipleNamedActions xs)
    ;; *)

    (****************************************************************************)

    let update_acc (trace : Trace.t) (label : Label.t) (acc : ActionPairs.t) =
      Log.trace __FUNCTION__;
      Trace.to_annotation trace
      |> Annotations.extrapolate
      |> Annotations.to_list
      |> List.map (fun (x : Annotation.t) : ActionPair.t ->
        let y : Action.t =
          { label; annotation = Some x; constructor_trees = Trees.empty }
        in
        y, States.singleton (Annotation.last x).goto)
      |> ActionPairs.merge_list acc
    ;;

    (** [stop] *)
    let stop (d : data) (goto : State.t) (acc : ActionPairs.t) : ActionPairs.t =
      Log.trace __FUNCTION__;
      match d.current, d.named with
      | Some current, Some named ->
        let () = Trace.validate current in
        let trace : Trace.t = Trace.set_goto goto current in
        update_traces d trace;
        update_acc trace named acc
      (* NOTE: skip and return [acc] otherwise *)
      | _, _ -> acc
    ;;

    (****************************************************************************)

    let finish_with_trace
          (z : Trace.t)
          (d : data)
          (named : Label.t)
          (acc : ActionPairs.t)
      : ActionPairs.t
      =
      Log.trace __FUNCTION__;
      let z : Trace.t = Trace.seq_opt d.current z in
      update_traces d z;
      update_acc z named acc
    ;;

    let finish_with_trace_upto
          (z : Trace.t)
          (d : data)
          (named : Label.t)
          (acc : ActionPairs.t)
      : ActionPairs.t
      =
      Log.trace __FUNCTION__;
      try
        let z : Trace.t = Trace.upto_named z in
        finish_with_trace z d named acc
      with
      (* NOTE: stop here as [x] begins with named action. *)
      | Not_found -> acc
    ;;

    (** [check_from] explores the outgoing actions of state [from], which is some destination of another action.
    *)
    let rec check_from (d : data) (from : State.t) (acc : ActionPairs.t)
      : ActionPairs.t
      =
      Log.trace __FUNCTION__;
      if already_visited from d
      then stop d from acc
      else (
        let d : data = update_visited from d in
        match get_old_actions from d with
        | None -> stop d from acc
        | Some old_actions -> check_actions d from old_actions acc)

    and check_actions (d : data) (from : State.t) (xs : ActionMap.t')
      : ActionPairs.t -> ActionPairs.t
      =
      Log.trace __FUNCTION__;
      ActionMap.fold
        (fun (x : Action.t) (ys : States.t) (acc : ActionPairs.t) ->
          if skip_action x d
          then stop d from acc
          else (
            try
              if !(d.can_collect_traces)
              then collect_from_traces d from x ys acc
              else raise Not_found
            with
            | Not_found ->
              (* NOTE: continue exploring un-traced state-space *)
              continue_check_destinations d from x ys acc))
        xs

    and collect_from_traces
          (d : data)
          (from : State.t)
          (x : Action.t)
          (ys : States.t)
          (acc : ActionPairs.t)
      : ActionPairs.t
      =
      Log.trace __FUNCTION__;
      let wip : WIP.t = WIP.create from x in
      let traces : Traces.t = Traces.get wip !(d.traces) in
      (* NOTE: add all traces that already have named action (if we don't) -- keep exploring with traces *)
      Traces.fold
        (fun (z : Trace.t) (acc : ActionPairs.t) : ActionPairs.t ->
          (* Log.thing ~__FUNCTION__ Debug "z" z (Of Trace.to_string); *)
          match d.named, Trace.get_named_opt z with
          | Some named, None ->
            Log.trace ~__FUNCTION__ "stop (data)";
            (* NOTE: stop as named is in some [current]. *)
            finish_with_trace z d named acc
          | None, Some named ->
            Log.trace ~__FUNCTION__ "stop (trace)";
            (* NOTE: stop since the trace is named (and already explored). *)
            finish_with_trace z d named acc
          | None, None ->
            Log.trace ~__FUNCTION__ "continue (full)";
            (* NOTE: continue exploring un-traced state-space as the [named] must occur earlier in the trace and has been pruned *)
            (* NOTE: we can only use the traces once *)
            d.can_collect_traces := false;
            continue_check_destinations d from x ys acc
          | Some named, Some _ ->
            Log.trace ~__FUNCTION__ "continue (upto)";
            (* NOTE: we can only continue with the trace up-to the named action *)
            finish_with_trace_upto z d named acc)
        traces
        acc

    and continue_check_destinations
          (d : data)
          (from : State.t)
          (x : Action.t)
          (ys : States.t)
      : ActionPairs.t -> ActionPairs.t
      =
      Log.trace __FUNCTION__;
      let wip : WIP.t = WIP.create from x in
      let d : data (* NOTE: copy [d] *) = update_current wip d in
      let d : data = update_named x d in
      check_destinations d from ys

    and check_destinations (d : data) (from : State.t) (xs : States.t)
      : ActionPairs.t -> ActionPairs.t
      =
      Log.trace __FUNCTION__;
      States.fold (check_from d) xs
    ;;

    (****************************************************************************)

    (** [edge_action_destinations] returns a list of saturated actions tupled with their respective destinations, which is the reflexive-transitive closure of visible actions that may weakly be performed from each of [the_destinations].
        edge -> edge_actions -> edge_action_destinations -> ( ... )
        @param ys
          is the set of destination [States.t] reachable from state [from] via actions that have already been recorded in [d.notes] as a [wip].
    *)
    let edge_action_destinations (d : data) (from : State.t) (ys : States.t)
      : ActionPairs.t
      =
      Log.trace __FUNCTION__;
      States.fold
        (fun (y : State.t) (acc : ActionPairs.t) ->
          Log.thing ~__FUNCTION__ Debug "y" y (Of State.to_string);
          check_from d y ActionPairs.empty)
        ys
        ActionPairs.empty
    ;;

    (** [edge_actions] returns a list of saturated actions tupled with their respective destinations, obtained from [edge_action_destinations] which explores the reflexive-transitive closure
        edge -> edge_actions -> edge_action_destinations -> ( ... ) *)
    let edge_actions
          (from : State.t)
          (old_actions : ActionMap.t')
          (old_edges : EdgeMap.t')
          (traces : Traces.t ref)
      : ActionPairs.t
      =
      Log.trace __FUNCTION__;
      ActionMap.fold
        (fun (x : Action.t) (ys : States.t) (acc : ActionPair.t list) ->
          Log.thing ~__FUNCTION__ Debug "x" x (Of Action.to_string);
          let d : data =
            initial_data traces old_edges
            |> update_named x
            |> update_current (WIP.create from x)
          in
          edge_action_destinations d from ys
          |> ActionPairs.to_list
          |> ActionPair.merge_lists acc)
        old_actions
        []
      |> ActionPairs.of_list
    ;;

    (** [edge] updates [new_actions] with actions saturated by [edge_actions]
        edge -> edge_actions -> edge_action_destinations -> ( ... ) *)
    let edge
          (new_actions : ActionMap.t')
          (from : State.t)
          (old_actions : ActionMap.t')
          (old_edges : EdgeMap.t')
          (traces : Traces.t ref)
      : unit
      =
      Log.trace __FUNCTION__;
      edge_actions from old_actions old_edges traces
      |> ActionPairs.iter
           (fun ((saturated_action, destinations) : Action.t * States.t) ->
           ActionMap.update new_actions saturated_action destinations)
    ;;

    (** [] *)
    let edges (labels : Labels.t) (states : States.t) (old_edges : EdgeMap.t')
      : EdgeMap.t'
      =
      Log.trace __FUNCTION__;
      let new_edges : EdgeMap.t' = EdgeMap.create 0 in
      let traces : Traces.t ref = ref Traces.empty in
      EdgeMap.iter
        (fun (from : State.t) (old_actions : ActionMap.t') ->
          Log.thing ~__FUNCTION__ Debug "from" from (Of State.to_string);
          (* NOTE: populate [new_actions] with saturated [old_actions] *)
          let new_actions : ActionMap.t' = ActionMap.create 0 in
          let () = edge new_actions from old_actions old_edges traces in
          EdgeMap.replace new_edges from new_actions)
        old_edges;
      Log.thing ~__FUNCTION__ Debug "traces" !traces (Of Traces.to_string);
      new_edges
    ;;

    let fsm ?(only_if_weak : bool = true) (x : FSM.t) : FSM.t =
      Log.trace __FUNCTION__;
      Log.thing ~__FUNCTION__ Debug "x" x (Of FSM.to_string);
      if only_if_weak && Bool.not (FSM.is_weak_mode x)
      then (
        Log.debug ~__FUNCTION__ "Not weak, returning unchanged";
        x)
      else { x with edges = edges x.alphabet x.states (EdgeMap.copy x.edges) }
    ;;
  end

  module Minimize = struct
    type t =
      { fsm : FSM.t
      ; pi : Partition.t
      }

    exception CannotSplitEmptyBlock of unit

    let ensure_nonempty (a : States.t) : unit =
      Log.trace __FUNCTION__;
      try assert (States.is_empty a |> Bool.not) with
      | Assert_failure _ -> raise (CannotSplitEmptyBlock ())
    ;;

    let split_block
          (pi : Partition.t)
          (s : State.t)
          (edges : EdgeMap.t')
          (block : States.t)
      : States.t * States.t option
      =
      Log.trace __FUNCTION__;
      ensure_nonempty block;
      let reachable_from_s : Partition.t = Partition.reachable s edges pi in
      Partition.log ~__FUNCTION__ ~s:"reachable" reachable_from_s;
      States.fold
        (fun (t : State.t) ((b1, b2) : States.t * States.t option) ->
          if State.equal s t
          then States.add s b1, b2
          else (
            let reachable_from_t : Partition.t =
              Partition.reachable t edges pi
            in
            (* NOTE: split if [s] and [t] can reach different blocks *)
            if Partition.equal reachable_from_s reachable_from_t
            then States.add t b1, b2
            else b1, Some (States.add_to_opt t b2)))
        block
        (States.empty, None)
    ;;

    exception Split_OnlyReturnedOneBlock_ButNeqBlock of (States.t * States.t)

    let ensure_equal (a : States.t) (b : States.t) : unit =
      Log.trace __FUNCTION__;
      try assert (States.equal a b) with
      | Assert_failure _ ->
        raise (Split_OnlyReturnedOneBlock_ButNeqBlock (a, b))
    ;;

    let for_each_label
          (pi : Partition.t ref)
          (changed : bool ref)
          (edges : EdgeMap.t')
          (block : States.t ref)
          (label : Label.t)
      : unit
      =
      Log.trace __FUNCTION__;
      Label.log ~__FUNCTION__ ~s:"split by label" label;
      let edges : EdgeMap.t' = EdgeMap.reduce_by_label edges label in
      (* NOTE: select some state [s] from [block] *)
      let s : State.t = States.min_elt !block in
      State.log ~__FUNCTION__ ~s:"split from state" s;
      match split_block !pi s edges !block with
      | a, None -> ensure_equal a !block
      | a, Some b ->
        pi := Partition.remove !block !pi |> Partition.add a |> Partition.add b;
        block := a;
        changed := true
    ;;

    let for_each_block
          (pi : Partition.t ref)
          (changed : bool ref)
          (alphabet : Labels.t)
          (edges : EdgeMap.t')
          (block : States.t)
      : unit
      =
      Log.trace __FUNCTION__;
      Labels.non_silent alphabet
      |> Labels.iter (for_each_label pi changed edges (ref block))
    ;;

    let partition_states (fsm : FSM.t) : Partition.t =
      Log.trace __FUNCTION__;
      let pi : Partition.t ref = ref (Partition.singleton fsm.states) in
      let changed : bool ref = ref true in
      while !changed do
        changed := false;
        Partition.iter (for_each_block pi changed fsm.alphabet fsm.edges) !pi
      done;
      !pi
    ;;

    let fsm (fsm : FSM.t) : t =
      Log.trace __FUNCTION__;
      { fsm; pi = Saturate.fsm ~only_if_weak:true fsm |> partition_states }
    ;;

    let to_string ({ fsm; pi } : t) : string =
      Utils.Strfy.record
        [ "fsm", FSM.to_string fsm; "pi", Partition.to_string pi ]
    ;;
  end

  module Bisimilar = struct
    type t =
      { fsm_a : fsm_pair
      ; fsm_b : fsm_pair
      ; merged : FSM.t
      ; result : result
      }

    and result =
      { bisim_states : Partition.t
      ; non_bisim_states : Partition.t
      }

    and fsm_pair =
      { original : FSM.t
      ; saturated : FSM.t
      }

    let fsm_pair (original : FSM.t) : fsm_pair =
      Log.trace __FUNCTION__;
      { original; saturated = Saturate.fsm ~only_if_weak:true original }
    ;;

    let are_bisimilar ({ non_bisim_states; _ } : result) : bool =
      Log.trace __FUNCTION__;
      Partition.is_empty non_bisim_states
    ;;

    let the_cached_result : t option ref = ref None
    let set_the_result (x : t) : unit = the_cached_result := Some x

    exception NoCachedResult of unit

    let get_the_result () : t =
      Log.trace __FUNCTION__;
      match !the_cached_result with
      | None -> raise (NoCachedResult ())
      | Some x -> x
    ;;

    let split (pi : Partition.t) (a : States.t) (b : States.t) : result =
      Log.trace __FUNCTION__;
      let bisim_states, non_bisim_states =
        Partition.fold
          (fun (x : States.t) (bisim_states, non_bisim_states) ->
            if States.has_shared_origin x a b
            then Partition.add x bisim_states, non_bisim_states
            else bisim_states, Partition.add x non_bisim_states)
          pi
          (Partition.empty, Partition.empty)
      in
      { bisim_states; non_bisim_states }
    ;;

    let fsm (a : FSM.t) (b : FSM.t) : t =
      Log.trace __FUNCTION__;
      let fsm_a : fsm_pair = fsm_pair a in
      let fsm_b : fsm_pair = fsm_pair b in
      let merged : FSM.t = FSM.merge fsm_a.saturated fsm_b.saturated in
      let pi : Partition.t = (Minimize.fsm merged).pi in
      let result = split pi fsm_a.original.states fsm_b.original.states in
      { fsm_a; fsm_b; merged; result }
    ;;

    let fsm_pair_to_string ({ original; saturated } : fsm_pair) : string =
      Utils.Strfy.record
        [ "original", FSM.to_string original
        ; "saturated", FSM.to_string saturated
        ]
    ;;

    let result_to_string ({ bisim_states; non_bisim_states } : result) : string =
      Utils.Strfy.record
        [ "bisim_states", Partition.to_string bisim_states
        ; "non_bisim_states", Partition.to_string non_bisim_states
        ]
    ;;

    let to_string ({ fsm_a; fsm_b; merged; result } : t) : string =
      Utils.Strfy.record
        [ "######## fsm_a", fsm_pair_to_string fsm_a
        ; "\n######## fsm_b", fsm_pair_to_string fsm_b
        ; "\n######## merged", FSM.to_string merged
        ; "\n######## result", result_to_string result
        ]
    ;;
  end
end
