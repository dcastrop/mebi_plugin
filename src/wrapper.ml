module type S = sig
  type enc
  type node
  type tree
  type trees

  module M : Rocq_monad_utils.S with type enc = enc and type tree = tree
  module Bindings : Bindings.S with type 'a mm = 'a M.mm

  module ConstructorBindings :
    Constructor_bindings.S
    with type 'a mm = 'a M.mm
     and type ind = M.Ind.t
     and type instructions = Bindings.Instructions.t
     and type bindings = Bindings.t
     and type constrmap = Bindings.ConstrMap.t'

  module Model :
    Model.S
    with type base = enc
     and type tree = tree
     and type trees = trees
     and type constructorbindings = ConstructorBindings.t

  module Decode :
    Decoder.S
    with type enc = enc
     and type state = Model.State.t
     and type states = Model.States.t
     and type partition = Model.Partition.t
     and type label = Model.Label.t
     and type labels = Model.Labels.t
     and type note = Model.Note.t
     and type annotation = Model.Annotation.t
     and type annotations = Model.Annotations.t
     and type transition = Model.Transition.t
     and type transitions = Model.Transitions.t
     and type action = Model.Action.t
     and type actions = Model.Actions.t
     and type actionmap = Model.ActionMap.t'
     and type edgemap = Model.EdgeMap.t'
     and type rocqlts = Model.Info.Meta.RocqLTS.t
     and type info = Model.Info.t
     and type lts = Model.LTS.t
     and type fsm = Model.FSM.t
     and type result = Model.Bisimilarity.Result.t
     and type bisimilarity = Model.Bisimilarity.t

  module IsTheory : sig
    val is_theory : EConstr.t -> EConstr.t -> bool M.mm
    val is_exists : EConstr.t -> bool M.mm
    val is_weak_sim : EConstr.t -> bool M.mm
    val is_weak : EConstr.t -> bool M.mm
    val is_tau : EConstr.t -> bool M.mm
    val is_silent : EConstr.t -> bool M.mm
    val is_silent1 : EConstr.t -> bool M.mm
    val is_LTS : EConstr.t -> bool M.mm
    val is_None : EConstr.t -> bool M.mm
    val is_Some : EConstr.t -> bool M.mm
    val get_theory_enc : (EConstr.t -> bool M.mm) -> enc M.mm

    exception NoEncodingFoundFor_TheoriesNone of unit

    val get_None_enc : unit -> enc M.mm

    exception NoEncodingFoundFor_TheoriesSome of unit

    val get_Some_enc : unit -> enc M.mm

    exception NotEqTheory of unit

    val get_theory_enc_if_eq : EConstr.t -> (EConstr.t -> bool M.mm) -> enc M.mm
    val get_None_enc_if_eq : EConstr.t -> enc M.mm
    val get_Some_enc_if_eq : EConstr.t -> enc M.mm
  end

  module Weak : Weak.S with type enc = enc

  module Config : sig
    val load_weak_arg : Api.weak_arg -> Weak.t M.mm
    val load_weak_arg_opt : Api.weak_arg option -> Weak.t option M.mm

    type weak_args =
      { a : Weak.t option
      ; b : Weak.t option
      }

    val the_weak_args : weak_args ref option ref
    val reset_the_weak_args : unit -> unit
    val load_weak_args : unit -> unit M.mm
    val get_the_weak_args : unit -> weak_args option
    val get_the_weak_arg1 : unit -> Weak.t option
    val get_the_weak_arg2 : unit -> Weak.t option

    (* val api_bounds_to_model_bounds : Api.bounds_args -> Model.Info.Meta.bounds *)
    val the_bounds_args : Api.bounds_args ref
    val load_the_bounds_args : unit -> unit
  end

  module type X_Args = sig
    val primary_lts : Libnames.qualid
    val grefs : Names.GlobRef.t list
    val weak : Weak.t option
    val bounds : Api.bounds_args
  end

  module Graph : (T0 : Hashtbl.S with type key = enc)
      (V0 : Set.S with type elt = enc)
      (D0 : Set.S with type elt = enc * tree)
      (X : X_Args)
      -> sig
    module V : sig
      include Set.S with type elt = enc

      val to_string : t -> string
    end

    module D : sig
      include Set.S with type elt = enc * tree

      val to_string : t -> string
    end

    module A : sig
      include Hashtbl.S with type key = Model.Action.t

      type t' = D.t t

      val size : t' -> int
      val update : t' -> Model.Action.t -> D.t -> unit
      val to_string : t' -> string
    end

    module T : sig
      include Hashtbl.S with type key = enc

      type t' = A.t' t

      val update : t' -> V.elt -> A.key -> D.t -> unit
      val size : t' -> int
      val to_string : t' -> string
    end

    type t =
      { to_visit : V.elt Queue.t
      ; init : V.elt
      ; states : V.t
      ; transitions : T.t'
      ; ind_defs : M.Ind.t M.B.t
      ; weak : Weak.t option
      }

    val empty : V0.elt -> M.Ind.t M.B.t -> t
    val _log_to_visit : t -> unit
    val _log_ind_defs : M.Ind.t M.B.t -> unit
    val is_silent_transition : EConstr.t -> Weak.t option -> bool option M.mm

    module type Y_Args = sig
      val primary_lts : M.Ind.t
      val rocq_defs : M.Ind.t M.B.t
      val stop : t -> bool
    end

    module type Z_Args = sig
      val g : t ref
      val ind_defs : M.Ind.t M.B.t
    end

    module Make : (Y : Y_Args) -> sig
      val next_to_visit : t -> T.key
      val update_to_visit : t -> T.key -> unit
      val update_states : t -> V.t -> t
      val get_new_constrs : T.key -> M.Constructor.t list M.mm
      val get_new_states : t -> T.key -> V.t M.mm
      val build : t -> t M.mm
    end

    module Extract : (Z : Z_Args) -> sig
      val state : V.elt -> Model.EdgeMap.key
      val states : unit -> Model.Partition.elt
      val terminals : unit -> Model.Partition.elt
      val label : A.key -> Model.Label.t
      val transitions : unit -> Model.Transitions.t
      val constructor_info : unit -> Model.Info.Meta.RocqLTS.t list M.mm
      val meta : unit -> Model.Info.Meta.t M.mm
      val weak_labels : Model.Labels.t -> Model.Labels.t M.mm
      val lts : unit -> Model.LTS.t M.mm
    end

    val build_ind_defs : unit -> M.Ind.t M.B.t M.mm
    val find_primary_lts : M.Ind.t M.B.t -> M.Ind.t M.mm
    val initial_term : Constrexpr.constr_expr -> EConstr.t M.mm
    val make_yargs : M.Ind.t -> M.Ind.t M.B.t -> 'a -> (module Y_Args)
    val make_zargs : M.Ind.t M.B.t -> t ref -> (module Z_Args)
    val build : Constrexpr.constr_expr -> Model.LTS.t M.mm
  end

  val make_xargs
    :  Libnames.qualid
    -> Names.GlobRef.t list
    -> Weak.t option
    -> (module X_Args)

  val fail_if_empty : Model.LTS.t -> unit
  val fail_if_incomplete : Model.LTS.t -> unit
  val fail_if_not_bisim : Model.Bisimilarity.Result.t -> unit

  val extract_lts
    :  Libnames.qualid
    -> Constrexpr.constr_expr
    -> Libnames.qualid list
    -> Weak.t option
    -> Model.LTS.t M.mm

  module Command : sig
    val default_weak_arg : Weak.t option -> Weak.t option

    val build_lts
      :  ?weak:Weak.t option
      -> Libnames.qualid
      -> Constrexpr.constr_expr
      -> Libnames.qualid list
      -> Model.LTS.t M.mm

    val build_fsm
      :  ?weak:Weak.t option
      -> Libnames.qualid
      -> Constrexpr.constr_expr
      -> Libnames.qualid list
      -> Model.FSM.t M.mm

    type t =
      | MakeLTS of rocq_args
      | MakeFSM of rocq_args
      | Saturate of rocq_args
      | Minimize of rocq_args
      | Merge of rocq_pair
      | CheckBisim of rocq_pair

    and rocq_args = Constrexpr.constr_expr * Libnames.qualid

    and rocq_pair =
      { a : rocq_args
      ; b : rocq_args
      }

    val do_make_lts
      :  Constrexpr.constr_expr * Libnames.qualid
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    val do_make_fsm
      :  Constrexpr.constr_expr * Libnames.qualid
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    val do_saturate
      :  Constrexpr.constr_expr * Libnames.qualid
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    val do_minimize
      :  Constrexpr.constr_expr * Libnames.qualid
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    val build_fsms
      :  rocq_args
      -> rocq_args
      -> Libnames.qualid list
      -> (Model.FSM.t * Model.FSM.t) M.mm

    val do_merge
      :  rocq_pair
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    val do_check_bisim
      :  rocq_pair
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    val run : Libnames.qualid list -> t -> Model.Bisimilarity.t option M.mm
  end
end

module Make (Log : Logger.S) (Ctx : Rocq_context.S) (Enc : Encoding.S) :
  S
  with type enc = Enc.t
   and type node = Enc.Tree.Node.t
   and type tree = Enc.Tree.t
   and type trees = Enc.Trees.t = struct
  type enc = Enc.t
  type node = Enc.Tree.Node.t
  type tree = Enc.Tree.t
  type trees = Enc.Trees.t

  module M : Rocq_monad_utils.S with type enc = enc and type tree = tree =
    Rocq_monad_utils.Make (Log) (Ctx) (Enc)

  module Bindings : Bindings.S with type 'a mm = 'a M.mm =
    Bindings.Make (Log) (M)

  module ConstructorBindings :
    Constructor_bindings.S
    with type 'a mm = 'a M.mm
     and type ind = M.Ind.t
     and type instructions = Bindings.Instructions.t
     and type bindings = Bindings.t
     and type constrmap = Bindings.ConstrMap.t' =
    Constructor_bindings.Make (Log) (M) (Bindings)

  module Model :
    Model.S
    with type base = enc
     and type tree = tree
     and type trees = trees
     and type constructorbindings = ConstructorBindings.t =
    Model.Make (Log) (Enc) (ConstructorBindings)

  module LTS = Model.LTS
  module FSM = Model.FSM

  (** [Decode] handles obtaining [EConstr.t] from [module M]. *)
  module Decode :
    Decoder.S
    with type enc = Enc.t
     and type state = Model.State.t
     and type states = Model.States.t
     and type partition = Model.Partition.t
     and type label = Model.Label.t
     and type labels = Model.Labels.t
     and type note = Model.Note.t
     and type annotation = Model.Annotation.t
     and type annotations = Model.Annotations.t
     and type transition = Model.Transition.t
     and type transitions = Model.Transitions.t
     and type action = Model.Action.t
     and type actions = Model.Actions.t
     and type actionmap = Model.ActionMap.t'
     and type edgemap = Model.EdgeMap.t'
     and type rocqlts = Model.Info.Meta.RocqLTS.t
     and type info = Model.Info.t
     and type lts = Model.LTS.t
     and type fsm = Model.FSM.t
     and type result = Model.Bisimilarity.Result.t
     and type bisimilarity = Model.Bisimilarity.t =
    Decoder.Make (Log) (Enc) (M) (ConstructorBindings) (Model)

  let result_log
        ?(decode : bool = true)
        (type a)
        (module FEnc : Json.S with type k = a)
        (module FDec : Json.S with type k = a)
    : (module Json.S with type k = a)
    =
    let module E : Json.S with type k = a =
      (val if decode && !Api.the_output_config.decode_results
           then (module FDec : Json.S with type k = a)
           else (module FEnc : Json.S with type k = a))
    in
    (module E : Json.S with type k = a)
  ;;

  let handle_results
        (type a)
        (m : Output.Kind.t)
        (s : string)
        (x : a)
        (module FLog : Json.S with type k = a)
    : unit
    =
    FLog.log ~m ~s x;
    match m with
    | Result -> if !Api.the_output_config.dump_results then FLog.write s x
    | _ -> ()
  ;;

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
    let get_theory_enc (f : EConstr.t -> bool M.mm) : Enc.t M.mm =
      Log.trace __FUNCTION__;
      let open M.Syntax in
      let* fm = M.get_fwdmap in
      let rec find_theory : (EConstr.t * Enc.t) list -> Enc.t M.mm = function
        | [] -> raise Not_found
        | (x, y) :: tl ->
          let is_match : bool = M.run (f x) in
          if is_match then M.return y else find_theory tl
      in
      M.F.to_seq fm |> List.of_seq |> find_theory
    ;;

    exception NoEncodingFoundFor_TheoriesNone of unit

    let get_None_enc () : Enc.t M.mm =
      Log.trace __FUNCTION__;
      try get_theory_enc is_None with
      | Not_found -> raise (NoEncodingFoundFor_TheoriesNone ())
    ;;

    exception NoEncodingFoundFor_TheoriesSome of unit

    let get_Some_enc () : Enc.t M.mm =
      Log.trace __FUNCTION__;
      try get_theory_enc is_Some with
      | Not_found -> raise (NoEncodingFoundFor_TheoriesSome ())
    ;;

    exception NotEqTheory of unit

    (** *)
    let get_theory_enc_if_eq (x : EConstr.t) (f : EConstr.t -> bool M.mm)
      : Enc.t M.mm
      =
      Log.trace __FUNCTION__;
      let is_eq : bool = M.run (f x) in
      try if is_eq then get_theory_enc f else raise Not_found with
      | Not_found -> raise (NotEqTheory ())
    ;;

    let get_None_enc_if_eq (x : EConstr.t) : Enc.t M.mm =
      Log.trace __FUNCTION__;
      get_theory_enc_if_eq x is_None
    ;;

    let get_Some_enc_if_eq (x : EConstr.t) : Enc.t M.mm =
      Log.trace __FUNCTION__;
      get_theory_enc_if_eq x is_Some
    ;;
  end

  module Weak : Weak.S with type enc = Enc.t = Weak.Make (Log) (Enc) (M)

  (** Config *)
  module Config = struct
    let load_weak_arg : Api.weak_arg -> Weak.t M.mm =
      let open M.Syntax in
      function
      | Api.Option label_tref ->
        let* label : EConstr.t = M.constrexpr_to_econstr label_tref in
        let label_enc : Enc.t = M.encode label in
        (* NOTE: sanity check we can decode these *)
        let _ : EConstr.t = M.decode label_enc in
        Weak.Option label_enc |> M.return
      | Api.Custom (tau_tref, label_ref) ->
        let* tau : EConstr.t = M.constrexpr_to_econstr tau_tref in
        let tau_enc : Enc.t = M.encode tau in
        let* ind, (mib, mip) =
          Nametab.global label_ref |> M.Ind.lts_type_mind
        in
        let label : EConstr.t = Rocq_utils.get_ind_ty ind mib in
        let label_enc : Enc.t = M.encode label in
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
      Log.trace __FUNCTION__;
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

    let get_the_weak_args () : weak_args option =
      match !the_weak_args with None -> None | Some x -> Some !x
    ;;

    let get_the_weak_arg1 () : Weak.t option =
      match get_the_weak_args () with None -> None | Some x -> x.a
    ;;

    let get_the_weak_arg2 () : Weak.t option =
      match get_the_weak_args () with None -> None | Some x -> x.b
    ;;

    (***********************************************************************)

    let the_bounds_args : Api.bounds_args ref = ref Api.default_bounds
    let load_the_bounds_args () : unit = the_bounds_args := !Api.the_bounds_args
  end

  module type X_Args = sig
    val primary_lts : Libnames.qualid
    val grefs : Names.GlobRef.t list
    val weak : Weak.t option
    val bounds : Api.bounds_args
  end

  module Graph
      (T0 : Hashtbl.S with type key = Enc.t)
      (V0 : Set.S with type elt = Enc.t)
      (D0 : Set.S with type elt = Enc.t * Enc.Tree.t)
      (X : X_Args) =
  struct
    (** [module S] is a [Graph] alternative to [module Model.States] for tracking the visited states.
    *)
    module V = struct
      module V2 : Set.S with type elt = Enc.t = V0
      include V2

      let to_string (xs : t) : string =
        Log.trace __FUNCTION__;
        "TODO: Graph.V.to_string"
      ;;
    end

    (** [module D] is similar to [module S], but each "destination state" is paired with a constructor tree detailing which constructors to take to reach it, which in the context of [module A] and [module T] later illustrates how to get from one state to another via certain constructors.
    *)
    module D = struct
      module D2 : Set.S with type elt = Enc.t * Enc.Tree.t = D0
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
      module T2 : Hashtbl.S with type key = Enc.t = T0
      include T2

      type t' = A.t' t

      let update
            (x : t')
            (from : Enc.t)
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
                    [ "from", Enc.to_string k; "->", A.to_string v ]))
      ;;
    end

    (** [t] is a record containing a queue of [EConstr.t]s [to_visit], a set of states visited (i.e., [EConstr.t]s), and a hashtbl mapping [EConstr.t] to a map of [constr_transitions], which maps [action]s to [EConstr.t]s and their [Tree.t].
    *)
    type t =
      { to_visit : Enc.t Queue.t
      ; init : Enc.t
      ; states : V.t
      ; transitions : T.t'
      ; ind_defs : M.Ind.t M.B.t
      ; weak : Weak.t option
      }

    (** [empty init ind_defs weak] is the initial (empty) graph type [t].
        @param init is the initial state.
        @param rocq_defs is a map of rocq inductive definitions of the LTS.
        @param weak is the optional weak arguments. *)
    let empty (init : Enc.t) (ind_defs : M.Ind.t M.B.t) : t =
      Log.trace __FUNCTION__;
      { to_visit = Queue.create ()
      ; init
      ; states = V.empty
      ; transitions = T.create 0
      ; ind_defs
      ; weak = X.weak
      }
    ;;

    let _log_to_visit (x : t) : unit =
      Log.things
        ~__FUNCTION__
        Debug
        "to visit"
        (x.to_visit |> Queue.to_seq |> List.of_seq)
        (Of Enc.to_string)
    ;;

    let _log_ind_defs (xs : M.Ind.t M.B.t) : unit =
      Log.things
        ~__FUNCTION__
        Debug
        "ind_defs"
        (M.B.to_seq xs |> List.of_seq)
        (Of
           (fun ((k, v) : Enc.t * M.Ind.t) ->
             `Assoc
               [ "enc", Enc.json ~as_elt:true k
               ; "ind", M.Ind.json ~as_elt:true v
               ]
             |> Yojson.pretty_to_string))
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
        let b' = if b then "silent" else "not silent" in
        Log.thing ~__FUNCTION__ Debug b' x (Of M.Strfy.econstr);
        M.return (Some b)
      | Some (Custom (tau_enc, label_enc)) ->
        (* let tau_decoding : EConstr.t = M.decode tau_enc in *)
        (* let label_decoding : EConstr.t = M.decode label_enc in *)
        let act_enc : Enc.t = M.encode x in
        let b : bool = Enc.equal tau_enc act_enc in
        let b' = if b then "silent" else "not silent" in
        Log.thing ~__FUNCTION__ Debug b' x (Of M.Strfy.econstr);
        M.return (Some b)
    ;;

    (*********************************************************)

    (* used to make graph *)
    module type Y_Args = sig
      val primary_lts : M.Ind.t
      val rocq_defs : M.Ind.t M.B.t
      val stop : t -> bool
    end

    (* used for extraction *)
    module type Z_Args = sig
      val g : t ref
      val ind_defs : M.Ind.t M.B.t
    end

    module Make (Y : Y_Args) = struct
      let next_to_visit (g : t) : Enc.t =
        Log.trace __FUNCTION__;
        Queue.pop g.to_visit
      ;;

      let update_to_visit (g : t) (x : Enc.t) : unit =
        Log.trace __FUNCTION__;
        Queue.push x g.to_visit
      ;;

      let update_states (g : t) (xs : V.t) : t =
        Log.trace __FUNCTION__;
        { g with states = V.union g.states xs }
      ;;

      let get_new_constrs (from : Enc.t) : M.Constructor.t list M.mm =
        Log.trace __FUNCTION__;
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

      let get_new_states (g : t) (from : Enc.t) : V.t M.mm =
        Log.trace __FUNCTION__;
        Enc.log ~__FUNCTION__ ~m:Debug ~s:"from" from;
        let open M.Syntax in
        let* new_constrs : M.Constructor.t list = get_new_constrs from in
        let iter_body (i : int) (new_states : V.t) =
          let (act, tgt, int_tree) : M.Constructor.t = List.nth new_constrs i in
          let act_dec : EConstr.t = M.decode act in
          let* is_silent : bool option = is_silent_transition act_dec g.weak in
          let label : Model.Label.t = { base = act; is_silent } in
          let trees : Enc.Trees.t = Enc.Trees.singleton int_tree in
          let to_add : Model.Action.t = { label; trees; annotation = None } in
          D.singleton (tgt, int_tree) |> T.update g.transitions from to_add;
          (* NOTE: if [tgt] has not been explored then add [to_visit] *)
          if T.mem g.transitions tgt || V.mem tgt g.states
          then ()
          else update_to_visit g tgt;
          (* NOTE: add [tgt] to [new_states] *)
          M.return (V.add tgt new_states)
        in
        M.iterate 0 (List.length new_constrs - 1) (V.singleton from) iter_body
      ;;

      (** *)
      let rec build (g : t) : t M.mm =
        Log.trace __FUNCTION__;
        (* _log_to_visit g; *)
        Feedback.feedback (WorkerStatus ("a", "b"));
        if Y.stop g
        then (
          Log.trace ~__FUNCTION__ "stop condition";
          M.return g)
        else if Queue.is_empty g.to_visit
        then (
          Log.trace ~__FUNCTION__ "no more to visit";
          M.return g)
        else (
          (* Log.trace ~__FUNCTION__ "continue"; *)
          let enc_to_visit : Enc.t = next_to_visit g in
          let open M.Syntax in
          (* NOTE: [get_new_states] also updates [g.to_visit] *)
          let* new_states : V.t = get_new_states g enc_to_visit in
          let g : t = update_states g new_states in
          build g)
      ;;
    end

    module Extract (Z : Z_Args) = struct
      let state (x : Enc.t) : Model.State.t = { base = x }

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

      let label (x : Model.Action.t) : Model.Label.t = x.label

      let transitions () : Model.Transitions.t =
        Log.trace __FUNCTION__;
        T.fold
          (fun (from : Enc.t)
            (vs : A.t')
            : (Model.Transitions.t -> Model.Transitions.t) ->
            let from : Model.State.t = state from in
            A.fold
              (fun (action : Model.Action.t)
                (vs : D.t)
                : (Model.Transitions.t -> Model.Transitions.t) ->
                let label : Model.Label.t = label action in
                D.fold
                  (fun ((goto, constructor_tree) : Enc.t * Enc.Tree.t)
                    : (Model.Transitions.t -> Model.Transitions.t) ->
                    let goto : Model.State.t = state goto in
                    Model.Transitions.add
                      { from
                      ; goto
                      ; label
                      ; tree = Some constructor_tree
                      ; annotation = None
                      })
                  vs)
              vs)
          !Z.g.transitions
          Model.Transitions.empty
      ;;

      let constructor_info () : Model.Info.Meta.RocqLTS.t list M.mm =
        Log.trace __FUNCTION__;
        let xs = M.B.to_seq Z.ind_defs |> List.of_seq in
        let open M.Syntax in
        let f (i : int) (acc : Model.Info.Meta.RocqLTS.t list) =
          let (enc, v) : Enc.t * M.Ind.t = List.nth xs i in
          match v.kind with
          | LTS x ->
            let* constructors : ConstructorBindings.t list =
              ConstructorBindings.extract_info v
            in
            let open Model.Info.Meta.RocqLTS in
            { base = enc; constructors } :: acc |> M.return
          | _ -> M.return acc
        in
        M.iterate 0 (List.length xs - 1) [] f
      ;;

      let meta () : Model.Info.Meta.t M.mm =
        Log.trace __FUNCTION__;
        let open M.Syntax in
        let* lts : Model.Info.Meta.RocqLTS.t list = constructor_info () in
        let open Model.Info.Meta in
        { is_complete = Queue.is_empty !Z.g.to_visit
        ; is_merged = false
        ; bounds =
            (match X.bounds with
             | States n -> States n
             | Transitions n -> Transitions n)
        ; lts
        }
        |> M.return
      ;;

      let weak_labels (xs : Model.Labels.t) : Model.Labels.t M.mm =
        Log.trace __FUNCTION__;
        match !Z.g.weak with
        | None -> Model.Labels.empty |> M.return
        | Some weak ->
          let f : Enc.t -> bool M.mm =
            match weak with
            | Weak.Option x -> fun (y : Enc.t) -> M.decode y |> IsTheory.is_None
            | Weak.Custom (tau_enc, _) ->
              fun (y : Enc.t) -> Enc.equal tau_enc y |> M.return
          in
          let open M.Syntax in
          let xs : Model.Label.t list = Model.Labels.to_list xs in
          let g (i : int) (acc : Model.Labels.t) =
            let x : Model.Label.t = List.nth xs i in
            let* is_weak : bool = f x.base in
            if is_weak then Model.Labels.add x acc |> M.return else M.return acc
          in
          M.iterate 0 (List.length xs - 1) Model.Labels.empty g
      ;;

      let lts () : LTS.t M.mm =
        Log.trace __FUNCTION__;
        let open M.Syntax in
        let transitions : Model.Transitions.t = transitions () in
        let alphabet : Model.Labels.t = Model.Transitions.labels transitions in
        let* meta : Model.Info.Meta.t = meta () in
        let* weak_labels : Model.Labels.t = weak_labels alphabet in
        let states : Model.States.t = states () in
        let open LTS in
        { init = Some (state !Z.g.init)
        ; terminals = terminals ()
        ; alphabet
        ; states
        ; transitions
        ; info =
            { meta = Some meta
            ; weak_labels
            ; num_states = Model.States.cardinal states
            }
        }
        |> M.return
      ;;
    end

    let build_ind_defs () : M.Ind.t M.B.t M.mm =
      Log.trace __FUNCTION__;
      let num : int = List.length X.grefs in
      Log.thing ~__FUNCTION__ Debug "num" num (Of Utils.Strfy.int);
      let ind_defs : M.Ind.t M.B.t = M.B.create num in
      let open M.Syntax in
      let f (i : int) () =
        let gref : Names.GlobRef.t = List.nth X.grefs i in
        (* NOTE: [M.Ind.lts] encodes [x.ind] into the bi-enc maps. *)
        let* x : M.Ind.t = M.Ind.lts gref in
        M.Ind.log ~__FUNCTION__ ~m:Debug ~s:"x" x;
        (* NOTE: [ind_defs] is a separate map, so add again using same enc. *)
        M.B.replace ind_defs x.enc x;
        let x_enc : Enc.t = M.encode x.ind in
        Enc.log ~__FUNCTION__ ~m:Debug ~s:"x_enc" x_enc;
        let x_dec : EConstr.t = M.decode x.enc in
        M.log_econstr ~__FUNCTION__ ~m:Debug ~s:"x_dec" x_dec;
        M.return ()
      in
      let* () = M.iterate 0 (num - 1) () f in
      ind_defs |> M.return
    ;;

    let find_primary_lts (ind_defs : M.Ind.t M.B.t) : M.Ind.t M.mm =
      Log.trace __FUNCTION__;
      let open M.Syntax in
      let* x : M.Ind.t = Nametab.global X.primary_lts |> M.Ind.lts in
      M.Ind.log ~__FUNCTION__ ~m:Debug ~s:"primary lts" x;
      (* NOTE: catch-all sanity check *)
      M.encode x.ind |> M.B.find ind_defs |> M.return
    ;;

    (** normalize and encode the initial term *)
    let initial_term (init_term : Constrexpr.constr_expr) : EConstr.t M.mm =
      Log.trace __FUNCTION__;
      let open M.Syntax in
      let* init_term : EConstr.t = M.constrexpr_to_econstr init_term in
      let* init_term : EConstr.t = M.econstr_normalize init_term in
      M.return init_term
    ;;

    let make_yargs primary_lts ind_defs the_graph : (module Y_Args) =
      Log.trace __FUNCTION__;
      let module Y = struct
        let primary_lts : M.Ind.t = primary_lts
        let rocq_defs : M.Ind.t M.B.t = ind_defs

        let stop (g : t) : bool =
          Log.trace __FUNCTION__;
          match X.bounds with
          | States n -> V.cardinal g.states > n
          | Transitions n -> T.size g.transitions > n
        ;;
      end
      in
      (module Y : Y_Args)
    ;;

    let make_zargs ind_defs the_graph : (module Z_Args) =
      Log.trace __FUNCTION__;
      let module Z = struct
        let ind_defs : M.Ind.t M.B.t = ind_defs
        let g : t ref = the_graph
      end
      in
      (module Z : Z_Args)
    ;;

    let build (init_term : Constrexpr.constr_expr) : LTS.t M.mm =
      Log.trace __FUNCTION__;
      let open M.Syntax in
      (* NOTE: encode rocq inductive defs *)
      let* ind_defs : M.Ind.t M.B.t = build_ind_defs () in
      let* primary_lts : M.Ind.t = find_primary_lts ind_defs in
      let* init_term : EConstr.t = initial_term init_term in
      let$* _unit env sigma =
        M.Ind.get_lts_term_type primary_lts |> Typing.check env sigma init_term
      in
      let init : Enc.t = M.encode init_term in
      (* NOTE: build the graph *)
      Log.info "Building the Graph...";
      let the_graph : t ref = ref (empty init ind_defs) in
      Queue.push init !the_graph.to_visit;
      (* _log_to_visit !the_graph; *)
      let module G = Make ((val make_yargs primary_lts ind_defs the_graph)) in
      Feedback.feedback (WorkerStatus ("a", "b"));
      let* the_graph : t = G.build !the_graph in
      (* M.return !the_graph *)
      Log.info ~__FUNCTION__ "Completed Graph, Extracting LTS...";
      let module L = Extract ((val make_zargs ind_defs (ref the_graph))) in
      let* the_lts : LTS.t = L.lts () in
      M.return the_lts
    ;;

    (* let extract_lts (init_term : Constrexpr.constr_expr) : LTS.t M.mm =
       let open M.Syntax in
       let* the_graph = build init_term in
       let module L = Extract ((val make_zargs ind_defs the_graph)) in
       let* the_lts : LTS.t = L.lts () in
       M.return the_lts *)
  end

  let make_xargs primary_lts grefs weak : (module X_Args) =
    Log.trace __FUNCTION__;
    let module X = struct
      let primary_lts : Libnames.qualid = primary_lts
      let grefs : Names.GlobRef.t list = Nametab.global primary_lts :: grefs
      let weak : Weak.t option = weak
      let bounds : Api.bounds_args = !Config.the_bounds_args
    end
    in
    (module X : X_Args)
  ;;

  let fail_if_empty (x : LTS.t) : unit =
    if
      !Api.the_fail_flags.empty
      && (Int.equal (Model.States.cardinal x.states) 1
          || Model.States.is_empty x.states)
      && Model.Transitions.is_empty x.transitions
    then (
      Log.trace ~__FUNCTION__ "LTS Empty";
      M.Err.lts_empty ())
  ;;

  let fail_if_incomplete (x : LTS.t) : unit =
    if !Api.the_fail_flags.incomplete
    then (
      match x with
      | { info = { meta = Some { is_complete = false; _ }; _ }; _ } ->
        Log.trace ~__FUNCTION__ "LTS Incomplete";
        M.Err.lts_incomplete "TODO..."
      | _ -> ())
  ;;

  let fail_if_not_bisim (x : Model.Bisimilarity.Result.t) : unit =
    if !Api.the_fail_flags.incomplete
    then
      if Bool.not (Model.Bisimilarity.Result.are_bisimilar x)
      then (
        Log.trace ~__FUNCTION__ "Not Bisimilar";
        M.Err.not_bisimilar ())
  ;;

  let extract_lts
        (primary_lts : Libnames.qualid)
        (init : Constrexpr.constr_expr)
        (names : Libnames.qualid list)
        (weak : Weak.t option)
    : LTS.t M.mm
    =
    Log.trace __FUNCTION__;
    let t = M.make_enc_hashtbl () in
    let v = M.make_enc_set () in
    let d = M.make_state_tree_pair_set () in
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
    (** [default_weak_arg x] will return [Config.get_the_weak_arg1] in the case that [x] is [None], else returns [x].
    *)
    let default_weak_arg : Weak.t option -> Weak.t option = function
      | None -> Config.get_the_weak_arg1 ()
      | Some x -> Some x
    ;;

    let build_lts
          ?(weak : Weak.t option = None)
          (primary_lts : Libnames.qualid)
          (init : Constrexpr.constr_expr)
          (names : Libnames.qualid list)
      : LTS.t M.mm
      =
      Log.trace __FUNCTION__;
      default_weak_arg weak |> extract_lts primary_lts init names
    ;;

    let build_fsm
          ?(weak : Weak.t option = None)
          (primary_lts : Libnames.qualid)
          (init : Constrexpr.constr_expr)
          (names : Libnames.qualid list)
      : FSM.t M.mm
      =
      Log.trace __FUNCTION__;
      let open M.Syntax in
      let* the_lts = build_lts ~weak primary_lts init names in
      Model.FSM.of_lts the_lts |> M.return
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

    let do_make_lts (x, primary_lts) refs : Model.Bisimilarity.t option M.mm =
      Log.trace __FUNCTION__;
      let open M.Syntax in
      Log.info "Extracting LTS...";
      let* the_lts = build_lts primary_lts x refs in
      result_log (module Model.LTS) (module Decode.LTS)
      |> handle_results Result "Finished Extracting LTS" the_lts;
      M.return None
    ;;

    let do_make_fsm (x, primary_lts) refs : Model.Bisimilarity.t option M.mm =
      Log.trace __FUNCTION__;
      Log.info "Making FSM (from extracted LTS)...";
      let open M.Syntax in
      let* the_fsm = build_fsm primary_lts x refs in
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Result "Finished Making FSM" the_fsm;
      M.return None
    ;;

    let do_saturate (x, primary_lts) refs : Model.Bisimilarity.t option M.mm =
      Log.trace __FUNCTION__;
      Log.info "Making FSM (from extracted LTS)...";
      let open M.Syntax in
      let* the_fsm = build_fsm primary_lts x refs in
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Info "Finished Making FSM" the_fsm;
      Log.info "Saturating FSM...";
      let the_fsm = Model.FSM.saturate the_fsm in
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Result "Finished Saturating FSM" the_fsm;
      M.return None
    ;;

    let do_minimize (x, primary_lts) refs : Model.Bisimilarity.t option M.mm =
      Log.trace __FUNCTION__;
      Log.info "Making FSM (from extracted LTS)...";
      let open M.Syntax in
      let* the_fsm = build_fsm primary_lts x refs in
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Info "Finished Making FSM" the_fsm;
      Log.info "Minimizing FSM...";
      let { fsm; pi } : Model.Minimize.t = Model.Minimize.fsm the_fsm in
      Decode.Partition.log ~m:Info ~s:"pi" pi;
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Result "Finished Minimizing FSM" the_fsm;
      M.return None
    ;;

    let build_fsms
          ((ax, alts) : rocq_args)
          ((bx, blts) : rocq_args)
          (refs : Libnames.qualid list)
      : (FSM.t * FSM.t) M.mm
      =
      Log.trace __FUNCTION__;
      Log.info "Making FSMs...";
      let open M.Syntax in
      Log.info "Making FSM A...";
      let weak1 : Weak.t option = Config.get_the_weak_arg1 () in
      let* the_fsm_a = build_fsm ~weak:weak1 alts ax refs in
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Info "Finished Making FSM A" the_fsm_a;
      Log.info "Making FSM B...";
      let weak2 : Weak.t option = Config.get_the_weak_arg2 () in
      let* the_fsm_b = build_fsm ~weak:weak2 blts bx refs in
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Info "Finished Making FSM B" the_fsm_b;
      M.return (the_fsm_a, the_fsm_b)
    ;;

    let do_merge { a; b } refs : Model.Bisimilarity.t option M.mm =
      Log.trace __FUNCTION__;
      let open M.Syntax in
      let* the_fsm_a, the_fsm_b = build_fsms a b refs in
      Log.info "Merging FSMs...";
      let the_fsm = FSM.merge the_fsm_a the_fsm_b in
      result_log (module Model.FSM) (module Decode.FSM)
      |> handle_results Result "Finished Merging FSMs" the_fsm;
      M.return None
    ;;

    let do_check_bisim { a; b } refs : Model.Bisimilarity.t option M.mm =
      Log.trace __FUNCTION__;
      let open M.Syntax in
      let* the_fsm_a, the_fsm_b = build_fsms a b refs in
      Log.info "Checking Bisimilarity of FSMs...";
      let result = Model.Bisimilarity.fsm the_fsm_a the_fsm_b in
      let r = result_log (module Model.FSM) (module Decode.FSM) in
      r |> handle_results Result "FSM a (original)" result.fsm_a.original;
      r |> handle_results Result "FSM a (saturated)" result.fsm_a.saturated;
      r |> handle_results Result "FSM b (original)" result.fsm_b.original;
      r |> handle_results Result "FSM b (saturated)" result.fsm_b.saturated;
      result_log
        ~decode:false
        (module Model.Bisimilarity.Result)
        (module Decode.Result)
      |> handle_results Result "Finished Merging FSMs" result.result;
      fail_if_not_bisim result.result;
      M.return (Some result)
    ;;

    let run (refs : Libnames.qualid list) (x : t)
      : Model.Bisimilarity.t option M.mm
      =
      Log.trace __FUNCTION__;
      let open M.Syntax in
      Config.load_the_bounds_args ();
      let* () = Config.load_weak_args () in
      let r =
        match x with
        | MakeLTS args -> do_make_lts args refs
        | MakeFSM args -> do_make_fsm args refs
        | Saturate args -> do_saturate args refs
        | Minimize args -> do_minimize args refs
        | Merge args -> do_merge args refs
        | CheckBisim args -> do_check_bisim args refs
      in
      Log.notice "(done.)";
      r
    ;;
  end
end

(** [make ?log ?enc ?ctx] constructs a [Wrapper.S] module.
    @param ?log
      is a function that returns a [module Logger.S]. By default, this is obtained from the configuration in [module Api], via [Api.make_logger ()]. This is then used to construct the [Encoding.S] as well as [Wrapper.S].
    @param ?enc
      is a function that takes a [module Logger.S] and returns a [module Encoding.S]. The default encoding uses [Int.t].
    @param ?ctx is the rocq-context. *)
let make
      ?(log : unit -> (module Logger.S) = Api.make_logger)
      ?(enc : (module Logger.S) -> (module Encoding.S) = Api.make_enc_int)
      ?(ctx : (module Rocq_context.S) = (module Rocq_context.Default))
      ()
  : (module S)
  =
  let module Log : Logger.S = (val log ()) in
  let module Enc : Encoding.S = (val enc (module Log)) in
  let module W : S = Make (Log) ((val ctx)) (Enc) in
  (module W : S)
;;
