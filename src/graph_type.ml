module type S = sig
  type enc
  type tree
  type ind
  type action
  type weak
  type 'a mm

  module States : Set.S with type elt = enc
  module Destinations : Set.S with type elt = enc * tree

  module Actions : sig
    include Hashtbl.S with type key = action

    type t' = Destinations.t t

    val size : t' -> int
    val update : t' -> key -> Destinations.t -> unit
  end

  module Transitions : sig
    include Hashtbl.S with type key = enc

    type t' = Actions.t' t

    val size : t' -> int
    val update : t' -> key -> action -> Destinations.t -> unit
  end

  module B : Hashtbl.S with type key = enc
  module F : Hashtbl.S with type key = EConstr.t

  type indmap = ind B.t

  type t =
    { to_visit : enc Queue.t
    ; init : enc
    ; states : States.t
    ; transitions : Transitions.t'
    ; ltsmap : indmap
    ; primarylts : ind
    ; weak : weak option
    ; bounds : Api.bounds_args
    }

  val create : enc -> indmap -> ind -> weak option -> t

  exception NoMoreToVisit

  val next_to_visit : t -> enc
  val update_to_visit : t -> enc -> unit
  val update_states : t -> States.t -> t
  val is_silent_label : Evd.econstr -> weak option -> bool option mm
end

module type Args = sig
  type enc
  type tree

  module S : Set.S with type elt = enc
  module D : Set.S with type elt = enc * tree
  module T : Hashtbl.S with type key = enc

  val bounds : Api.bounds_args
end

module Make
    (Log : Logger.S)
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
       Model_.S
       with type base = Enc.t
        and type tree = Enc.Tree.t
        and type trees = Enc.Trees.t
        and type constructorbindings = ConstructorBindings.t)
    (X : Args with type enc = Enc.t and type tree = Enc.Tree.t) :
  S
  with type enc = Enc.t
   and type tree = Enc.Tree.t
   and type ind = M.Ind.t
   and type action = Model.Action.t
   and type weak = Weak.t
   and module B = M.B
   and module F = M.F
   and type 'a mm = 'a M.mm = struct
  module B = M.B
  module F = M.F

  type enc = Enc.t
  type tree = Enc.Tree.t
  type ind = M.Ind.t
  type action = Model.Action.t
  type weak = Weak.t
  type 'a mm = 'a M.mm

  (** [module Action] is [Model.Action]. *)
  module Action = Model.Action

  (** [module States] is for tracking visited states, an alternative to [Model.States].
  *)
  module States : Set.S with type elt = Enc.t = X.S
  (* (val M.make_enc_set ()) *)

  (** [module Destinations] is similar to [module States], but each "destination state" is paired with a constructor tree detailing which constructors to take to reach it, which in the context of [module Actions] and [module Transitions] later illustrates how to get from one state to another via certain constructors.
  *)
  module Destinations : Set.S with type elt = Enc.t * Enc.Tree.t = X.D

  (** [module Actions] is a [Graph] alternative to [Model.ActionMap] *)
  module Actions = struct
    module Map_ : Hashtbl.S with type key = Action.t = Hashtbl.Make (Action)
    include Map_

    type t' = Destinations.t t

    let size (xs : t') : int =
      Log.trace __FUNCTION__;
      fold (fun k v n -> Destinations.cardinal v + n) xs 0
    ;;

    let update (x : t') (action : Action.t) (states : Destinations.t) : unit =
      Log.trace __FUNCTION__;
      if Destinations.is_empty states
      then ()
      else (
        match find_opt x action with
        | None -> add x action states
        | Some old_states ->
          replace x action (Destinations.union old_states states))
    ;;
  end

  (** [module Transitions] is an alternative to [Model.EdgeMap], but for transitions.
  *)
  module Transitions = struct
    module Map_ : Hashtbl.S with type key = Enc.t = X.T
    include Map_

    type t' = Actions.t' t

    let size (xs : t') : int =
      Log.trace __FUNCTION__;
      fold (fun k v n -> Actions.size v + n) xs 0
    ;;

    let update
          (x : t')
          (from : Enc.t)
          (action : Action.t)
          (destinations : Destinations.t)
      : unit
      =
      Log.trace __FUNCTION__;
      match find_opt x from with
      | None ->
        [ action, destinations ] |> List.to_seq |> Actions.of_seq |> add x from
      | Some actions -> Actions.update actions action destinations
    ;;
  end

  (** [type indmap] maps encoded terms to their inductive Rocq LTS. @see [module M.Ind]. *)
  type indmap = M.Ind.t M.B.t

  (** [type t] is a record containing a queue of [EConstr.t]s [to_visit], a set of states visited (i.e., [EConstr.t]s), and a hashtbl mapping [EConstr.t] to a map of [constr_transitions], which maps [action]s to [EConstr.t]s and their [Tree.t].
  *)
  type t =
    { to_visit : Enc.t Queue.t
    ; init : Enc.t
    ; states : States.t
    ; transitions : Transitions.t'
    ; ltsmap : indmap
    ; primarylts : M.Ind.t
    ; weak : Weak.t option
    ; bounds : Api.bounds_args
    }

  (** [create init ltsmap primarylts weak] returns an initial [t]. *)
  let create
        (init : Enc.t)
        (ltsmap : indmap)
        (primarylts : M.Ind.t)
        (weak : Weak.t option)
    : t
    =
    { to_visit = Queue.create ()
    ; init
    ; states = States.empty
    ; transitions = Transitions.create 0
    ; ltsmap
    ; primarylts
    ; weak
    ; bounds = X.bounds
    }
  ;;

  exception NoMoreToVisit

  let next_to_visit (g : t) : Enc.t =
    Log.trace __FUNCTION__;
    try Queue.take g.to_visit with Queue.Empty -> raise NoMoreToVisit
  ;;

  let update_to_visit (g : t) (x : Enc.t) : unit =
    Log.trace __FUNCTION__;
    Queue.add x g.to_visit
  ;;

  let update_states (g : t) (xs : States.t) : t =
    Log.trace __FUNCTION__;
    { g with states = States.union g.states xs }
  ;;

  (** [is_silent_label x weakopt] returns [Some bool] indicating if [x] is recognized to be representative of a silent action, as configured by [weakopt]. If [weakopt=None] then returns [None].
  *)
  let is_silent_label (x : EConstr.t) : Weak.t option -> bool option M.mm =
    Log.trace __FUNCTION__;
    function
    | None -> M.return None
    | Some (Option label_enc) ->
      let open M.Syntax in
      let* b : bool = Theory.is_None x in
      M.return (Some b)
    | Some (Custom (tau_enc, label_enc)) ->
      let act_enc : Enc.t = M.encode x in
      let b : bool = Enc.equal tau_enc act_enc in
      M.return (Some b)
  ;;
end
