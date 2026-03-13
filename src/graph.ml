(* module type S = sig
  type enc
  type tree
  type action

  module States : Set.S with type elt = enc
  module Destinations : Set.S with type elt = enc * tree
  module Actions : Hashtbl.S with type key = action
end

module Make
    (Log : Logger.S)
    (Enc : Encoding.S)
    (Action : Action.S with type trees = Enc.Trees.t)
    (S : Set.S with type elt = Enc.t)
    (D : Set.S with type elt = Enc.t * Enc.Tree.t)
    (A : Hashtbl.S with type key = Action.t)
    (T : Hashtbl.S with type key = Enc.t) :
  S with type enc = Enc.t and type tree = Enc.Tree.t with type action = Action.t =
struct
  type enc = Enc.t
  type tree = Enc.Tree.t
  type action = Action.t

  (** [module States] is for tracking visited states, an alternative to [Model.States].
  *)
  module States : Set.S with type elt = Enc.t = S

  (** [module Destinations] is similar to [module S], but each "destination state" is paired with a constructor tree detailing which constructors to take to reach it, which in the context of [module Actions] and [module Transitions] later illustrates how to get from one state to another via certain constructors.
  *)
  module Destinations : Set.S with type elt = Enc.t * Enc.Tree.t = D

  (** [module Actions] is a [Graph] alternative to [Model.ActionMap] *)
  module Actions = struct
    module Map_ : Hashtbl.S with type key = Action.t = A
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
    module Map_ : Hashtbl.S with type key = Enc.t = T
    include T

    type t' = Actions.t' t

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

    let size (xs : t') : int =
      Log.trace __FUNCTION__;
      fold (fun k v n -> Actions.size v + n) xs 0
    ;;
  end

  (** [t] is a record containing a queue of [EConstr.t]s [to_visit], a set of states visited (i.e., [EConstr.t]s), and a hashtbl mapping [EConstr.t] to a map of [constr_transitions], which maps [action]s to [EConstr.t]s and their [Tree.t].
  *)
  (* type t =
      { to_visit : Enc.t Queue.t
      ; init : Enc.t
      ; states : Visited.t
      ; transitions : Transitions.t'
      ; ind_defs : M.Ind.t M.B.t
      ; weak : Weak.t option
      } *)
end *)
