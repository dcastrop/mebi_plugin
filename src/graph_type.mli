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
       Model.S
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
   and type 'a mm = 'a M.mm
