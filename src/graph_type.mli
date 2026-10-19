module type S = sig
  type enc
  type tree
  type ind
  type action
  type weak
  type 'a mm

  (** The states found so far, as encodings. *)
  module States : Set.S with type elt = enc

  (** A step's targets, each with the derivation tree that reaches it. *)
  module Destinations : Set.S with type elt = enc * tree

  (** The steps from one state: a table from each action to its
      {!Destinations}. *)
  module Actions : sig
    include Hashtbl.S with type key = action

    type t' = Destinations.t t

    (** [size m] is the number of (action, destination) pairs in [m]. Raises
        nothing. *)
    val size : t' -> int

    (** [update m a d] adds the destinations [d] to the action [a] in [m]
        (nothing if [d] is empty). Raises nothing. *)
    val update : t' -> key -> Destinations.t -> unit
  end

  (** The graph's steps: a table from each state to its {!Actions}. *)
  module Transitions : sig
    include Hashtbl.S with type key = enc

    type t' = Actions.t' t

    (** [size m] is the number of (state, action, destination) triples in
        [m]. Raises nothing. *)
    val size : t' -> int

    (** [update m s a d] adds the destinations [d] to the action [a] from
        [s], giving [s] an entry if it has none. Raises nothing. *)
    val update : t' -> key -> action -> Destinations.t -> unit
  end

  (** Tables keyed by encoding. *)
  module B : Hashtbl.S with type key = enc

  (** Tables keyed by term. *)
  module F : Hashtbl.S with type key = EConstr.t

  (** The LTSs given in [Using], by the encoding of each. *)
  type indmap = ind B.t

  (** A graph under exploration: the states still [to_visit], the initial
      state, the states found, the [transitions] found from them, the LTSs
      that may be used ([ltsmap]) and the one being explored ([primarylts]),
      the silent label, if any, and the bounds that stop exploration. *)
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

  (** [create init ltsmap primary weak] is a graph about to explore from
      [init], with nothing visited yet. Raises nothing. *)
  val create : enc -> indmap -> ind -> weak option -> t

  (** Raised by {!next_to_visit}: nothing is left to visit. *)
  exception NoMoreToVisit

  (** [next_to_visit g] is the next state to visit, taken off [g]'s queue.

      @raise NoMoreToVisit if the queue is empty (raised here). *)
  val next_to_visit : t -> enc

  (** [update_to_visit g x] queues [x] to be visited. Raises nothing. *)
  val update_to_visit : t -> enc -> unit

  (** [update_states g xs] is [g] with the states [xs] added. Raises
      nothing. *)
  val update_states : t -> States.t -> t

  (** [is_silent_label x w] is whether the label [x] is silent under the
      silent label [w]: [None] if there is none; for [Option], whether [x]
      is [None]; for [Custom (tau, _)], whether [x] is [tau]. Raises
      nothing. *)
  val is_silent_label : Evd.econstr -> weak option -> bool option mm
end

module type Args = sig
  type enc
  type tree

  module S : Set.S with type elt = enc
  module D : Set.S with type elt = enc * tree
  module T : Hashtbl.S with type key = enc

  (** The bounds that stop exploration. *)
  val bounds : Api.bounds_args
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
