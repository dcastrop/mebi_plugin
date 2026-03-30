module type S = sig
  type enc
  type node
  type state
  type label
  type annotation
  type transition

  module ApplicableConstructors : sig
    module Nodes : sig
      type t = node list

      include Json.S with type k = t
    end

    type t =
      { label : label
      ; destination : state
      ; current : Nodes.t option
      ; remaining : annotation option
      }

    include Json.S with type k = t

    exception TransitionHasNoConstructorsToApply

    val init : transition -> t
  end

  module StateM : sig
    type t =
      | Done
      | NewProof of (Constrexpr.constr_expr * Constrexpr.constr_expr)
      | WeakSim
      | Exists of transition option
      | ApplyConstructors of ApplicableConstructors.t

    include Json.S with type k = t
  end

  type t =
    { p : Declare.Proof.t
    ; x : StateM.t
    }

  val the_state : t ref option ref

  exception NoStateFound

  val get : unit -> t ref
  val set : Declare.Proof.t -> StateM.t -> unit

  val init
    :  Declare.Proof.t
    -> Constrexpr.constr_expr * Constrexpr.constr_expr
    -> unit

  val get_pstate : unit -> Declare.Proof.t
  val get_statem : unit -> StateM.t
  val update_pstate : Declare.Proof.t -> unit
  val update_statem : StateM.t -> unit
  val is_done : unit -> bool
  val log : ?__FUNCTION__:string -> unit -> unit
end

module Make
    (Log : Logger.S)
    (Enc : Encoding.S)
    (W :
       Results.S
       with type enc = Enc.t
        and type node = Enc.Tree.Node.t
        and type tree = Enc.Tree.t
        and type trees = Enc.Trees.t) :
  S
  with type enc = Enc.t
   and type node = Enc.Tree.Node.t
   and type state = W.Model.State.t
   and type label = W.Model.Label.t
   and type annotation = W.Model.Annotation.t
   and type transition = W.Model.Transition.t = struct
  type enc = Enc.t
  type node = Enc.Tree.Node.t
  type state = W.Model.State.t
  type label = W.Model.Label.t
  type annotation = W.Model.Annotation.t
  type transition = W.Model.Transition.t

  module ApplicableConstructors = struct
    module Nodes = struct
      type t = Enc.Tree.Node.t list

      include
        Json.List.Make
          (Log)
          (struct
            type k = Enc.Tree.Node.t

            let name = "Nodes"
            let json = Enc.Tree.Node.json
          end)
    end

    type t =
      { label : W.Model.Label.t
      ; destination : W.Model.State.t
      ; current : Nodes.t option
      ; remaining : W.Model.Annotation.t option
      }

    include
      Json.Thing.Make
        (Log)
        (struct
          type k = t

          let name = "ApplicableConstructors"

          let json ?(as_elt : bool = false) (x : t) : Yojson.t =
            `Assoc
              [ "label", W.Model.Label.json ~as_elt:true x.label
              ; "destination", W.Model.State.json ~as_elt:true x.destination
              ; "current", Json.option ~as_elt:true Nodes.json x.current
              ; ( "remaining"
                , Json.option ~as_elt:true W.Model.Annotation.json x.remaining )
              ]
          ;;
        end)

    exception TransitionHasNoConstructorsToApply

    let init ({ from; goto; label; tree; annotation } : W.Model.Transition.t)
      : t
      =
      { current = None
      ; destination = goto
      ; label
      ; remaining =
          (match annotation, tree with
           (* NOTE: is a saturated transition *)
           | Some y, _ -> Some y
           (* NOTE: is an unsaturated transition, so we use the contructor tree *)
           | None, Some y ->
             Some
               { this = { from; label; goto; using = Enc.Trees.singleton y }
               ; next = None
               }
           (* NOTE: is neither saturated nor has a constructor tree *)
           | None, None -> raise TransitionHasNoConstructorsToApply)
      }
    ;;
  end

  module StateM = struct
    (** [t] represents the internal state-machine used to solve proofs.
        (Note: We are required to split some of the states over several steps since we need to update the proof iteratively in order to apply what we need to apply. E.g., it is easier to apply the constructors one after the other, since we require the proof to be updated by the previous appliction in order to apply the next.)
        - [NewProof] the start state. We unfold any terms that we can before proceeding to [WeakSim].
        - [WeakSim] either: (i) check if can be solved by cofix in hyps, or (ii) create new cofix. Either stays in [WeakSim] (to invert or unfold the hyps) or proceeds to [Exists], or [Done].
        - [Exists] means that the conclusion begins with an [exists a n2] (where [n2] is some state reached from [n] after taking action [a]). We: (1) extract the transition made by fsm "a" from the hyps (possible requiring inversion beforehand), (2) determine which transition fsm "b" will make in response to the one made by fsm "a" (in the hyps), and (3) apply [ex_intro] and [split] tactics to the conclusion (since we now know what to instantiate state [n2] with). If We may re-enter [Exists] if we make "b" do a reflexive transition, in which case we apply the necessary constructors to finish the case and proceed to [WeakSim], else we proceed to [ApplyConstructors].
        - [ApplyConstructors] is for applying the constructors we know we need to apply in order for fsm "b" to reach a state that is bisimilar to that reached by fsm "a".
        - [Done] means the proof is finished. *)
    type t =
      | Done
      | NewProof of (Constrexpr.constr_expr * Constrexpr.constr_expr)
      | WeakSim
      | Exists of W.Model.Transition.t option
      | ApplyConstructors of ApplicableConstructors.t

    include
      Json.Thing.Make
        (Log)
        (struct
          type k = t

          let name = "PState"

          let json ?(as_elt : bool = false) : t -> Yojson.t = function
            | Done -> `String "Done"
            | WeakSim -> `String "WeakSim"
            | NewProof (_, _) -> `String "NewProof"
            | Exists None -> `String "Exists (None)"
            | Exists (Some _) -> `String "Exists (Some _)"
            | ApplyConstructors _ -> `String "ApplyConstructors"
          ;;
        end)
  end

  type t =
    { p : Declare.Proof.t
    ; x : StateM.t
    }

  let the_state : t ref option ref = ref None

  exception NoStateFound

  let get () : t ref =
    match !the_state with None -> raise NoStateFound | Some x -> x
  ;;

  let set (pstate : Declare.Proof.t) (x : StateM.t) : unit =
    the_state := Some (ref { p = pstate; x })
  ;;

  let init
        (pstate : Declare.Proof.t)
        (x : Constrexpr.constr_expr * Constrexpr.constr_expr)
    : unit
    =
    set pstate (NewProof x)
  ;;

  let get_pstate () : Declare.Proof.t = !(get ()).p
  let get_statem () : StateM.t = !(get ()).x

  let update_pstate (pstate : Declare.Proof.t) : unit =
    the_state := Some (ref { !(get ()) with p = pstate })
  ;;

  let update_statem (state : StateM.t) : unit =
    the_state := Some (ref { !(get ()) with x = state })
  ;;

  (** [is_done ()] is true if the state [x] is [Done], else checks if [p] is done (via [Proof.is_done]).
  *)
  let is_done () : bool =
    match !(get ()) with
    | { p; x = Done } -> true
    | { p; x } -> Proof.is_done (Declare.Proof.get p)
  ;;

  let log ?(__FUNCTION__ : string = "") () : unit =
    Log.thing
      ~__FUNCTION__
      Debug
      "ProofState.StateM"
      (get_statem ())
      StateM.to_string
  ;;
end
