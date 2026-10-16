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
      ; step_goto : state option
      }

    include Json.S with type k = t

    exception TransitionHasNoConstructorsToApply

    val init : transition -> t
  end

  module StateM : sig
    type t =
      | Done
      | NewProof of (Constrexpr.constr_expr * Constrexpr.constr_expr)
      | OpenBlock
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

      include Json.List.Make (struct
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
      ; step_goto : W.Model.State.t option
        (** the current strong step's target, bound into its first
            constructor's arguments (backlog I2, stage 2) *)
      }

    include Json.Thing.Make (struct
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

    (* See the [.mli]. *)
    let init ({ from; goto; label; tree; annotation } : W.Model.Transition.t)
      : t
      =
      { current = None
      ; step_goto = None
      ; destination = goto
      ; label
      ; remaining =
          (match annotation, tree with
           (* NOTE: is a saturated transition *)
           | Some y, _ -> Some y
           (* NOTE: is an unsaturated transition, so we use the contructor tree
           *)
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
    (* See the [.mli]. *)
    type t =
      | Done
      | NewProof of (Constrexpr.constr_expr * Constrexpr.constr_expr)
      | OpenBlock
      | WeakSim
      | Exists of W.Model.Transition.t option
      | ApplyConstructors of ApplicableConstructors.t

    include Json.Thing.Make (struct
        type k = t

        let name = "PState"

        let json ?(as_elt : bool = false) : t -> Yojson.t = function
          | Done -> `String "Done"
          | OpenBlock -> `String "OpenBlock"
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

  (* See the [.mli]. *)
  let get () : t ref =
    match !the_state with None -> raise NoStateFound | Some x -> x
  ;;

  (* See the [.mli]. *)
  let set (pstate : Declare.Proof.t) (x : StateM.t) : unit =
    the_state := Some (ref { p = pstate; x })
  ;;

  (* See the [.mli]. *)
  let init
        (pstate : Declare.Proof.t)
        (x : Constrexpr.constr_expr * Constrexpr.constr_expr)
    : unit
    =
    set pstate (NewProof x)
  ;;

  (* See the [.mli]. *)
  let get_pstate () : Declare.Proof.t = !(get ()).p

  (* See the [.mli]. *)
  let get_statem () : StateM.t = !(get ()).x

  (* See the [.mli]. *)
  let update_pstate (pstate : Declare.Proof.t) : unit =
    the_state := Some (ref { !(get ()) with p = pstate })
  ;;

  (* See the [.mli]. *)
  let update_statem (state : StateM.t) : unit =
    the_state := Some (ref { !(get ()) with x = state })
  ;;

  (* See the [.mli]. *)
  let is_done () : bool =
    match !(get ()) with
    | { p; x = Done } -> true
    | { p; x } -> Proof.is_done (Declare.Proof.get p)
  ;;

  (* See the [.mli]. *)
  let log ?(__FUNCTION__ : string = "") () : unit =
    Logger.thing
      ~__FUNCTION__
      Debug
      "ProofState.StateM"
      (get_statem ())
      StateM.to_string
  ;;
end
