module type S = sig
  include Wrapper.S

  val the_result : Decode.bisimilarity ref option ref

  exception NoResultFound

  val get_the_result : unit -> Model.Bisimilarity.t
  val get_fsm_a : ?saturated:bool -> unit -> Model.FSM.t
  val get_fsm_b : ?saturated:bool -> unit -> Model.FSM.t

  exception CannotOverrideResult of Model.Bisimilarity.t

  val set_the_result : Model.Bisimilarity.t -> unit

  exception BisimilarityResultNotFound

  val check_bisimilarity
    :  Libnames.qualid list
    -> Constrexpr.constr_expr * Libnames.qualid
    -> Constrexpr.constr_expr * Libnames.qualid
    -> unit

  val get_bisimilar_partition : unit -> Model.Partition.t

  val get_bisimilar_states
    :  ?pi:Model.Partition.t
    -> Model.State.t
    -> Model.States.t

  val are_states_bisimilar : Model.State.t -> Model.State.t -> bool

  (* val get_candidates : Model.State.t -> Model.Label.t -> Model.EdgeMap.t' -> Model.State.t -> Model.States.t *)
end

module Make
    (Log : Logger.S)
    (* (W : Wrapper.S) *)
     (Ctx : Rocq_context.S)
    (Enc : Encoding.S) :
  S
  with module M.Ctx = Ctx
   and type enc = Enc.t
   and type node = Enc.Tree.Node.t
   and type tree = Enc.Tree.t
   and type trees = Enc.Trees.t
(* with type enc = W.enc
   and type node = W.node
   and type tree = W.tree
   and type trees = W.trees *) = struct
  module W :
    Wrapper.S
    with module M.Ctx = Ctx
     and type enc = Enc.t
     and type node = Enc.Tree.Node.t
     and type tree = Enc.Tree.t
     and type trees = Enc.Trees.t =
    Wrapper.Make (Log) (Ctx) (Enc)

  include W

  let the_result : Model.Bisimilarity.t ref option ref = ref None

  exception NoResultFound

  let get_the_result () : Model.Bisimilarity.t =
    match !the_result with None -> raise NoResultFound | Some x -> !x
  ;;

  let get_fsm_a ?(saturated : bool = false) () : Model.FSM.t =
    if saturated
    then (get_the_result ()).fsm_a.saturated
    else (get_the_result ()).fsm_a.original
  ;;

  let get_fsm_b ?(saturated : bool = false) () : Model.FSM.t =
    if saturated
    then (get_the_result ()).fsm_b.saturated
    else (get_the_result ()).fsm_b.original
  ;;

  exception CannotOverrideResult of Model.Bisimilarity.t

  let set_the_result (x : Model.Bisimilarity.t) : unit =
    match !the_result with
    | None -> the_result := Some (ref x)
    | Some y -> raise (CannotOverrideResult !y)
  ;;

  exception BisimilarityResultNotFound

  let check_bisimilarity
        (refs : Libnames.qualid list)
        (a : Constrexpr.constr_expr * Libnames.qualid)
        (b : Constrexpr.constr_expr * Libnames.qualid)
    : unit
    =
    let r : Model.Bisimilarity.t option =
      Command.run refs (Command.CheckBisim { a; b })
      |> M.run ~reset_encoding:true
    in
    match r with
    | None -> raise BisimilarityResultNotFound
    | Some r -> set_the_result r
  ;;

  let get_bisimilar_partition () : Model.Partition.t =
    (get_the_result ()).result.bisim_states
  ;;

  let get_bisimilar_states
        ?(pi : Model.Partition.t = get_bisimilar_partition ())
        (x : Model.State.t)
    : Model.States.t
    =
    try pi |> Model.Partition.get_bisimilar x with
    | Not_found -> Model.States.empty
  ;;

  let are_states_bisimilar (x : Model.State.t) (y : Model.State.t) : bool =
    get_bisimilar_states x |> Model.States.mem y
  ;;

  (** [get_candidates from goto edges] returns the set of states reachable from state [from] that are bisimilar with state [goto].
      @param from is a state of fsm "b".
      @param label is the label of the action taken by fsm "b".
      @param edges is the [Model.EdgeMap.t'] of fsm "b".
      @param goto is a state of fsm "a". *)
  (* let get_candidates
     (from : Model.State.t)
     (label : Model.Label.t)
     (edges : Model.EdgeMap.t')
     (goto : Model.State.t)
     : Model.States.t
     =
     let reachable : Model.Partition.t =
     get_bisimilar_partition ()
     |> Model.Partition.reachable_by_label from label edges
     in
     get_bisimilar_states ~pi:reachable goto
     ;; *)
end

(* let make
   ?(log : unit -> (module Logger.S) = Api.make_logger)
   ?(enc : (module Logger.S) -> (module Encoding.S) = Api.make_enc_int)
   ?(ctx : (module Rocq_context.S) = (module Rocq_context.Default))
   ()
   : (module S)
   =
   let module Log : Logger.S = (val log ()) in
   let module W : Wrapper.S =
   (val Wrapper.make ~log:(fun () -> (module Log)) ~enc ~ctx ())
   in
   (module Make (Log) (W) : S)
   ;; *)
