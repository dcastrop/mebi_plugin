module type S = sig
  include Wrapper.S

  (* module Command :
     Command.S
     with type weak = Weak.t
     and type 'a mm = 'a M.mm
     and type lts = Model.LTS.t
     and type fsm = Model.FSM.t
     and type bisimilarity = Model.Bisimilarity.t
     and type result = Model.Bisimilarity.Result.t *)

  val the_result : Decode.bisimilarity ref option ref

  exception NoResultFound

  val get_the_result : unit -> Model.Bisimilarity.t

  (** Whether the two systems' roles are swapped for the goal in focus: [true]
      while answering the right-hand obligation ([bisim_r]) of a
      [weak_bisimilar] goal, where the {e right} system moves and the left
      answers. {!get_fsm_a} and {!get_fsm_b} follow it. *)
  val swapped : bool ref

  (** FSM "a": the system whose move is being answered -- the left one,
      unless {!swapped}. *)
  val get_fsm_a : ?saturated:bool -> unit -> Model.FSM.t

  (** FSM "b": the system that answers -- the right one, unless
      {!swapped}. *)
  val get_fsm_b : ?saturated:bool -> unit -> Model.FSM.t

  exception CannotOverrideResult of Model.Bisimilarity.t

  val set_the_result : Model.Bisimilarity.t -> unit

  exception BisimilarityResultNotFound

  (** For a [weak_sim] goal whose two states are not bisimilar: each left
      state's simulators ({!Model.Product.simulation}), the answers the
      solver falls back on when no bisimilar one exists. [None] otherwise. *)
  val simulators : (Model.State.t -> Model.State.Set.t) option ref

  (** The answer plan for the proof now being solved, when the answer policy
      is not [Default] ({!Model.Product.Policy.plan}); [None] otherwise, and
      the solver answers with {!Model.Product.answer}. *)
  val plan : Model.Product.Policy.plan option ref

  val check_bisimilarity
    :  ?fail_if_not_bisim:bool
    -> Libnames.qualid list
    -> Constrexpr.constr_expr * Libnames.qualid
    -> Constrexpr.constr_expr * Libnames.qualid
    -> unit

  val get_bisimilar_partition : unit -> Model.Partition.t

  val get_bisimilar_states
    :  ?pi:Model.Partition.t
    -> Model.State.t
    -> Model.State.Set.t
end

module Make (Enc : Encoding.S) :
  S
  with type enc = Enc.t
   and type node = Enc.Tree.Node.t
   and type tree = Enc.Tree.t
   and type trees = Enc.Trees.t = struct
  module W = Wrapper.Make (Enc)
  include W

  (* See the [.mli]. *)
  let the_result : Model.Bisimilarity.t ref option ref = ref None

  exception NoResultFound

  (* See the [.mli]. *)
  let get_the_result () : Model.Bisimilarity.t =
    match !the_result with None -> raise NoResultFound | Some x -> !x
  ;;

  (* See the [.mli]. *)
  let swapped : bool ref = ref false

  (** [pick p saturated] is the saturated or the original FSM of the pair
      [p]. The right-hand obligation of a [weak_bisimilar] goal is the
      left-hand one with the two systems exchanged, so the solver answers it
      by reading the FSMs the other way round ({!swapped}) rather than with a
      mirrored copy of itself. Raises nothing. *)
  let pick (x : Model.Bisimilarity.FSMPair.t) (saturated : bool) : Model.FSM.t =
    if saturated then x.saturated else x.original
  ;;

  (* See the [.mli]. *)
  let get_fsm_a ?(saturated : bool = false) () : Model.FSM.t =
    let r = get_the_result () in
    pick (if !swapped then r.fsm_b else r.fsm_a) saturated
  ;;

  (* See the [.mli]. *)
  let get_fsm_b ?(saturated : bool = false) () : Model.FSM.t =
    let r = get_the_result () in
    pick (if !swapped then r.fsm_a else r.fsm_b) saturated
  ;;

  exception CannotOverrideResult of Model.Bisimilarity.t

  (* See the [.mli]. *)
  let set_the_result (x : Model.Bisimilarity.t) : unit =
    match !the_result with
    | None -> the_result := Some (ref x)
    | Some y -> raise (CannotOverrideResult !y)
  ;;

  exception BisimilarityResultNotFound

  (* See the [.mli]. *)
  let simulators : (Model.State.t -> Model.State.Set.t) option ref = ref None

  (* See the [.mli]. *)
  let plan : Model.Product.Policy.plan option ref = ref None

  (* See the [.mli]. The fail flag is restored however the check ends. *)
  let check_bisimilarity
        ?(fail_if_not_bisim : bool = true)
        (refs : Libnames.qualid list)
        (a : Constrexpr.constr_expr * Libnames.qualid)
        (b : Constrexpr.constr_expr * Libnames.qualid)
    : unit
    =
    (* Set directly, not via [Api.set_fail_flag_non_bisimilar], which
       announces the change to the user. *)
    let flags : Api.fail_flags = !Api.the_fail_flags in
    if Bool.not fail_if_not_bisim
    then Api.the_fail_flags := { flags with non_bisimilar = false };
    let r : Model.Bisimilarity.t option =
      Fun.protect
        ~finally:(fun () -> Api.the_fail_flags := flags)
        (fun () ->
          Command.run refs (Command.CheckBisim { a; b })
          |> M.run ~reset_encoding:true)
    in
    match r with
    | None -> raise BisimilarityResultNotFound
    | Some r -> set_the_result r
  ;;

  (* See the [.mli]. *)
  let get_bisimilar_partition () : Model.Partition.t =
    (get_the_result ()).result.bisim_states
  ;;

  (* See the [.mli]. *)
  let get_bisimilar_states
        ?(pi : Model.Partition.t = get_bisimilar_partition ())
        (x : Model.State.t)
    : Model.State.Set.t
    =
    try pi |> Model.Partition.get_bisimilar x with
    | Not_found -> Model.State.Set.empty
  ;;
end
