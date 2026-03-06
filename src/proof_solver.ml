exception NothingToDo
exception NotImplemented

module Make (Log : Logger.S) (E : Encoding.SEncoding) = struct
  (** [W] is for running the main part of the algorithm (pre-proof). *)
  module W = struct
    include Wrapper.Make (Log) (Rocq_context.Default) (E)

    let the_result : Model.Bisimilar.t ref option ref = ref None

    exception NoResultFound

    let get_the_result () : Model.Bisimilar.t =
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
    let get_candidates
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
    ;;

    exception CannotOverrideResult of Model.Bisimilar.t

    let set_the_result (x : Model.Bisimilar.t) : unit =
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
      let r : Model.Bisimilar.t option =
        Command.run refs (Command.CheckBisim { a; b })
        |> M.run ~reset_encoding:true
      in
      match r with
      | None -> raise BisimilarityResultNotFound
      | Some r -> set_the_result r
    ;;

    (** [Decode] handles obtaining [EConstr.t] from [module M]. *)
    module Decode = struct
      let enc (x : M.Enc.t) : EConstr.t = M.decode x

      let handle (x : M.Enc.t) (e : exn) : EConstr.t =
        try enc x with M.CannotDecode _ -> raise e
      ;;

      exception CouldNotDecode_State of Model.State.t

      let state (x : Model.State.t) : EConstr.t =
        handle x.term (CouldNotDecode_State x)
      ;;

      exception CouldNotDecode_Label of Model.Label.t

      let label (x : Model.Label.t) : EConstr.t =
        handle x.term (CouldNotDecode_Label x)
      ;;

      exception CouldNotDecode_LTS_Constructor of Model.Info.lts

      let lts_constructor (x : Model.Info.lts) : EConstr.t =
        handle x.enc (CouldNotDecode_LTS_Constructor x)
      ;;
    end
  end

  let check_bisimilarity
    :  Libnames.qualid list
    -> Constrexpr.constr_expr * Libnames.qualid
    -> Constrexpr.constr_expr * Libnames.qualid
    -> unit
    =
    W.check_bisimilarity
  ;;

  module M = W.M
  module Model = W.Model
  module Decode = W.Decode

  let _log_biencoding ?(__FUNCTION__ : string = "") () : unit =
    Log.things
      ~__FUNCTION__
      Debug
      "M.BiEncoding"
      (* (M.bienc_to_list ()) *)
      (M.F.to_seq (M.fwdmap ()) |> List.of_seq)
      (Of
         (fun (econstr, enc) ->
           Printf.sprintf
             "%s :=> %s"
             (M.Enc.to_string enc)
             (M.Strfy.econstr econstr)))
  ;;

  let _log_transition
        ?(__FUNCTION__ : string = "")
        (s : string)
        (x : Model.Transition.t)
    : unit
    =
    Log.thing ~__FUNCTION__ Debug s x (Of Model.Transition.to_string)
  ;;

  let _log_actionpair
        ?(__FUNCTION__ : string = "")
        (s : string)
        (x : Model.ActionPair.t)
    : unit
    =
    Log.thing ~__FUNCTION__ Debug s x (Of Model.ActionPair.to_string)
  ;;

  let _log_state ?(__FUNCTION__ : string = "") (s : string) (x : Model.State.t)
    : unit
    =
    Log.thing ~__FUNCTION__ Debug s x (Of Model.State.to_string)
  ;;

  let _log_states
        ?(__FUNCTION__ : string = "")
        (s : string)
        (x : Model.States.t)
    : unit
    =
    Log.thing ~__FUNCTION__ Debug s x (Of Model.States.to_string)
  ;;

  let _log_label ?(__FUNCTION__ : string = "") (s : string) (x : Model.Label.t)
    : unit
    =
    Log.thing ~__FUNCTION__ Debug s x (Of Model.Label.to_string)
  ;;

  (***********************************************************************)

  (* module Transition = struct
    module Opt = struct
      (** [t] is similar to [Model.Transition.t] {!type:Model.Transition.t} but where [goto] and [label] are optional.
          @see 'Model.ml' for {!type:Model.Transition.t}. *)
      type t =
        { from : Model.State.t
        ; goto : Model.State.t option
        ; label : Model.Label.t option
        ; annotation : Model.Annotation.t option
        ; constructor_tree : Model.Tree.t option
        }

      let from (from : Model.State.t) : t =
        { from
        ; goto = None
        ; label = None
        ; annotation = None
        ; constructor_tree = None
        }
      ;;
    end

    type t =
      { hyp : Model.Transition.t
      ; goal : Opt.t
      }
  end *)

  module ApplicableConstructors = struct
    (** @param current
          is an option type so that we can make sure we do the necessary tactics the first time we enter this state.
        @param annotation
          is an option type since [None] represents the end of the annotation.
    *)
    type t =
      { current : Model.TreeNode.t list option
      ; annotation : Model.Annotation.t option
      ; label : Model.Label.t
      ; destination : Model.State.t
      }

    let init ({ goto; label; annotation; _ } : Model.Transition.t) : t =
      { current = None; annotation; label; destination = goto }
    ;;

    let to_string (x : t) : string =
      Utils.Strfy.record
        [ "label", M.Enc.to_string x.label.term
        ; "destination", Model.State.to_string x.destination
        ; ( "current"
          , Utils.Strfy.option
              (Of (Utils.Strfy.list (Of Model.TreeNode.to_string)))
              x.current )
        ; ( "annotation"
          , Utils.Strfy.option (Of Model.Annotation.to_string) x.annotation )
        ]
    ;;
  end

  let _log_applicable_constructors
        ?(__FUNCTION__ : string = "")
        (s : string)
        (x : ApplicableConstructors.t)
    : unit
    =
    Log.thing ~__FUNCTION__ Debug s x (Of ApplicableConstructors.to_string)
  ;;

  (** internal proof state *)
  module State = struct
    (** [t] represents the internal state-machine used to solve proofs.
        (Note: We are required to split some of the states over several steps since we need to update the proof iteratively in order to apply what we need to apply. E.g., it is easier to apply the constructors one after the other, since we require the proof to be updated by the previous appliction in order to apply the next.)
        - [NewProof] the start state. We unfold any terms that we can before proceeding to [WeakSim].
        - [WeakSim] either: (i) check if can be solved by cofix in hyps, or (ii) create new cofix. Either stays in [WeakSim] (to invert or unfold the hyps) or proceeds to [Exists], or [Done].
        - [Exists] means that the conclusion begins with an [exists a n2] (where [n2] is some state reached from [n] after taking action [a]). We: (1) extract the transition made by fsm "a" from the hyps (possible requiring inversion beforehand), (2) determine which transition fsm "b" will make in response to the one made by fsm "a" (in the hyps), and (3) apply [ex_intro] and [split] tactics to the conclusion (since we now know what to instantiate state [n2] with). If We may re-enter [Exists] if we make "b" do a reflexive transition, in which case we apply the necessary constructors to finish the case and proceed to [WeakSim], else we proceed to [ApplyConstructors].
        - [ApplyConstructors] is for applying the constructors we know we need to apply in order for fsm "b" to reach a state that is bisimilar to that reached by fsm "a".
        - [Done] means the proof is finished. *)
    type t =
      | NewProof of (Constrexpr.constr_expr * Constrexpr.constr_expr)
      | WeakSim
      | Exists of Model.Transition.t option
      (* | GoalTransition of Transition.t *)
      | ApplyConstructors of ApplicableConstructors.t
      | Done

    let to_string : t -> string = function
      | NewProof _ -> "NewProof"
      | WeakSim -> "WeakSim"
      | Exists hyp_opt ->
        Printf.sprintf
          "Exists; hyp_opt: %s"
          (Utils.Strfy.option (Of Model.Transition.to_string) hyp_opt)
      (* | GoalTransition _ -> "GoalTransition" *)
      | ApplyConstructors args ->
        Printf.sprintf
          "ApplyConstructors: %s"
          (ApplicableConstructors.to_string args)
      | Done -> "Done"
    ;;
  end

  (* TODO: rethink if we need to store the proofstate here *)
  type state =
    { p : Declare.Proof.t
    ; x : State.t
    }

  let the_state : state ref option ref = ref None

  exception NoStateFound

  let get_the_state () : state ref =
    match !the_state with None -> raise NoStateFound | Some x -> x
  ;;

  let get_pstate () : Declare.Proof.t = !(get_the_state ()).p
  let get_state () : State.t = !(get_the_state ()).x

  let log_proofstate () : unit =
    Log.thing Debug "proofstate =>" (get_state ()) (Of State.to_string)
  ;;

  (** [is_done ()] is true if the state [x] is [Done], else checks if [p] is done (via [Proof.is_done]).
  *)
  let is_done () : bool =
    match !(get_the_state ()) with
    | { p; x = Done } -> true
    | { p; x } -> Proof.is_done (Declare.Proof.get p)
  ;;

  let set_the_state (pstate : Declare.Proof.t) (x : State.t) : unit =
    the_state := Some (ref { p = pstate; x })
  ;;

  let update_pstate (pstate : Declare.Proof.t) : unit =
    the_state := Some (ref { !(get_the_state ()) with p = pstate })
  ;;

  let update_state (state : State.t) : unit =
    the_state := Some (ref { !(get_the_state ()) with x = state })
  ;;

  let init
        (pstate : Declare.Proof.t)
        (x : Constrexpr.constr_expr * Constrexpr.constr_expr)
    : unit
    =
    set_the_state pstate (NewProof x)
  ;;

  (* let get_gl () : Proofview.Goal.t = !(get_the_state ()).p *)

  (***********************************************************************)

  module Tactic = struct
    type t =
      { this : tactic
      ; next : t option
      }

    and tactic =
      { get : unit Proofview.tactic
      ; msg : (Output_kind.t * string) option
      }

    (** [tactic ?msg tactic] *)
    let tactic
          ?(level : Output_kind.t = Info)
          ?(msg : string option)
          (x : unit Proofview.tactic)
      : t
      =
      { this =
          { msg = Option.cata (fun m -> Some (level, m)) None msg; get = x }
      ; next = None
      }
    ;;

    let rec to_string : t -> string =
      let f : (Output_kind.t * string) option -> string = function
        | None -> ""
        (* | Some (Debug, y) -> y *)
        (* | Some (Info, y) -> y *)
        (* | Some (Notice, y) -> y *)
        (* | Some (Warning, y) -> y *)
        (* | Some (Error, y) -> y *)
        (* | Some (Trace, y) -> y *)
        (* | Some (Result, y) -> y *)
        (* | Some (Show, y) -> y *)
        | Some (x, y) -> y
      in
      let g : tactic -> string = function
        | { msg = None; _ } -> ""
        (* | { msg = Some (Debug, y); _ } -> "" *)
        (* | { msg = Some (Trace, y); _ } -> "" *)
        | { msg = Some (x, y); _ } -> Printf.sprintf "%s; " (f (Some (x, y)))
      in
      function
      | { this; next = None } -> Printf.sprintf "%s." (f this.msg)
      | { this; next = Some next } ->
        Printf.sprintf "%s%s" (g this) (to_string next)
    ;;

    let do_nothing () : t =
      tactic ~level:Debug ~msg:"(skip)" (Proofview.tclUNIT ())
    ;;

    (* let unpack (x : tactic) : unit Proofview.tactic =
       let () =
       match x.msg with
       | None -> ()
       | Some (Debug, z) -> Log.debug ~__FUNCTION__ z
       | Some (Info, z) -> Log.info ~__FUNCTION__ z
       | Some (Notice, z) -> Log.notice ~__FUNCTION__ z
       | Some (Warning, z) -> Log.warning ~__FUNCTION__ z
       | Some (Error, z) -> Log.error ~__FUNCTION__ z
       | Some (Trace, z) -> Log.trace ~__FUNCTION__ z
       | Some (Result, z) -> Log.result ~__FUNCTION__ z
       | Some (Show, z) -> Log.show ~__FUNCTION__ z
       in
       x.get
       ;; *)

    let unpack (x : t) : unit Proofview.tactic =
      let rec f : t -> unit Proofview.tactic = function
        | { this; next = None } -> this.get
        | { this; next = Some next } -> Proofview.tclTHEN this.get (f next)
      in
      let y = f x in
      Log.notice ~__FUNCTION__ (to_string x);
      y
    ;;

    (** [seq a b] appends [b] to the sequence of [a]. *)
    let rec seq : t -> t -> t = function
      | { this; next = None } -> fun (b : t) -> { this; next = Some b }
      | { this; next = Some next } ->
        fun (b : t) -> { this; next = Some (seq next b) }
    ;;

    let empty () : t = tactic (Proofview.tclUNIT ())

    exception EmptyTacticChain

    (** [chain ?nonempty (x::xs)] applies [seq x (chain xs)].
        @raise EmptyTacticChain if the list is empty and [?nonempty] s true. *)
    let rec chain ?(nonempty : bool = false) : t list -> t = function
      | [] -> if nonempty then raise EmptyTacticChain else empty ()
      | h :: [] -> h
      | h :: tl -> seq h (chain tl)
    ;;
  end

  (***********************************************************************)

  (** [module P] is a monad that contains the proof environment and context. *)
  module P (X : sig
      val gl : Proofview.Goal.t ref
    end) =
  struct
    let gl () : Proofview.Goal.t = !X.gl

    include
      Rocq_monad_utils.Make
        (Log)
        (Rocq_context.Make (struct
             let env : unit -> Environ.env ref =
               fun () -> ref (Proofview.Goal.env (gl ()))
             ;;

             let sigma : unit -> Evd.evar_map ref =
               fun () -> ref (Proofview.Goal.sigma (gl ()))
             ;;
           end))
        (W.M.Enc)

    (** [EConstrSet] is a custom [Set] of [EConstr.t] that allows terms to be compared more efficiently during {b a single proof step only} -- since this is built for each step. {e Though, since each proof step we have a new [env] and [sigma], the same term may be encoded differently across iteration steps, so there isn't necessarily a way for us to compare terms in a proof across iterations anyway. {b ! This needs to be investigated.}}
    *)
    module EConstrSet = struct
      include Set.Make (struct
          type t = EConstr.t

          let compare (a : t) (b : t) : int = econstr_compare a b
        end)

      (* let add (x : elt) (xs : t) : t = add (run (econstr_normalize x)) xs *)
    end

    (** [UnfoldCache] is a set containing all of the terms that we have detected can be unfolded.
        (* TODO: not implementing yet as I'm unsure if there will be issues with the env/sigma not being preserved across proof iterations. *)
    *)
    (* module UnfoldCache = struct
       let the_cache : EConstrSet.s
       end *)

    let to_atomic (x : EConstr.t) : EConstr.t Rocq_utils.kind_pair mm =
      let open Syntax in
      let* sigma = get_sigma in
      Rocq_utils.econstr_to_atomic sigma x |> return
    ;;

    let to_lambda (x : EConstr.t) : Rocq_utils.lambda_triple mm =
      let open Syntax in
      let* sigma = get_sigma in
      Rocq_utils.econstr_to_lambda sigma x |> return
    ;;

    let to_app (x : EConstr.t) : EConstr.t Rocq_utils.kind_pair mm =
      let open Syntax in
      let* sigma = get_sigma in
      Rocq_utils.econstr_to_app sigma x |> return
    ;;

    let get_concl () : EConstr.t = Proofview.Goal.concl (gl ())

    let hyp_compare_type (a : Rocq_utils.hyp) (b : Rocq_utils.hyp) : int =
      let a : EConstr.t = Context.Named.Declaration.get_type a in
      let b : EConstr.t = Context.Named.Declaration.get_type b in
      econstr_compare a b
    ;;

    let hyp_compare_name (a : Rocq_utils.hyp) (b : Rocq_utils.hyp) : int =
      let a : Names.Id.t = Context.Named.Declaration.get_id a in
      let b : Names.Id.t = Context.Named.Declaration.get_id b in
      Names.Id.compare a b
    ;;

    let get_hyps () : Rocq_utils.hyp list =
      (* List.sort_uniq hyp_compare_type *)
      Proofview.Goal.hyps (gl ())
    ;;

    let _log_econstr_eq
          ?(__FUNCTION__ : string = "")
          (x : EConstr.t)
          (y : EConstr.t)
      : unit
      =
      let eq : bool = econstr_eq x y in
      Log.thing ~__FUNCTION__ Debug "eq x y" eq (Of Utils.Strfy.bool)
    ;;

    let _log_constr_kind
          ?(__FUNCTION__ : string = "")
          (s : string)
          (x : Constr.t)
      : unit
      =
      Log.thing ~__FUNCTION__ Debug s x (Of Strfy.constr_kind)
    ;;

    let _log_econstr_kind
          ?(__FUNCTION__ : string = "")
          (s : string)
          (x : EConstr.t)
      : unit
      =
      Log.thing ~__FUNCTION__ Debug s x (Of Strfy.econstr_kind)
    ;;

    let log_econstr ?(__FUNCTION__ : string = "") (s : string) (x : EConstr.t)
      : unit
      =
      Log.thing ~__FUNCTION__ Debug s x (Of Strfy.econstr)
    ;;

    let log_econstrs
          ?(__FUNCTION__ : string = "")
          (s : string)
          (x : EConstr.t list)
      : unit
      =
      Log.things ~__FUNCTION__ Debug s x (Of Strfy.econstr)
    ;;

    let log_concl () : unit = log_econstr "concl" (get_concl ())

    let log_hyps () : unit =
      Log.things Debug "hyps" (get_hyps ()) (Of Strfy.hyp)
    ;;

    let get_hyp_name (x : Rocq_utils.hyp) : Names.Id.t =
      Context.Named.Declaration.get_id x
    ;;

    let get_hyp_names () : Names.Id.Set.t = Context.Named.to_vars (get_hyps ())

    let next_name_of (names : Names.Id.Set.t) (x : Names.Id.t) : Names.Id.t =
      Namegen.next_ident_away x names
    ;;

    let new_name_of_string (x : string) : Names.Id.t =
      next_name_of (get_hyp_names ()) (Names.Id.of_string x)
    ;;

    let new_cofix_name () : Names.Id.t = new_name_of_string "Cofix0"
    let new_H_name () : Names.Id.t = new_name_of_string "H0"

    let get_all_cofix_hyp_names () : Names.Id.Set.t =
      Names.Id.Set.filter
        (fun (x : Names.Id.t) ->
          Names.Id.equal (Nameops.root_of_id x) (Names.Id.of_string "Cofix"))
        (get_hyp_names ())
    ;;

    let get_all_non_cofix_hyp_names () : Names.Id.Set.t =
      Names.Id.Set.diff (get_hyp_names ()) (get_all_cofix_hyp_names ())
    ;;

    (***********************************************************************)

    module Theory = struct
      (** [is_any_theory x] is [true] if term [x] is equal to any of the terms presented in [Theories].
      *)
      let is_any_theory (x : EConstr.t) : bool =
        Theories.collect_bisimilarity_theories ()
        |> List.exists (fun (y : EConstr.t) -> econstr_eq x y)
      ;;

      (** [is_theory x y] checks if term [x] is equal to theory term [y], catching the exception thrown when [EConstr.kind_of_type x] is not [AtomicType (ty, tys)].
      *)
      let is_theory (x : EConstr.t) (y : EConstr.t) : bool mm =
        try
          let open Syntax in
          let* ty, tys = to_atomic x in
          econstr_eq y ty |> return
        with
        | Rocq_utils.Rocq_utils_EConstrIsNotA_Type _ -> return false
      ;;

      (** exists *)
      let is_exists (x : EConstr.t) : bool mm = is_theory x (Theories.c_ex ())

      (** weak simulation*)
      let is_weak_sim (x : EConstr.t) : bool mm =
        is_theory x (Theories.c_weak_sim ())
      ;;

      (** weak transition *)
      let is_weak (x : EConstr.t) : bool mm = is_theory x (Theories.c_weak ())

      let is_tau (x : EConstr.t) : bool mm = is_theory x (Theories.c_tau ())

      let is_silent (x : EConstr.t) : bool mm =
        is_theory x (Theories.c_silent ())
      ;;

      let is_silent1 (x : EConstr.t) : bool mm =
        is_theory x (Theories.c_silent1 ())
      ;;

      let is_LTS (x : EConstr.t) : bool mm = is_theory x (Theories.c_LTS ())
      let is_None (x : EConstr.t) : bool mm = is_theory x (Theories.c_None ())
      let is_Some (x : EConstr.t) : bool mm = is_theory x (Theories.c_Some ())

      exception EnsureFail

      (** assert *)
      let ensure (x : EConstr.t) (f : EConstr.t -> bool mm) : unit mm =
        let open Syntax in
        let* b = f x in
        if b then return () else raise EnsureFail
      ;;

      (** *)
      let get_theory_enc (f : EConstr.t -> bool mm) : M.Enc.t M.mm =
        let open M.Syntax in
        let* fm = M.get_fwdmap in
        let rec find_theory : (EConstr.t * M.Enc.t) list -> M.Enc.t M.mm =
          function
          | [] -> raise Not_found
          | (x, y) :: tl ->
            let is_match : bool = run (f x) in
            if is_match then M.return y else find_theory tl
        in
        M.F.to_seq fm |> List.of_seq |> find_theory
      ;;

      exception NoEncodingFoundFor_TheoriesNone

      let get_None_enc () : M.Enc.t M.mm =
        try get_theory_enc is_None with
        | Not_found -> raise NoEncodingFoundFor_TheoriesNone
      ;;

      exception NoEncodingFoundFor_TheoriesSome

      let get_Some_enc () : M.Enc.t M.mm =
        try get_theory_enc is_Some with
        | Not_found -> raise NoEncodingFoundFor_TheoriesSome
      ;;

      exception NotEqTheory

      (** *)
      let get_theory_enc_if_eq (x : EConstr.t) (f : EConstr.t -> bool mm)
        : M.Enc.t M.mm
        =
        let is_eq : bool = run (f x) in
        try if is_eq then get_theory_enc f else raise Not_found with
        | Not_found -> raise NotEqTheory
      ;;

      let get_None_enc_if_eq (x : EConstr.t) : M.Enc.t M.mm =
        get_theory_enc_if_eq x is_None
      ;;

      let get_Some_enc_if_eq (x : EConstr.t) : M.Enc.t M.mm =
        get_theory_enc_if_eq x is_Some
      ;;

      exception FSM_HasNoSilentLabel of Model.FSM.t

      let is_fsm_silent_label (x : EConstr.t) (m : Model.FSM.t) : bool =
        match
          Model.Labels.filter Model.Label.is_silent m.info.weak_labels
          |> Model.Labels.to_list
        with
        | [] -> raise (FSM_HasNoSilentLabel m)
        | ys -> M.exists_eq x ys Decode.label
      ;;

      exception FSM_HasNoVisibleLabel of Model.FSM.t

      (** i.e., not silent label *)
      let is_fsm_visible_label (x : EConstr.t) (m : Model.FSM.t) : bool =
        match
          Model.Labels.filter
            (fun y -> Model.Label.is_silent y |> Bool.not)
            m.info.weak_labels
          |> Model.Labels.to_list
        with
        | [] -> raise (FSM_HasNoVisibleLabel m)
        | ys -> M.exists_eq x ys Decode.label
      ;;

      exception FSM_HasNoWeakLabels of Model.FSM.t

      let is_fsm_weak_labels (x : EConstr.t) (m : Model.FSM.t) : bool =
        let is_silent = is_fsm_silent_label x m in
        if is_silent then true else is_fsm_visible_label x m
      ;;

      exception FSM_HasNoConstructors of Model.FSM.t

      let is_fsm_constructor (x : EConstr.t) (m : Model.FSM.t) : bool =
        match m with
        | { info = { meta = None; _ }; _ } -> raise (FSM_HasNoConstructors m)
        | { info = { meta = Some { lts; _ }; _ }; _ } ->
          if is_any_theory x
          then false
          else M.exists_eq x lts Decode.lts_constructor
      ;;
    end

    (** tactics *)
    module Tacs = struct
      let inversion (x : Rocq_utils.hyp) : Tactic.t mm =
        Inv.inv_tac (Context.Named.Declaration.get_id x)
        |> Tactic.tactic
             ~msg:
               (Printf.sprintf
                  "inversion %s"
                  (Strfy.hyp_name x) (* (Strfy.hyp_type x) *))
        |> return
      ;;

      let subst_all () : Tactic.t mm =
        Equality.subst_all ()
        |> Tactic.tactic ~msg:(Printf.sprintf "(subst all)")
        |> return
      ;;

      (** by specifying [None] it appears to be the same as using [simpl in *].
      *)
      let simplify () : Tactic.t mm =
        Tactics.simpl_option None |> Tactic.tactic ~msg:"simpl" |> return
      ;;

      let simplify_concl () : Tactic.t mm =
        Tactics.simpl_in_concl |> Tactic.tactic ~msg:"simpl" |> return
      ;;

      let simplify_hyp (x : Rocq_utils.hyp) : Tactic.t mm =
        Tactics.simpl_in_hyp (Context.Named.Declaration.get_id x, Locus.InHyp)
        |> Tactic.tactic ~msg:(Printf.sprintf "simpl in %s" (Strfy.hyp_name x))
        |> return
      ;;

      let simplify_hyps () : Tactic.t mm =
        match get_hyps () with
        | [] -> Tactic.empty () |> return
        | x :: [] -> simplify_hyp x
        | x :: xs ->
          let open Syntax in
          let* x : Tactic.t = simplify_hyp x in
          let f (i : int) (x : Tactic.t) : Tactic.t mm =
            let y : Rocq_utils.hyp = List.nth xs i in
            let* y : Tactic.t = simplify_hyp y in
            Tactic.seq x y |> return
          in
          iterate 0 (List.length xs - 1) x f
      ;;

      (** not needed, manual/explicit version of [simplfy ()] *)
      let simplify_all () : Tactic.t mm =
        let open Syntax in
        let* concl : Tactic.t = simplify_concl () in
        let* hyps : Tactic.t = simplify_hyps () in
        Tactic.seq concl hyps |> return
      ;;

      let simplify_and_subst_all () : Tactic.t mm =
        let open Syntax in
        (* let* simpls : Tactic.t = simplify_all () in *)
        let* simpls : Tactic.t = simplify () in
        let* substs : Tactic.t = subst_all () in
        Tactic.seq simpls substs |> return
      ;;

      let cofix () : Tactic.t mm =
        let name : Names.Id.t = new_cofix_name () in
        Tactics.cofix name
        |> Tactic.tactic
             ~msg:(Printf.sprintf "cofix %s" (Names.Id.to_string name))
        |> return
      ;;

      let trivial ?(msg : string = "trivial") () : Tactic.t mm =
        Tactic.tactic ~msg (Auto.gen_trivial ~debug:Hints.Info [] None)
        |> return
      ;;

      let ex_intro (x : Model.State.t) : Tactic.t mm =
        let t : EConstr.t = M.decode x.term in
        let bindings = Tactypes.ImplicitBindings [ t ] in
        let msg = Printf.sprintf "exists %s" (M.Strfy.econstr t) in
        Tactic.tactic ~msg (Tactics.constructor_tac true None 1 bindings)
        |> return
      ;;

      let split () : Tactic.t mm =
        Tactic.tactic (Tactics.split Tactypes.NoBindings) |> return
      ;;

      let ex_intro_split (x : Model.State.t) : Tactic.t mm =
        let open Syntax in
        let* ex_intro : Tactic.t = ex_intro x in
        let* split : Tactic.t = split () in
        Tactic.seq ex_intro split |> return
      ;;

      let intros_all () : Tactic.t mm =
        Tactics.intros |> Tactic.tactic ~msg:"intros" |> return
      ;;

      (** [intro_as x] applies the introduction tactic using the (next non-conficting) name [x].
      *)
      let intro_as (x : string) : Tactic.t mm =
        let name : Names.Id.t = new_name_of_string x in
        Tactics.introduction name
        |> Tactic.tactic
             ~msg:(Printf.sprintf "intro %s" (Names.Id.to_string name))
        |> return
      ;;

      (* *)
      let apply (x : EConstr.t) : Tactic.t mm =
        Tactics.apply x
        |> Tactic.tactic ~msg:(Printf.sprintf "apply %s" (Strfy.econstr x))
        |> return
      ;;

      let apply_Pack_sim () : Tactic.t mm = apply (Theories.c_Pack_sim ())
      let apply_In_sim () : Tactic.t mm = apply (Theories.c_In_sim ())
      let apply_wk_none () : Tactic.t mm = apply (Theories.c_wk_none ())
      let apply_rt1n_refl () : Tactic.t mm = apply (Theories.c_rt1n_refl ())
      let apply_rt1n_trans () : Tactic.t mm = apply (Theories.c_rt1n_trans ())

      let apply_weak_sim_refl () : Tactic.t mm =
        apply (Theories.c_weak_sim_refl ())
      ;;

      (* let apply_wk_bisim_refl () : Tactic.t mm = apply (Theories.c_wk_bisim_refl ()) *)

      let eapply (x : EConstr.t) : Tactic.t mm =
        Tactics.eapply x
        |> Tactic.tactic ~msg:(Printf.sprintf "eapply %s" (Strfy.econstr x))
        |> return
      ;;

      let eapply_wk_some () : Tactic.t mm = eapply (Theories.c_wk_some ())
      let eapply_rt1n_refl () : Tactic.t mm = eapply (Theories.c_rt1n_refl ())
      let eapply_rt1n_trans () : Tactic.t mm = eapply (Theories.c_rt1n_trans ())

      (** {b counter intuitively, this applies a transition if the label is silent.} This is because we use this to determine if we need to unfold the [weak] transition from the [Bisimilarity.v] theory.
      *)
      let eapply_rt1n_via (x : Model.Label.t) : Tactic.t mm =
        if Model.Label.is_silent x
        then eapply_rt1n_trans ()
        else eapply_rt1n_refl ()
      ;;

      (* *)
      exception CannotUnfoldConstr of Constr.t

      (** [unfold_constr ?in_hyp x] ... {e NOTE: term [x] is always unfolded. If [?in_hyp] is provided then we {b also} unfold [x] [in_hyp].}
          @raise CannotUnfoldConstr
            of [x] if [Constr.kind x] is not [Const (_, _)]. *)
      let unfold_constr ?(in_hyp : Rocq_utils.hyp option) (x : Constr.t)
        : Tactic.t mm
        =
        Log.trace __FUNCTION__;
        match Constr.kind x with
        | Const (name, _) ->
          let f (name : Names.Constant.t) : unit Proofview.tactic =
            match in_hyp with
            | None -> Tactics.unfold_constr (Names.GlobRef.ConstRef name)
            | Some y ->
              Proofview.tclTHEN
                (Tactics.unfold_in_hyp
                   [ Locus.AllOccurrences, Evaluable.EvalConstRef name ]
                   (Context.Named.Declaration.get_id y, Locus.InHyp))
                (Tactics.unfold_constr (Names.GlobRef.ConstRef name))
          in
          f name
          |> Tactic.tactic
               ~msg:(Printf.sprintf "unfold %s" (Names.Constant.to_string name))
          |> return
        | _ -> raise (CannotUnfoldConstr x)
      ;;

      (** [handle_unfold_hyp_opt f ?in_hyp x] helps keep this function cleaner to use. i.e., [unfold_econstr ~in_hyp:x] rather than [~in_hyp:(Some x)].
      *)
      let f_unfold_hyp
            (f : ?in_hyp:Rocq_utils.hyp -> 'a -> Tactic.t mm)
            ?(in_hyp : Rocq_utils.hyp option = None)
            (x : 'a)
        : Tactic.t mm
        =
        Log.trace __FUNCTION__;
        match in_hyp with None -> f x | Some in_hyp -> f ~in_hyp x
      ;;

      let unfold_econstr ?(in_hyp : Rocq_utils.hyp option) (x : EConstr.t)
        : Tactic.t mm
        =
        Log.trace __FUNCTION__;
        let open Syntax in
        let* y : Constr.t = econstr_to_constr x in
        f_unfold_hyp unfold_constr ~in_hyp y
      ;;

      let unfold_constrexpr
            ?(in_hyp : Rocq_utils.hyp option)
            (x : Constrexpr.constr_expr)
        : Tactic.t mm
        =
        Log.trace __FUNCTION__;
        let open Syntax in
        let* y : EConstr.t = constrexpr_to_econstr x in
        f_unfold_hyp unfold_econstr ~in_hyp y
      ;;

      let unfold_opt_constrexpr_list ?(in_hyp : Rocq_utils.hyp option)
        : Constrexpr.constr_expr list -> Tactic.t option mm
        =
        Log.trace __FUNCTION__;
        (* NOTE: optional argument wrapper *)
        let unfold_constrexpr (x : Constrexpr.constr_expr) : Tactic.t mm =
          match in_hyp with
          | None -> unfold_constrexpr x
          | Some in_hyp -> unfold_constrexpr ~in_hyp x
        in
        (* NOTE: iterate and combine tactics *)
        let open Syntax in
        let iterate h xs =
          let f (i : int) (ys : Tactic.t) =
            let* x = List.nth xs i |> unfold_constrexpr in
            Tactic.seq h x |> return
          in
          iterate 0 (List.length xs - 1) h f
        in
        function
        | [] -> return None
        | h :: tl ->
          let* h = unfold_constrexpr h in
          let* x = iterate h tl in
          return (Some x)
      ;;

      let unfold_silent () : Tactic.t mm = unfold_econstr (Theories.c_silent ())

      let unfold_silent1 () : Tactic.t mm =
        unfold_econstr (Theories.c_silent1 ())
      ;;

      (* *)
      let do_refl () : Tactic.t mm =
        let open Syntax in
        let* wk_none = apply_wk_none () in
        let* unfold_silent = unfold_silent () in
        let* rt1n_refl = apply_rt1n_refl () in
        Tactic.chain [ wk_none; unfold_silent; rt1n_refl ] |> return
      ;;

      (* *)
      let collect_component_econstrs (sigma : Evd.evar_map) (x : EConstr.t)
        : EConstrSet.t
        =
        (* Log.trace __FUNCTION__; *)
        (* log_econstr ~__FUNCTION__ "x" x; *)
        let is_constr_ref (x : EConstr.t) : bool =
          EConstr.isRef sigma x && EConstr.isConst sigma x
        in
        let acc_constr_ref (x : EConstr.t) (acc : EConstrSet.t) : EConstrSet.t =
          if is_constr_ref x then EConstrSet.add x acc else acc
        in
        let rec f (acc : EConstrSet.t) (y : EConstr.t) : EConstrSet.t =
          (* log_econstr ~__FUNCTION__ "y" y; *)
          let acc : EConstrSet.t = acc_constr_ref y acc in
          try
            let ty, tys = Rocq_utils.econstr_to_atomic sigma y in
            (* log_econstr ~__FUNCTION__ "ty" ty; *)
            (* _log_econstr_kind ~__FUNCTION__ "ty" ty; *)
            let acc : EConstrSet.t =
              match EConstr.kind sigma ty with
              | Case (_, _, _, _, _, c, _) ->
                (* _log_econstr_kind ~__FUNCTION__ "c" c; *)
                (match EConstr.kind sigma c with
                 | App (ty, _) -> acc_constr_ref ty acc
                 | _ -> acc)
              | _ -> acc
            in
            (* log_econstrs ~__FUNCTION__ "tys" (Array.to_list tys); *)
            let acc : EConstrSet.t = acc_constr_ref ty acc in
            Array.fold_left
              (fun (acc : EConstrSet.t) (z : EConstr.t) -> f acc z)
              acc
              tys
          with
          | Rocq_utils.Rocq_utils_EConstrIsNotA_Type _ ->
            (* log_econstr ~__FUNCTION__ "Err: Rocq_utils_EConstrIsNotA_Type" x; *)
            acc
        in
        f EConstrSet.empty x
      ;;

      (** [can_be_unfolded sigma x] returns [true] if [x] can be {e unfolded}, i.e., refers to a definition, e.g., of a definition, fixpoint or example.
      *)
      let can_be_unfolded (sigma : Evd.evar_map) (x : EConstr.t) : bool =
        (* Log.trace __FUNCTION__; *)
        try
          let g, i = EConstr.destRef sigma x in
          match g with
          | ConstRef y ->
            (match Global.lookup_constant y with
             | { const_body = Def z; const_type; _ } ->
               (* log_econstr ~__FUNCTION__ "x" x; *)
               (* _log_constr_kind ~__FUNCTION__ "(z kinds, z)" z; *)
               (* _log_constr_kind ~__FUNCTION__ "(kinds, const_type)" const_type; *)
               (match Constr.kind z with
                | Fix _ ->
                  (* log_econstr ~__FUNCTION__ "is Fix" x; *)
                  Constr.isProd const_type
                | Lambda _ ->
                  (* log_econstr ~__FUNCTION__ "is Lambda" x; *)
                  Constr.isProd const_type
                | App _ ->
                  (* log_econstr ~__FUNCTION__ "is App" x; *)
                  Constr.isRef const_type
                  && (Constr.isConst const_type || Constr.isInd const_type)
                | Construct _ ->
                  (* log_econstr ~__FUNCTION__ "is Construct" x; *)
                  Constr.isRef z
                  && Constr.isConst const_type
                  && Constr.isRef const_type
                | _ ->
                  (* NOTE: unfold, e.g., [SomeModule.example_1] *)
                  (* log_econstr ~__FUNCTION__ "(skip)" x; *)
                  Constr.isConst z
                  && Constr.isRef z
                  && Constr.isConst const_type
                  && Constr.isRef const_type)
             | _ -> false)
          | _ -> false
        with
        | Constr.DestKO ->
          log_econstr ~__FUNCTION__ "Err: Constr.DestKO, x" x;
          false
      ;;

      (** [try_unfold_any x]
          (* TODO: make sure you remove duplicate things to unfold. *) *)
      let try_unfold_any ?(in_hyp : Rocq_utils.hyp option) (x : EConstr.t)
        : Tactic.t option mm
        =
        (* Log.trace __FUNCTION__; *)
        (* log_econstr ~__FUNCTION__ "x" x; *)
        let open Syntax in
        let* sigma = get_sigma in
        (* NOTE: [collect_component_econstrs] ensures no duplicates. *)
        match collect_component_econstrs sigma x |> EConstrSet.to_list with
        | [] -> return None
        | to_check ->
          (* log_econstrs ~__FUNCTION__ "to_check" to_check; *)
          let f (i : int) (acc : Tactic.t list) =
            let x : EConstr.t = List.nth to_check i in
            if Theory.is_any_theory x
            then
              (* log_econstr ~__FUNCTION__ "skip unfold theory" x; *)
              return acc
            else if can_be_unfolded sigma x
            then
              (* log_econstr ~__FUNCTION__ "can be unfolded" x; *)
              let* y : Tactic.t = f_unfold_hyp unfold_econstr ~in_hyp x in
              return (y :: acc)
            else
              (* log_econstr ~__FUNCTION__ "not unfoldable" x; *)
              return acc
          in
          let* ys = iterate 0 (List.length to_check - 1) [] f in
          (match ys with
           | [] -> return None
           | ys -> Some (Tactic.chain ys) |> return)
      ;;

      let rec try_unfold_any_of : EConstr.t list -> Tactic.t option mm =
        (* Log.trace __FUNCTION__; *)
        function
        | [] -> return None
        | h :: tl ->
          let open Syntax in
          let* h_opt = try_unfold_any h in
          let* tl_opt = try_unfold_any_of tl in
          (match h_opt, tl_opt with
           | Some x, Some y -> Some (Tactic.seq x y) |> return
           | None, Some y -> return (Some y)
           | Some x, None -> return (Some x)
           | None, None -> return None)
      ;;

      (***********************************************************************)

      exception NoConstructorFoundWithEnc of M.Enc.t

      let find_lts (lts_enc : M.Enc.t) : Model.Info.lts list -> Model.Info.lts =
        List.find (fun ({ enc; _ } : Model.Info.lts) -> M.Enc.equal enc lts_enc)
      ;;

      let find_constructor (constructor_index : int)
        : Rocq_bindings.constructor list -> Rocq_bindings.constructor
        =
        List.find (fun ({ index; _ } : Rocq_bindings.constructor) ->
          Int.equal index constructor_index)
      ;;

      type binding_args =
        { from : EConstr.t
        ; goto : EConstr.t option
        ; label : EConstr.t option
        }

      let get_constructor_bindings
            ({ from; goto; label } : binding_args)
            (bindings : Rocq_bindings.t)
        : EConstr.t Tactypes.bindings mm
        =
        let open Syntax in
        let* env = get_env in
        let* sigma = get_sigma in
        Rocq_bindings.get env sigma from label goto bindings |> return
      ;;

      (** if we have no way of obtaining the bindings (i.e., not info.meta) then we use no bindings.
          (* TODO: check if we can optimize this so we use [NoBindings] where possible *)
      *)
      let try_get_constructor_bindings
            ((enc, index) : Model.TreeNode.t)
            (args : binding_args)
        : EConstr.t Tactypes.bindings mm
        =
        match (W.get_fsm_b ()).info.meta with
        | None -> return Tactypes.NoBindings
        | Some { lts; _ } ->
          let { constructors; _ } : Model.Info.lts = find_lts enc lts in
          let { bindings; _ } : Rocq_bindings.constructor =
            find_constructor index constructors
          in
          get_constructor_bindings args bindings
      ;;

      let apply_constructor
            ((enc, index) : Model.TreeNode.t)
            (args : binding_args)
        : Tactic.t mm
        =
        (* NOTE: constructors index from 1 *)
        let index : int = index + 1 in
        let msg : string = Printf.sprintf "constructor %i" index in
        let open Syntax in
        let* bindings = try_get_constructor_bindings (enc, index) args in
        Log.thing
          ~__FUNCTION__
          Debug
          "bindings"
          bindings
          (Of Strfy.econstr_bindings);
        Tactic.tactic ~msg (Tactics.one_constructor index bindings) |> return
      ;;
    end

    (** remodel *)
    module ReModel = struct
      exception
        CouldNotFind_State of
          { x : EConstr.t
          ; states : Model.States.t
          }

      let state (x : EConstr.t) (ys : Model.States.t) : Model.State.t M.mm =
        Log.trace __FUNCTION__;
        try
          let term : M.Enc.t = M.get_encoding x in
          (* NOTE: [Model.States.compare] only cares about [term]. *)
          Model.States.find { term; pp = None } ys |> M.return
        with
        | M.EncodingNotFound _ ->
          log_econstr ~__FUNCTION__ "Err: M.EncodingNotFound" x;
          raise (CouldNotFind_State { x; states = ys })
        | Not_found -> raise (CouldNotFind_State { x; states = ys })
      ;;

      let state_opt (x : EConstr.t) (ys : Model.States.t)
        : Model.State.t option M.mm
        =
        Log.trace __FUNCTION__;
        try
          let open M.Syntax in
          let* z = state x ys in
          M.return (Some z)
        with
        | CouldNotFind_State _ -> M.return None
      ;;

      exception
        CouldNotFind_Label of
          { x : EConstr.t
          ; alphabet : Model.Labels.t
          }

      let label (x : EConstr.t) (ys : Model.Labels.t) : Model.Label.t M.mm =
        Log.trace __FUNCTION__;
        let f (term : M.Enc.t) : Model.Label.t M.mm =
          (* NOTE: [Model.Labels.compare] only cares about [is_silent=Some _] *)
          Model.Labels.find { term; is_silent = None; pp = None } ys |> M.return
        in
        try M.get_encoding x |> f with
        | M.EncodingNotFound _ ->
          log_econstr ~__FUNCTION__ "Err: M.EncodingNotFound" x;
          raise (CouldNotFind_Label { x; alphabet = ys })
        | Not_found ->
          let open M.Syntax in
          (* NOTE: is it [None]? (i.e., a silent action) *)
          (try
             let* term : M.Enc.t = Theory.get_None_enc_if_eq x in
             f term
           with
           | Theory.NotEqTheory ->
             (* NOTE: is it [Some]? (i.e., a visible action) *)
             (try
                let* term : M.Enc.t = Theory.get_Some_enc_if_eq x in
                f term
              with
              | Theory.NotEqTheory ->
                raise (CouldNotFind_Label { x; alphabet = ys })))
      ;;

      let label_opt (x : EConstr.t) (ys : Model.Labels.t)
        : Model.Label.t option M.mm
        =
        Log.trace __FUNCTION__;
        try
          let open M.Syntax in
          let* z = label x ys in
          M.return (Some z)
        with
        | CouldNotFind_Label _ -> M.return None
      ;;

      exception
        CouldNotFind_Transition of
          { from : Model.State.t
          ; goto : Model.State.t
          ; label : Model.Label.t
          ; edges : Model.EdgeMap.t'
          }

      let transition
            (from : Model.State.t)
            (goto : Model.State.t)
            (label : Model.Label.t)
            (edges : Model.EdgeMap.t')
        : Model.Transition.t
        =
        Log.trace __FUNCTION__;
        (* TODO: export some of this to the [Model.ActionMap] ? *)
        let actions = Model.EdgeMap.find edges from in
        let labelled = Model.ActionMap.reduce_by_label actions label in
        if Model.ActionMap.length labelled |> Int.equal 0
        then raise (CouldNotFind_Transition { from; goto; label; edges })
        else (
          let actionpairs =
            Model.ActionMap.to_seq labelled
            |> List.of_seq
            |> List.filter
                 (fun
                     ((action, destinations) : Model.Action.t * Model.States.t)
                    -> Model.States.mem goto destinations)
          in
          match actionpairs with
          | [] -> raise (CouldNotFind_Transition { from; goto; label; edges })
          | ({ annotation; constructor_trees; _ }, _) :: [] ->
            let constructor_tree = Model.Trees.min_opt constructor_trees in
            { from; goto; label; annotation; constructor_tree }
          | h :: tl ->
            (* TODO: move this proceed to [Model] and handle this case *)
            Log.warning ~__FUNCTION__ "Multiple actionpairs found";
            raise (CouldNotFind_Transition { from; goto; label; edges }))
      ;;
    end

    (***********************************************************************)

    module Hyp = struct
      type t = Rocq_utils.hyp

      let to_string (x : t) : string = Strfy.hyp x
      let name_to_string (x : t) : string = Strfy.hyp_name x

      let to_atomic (x : t) : EConstr.t Rocq_utils.kind_pair mm =
        let open Syntax in
        let* sigma = get_sigma in
        Rocq_utils.hyp_to_atomic sigma x |> return
      ;;

      (** [invertibility x] returns an integer denoting whether [x] need be inverted, with the higher numbers being of more importance to invert and [0] denoting [x] does not need to be inverted.
      *)
      let invertibility (x : t) : int mm =
        Log.trace __FUNCTION__;
        let open Syntax in
        let* _, tys = to_atomic x in
        let* sigma = get_sigma in
        (* NOTE: returns true if can be inverted *)
        let rec f (x : EConstr.t) : bool =
          match EConstr.kind sigma x with
          | Var _ -> EConstr.isRef sigma x
          | App (_, tys) -> Array.exists f tys
          | _ -> false
        in
        (* NOTE: since [2] is the goto-state and [1] is the label, [g] allows us to clearly see which hyp needs to be inverted first. *)
        let g (i : int) : int =
          try if f tys.(i) then i else 0 with
          (* NOTE: handles "Index out of bounds" for accessing [tys] array. *)
          | Invalid_argument _ -> 0
        in
        g 2 + g 1 |> return
      ;;

      let need_inversion (x : t) : bool mm =
        Log.trace __FUNCTION__;
        let open Syntax in
        let* n : int = invertibility x in
        if Int.equal n 0 then return false else return true
      ;;

      let invert (x : t) : Tactic.t mm = Tacs.inversion x

      (** [try_unfold_any x] obtains the [Atomic (ty, tys)] of the given hyp [x], and first checks to see if there is anything that can be unfolded in [ty] (via [Tacs.try_unfold_any ~in_hyp:x]) then returns it. Else, if [ty] cannot be unfolded, then we check each of [tys] and return any.
      *)
      let try_unfold_any (x : t) : Tactic.t option mm =
        let open Syntax in
        let* sigma = get_sigma in
        try
          let ty, tys = Rocq_utils.hyp_to_atomic sigma x in
          let* ty_opt : Tactic.t option = Tacs.try_unfold_any ~in_hyp:x ty in
          match ty_opt with
          | Some y -> return (Some y)
          | None ->
            (* NOTE: check if any in [tys] can be unfolded *)
            let f (i : int) (acc : Tactic.t option) : Tactic.t option mm =
              let y = tys.(i) in
              let* y : Tactic.t option = Tacs.try_unfold_any ~in_hyp:x y in
              match y with
              | None -> return acc
              | Some y ->
                (match acc with
                 | None -> return (Some y)
                 | Some acc -> return (Some (Tactic.seq acc y)))
            in
            iterate 0 (Array.length tys - 1) None f
        with
        | Rocq_utils.Rocq_utils_HypIsNot_Atomic _ -> return None
      ;;

      exception
        CouldNotGetTransition of
          { hyp : t
          ; fsm : Model.FSM.t
          }

      (** *)
      let get_transition (x : t) (m : Model.FSM.t) : Model.Transition.t mm =
        Log.trace __FUNCTION__;
        let open Syntax in
        let* ty, tys = to_atomic x in
        if Theory.is_fsm_constructor ty m
        then (
          try
            let from : Model.State.t = M.run (ReModel.state tys.(0) m.states) in
            _log_state ~__FUNCTION__ "from" from;
            log_econstr ~__FUNCTION__ "from" (M.decode from.term);
            let goto : Model.State.t = M.run (ReModel.state tys.(2) m.states) in
            _log_state ~__FUNCTION__ "goto" goto;
            log_econstr ~__FUNCTION__ "goto" (M.decode goto.term);
            let label : Model.Label.t =
              M.run (ReModel.label tys.(1) m.alphabet)
            in
            _log_label ~__FUNCTION__ "label" label;
            log_econstr ~__FUNCTION__ "label" (M.decode label.term);
            ReModel.transition from goto label m.edges |> return
          with
          (* | M.EncodingNotFound z ->
            log_econstr ~__FUNCTION__ "Err: M.EncodingNotFound" z;
            raise (CouldNotGetTransition { hyp = x; fsm = m }) *)
          | ReModel.CouldNotFind_State _ ->
            Log.trace ~__FUNCTION__ "Err: ReModel.CouldNotFind_State";
            raise (CouldNotGetTransition { hyp = x; fsm = m })
          | ReModel.CouldNotFind_Label _ ->
            Log.trace ~__FUNCTION__ "Err: ReModel.CouldNotFind_Label";
            raise (CouldNotGetTransition { hyp = x; fsm = m })
          | ReModel.CouldNotFind_Transition _ ->
            Log.trace ~__FUNCTION__ "Err: ReModel.CouldNotFind_Transition";
            raise (CouldNotGetTransition { hyp = x; fsm = m }))
        else (
          Log.trace ~__FUNCTION__ "(else)";
          raise (CouldNotGetTransition { hyp = x; fsm = m }))
      ;;
    end

    (** conclusion *)
    module Concl = struct
      let eq (x : EConstr.t) : bool = get_concl () |> econstr_eq x

      let eq_hyp (x : Rocq_utils.hyp) : bool =
        Context.Named.Declaration.get_type x |> eq
      ;;

      let rec eq_any_hyps : Rocq_utils.hyp list -> bool mm = function
        | [] -> return false
        | h :: tl -> if eq_hyp h then return true else eq_any_hyps tl
      ;;

      let is_weak_refl () : bool mm =
        let open Syntax in
        let* ty, tys = get_concl () |> to_atomic in
        if econstr_eq tys.(3) tys.(4)
        then econstr_eq tys.(5) tys.(6) |> return
        else return false
      ;;

      let is_weak_sim () : bool mm = get_concl () |> Theory.is_weak_sim
      let is_exists () : bool mm = get_concl () |> Theory.is_exists
      let is_tau () : bool mm = get_concl () |> Theory.is_tau

      (** [try_unfold_any ()] is similar to [Hyp.try_unfold_any _], except that instead of a hypothesis, it uses the conclusion. Uses [Tacs.try_unfold_any].
      *)
      let try_unfold_any () : Tactic.t option mm =
        Log.trace __FUNCTION__;
        let open Syntax in
        let* ty, tys = get_concl () |> to_atomic in
        let* ty_opt : Tactic.t option = Tacs.try_unfold_any ty in
        match ty_opt with
        | Some y -> return (Some y)
        | None ->
          (* NOTE: check if any in [tys] can be unfolded *)
          let f (i : int) (acc : Tactic.t option) : Tactic.t option mm =
            let y = tys.(i) in
            let* y : Tactic.t option = Tacs.try_unfold_any y in
            match y with
            | None -> return acc
            | Some y ->
              (match acc with
               | None -> return (Some y)
               | Some acc -> return (Some (Tactic.seq acc y)))
          in
          iterate 0 (Array.length tys - 1) None f
      ;;

      type wk_conj =
        { wk_trans : EConstr.t
        ; wk_sim : EConstr.t
        }

      type conj =
        { a' : Model.State.t
        ; b : Model.State.t
        }

      let get_a'_from_wk_sim (wk_sim : EConstr.t) : Model.State.t mm =
        let open Syntax in
        let* _, tys = to_atomic wk_sim in
        (W.get_fsm_a ()).states |> ReModel.state tys.(5) |> M.run |> return
      ;;

      let get_b_from_wk_trans (wk_trans : EConstr.t) : Model.State.t mm =
        let open Syntax in
        let* _, tys = to_atomic wk_trans in
        (W.get_fsm_b ()).states |> ReModel.state tys.(3) |> M.run |> return
      ;;

      exception ConclDoesNotMatchConj

      let get_wk_conj () : wk_conj mm =
        Log.trace __FUNCTION__;
        let open Syntax in
        let* ty, tys = get_concl () |> to_atomic in
        let* () = Theory.ensure ty Theory.is_exists in
        let* _, _, x = to_lambda tys.(1) in
        let* _, app_tys = to_app x in
        match Array.to_list app_tys with
        | [ wk_trans; wk_sim ] -> return { wk_trans; wk_sim }
        | _ -> raise ConclDoesNotMatchConj
      ;;

      let get_conj ({ wk_trans; wk_sim } : wk_conj) : conj mm =
        Log.trace __FUNCTION__;
        let open Syntax in
        let* () = Theory.ensure wk_sim Theory.is_weak_sim in
        let* a' = get_a'_from_wk_sim wk_sim in
        let* b = get_b_from_wk_trans wk_trans in
        return { a'; b }
      ;;
    end

    (** hypothesis *)
    module Hyps = struct
      (** [get_cofixes ()] filters the hyps by name according to [get_all_cofix_hyp_names ()].
      *)
      let get_cofixes () : Rocq_utils.hyp list =
        let cofix_names : Names.Id.Set.t = get_all_cofix_hyp_names () in
        get_hyps ()
        |> List.filter (fun (x : Rocq_utils.hyp) ->
          Names.Id.Set.mem (get_hyp_name x) cofix_names)
        |> List.sort hyp_compare_name
      ;;

      (** [get_non_cofixes ()] filters the hyps by name according to [get_all_non_cofix_hyp_names ()].
      *)
      let get_non_cofixes () : Rocq_utils.hyp list =
        let cofix_names : Names.Id.Set.t = get_all_non_cofix_hyp_names () in
        get_hyps ()
        |> List.filter (fun (x : Rocq_utils.hyp) ->
          Names.Id.Set.mem (get_hyp_name x) cofix_names)
        |> List.sort hyp_compare_name
      ;;

      let log ?(cofix_only : bool option = None) () : unit =
        match cofix_only with
        | None -> log_hyps ()
        | Some true ->
          Log.things Debug "hyps (cofixes)" (get_cofixes ()) (Of Strfy.hyp)
        | Some false ->
          Log.things
            Debug
            "hyps (non-cofixes)"
            (get_non_cofixes ())
            (Of Strfy.hyp)
      ;;

      (** [can_solve_concl_cofix ()] returns true if there is a hyp that can solve the current a tactic to solve the current goal using one of he cofixes in the hyps.
      *)
      let can_solve_concl_cofix () : bool mm =
        get_cofixes () |> Concl.eq_any_hyps
      ;;

      (** [clear_non_cofix ()] returns a tactic that will clear all the hyps that are named according to [get_all_non_cofix_hyp_names ()]. This is to be used at the end of a case of the proof has been solved.
          (* TODO: check if this is necessary -- or could be problematic? *) *)
      let clear_non_cofix () : Tactic.t =
        Tactic.tactic
          ~msg:"(Clearing non-cofix Hyps)"
          (Tactics.clear
             (Names.Id.Set.to_seq (get_all_non_cofix_hyp_names ())
              |> List.of_seq))
      ;;

      (** [try_invert_any inverted_hyps] returns either [None] if no hyps can be inverted (as determined by [Hyp.invertibility]), else a [Tactic.t] that will invert the hypothesis deemed to be the most important to invert. (Only checks non-cofix hyps as by [get_non_cofixes ()].)
      *)
      let try_invert_any () : Tactic.t option mm =
        Log.trace __FUNCTION__;
        (* log_econstrs ~__FUNCTION__ "inverted hyps" !inverted_hyps; *)
        let hyps : Hyp.t list = get_non_cofixes () in
        let open Syntax in
        let f (i : int) (xopt : (int * Hyp.t) option) : (int * Hyp.t) option mm =
          let y : Hyp.t = List.nth hyps i in
          let* grade : int = Hyp.invertibility y in
          Log.debug
            ~__FUNCTION__
            (Printf.sprintf "grade %i : %s" grade (Hyp.name_to_string y));
          match xopt with
          | None -> Some (grade, y) |> return
          | Some (n, x) ->
            (match Int.compare grade n with
             | -1 -> Some (n, x) |> return
             | _ -> Some (grade, y) |> return)
        in
        let* to_invert_opt = iterate 0 (List.length hyps - 1) None f in
        match to_invert_opt with
        | None -> return None
        | Some (0, x) -> return None
        (* NOTE: we only want to invert hyps with non-zero grades. *)
        | Some (grade, x) ->
          let* y = Hyp.invert x in
          return (Some y)
      ;;

      (** [try_unfold_any ()] returns the optional [Tactic.t] that is a sequence derived from all of the hyps ([xs]) from [Hyp.try_unfold_any x] (where [x] is a hyp in [xs]).
      *)
      let try_unfold_any () : Tactic.t option mm =
        let hyps = get_non_cofixes () in
        let open Syntax in
        let f (i : int) (acc : Tactic.t option) : Tactic.t option mm =
          let x = List.nth hyps i in
          let* x = Hyp.try_unfold_any x in
          match x with
          | None -> return acc
          | Some x ->
            (match acc with
             | None -> return (Some x)
             | Some acc -> return (Some (Tactic.seq acc x)))
        in
        iterate 0 (List.length hyps - 1) None f
      ;;

      exception CannotGetTransition of Model.FSM.t

      let get_transition (m : Model.FSM.t) : Model.Transition.t mm =
        Log.trace __FUNCTION__;
        let hyps = get_non_cofixes () in
        let open Syntax in
        let f (i : int)
          : Model.Transition.t option -> Model.Transition.t option mm
          =
          (* Log.thing ~__FUNCTION__ Trace "i" i (Of Utils.Strfy.int); *)
          function
          | Some x -> return (Some x)
          | None ->
            let y = List.nth hyps i in
            (try
               let y : Model.Transition.t = Hyp.get_transition y m |> run in
               return (Some y)
             with
             | Hyp.CouldNotGetTransition _ -> return None)
        in
        let* x = iterate 0 (List.length hyps - 1) None f in
        match x with
        | None -> raise (CannotGetTransition m)
        | Some x -> return x
      ;;
    end

    (***********************************************************************)

    (** [handle_new_cofix ()] returns a sequence of tactics to handle the creation of a new cofix in the hyps, followed by the necessary application of constructors and introduction of terms to get started on a new case.
    *)
    let handle_new_cofix () : Tactic.t mm =
      Log.trace __FUNCTION__;
      let open Syntax in
      let* unfold_opt = Concl.try_unfold_any () in
      match unfold_opt with
      | Some x ->
        Log.trace ~__FUNCTION__ "do unfold";
        return x
      | None ->
        Log.trace ~__FUNCTION__ "nothing to unfold";
        let* cofix : Tactic.t = Tacs.cofix () in
        Log.trace ~__FUNCTION__ "A";
        let clear : Tactic.t = Hyps.clear_non_cofix () in
        Log.trace ~__FUNCTION__ "B";
        let* apply_In_sim : Tactic.t = Tacs.apply_In_sim () in
        Log.trace ~__FUNCTION__ "C";
        let* apply_Pack_sim : Tactic.t = Tacs.apply_Pack_sim () in
        Log.trace ~__FUNCTION__ "D";
        let* intros_all : Tactic.t = Tacs.intros_all () in
        Log.trace ~__FUNCTION__ "E";
        Tactic.chain [ cofix; clear; apply_In_sim; apply_Pack_sim; intros_all ]
        |> return
    ;;

    exception CouldNotFindGotoState

    let try_get_visible_transition
          ?(saturated : bool = false)
          (bisimilar : Model.States.t)
          (tys : EConstr.t array)
      : Model.Transition.t
      =
      let m : Model.FSM.t = W.get_fsm_b ~saturated () in
      let from : Model.State.t = M.run (ReModel.state tys.(3) m.states) in
      let label : Model.Label.t = M.run (ReModel.label tys.(5) m.alphabet) in
      try
        let ({ annotation; constructor_trees; _ }, destinations)
          : Model.ActionPair.t
          =
          (* NOTE: get actions [from] with [label] *)
          Model.ActionMap.reduce_by_label
            (Model.EdgeMap.find m.edges from)
            label
          |> Model.ActionMap.to_actionpairs
          (* NOTE: keep only those that are [bisimilar] *)
          |> Model.ActionPairs.filter_map (fun ((x, y) : Model.ActionPair.t) ->
            if Model.States.disjoint bisimilar y
            then None
            else Some (x, Model.States.inter bisimilar y))
          (* NOTE: get the pair with the shortest annotation (less steps to do) *)
          |> Model.ActionPairs.shortest_annotation
        in
        let constructor_tree = Model.Trees.min_opt constructor_trees in
        let goto : Model.State.t = Model.States.min_elt destinations in
        { from; goto; label; annotation; constructor_tree }
      with
      | Model.ActionPairs.IsEmpty -> raise CouldNotFindGotoState
    ;;

    exception MisMatchedStates of (Model.State.t * Model.State.t)

    let ensure_matching_states (x : Model.State.t) (y : Model.State.t) : unit =
      if Model.State.equal x y then () else raise (MisMatchedStates (x, y))
    ;;

    exception
      CouldNotGetGoalTransition of
        { b : Model.State.t
        ; wk_trans : EConstr.t
        }

    (** [handle_visible_transition hyp b wk_trans] ... introduces *)
    let handle_visible_transition
          (hyp : Model.Transition.t)
          (b : Model.State.t)
          (wk_trans : EConstr.t)
      : Tactic.t mm
      =
      Log.trace __FUNCTION__;
      (* log_econstr ~__FUNCTION__ "wk_trans" wk_trans; *)
      let bisimilar : Model.States.t = W.get_bisimilar_states hyp.goto in
      (* log_states ~__FUNCTION__ "bisimilar" bisimilar; *)
      let open Syntax in
      let* ty, tys = to_atomic wk_trans in
      (* NOTE: first try to do single action, if fail then saturated *)
      let goal : Model.Transition.t =
        (* try try_get_visible_transition ~saturated:false bisimilar tys with
           | CouldNotFindGotoState -> *)
        try try_get_visible_transition ~saturated:true bisimilar tys with
        | CouldNotFindGotoState ->
          raise (CouldNotGetGoalTransition { b; wk_trans })
      in
      _log_transition ~__FUNCTION__ "goal" goal;
      ensure_matching_states goal.from b;
      update_state (ApplyConstructors (ApplicableConstructors.init goal));
      Tacs.ex_intro_split goal.goto
    ;;

    let handle_wk_concl
          (hyp : Model.Transition.t)
          ({ wk_trans; wk_sim } : Concl.wk_conj)
      : Tactic.t mm
      =
      let open Syntax in
      let* { a'; b } = Concl.get_conj { wk_trans; wk_sim } in
      ensure_matching_states hyp.goto a';
      if Model.Transition.is_silent hyp && W.are_states_bisimilar a' b
      then (
        Log.trace ~__FUNCTION__ "is_exists, silent";
        Tacs.ex_intro_split b)
      else (
        Log.trace ~__FUNCTION__ "is_exists, trans";
        handle_visible_transition hyp b wk_trans)
    ;;

    (* * [handle_hyp_transition ()] determines which term to introduce for [exists b'], checking whether we can do this via a silent/tau transition, and sets up the information we will need for the next state. *)
    let handle_hyp_transition () : Tactic.t mm =
      Log.trace __FUNCTION__;
      let open Syntax in
      let* hyp : Model.Transition.t = Hyps.get_transition (W.get_fsm_a ()) in
      _log_transition ~__FUNCTION__ "hyp" hyp;
      update_state (Exists (Some hyp));
      let* { wk_trans; wk_sim } = Concl.get_wk_conj () in
      Log.trace ~__FUNCTION__ "wk_trans; wk_sim";
      let* unfold_opt = Tacs.try_unfold_any_of [ wk_trans; wk_sim ] in
      match unfold_opt with
      | Some x -> return x
      | None -> handle_wk_concl hyp { wk_trans; wk_sim }
    ;;

    (** [handle_appconstrs_entry_point args] ... *)
    let handle_appconstrs_entry_point (label : Model.Label.t) : Tactic.t mm =
      Log.trace __FUNCTION__;
      let open Syntax in
      let* constructor =
        if Model.Label.is_silent label
        then Tacs.apply_wk_none ()
        else Tacs.eapply_wk_some ()
      in
      let* unfold_silent = Tacs.unfold_silent () in
      Tactic.seq constructor unfold_silent |> return
    ;;

    (** [handle_appconstrs_stop ()] ... *)
    let handle_appconstrs_stop () : Tactic.t mm =
      Log.trace __FUNCTION__;
      let open Syntax in
      let* simplify = Tacs.simplify_and_subst_all () in
      let* refl = Tacs.eapply_rt1n_refl () in
      Tactic.seq simplify refl |> return
    ;;

    let handle_appconstrs_update_args ({ this; next } : Model.Annotation.t)
      : Model.TreeNode.t list option * Model.Annotation.t option
      =
      Some (Model.Trees.min this.using |> Model.Tree.minimize), next
    ;;

    (** [handle_appconstrs_update label] ... *)
    let handle_appconstrs_update (label : Model.Label.t) : Tactic.t mm =
      Log.trace __FUNCTION__;
      let open Syntax in
      let* rt1n = Tacs.eapply_rt1n_via label in
      let* unfold = Concl.try_unfold_any () in
      match unfold with
      | None -> return rt1n
      | Some unfold -> Tactic.seq unfold rt1n |> return
    ;;

    (** [handle_appconstrs_apply x] ...
        (* NOTE: relies on the bindings we extract early on *) *)
    let handle_appconstrs_apply (x : Model.TreeNode.t) : Tactic.t mm =
      Log.trace __FUNCTION__;
      let open Syntax in
      let* _, tys = get_concl () |> to_atomic in
      let tys = Array.map (fun x -> econstr_normalize x |> run) tys in
      let* is_tau = Concl.is_tau () in
      (* NOTE: we can't rely on the terms in [tys] being encoded since they may be from an intermediate layer of the LTS. *)
      (if is_tau
       then (* NOTE: index (3) since [tau lts x] => [tau (term * label) x] *)
         { from = tys.(3); goto = None; label = Some (Theories.c_None ()) }
       else { from = tys.(0); goto = None; label = None })
      |> Tacs.apply_constructor x
    ;;

    (** [handle_ ()] ... *)
    (* let handle_ () : Tactic.t mm = raise NotImplemented *)

    (***********************************************************************)

    exception StateNotImplemented of State.t
    exception StateCouldNothandle of State.t
    exception SkipNewProof
    exception ExitWeakSim
    exception ProofComplete

    let handle_new_proof
          ((a, b) : Constrexpr.constr_expr * Constrexpr.constr_expr)
      : Tactic.t mm
      =
      Log.trace __FUNCTION__;
      let open Syntax in
      let* x = Tacs.unfold_opt_constrexpr_list [ a; b ] in
      match x with
      | None -> raise SkipNewProof
      | Some x ->
        update_state WeakSim;
        return x
    ;;

    let handle_weaksim () : Tactic.t mm =
      Log.trace __FUNCTION__;
      let open Syntax in
      let* is_weak_sim : bool = Concl.is_weak_sim () in
      if is_weak_sim
      then (
        Log.trace ~__FUNCTION__ "is weak sim";
        let* is_weak_refl : bool = Concl.is_weak_refl () in
        if is_weak_refl
        then (
          Log.trace ~__FUNCTION__ "is weak refl";
          Tacs.apply_weak_sim_refl ())
        else
          let* has_hyp_cofix : bool = Hyps.can_solve_concl_cofix () in
          if has_hyp_cofix
          then Tacs.trivial ~msg:"trivial (solve cofix)" ()
          else handle_new_cofix ())
      else if is_done ()
      then raise ProofComplete
      else
        (* NOTE: try invert any that need to be inverted *)
        let* invert_opt = Hyps.try_invert_any () in
        match invert_opt with
        | Some x -> return x
        | None ->
          Log.trace ~__FUNCTION__ "no hyps to invert";
          (* NOTE: check if we need to unfold anything in the inverted hyps. *)
          let* unfold_opt = Hyps.try_unfold_any () in
          (match unfold_opt with
           | Some x -> return x
           | None ->
             Log.trace ~__FUNCTION__ "no terms to unfold";
             raise ExitWeakSim)
    ;;

    let handle_exists (hyp_opt : Model.Transition.t option) : Tactic.t mm =
      Log.trace __FUNCTION__;
      let open Syntax in
      let* is_exists : bool = Concl.is_exists () in
      if is_exists
      then (
        match hyp_opt with
        | None -> handle_hyp_transition ()
        | Some hyp ->
          Log.trace ~__FUNCTION__ "Some hyp";
          let open Syntax in
          let* { wk_trans; wk_sim } = Concl.get_wk_conj () in
          handle_wk_concl hyp { wk_trans; wk_sim })
      else (
        (* NOTE: assume we need to finish handling a silent action. *)
        Log.trace ~__FUNCTION__ "not exists, do_refl";
        update_state WeakSim;
        Tacs.do_refl ())
    ;;

    (* let handle_goal_transition ({ hyp; goal } : Transition.t) : Tactic.t mm =
      Log.trace __FUNCTION__;
      raise (StateNotImplemented (GoalTransition { hyp; goal }))
    ;; *)

    let handle_apply_constructors (args : ApplicableConstructors.t)
      : Tactic.t mm
      =
      Log.trace __FUNCTION__;
      _log_applicable_constructors "args" args;
      (* let open Syntax in *)
      (* NOTE: we first check if anything needs to be unfolded before proceeding *)
      (* let* unfold_opt = Concl.try_unfold_any () in
      match unfold_opt with
      | Some x -> return x
      | None -> *)
      (* NOTE: if nothing to unfold, keep applying constructors *)
      match args with
      | { current = None; label; _ } ->
        (* NOTE: entry-point *)
        update_state (ApplyConstructors { args with current = Some [] });
        handle_appconstrs_entry_point label
      | { current = Some []; annotation; _ } ->
        (match annotation with
         | None ->
           (* NOTE: stop *)
           update_state WeakSim;
           handle_appconstrs_stop ()
         | Some anno ->
           (* NOTE: update current, prepare for next transition *)
           let current, annotation = handle_appconstrs_update_args anno in
           update_state (ApplyConstructors { args with current; annotation });
           handle_appconstrs_update anno.this.label)
      | { current = Some (h :: tl); _ } ->
        (* NOTE: continue applying constructors *)
        update_state (ApplyConstructors { args with current = Some tl });
        handle_appconstrs_apply h
    ;;

    let handle_state () : Tactic.t mm =
      log_proofstate ();
      Hyps.log ~cofix_only:(Some false) ();
      log_concl ();
      match get_state () with
      | NewProof ab -> handle_new_proof ab
      | WeakSim -> handle_weaksim ()
      | Exists hyp_opt -> handle_exists hyp_opt
      (* | GoalTransition args -> handle_goal_transition args *)
      | ApplyConstructors xs -> handle_apply_constructors xs
      | Done -> raise NothingToDo
    ;;

    (** [step ()] ... *)
    let rec step () : Tactic.t =
      Log.trace __FUNCTION__;
      try run (handle_state ()) with
      | ProofComplete ->
        Log.trace ~__FUNCTION__ "_:ProofComplete => Done";
        update_state Done;
        Tactic.tactic ~msg:"Proof Complete" (Proofview.tclUNIT ())
      | SkipNewProof ->
        Log.trace ~__FUNCTION__ "NewProof:SkipNewProof => WeakSim";
        update_state WeakSim;
        step ()
      | ExitWeakSim ->
        Log.trace ~__FUNCTION__ "WeakSim:ExitWeakSim => Exists";
        update_state (Exists None);
        step ()
      (********************)
      | M.EncodingNotFound x ->
        Log.thing Warning "M.EncodingNotFound" x (Of M.Strfy.econstr);
        Log.thing Warning "(using P) EConstr" x (Of Strfy.econstr);
        Log.thing
          Warning
          "(using P) is encoded"
          (encoded x)
          (Of Utils.Strfy.bool);
        raise (M.EncodingNotFound x)
      | EncodingNotFound x ->
        Log.thing Warning "(M).EncodingNotFound" x (Of Strfy.econstr);
        Log.thing Warning "(using M) EConstr" x (Of M.Strfy.econstr);
        Log.thing
          Warning
          "(using P) is encoded"
          (M.encoded x)
          (Of Utils.Strfy.bool);
        raise (M.EncodingNotFound x)
    ;;
  end

  (** [get_updated_pstate x] returns the [pstate] updated by tactic [x]. *)
  let get_updated_pstate (x : unit Proofview.tactic) : Declare.Proof.t =
    Log.trace __FUNCTION__;
    let new_pstate, is_safe_tactic = Declare.Proof.by x (get_pstate ()) in
    if Bool.not is_safe_tactic
    then Log.warning ~__FUNCTION__ "unsafe tactic used";
    (* Log.notice "\n- - - -\n"; *)
    new_pstate
  ;;

  let exit_proof () : unit =
    update_state Done;
    raise NothingToDo
  ;;

  (** [step pstate] enters a fresh module [P] for this specific [pstate], and returns one updated with
  *)
  let step (pstate : Declare.Proof.t) : Declare.Proof.t =
    Log.trace __FUNCTION__;
    update_pstate pstate;
    if Proof.is_done (Declare.Proof.get pstate) then exit_proof ();
    Proofview.Goal.enter (fun gl ->
      let module P =
        P (struct
          let gl = ref gl
        end)
      in
      let x = P.step () in
      let y = P.run (P.Tacs.simplify_and_subst_all ()) in
      let z = Tactic.seq x y in
      (* Log.thing ~__FUNCTdION__ Debug "tactic" z (Of Tactic.to_string); *)
      Tactic.unpack z)
    |> get_updated_pstate
  ;;
end

(***********************************************************************)

module Log = Logger.Default

module type S = module type of Make (Log) ((val Api.default_encoding ()))

let the_proof_solver : (module S) ref option ref = ref None
let reset_the_proof_solver () : unit = the_proof_solver := None

exception NoProofSolverFound

let get_the_proof_solver () : (module S) ref =
  match !the_proof_solver with None -> raise NoProofSolverFound | Some x -> x
;;

let new_proof_solver () : (module S) ref =
  let module M : S = Make (Log) ((val Api.default_encoding ())) in
  the_proof_solver := Some (ref (module M : S));
  get_the_proof_solver ()
;;

let is_done () : bool =
  let module Ps = (val !(get_the_proof_solver ())) in
  Ps.is_done ()
;;

(***********************************************************************)

exception BisimilarityResultNotFound

(** [init ] ... *)
let init
      (pstate : Declare.Proof.t)
      (refs : Libnames.qualid list)
      (a : Constrexpr.constr_expr * Libnames.qualid)
      (b : Constrexpr.constr_expr * Libnames.qualid)
  : Declare.Proof.t
  =
  let ps : (module S) ref = new_proof_solver () in
  let module Ps : S = (val !ps) in
  Ps.check_bisimilarity refs a b;
  Ps.init pstate (fst a, fst b);
  pstate
;;

(** [step] ... *)
let step (pstate : Declare.Proof.t) : Declare.Proof.t =
  let module Ps = (val !(get_the_proof_solver ())) in
  Ps.step pstate
;;

let log_bound (x : int) : unit =
  Log.thing
    ~__FUNCTION__
    Warning
    "Stopping, exceeded bound"
    x
    (Of Utils.Strfy.int)
;;

let log_stop (x : int) : unit =
  Log.notice
    ~__FUNCTION__
    (Printf.sprintf
       "(Stopped) %s after %i iterations."
       (if is_done () then "solved" else "unsolved")
       x)
;;

(** [solve] ... *)
let solve ?(bound : int = 10) (pstate : Declare.Proof.t) : Declare.Proof.t =
  Log.trace __FUNCTION__;
  let rec f (n : int) (p : Declare.Proof.t) : int * Declare.Proof.t =
    Log.thing ~__FUNCTION__ Debug "iter" n (Of Utils.Strfy.int);
    match Int.compare n bound with
    | 1 ->
      log_bound bound;
      n, p
    | _ -> (try step p |> f (n + 1) with NothingToDo -> n, p)
  in
  let num, pstate = f 0 pstate in
  log_stop num;
  pstate
;;
