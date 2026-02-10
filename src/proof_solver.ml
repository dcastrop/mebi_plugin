module Make (Log : Logger.SLogger) (E : Encoding.SEncoding) = struct
  (** [module W] is for running the main part of the algorithm (pre-proof). *)
  include Wrapper.Make (Log) (Rocq_context.Default) (E)

  (***********************************************************************)

  let the_result : Model.Bisimilar.t ref option ref = ref None

  exception NoResultFound of unit

  let get_the_result () : Model.Bisimilar.t =
    match !the_result with None -> raise (NoResultFound ()) | Some x -> !x
  ;;

  exception CannotOverrideResult of Model.Bisimilar.t

  let set_the_result (x : Model.Bisimilar.t) : unit =
    match !the_result with
    | None -> the_result := Some (ref x)
    | Some y -> raise (CannotOverrideResult !y)
  ;;

  (***********************************************************************)

  (** [module P] is a monad that contains the proof environment and context. *)
  module P (X : sig
      (* val pstate : Declare.Proof.t *)
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
        (M.Enc)

    let get_concl () : EConstr.t = Proofview.Goal.concl (gl ())
    let get_hyps () : Rocq_utils.hyp list = Proofview.Goal.hyps (gl ())
    let get_hyp_names () : Names.Id.Set.t = Context.Named.to_vars (get_hyps ())

    let next_name_of (names : Names.Id.Set.t) (x : Names.Id.t) : Names.Id.t =
      Namegen.next_ident_away x names
    ;;

    let new_name_of_string (x : string) : Names.Id.t =
      next_name_of (get_hyp_names ()) (Names.Id.of_string x)
    ;;

    let new_cofix_name () : Names.Id.t = new_name_of_string "Cofix0"
    let new_H_name () : Names.Id.t = new_name_of_string "H0"
  end

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

    let unpack (x : tactic) : unit Proofview.tactic =
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
    ;;

    (** [seq a b] appends [b] to the sequence of [a]. *)
    let rec seq : t -> t -> t = function
      | { this; next = None } -> fun (b : t) -> { this; next = Some b }
      | { this; next = Some next } ->
        fun (b : t) -> { this; next = Some (seq next b) }
    ;;

    let empty () : t = tactic (Proofview.tclUNIT ())

    exception EmptyTacticChain of unit

    (** [chain ?nonempty (x::xs)] applies [seq x (chain xs)].
        @raise EmptyTacticChain if the list is empty and [?nonempty] s true. *)
    let rec chain ?(nonempty : bool = false) : t list -> t = function
      | [] -> if nonempty then raise (EmptyTacticChain ()) else empty ()
      | h :: [] -> h
      | h :: tl -> seq h (chain tl)
    ;;

    (** [update_proof pstate x] applies tactic(s) [x] and returns the updated [pstate].
    *)
    let update_proof (pstate : Declare.Proof.t) (x : t) : Declare.Proof.t =
      let rec f : t -> unit Proofview.tactic = function
        | { this; next = None } -> unpack this
        | { this; next = Some next } -> Proofview.tclTHEN this (f next)
      in
      let new_pstate, is_safe_tactic = Declare.Proof.by (f x) pstate in
      if Bool.not is_safe_tactic
      then Log.warning ~__FUNCTION__ "unsafe tactic used";
      new_pstate
    ;;
  end

  (** tactics *)
  module Tacs = struct
    let inversion (x : Rocq_utils.hyp) : Tactic.t P.mm =
      Inv.inv_tac (Context.Named.Declaration.get_id x)
      |> Tactic.tactic
           ~msg:
             (Printf.sprintf
                "inversion %s"
                (P.Strfy.hyp_name x) (* (P.Strfy.hyp_type x) *))
      |> P.return
    ;;

    let subst_all () : Tactic.t P.mm =
      Equality.subst_all ()
      |> Tactic.tactic ~msg:(Printf.sprintf "(subst all)")
      |> P.return
    ;;

    let simplify_concl () : Tactic.t P.mm =
      Tactics.simpl_in_concl |> Tactic.tactic ~msg:"simpl" |> P.return
    ;;

    let simplify_hyp (x : Rocq_utils.hyp) : Tactic.t P.mm =
      Tactics.simpl_in_hyp (Context.Named.Declaration.get_id x, Locus.InHyp)
      |> Tactic.tactic ~msg:(Printf.sprintf "simpl in %s" (P.Strfy.hyp_name x))
      |> P.return
    ;;

    let simplify_hyps () : Tactic.t P.mm =
      match Proofview.Goal.hyps !gl with
      | [] -> Tactic.empty () |> P.return
      | x :: [] -> simplify_hyp x
      | x :: xs ->
        let open P.Syntax in
        let* x : Tactic.t = simplify_hyp x in
        let f (i : int) (x : Tactic.t) : Tactic.t P.mm =
          let y : Rocq_utils.hyp = List.nth xs i in
          let* y : Tactic.t = simplify_hyp y in
          Tactic.seq x y |> P.return
        in
        P.iterate 0 (List.length xs - 1) x f
    ;;

    let simplify_all () : Tactic.t P.mm =
      let open P.Syntax in
      let* concl : Tactic.t = simplify_concl () in
      let* hyps : Tactic.t = simplify_hyps () in
      Tactic.seq concl hyps |> P.return
    ;;

    let simplify_and_subst_all () : Tactic.t P.mm =
      let open P.Syntax in
      let* simpls : Tactic.t = simplify_all () in
      let* substs : Tactic.t = subst_all () in
      Tactic.seq simpls substs |> P.return
    ;;

    let cofix () : Tactic.t P.mm =
      let name : Names.Id.t = new_cofix_name () in
      Tactics.cofix name
      |> Tactic.tactic
           ~msg:(Printf.sprintf "cofix %s" (Names.Id.to_string name))
      |> P.return
    ;;

    let intros_all () : Tactic.t P.mm =
      Tactics.intros |> Tactic.tactic ~msg:"intros" |> P.return
    ;;

    (** [intro_as x] applies the introduction tactic using the (next non-conficting) name [x].
    *)
    let intro_as (x : string) : Tactic.t P.mm =
      let name : Names.Id.t = new_name_of_string x in
      Tactics.introduction name
      |> Tactic.tactic
           ~msg:(Printf.sprintf "intro %s" (Names.Id.to_string name))
      |> P.return
    ;;

    (* *)
    let apply (x : EConstr.t) : Tactic.t P.mm =
      Tactics.apply x
      |> Tactic.tactic ~msg:(Printf.sprintf "apply %s" (P.Strfy.econstr x))
      |> P.return
    ;;

    let eapply (x : EConstr.t) : Tactic.t P.mm =
      Tactics.eapply x
      |> Tactic.tactic ~msg:(Printf.sprintf "eapply %s" (P.Strfy.econstr x))
      |> P.return
    ;;

    (* *)
    exception CannotUnfoldConstr of Constr.t

    (** [unfold_constr x]
        @raise CannotUnfoldConstr
          of [x] if [Constr.kind x] is not [Const (_, _)]. *)
    let unfold_constr ?(in_hyp : Rocq_utils.hyp option) (x : Constr.t)
      : Tactic.t P.mm
      =
      let f (name : Names.Constant.t) : unit Proofview.tactic =
        Option.cata
          (fun (y : Rocq_utils.hyp) ->
            Tactics.unfold_in_hyp
              [ Locus.AllOccurrences, Evaluable.EvalConstRef name ]
              (Context.Named.Declaration.get_id y, Locus.InHyp))
          (Tactics.unfold_constr (Names.GlobRef.ConstRef name))
          in_hyp
      in
      match Constr.kind x with
      | Const (name, _) ->
        f name
        |> Tactic.tactic
             ~msg:(Printf.sprintf "unfold %s" (Names.Constant.to_string name))
        |> P.return
      | _ -> raise (CannotUnfoldConstr x)
    ;;

    let unfold_econstr ?(in_hyp : Rocq_utils.hyp option) (x : EConstr.t)
      : Tactic.t P.mm
      =
      let open P.Syntax in
      let* y : Constr.t = P.econstr_to_constr x in
      (* NOTE: below helps keep this function cleaner to use. i.e., [unfold_econstr ~in_hyp:x] rather than [~in_hyp:(Some x)] *)
      Option.cata
        (fun in_hyp -> unfold_constr ~in_hyp y)
        (unfold_constr y)
        in_hyp
    ;;

    let unfold_constrexpr
          ?(in_hyp : Rocq_utils.hyp option)
          (x : Constrexpr.constr_expr)
      : Tactic.t P.mm
      =
      let open P.Syntax in
      let* y : EConstr.t = P.constrexpr_to_econstr x in
      (* NOTE: below helps keep this function cleaner to use. i.e., [unfold_constrexpr ~in_hyp:x] rather than [~in_hyp:(Some x)] *)
      Option.cata
        (fun in_hyp -> unfold_econstr ~in_hyp y)
        (unfold_econstr y)
        in_hyp
    ;;
  end

  (** [module Decode] handles obtaining [EConstr.t] from [module M]. *)
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

  module Theory = struct
    let apply_Pack_sim () : Tactic.t P.mm = Tacs.apply (Theories.c_Pack_sim ())
    let apply_In_sim () : Tactic.t P.mm = Tacs.apply (Theories.c_In_sim ())
    let apply_wk_some () : Tactic.t P.mm = Tacs.apply (Theories.c_wk_some ())
    let apply_wk_none () : Tactic.t P.mm = Tacs.apply (Theories.c_wk_none ())

    let apply_rt1n_refl () : Tactic.t P.mm =
      Tacs.apply (Theories.c_rt1n_refl ())
    ;;

    let apply_rt1n_trans () : Tactic.t P.mm =
      Tacs.apply (Theories.c_rt1n_trans ())
    ;;

    let eapply_rt1n_refl () : Tactic.t P.mm =
      Tacs.eapply (Theories.c_rt1n_refl ())
    ;;

    let eapply_rt1n_trans () : Tactic.t P.mm =
      Tacs.eapply (Theories.c_rt1n_trans ())
    ;;

    let unfold_silent () : Tactic.t P.mm =
      Tacs.unfold_econstr (Theories.c_silent ())
    ;;

    let unfold_silent1 () : Tactic.t P.mm =
      Tacs.unfold_econstr (Theories.c_silent1 ())
    ;;

    (** [is_theory x y] checks if term [x] is equal to theory term [y], catching the exception thrown when [EConstr.kind_of_type x] is not [AtomicType (ty, tys)].
    *)
    let is_theory (x : EConstr.t) (y : EConstr.t) : bool P.mm =
      try
        let open P.Syntax in
        let* sigma = P.get_sigma in
        Rocq_utils.econstr_to_atomic sigma x |> fst |> P.econstr_eq y
      with
      | Rocq_utils.Rocq_utils_EConstrIsNotA_Type _ -> P.return false
    ;;

    (** exists *)
    let is_exists (x : EConstr.t) : bool P.mm = is_theory x (Theories.c_ex ())

    (** weak simulation*)
    let is_weak_sim (x : EConstr.t) : bool P.mm =
      is_theory x (Theories.c_weak_sim ())
    ;;

    (** weak transition *)
    let is_weak (x : EConstr.t) : bool P.mm = is_theory x (Theories.c_weak ())

    let is_tau (x : EConstr.t) : bool P.mm = is_theory x (Theories.c_tau ())

    let is_silent (x : EConstr.t) : bool P.mm =
      is_theory x (Theories.c_silent ())
    ;;

    let is_silent1 (x : EConstr.t) : bool P.mm =
      is_theory x (Theories.c_silent1 ())
    ;;

    let is_LTS (x : EConstr.t) : bool P.mm = is_theory x (Theories.c_LTS ())
    let is_None (x : EConstr.t) : bool P.mm = is_theory x (Theories.c_None ())
    let is_Some (x : EConstr.t) : bool P.mm = is_theory x (Theories.c_Some ())
    (* let is_ (x : EConstr.t) : bool P.mm = is_theory x (Theories.c_ ()) *)

    (** *)
    let get_theory_enc (f : EConstr.t -> bool P.mm) : M.Enc.t M.mm =
      let open M.Syntax in
      let* fm = M.get_fwdmap in
      let rec find_theory : (EConstr.t * M.Enc.t) list -> M.Enc.t M.mm =
        function
        | [] -> raise Not_found
        | (x, y) :: tl ->
          let is_match : bool = P.run (f x) in
          if is_match then M.return y else find_theory tl
      in
      M.F.to_seq fm |> List.of_seq |> find_theory
    ;;

    exception NoEncodingFoundFor_TheoriesNone of unit

    let get_None_enc () : M.Enc.t M.mm =
      try get_theory_enc is_None with
      | Not_found -> raise (NoEncodingFoundFor_TheoriesNone ())
    ;;

    exception NoEncodingFoundFor_TheoriesSome of unit

    let get_Some_enc () : M.Enc.t M.mm =
      try get_theory_enc is_Some with
      | Not_found -> raise (NoEncodingFoundFor_TheoriesSome ())
    ;;

    exception NotEqTheory of unit

    (** *)
    let get_theory_enc_if_eq (x : EConstr.t) (f : EConstr.t -> bool P.mm)
      : M.Enc.t M.mm
      =
      let is_eq : bool = P.run (f x) in
      try if is_eq then get_theory_enc f else raise Not_found with
      | Not_found -> raise (NotEqTheory ())
    ;;

    let get_None_enc_if_eq (x : EConstr.t) : M.Enc.t M.mm =
      get_theory_enc_if_eq x is_None
    ;;

    let get_Some_enc_if_eq (x : EConstr.t) : M.Enc.t M.mm =
      get_theory_enc_if_eq x is_Some
    ;;

    exception FSM_HasNoSilentLabel of Model.FSM.t

    let is_fsm_silent_label (x : EConstr.t) (m : Model.FSM.t) : bool M.mm =
      match
        Model.Labels.filter Model.Label.is_silent m.info.weak_labels
        |> Model.Labels.to_list
      with
      | [] -> raise (FSM_HasNoSilentLabel m)
      | ys -> M.exists_eq x ys Decode.label
    ;;

    exception FSM_HasNoVisibleLabel of Model.FSM.t

    (** i.e., not silent label *)
    let is_fsm_visible_label (x : EConstr.t) (m : Model.FSM.t) : bool M.mm =
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

    let is_fsm_weak_labels (x : EConstr.t) (m : Model.FSM.t) : bool M.mm =
      let open M.Syntax in
      let* is_silent = is_fsm_silent_label x m in
      if is_silent then M.return true else is_fsm_visible_label x m
    ;;

    exception FSM_HasNoConstructors of Model.FSM.t

    let is_fsm_constructor (x : EConstr.t) (m : Model.FSM.t) : bool M.mm =
      match m with
      | { info = { meta = None; _ }; _ } -> raise (FSM_HasNoConstructors m)
      | { info = { meta = Some { lts; _ }; _ }; _ } ->
        M.exists_eq x lts Decode.lts_constructor
    ;;
  end

  module ReModel = struct
    exception CouldNotFind_State of (EConstr.t * Model.States.t)

    let state (x : EConstr.t) (ys : Model.States.t) : Model.State.t M.mm =
      try
        let term : M.Enc.t = M.get_encoding x in
        (* NOTE: [Model.States.compare] only cares about [term]. *)
        Model.States.find { term; pp = None } ys |> M.return
      with
      | Not_found -> raise (CouldNotFind_State (x, ys))
    ;;

    exception CouldNotFind_Label of (EConstr.t * Model.Labels.t)

    let label (x : EConstr.t) (ys : Model.Labels.t) : Model.Label.t M.mm =
      let f (term : M.Enc.t) : Model.Label.t M.mm =
        (* NOTE: [Model.Labels.compare] only cares about [is_silent=Some _] *)
        Model.Labels.find { term; is_silent = None; pp = None } ys |> M.return
      in
      try M.get_encoding x |> f with
      | Not_found ->
        let open M.Syntax in
        (* NOTE: is it [None] (i.e., a silent action) *)
        (try
           let* term : M.Enc.t = Theory.get_None_enc_if_eq x in
           f term
         with
         | Theory.NotEqTheory () ->
           (* NOTE: is it [Some] (i.e., a visible action) *)
           (try
              let* term : M.Enc.t = Theory.get_Some_enc_if_eq x in
              f term
            with
            | Theory.NotEqTheory () -> raise (CouldNotFind_Label (x, ys))))
    ;;
  end

  (** transition *)
  module Trans = struct
    module Make (X : sig
        type state
        type label

        val eq_state : state -> state -> bool M.mm
        val eq_label : label -> label -> bool M.mm
      end) =
    struct
      type t =
        | Full of
            { from : X.state
            ; label : X.label
            ; goto : X.state
            }
        | Partial of partial

      and partial =
        | NoGoto of
            { from : X.state
            ; label : X.label
            }
        | NoLabel of
            { from : X.state
            ; goto : X.state
            }
        | Just of { from : X.state }

      exception LabelArgNotEqExisting of (X.label * partial)
      exception GotoArgNotEqExisting of (X.state * partial)
      exception ArgsFillNothing of unit

      (** [fill ?label ?goto x] handles "filling out" partial transition [x] with either [?label] or [?goto].
          @raise LabelArgNotEqExisting
            if [?label=Some _] and not equal [x.label].
          @raise GotoArgNotEqExisting if [?label=Some _] and not equal [x.goto].
          @raise ArgsFillNothing if [x] would be unchanged by this function. *)
      let fill ?(label : X.label option) ?(goto : X.state option) (x : partial)
        : t M.mm
        =
        let open M.Syntax in
        let none_or_eq (type a) (f : a -> a -> bool M.mm) (e : a -> exn) (y : a)
          : a option -> unit M.mm
          = function
          | None -> M.return ()
          | Some z ->
            let* is_eq : bool = f y z in
            if is_eq then M.return () else raise (e z)
        in
        match x with
        | NoGoto { from; label = label' } ->
          let* () =
            none_or_eq
              X.eq_label
              (fun a -> LabelArgNotEqExisting (a, x))
              label'
              label
          in
          (match goto with
           | None -> raise (ArgsFillNothing ())
           | Some goto -> Full { from; label = label'; goto } |> M.return)
        | NoLabel { from; goto = goto' } ->
          let* () =
            none_or_eq
              X.eq_state
              (fun a -> GotoArgNotEqExisting (a, x))
              goto'
              goto
          in
          (match label with
           | None -> raise (ArgsFillNothing ())
           | Some label -> Full { from; label; goto = goto' } |> M.return)
        | Just { from } ->
          (match label, goto with
           | None, None -> raise (ArgsFillNothing ())
           | Some label, None -> Partial (NoGoto { from; label }) |> M.return
           | None, Some goto -> Partial (NoLabel { from; goto }) |> M.return
           | Some label, Some goto -> Full { from; label; goto } |> M.return)
      ;;
    end

    module RocqTrans = Make (struct
        type t = EConstr.t
        type state = t
        type label = t

        let eq = M.econstr_eq
        let eq_state = eq
        let eq_label = eq
      end)

    module EncTrans = Make (struct
        type t = M.Enc.t
        type state = t
        type label = t

        let eq (a : t) (b : t) : bool M.mm = M.Enc.equal a b |> M.return
        let eq_state = eq
        let eq_label = eq
      end)

    module ModelTrans = Make (struct
        type state = Model.State.t
        type label = Model.Label.t

        let eq_state (a : state) (b : state) : bool M.mm =
          Model.State.equal a b |> M.return
        ;;

        let eq_label (a : label) (b : label) : bool M.mm =
          Model.Label.equal a b |> M.return
        ;;
      end)

    type t =
      | RocqTransition of RocqTrans.t
      | EncTransition of EncTrans.t
      | ModelTransition of ModelTrans.t
      | Complete of Model.Transition.t

    (** [elevate x] attempts to transform [x] via the order [RocqTransition < EncTransition < ModelTransition < Complete].
        @raise _
          if [EncTransition] is not [Full], since a [ModelTransition] would not be able to be obtained.
    *)
    (* let elevate : t -> t M.mm = 
    let f_enc (x:EConstr.t): M.Enc.t M.mm= (M.get_encoding x) |> M.return in  
    let f_state(x:EConstr.t):Model.State.t M.mm = (ReModel.state x) in  
    let f_label(x:EConstr.t):Model.Label.t M.mm = (ReModel.label x) in  
    function
    | RocqTransition (Full { from; label; goto }) -> (
      EncTransition (Full {from=f_enc from;label=f_enc label;goto=f_enc goto}) |> M.return
    )
    | RocqTransition (Partial (NoLabel { from; goto })) -> ()
    | RocqTransition (Partial (NoGoto { from; label })) -> ()
    | RocqTransition (Partial (Just { from })) -> ()
    | _ -> ()
  ;; *)
  end

  (** hypothesis *)
  module Hyp = struct end
end

(***********************************************************************)

module type S =
    module type of Make (Logger.Default) ((val Api.default_encoding ()))

let the_proof_solver : (module S) ref option ref = ref None
let reset_the_proof_solver () : unit = the_proof_solver := None

exception NoProofSolverFound of unit

let get_the_proof_solver () : (module S) ref =
  match !the_proof_solver with
  | None -> raise (NoProofSolverFound ())
  | Some x -> x
;;

let new_proof_solver () : (module S) ref =
  let module M : S = Make (Logger.Default) ((val Api.default_encoding ())) in
  the_proof_solver := Some (ref (module M : S));
  get_the_proof_solver ()
;;

exception BisimilarityResultNotFound of unit

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
  let r : Ps.Model.Bisimilar.t option =
    Ps.Command.run refs (Ps.Command.CheckBisim { a; b })
    |> Ps.M.run ~reset_encoding:true
  in
  let () =
    match r with
    | None -> raise (BisimilarityResultNotFound ())
    | Some r -> Ps.set_the_result r
  in
  (* TODO: fix Ps.P.mm *)
  (* Ps.Tacs.unfold_constrexpr (fst a)
  |> Ps.Tactic.update_proof pstate
  |> Ps.Tacs.unfold_constrexpr (fst b)
  |> Ps.Tactic.update_proof pstate *)
  pstate
;;
