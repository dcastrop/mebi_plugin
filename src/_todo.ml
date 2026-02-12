module Wrapper = struct
  module type TYPE = sig
    module Monad : Monad.TYPE
    module Term : Term.TYPE
    module Model : Model.TYPE
  end

  module Make (Ctx : Context.TYPE) (Enc : Encoding.TYPE) : TYPE = struct
    module Monad = Monad.Make (Ctx) (Enc)

    (* module Monad = Monad.Make (Ctx) (Term) *)
    module Model = Model.Make (Term.MakeFromEnc (Enc))
    module Term = Model.T
    include Model

    (* TODO: *)
    (* let decode_term (x : Term.t) : EConstr.t = Monad.decode x *)
    (* let decode_label (x : Label.t) : EConstr.t = decode_term x.term *)
  end
end

module MEBI (Ctx : Context.TYPE) : Wrapper.TYPE =
  Wrapper.Make (Ctx) (Encoding.Int)

(***********************************************************************)

module type SProofSolver = sig end

module ProofSolver : SProofSolver = struct
  module type TYPE = sig
    val gl : unit -> Proofview.Goal.t ref

    module Ctx : Context.TYPE

    val update : Proofview.Goal.t ref -> unit
  end

  module type S = sig
    val gl : unit -> Proofview.Goal.t ref
  end

  module Make (X : S) : TYPE = struct
    let gl () : Proofview.Goal.t ref = X.gl ()

    module Ctx : Context.TYPE = Context.Make (struct
        let env () : Environ.env ref = ref (Proofview.Goal.env !(gl ()))
        let sigma () : Evd.evar_map ref = ref (Proofview.Goal.sigma !(gl ()))
      end)

    let update (gl : Proofview.Goal.t ref) : unit =
      Ctx.update (ref (Proofview.Goal.env !gl)) (ref (Proofview.Goal.sigma !gl))
    ;;
  end
end

let p () : (module SProofSolver) option ref = ref None

exception NoP of unit

let get_p () : (module SProofSolver) ref =
  match !(p ()) with None -> raise (NoP ()) | Some p -> ref p
;;

(* TODO:
   - in [loader.v] add command to initialise the plugin, creating references to the constructed modules.
   - thereafter, each command will "get" the module, which if [None] will throw a "reload the plugin" error *)

(***********************************************************************)

(* module _ = struct

   module type TYPE = sig

   end

   module type S = sig

   end

   module Make (X:S) : TYPE = struct

   end

   end *)

(***********************************************************************)

(** transition *)
(* module Trans = struct
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
  end *)
