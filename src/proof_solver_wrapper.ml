module type S = sig
  val gl : unit -> Proofview.Goal.t
  val get_concl : unit -> Evd.econstr
  val get_hyps : unit -> Rocq_utils.hyp list
  val get_hyp_name : Rocq_utils.hyp -> Names.Id.t
  val get_hyp_names : unit -> Names.Id.Set.t
  val next_name_of : Names.Id.Set.t -> Names.Id.t -> Names.Id.t
  val new_name_of_string : string -> Names.Id.t
  val new_cofix_name : unit -> Names.Id.t
  val new_H_name : unit -> Names.Id.t
  val get_all_cofix_hyp_names : unit -> Names.Id.Set.t
  val get_all_non_cofix_hyp_names : unit -> Names.Id.Set.t

  include Rocq_monad_utils.S

  val log_concl : unit -> unit
  val log_hyps : unit -> unit

  module EConstrSet : sig
    include Set.S with type elt = EConstr.t
  end
end

module type Args = sig
  val gl : Proofview.Goal.t ref
end

module Make (Enc : Encoding.S) (X : Args) :
  S with type enc = Enc.t and type tree = Enc.Tree.t = struct
  let gl () : Proofview.Goal.t = !X.gl
  let get_concl () : EConstr.t = Proofview.Goal.concl (gl ())
  let get_hyps () : Rocq_utils.hyp list = Proofview.Goal.hyps (gl ())

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

  (** This step's own monad/encoding stack, separate from the command-time one.

      Separate because the two read different [env]/[sigma]: this one the goal,
      the command-time one the global environment. [Bi_encoding] hashes its
      [EConstr.t] keys under whichever it is given, so one table cannot serve
      both -- entries would go in under one [sigma] and be looked up under the
      other.

      Nothing is lost by not sharing. The lookups that have to hit the model --
      [ReModel.state] and [ReModel.label] in [Proof_solver_step] -- go through
      the command-time [W.M] and always did. This table only ever backs [encode]
      / [econstr_compare] / [EConstrSet] below, all of which are per-step by
      construction. *)
  module I : Rocq_monad_utils.S with type enc = Enc.t and type tree = Enc.Tree.t =
    Rocq_monad_utils.Make (Enc)

  (* Installed once, here, rather than per [I.run]: see [Bi_encoding.set_ctx].
     [of_goal] closes over [X.gl], so it still tracks the proof as it advances
     -- what is fixed is *where* the context is read from, not its contents. *)
  let () = I.set_ctx (Rocq_context.of_goal X.gl)

  include I

  let log_concl () : unit = log_econstr ~s:"concl" (get_concl ())
  let log_hyps () : unit = Logger.things Debug "hyps" (get_hyps ()) Strfy.hyp

  (** [EConstrSet] is a custom [Set] of [EConstr.t] that allows terms to be compared more efficiently during {b a single proof step only}. Since each proof step gets a new [env] and [sigma] (a fresh [module Iter], and with it a fresh [EConstrSet], is created on every call to {!Proof_solver.step} -- see [make]/[step] there), the same underlying term may encode differently across steps, so an [EConstrSet.t] built in one step is not meaningful to compare against one built in another.

      {b Audited 2026-09-27:} no call site does this. Every use ([Proof_solver_tactics.collect_component_econstrs]/[try_unfold_any]) builds, consumes and discards an [EConstrSet.t] within a single function call, and no persistent state type ([Proof_solver_statem.S], [Proof_solver.t]) ever stores one. This holds structurally, not by convention: the whole module tree containing [EConstrSet] is torn down and rebuilt fresh each step, so a value could not survive to the next step even if something tried to stash it. If a future change introduces a call site that returns or stores an [EConstrSet.t] outside of one step's local computation, that would break this invariant and needs the same scrutiny this comment once flagged. *)
  module EConstrSet = struct
    include Set.Make (struct
        type t = EConstr.t

        let compare (a : t) (b : t) : int = econstr_compare a b
      end)

    (* let add (x : elt) (xs : t) : t = add (run (econstr_normalize x)) xs *)
  end
end
