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

module Make (Log : Logger.S) (Enc : Encoding.S) (X : Args) :
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

  module I :
    Rocq_monad_utils.S with type enc = Enc.t and type tree = Enc.Tree.t =
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
      (Enc)

  include I

  let log_concl () : unit = log_econstr ~s:"concl" (get_concl ())
  let log_hyps () : unit = Log.things Debug "hyps" (get_hyps ()) Strfy.hyp

  (** [EConstrSet] is a custom [Set] of [EConstr.t] that allows terms to be compared more efficiently during {b a single proof step only} -- since this is built for each step. {e Though, since each proof step we have a new [env] and [sigma], the same term may be encoded differently across iteration steps, so there isn't necessarily a way for us to compare terms in a proof across iterations anyway. {b ! This needs to be investigated.}}
  *)
  module EConstrSet = struct
    include Set.Make (struct
        type t = EConstr.t

        let compare (a : t) (b : t) : int = econstr_compare a b
      end)

    (* let add (x : elt) (xs : t) : t = add (run (econstr_normalize x)) xs *)
  end
end
