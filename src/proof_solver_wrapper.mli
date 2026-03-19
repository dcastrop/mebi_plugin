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
  S with type enc = Enc.t and type tree = Enc.Tree.t
