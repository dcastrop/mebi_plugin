(** Rocq terms and hypotheses as strings, printed in the monad's current
    environment and evar map ([M.fstring] over {!Rocq_utils.Strfy}). Each
    function raises nothing. *)
module type S = sig
  (** [constr x] is [x] printed. *)
  val constr : Constr.t -> string

  (** [constr_kind x] is the name of [x]'s kind (its head constructor). *)
  val constr_kind : Constr.t -> string

  (** [econstr x] is [x] printed. *)
  val econstr : EConstr.t -> string

  (** [econstr_kind x] is the name of [x]'s kind. *)
  val econstr_kind : EConstr.t -> string

  (** [econstr_rel_decl d] is the local declaration [d] printed. *)
  val econstr_rel_decl : EConstr.rel_declaration -> string

  (** [hyp_name h] is the name of the hypothesis [h]. *)
  val hyp_name : Rocq_utils.hyp -> string

  (** [hyp_type h] is the type of the hypothesis [h], printed. *)
  val hyp_type : Rocq_utils.hyp -> string

  (** [hyp h] is ["name: type"] for the hypothesis [h]. *)
  val hyp : Rocq_utils.hyp -> string

  (** [hyp_value h] is the value of the hypothesis [h] (a local
      definition's body), printed. *)
  val hyp_value : Rocq_utils.hyp -> string

  (** [econstr_bindings b] is ["NoBindings"] for no bindings; for implicit
      or explicit bindings, only a placeholder ("TODO: ...") so far. *)
  val econstr_bindings : EConstr.t Tactypes.bindings -> string
end

(** [Make (M)] prints with [M]'s current environment and evar map. *)
module Make (M : Rocq_monad.S) : S
