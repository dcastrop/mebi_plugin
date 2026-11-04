module type S = sig
  val constr : Constr.t -> string
  val constr_kind : Constr.t -> string
  val econstr : EConstr.t -> string
  val econstr_kind : EConstr.t -> string
  val econstr_rel_decl : EConstr.rel_declaration -> string
  val hyp_name : Rocq_utils.hyp -> string
  val hyp_type : Rocq_utils.hyp -> string
  val hyp : Rocq_utils.hyp -> string
  val hyp_value : Rocq_utils.hyp -> string
  val econstr_bindings : EConstr.t Tactypes.bindings -> string
end

(* See the [.mli]. Each printer is {!Rocq_utils.Strfy}'s, given the
   current environment and evar map by [M.fstring]. *)
module Make (M : Rocq_monad.S) : S = struct
  open M

  let constr : Constr.t -> string = fstring Rocq_utils.Strfy.constr
  let constr_kind : Constr.t -> string = fstring Rocq_utils.Strfy.constr_kind
  let econstr : EConstr.t -> string = fstring Rocq_utils.Strfy.econstr
  let econstr_kind : EConstr.t -> string = fstring Rocq_utils.Strfy.econstr_kind

  let econstr_rel_decl : EConstr.rel_declaration -> string =
    fstring Rocq_utils.Strfy.econstr_rel_decl
  ;;

  let hyp_name : Rocq_utils.hyp -> string = Rocq_utils.Strfy.hyp_name
  let hyp_type : Rocq_utils.hyp -> string = fstring Rocq_utils.Strfy.hyp_type

  let hyp (x : Rocq_utils.hyp) : string =
    Printf.sprintf "%s: %s" (hyp_name x) (hyp_type x)
  ;;

  let hyp_value : Rocq_utils.hyp -> string = fstring Rocq_utils.Strfy.hyp_value

  let econstr_bindings : EConstr.t Tactypes.bindings -> string = function
    | NoBindings -> "NoBindings"
    | ImplicitBindings _ -> "TODO: implicit bindings"
    | ExplicitBindings _ -> "TODO: explicit bindings"
  ;;
end
