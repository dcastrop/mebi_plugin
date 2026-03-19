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

  (* let rocq_ind (f : 'a -> string) : 'a Rocq_ind.t -> string =
     fstring (Rocq_ind.to_string f)
     ;; *)

  let econstr_bindings : EConstr.t Tactypes.bindings -> string = function
    | NoBindings -> "NoBindings"
    | ImplicitBindings xs ->
      (* Utils.Strfy.list econstr xs |> Printf.sprintf "ImplicitBindings: %s" *)
      "TODO: implicit bindings"
    | ExplicitBindings xs ->
      (* Utils.Strfy.list
        (Of
           (fun ({ v = x, y; _ } :
                  (Tactypes.quantified_hypothesis * EConstr.t) CAst.t) ->
             Printf.sprintf
               "%s : %s"
               (match x with
                | AnonHyp x -> Printf.sprintf "%i" x
                | NamedHyp { v; _ } -> Names.Id.to_string v)
               (econstr y)))
        xs
      |> Printf.sprintf "ExplicitBindings: %s" *)
      "TODO: explicit bindings"
  ;;
end
