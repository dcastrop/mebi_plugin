module type S = sig
  type t =
    (* NOTE: *)
    | Invalid_Sort_LTS of Sorts.family
    | Invalid_Sort_Type of Sorts.family
    (* NOTE: *)
    | InvalidCheckUpdatedCtx of
        (Environ.env
        * Evd.evar_map
        * EConstr.t list
        * EConstr.rel_declaration list)

  exception MEBI_exn of t

  (* NOTE: *)
  val invalid_sort_lts : Sorts.family -> exn
  val invalid_sort_type : Sorts.family -> exn

  (* NOTE: *)
  val invalid_check_updated_ctx
    :  Environ.env
    -> Evd.evar_map
    -> EConstr.t list
    -> EConstr.rel_declaration list
    -> exn
end

module Make (_ : Rocq_context.SRocq_context) : S = struct
  type t =
    (* NOTE: *)
    | Invalid_Sort_LTS of Sorts.family
    | Invalid_Sort_Type of Sorts.family
    (* NOTE: *)
    | InvalidCheckUpdatedCtx of
        (Environ.env
        * Evd.evar_map
        * EConstr.t list
        * EConstr.rel_declaration list)

  exception MEBI_exn of t

  let invalid_sort_lts x = MEBI_exn (Invalid_Sort_LTS x)
  let invalid_sort_type x = MEBI_exn (Invalid_Sort_Type x)

  let invalid_check_updated_ctx env sigma x y =
    MEBI_exn (InvalidCheckUpdatedCtx (env, sigma, x, y))
  ;;

  let mebi_handler : t -> string = function
    (* NOTE: *)
    | Invalid_Sort_LTS x -> "Invalid_Sort_LTS"
    | Invalid_Sort_Type x -> "Invalid_Sort_Type"
    (* NOTE: *)
    | InvalidCheckUpdatedCtx (env, sigma, x, y) ->
      Printf.sprintf
        "Invalid Args to check_updated_ctx. Should both be empty, or both have \
         some.\n\
         substls: %s.\n\
         ctx_tys: %s."
        (Utils.Strfy.list (feconstr env sigma) x)
        (Utils.Strfy.list (mebi_to_string econstr_rel_decl) y)
  ;;

  let _ =
    CErrors.register_handler (fun e ->
      match e with MEBI_exn e -> Some (Pp.str (mebi_handler e)) | _ -> None)
  ;;
end
