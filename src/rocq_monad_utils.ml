module Make
    (C : Rocq_context.SRocq_context)
    (E : Encoding.SEncoding)
     (* :
        S
        with module M.BiEnc.Enc = E
        and type M.BiEnc.Enc.t = E.t
        and module M.Tree.Enc = E
        and type M.Tree.Enc.t = E.t
        and type M.Tree.TreeNode.t = E.t * int *) =
struct
  (* module
     M
     (* : *)
     (* SRocq_monad with module BiEnc.Enc = E and type BiEnc.Enc.t = E.t *) =
     Make (C) (E) *)

  include Rocq_monad.Make (C) (E)
  (* module Enc : Encoding.SEncoding with type t = Enc.t = Enc *)
  (* M.Enc *)

  (* module
     BiEnc
     (* : Bi_encoding.S *)
     (* with module Enc = Enc and type Enc.t = Enc.t *) =
     M.BiEnc *)

  (* module F : Hashtbl.S with type key = EConstr.t = M.BiEnc.FwdMap *)
  (* module B : Hashtbl.S with type key = Enc.t = M.BiEnc.BckMap *)

  (* module
     Tree
     (* :
     Enc_tree.S
     with module Enc = BiEnc.Enc
     and type Enc.t = BiEnc.Enc.t
     and type TreeNode.t = Enc.t * int *) =
     M.Tree *)

  (* module Constructor = M.Constructor *)
  (* module Syntax : M.SYNTAX = M.Syntax *)

  (* type 'a mm = 'a M.mm *)
  (* type 'a in_wrapper = 'a M.in_wrapper *)
  (* type wrapper = M.wrapper *)

  (* let env () : Environ.env mm = M.get_env
     let sigma () : Evd.evar_map mm = M.get_sigma
     let run = M.run
     let return = M.return
     let iterate = M.iterate
     let state = M.state
     let sandbox = M.sandbox *)

  let fresh_evar (x : Rocq_utils.evar_source) : EConstr.t mm =
    state (fun env sigma -> Rocq_utils.get_next env sigma x)
  ;;

  let econstr_eq a b : bool mm =
    state (fun env sigma -> sigma, EConstr.eq_constr sigma a b)
  ;;

  let econstr_normalize (x : EConstr.t) : EConstr.t mm =
    let open Syntax in
    let$+ t env sigma = Reductionops.nf_all env sigma x in
    return t
  ;;

  let econstr_kind (x : EConstr.t) : Rocq_utils.econstr_kind mm =
    state (fun env sigma -> sigma, EConstr.kind sigma x)
  ;;

  let econstr_is_evar (x : EConstr.t) : bool mm =
    state (fun env sigma -> sigma, EConstr.isEvar sigma x)
  ;;

  let econstr_to_constr
        ?(abort_on_undefined_evars : bool = false)
        (x : EConstr.t)
    : Constr.t mm
    =
    state (fun env sigma -> sigma, Rocq_convert.econstr_to_constr sigma x)
  ;;

  let econstr_to_constr_opt (x : EConstr.t) : Constr.t option mm =
    state (fun env sigma -> sigma, Rocq_convert.econstr_to_constr_opt sigma x)
  ;;

  (* module type SStrfy = sig
     val mm : (Environ.env -> Evd.evar_map -> 'a -> string) -> 'a -> string
     val econstr : EConstr.t -> string
     val econstr_rel_decl : EConstr.rel_declaration -> string
     end *)

  module Strfy (* : SStrfy *) = struct
    (* let mm = fstring *)
    let econstr : EConstr.t -> string = fstring Rocq_utils.Strfy.econstr

    let econstr_rel_decl : EConstr.rel_declaration -> string =
      fstring Rocq_utils.Strfy.econstr_rel_decl
    ;;
  end

  module type SErrors = sig
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
        (* NOTE: *)
      | InvalidLTSArgsLength of int
      | InvalidLTSTermKind of Environ.env * Evd.evar_map * Constr.t

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

    (* NOTE: *)
    val invalid_lts_args_length : int -> exn
    val invalid_lts_term_kind : Environ.env -> Evd.evar_map -> Constr.t -> exn
  end

  module Errors : SErrors = struct
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
        (* NOTE: *)
      | InvalidLTSArgsLength of int
      | InvalidLTSTermKind of Environ.env * Evd.evar_map * Constr.t

    exception MEBI_exn of t

    let invalid_sort_lts x = MEBI_exn (Invalid_Sort_LTS x)
    let invalid_sort_type x = MEBI_exn (Invalid_Sort_Type x)

    let invalid_check_updated_ctx env sigma x y =
      MEBI_exn (InvalidCheckUpdatedCtx (env, sigma, x, y))
    ;;

    (** Assert args length == 3 in [Command.extract_args]. *)
    let invalid_lts_args_length i = MEBI_exn (InvalidLTSArgsLength i)

    (** Assert Constr.kind tm is App _ in [Command.extract_args]. *)
    let invalid_lts_term_kind env sigma x =
      MEBI_exn (InvalidLTSTermKind (env, sigma, x))
    ;;

    let mebi_handler : t -> string = function
      (* NOTE: *)
      | Invalid_Sort_LTS x -> "Invalid_Sort_LTS"
      | Invalid_Sort_Type x -> "Invalid_Sort_Type"
      (* NOTE: *)
      | InvalidCheckUpdatedCtx (env, sigma, x, y) ->
        Printf.sprintf
          "Invalid Args to check_updated_ctx. Should both be empty, or both \
           have some.\n\
           substls: %s.\n\
           ctx_tys: %s."
          (Utils.Strfy.list (Of Strfy.econstr) x)
          (Utils.Strfy.list (Of Strfy.econstr_rel_decl) y)
        (* NOTE: *)
      | InvalidLTSArgsLength i ->
        Printf.sprintf "assertion: Array.length args == 3 failed. Got %i" i
      | InvalidLTSTermKind (env, sigma, tm) ->
        Printf.sprintf
          "assertion: Constr.kind tm matches App _ failed. Got %s which \
           matches with: %s"
          (Rocq_utils.Strfy.constr env sigma tm)
          (Rocq_utils.Strfy.constr_kind env sigma tm)
    ;;

    let _ =
      CErrors.register_handler (fun e ->
        match e with MEBI_exn e -> Some (Pp.str (mebi_handler e)) | _ -> None)
    ;;
  end

  module type SErr = sig
    val invalid_check_updated_ctx
      :  EConstr.t list
      -> EConstr.rel_declaration list
      -> 'a mm

    val invalid_lts_args_length : int -> 'a
    val invalid_lts_term_kind : Constr.t -> 'a mm
  end

  module Err : SErr = struct
    let invalid_check_updated_ctx
          (substl : EConstr.t list)
          (ctxl : EConstr.rel_declaration list)
      : 'a mm
      =
      state (fun env sigma ->
        raise (Errors.invalid_check_updated_ctx env sigma substl ctxl))
    ;;

    let invalid_lts_args_length (x : int) : 'a =
      raise (Errors.invalid_lts_args_length x)
    ;;

    let invalid_lts_term_kind (x : Constr.t) : 'a =
      state (fun env sigma -> raise (Errors.invalid_lts_term_kind env sigma x))
    ;;
  end

  let mk_ctx_substl
        (acc : EConstr.Vars.substl)
        (xs : ('a, EConstr.t, 'b) Context.Rel.Declaration.pt list)
    : EConstr.Vars.substl mm
    =
    state (fun env sigma -> Rocq_utils.mk_ctx_substl env sigma acc xs)
  ;;

  (** [extract_args ?substl term] returns an [EConstr.t] triple of arguments of an inductively defined LTS, e.g., [term -> option action -> term -> Prop].
      @param ?substl
        is a list of substitutions applied to the terms prior to being returned.
      @param term
        must be of [Constr.kind] [App(fn, args)] (i.e., the application of some inductively defined LTS, e.g., [termLTS (tpar (tact (Send A) tend) (tact (Recv A) tend)) (Some A) (tpar tend tend)]).
      @return a triple of [lhs_term, action, rhs_term]. *)
  let extract_args ?(substl : EConstr.Vars.substl = []) (term : Constr.t)
    : Rocq_utils.constructor_args mm
    =
    try return (Rocq_utils.extract_args ~substl term) with
    | Rocq_utils.Rocq_utils_InvalidLtsArgLength x ->
      (* TODO: err *) Err.invalid_lts_args_length x
    | Rocq_utils.Rocq_utils_InvalidLtsTermKind x ->
      Err.invalid_lts_term_kind term
  ;;

  (*********************************************************)

  (* module Unification = Rocq_unification.Make (struct
     module Enc = Enc
     module F = F
     module B = B

     type 'a mm = 'a mm

     module Strfy = Strfy
     end) *)
end
