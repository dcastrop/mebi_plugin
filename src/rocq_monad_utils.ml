module Make (C : Rocq_context.SRocq_context) (E : Encoding.SEncoding) = struct
  include Rocq_monad.Make (C) (E)

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

  module Strfy = struct
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

  module Unification = struct
    module type SPair = sig
      type t =
        { to_check : EConstr.t
        ; acc : EConstr.t
        }

      val to_string : Environ.env -> Evd.evar_map -> t -> string

      val make
        :  Environ.env
        -> Evd.evar_map
        -> EConstr.t
        -> EConstr.t
        -> Evd.evar_map * t

      val unify : Environ.env -> Evd.evar_map -> t -> Evd.evar_map * bool
    end

    module Pair : SPair = struct
      (** [fst] is a term (e.g., destination) that we want to check unifies with [snd] (which we have already reached).
      @see Mebi_setup.unif_problem where [type unif_problem = {termL:EConstr.t;termR:EConstr.t}] *)
      type t =
        { to_check : EConstr.t
        ; acc : EConstr.t
        }

      let to_string
            (env : Environ.env)
            (sigma : Evd.evar_map)
            ({ to_check; acc } : t)
        : string
        =
        Utils.Strfy.record
          [ "to_check", Strfy.econstr to_check; "b", Strfy.econstr acc ]
      ;;

      let fresh
            (env : Environ.env)
            (sigma : Evd.evar_map)
            ({ to_check; acc } : t)
        : Evd.evar_map * t
        =
        let sigma, to_check = Rocq_utils.get_next env sigma (TypeOf to_check) in
        sigma, { to_check; acc }
      ;;

      let make
            (env : Environ.env)
            (sigma : Evd.evar_map)
            (to_check : EConstr.t)
            (acc : EConstr.t)
        : Evd.evar_map * t
        =
        if EConstr.isEvar sigma to_check
        then fresh env sigma { to_check; acc }
        else sigma, { to_check; acc }
      ;;

      let w_unify
            (env : Environ.env)
            (sigma : Evd.evar_map)
            ({ to_check; acc } : t)
        : Evd.evar_map * bool
        =
        try
          Unification.w_unify env sigma Conversion.CUMUL to_check acc, true
        with
        | Pretype_errors.PretypeError (_, _, CannotUnify (c, d, _e)) ->
          sigma, false
      ;;

      (** [unify a b] tries to unify [a] and [b] within the context of the [env] and [sigma] of [mm]. @returns [true] if successful, [false] otherwise. *)
      let unify = w_unify
    end

    module type SProblem = sig
      type t =
        { act : Pair.t
        ; goto : Pair.t
        ; tree : Tree.t
        }

      val to_string : Environ.env -> Evd.evar_map -> t -> string
      val unify_opt : t -> Tree.t option mm
    end

    module Problem : SProblem = struct
      (** if [fst] is sucessfully unified then [snd] represents a tree of constructors that lead to that term (from some previously visited term).
      *)
      type t =
        { act : Pair.t
        ; goto : Pair.t
        ; tree : Tree.t
        }

      let to_string
            (env : Environ.env)
            (sigma : Evd.evar_map)
            ({ act; goto; tree } : t)
        : string
        =
        Utils.Strfy.record
          [ "act", Pair.to_string env sigma act
          ; "goto", Pair.to_string env sigma goto
          ; "tree", Tree.to_string tree
          ]
      ;;

      let unify_pair_opt (pair : Pair.t) : bool mm =
        state (fun env sigma -> Pair.unify env sigma pair)
      ;;

      let unify_opt ({ act; goto; tree } : t) : Tree.t option mm =
        let open Syntax in
        let* unified_act_opt = unify_pair_opt act in
        let* unified_goto_opt = unify_pair_opt goto in
        match unified_act_opt, unified_goto_opt with
        | true, true -> return (Some tree)
        | _, _ -> return None
      ;;
    end

    module type SProblems = sig
      type t =
        { sigma : Evd.evar_map
        ; to_unify : Problem.t list
        }

      val empty : unit -> t mm
      val list_is_empty : t list -> bool
      val to_string : Environ.env -> t -> string
      val list_to_string : Environ.env -> t list -> string

      val sandbox_unify_all_opt
        :  Evd.econstr
        -> Evd.econstr
        -> t
        -> (Evd.econstr * Evd.econstr * Tree.t list) option mm
    end

    module Problems : SProblems = struct
      type t =
        { sigma : Evd.evar_map
        ; to_unify : Problem.t list
        }

      let empty () : t mm =
        let open Syntax in
        let* sigma = get_sigma in
        return { sigma; to_unify = [] }
      ;;

      let is_empty : t -> bool = function
        | { to_unify = []; _ } -> true
        | _ -> false
      ;;

      let list_is_empty : t list -> bool = function
        | [] -> true
        | [ p ] -> is_empty p
        | _ -> false
      ;;

      let to_string (env : Environ.env) ({ sigma; to_unify } : t) : string =
        Utils.Strfy.list
          ~args:
            { (Utils.Strfy.style_args ()) with
              style = Some (Utils.Strfy.list_style ())
            ; name = Some "unification problems"
            }
          (Of (Problem.to_string env sigma))
          to_unify
      ;;

      let list_to_string (env : Environ.env) : t list -> string =
        Utils.Strfy.list
          ~args:
            { (Utils.Strfy.style_args ()) with
              style = Some (Utils.Strfy.list_style ())
            ; name = Some "unification problems list"
            }
          (Of (to_string env))
      ;;

      let rec unify_list_opt : Problem.t list -> Tree.t list option mm =
        let open Syntax in
        function
        | [] -> return (Some [])
        | h :: tl ->
          let* success_opt = Problem.unify_opt h in
          (match success_opt with
           | None -> return None
           | Some constructor_tree ->
             let* unified_opt = unify_list_opt tl in
             (match unified_opt with
              | None -> return None
              | Some acc -> return (Some (constructor_tree :: acc))))
      ;;

      let sandbox_unify_all_opt
            (act : EConstr.t)
            (goto : EConstr.t)
            ({ sigma; to_unify } : t)
        : (EConstr.t * EConstr.t * Tree.t list) option mm
        =
        let open Syntax in
        sandbox
          ~sigma
          (let* unified_opt = unify_list_opt to_unify in
           match unified_opt with
           | None -> return None
           | Some constructor_trees ->
             let$+ act env sigma = Reductionops.nf_all env sigma act in
             let$+ goto env sigma = Reductionops.nf_all env sigma goto in
             let$+ is_act_undefined _ sigma = EConstr.isEvar sigma act in
             let$+ is_goto_undefined _ sigma = EConstr.isEvar sigma goto in
             if is_act_undefined && is_goto_undefined
             then return None
             else return (Some (act, goto, constructor_trees)))
      ;;
    end

    module type SConstructors = sig
      type t = Constructor.t list

      val to_string : Environ.env -> Evd.evar_map -> t -> string

      val retrieve
        :  int
        -> t
        -> Evd.econstr
        -> Evd.econstr
        -> Enc.t * Problems.t list
        -> t mm
    end

    module Constructors : SConstructors = struct
      type t = Constructor.t list

      let to_string (env : Environ.env) (sigma : Evd.evar_map) : t -> string =
        Utils.Strfy.list (Of (Constructor.to_string env sigma))
      ;;

      let rec retrieve
                (constructor_index : int)
                (acc : t)
                (act : EConstr.t)
                (tgt : EConstr.t)
        : Enc.t * Problems.t list -> t mm
        =
        let open Syntax in
        function
        | _, [] -> return acc
        | lts_enc, problems :: tl ->
          let* acc = retrieve constructor_index acc act tgt (lts_enc, tl) in
          let* constructor_opt : Constructor.t option =
            sandbox
              (let* success = Problems.sandbox_unify_all_opt act tgt problems in
               match success with
               | None -> return None
               | Some (act, goto, constructor_trees) ->
                 let tree : Tree.t =
                   Tree.Node ((lts_enc, constructor_index), constructor_trees)
                 in
                 let constructor : Constructor.t = act, goto, tree in
                 return (Some constructor))
          in
          (match constructor_opt with
           | None -> return acc
           | Some constructor -> return (constructor :: acc))
      ;;
    end

    (** creates unification problems between the rhs of the current constructor and the lhs of the next, along with the actions of both.
        (* NOTE: this is only relevant when deciding whether to explore a given constructor from a premise of another *)
    *)
    let constr_to_problem (args : Rocq_utils.constructor_args)
      : Constructor.t -> Problem.t
      = function
      | act, rhs, tree ->
        let act : Pair.t = { to_check = args.act; acc = act } in
        let goto : Pair.t = { to_check = args.rhs; acc = rhs } in
        { act; goto; tree }
    ;;

    let map_problems args (constructors : Constructors.t) : Problems.t mm =
      let open Syntax in
      let* sigma = get_sigma in
      let to_unify : Problem.t list =
        List.map (constr_to_problem args) constructors
      in
      let p : Problems.t = { sigma; to_unify } in
      return p
    ;;

    let cross_product (acc : Problems.t list) ({ sigma; to_unify } : Problems.t)
      : Problems.t list
      =
      List.concat_map
        (fun ({ to_unify = xs; _ } : Problems.t) : Problems.t list ->
          List.map
            (fun (y : Problem.t) : Problems.t -> { sigma; to_unify = y :: xs })
            to_unify)
        acc
    ;;

    (*********************************************************)

    let does_constructor_unify (to_check : EConstr.t) (acc : EConstr.t)
      : bool mm
      =
      state (fun env sigma -> Pair.unify env sigma { to_check; acc })
    ;;

    let check_constructor_args_unify
          (lhs : EConstr.t)
          (act : EConstr.t)
          (args : Rocq_utils.constructor_args)
      : bool mm
      =
      let open Syntax in
      let f = does_constructor_unify in
      let* lhs_unifies : bool = f args.lhs lhs in
      if lhs_unifies then f args.act act else return false
    ;;

    let axiom_constructor
          (act : EConstr.t)
          (tgt : EConstr.t)
          (constructor_index : Enc.t * int)
          (constructors : Constructors.t)
      : Constructors.t mm
      =
      let open Syntax in
      let* is_evar : bool = econstr_is_evar tgt in
      if is_evar
      then return constructors
      else (
        let tree : Tree.t = Tree.Node (constructor_index, []) in
        let axiom : Constructor.t = act, tgt, tree in
        return (axiom :: constructors))
    ;;

    (** Checks possible transitions for this term: *)
    let rec check_valid_constructors
              (constructors : Rocq_ind.LTS.constructor array)
              (indmap : Enc.t Rocq_ind.t F.t)
              (from_term : EConstr.t)
              (act_term : EConstr.t)
              (lts_enc : Enc.t)
      : Constructors.t mm
      =
      let open Syntax in
      let* from_term : EConstr.t = econstr_normalize from_term in
      (* let* () = debug_validconstrs_start from_term in *)
      let iter_body (i : int) (acc : Constructors.t) : Constructors.t mm =
        (* let* () = debug_validconstrs_iter_start i constructors in *)
        (* NOTE: extract args for constructor *)
        let { constructor = ctx, tm; _ } : Rocq_ind.LTS.constructor =
          constructors.(i)
        in
        let decls : Rocq_utils.econstr_decl list =
          Rocq_utils.get_econstr_decls ctx
        in
        let* substl = mk_ctx_substl [] (List.rev decls) in
        let* args : Rocq_utils.constructor_args = extract_args ~substl tm in
        (* NOTE: make fresh [act_term] to avoid conflicts with sibling constructors *)
        let* act_term : EConstr.t = fresh_evar (TypeOf act_term) in
        let* success = check_constructor_args_unify from_term act_term args in
        if success
        then (
          (* NOTE: replace [act] with the fresh [act_term] *)
          let fresh_args : Rocq_utils.constructor_args =
            { args with act = act_term }
          in
          explore_valid_constructor
            indmap
            from_term
            lts_enc
            fresh_args
            (i, acc)
            (substl, decls))
        else
          (* let* () = debug_validconstrs_iter_close i constructors in *)
          return acc
      in
      let* constructors =
        iterate 0 (Array.length constructors - 1) [] iter_body
      in
      (* let* () = debug_validconstrs_close from_term constructors in *)
      return constructors

    (** *)
    and explore_valid_constructor
          (indmap : Enc.t Rocq_ind.t F.t)
          (from_term : EConstr.t)
          (lts_enc : Enc.t)
          (args : Rocq_utils.constructor_args)
          ((i, constructors) : int * Constructors.t)
          ((substl, decls) : EConstr.Vars.substl * EConstr.rel_declaration list)
      : Constructors.t mm
      =
      let open Syntax in
      (* NOTE: unpack and normalize [act] and [tgt] from [args] *)
      let tgt : EConstr.t = EConstr.Vars.substl substl args.rhs in
      let* tgt : EConstr.t = econstr_normalize tgt in
      let* act : EConstr.t = econstr_normalize args.act in
      (* TODO: make fresh sigma from fresh act+tgt, and use that from this point onwards. then, during cross-product, make sure that it is used over the parents sigma stored within the unification problems during ctx *)
      (* let* constructors =
    sandbox *)
      let* empty_problems : Problems.t = Problems.empty () in
      let* next_constructor_problems : (Enc.t * Problems.t list) option =
        check_updated_ctx lts_enc [ empty_problems ] indmap (substl, decls)
      in
      check_for_next_constructors
        i
        act
        tgt
        constructors
        next_constructor_problems

    (* Should return a list of unification problems *)
    and check_updated_ctx
          (lts_enc : Enc.t)
          (acc : Problems.t list)
          (indmap : Enc.t Rocq_ind.t F.t)
      :  EConstr.Vars.substl * EConstr.rel_declaration list
      -> (Enc.t * Problems.t list) option mm
      = function
      | [], [] -> return (Some (lts_enc, acc))
      | _hsubstl :: substl, t :: tl ->
        let open Syntax in
        let ty_t : EConstr.t = Context.Rel.Declaration.get_type t in
        let$+ upd_t env sigma = EConstr.Vars.substl substl ty_t in
        let* env = get_env in
        let* sigma = get_sigma in
        (match EConstr.kind sigma upd_t with
         | App (name, args) ->
           (match F.find_opt indmap name with
            | None ->
              (* let* () = debug_updtcontext_close_app name in *)
              check_updated_ctx lts_enc acc indmap (substl, tl)
            | Some c ->
              let args : Rocq_utils.constructor_args =
                Rocq_utils.constructor_args args
              in
              let$+ lhs env sigma = Reductionops.nf_evar sigma args.lhs in
              let$+ act env sigma = Reductionops.nf_evar sigma args.act in
              let args = { args with lhs; act } in
              let next_lts : Rocq_ind.LTS.constructor array =
                Rocq_ind.get_lts_constructor_types c
              in
              let* next_constructors : Constructors.t =
                check_valid_constructors next_lts indmap lhs act c.enc
              in
              (match next_constructors with
               | [] -> return None
               | next_constructors ->
                 let* problems : Problems.t =
                   map_problems args next_constructors
                 in
                 let acc : Problems.t list = cross_product acc problems in
                 check_updated_ctx lts_enc acc indmap (substl, tl)))
         | _ -> check_updated_ctx lts_enc acc indmap (substl, tl))
      | _substl, _ctxl -> Err.invalid_check_updated_ctx _substl _ctxl
    (* ! Impossible ! *)
    (* FIXME: should fail if [t] is an evar -- but *NOT* if it contains evars! *)

    and check_for_next_constructors
          (i : int)
          (outer_act : EConstr.t)
          (tgt_term : EConstr.t)
          (constructors : Constructors.t)
      : (Enc.t * Problems.t list) option -> Constructors.t mm
      = function
      | None -> return constructors
      | Some (next_lts_enc, next_problems) ->
        if Problems.list_is_empty next_problems
        then axiom_constructor outer_act tgt_term (next_lts_enc, i) constructors
        else
          Constructors.retrieve
            i
            constructors
            outer_act
            tgt_term
            (next_lts_enc, next_problems)
    ;;

    let collect_valid_constructors
          (constructors : Rocq_ind.LTS.constructor array)
          (indmap : Enc.t Rocq_ind.t F.t)
          (from_term : EConstr.t)
          (label_type : EConstr.t)
          (lts_enc : Enc.t)
      : Constructors.t mm
      =
      let open Syntax in
      let* fresh_evar = fresh_evar (OfType label_type) in
      let* constructors : Constructors.t =
        check_valid_constructors
          constructors
          indmap
          from_term
          fresh_evar
          lts_enc
      in
      return constructors
    ;;
  end
end
