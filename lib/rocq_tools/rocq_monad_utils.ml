module type S = sig
  type tree

  include Rocq_monad.S

  val fresh_evar : Rocq_utils.evar_source -> EConstr.t mm
  val econstr_eq : ?enc:bool -> EConstr.t -> EConstr.t -> bool mm
  val econstr_compare : EConstr.t -> EConstr.t -> int
  val get_encoding : EConstr.t -> enc
  val econstr_kind : EConstr.t -> Rocq_utils.econstr_kind mm
  val econstr_is_evar : EConstr.t -> bool mm

  val econstr_to_constr
    :  ?abort_on_undefined_evars:bool
    -> EConstr.t
    -> Constr.t mm

  val econstr_to_constr_opt : EConstr.t -> Constr.t option mm
  val constrexpr_to_econstr : Constrexpr.constr_expr -> EConstr.t mm
  val to_atomic : EConstr.t -> EConstr.t Rocq_utils.kind_pair mm
  val to_lambda : EConstr.t -> Rocq_utils.lambda_triple mm
  val to_app : EConstr.t -> EConstr.t Rocq_utils.kind_pair mm
  val exists_eq : EConstr.t -> 'a list -> ('a -> EConstr.t) -> bool mm
  val type_of_econstr : EConstr.t -> EConstr.t mm
  val type_of_constrexpr : Constrexpr.constr_expr -> EConstr.t mm

  module Strfy : Rocq_monad_strfy.S

  val log_econstr
    :  ?__FUNCTION__:string
    -> ?m:Output.Kind.t
    -> ?s:string
    -> EConstr.t
    -> unit

  val log_econstrs
    :  ?__FUNCTION__:string
    -> ?m:Output.Kind.t
    -> ?s:string
    -> EConstr.t list
    -> unit

  val log_constr
    :  ?__FUNCTION__:string
    -> ?m:Output.Kind.t
    -> ?s:string
    -> Constr.t
    -> unit

  val log_constrs
    :  ?__FUNCTION__:string
    -> ?m:Output.Kind.t
    -> ?s:string
    -> Constr.t list
    -> unit

  module type SErrors = sig
    type t =
      | LTS_Empty
      | LTS_Incomplete of string
      | Saturation_Too_Large of string
      | Not_Bisimilar
      | Invalid_Ind_Kind_Type of EConstr.t option
      | Invalid_Sort_LTS of Sorts.Quality.t
      | Invalid_Sort_Type of Sorts.Quality.t
      | Invalid_Ref_LTS of Names.GlobRef.t
      | Invalid_Ref_Type of Names.GlobRef.t
      | Invalid_Arity of (Environ.env * Evd.evar_map * Constr.t)
      | InvalidCheckUpdatedCtx of
          (Environ.env
          * Evd.evar_map
          * EConstr.t list
          * EConstr.rel_declaration list)
      | InvalidLTSArgsLength of int
      | InvalidLTSTermKind of Environ.env * Evd.evar_map * Constr.t

    exception MEBI_exn of t

    val lts_empty : unit -> exn
    val lts_incomplete : string -> exn
    val saturation_too_large : string -> exn
    val not_bisimilar : unit -> exn
    val invalid_ind_kind_type : EConstr.t option -> exn
    val invalid_sort_lts : Sorts.Quality.t -> exn
    val invalid_sort_type : Sorts.Quality.t -> exn
    val invalid_ref_lts : Names.GlobRef.t -> exn
    val invalid_ref_type : Names.GlobRef.t -> exn
    val invalid_arity : Environ.env -> Evd.evar_map -> Constr.t -> exn

    val invalid_check_updated_ctx
      :  Environ.env
      -> Evd.evar_map
      -> EConstr.t list
      -> EConstr.rel_declaration list
      -> exn

    val invalid_lts_args_length : int -> exn
    val invalid_lts_term_kind : Environ.env -> Evd.evar_map -> Constr.t -> exn
  end

  module Errors : SErrors

  module type SErr = sig
    val lts_empty : unit -> 'a
    val lts_incomplete : string -> 'a
    val saturation_too_large : string -> 'a
    val not_bisimilar : unit -> 'a
    val invalid_ind_kind_type : EConstr.t option -> 'a
    val invalid_sort_lts : Sorts.Quality.t -> 'a
    val invalid_sort_type : Sorts.Quality.t -> 'a
    val invalid_ref_lts : Names.GlobRef.t -> 'a
    val invalid_ref_type : Names.GlobRef.t -> 'a
    val invalid_arity : Constr.t -> 'a mm

    val invalid_check_updated_ctx
      :  EConstr.t list
      -> EConstr.rel_declaration list
      -> 'a mm

    val invalid_lts_args_length : int -> 'a
    val invalid_lts_term_kind : Constr.t -> 'a mm
  end

  module Err : SErr

  module Ind : sig
    module LTS : sig
      type t =
        { term_type : EConstr.t
        ; label_type : EConstr.t
        ; constructor_types : constructor array
        }

      and constructor =
        { name : Names.Id.t
        ; constructor : Rocq_utils.ind_constr
        }

      include Json.S with type k = t
    end

    type t =
      { enc : enc
      ; ind : EConstr.t
      ; kind : kind
      }

    and kind =
      | Type of EConstr.t option
      | LTS of LTS.t

    include Json.S with type k = t

    val get_lts : t -> LTS.t
    val get_lts_term_type : t -> EConstr.t
    val get_lts_label_type : t -> EConstr.t
    val get_lts_constructor_types : t -> LTS.constructor array
    val lookup : Names.inductive -> Declarations.mind_specif mm
    val get_lts_constructor_names : t -> Names.Id.t array
    val get_lts_constructors : t -> Rocq_utils.ind_constr array

    val assert_mip_arity_is_type_or_set
      :  Declarations.one_inductive_body
      -> unit mm

    val assert_mip_arity_is_prop : Declarations.one_inductive_body -> unit mm

    val lts_mind
      :  Names.GlobRef.t
      -> (Names.inductive * Declarations.mind_specif) mm

    val lts_type_mind
      :  Names.GlobRef.t
      -> (Names.inductive * Declarations.mind_specif) mm

    val lts_prop_mind
      :  Names.GlobRef.t
      -> (Names.inductive * Declarations.mind_specif) mm

    val lts_labels_and_terms
      :  Declarations.mind_specif
      -> (Constr.rel_declaration * Constr.rel_declaration) mm

    val lts : Names.GlobRef.t -> t mm
  end

  val mk_ctx_substl
    :  EConstr.Vars.substl
    -> ('a, EConstr.t, 'b) Context.Rel.Declaration.pt list
    -> EConstr.Vars.substl mm

  val extract_args
    :  ?substl:EConstr.Vars.substl
    -> Constr.t
    -> Rocq_utils.constructor_args mm

  module Constructor : sig
    type t = enc * enc * tree

    include Json.S with type k = t

    val encode : EConstr.t -> EConstr.t -> tree -> t
  end

  val make_state_tree_pair_set
    :  unit
    -> (module Set.S with type elt = enc * tree)

  module Unification : sig
    module Pair : sig
      type t =
        { to_check : EConstr.t
        ; acc : EConstr.t
        }

      include Json.S with type k = t

      val fresh : Environ.env -> Evd.evar_map -> t -> Evd.evar_map * t

      val make
        :  Environ.env
        -> Evd.evar_map
        -> EConstr.t
        -> EConstr.t
        -> Evd.evar_map * t

      val unify : Environ.env -> Evd.evar_map -> t -> Evd.evar_map * bool
      val unifies : EConstr.t -> EConstr.t -> bool mm
    end

    module Problem : sig
      type t =
        { act : Pair.t
        ; goto : Pair.t
        ; tree : tree
        }

      include Json.S with type k = t

      val unify_pair_opt : Pair.t -> bool mm
      val unify_opt : t -> tree option mm
      val of_constructor : Rocq_utils.constructor_args -> Constructor.t -> t
    end

    module Problems : sig
      (** An equation premise not decidable when its constructor was matched:
          the LTS it belongs to, and its head and arguments. *)
      type deferred = enc * EConstr.t * EConstr.t array

      type t =
        { sigma : Evd.evar_map
        ; to_unify : Problem.t list
        ; deferred : deferred list
        }

      include Json.S with type k = t

      val empty : unit -> t mm
      val is_empty : t -> bool
      val unify_list_opt : Problem.t list -> tree list option mm

      val sandbox_unify_all
        :  enc
        -> EConstr.t
        -> EConstr.t
        -> t
        -> (EConstr.t * EConstr.t * tree list) list mm
    end

    module ListOfProblems : sig
      type t = Problems.t list

      include Json.S with type k = t

      val is_empty : t -> bool
      val cross_product : Problems.t -> t -> t
    end

    module Constructors : sig
      type t = Constructor.t list

      include Json.S with type k = t

      val retrieve
        :  int
        -> t
        -> EConstr.t
        -> EConstr.t
        -> enc * ListOfProblems.t
        -> t mm

      val to_problems : Rocq_utils.constructor_args -> t -> Problems.t mm
      val axiom : EConstr.t -> EConstr.t -> enc * int -> t -> t mm
    end

    val check_constructor_args_unify
      :  EConstr.t
      -> EConstr.t
      -> Rocq_utils.constructor_args
      -> bool mm

    val check_valid_constructors
      :  Ind.LTS.constructor array
      -> Ind.t F.t
      -> EConstr.t
      -> EConstr.t
      -> enc
      -> Constructors.t mm

    val explore_valid_constructor
      :  Ind.t F.t
      -> EConstr.t
      -> enc
      -> Rocq_utils.constructor_args
      -> int * Constructors.t
      -> EConstr.Vars.substl * EConstr.rel_declaration list
      -> Constructors.t mm

    val check_updated_ctx
      :  enc
      -> ListOfProblems.t
      -> Ind.t F.t
      -> EConstr.Vars.substl * EConstr.rel_declaration list
      -> (enc * ListOfProblems.t) option mm

    val check_for_next_constructors
      :  int
      -> EConstr.t
      -> EConstr.t
      -> Constructors.t
      -> (enc * ListOfProblems.t) option
      -> Constructors.t mm

    val collect_valid_constructors
      :  Ind.LTS.constructor array
      -> Ind.t F.t
      -> EConstr.t
      -> EConstr.t
      -> enc
      -> Constructors.t mm
  end

  val make_enc_hashtbl : unit -> (module Hashtbl.S with type key = enc)
  val make_enc_set : unit -> (module Set.S with type elt = enc)
  val make_econstr_set : unit -> (module Set.S with type elt = EConstr.t)
end

(** What made the LTS being extracted approximate: a premise left undecided
    (the LTS may contain transitions that do not exist), or one whose
    solutions may be incomplete, or a transition MeBi could not determine
    (it may be missing some). Each warning is printed once per LTS and
    session, but noted here every time, so an extraction knows it is
    approximate even when the warning was printed by an earlier command.
    [Wrapper.extract_lts] resets it and marks such an LTS incomplete
    (2026-10-03). *)
module Approximations = struct
  let notes : string list ref = ref []
  let reset () : unit = notes := []

  let note (s : string) : unit =
    if Bool.not (List.mem s !notes) then notes := !notes @ [ s ]
  ;;

  let get () : string list = !notes
end

module Make (Enc : Encoding.S) :
  S with type enc = Enc.t and type tree = Enc.Tree.t = struct
  (*****************************************)

  module M = Rocq_monad.Make (Enc)
  include M

  type tree = Enc.Tree.t

  (* See the [.mli]. *)
  let fresh_evar (x : Rocq_utils.evar_source) : EConstr.t mm =
    Logger.trace __FUNCTION__;
    state (fun env sigma -> Rocq_utils.get_next env sigma x)
  ;;

  (* See the [.mli]. *)
  let econstr_eq ?(enc : bool = true) (a : EConstr.t) (b : EConstr.t) : bool mm =
    Logger.trace __FUNCTION__;
    if enc
    then (
      let a = encode a in
      let b = encode b in
      Enc.equal a b |> return)
    else
      let open Syntax in
      let* sigma = get_sigma in
      EConstr.eq_constr sigma a b |> return
  ;;

  (* See the [.mli]. *)
  let econstr_compare (a : EConstr.t) (b : EConstr.t) : int =
    let a = encode a in
    let b = encode b in
    Enc.compare a b
  ;;

  (* See the [.mli]. *)
  let get_encoding (x : EConstr.t) : Enc.t =
    Logger.trace __FUNCTION__;
    run
      (let open Syntax in
       let* x : EConstr.t = econstr_normalize x in
       get_encoding x |> return)
  ;;

  (* See the [.mli]. *)
  let econstr_kind (x : EConstr.t) : Rocq_utils.econstr_kind mm =
    Logger.trace __FUNCTION__;
    let open Syntax in
    let* sigma = get_sigma in
    let* x : EConstr.t = econstr_normalize x in
    EConstr.kind sigma x |> return
  ;;

  (* See the [.mli]. *)
  let econstr_is_evar (x : EConstr.t) : bool mm =
    Logger.trace __FUNCTION__;
    state (fun env sigma -> sigma, EConstr.isEvar sigma x)
  ;;

  (* See the [.mli]: [abort_on_undefined_evars] is not passed on. *)
  let econstr_to_constr
        ?(abort_on_undefined_evars : bool = false)
        (x : EConstr.t)
    : Constr.t mm
    =
    Logger.trace __FUNCTION__;
    state (fun env sigma -> sigma, Rocq_utils.econstr_to_constr sigma x)
  ;;

  (* See the [.mli]. *)
  let econstr_to_constr_opt (x : EConstr.t) : Constr.t option mm =
    Logger.trace __FUNCTION__;
    state (fun env sigma -> sigma, Rocq_utils.econstr_to_constr_opt sigma x)
  ;;

  (* See the [.mli]. *)
  let constrexpr_to_econstr (x : Constrexpr.constr_expr) : EConstr.t mm =
    Logger.trace __FUNCTION__;
    state (fun env sigma -> Rocq_utils.constrexpr_to_econstr env sigma x)
  ;;

  (* See the [.mli] for [to_atomic], [to_lambda] and [to_app]. *)
  let to_atomic (x : EConstr.t) : EConstr.t Rocq_utils.kind_pair mm =
    let open Syntax in
    let* sigma = get_sigma in
    Rocq_utils.econstr_to_atomic sigma x |> return
  ;;

  let to_lambda (x : EConstr.t) : Rocq_utils.lambda_triple mm =
    let open Syntax in
    let* sigma = get_sigma in
    Rocq_utils.econstr_to_lambda sigma x |> return
  ;;

  let to_app (x : EConstr.t) : EConstr.t Rocq_utils.kind_pair mm =
    let open Syntax in
    let* sigma = get_sigma in
    Rocq_utils.econstr_to_app sigma x |> return
  ;;

  (* See the [.mli]. One comparison per element, in order. *)
  let exists_eq (x : EConstr.t) (ys : 'a list) (decoder : 'a -> EConstr.t)
    : bool mm
    =
    Logger.trace __FUNCTION__;
    (* List.exists (fun y -> decoder y |> econstr_eq x) ys *)
    let open Syntax in
    let f (i : int) (a : bool) =
      let y : EConstr.t = List.nth ys i |> decoder in
      let* b : bool = econstr_eq x y in
      (a || b) |> return
    in
    iterate 0 (List.length ys - 1) false f
  ;;

  (* See the [.mli]. *)
  let type_of_econstr (x : EConstr.t) : EConstr.t mm =
    (* Logger.trace __FUNCTION__; *)
    let open Syntax in
    let* t : EConstr.t = econstr_normalize x in
    state (fun env sigma -> Typing.type_of env sigma t)
  ;;

  (* See the [.mli]. *)
  let type_of_constrexpr (x : Constrexpr.constr_expr) : EConstr.t mm =
    (* Logger.trace __FUNCTION__; *)
    let open Syntax in
    let* t : EConstr.t = constrexpr_to_econstr x in
    type_of_econstr t
  ;;

  (*********************************************************)

  module Strfy = Rocq_monad_strfy.Make (M)

  (* See the [.mli] for the four loggers. *)
  let log_econstr
        ?(__FUNCTION__ : string = "")
        ?(m : Output.Kind.t = Output.Kind.Debug)
        ?(s : string = "EConstr")
        (x : EConstr.t)
    : unit
    =
    Logger.thing ~__FUNCTION__ m s x Strfy.econstr
  ;;

  let log_econstrs
        ?(__FUNCTION__ : string = "")
        ?(m : Output.Kind.t = Output.Kind.Debug)
        ?(s : string = "EConstrs")
        (x : EConstr.t list)
    : unit
    =
    Logger.things ~__FUNCTION__ m s x Strfy.econstr
  ;;

  let log_constr
        ?(__FUNCTION__ : string = "")
        ?(m : Output.Kind.t = Output.Kind.Debug)
        ?(s : string = "Constr")
        (x : Constr.t)
    : unit
    =
    Logger.thing ~__FUNCTION__ m s x Strfy.constr
  ;;

  let log_constrs
        ?(__FUNCTION__ : string = "")
        ?(m : Output.Kind.t = Output.Kind.Debug)
        ?(s : string = "Constrs")
        (x : Constr.t list)
    : unit
    =
    Logger.things ~__FUNCTION__ m s x Strfy.constr
  ;;

  module type SErrors = sig
    type t =
      (* NOTE: *)
      | LTS_Empty
      | LTS_Incomplete of string
      | Saturation_Too_Large of string
      | Not_Bisimilar
      (* NOTE: *)
      | Invalid_Ind_Kind_Type of EConstr.t option
      (* NOTE: *)
      | Invalid_Sort_LTS of Sorts.Quality.t
      | Invalid_Sort_Type of Sorts.Quality.t
      (* NOTE: *)
      | Invalid_Ref_LTS of Names.GlobRef.t
      | Invalid_Ref_Type of Names.GlobRef.t
      (* NOTE: *)
      | Invalid_Arity of (Environ.env * Evd.evar_map * Constr.types)
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
    val lts_empty : unit -> exn
    val lts_incomplete : string -> exn
    val saturation_too_large : string -> exn
    val not_bisimilar : unit -> exn

    (* NOTE: *)
    val invalid_ind_kind_type : EConstr.t option -> exn

    (* NOTE: *)
    val invalid_sort_lts : Sorts.Quality.t -> exn
    val invalid_sort_type : Sorts.Quality.t -> exn

    (* NOTE: *)
    val invalid_ref_lts : Names.GlobRef.t -> exn
    val invalid_ref_type : Names.GlobRef.t -> exn

    (* NOTE: *)
    val invalid_arity : Environ.env -> Evd.evar_map -> Constr.types -> exn

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

  (* See the [.mli]. *)
  module Errors : SErrors = struct
    type t =
      (* NOTE: *)
      | LTS_Empty
      | LTS_Incomplete of string
      | Saturation_Too_Large of string
      | Not_Bisimilar
      (* NOTE: *)
      | Invalid_Ind_Kind_Type of EConstr.t option
      (* NOTE: *)
      | Invalid_Sort_LTS of Sorts.Quality.t
      | Invalid_Sort_Type of Sorts.Quality.t
      (* NOTE: *)
      | Invalid_Ref_LTS of Names.GlobRef.t
      | Invalid_Ref_Type of Names.GlobRef.t
      (* NOTE: *)
      | Invalid_Arity of (Environ.env * Evd.evar_map * Constr.types)
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

    let lts_empty () = MEBI_exn LTS_Empty
    let lts_incomplete (msg : string) = MEBI_exn (LTS_Incomplete msg)

    let saturation_too_large (msg : string) =
      MEBI_exn (Saturation_Too_Large msg)
    ;;

    let not_bisimilar () = MEBI_exn Not_Bisimilar

    let invalid_ind_kind_type (x : EConstr.t option) =
      MEBI_exn (Invalid_Ind_Kind_Type x)
    ;;

    let invalid_sort_lts x = MEBI_exn (Invalid_Sort_LTS x)
    let invalid_sort_type x = MEBI_exn (Invalid_Sort_Type x)

    let invalid_ref_lts (x : Names.GlobRef.t) : 'a =
      MEBI_exn (Invalid_Ref_LTS x)
    ;;

    let invalid_ref_type (x : Names.GlobRef.t) : 'a =
      MEBI_exn (Invalid_Ref_Type x)
    ;;

    let invalid_arity env sigma x = MEBI_exn (Invalid_Arity (env, sigma, x))

    let invalid_check_updated_ctx env sigma x y =
      MEBI_exn (InvalidCheckUpdatedCtx (env, sigma, x, y))
    ;;

    (** Assert args length == 3 in [Command.extract_args]. *)
    let invalid_lts_args_length i = MEBI_exn (InvalidLTSArgsLength i)

    (** Assert Constr.kind tm is App _ in [Command.extract_args]. *)
    let invalid_lts_term_kind env sigma x =
      MEBI_exn (InvalidLTSTermKind (env, sigma, x))
    ;;

    (** [mebi_handler e] is the message a user sees for the error [e]
        (registered with Rocq below). Raises nothing. *)
    let mebi_handler : t -> string = function
      (* NOTE: *)
      | LTS_Empty -> "LTS_Empty"
      | LTS_Incomplete x -> Printf.sprintf "LTS_Incomplete: %s" x
      | Saturation_Too_Large x -> Printf.sprintf "Saturation_Too_Large: %s" x
      | Not_Bisimilar -> "Not_Bisimilar"
      (* NOTE: *)
      | Invalid_Ind_Kind_Type x -> "Invalid_Ind_Kind (Type, expected LTS)"
      (* NOTE: *)
      | Invalid_Sort_LTS x -> "Invalid_Sort_LTS"
      | Invalid_Sort_Type x -> "Invalid_Sort_Type"
      (* NOTE: *)
      | Invalid_Ref_LTS x -> "Invalid_Ref_LTS"
      | Invalid_Ref_Type x -> "Invalid_Ref_Type"
      (* NOTE: *)
      | Invalid_Arity (env, sigma, x) ->
        Printf.sprintf "Invalid_Arity: %s" (Rocq_utils.Strfy.constr env sigma x)
      (* NOTE: *)
      | InvalidCheckUpdatedCtx (env, sigma, x, y) ->
        Printf.sprintf
          "Invalid Args to check_updated_ctx. Should both be empty, or both \
           have some.\n\
           substls: %s.\n\
           ctx_tys: %s."
          (* (Utils.Strfy.list Strfy.econstr x) *)
          (* (Utils.Strfy.list Strfy.econstr_rel_decl y) *)
          "TODO: substls"
          "TODO: ctx_tys"
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
    (* NOTE: *)
    val lts_empty : unit -> 'a
    val lts_incomplete : string -> 'a
    val saturation_too_large : string -> 'a
    val not_bisimilar : unit -> 'a

    (* NOTE: *)
    val invalid_ind_kind_type : EConstr.t option -> 'a

    (* NOTE: *)
    val invalid_sort_lts : Sorts.Quality.t -> 'a
    val invalid_sort_type : Sorts.Quality.t -> 'a

    (* NOTE: *)
    val invalid_ref_lts : Names.GlobRef.t -> 'a
    val invalid_ref_type : Names.GlobRef.t -> 'a

    (* NOTE: *)
    val invalid_arity : Constr.types -> 'a mm

    (* NOTE: *)
    val invalid_check_updated_ctx
      :  EConstr.t list
      -> EConstr.rel_declaration list
      -> 'a mm

    (* NOTE: *)
    val invalid_lts_args_length : int -> 'a
    val invalid_lts_term_kind : Constr.t -> 'a mm
  end

  (* See the [.mli]. *)
  module Err : SErr = struct
    let lts_empty () = raise (Errors.lts_empty ())
    let lts_incomplete (msg : string) = raise (Errors.lts_incomplete msg)

    let saturation_too_large (msg : string) =
      raise (Errors.saturation_too_large msg)
    ;;

    let not_bisimilar () = raise (Errors.not_bisimilar ())

    (* NOTE: *)
    let invalid_ind_kind_type (x : EConstr.t option) : 'a =
      raise (Errors.invalid_ind_kind_type x)
    ;;

    let invalid_sort_lts (x : Sorts.Quality.t) : 'a =
      raise (Errors.invalid_sort_lts x)
    ;;

    let invalid_sort_type (x : Sorts.Quality.t) : 'a =
      raise (Errors.invalid_sort_type x)
    ;;

    let invalid_ref_lts (x : Names.GlobRef.t) : 'a =
      raise (Errors.invalid_ref_lts x)
    ;;

    let invalid_ref_type (x : Names.GlobRef.t) : 'a =
      raise (Errors.invalid_ref_type x)
    ;;

    let invalid_arity (x : Constr.types) : 'a mm =
      state (fun env sigma -> raise (Errors.invalid_arity env sigma x))
    ;;

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

  (*********************************************************)

  module Ind = struct
    module LTS = struct
      type t =
        { term_type : EConstr.t
        ; label_type : EConstr.t
        ; constructor_types : constructor array
        }

      and constructor =
        { name : Names.Id.t
        ; constructor : Rocq_utils.ind_constr
        }

      include Json.Thing.Make (struct
          type k = t

          let name = "LTS"

          let json ?(as_elt : bool = false) (x : t) : Yojson.t =
            `Assoc
              [ "term type", `String (Strfy.econstr x.term_type)
              ; "label type", `String (Strfy.econstr x.label_type)
              ; ( "constructor types"
                , `List
                    (x.constructor_types
                     |> Array.map (fun (y : constructor) ->
                       `Assoc
                         [ "name", `String (Rocq_utils.Strfy.name_id y.name)
                         ; ( "constructor"
                           , `String
                               (fstring
                                  Rocq_utils.Strfy.ind_constr
                                  y.constructor) )
                         ])
                     |> Array.to_list) )
              ]
          ;;
        end)
    end

    type t =
      { enc : Enc.t
      ; ind : EConstr.t
      ; kind : kind
      }

    and kind =
      | Type of EConstr.t option
      | LTS of LTS.t

    include Json.Thing.Make (struct
        type k = t

        let name = "Ind"

        let json ?(as_elt : bool = false) (x : t) : Yojson.t =
          `Assoc
            [ "enc", Enc.json ~as_elt:true x.enc
            ; "ind", `String (Strfy.econstr x.ind)
            ; ( "kind"
              , match x.kind with
                | Type None -> `Null
                | Type (Some x) -> `String (Strfy.econstr x)
                | LTS x -> LTS.json ~as_elt:true x )
            ]
        ;;
      end)

    (* See the [.mli] for [get_lts] and the [get_lts_*] accessors. *)
    let get_lts : t -> LTS.t = function
      | { kind = LTS x; _ } -> x
      | { kind = Type x; _ } -> Err.invalid_ind_kind_type x
    ;;

    let get_lts_term_type (x : t) : EConstr.t = (get_lts x).term_type
    let get_lts_label_type (x : t) : EConstr.t = (get_lts x).label_type

    let get_lts_constructor_types (x : t) : LTS.constructor array =
      (get_lts x).constructor_types
    ;;

    let get_lts_constructor_names (x : t) : Names.Id.t array =
      Logger.trace __FUNCTION__;
      get_lts_constructor_types x
      |> Array.map (fun ({ name; _ } : LTS.constructor) -> name)
    ;;

    let get_lts_constructors (x : t) : Rocq_utils.ind_constr array =
      Logger.trace __FUNCTION__;
      get_lts_constructor_types x
      |> Array.map (fun ({ constructor; _ } : LTS.constructor) -> constructor)
    ;;

    (* See the [.mli]. *)
    let lookup (x : Names.inductive) : Declarations.mind_specif mm =
      (* Logger.trace __FUNCTION__; *)
      let open Syntax in
      let* env = get_env in
      Inductive.lookup_mind_specif env x |> return
    ;;

    (* See the [.mli]. *)
    let assert_mip_arity_is_type_or_set (mip : Declarations.one_inductive_body)
      : unit mm
      =
      (* Logger.trace __FUNCTION__; *)
      match mip.mind_sort with
      | Type _ -> return ()
      | Set -> return ()
      | _ -> Err.invalid_sort_type (Sorts.quality mip.mind_sort)
    ;;

    (* See the [.mli]. *)
    let assert_mip_arity_is_prop (mip : Declarations.one_inductive_body)
      : unit mm
      =
      (* Logger.trace __FUNCTION__; *)
      match mip.mind_sort with
      | Prop -> return ()
      | _ -> Err.invalid_sort_lts (Sorts.quality mip.mind_sort)
    ;;

    (* See the [.mli]. *)
    let lts_mind
      : Names.GlobRef.t -> (Names.inductive * Declarations.mind_specif) mm
      =
      (* Logger.trace __FUNCTION__; *)
      function
      | Names.GlobRef.IndRef ind ->
        let open Syntax in
        let* (mib, mip) : Declarations.mind_specif = lookup ind in
        (ind, (mib, mip)) |> return
      | x -> Err.invalid_ref_lts x
    ;;

    (* See the [.mli]. *)
    let lts_type_mind (x : Names.GlobRef.t)
      : (Names.inductive * Declarations.mind_specif) mm
      =
      (* Logger.trace __FUNCTION__; *)
      let open Syntax in
      let* ind, (mib, mip) = lts_mind x in
      let* () = assert_mip_arity_is_type_or_set mip in
      (ind, (mib, mip)) |> return
    ;;

    (* See the [.mli]. *)
    let lts_prop_mind (x : Names.GlobRef.t)
      : (Names.inductive * Declarations.mind_specif) mm
      =
      (* Logger.trace __FUNCTION__; *)
      let open Syntax in
      let* ind, (mib, mip) = lts_mind x in
      let* () = assert_mip_arity_is_prop mip in
      (ind, (mib, mip)) |> return
    ;;

    (* See the [.mli]. *)
    let lts_labels_and_terms ((mib, mip) : Declarations.mind_specif)
      : (Constr.rel_declaration * Constr.rel_declaration) mm
      =
      (* Logger.trace __FUNCTION__; *)
      (* NOTE: get the type of [mip] from [mib]. *)
      let typ = Inductive.type_of_inductive (UVars.in_punivs (mib, mip)) in
      match mip.mind_arity_ctxt |> Utils.split_at mip.mind_nrealdecls with
      | [ t1; a; t2 ] ->
        let open Context.Rel in
        if Declaration.equal Sorts.relevance_equal Constr.equal t1 t2
        then return (a, t1)
        else Err.invalid_arity typ
      | _ -> Err.invalid_arity typ
    ;;

    (** Raised by {!mip_to_lts_constructors}. *)
    exception Mip_InconsistentNumConstructors of Declarations.one_inductive_body

    (** [mip_to_lts_constructors mip] is the constructors of the inductive
        [mip], each with its name, in order.

        @raise Mip_InconsistentNumConstructors
          if [mip] has different numbers of constructor names and types
          (raised here). *)
    let mip_to_lts_constructors (mip : Declarations.one_inductive_body)
      : LTS.constructor array
      =
      Logger.trace __FUNCTION__;
      try
        Array.combine mip.mind_consnames mip.mind_nf_lc
        |> Array.fold_left
             (fun (acc : LTS.constructor list)
               ((name, constructor) : Names.Id.t * Rocq_utils.ind_constr) ->
               { name; constructor } :: acc)
             []
        |> List.rev
        |> Array.of_list
      with
      | Invalid_argument _ -> raise (Mip_InconsistentNumConstructors mip)
    ;;

    (* See the [.mli]. *)
    let lts (x : Names.GlobRef.t) : t mm =
      (* Logger.trace __FUNCTION__; *)
      let open Syntax in
      let* ind, (mib, mip) = lts_prop_mind x in
      let* label, term = lts_labels_and_terms (mib, mip) in
      let name : EConstr.t = Rocq_utils.get_ind_ty ind mib in
      let enc : Enc.t = encode name in
      { enc
      ; ind = name
      ; kind =
          LTS
            { term_type = Rocq_utils.get_decl_type_of_constr term
            ; label_type = Rocq_utils.get_decl_type_of_constr label
            ; constructor_types = mip_to_lts_constructors mip
            }
      }
      |> return
    ;;
  end

  (* See the [.mli]. *)
  let mk_ctx_substl
        (acc : EConstr.Vars.substl)
        (xs : ('a, EConstr.t, 'b) Context.Rel.Declaration.pt list)
    : EConstr.Vars.substl mm
    =
    state (fun env sigma -> Rocq_utils.mk_ctx_substl env sigma acc xs)
  ;;

  (* See the [.mli]. *)
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

  module Constructor : sig
    include module type of Enc.Constructor_tree

    val encode : EConstr.t -> EConstr.t -> Enc.Tree.t -> t
  end = struct
    include Enc.Constructor_tree

    let encode (act : EConstr.t) (goto : EConstr.t) (tree : Enc.Tree.t) : t =
      Logger.trace __FUNCTION__;
      let act : Enc.t = encode act in
      let goto : Enc.t = encode goto in
      act, goto, tree
    ;;
  end

  (* See the [.mli]. *)
  let make_state_tree_pair_set ()
    : (module Set.S with type elt = Enc.t * Enc.Tree.t)
    =
    Logger.trace __FUNCTION__;
    (module Set.Make (struct
         type t = Enc.t * Enc.Tree.t

         let compare t1 t2 =
           Utils.compare_chain
             [ Enc.compare (fst t1) (fst t2)
             ; Enc.Tree.compare (snd t1) (snd t2)
             ]
         ;;
       end))
  ;;

  module Unification = struct
    module Pair = struct
      (** [fst] is a term (e.g., destination) that we want to check unifies with [snd] (which we have already reached).
      @see Mebi_setup.unif_problem where [type unif_problem = {termL:EConstr.t;termR:EConstr.t}] *)
      type t =
        { to_check : EConstr.t
        ; acc : EConstr.t
        }

      include Json.Thing.Make (struct
          type k = t

          let name = "Pair"

          let json ?as_elt ({ to_check; acc } : t) : Yojson.t =
            `Assoc
              [ "to_check", `String (Strfy.econstr to_check)
              ; "acc", `String (Strfy.econstr acc)
              ]
          ;;
        end)

      (* See the [.mli]. *)
      let fresh
            (env : Environ.env)
            (sigma : Evd.evar_map)
            ({ to_check; acc } : t)
        : Evd.evar_map * t
        =
        let sigma, to_check = Rocq_utils.get_next env sigma (TypeOf to_check) in
        sigma, { to_check; acc }
      ;;

      (* See the [.mli]. *)
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

      (* See the [.mli]: only [CannotUnify] is caught. *)
      let unify
            (env : Environ.env)
            (sigma : Evd.evar_map)
            ({ to_check; acc } : t)
        : Evd.evar_map * bool
        =
        try
          let _, sigma =
            Unification.w_unify env sigma Conversion.CUMUL to_check acc
          in
          sigma, true
        with
        | Pretype_errors.PretypeError (_, _, CannotUnify (c, d, _e)) ->
          sigma, false
      ;;

      (* See the [.mli]. *)
      let unifies (to_check : EConstr.t) (acc : EConstr.t) : bool mm =
        state (fun env sigma -> unify env sigma { to_check; acc })
      ;;
    end

    module Problem = struct
      (** if [fst] is sucessfully unified then [snd] represents a tree of constructors that lead to that term (from some previously visited term).
      *)
      type t =
        { act : Pair.t
        ; goto : Pair.t
        ; tree : Enc.Tree.t
        }

      include Json.Thing.Make (struct
          type k = t

          let name = "Problem"

          let json ?as_elt ({ act; goto; tree } : t) : Yojson.t =
            `Assoc
              [ "act", Pair.json ~as_elt:true act
              ; "goto", Pair.json ~as_elt:true act
              ; "tree", Enc.Tree.json ~as_elt:true tree
              ]
          ;;
        end)

      (* See the [.mli]. *)
      let unify_pair_opt (pair : Pair.t) : bool mm =
        state (fun env sigma -> Pair.unify env sigma pair)
      ;;

      (* See the [.mli]. *)
      let unify_opt ({ act; goto; tree } : t) : Enc.Tree.t option mm =
        let open Syntax in
        let* unified_act_opt = unify_pair_opt act in
        let* unified_goto_opt = unify_pair_opt goto in
        match unified_act_opt, unified_goto_opt with
        | true, true -> return (Some tree)
        | _, _ -> return None
      ;;

      (* See the [.mli]. Relevant only when deciding whether to explore a constructor from a premise of another. *)
      let of_constructor
            (args : Rocq_utils.constructor_args)
            ((act, rhs, tree) : Constructor.t)
        : t
        =
        let act : Pair.t = { to_check = args.act; acc = decode act } in
        let goto : Pair.t = { to_check = args.rhs; acc = decode rhs } in
        { act; goto; tree }
      ;;
    end

    (** Premise heads already warned about by {!warn_if_skipped_premise},
        keyed by LTS and head (by name: encodings are renumbered by every
        command). *)
    let skipped_premises : (string * string, unit) Hashtbl.t = Hashtbl.create 8

    (** [decide_premise env sigma (name, args)] is whether the premise
        [name args], not over an LTS, holds: [Some b] when that can be
        decided soundly (backlog item I2), [None] when not -- never a guess.
        An equation [l = r] whose sides are closed once fully normalised (no
        evars: typically guards on the source state, which matching has
        already instantiated) is decided directly: convertible sides hold;
        sides that differ in a constructor, at the head or under matching
        constructors, are false (constructors of an inductive type are
        disjoint); anything else is undecided. Any other proposition goes to
        {!Premise_search.prove} once closed; an open one is undecided.

        Raises nothing. *)
    let decide_premise
          (env : Environ.env)
          (sigma : Evd.evar_map)
          ((name, args) : EConstr.t * EConstr.t array)
      : bool option
      =
      let is_eq : bool =
        match EConstr.kind sigma name with
        | Ind (ind, _) -> Rocqlib.check_ind_ref "core.eq.type" ind
        | _ -> false
      in
      if Bool.not is_eq || Array.length args <> 3
      then (
        (* Any other proposition: bounded proof search, once it is closed
           ([Premise_search]; I2 stage 1). An open premise is [Unknown] and
           gets deferred like an open equation. *)
        match Premise_search.prove env sigma (EConstr.mkApp (name, args)) with
        | Premise_search.Proved _ -> Some true
        | Premise_search.Refuted -> Some false
        | Premise_search.Unknown -> None)
      else (
        let l = Reductionops.nf_all env sigma args.(1) in
        let r = Reductionops.nf_all env sigma args.(2) in
        let closed (x : EConstr.t) : bool =
          Evar.Set.is_empty (Evd.evars_of_term sigma x)
        in
        if Bool.not (closed l && closed r)
        then None
        else (
          let rec decide (l : EConstr.t) (r : EConstr.t) : bool option =
            if Reductionops.is_conv env sigma l r
            then Some true
            else (
              let hl, al = EConstr.decompose_app sigma l in
              let hr, ar = EConstr.decompose_app sigma r in
              match EConstr.kind sigma hl, EConstr.kind sigma hr with
              | Construct (cl, _), Construct (cr, _) ->
                if Bool.not (Names.Construct.CanOrd.equal cl cr)
                then Some false
                else if Array.length al <> Array.length ar
                then None
                else (
                  (* same constructor, not convertible: false if some
                     argument is decidedly different, else unknown *)
                  let ds = Array.map2 decide al ar in
                  if Array.exists (fun d -> d = Some false) ds
                  then Some false
                  else None)
              | _ -> None)
          in
          decide l r))
    ;;

    (** [warn_if_skipped_premise lts_enc (name, args)] warns, once per LTS
        and premise head, that a premise of a constructor of [lts_enc] was
        left undecided, so the constructor was applied as if it held and the
        LTS may contain transitions that do not exist (backlog item I2), and
        notes the approximation ({!Approximations}). A premise over the
        bounded-universal range ({!Premise_search.above_range}) gets its own
        message. Only for a proposition; a data binder ([xs : list nat]) is
        left alone. Raises nothing. *)
    let warn_if_skipped_premise
          (lts_enc : Enc.t)
          ((name, args) : EConstr.t * EConstr.t array)
      : unit mm
      =
      let open Syntax in
      let$+ _warned env sigma =
        let premise : EConstr.t = EConstr.mkApp (name, args) in
        let is_prop : bool =
          try
            match Retyping.get_sort_quality_of env sigma premise with
            | UnivGen.QualityOrSet.Qual q -> Sorts.Quality.is_qprop q
            | UnivGen.QualityOrSet.Set -> false
          with
          | _ -> false
        in
        if is_prop
        then (
          let head : string =
            Rocq_utils.Strfy.econstr env sigma name
            |> fun h ->
            if String.starts_with ~prefix:"@" h
            then String.sub h 1 (String.length h - 1)
            else h
          in
          (* by name: encodings are numbered afresh by every command, so
             keying by encoding silenced a later command's different LTS *)
          let key : string * string =
            Rocq_utils.Strfy.econstr env sigma (decode lts_enc), head
          in
          Approximations.note
            (Printf.sprintf
               "a premise headed by [%s] of %s was left undecided, so it may \
                contain transitions that do not exist"
               head
               (fst key));
          let premise_str : string =
            Rocq_utils.Strfy.econstr
              env
              sigma
              (Reductionops.nf_evar sigma premise)
          in
          if Bool.not (Hashtbl.mem skipped_premises key)
          then (
            Hashtbl.add skipped_premises key ();
            if Premise_search.above_range env sigma premise
            then
              (* undecided only because of [MeBi Config Premise Range]:
                 say that, not the generic reasons below *)
              Logger.warning
                (Printf.sprintf
                   "A constructor of %s has a premise [%s] that ranges over \
                    more than %i values of its bound variable, the most [MeBi \
                    Config Premise Range] allows: deciding it costs a premise \
                    search per value, and its proof grows with the square of \
                    the range. So it is left undecided and the constructor \
                    applied whether or not it holds: the extracted LTS may \
                    contain transitions that do not exist. Raise the range to \
                    decide it. See [MeBi Help Premises]."
                   (Rocq_utils.Strfy.econstr env sigma (decode lts_enc))
                   premise_str
                   !Premise_search.max_range)
            else
              Logger.warning
                (Printf.sprintf
                   "A constructor of %s has a premise headed by [%s] (first \
                    met as [%s]) that MeBi cannot decide: premises over the \
                    LTSs given in [Using], equations, negations and inductive \
                    propositions are decided, by a proof search at most [MeBi \
                    Config Premise Depth] deep, but this one is not closed, \
                    mentions something opaque, or needs a deeper search. So \
                    the constructor is applied whether or not it holds: the \
                    extracted LTS may contain transitions that do not exist, \
                    and a [MeBi Run Bisim] verdict on it may be wrong (a proof \
                    cannot be: [Qed] still checks the premise). See [MeBi Help \
                    Premises]."
                   (Rocq_utils.Strfy.econstr env sigma (decode lts_enc))
                   head
                   premise_str)))
      in
      return ()
    ;;

    (** Premises already warned about as possibly incomplete, keyed like
        {!skipped_premises}. *)
    let partial_premises : (string * string, unit) Hashtbl.t = Hashtbl.create 8

    (** [warn_partial_premise lts_enc (name, args)] warns, once per LTS and
        head, that an open premise's solutions were enumerated but perhaps
        not all of them (depth bound, solution cap, something opaque), so
        transitions may be missing -- an under-approximation -- and notes it
        ({!Approximations}). Raises nothing. *)
    let warn_partial_premise
          (lts_enc : Enc.t)
          ((name, args) : EConstr.t * EConstr.t array)
      : unit mm
      =
      let open Syntax in
      let$+ _warned env sigma =
        let head = Rocq_utils.Strfy.econstr env sigma name in
        let key = Rocq_utils.Strfy.econstr env sigma (decode lts_enc), head in
        Approximations.note
          (Printf.sprintf
             "the solutions of a premise headed by [%s] of %s may be \
              incomplete, so it may be missing transitions"
             head
             (fst key));
        if Bool.not (Hashtbl.mem partial_premises key)
        then (
          Hashtbl.add partial_premises key ();
          Logger.warning
            (Printf.sprintf
               "A constructor of %s has a premise headed by [%s] (first met as \
                [%s]) whose solutions MeBi may not have found all of (the \
                search hit [MeBi Config Premise Depth], its solution cap, or \
                something opaque). The extracted LTS may be missing \
                transitions. See [MeBi Help Premises]."
               (Rocq_utils.Strfy.econstr env sigma (decode lts_enc))
               head
               (Rocq_utils.Strfy.econstr
                  env
                  sigma
                  (Reductionops.nf_evar sigma (EConstr.mkApp (name, args))))))
      in
      return ()
    ;;

    (** LTSs already warned about for transitions MeBi could not determine. *)
    let undetermined : (string, unit) Hashtbl.t = Hashtbl.create 8

    (** [warn_undetermined lts_enc what] warns, once per LTS, that a
        transition whose label or target still has an unknown in it
        ([what]) once everything that could fix it has been tried was left
        out: a constructor binder nothing determines
        ([u n : lts (S n) None n], met from an open source), or an LTS
        premise whose sources the search leaves open. It stands for a
        family of transitions, possibly infinite, so the LTS may be missing
        transitions. Notes the approximation ({!Approximations}). Raises
        nothing. *)
    let warn_undetermined (lts_enc : Enc.t) (what : EConstr.t) : unit mm =
      let open Syntax in
      let$+ _warned env sigma =
        (* by name: encodings are numbered afresh by every command *)
        let lts = Rocq_utils.Strfy.econstr env sigma (decode lts_enc) in
        Approximations.note
          (Printf.sprintf
             "a constructor of %s gives transitions MeBi cannot determine, so \
              it may be missing transitions"
             lts);
        if Bool.not (Hashtbl.mem undetermined lts)
        then (
          Hashtbl.add undetermined lts ();
          Logger.warning
            (Printf.sprintf
               "A constructor of %s gives a transition MeBi cannot determine \
                ([%s] still has an unknown in it): some binder is fixed by \
                nothing, neither the source term nor a premise, so it may \
                stand for infinitely many terms. It is left out, so the \
                extracted LTS may be missing transitions, and a [MeBi Run \
                Bisim] verdict on it may be wrong (a proof cannot be: [Qed] \
                checks every step). Add a premise that bounds the binder. See \
                [MeBi Help Premises]."
               lts
               (Rocq_utils.Strfy.econstr
                  env
                  sigma
                  (Reductionops.nf_evar sigma what))))
      in
      return ()
    ;;

    (** [has_evars sigma x] is whether [x] has evars under [sigma]. Raises
        nothing. *)
    let has_evars (sigma : Evd.evar_map) (x : EConstr.t) : bool =
      Bool.not (Evar.Set.is_empty (Evd.evars_of_term sigma x))
    ;;

    (** [resolve_deferred env sigma deferred] is the evar maps in which a
        constructor's deferred premises hold, with the premises left
        undecided and those whose solutions may be incomplete. The premises
        are decided left to right over every map reached so far (backlog
        item I2, stage 2): a map is dropped where a premise is refuted,
        multiplied where an open premise has several solutions (each
        instantiating what it computes, a target say), and kept unchanged
        where a premise stays undecided (an over-approximation).

        Raises nothing. *)
    let resolve_deferred
          (env : Environ.env)
          (sigma : Evd.evar_map)
          (deferred : (Enc.t * EConstr.t * EConstr.t array) list)
      : Evd.evar_map list
        * (Enc.t * EConstr.t * EConstr.t array) list
        * (Enc.t * EConstr.t * EConstr.t array) list
      =
      List.fold_left
        (fun (states, undecided, partial) ((_, name, args) as d) ->
          let undecided = ref undecided
          and partial = ref partial in
          let states =
            List.concat_map
              (fun sigma ->
                let premise =
                  Reductionops.nf_evar sigma (EConstr.mkApp (name, args))
                in
                match Premise_search.enumerate env sigma premise with
                | [], true -> []
                | [], false ->
                  (* last resort for a closed premise: [prove] also tries the
                     user tactic *)
                  (match Premise_search.prove env sigma premise with
                   | Premise_search.Proved _ -> [ sigma ]
                   | Premise_search.Refuted -> []
                   | Premise_search.Unknown ->
                     undecided := d :: !undecided;
                     [ sigma ])
                | sols, complete ->
                  if Bool.not complete then partial := d :: !partial;
                  sols)
              states
          in
          states, !undecided, !partial)
        ([ sigma ], [], [])
        deferred
    ;;

    module Problems = struct
      type deferred = Enc.t * EConstr.t * EConstr.t array

      (** [deferred] holds the premises that were not over an LTS and could
          not be decided when the constructor was matched -- typically because
          they mention what its LTS premises will instantiate (a label, a
          target). They are decided once those are unified, in
          [sandbox_unify_all] (backlog item I2). *)
      type t =
        { sigma : Evd.evar_map
        ; to_unify : Problem.t list
        ; deferred : deferred list
        }

      include Json.Thing.Make (struct
          type k = t

          let name = "Problems"

          let json ?as_elt ({ to_unify; _ } : t) : Yojson.t =
            `Assoc
              [ ( "to_unify"
                , `List (List.map (Problem.json ~as_elt:true) to_unify) )
              ]
          ;;
        end)

      (* See the [.mli]. *)
      let empty () : t mm =
        let open Syntax in
        let* sigma = get_sigma in
        return { sigma; to_unify = []; deferred = [] }
      ;;

      let is_empty : t -> bool = function
        | { to_unify = []; _ } -> true
        | _ -> false
      ;;

      (* See the [.mli]. *)
      let rec unify_list_opt : Problem.t list -> Enc.Tree.t list option mm =
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

      (* See the [.mli]. *)
      let sandbox_unify_all
            (lts_enc : Enc.t)
            (act : EConstr.t)
            (goto : EConstr.t)
            ({ sigma; to_unify; deferred } : t)
        : (EConstr.t * EConstr.t * Enc.Tree.t list) list mm
        =
        let open Syntax in
        sandbox
          ~sigma
          (let* unified_opt = unify_list_opt to_unify in
           match unified_opt with
           | None -> return []
           | Some constructor_trees ->
             let$+ resolved env sigma = resolve_deferred env sigma deferred in
             let states, undecided, partial = resolved in
             let warn f (ds : deferred list) =
               iterate
                 0
                 (List.length ds - 1)
                 ()
                 (fun i () ->
                   let lts_enc, name, args = List.nth ds i in
                   f lts_enc (name, args))
             in
             let* () = warn warn_if_skipped_premise undecided in
             let* () = warn warn_partial_premise partial in
             let* env = get_env in
             (* one transition per way the deferred premises hold; one with
                an unknown left in it is dropped, with a warning *)
             let found =
               List.map
                 (fun sigma ->
                   ( sigma
                   , Reductionops.nf_all env sigma act
                   , Reductionops.nf_all env sigma goto ))
                 states
             in
             let* () =
               match
                 List.find_opt
                   (fun (sigma, act, goto) ->
                     has_evars sigma act || has_evars sigma goto)
                   found
               with
               | Some (_, _, goto) -> warn_undetermined lts_enc goto
               | None -> return ()
             in
             return
               (List.filter_map
                  (fun (sigma, act, goto) ->
                    if has_evars sigma act || has_evars sigma goto
                    then None
                    else Some (act, goto, constructor_trees))
                  found))
      ;;
    end

    module ListOfProblems = struct
      type t = Problems.t list

      include Json.List.Make (struct
          include Problems

          let name = "Problems"
        end)

      let is_empty : t -> bool =
        Logger.trace __FUNCTION__;
        function [] -> true | [ p ] -> Problems.is_empty p | _ :: _ -> false
      ;;

      (* See the [.mli]. *)
      let cross_product ({ sigma; to_unify; _ } : Problems.t) : t -> t =
        Logger.trace __FUNCTION__;
        List.concat_map
          (fun ({ to_unify = xs; deferred; _ } : Problems.t) : t ->
          List.map
            (fun (y : Problem.t) : Problems.t ->
              (* keep the accumulated problems' deferred premises *)
              { sigma; to_unify = y :: xs; deferred })
            to_unify)
      ;;
    end

    module Constructors = struct
      include Enc.Constructor_trees

      (* See the [.mli]. *)
      let rec retrieve
                (constructor_index : int)
                (acc : t)
                (act : EConstr.t)
                (tgt : EConstr.t)
        : Enc.t * ListOfProblems.t -> t mm
        =
        Logger.trace __FUNCTION__;
        let open Syntax in
        function
        | _, [] -> return acc
        | lts_enc, problems :: tl ->
          let* acc = retrieve constructor_index acc act tgt (lts_enc, tl) in
          let* found : Constructor.t list =
            sandbox
              (let* results =
                 Problems.sandbox_unify_all lts_enc act tgt problems
               in
               return
                 (List.map
                    (fun (act, goto, constructor_trees) ->
                      let tree : Enc.Tree.t =
                        N ((lts_enc, constructor_index), constructor_trees)
                      in
                      Constructor.encode act goto tree)
                    results))
          in
          return (List.rev_append found acc)
      ;;

      (* See the [.mli]. *)
      let to_problems args (constructors : t) : Problems.t mm =
        Logger.trace __FUNCTION__;
        let open Syntax in
        let* sigma = get_sigma in
        let to_unify : Problem.t list =
          List.map (Problem.of_constructor args) constructors
        in
        let p : Problems.t = { sigma; to_unify; deferred = [] } in
        return p
      ;;

      (* See the [.mli]. *)
      let axiom
            (act : EConstr.t)
            (tgt : EConstr.t)
            (constructor_index : Enc.t * int)
            (constructors : t)
        : t mm
        =
        Logger.trace __FUNCTION__;
        log_econstr ~__FUNCTION__ ~s:"act" act;
        log_econstr ~__FUNCTION__ ~s:"tgt" tgt;
        let open Syntax in
        let$+ open_ _ sigma = has_evars sigma act || has_evars sigma tgt in
        if open_
        then
          (* a binder nothing fixes: see [warn_undetermined] *)
          let* () = warn_undetermined (fst constructor_index) tgt in
          return constructors
        else (
          let tree : Enc.Tree.t = N (constructor_index, []) in
          let axiom : Constructor.t = Constructor.encode act tgt tree in
          return (axiom :: constructors))
      ;;
    end

    (** [warn_each warn ds] is [warn lts_enc (name, args)] for each deferred
        premise [(lts_enc, name, args)] of [ds], in order (one of the
        [warn_*] functions above). Raises nothing. *)
    let warn_each
          (warn : Enc.t -> EConstr.t * EConstr.t array -> unit mm)
          (ds : Problems.deferred list)
      : unit mm
      =
      iterate
        0
        (List.length ds - 1)
        ()
        (fun i () ->
          let lts_enc, name, args = List.nth ds i in
          warn lts_enc (name, args))
    ;;

    (** [axioms_per_state states (act, tgt) (lts_enc, i) cs] is [cs] with,
        for each evar map of [states] in turn, the constructor [i] of
        [lts_enc] as an axiom ({!Constructors.axiom}) from [act] to [tgt],
        both fully normalised under that map. Each runs in that map, the
        state left unchanged ({!sandbox}). Raises nothing. *)
    let axioms_per_state
          (states : Evd.evar_map list)
          ((act, tgt) : EConstr.t * EConstr.t)
          (constructor_index : Enc.t * int)
          (constructors : Constructors.t)
      : Constructors.t mm
      =
      let open Syntax in
      let* env = get_env in
      iterate
        0
        (List.length states - 1)
        constructors
        (fun k acc ->
          let sigma = List.nth states k in
          let act = Reductionops.nf_all env sigma act in
          let tgt = Reductionops.nf_all env sigma tgt in
          sandbox ~sigma (Constructors.axiom act tgt constructor_index acc))
    ;;

    (** [combine_branches branches] is the alternatives found in
        [branches] (one per way an LTS premise was explored; [None] where it
        led nowhere) as one: the first one's encoding with every one's
        problems, in order, or [None] if all are [None]. Raises nothing. *)
    let combine_branches (branches : (Enc.t * ListOfProblems.t) option list)
      : (Enc.t * ListOfProblems.t) option
      =
      match List.filter_map Fun.id branches with
      | [] -> None
      | (enc, _) :: _ as found -> Some (enc, List.concat_map snd found)
    ;;

    (** [premises_ahead env sigma lts_enc indmap (substl, binders)] is the
        premises among [binders] that are not over an LTS (heads not in
        [indmap]), each as a deferred premise of [lts_enc], in order:
        applications and other propositions alike, as
        {!check_updated_ctx} sees them. Data binders and LTS premises are
        skipped. Raises nothing. *)
    let premises_ahead
          (env : Environ.env)
          (sigma : Evd.evar_map)
          (lts_enc : Enc.t)
          (indmap : Ind.t F.t)
      :  EConstr.Vars.substl * EConstr.rel_declaration list
      -> Problems.deferred list
      =
      let rec walk acc = function
        | _ :: substl, t :: tl ->
          let ty =
            EConstr.Vars.substl substl (Context.Rel.Declaration.get_type t)
          in
          let acc =
            match EConstr.kind sigma ty with
            | App (h, a)
              when Option.is_empty (F.find_opt indmap h)
                   && Premise_search.is_prop env sigma ty ->
              (lts_enc, h, a) :: acc
            | App _ -> acc
            (* a premise that is not an application, as in
               [check_updated_ctx] *)
            | _ when Premise_search.is_prop env sigma ty ->
              (lts_enc, ty, [||]) :: acc
            | _ -> acc
          in
          walk acc (substl, tl)
        | _ -> List.rev acc
      in
      walk []
    ;;

    (* See the [.mli]. *)
    let check_constructor_args_unify
          (lhs : EConstr.t)
          (act : EConstr.t)
          (args : Rocq_utils.constructor_args)
      : bool mm
      =
      Logger.trace __FUNCTION__;
      let open Syntax in
      let* lhs_unifies : bool = Pair.unifies args.lhs lhs in
      if lhs_unifies then Pair.unifies args.act act else return false
    ;;

    (* See the [.mli]. *)
    let rec check_valid_constructors
              (constructors : Ind.LTS.constructor array)
              (indmap : Ind.t F.t)
              (from_term : EConstr.t)
              (act_term : EConstr.t)
              (lts_enc : Enc.t)
      : Constructors.t mm
      =
      Logger.trace __FUNCTION__;
      let open Syntax in
      let* from_term : EConstr.t = econstr_normalize from_term in
      let iter_body (i : int) (acc : Constructors.t) : Constructors.t mm =
        (* NOTE: extract args for constructor *)
        let { constructor = ctx, tm; _ } : Ind.LTS.constructor =
          constructors.(i)
        in
        let decls = Rocq_utils.get_econstr_decls ctx in
        let* substl = mk_ctx_substl [] (List.rev decls) in
        let* args : Rocq_utils.constructor_args = extract_args ~substl tm in
        (* NOTE: make fresh [act_term] to avoid conflicts with sibling constructors *)
        let* act_term : EConstr.t = fresh_evar (TypeOf act_term) in
        let* success = check_constructor_args_unify from_term act_term args in
        if success
        then (
          (* NOTE: replace [act] with the fresh [act_term] *)
          let fresh_args = { args with act = act_term } in
          explore_valid_constructor
            indmap
            from_term
            lts_enc
            fresh_args
            (i, acc)
            (substl, decls))
        else return acc
      in
      iterate 0 (Array.length constructors - 1) [] iter_body

    (* See the [.mli]. *)
    and explore_valid_constructor
          (indmap : Ind.t F.t)
          (from_term : EConstr.t)
          (lts_enc : Enc.t)
          (args : Rocq_utils.constructor_args)
          ((i, constructors) : int * Constructors.t)
          ((substl, decls) : EConstr.Vars.substl * EConstr.rel_declaration list)
      : Constructors.t mm
      =
      Logger.trace __FUNCTION__;
      let open Syntax in
      (* NOTE: unpack and normalize [act] and [tgt] from [args] *)
      let tgt : EConstr.t = EConstr.Vars.substl substl args.rhs in
      let* tgt : EConstr.t = econstr_normalize tgt in
      let* act : EConstr.t = econstr_normalize args.act in
      let* empty_problems : Problems.t = Problems.empty () in
      let* next_constructor_problems : (Enc.t * ListOfProblems.t) option =
        check_updated_ctx lts_enc [ empty_problems ] indmap (substl, decls)
      in
      check_for_next_constructors
        i
        act
        tgt
        constructors
        next_constructor_problems

    (* See the [.mli]. *)
    and check_for_next_constructors
          (i : int)
          (outer_act : EConstr.t)
          (tgt_term : EConstr.t)
          (constructors : Constructors.t)
      : (Enc.t * ListOfProblems.t) option -> Constructors.t mm
      =
      Logger.trace __FUNCTION__;
      function
      | None -> return constructors
      | Some (next_lts_enc, next_problems) ->
        if ListOfProblems.is_empty next_problems
        then (
          (* No LTS premises to wait for: decide the deferred ones now --
             each way they hold (an open one may compute the target) is an
             axiom of its own. *)
          let deferred : Problems.deferred list =
            List.concat_map (fun (p : Problems.t) -> p.deferred) next_problems
          in
          let open Syntax in
          let$+ resolved env sigma = resolve_deferred env sigma deferred in
          let states, undecided, partial = resolved in
          let* () = warn_each warn_if_skipped_premise undecided in
          let* () = warn_each warn_partial_premise partial in
          axioms_per_state
            states
            (outer_act, tgt_term)
            (next_lts_enc, i)
            constructors)
        else
          Constructors.retrieve
            i
            constructors
            outer_act
            tgt_term
            (next_lts_enc, next_problems)

    (* See the [.mli]. *)
    and check_updated_ctx
          (lts_enc : Enc.t)
          (acc : ListOfProblems.t)
          (indmap : Ind.t F.t)
      :  EConstr.Vars.substl * EConstr.rel_declaration list
      -> (Enc.t * ListOfProblems.t) option mm
      =
      Logger.trace __FUNCTION__;
      function
      | [], [] -> return (Some (lts_enc, acc))
      | _hsubstl :: substl, t :: tl ->
        let open Syntax in
        let ty_t : EConstr.t = Context.Rel.Declaration.get_type t in
        let$+ upd_t env sigma = EConstr.Vars.substl substl ty_t in
        let* env = get_env in
        let* sigma = get_sigma in
        (match EConstr.kind sigma upd_t with
         | App (name, args) ->
           handle_app lts_enc acc indmap (substl, tl) (name, args)
         | _ when Premise_search.is_prop env sigma upd_t ->
           (* A premise that is not an application: an implication or a
              [forall] ([P -> False], [forall k, k < n -> P k]), or a bare
              proposition. Until 2026-10-02 these fell into the case below,
              as if they were a variable's type, and were dropped without a
              warning, so the LTS silently gained transitions. They are
              premises like any other, decided or deferred by
              [check_unknown_app] (as [(t, [||])]: [mkApp (t, [||])] is [t]). *)
           check_unknown_app lts_enc acc indmap (substl, tl) (upd_t, [||])
         | _ -> check_updated_ctx lts_enc acc indmap (substl, tl))
      | _substl, _ctxl -> Err.invalid_check_updated_ctx _substl _ctxl
    (* ! Impossible ! *)
    (* FIXME: should fail if [t] is an evar -- but *NOT* if it contains evars! *)

    (** [handle_app lts_enc acc indmap (substl, binders) (name, args)] is
        {!check_updated_ctx} on the remaining [binders], after the premise
        [name args]: if [name] is not one of the LTSs given in [Using], it
        is decided or deferred ({!check_unknown_app}); if it is, it is an LTS
        step, explored for its solutions, which become problems. A step
        whose source is still open is explored once per source that its
        earlier premises fix (resolving them first), or, if none does, once
        per source a bounded search enumerates for it.

        Raises as {!check_valid_constructors}, when run. *)
    and handle_app
          (lts_enc : Enc.t)
          (acc : ListOfProblems.t)
          (indmap : Ind.t F.t)
          ((substl, tl) : EConstr.Vars.substl * EConstr.rel_declaration list)
          ((name, args) : EConstr.t * EConstr.t array)
      =
      Logger.trace __FUNCTION__;
      match F.find_opt indmap name with
      | None -> check_unknown_app lts_enc acc indmap (substl, tl) (name, args)
      | Some c ->
        let open Syntax in
        let raw_args = args in
        let lhs_raw = (Rocq_utils.constructor_args raw_args).lhs in
        (* Explore this LTS premise from the current evar map, its source
           closed, then carry on with the remaining binders. *)
        let explore_closed () =
          let args = Rocq_utils.constructor_args raw_args in
          let$+ lhs env sigma = Reductionops.nf_evar sigma args.lhs in
          let$+ act env sigma = Reductionops.nf_evar sigma args.act in
          let args = { args with lhs; act } in
          let next_lts : Ind.LTS.constructor array =
            Ind.get_lts_constructor_types c
          in
          let* next_constructors : Constructors.t =
            check_valid_constructors next_lts indmap lhs act c.enc
          in
          match next_constructors with
          | [] -> return None
          | next_constructors ->
            let* problems : Problems.t =
              Constructors.to_problems args next_constructors
            in
            let acc = ListOfProblems.cross_product problems acc in
            check_updated_ctx lts_enc acc indmap (substl, tl)
        in
        (* The premise's source is open and nothing fixes it. Matching
           constructors against an open source cannot be trusted: the first
           match fixes it for all its siblings, so only the first constructor
           was ever found, and a recursive one could recurse without bound.
           Instead the premise search, which is bounded and knows when it is
           complete, enumerates the premise, and each distinct source it finds
           is explored as usual (note 9, option B). Until 2026-10-03 this
           explored from the open source, with a warning. *)
        let explore_sources () =
          let$+ found env sigma =
            let premise =
              Reductionops.nf_evar sigma (EConstr.mkApp (name, raw_args))
            in
            let sols, complete = Premise_search.enumerate env sigma premise in
            let source s = Reductionops.nf_all env s lhs_raw in
            let closed, open_ =
              List.partition (fun s -> Bool.not (has_evars s (source s))) sols
            in
            (* closed terms: comparing them under any evar map is sound *)
            let sources =
              List.fold_left
                (fun acc s ->
                  let l = source s in
                  if List.exists (EConstr.eq_constr sigma l) acc
                  then acc
                  else l :: acc)
                []
                closed
            in
            let undetermined =
              match open_ with
              | s :: _ ->
                Some (Reductionops.nf_evar s (EConstr.mkApp (name, raw_args)))
              | [] -> None
            in
            List.rev sources, complete, undetermined
          in
          let sources, complete, undetermined = found in
          let* () =
            if complete
            then return ()
            else warn_partial_premise lts_enc (name, raw_args)
          in
          let* () =
            match undetermined with
            | Some t -> warn_undetermined lts_enc t
            | None -> return ()
          in
          let* branches =
            iterate
              0
              (List.length sources - 1)
              []
              (fun k acc ->
                let* r =
                  sandbox
                    (let* ok = Pair.unifies lhs_raw (List.nth sources k) in
                     if ok then explore_closed () else return None)
                in
                return (r :: acc))
          in
          return (combine_branches branches)
        in
        let explore () =
          let$+ lhs_open _ sigma =
            has_evars sigma (Reductionops.nf_evar sigma lhs_raw)
          in
          if lhs_open then explore_sources () else explore_closed ()
        in
        let$+ lhs_open _ sigma =
          Bool.not
            (Evar.Set.is_empty
               (Evd.evars_of_term sigma (Reductionops.nf_evar sigma lhs_raw)))
        in
        let deferred : Problems.deferred list =
          match acc with (p : Problems.t) :: _ -> p.deferred | [] -> []
        in
        (* Binders are walked last to first, so premises {e declared} before
           this one -- the natural place for whatever determines its source,
           [In q l -> lts q a q'] -- have not been reached yet: look ahead at
           them too. They are decided again when the walk reaches them, which
           is harmless (in each branch they are closed by then, and hold). *)
        let$+ ahead env sigma =
          premises_ahead env sigma lts_enc indmap (substl, tl)
        in
        let deferred = deferred @ ahead in
        if Bool.not lhs_open || List.is_empty deferred
        then explore ()
        else
          (* The premise's source is still open, and earlier premises may
             fix it ([In q l -> lts q a q']): resolve those first and explore
             it once per way they hold (backlog item I2, stage 2). Exploring
             from an open source finds only some of its steps. *)
          let$+ resolved env sigma = resolve_deferred env sigma deferred in
          let states, undecided, partial = resolved in
          let* () = warn_each warn_if_skipped_premise undecided in
          let* () = warn_each warn_partial_premise partial in
          let* branches =
            iterate
              0
              (List.length states - 1)
              []
              (fun k acc ->
                let* r = sandbox ~sigma:(List.nth states k) (explore ()) in
                return (r :: acc))
          in
          return (combine_branches branches)

    (** [check_unknown_app lts_enc acc indmap (substl, binders) (name, args)]
        is {!check_updated_ctx} on the remaining [binders] after a premise not
        over an LTS: [None] if {!decide_premise} finds it false, the problems
        unchanged if true, and, if undecided, the premise deferred on every
        problem set, to be decided once the LTS premises are unified.

        Raises as {!check_valid_constructors}, when run. *)
    and check_unknown_app
          (lts_enc : Enc.t)
          (acc : ListOfProblems.t)
          (indmap : Ind.t F.t)
          ((substl, tl) : EConstr.Vars.substl * EConstr.rel_declaration list)
          ((name, args) : EConstr.t * EConstr.t array)
      : (Enc.t * ListOfProblems.t) option mm
      =
      Logger.trace __FUNCTION__;
      if Logger.is_enabled Debug
      then log_econstr ~__FUNCTION__ ~m:Warning ~s:"name not indmap" name;
      (* Array.to_list args |> log_econstrs ~__FUNCTION__ ~m:Warning ~s:"args"; *)
      let open Syntax in
      let$+ decided env sigma = decide_premise env sigma (name, args) in
      match decided with
      | Some false ->
        (* the premise is false here: this constructor does not apply *)
        return None
      | Some true -> check_updated_ctx lts_enc acc indmap (substl, tl)
      | None ->
        (* not decidable yet: decide it once the LTS premises are unified *)
        let acc : ListOfProblems.t =
          List.map
            (fun (p : Problems.t) : Problems.t ->
              { p with deferred = (lts_enc, name, args) :: p.deferred })
            acc
        in
        check_updated_ctx lts_enc acc indmap (substl, tl)
    ;;

    (* See the [.mli]. *)
    let collect_valid_constructors
          (constructors : Ind.LTS.constructor array)
          (indmap : Ind.t F.t)
          (from_term : EConstr.t)
          (label_type : EConstr.t)
          (lts_enc : Enc.t)
      : Constructors.t mm
      =
      Logger.trace __FUNCTION__;
      let open Syntax in
      let* fresh_evar = fresh_evar (OfType label_type) in
      check_valid_constructors constructors indmap from_term fresh_evar lts_enc
    ;;
  end

  (* See the [.mli] for the three set and table makers. *)
  let make_enc_hashtbl () : (module Hashtbl.S with type key = Enc.t) =
    (module Hashtbl.Make (Enc))
  ;;

  let make_enc_set () : (module Set.S with type elt = Enc.t) =
    (module Set.Make (Enc))
  ;;

  let make_econstr_set () : (module Set.S with type elt = EConstr.t) =
    Logger.trace __FUNCTION__;
    (module Set.Make (struct
         type t = EConstr.t

         let compare (a : t) (b : t) : int =
           let a = encode a in
           let b = encode b in
           Enc.compare a b
         ;;
       end))
  ;;
end
