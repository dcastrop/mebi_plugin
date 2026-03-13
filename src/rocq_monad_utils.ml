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
  val exists_eq : EConstr.t -> 'a list -> ('a -> EConstr.t) -> bool mm
  val type_of_econstr : EConstr.t -> EConstr.t mm
  val type_of_constrexpr : Constrexpr.constr_expr -> EConstr.t mm

  module Strfy : sig
    val constr : Constr.t -> string
    val constr_kind : Constr.t -> string
    val econstr : EConstr.t -> string
    val econstr_kind : EConstr.t -> string
    val econstr_rel_decl : EConstr.rel_declaration -> string
    val hyp_name : Rocq_utils.hyp -> string
    val hyp_type : Rocq_utils.hyp -> string
    val hyp : Rocq_utils.hyp -> string
    val hyp_value : Rocq_utils.hyp -> string

    (* val rocq_ind : ('a -> string) -> 'a Rocq_ind.t -> string *)
    val econstr_bindings : EConstr.t Tactypes.bindings -> string
  end

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

  module type SErrors = sig
    type t =
      | LTS_Empty
      | LTS_Incomplete of string
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

      val json : ?as_elt:bool -> t -> Yojson.t
      val to_string : ?pretty:bool -> t -> string

      val log
        :  ?__FUNCTION__:string
        -> ?m:Output.Kind.t
        -> ?s:string
        -> t
        -> unit
    end

    type t =
      { enc : enc
      ; ind : EConstr.t
      ; kind : kind
      }

    and kind =
      | Type of EConstr.t option
      | LTS of LTS.t

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
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

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
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

      type k = t

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string

      val log
        :  ?__FUNCTION__:string
        -> ?m:Output.Kind.t
        -> ?s:string
        -> k
        -> unit

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

      type k = t

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string

      val log
        :  ?__FUNCTION__:string
        -> ?m:Output.Kind.t
        -> ?s:string
        -> k
        -> unit

      val unify_pair_opt : Pair.t -> bool mm
      val unify_opt : t -> tree option mm
      val of_constructor : Rocq_utils.constructor_args -> Constructor.t -> t
    end

    module Problems : sig
      type t =
        { sigma : Evd.evar_map
        ; to_unify : Problem.t list
        }

      type k = t

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string

      val log
        :  ?__FUNCTION__:string
        -> ?m:Output.Kind.t
        -> ?s:string
        -> k
        -> unit

      val empty : unit -> t mm
      val is_empty : t -> bool
      val unify_list_opt : Problem.t list -> tree list option mm

      val sandbox_unify_all_opt
        :  EConstr.t
        -> EConstr.t
        -> t
        -> (EConstr.t * EConstr.t * tree list) option mm
    end

    module ListOfProblems : sig
      type t = Problems.t list
      type k = Problems.t list

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string

      val log
        :  ?__FUNCTION__:string
        -> ?m:Output.Kind.t
        -> ?s:string
        -> k
        -> unit

      val is_empty : t -> bool
      val cross_product : Problems.t -> t -> t
    end

    module Constructors : sig
      type t = Constructor.t list

      val json : ?as_elt:bool -> t -> Yojson.t
      val to_string : ?pretty:bool -> t -> string

      val log
        :  ?__FUNCTION__:string
        -> ?m:Output.Kind.t
        -> ?s:string
        -> t
        -> unit

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

  module Bindings : sig
    module Instructions : sig
      type t =
        | Undefined
        | Done
        | Arg of
            { root : Constr.t
            ; index : int
            ; cont : t
            }

      val json : ?as_elt:bool -> t -> Yojson.t
      val to_string : ?pretty:bool -> t -> string

      val log
        :  ?__FUNCTION__:string
        -> ?m:Output.Kind.t
        -> ?s:string
        -> t
        -> unit

      exception Rocq_bindings_CannotAppendDone of unit

      val append : t -> t -> t
      val length : t -> int
    end

    module ConstrMap : sig
      include Hashtbl.S with type key = Constr.t

      type v = Names.Name.t * Instructions.t
      type t' = v t

      val json : ?as_elt:bool -> t' -> Yojson.t
      val to_string : ?pretty:bool -> t' -> string

      val log
        :  ?__FUNCTION__:string
        -> ?m:Output.Kind.t
        -> ?s:string
        -> t'
        -> unit

      val update : t' -> Constr.t -> v -> unit

      exception Rocq_bindings_CannotFindBindingName of EConstr.t

      val find_name
        :  (EConstr.t * Names.Name.t) list
        -> EConstr.t
        -> Names.Name.t mm

      val extract_binding_map
        :  (EConstr.t * Names.Name.t) list
        -> EConstr.t
        -> Constr.t
        -> t' mm

      val make_opt
        :  (EConstr.t * Names.Name.t) list
        -> EConstr.t * Constr.t
        -> t' option mm
    end

    type t =
      | No_Bindings
      | Use_Bindings of
          { from : ConstrMap.t' option
          ; action : ConstrMap.t' option
          ; goto : ConstrMap.t' option
          }

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
    val use_no_bindings : ConstrMap.t' option list -> bool

    val extract
      :  (EConstr.t * Names.Name.t) list
      -> EConstr.t * Constr.t
      -> EConstr.t * Constr.t
      -> EConstr.t * Constr.t
      -> t mm
  end

  module ConstructorBindings : sig
    type t =
      { index : int
      ; name : string
      ; bindings : Bindings.t
      }

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
    val extract_info : Ind.t -> t list mm
    val get_quantified_hyp : Names.Name.t -> Tactypes.quantified_hypothesis

    exception Rocq_bindings_BindingInstruction_NotApp of EConstr.t

    exception
      Rocq_bindings_BindingInstruction_Undefined of EConstr.t * EConstr.t

    exception
      Rocq_bindings_BindingInstruction_IndexOutOfBounds of EConstr.t * int

    exception Rocq_bindings_BindingInstruction_NEQ of EConstr.t * Constr.t

    val get_bound_term : EConstr.t -> Bindings.Instructions.t -> EConstr.t mm

    val get_explicit_bindings
      :  EConstr.t * Bindings.ConstrMap.t' option
      -> EConstr.t Tactypes.explicit_bindings mm

    val get
      :  EConstr.t
      -> EConstr.t option
      -> EConstr.t option
      -> Bindings.t
      -> EConstr.t Tactypes.bindings mm
  end
end

module Make (Log : Logger.S) (Ctx : Rocq_context.S) (Enc : Encoding.S) :
  S with type enc = Enc.t and type tree = Enc.Tree.t = struct
  module Log' = Log

  (** [module Log] is a modified [Log] -- here disables trace & debug printing
  *)
  module Log : Logger.S with module Config.Mode = Log.Config.Mode =
    Logger.ReMake
      (Log)
      (struct
        let level : Output.Kind.level -> bool = function
          | Debug -> false
          | Info -> true
          | Notice -> true
          | Warning -> true
          | Error -> true
        ;;

        let special : Output.Kind.special -> bool = function
          | Trace -> false
          | Result -> true
          | Show -> true
        ;;
      end)

  (*****************************************)

  include Rocq_monad.Make (Log) (Ctx) (Enc)

  type tree = Enc.Tree.t

  (*****************************************)

  let fresh_evar (x : Rocq_utils.evar_source) : EConstr.t mm =
    Log.trace __FUNCTION__;
    state (fun env sigma -> Rocq_utils.get_next env sigma x)
  ;;

  let econstr_eq ?(enc : bool = true) (a : EConstr.t) (b : EConstr.t) : bool mm =
    Log.trace __FUNCTION__;
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

  let econstr_compare (a : EConstr.t) (b : EConstr.t) : int =
    let a = encode a in
    let b = encode b in
    Enc.compare a b
  ;;

  let get_encoding (x : EConstr.t) : Enc.t =
    Log.trace __FUNCTION__;
    run
      (let open Syntax in
       let* x : EConstr.t = econstr_normalize x in
       get_encoding x |> return)
  ;;

  let econstr_kind (x : EConstr.t) : Rocq_utils.econstr_kind mm =
    Log.trace __FUNCTION__;
    let open Syntax in
    let* sigma = get_sigma in
    let* x : EConstr.t = econstr_normalize x in
    EConstr.kind sigma x |> return
  ;;

  let econstr_is_evar (x : EConstr.t) : bool mm =
    Log.trace __FUNCTION__;
    state (fun env sigma -> sigma, EConstr.isEvar sigma x)
  ;;

  (*********************************************************)

  let econstr_to_constr
        ?(abort_on_undefined_evars : bool = false)
        (x : EConstr.t)
    : Constr.t mm
    =
    Log.trace __FUNCTION__;
    state (fun env sigma -> sigma, Rocq_utils.econstr_to_constr sigma x)
  ;;

  let econstr_to_constr_opt (x : EConstr.t) : Constr.t option mm =
    Log.trace __FUNCTION__;
    state (fun env sigma -> sigma, Rocq_utils.econstr_to_constr_opt sigma x)
  ;;

  let constrexpr_to_econstr (x : Constrexpr.constr_expr) : EConstr.t mm =
    Log.trace __FUNCTION__;
    state (fun env sigma -> Rocq_utils.constrexpr_to_econstr env sigma x)
  ;;

  (*********************************************************)

  let exists_eq (x : EConstr.t) (ys : 'a list) (decoder : 'a -> EConstr.t)
    : bool mm
    =
    Log.trace __FUNCTION__;
    (* List.exists (fun y -> decoder y |> econstr_eq x) ys *)
    let open Syntax in
    let f (i : int) (a : bool) =
      let y : EConstr.t = List.nth ys i |> decoder in
      let* b : bool = econstr_eq x y in
      (a || b) |> return
    in
    iterate 0 (List.length ys - 1) false f
  ;;

  (*********************************************************)

  let type_of_econstr (x : EConstr.t) : EConstr.t mm =
    (* Log.trace __FUNCTION__; *)
    let open Syntax in
    let* t : EConstr.t = econstr_normalize x in
    state (fun env sigma -> Typing.type_of env sigma t)
  ;;

  let type_of_constrexpr (x : Constrexpr.constr_expr) : EConstr.t mm =
    (* Log.trace __FUNCTION__; *)
    let open Syntax in
    let* t : EConstr.t = constrexpr_to_econstr x in
    type_of_econstr t
  ;;

  (*********************************************************)

  module Strfy = struct
    let constr : Constr.t -> string = fstring Rocq_utils.Strfy.constr
    let constr_kind : Constr.t -> string = fstring Rocq_utils.Strfy.constr_kind
    let econstr : EConstr.t -> string = fstring Rocq_utils.Strfy.econstr

    let econstr_kind : EConstr.t -> string =
      fstring Rocq_utils.Strfy.econstr_kind
    ;;

    let econstr_rel_decl : EConstr.rel_declaration -> string =
      fstring Rocq_utils.Strfy.econstr_rel_decl
    ;;

    let hyp_name : Rocq_utils.hyp -> string = Rocq_utils.Strfy.hyp_name
    let hyp_type : Rocq_utils.hyp -> string = fstring Rocq_utils.Strfy.hyp_type

    let hyp (x : Rocq_utils.hyp) : string =
      Printf.sprintf "%s: %s" (hyp_name x) (hyp_type x)
    ;;

    let hyp_value : Rocq_utils.hyp -> string =
      fstring Rocq_utils.Strfy.hyp_value
    ;;

    (* let rocq_ind (f : 'a -> string) : 'a Rocq_ind.t -> string =
       fstring (Rocq_ind.to_string f)
       ;; *)

    let econstr_bindings : EConstr.t Tactypes.bindings -> string = function
      | NoBindings -> "NoBindings"
      | ImplicitBindings xs ->
        Utils.Strfy.list (Of econstr) xs
        |> Printf.sprintf "ImplicitBindings: %s"
      | ExplicitBindings xs ->
        Utils.Strfy.list
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
        |> Printf.sprintf "ExplicitBindings: %s"
    ;;
  end

  let log_econstr
        ?(__FUNCTION__ : string = "")
        ?(m : Output.Kind.t = Output.Kind.Debug)
        ?(s : string = "EConstr")
        (x : EConstr.t)
    : unit
    =
    Log'.thing ~__FUNCTION__ m s x (Of Strfy.econstr)
  ;;

  let log_econstrs
        ?(__FUNCTION__ : string = "")
        ?(m : Output.Kind.t = Output.Kind.Debug)
        ?(s : string = "EConstrs")
        (x : EConstr.t list)
    : unit
    =
    Log'.things ~__FUNCTION__ m s x (Of Strfy.econstr)
  ;;

  module type SErrors = sig
    type t =
      (* NOTE: *)
      | LTS_Empty
      | LTS_Incomplete of string
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

  module Errors : SErrors = struct
    type t =
      (* NOTE: *)
      | LTS_Empty
      | LTS_Incomplete of string
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

    let mebi_handler : t -> string = function
      (* NOTE: *)
      | LTS_Empty -> "LTS_Empty"
      | LTS_Incomplete x -> Printf.sprintf "LTS_Incomplete: %s" x
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
    (* NOTE: *)
    val lts_empty : unit -> 'a
    val lts_incomplete : string -> 'a
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

  module Err : SErr = struct
    let lts_empty () = raise (Errors.lts_empty ())
    let lts_incomplete (msg : string) = raise (Errors.lts_incomplete msg)
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

      include
        Json.Thing.Make
          (Log)
          (struct
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

    include
      Json.Thing.Make
        (Log)
        (struct
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
      Log.trace __FUNCTION__;
      get_lts_constructor_types x
      |> Array.map (fun ({ name; _ } : LTS.constructor) -> name)
    ;;

    let get_lts_constructors (x : t) : Rocq_utils.ind_constr array =
      Log.trace __FUNCTION__;
      get_lts_constructor_types x
      |> Array.map (fun ({ constructor; _ } : LTS.constructor) -> constructor)
    ;;

    (** [lookup x] is a wrapper for [Inductive.lookup_mind_specif] *)
    let lookup (x : Names.inductive) : Declarations.mind_specif mm =
      (* Log.trace __FUNCTION__; *)
      let open Syntax in
      let* env = get_env in
      Inductive.lookup_mind_specif env x |> return
    ;;

    (** []
        @raise Errors.Invalid_Sort_Type if [x.mind_sort] is not [Type] or [Set]
    *)
    let assert_mip_arity_is_type_or_set (mip : Declarations.one_inductive_body)
      : unit mm
      =
      (* Log.trace __FUNCTION__; *)
      match mip.mind_sort with
      | Type _ -> return ()
      | Set -> return ()
      | _ -> Err.invalid_sort_type (Sorts.quality mip.mind_sort)
    ;;

    (** []
        @raise Errors.Invalid_Sort_LTS if [x.mind_sort] is not [Prop] *)
    let assert_mip_arity_is_prop (mip : Declarations.one_inductive_body)
      : unit mm
      =
      (* Log.trace __FUNCTION__; *)
      match mip.mind_sort with
      | Prop -> return ()
      | _ -> Err.invalid_sort_lts (Sorts.quality mip.mind_sort)
    ;;

    (** []
        @raise Errors.Invalid_Ref_LTS if [x] is not [Names.GlobRef.IndRef] *)
    let lts_mind
      : Names.GlobRef.t -> (Names.inductive * Declarations.mind_specif) mm
      =
      (* Log.trace __FUNCTION__; *)
      function
      | Names.GlobRef.IndRef ind ->
        let open Syntax in
        let* (mib, mip) : Declarations.mind_specif = lookup ind in
        (ind, (mib, mip)) |> return
      | x -> Err.invalid_ref_lts x
    ;;

    (** [] *)
    let lts_type_mind (x : Names.GlobRef.t)
      : (Names.inductive * Declarations.mind_specif) mm
      =
      (* Log.trace __FUNCTION__; *)
      let open Syntax in
      let* ind, (mib, mip) = lts_mind x in
      let* () = assert_mip_arity_is_type_or_set mip in
      (ind, (mib, mip)) |> return
    ;;

    (** [] *)
    let lts_prop_mind (x : Names.GlobRef.t)
      : (Names.inductive * Declarations.mind_specif) mm
      =
      (* Log.trace __FUNCTION__; *)
      let open Syntax in
      let* ind, (mib, mip) = lts_mind x in
      let* () = assert_mip_arity_is_prop mip in
      (ind, (mib, mip)) |> return
    ;;

    (** []
        @raise Errors.Invalid_Arity if ... *)
    let lts_labels_and_terms ((mib, mip) : Declarations.mind_specif)
      : (Constr.rel_declaration * Constr.rel_declaration) mm
      =
      (* Log.trace __FUNCTION__; *)
      (* get the type of [mip] from [mib]. *)
      let typ = Inductive.type_of_inductive (UVars.in_punivs (mib, mip)) in
      match mip.mind_arity_ctxt |> Utils.split_at mip.mind_nrealdecls with
      | [ t1; a; t2 ] ->
        let open Context.Rel in
        if Declaration.equal Sorts.relevance_equal Constr.equal t1 t2
        then return (a, t1)
        else Err.invalid_arity typ
      | _ -> Err.invalid_arity typ
    ;;

    exception Mip_InconsistentNumConstructors of Declarations.one_inductive_body

    let mip_to_lts_constructors (mip : Declarations.one_inductive_body)
      : LTS.constructor array
      =
      Log.trace __FUNCTION__;
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

    (** [] *)
    let lts (x : Names.GlobRef.t) : t mm =
      (* Log.trace __FUNCTION__; *)
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

  (*********************************************************)

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

  module Constructor : sig
    include module type of Enc.Constructor_tree

    val encode : EConstr.t -> EConstr.t -> Enc.Tree.t -> t
  end = struct
    include Enc.Constructor_tree

    let encode (act : EConstr.t) (goto : EConstr.t) (tree : Enc.Tree.t) : t =
      Log.trace __FUNCTION__;
      let act : Enc.t = encode act in
      let goto : Enc.t = encode goto in
      act, goto, tree
    ;;
  end

  let make_state_tree_pair_set ()
    : (module Set.S with type elt = Enc.t * Enc.Tree.t)
    =
    Log.trace __FUNCTION__;
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
    (* module type SPair = sig
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
    end *)

    module Pair = struct
      (** [fst] is a term (e.g., destination) that we want to check unifies with [snd] (which we have already reached).
      @see Mebi_setup.unif_problem where [type unif_problem = {termL:EConstr.t;termR:EConstr.t}] *)
      type t =
        { to_check : EConstr.t
        ; acc : EConstr.t
        }

      include
        Json.Thing.Make
          (Log)
          (struct
            type k = t

            let name = "Pair"

            let json ?as_elt ({ to_check; acc } : t) : Yojson.t =
              `Assoc
                [ "to_check", `String (Strfy.econstr to_check)
                ; "acc", `String (Strfy.econstr acc)
                ]
            ;;
          end)

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

      (** [unify a b] tries to unify [a] and [b] within the context of the [env] and [sigma] of [mm]. @returns [true] if successful, [false] otherwise. *)
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

      include
        Json.Thing.Make
          (Log)
          (struct
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

      let unify_pair_opt (pair : Pair.t) : bool mm =
        state (fun env sigma -> Pair.unify env sigma pair)
      ;;

      let unify_opt ({ act; goto; tree } : t) : Enc.Tree.t option mm =
        let open Syntax in
        let* unified_act_opt = unify_pair_opt act in
        let* unified_goto_opt = unify_pair_opt goto in
        match unified_act_opt, unified_goto_opt with
        | true, true -> return (Some tree)
        | _, _ -> return None
      ;;

      (** creates unification problems between the rhs of the current constructor and the lhs of the next, along with the actions of both.
          (* NOTE: this is only relevant when deciding whether to explore a given constructor from a premise of another *)
      *)
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

    (* module type SProblems = sig
      type t =
        { sigma : Evd.evar_map
        ; to_unify : Problem.t list
        }

      val empty : unit -> t mm
      val list_is_empty : t list -> bool
      val to_string : Environ.env -> t -> string
      val list_to_string : Environ.env -> t list -> string

      val sandbox_unify_all_opt
        :  EConstr.t
        -> EConstr.t
        -> t
        -> (EConstr.t * EConstr.t * Enc.Tree.t list) option mm
    end *)

    module Problems = struct
      type t =
        { sigma : Evd.evar_map
        ; to_unify : Problem.t list
        }

      include
        Json.Thing.Make
          (Log)
          (struct
            type k = t

            let name = "Problems"

            let json ?as_elt ({ sigma; to_unify } : t) : Yojson.t =
              `Assoc
                [ ( "to_unify"
                  , `List (List.map (Problem.json ~as_elt:true) to_unify) )
                ]
            ;;
          end)

      let empty () : t mm =
        let open Syntax in
        let* sigma = get_sigma in
        return { sigma; to_unify = [] }
      ;;

      let is_empty : t -> bool = function
        | { to_unify = []; _ } -> true
        | _ -> false
      ;;

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

      let sandbox_unify_all_opt
            (act : EConstr.t)
            (goto : EConstr.t)
            ({ sigma; to_unify } : t)
        : (EConstr.t * EConstr.t * Enc.Tree.t list) option mm
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

    module ListOfProblems = struct
      type t = Problems.t list

      include
        Json.List.Make
          (Log)
          (struct
            include Problems

            let name = "Problems"
          end)

      let is_empty : t -> bool =
        Log.trace __FUNCTION__;
        function [] -> true | [ p ] -> Problems.is_empty p | _ :: _ -> false
      ;;

      let cross_product ({ sigma; to_unify } : Problems.t) : t -> t =
        Log.trace __FUNCTION__;
        List.concat_map (fun ({ to_unify = xs; _ } : Problems.t) : t ->
          List.map
            (fun (y : Problem.t) : Problems.t -> { sigma; to_unify = y :: xs })
            to_unify)
      ;;
    end

    module Constructors = struct
      include Enc.Constructor_trees

      let rec retrieve
                (constructor_index : int)
                (acc : t)
                (act : EConstr.t)
                (tgt : EConstr.t)
        : Enc.t * ListOfProblems.t -> t mm
        =
        Log.trace __FUNCTION__;
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
                 let tree : Enc.Tree.t =
                   N ((lts_enc, constructor_index), constructor_trees)
                 in
                 let constructor = Constructor.encode act goto tree in
                 return (Some constructor))
          in
          (match constructor_opt with
           | None -> return acc
           | Some constructor -> return (constructor :: acc))
      ;;

      let to_problems args (constructors : t) : Problems.t mm =
        Log.trace __FUNCTION__;
        let open Syntax in
        let* sigma = get_sigma in
        let to_unify : Problem.t list =
          List.map (Problem.of_constructor args) constructors
        in
        let p : Problems.t = { sigma; to_unify } in
        return p
      ;;

      let axiom
            (act : EConstr.t)
            (tgt : EConstr.t)
            (constructor_index : Enc.t * int)
            (constructors : t)
        : t mm
        =
        Log.trace __FUNCTION__;
        log_econstr ~__FUNCTION__ ~s:"act" act;
        log_econstr ~__FUNCTION__ ~s:"tgt" tgt;
        let open Syntax in
        let* is_evar : bool = econstr_is_evar tgt in
        if is_evar
        then return constructors
        else (
          let tree : Enc.Tree.t = N (constructor_index, []) in
          let axiom : Constructor.t = Constructor.encode act tgt tree in
          return (axiom :: constructors))
      ;;
    end

    (*********************************************************)

    let check_constructor_args_unify
          (lhs : EConstr.t)
          (act : EConstr.t)
          (args : Rocq_utils.constructor_args)
      : bool mm
      =
      Log.trace __FUNCTION__;
      let open Syntax in
      let* lhs_unifies : bool = Pair.unifies args.lhs lhs in
      if lhs_unifies then Pair.unifies args.act act else return false
    ;;

    (** Checks possible transitions for this term: *)
    let rec check_valid_constructors
              (constructors : Ind.LTS.constructor array)
              (indmap : Ind.t F.t)
              (from_term : EConstr.t)
              (act_term : EConstr.t)
              (lts_enc : Enc.t)
      : Constructors.t mm
      =
      Log.trace __FUNCTION__;
      (* log_econstr ~__FUNCTION__ ~s:"from_term" from_term; *)
      (* log_econstr ~__FUNCTION__ ~s:"act_term" act_term; *)
      let open Syntax in
      let* from_term : EConstr.t = econstr_normalize from_term in
      (* log_econstr ~__FUNCTION__ ~s:"from_term (normalized)" from_term; *)
      let iter_body (i : int) (acc : Constructors.t) : Constructors.t mm =
        (* NOTE: extract args for constructor *)
        let { constructor = ctx, tm; _ } : Ind.LTS.constructor =
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

    (** *)
    and explore_valid_constructor
          (indmap : Ind.t F.t)
          (from_term : EConstr.t)
          (lts_enc : Enc.t)
          (args : Rocq_utils.constructor_args)
          ((i, constructors) : int * Constructors.t)
          ((substl, decls) : EConstr.Vars.substl * EConstr.rel_declaration list)
      : Constructors.t mm
      =
      Log.trace __FUNCTION__;
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

    (* Should return a list of unification problems *)
    and check_updated_ctx
          (lts_enc : Enc.t)
          (acc : ListOfProblems.t)
          (indmap : Ind.t F.t)
      :  EConstr.Vars.substl * EConstr.rel_declaration list
      -> (Enc.t * ListOfProblems.t) option mm
      =
      Log.trace __FUNCTION__;
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
           (match F.find_opt indmap name with
            | None ->
              (* log_econstr ~__FUNCTION__ ~m:Warning ~s:"name not indmap" name; *)
              check_updated_ctx lts_enc acc indmap (substl, tl)
            | Some c ->
              let args : Rocq_utils.constructor_args =
                Rocq_utils.constructor_args args
              in
              let$+ lhs env sigma = Reductionops.nf_evar sigma args.lhs in
              let$+ act env sigma = Reductionops.nf_evar sigma args.act in
              let args = { args with lhs; act } in
              let next_lts : Ind.LTS.constructor array =
                Ind.get_lts_constructor_types c
              in
              let* next_constructors : Constructors.t =
                check_valid_constructors next_lts indmap lhs act c.enc
              in
              (match next_constructors with
               | [] -> return None
               | next_constructors ->
                 let* problems : Problems.t =
                   Constructors.to_problems args next_constructors
                 in
                 let acc = ListOfProblems.cross_product problems acc in
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
      : (Enc.t * ListOfProblems.t) option -> Constructors.t mm
      =
      Log.trace __FUNCTION__;
      function
      | None -> return constructors
      | Some (next_lts_enc, next_problems) ->
        if ListOfProblems.is_empty next_problems
        then
          Constructors.axiom outer_act tgt_term (next_lts_enc, i) constructors
        else
          Constructors.retrieve
            i
            constructors
            outer_act
            tgt_term
            (next_lts_enc, next_problems)
    ;;

    let collect_valid_constructors
          (constructors : Ind.LTS.constructor array)
          (indmap : Ind.t F.t)
          (from_term : EConstr.t)
          (label_type : EConstr.t)
          (lts_enc : Enc.t)
      : Constructors.t mm
      =
      Log.trace __FUNCTION__;
      let open Syntax in
      let* fresh_evar = fresh_evar (OfType label_type) in
      check_valid_constructors constructors indmap from_term fresh_evar lts_enc
    ;;
  end

  let make_enc_hashtbl () : (module Hashtbl.S with type key = Enc.t) =
    (module Hashtbl.Make (Enc))
  ;;

  let make_enc_set () : (module Set.S with type elt = Enc.t) =
    (module Set.Make (Enc))
  ;;

  let make_econstr_set () : (module Set.S with type elt = EConstr.t) =
    Log.trace __FUNCTION__;
    (module Set.Make (struct
         type t = EConstr.t

         let compare (a : t) (b : t) : int =
           let a = encode a in
           let b = encode b in
           Enc.compare a b
         ;;
       end))
  ;;

  module Bindings = struct
    (* module Log =
       Logger.ReMake
       (Log)
       (struct
       let level : Output.Kind.level -> bool = function
       | Debug -> true
       | Info -> true
       | Notice -> true
       | Warning -> true
       | Error -> true
       ;;

       let special : Output.Kind.special -> bool = function
       | Trace -> false
       | Result -> true
       | Show -> true
       ;;
       end) *)

    module Instructions = struct
      type t =
        | Undefined
        | Done
        | Arg of
            { root : Constr.t
            ; index : int
            ; cont : t
            }

      include
        Json.Thing.Make
          (Log)
          (struct
            type k = t

            let name = "Instructions"

            let json ?as_elt (x : t) : Yojson.t =
              let rec f : t -> Yojson.t = function
                | Undefined -> `String "Undefined"
                | Done -> `String "Done"
                | Arg { root; index; cont } ->
                  `Assoc
                    [ "root", `String (Strfy.constr root)
                    ; "index", `Int index
                    ; "cont", f cont
                    ]
              in
              f x
            ;;
          end)

      exception Rocq_bindings_CannotAppendDone of unit

      let rec append (x : t) : t -> t
        =
        (* Log.trace __FUNCTION__; *)
        function
        | Arg { root; index; cont } -> Arg { root; index; cont = append x cont }
        | Undefined -> x
        | Done -> raise (Rocq_bindings_CannotAppendDone ())
      ;;

      let rec length : t -> int =
        (* Log.trace __FUNCTION__; *)
        function
        | Undefined -> 0
        | Done -> -1
        | Arg { cont; _ } -> 1 + length cont
      ;;
    end

    module ConstrMap = struct
      module Map_ = Hashtbl.Make (struct
          type t = Constr.t

          let equal : t -> t -> bool = Constr.equal
          let hash : t -> int = Constr.hash
        end)

      include Map_

      type v = Names.Name.t * Instructions.t
      type t' = v t

      include
        Json.Map.Make
          (Log)
          (struct
            module Map = Map_

            type value = v

            let name = "ConstrMap"
            let kname = "Constr"
            let vname = "NamedInstructions"
            let kjson ?(as_elt : bool = false) k = `String (Strfy.constr k)

            let vjson ?(as_elt : bool = false) v =
              `Assoc
                [ "name", `String (Rocq_utils.Strfy.name (fst v))
                ; "instructions", Instructions.json ~as_elt:true (snd v)
                ]
            ;;
          end)

      let update (cmap : t') (k : Constr.t) ((name, inst) : v) : unit =
        Log.trace __FUNCTION__;
        match find_opt cmap k with
        | None -> add cmap k (name, inst)
        | Some (name', inst') ->
          let f = Instructions.length in
          (match Int.compare (f inst) (f inst') with
           | -1 -> replace cmap k (name, inst)
           | _ -> ())
      ;;

      exception Rocq_bindings_CannotFindBindingName of EConstr.t

      let find_name
            (name_pairs : (EConstr.t * Names.Name.t) list)
            (x : EConstr.t)
        : Names.Name.t mm
        =
        (* Log.trace __FUNCTION__; *)
        Log.thing ~__FUNCTION__ Debug "x" x (Of Strfy.econstr);
        let open Syntax in
        let f (i : int) : Names.Name.t option -> Names.Name.t option mm
          = function
          | Some n ->
            Log.thing ~__FUNCTION__ Trace "Some" n (Of Rocq_utils.Strfy.name);
            Some n |> return
          | None ->
            Log.trace ~__FUNCTION__ "None";
            let y, z = List.nth name_pairs i in
            let* eq = econstr_eq ~enc:false x y in
            if eq
            then (
              Log.thing ~__FUNCTION__ Debug "eq x" z (Of Rocq_utils.Strfy.name);
              Some z |> return)
            else return None
        in
        let* matches = iterate 0 (List.length name_pairs - 1) None f in
        match matches with
        | None ->
          Log.trace
            ~__FUNCTION__
            "Raise (Rocq_bindings_CannotFindBindingName x)";
          raise (Rocq_bindings_CannotFindBindingName x)
        | Some n ->
          Log.trace ~__FUNCTION__ "Some (_, n)";
          return n
      ;;

      let extract_binding_map
            (name_pairs : (EConstr.t * Names.Name.t) list)
            (x : EConstr.t)
            (y : Constr.t)
        : t' mm
        =
        Log.trace __FUNCTION__;
        let open Syntax in
        let m : t' = create 0 in
        let rec f
                  (acc : (Constr.t * v) list)
                  (b : Instructions.t)
                  ((x, y) : EConstr.t * Constr.t)
          : unit mm
          =
          Log.trace __FUNCTION__;
          let* x_kind = econstr_kind x in
          match x_kind, Constr.kind y with
          | App (xty, xtys), App (yty, ytys) ->
            let* eq = econstr_eq ~enc:false xty (EConstr.of_constr yty) in
            if eq
            then (
              (* NOTE: set to [-1] so that it is [0] on first use. *)
              let (tysindex, _), _ = Utils.new_int_counter ~start:(-1) () in
              let xytys = Array.combine xtys ytys in
              let iter_body (i : int) () =
                Log.trace __FUNCTION__;
                let b' =
                  Instructions.append
                    (Arg { root = yty; index = tysindex (); cont = Undefined })
                    b
                in
                f acc b' xytys.(i)
              in
              iterate 0 (Array.length xytys - 1) () iter_body)
            else return ()
          | _, Rel _ ->
            let* name = find_name name_pairs x in
            update m y (name, Instructions.append Done b);
            return ()
          | _, _ -> return ()
        in
        let* () = f [] Undefined (x, y) in
        return m
      ;;

      let make_opt
            (name_pairs : (EConstr.t * Names.Name.t) list)
            ((evar, rel) : EConstr.t * Constr.t)
        : t' option mm
        =
        Log.trace __FUNCTION__;
        let open Syntax in
        let* m = extract_binding_map name_pairs evar rel in
        match to_seq_values m |> List.of_seq with
        | [] -> return None
        | [ (_, Instructions.Done) ] -> return None
        | _ :: _ -> return (Some m)
      ;;
    end

    type t =
      | No_Bindings
      | Use_Bindings of
          { from : ConstrMap.t' option
          ; action : ConstrMap.t' option
          ; goto : ConstrMap.t' option
          }

    include
      Json.Thing.Make
        (Log)
        (struct
          type k = t

          let name = "Bindings"

          let json ?as_elt : t -> Yojson.t = function
            | No_Bindings -> `String "NoBindings"
            | Use_Bindings { from; action; goto } ->
              `Assoc
                [ "from", Json.option ~as_elt:true ConstrMap.json from
                ; "action", Json.option ~as_elt:true ConstrMap.json action
                ; "goto", Json.option ~as_elt:true ConstrMap.json goto
                ]
          ;;
        end)

    let use_no_bindings (xs : ConstrMap.t' option list) : bool =
      Log.trace __FUNCTION__;
      List.filter (function None -> false | _ -> true) xs |> List.is_empty
    ;;

    (** [] ...
        @param name_map is a tuple list of evars and corresponding binding_names
    *)
    let extract
          (name_pairs : (EConstr.t * Names.Name.t) list)
          (from : EConstr.t * Constr.t)
          (action : EConstr.t * Constr.t)
          (goto : EConstr.t * Constr.t)
      : t mm
      =
      Log.trace __FUNCTION__;
      let open Syntax in
      let f = ConstrMap.make_opt name_pairs in
      let* from : ConstrMap.t' option = f from in
      let* action : ConstrMap.t' option = f action in
      let* goto : ConstrMap.t' option = f goto in
      if use_no_bindings [ from; action; goto ]
      then return No_Bindings
      else return (Use_Bindings { from; action; goto })
    ;;
  end

  module ConstructorBindings = struct
    type t =
      { index : int
      ; name : string
      ; bindings : Bindings.t
      }

    include
      Json.Thing.Make
        (Log)
        (struct
          type k = t

          let name = "ConstructorBindings"

          let json ?as_elt (x : t) : Yojson.t =
            `Assoc
              [ "index", `Int x.index
              ; "name", `String x.name
              ; "bindings", Bindings.json ~as_elt:true x.bindings
              ]
          ;;
        end)

    let extract_info (x : Ind.t) : t list mm =
      Log.trace __FUNCTION__;
      let open Syntax in
      (* NOTE: constructor tactic index starts from 1 -- ignore 0 below *)
      let (get_constructor_index, _), _ = Utils.new_int_counter ~start:0 () in
      let tys : Ind.LTS.constructor array = Ind.get_lts_constructor_types x in
      let f (i : int) (acc : t list) : t list mm =
        Log.trace __FUNCTION__;
        let { name; constructor = ctx, c } : Ind.LTS.constructor = tys.(i) in
        let index : int = get_constructor_index () in
        let name : string = Names.Id.to_string name in
        let decls : Rocq_utils.econstr_decl list =
          Rocq_utils.get_econstr_decls ctx
        in
        let* substl = mk_ctx_substl [] (List.rev decls) in
        let name_pairs = Rocq_utils.map_decl_evar_pairs decls substl in
        let args : Rocq_utils.constructor_args =
          Rocq_utils.extract_args ~substl c
        in
        let from, action, goto =
          Rocq_utils.constr_to_app c |> Rocq_utils.unpack_constr_args
        in
        let* bindings : Bindings.t =
          Bindings.extract
            name_pairs
            (args.lhs, from)
            (args.act, action)
            (args.rhs, goto)
        in
        let h : t = { index; name; bindings } in
        h :: acc |> return
      in
      iterate 0 (Array.length tys - 1) [] f
    ;;

    (***********************************************************************)

    let get_quantified_hyp : Names.Name.t -> Tactypes.quantified_hypothesis =
      Log.trace __FUNCTION__;
      function
      | Names.Name.Anonymous -> Tactypes.AnonHyp (* FIXME: *) 0
      | Names.Name.Name v -> Tactypes.NamedHyp (CAst.make v)
    ;;

    exception Rocq_bindings_BindingInstruction_NotApp of EConstr.t

    exception
      Rocq_bindings_BindingInstruction_Undefined of EConstr.t * EConstr.t

    exception
      Rocq_bindings_BindingInstruction_IndexOutOfBounds of EConstr.t * int

    exception Rocq_bindings_BindingInstruction_NEQ of EConstr.t * Constr.t

    let rec get_bound_term (x : EConstr.t)
      : Bindings.Instructions.t -> EConstr.t mm
      =
      Log.trace __FUNCTION__;
      function
      | Undefined -> raise (Rocq_bindings_BindingInstruction_Undefined (x, x))
      | Done -> return x
      | Arg { root; index; cont } ->
        (try
           let open Syntax in
           let* kind = econstr_kind x in
           match kind with
           | App (xty, xtys) ->
             let* eq = econstr_eq ~enc:false xty (EConstr.of_constr root) in
             if eq
             then (
               try get_bound_term xtys.(index) cont with
               | Invalid_argument _ ->
                 raise
                   (Rocq_bindings_BindingInstruction_IndexOutOfBounds (x, index)))
             else raise (Rocq_bindings_BindingInstruction_NEQ (xty, root))
           | _ -> raise (Rocq_bindings_BindingInstruction_NotApp x)
         with
         | Rocq_bindings_BindingInstruction_Undefined (_, y) ->
           raise (Rocq_bindings_BindingInstruction_Undefined (x, y)))
    ;;

    let get_explicit_bindings
      :  EConstr.t * Bindings.ConstrMap.t' option
      -> EConstr.t Tactypes.explicit_bindings mm
      =
      Log.trace __FUNCTION__;
      function
      | _, None -> return []
      | x, Some xmap ->
        let open Syntax in
        let ys = Bindings.ConstrMap.to_seq_values xmap |> Array.of_seq in
        let f (i : int) (acc : EConstr.t Tactypes.explicit_bindings) =
          Log.trace __FUNCTION__;
          let name, inst = ys.(i) in
          let q = get_quantified_hyp name in
          let* bs = get_bound_term x inst in
          CAst.make (q, bs) :: acc |> return
        in
        iterate 0 (Array.length ys - 1) [] f
    ;;

    let get
          (from' : EConstr.t)
          (action' : EConstr.t option)
          (goto' : EConstr.t option)
      : Bindings.t -> EConstr.t Tactypes.bindings mm
      =
      Log.trace __FUNCTION__;
      function
      | No_Bindings -> return Tactypes.NoBindings
      | Use_Bindings { from; action; goto } ->
        let acc_some
              (acc : (EConstr.t * Bindings.ConstrMap.t' option) list)
              (x : Bindings.ConstrMap.t' option)
          : EConstr.t option -> (EConstr.t * Bindings.ConstrMap.t' option) list
          = function
          | None -> acc
          | Some y -> (y, x) :: acc
        in
        let to_iter : (EConstr.t * Bindings.ConstrMap.t' option) list =
          acc_some (acc_some [ from', from ] action action') goto goto'
        in
        let open Syntax in
        let* bindings : EConstr.t Tactypes.explicit_bindings =
          let f (i : int) acc =
            Log.trace __FUNCTION__;
            let* x = get_explicit_bindings (List.nth to_iter i) in
            x :: acc |> return
          in
          let* xs = iterate 0 (List.length to_iter - 1) [] f in
          List.flatten xs |> return
        in
        (match bindings with
         | [] -> return Tactypes.NoBindings
         | xs -> return (Tactypes.ExplicitBindings xs))
    ;;
  end
end
