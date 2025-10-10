module F = Mebi_setup.F
module E = Mebi_setup.E
module B = Mebi_setup.B

type wrapper =
  { coq_ref : Mebi_setup.coq_context ref
  ; fwd_enc : E.t F.t
  ; bck_enc : EConstr.t B.t
  }

type 'a in_context = 
  { state : wrapper ref
  ; value : 'a
  }

type 'a mm = wrapper ref -> 'a in_context

val run
  :  ?keep_encoding:bool
  -> ?fresh:bool
  -> ?new_proof:bool
  -> ?proof:Declare.Proof.t option
  -> 'a mm
  -> 'a

(* val string_mm : string mm -> string *)
val wrap : (Environ.env -> Evd.evar_map -> 'a -> string) -> 'a -> string

val return : 'a -> 'a mm
val bind : 'a mm -> ('a -> 'b mm) -> 'b mm
val map : ('a -> 'b) -> 'a mm -> 'b mm
val product : 'a mm -> 'b mm -> ('a * 'b) mm
val iterate : int -> int -> 'a -> (int -> 'a -> 'a mm) -> 'a mm

module type ERROR_TYPE = sig
  type mebi_error =
    | ProofvIsNone of unit
    | ParamsFailIfIncomplete of unit
    | ParamsFailIfNotBisim of unit
    | InvalidLTSArgsLength of int
    | InvalidLTSTermKind of Environ.env * Evd.evar_map * Constr.t
    | InvalidLTSSort of Sorts.family
    | InvalidTypeSort of Sorts.family
    | InvalidArity of Environ.env * Evd.evar_map * Constr.t
    | InvalidRefLTS of Names.GlobRef.t
    | InvalidRefType of Names.GlobRef.t
    | UnknownTermType of
        (Environ.env * Evd.evar_map * (EConstr.t * EConstr.t * EConstr.t list))
    | PrimaryLTSNotFound of (Environ.env * Evd.evar_map * EConstr.t * EConstr.t list)
    | UnknownDecodeKey of (Environ.env * Evd.evar_map * E.t * EConstr.t B.t)
    | ExpectedCoqIndDefOfLTSNotType of unit
    | InvalidCheckUpdatedCtx of
        (Environ.env
        * Evd.evar_map
        * EConstr.t list
        * EConstr.rel_declaration list)

  exception MEBI_exn of mebi_error

  val proofv_is_none : unit -> exn
  val params_fail_if_incomplete : unit -> exn
  val params_fail_if_not_bisim : unit -> exn
  val invalid_lts_args_length : int -> exn
  val invalid_lts_term_kind : Environ.env -> Evd.evar_map -> Constr.t -> exn
  val invalid_sort_lts : Sorts.family -> exn
  val invalid_sort_type : Sorts.family -> exn
  val invalid_arity : Environ.env -> Evd.evar_map -> Constr.t -> exn
  val invalid_ref_lts : Names.GlobRef.t -> exn
  val invalid_ref_type : Names.GlobRef.t -> exn
  val invalid_cindef_kind : unit -> exn

  val unknown_term_type
    :  Environ.env
    -> Evd.evar_map
    -> EConstr.t * EConstr.t * EConstr.t list
    -> exn

  val primary_lts_not_found
    :  Environ.env
    -> Evd.evar_map
    -> EConstr.t
    -> EConstr.t list
    -> exn

  val unknown_decode_key : Environ.env -> Evd.evar_map -> E.t -> EConstr.t B.t -> exn

  val invalid_check_updated_ctx
    :  Environ.env
    -> Evd.evar_map
    -> EConstr.t list
    -> EConstr.rel_declaration list
    -> exn
end

module Error : ERROR_TYPE

val proofv_is_none : unit -> 'a mm
val params_fail_if_incomplete : unit -> 'a mm
val params_fail_if_not_bisim : unit -> 'a mm
val invalid_lts_args_length : int -> 'a mm
val invalid_lts_term_kind : Constr.t -> 'a mm
val invalid_sort_lts : Sorts.family -> 'a mm
val invalid_sort_type : Sorts.family -> 'a mm
val invalid_arity : Constr.t -> 'a mm
val invalid_ref_lts : Names.GlobRef.t -> 'a mm
val invalid_ref_type : Names.GlobRef.t -> 'a mm
val invalid_cindef_kind : 'b -> 'a mm
val unknown_term_type : EConstr.t * EConstr.t * EConstr.t list -> 'a mm
val primary_lts_not_found : EConstr.t * EConstr.t list -> 'a mm
val unknown_decode_key : E.t * EConstr.t B.t -> 'a mm

val invalid_check_updated_ctx
  :  EConstr.t list
  -> EConstr.rel_declaration list
  -> 'a mm

val set_proof : Declare.Proof.t -> wrapper ref -> unit in_context
val get_env : wrapper ref -> Environ.env in_context
val get_sigma : wrapper ref -> Evd.evar_map in_context
val get_proofv : wrapper ref -> Mebi_setup.proof_context in_context
val get_fwd_enc : wrapper ref -> E.t F.t in_context
val get_bck_enc : wrapper ref -> EConstr.t B.t in_context

val state
  :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
  -> wrapper ref
  -> 'a in_context

val sandbox : 'a mm -> wrapper ref -> 'a in_context
val debug : (Environ.env -> Evd.evar_map -> Pp.t) -> unit mm
val show_proof_data : unit -> unit mm
val show_proof : unit -> unit mm
val show_names : unit -> unit mm

module type MEBI_MONAD_SYNTAX = sig
  val ( let+ ) : 'a mm -> ('a -> 'b) -> 'b mm
  val ( let* ) : 'a mm -> ('a -> 'b mm) -> 'b mm

  val ( let$ )
    :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
    -> ('a -> 'b mm)
    -> 'b mm

  val ( let$* )
    :  (Environ.env -> Evd.evar_map -> Evd.evar_map)
    -> (unit -> 'b mm)
    -> 'b mm

  val ( let$+ ) : (Environ.env -> Evd.evar_map -> 'a) -> ('a -> 'b mm) -> 'b mm
  val ( and+ ) : 'a mm -> 'b mm -> ('a * 'b) mm
end

module Syntax : MEBI_MONAD_SYNTAX

(* *)
val map_list_mm : ('a -> 'b mm) -> 'a list -> 'b list mm
val type_of_constrexpr : Constrexpr.constr_expr -> EConstr.t mm

val econstr_list_to_constr
  :  ?abort_on_undefined_evars:bool
  -> EConstr.t list
  -> Constr.t list mm

(* *)
val econstr_eq : EConstr.t -> EConstr.t -> bool mm

(* *)
val econstr_to_constr
  :  ?abort_on_undefined_evars:bool
  -> EConstr.t
  -> Constr.t mm

val econstr_to_constr_opt : EConstr.t -> Constr.t option mm
val constrexpr_to_econstr : Constrexpr.constr_expr -> EConstr.t mm
val globref_to_econstr : Names.GlobRef.t -> EConstr.t mm
val normalize_econstr : EConstr.t -> EConstr.t mm
val type_of_econstr : EConstr.t -> EConstr.t mm
val new_evar_of_econstr : EConstr.t -> EConstr.t mm

(* *)
val encode : EConstr.t -> E.t mm
val decode : E.t -> EConstr.t mm
val decode_to_string : E.t -> string
val get_encoding_opt : EConstr.t -> E.t option mm
val get_decoding_opt : E.t -> EConstr.t option mm
val has_encoding : EConstr.t -> bool mm
val has_decoding : E.t -> bool mm
val encode_map : 'a F.t -> 'a B.t mm
val decode_map : 'a B.t -> 'a F.t mm

(* *)
val is_none_term : EConstr.t -> bool mm

(* *)
val get_type_of_hyp : Names.Id.t -> EConstr.t mm

(* *)
val get_proof : unit -> Declare.Proof.t mm
val get_proof_env : unit -> Environ.env mm
val get_proof_sigma : unit -> Evd.evar_map mm
val get_proof_names : unit -> Names.Id.Set.t mm

val update_names
  :  ?replace:bool
  -> Names.Id.Set.t
  -> wrapper ref
  -> unit in_context

val add_name : Names.Id.t -> wrapper ref -> unit in_context
val next_name_of : Names.Id.t -> Names.Id.t mm
val new_name_of_string : ?add:bool -> string -> Names.Id.t mm
val update_proof_by_tactic : unit Proofview.tactic -> unit mm
val update_proof_by_tactic_mm : unit Proofview.tactic mm -> unit mm
val update_proof_by_tactics : unit Proofview.tactic list -> unit mm
val update_proof_by_tactics_mm : unit Proofview.tactic mm list -> unit mm

(* *)
val constr_to_string : Constr.t -> string
val econstr_to_string : EConstr.t -> string
val constr_rel_decl_to_string : Constr.rel_declaration -> string
val econstr_rel_decl_to_string : EConstr.rel_declaration -> string
val econstr_list_to_constr_opt_string : EConstr.t list -> string mm
val debug_encoding : unit -> unit mm

module Constr_tree : sig
  type 'a tree = Node of 'a * 'a tree list
  type t = (E.t * int) tree

  val eq : t -> t -> bool
  val compare : t -> t -> int
  val pstr : t -> string
end

type decoded_tree = (string * int) Constr_tree.tree

val pstr_decoded_tree : decoded_tree -> string
val decode_constr_tree_lts : Constr_tree.t -> decoded_tree mm

val make_transition_tbl
  :  wrapper ref
  -> (module Hashtbl.S with type key = E.t) in_context

val make_state_set
  :  wrapper ref
  -> (module Set.S with type elt = E.t) in_context

val make_state_tree_pair_set
  :  wrapper ref
  -> (module Set.S with type elt = E.t * Constr_tree.t) in_context

(* *)
val proof_query : Declare.Proof.t -> Proof.t
val proof_partial : Proof.t -> EConstr.t list

(* *)
val proof_test : unit -> unit Proofview.tactic mm

(* *)
val debug_econstr_kind : EConstr.t -> unit mm
val debug_constr_kind : Constr.t -> unit mm
val debug_econstr_constr_kind : EConstr.t -> unit mm
