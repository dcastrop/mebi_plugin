module Enc = Mebi_setup.Enc
module F = Enc.F
module B = Enc.B

type fwdmap = Enc.t F.t
type bckmap = Evd.econstr B.t

val the_enc_maps_cache : (fwdmap * bckmap) ref option ref
val reset_enc_maps : unit -> (fwdmap * bckmap) ref
val get_the_enc_maps : ?keep_encoding:bool -> unit -> (fwdmap * bckmap) ref

type wrapper =
  { coq_ref : Mebi_setup.coq_context ref
  ; fwd_enc : fwdmap
  ; bck_enc : bckmap
  }

type 'a in_context =
  { state : wrapper ref
  ; value : 'a
  }

type 'a mm = wrapper ref -> 'a in_context

val run : ?keep_encoding:bool -> ?fresh:bool -> 'a mm -> 'a
val runkeep : 'a mm -> 'a
val return : 'a -> 'a mm
val bind : 'a mm -> ('a -> 'b mm) -> 'b mm
val map : ('a -> 'b) -> 'a mm -> 'b mm
val product : 'a mm -> 'b mm -> ('a * 'b) mm
val iterate : int -> int -> 'a -> (int -> 'a -> 'a mm) -> 'a mm
val get_env : wrapper ref -> Environ.env in_context
val get_sigma : wrapper ref -> Evd.evar_map in_context
val get_fwd_enc : wrapper ref -> fwdmap in_context
val get_bck_enc : wrapper ref -> bckmap in_context

val state
  :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
  -> wrapper ref
  -> 'a in_context

val sandbox : ?using:Evd.evar_map -> 'a mm -> wrapper ref -> 'a in_context

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

val mebi_to_string
  :  (Environ.env -> Evd.evar_map -> 'a -> string)
  -> 'a Utils.Strfy.to_string

val constr_to_string : Constr.t -> string
val econstr_to_string : Evd.econstr -> string
val enc_to_string : Enc.t -> string

module type ERROR_TYPE = sig
  type mebi_error =
    | Invalid_KindOfTypeEConstr_Expected_Atomic of
        (Mebi_setup.coq_context ref * Evd.econstr)
    | Invalid_KindOfTypeEConstr_Expected_Cast of
        (Mebi_setup.coq_context ref * Evd.econstr)
    | Invalid_KindOfTypeEConstr_Expected_LetIn of
        (Mebi_setup.coq_context ref * Evd.econstr)
    | Invalid_KindOfTypeEConstr_Expected_Prod of
        (Mebi_setup.coq_context ref * Evd.econstr)
    | Invalid_KindOfTypeEConstr_Expected_Sort of
        (Mebi_setup.coq_context ref * Evd.econstr)
    | UnknownEncodeKey of (Mebi_setup.coq_context ref * Enc.t F.t * Evd.econstr)
    | UnknownDecodeKey of (Mebi_setup.coq_context ref * bckmap * Enc.t)
    | NoBisimResult of unit
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
        (Environ.env
        * Evd.evar_map
        * (Evd.econstr * Evd.econstr * Evd.econstr list))
    | PrimaryLTSNotFound of
        (Environ.env * Evd.evar_map * Evd.econstr * Evd.econstr list)
    | ExpectedCoqIndDefOfLTSNotType of unit
    | InvalidCheckUpdatedCtx of
        (Environ.env
        * Evd.evar_map
        * Evd.econstr list
        * EConstr.rel_declaration list)

  exception MEBI_exn of mebi_error

  val invalid_kind_of_econstr_expected_atomic
    :  Mebi_setup.coq_context ref
    -> Evd.econstr
    -> exn

  val invalid_kind_of_econstr_expected_cast
    :  Mebi_setup.coq_context ref
    -> Evd.econstr
    -> exn

  val invalid_kind_of_econstr_expected_letin
    :  Mebi_setup.coq_context ref
    -> Evd.econstr
    -> exn

  val invalid_kind_of_econstr_expected_prod
    :  Mebi_setup.coq_context ref
    -> Evd.econstr
    -> exn

  val invalid_kind_of_econstr_expected_sort
    :  Mebi_setup.coq_context ref
    -> Evd.econstr
    -> exn

  val cannot_get_encoding_of_unencoded_econstr
    :  Mebi_setup.coq_context ref
    -> Enc.t F.t
    -> Evd.econstr
    -> exn

  val cannot_get_decoding_of_unencoded_econstr
    :  Mebi_setup.coq_context ref
    -> bckmap
    -> Enc.t
    -> exn

  val missing_bisim_result : unit -> exn
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
    -> Evd.econstr * Evd.econstr * Evd.econstr list
    -> exn

  val primary_lts_not_found
    :  Environ.env
    -> Evd.evar_map
    -> Evd.econstr
    -> Evd.econstr list
    -> exn

  val invalid_check_updated_ctx
    :  Environ.env
    -> Evd.evar_map
    -> Evd.econstr list
    -> EConstr.rel_declaration list
    -> exn
end

module Error : ERROR_TYPE

val invalid_kind_of_econstr_expected_atomic : Evd.econstr -> 'a mm
val invalid_kind_of_econstr_expected_cast : Evd.econstr -> 'a mm
val invalid_kind_of_econstr_expected_letin : Evd.econstr -> 'a mm
val invalid_kind_of_econstr_expected_prod : Evd.econstr -> 'a mm
val invalid_kind_of_econstr_expected_sort : Evd.econstr -> 'a mm
val cannot_get_encoding_of_unencoded_econstr : Evd.econstr -> 'a mm
val cannot_get_decoding_of_unencoded_econstr : Enc.t -> 'a mm
val missing_bisim_result : unit -> 'a mm
val params_fail_if_incomplete : unit -> 'a mm
val params_fail_if_not_bisim : unit -> 'a mm

val invalid_check_updated_ctx
  :  Evd.econstr list
  -> EConstr.rel_declaration list
  -> 'a mm

val invalid_lts_args_length : int -> 'a mm
val invalid_lts_term_kind : Constr.t -> 'a mm
val invalid_arity : Constr.t -> 'a mm
val invalid_sort_lts : Sorts.family -> 'a mm
val invalid_sort_type : Sorts.family -> 'a mm
val invalid_ref_lts : Names.GlobRef.t -> 'a mm
val invalid_ref_type : Names.GlobRef.t -> 'a mm
val invalid_cindef_kind : 'b -> 'a mm
val unknown_term_type : Evd.econstr * Evd.econstr * Evd.econstr list -> 'a mm
val primary_lts_not_found : Evd.econstr * Evd.econstr list -> 'a mm
val encode : Evd.econstr -> Enc.t mm
val encoding : Evd.econstr -> Enc.t mm
val has_encoding : Evd.econstr -> bool
val get_encoding : Evd.econstr -> Enc.t
val encoding_opt : Evd.econstr -> Enc.t option mm
val get_encoding_opt : Evd.econstr -> Enc.t option
val decode : Enc.t -> Evd.econstr mm
val decoding : Enc.t -> Evd.econstr mm
val has_decoding : Enc.t -> bool
val get_decoding : Enc.t -> Evd.econstr
val decoding_opt : Enc.t -> Evd.econstr option mm
val get_decoding_opt : Enc.t -> Evd.econstr option
val encode_map : 'a F.t -> 'a B.t mm
val decode_map : 'a B.t -> 'a F.t mm

type decoded_tree = (string * int) Mebi_constr.Tree.tree

val decode_constr_tree_lts : Mebi_constr.Tree.t -> decoded_tree mm
val debug : (Environ.env -> Evd.evar_map -> Pp.t) -> unit mm
val debug_str : (Environ.env -> Evd.evar_map -> string) -> string mm

val make_transition_tbl
  :  wrapper ref
  -> (module Hashtbl.S with type key = Enc.t) in_context

val make_state_set
  :  wrapper ref
  -> (module Set.S with type elt = Enc.t) in_context

val make_state_tree_pair_set
  :  wrapper ref
  -> (module Set.S with type elt = Enc.t * Mebi_constr.Tree.t) in_context

val debug_enc : unit -> unit mm
