type term = EConstr.t

(* val enable_logging : bool ref *)

type coq_context =
  { coq_env : Environ.env
  ; coq_ctx : Evd.evar_map
  }

val coq_env_wrapper : Environ.env ref option ref
val new_coq_env : unit -> Environ.env ref
val the_coq_env : ?fresh:bool -> unit -> Environ.env ref
val coq_ctx_wrapper : Evd.evar_map ref option ref
val new_coq_ctx : ?fresh:bool -> unit -> Evd.evar_map ref
val the_coq_ctx : ?fresh:bool -> unit -> Evd.evar_map ref

module F : sig
  type key = term
  type !'a t

  val create : int -> 'a t
  val clear : 'a t -> unit
  val reset : 'a t -> unit
  val copy : 'a t -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val remove : 'a t -> key -> unit
  val find : 'a t -> key -> 'a
  val find_opt : 'a t -> key -> 'a option
  val find_all : 'a t -> key -> 'a list
  val replace : 'a t -> key -> 'a -> unit
  val mem : 'a t -> key -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
  val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
  val length : 'a t -> int
  val stats : 'a t -> Hashtbl.statistics
  val to_seq : 'a t -> (key * 'a) Seq.t
  val to_seq_keys : 'a t -> key Seq.t
  val to_seq_values : 'a t -> 'a Seq.t
  val add_seq : 'a t -> (key * 'a) Seq.t -> unit
  val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
  val of_seq : (key * 'a) Seq.t -> 'a t
end

module type ENCODING_TYPE = sig
  type t

  val init : t
  val cache : t ref
  val reset : unit -> unit
  val eq : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val to_string : t -> string
  val of_int : int -> t

  module type ENC_TBL = sig
    type key = t
    type !'a t

    val create : int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_opt : 'a t -> key -> 'a option
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
    val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
    val length : 'a t -> int
    val stats : 'a t -> Hashtbl.statistics
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_keys : 'a t -> key Seq.t
    val to_seq_values : 'a t -> 'a Seq.t
    val add_seq : 'a t -> (key * 'a) Seq.t -> unit
    val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
    val of_seq : (key * 'a) Seq.t -> 'a t
  end

  module Tbl : ENC_TBL

  val encode : t F.t -> term Tbl.t -> term -> t

  exception InvalidDecodeKey of (t * term Tbl.t)

  val decode : term Tbl.t -> t -> term
end

module IntEncoding : ENCODING_TYPE
module E = IntEncoding
module B = E.Tbl

type wrapper =
  { coq_ref : coq_context ref
  ; fwd_enc : E.t F.t
  ; bck_enc : term B.t
  }

type 'a in_context =
  { state : wrapper ref
  ; value : 'a
  }

type 'a mm = wrapper ref -> 'a in_context

val run : ?keep_encoding:bool -> 'a mm -> 'a
val return : 'a -> 'a mm
val bind : 'a mm -> ('a -> 'b mm) -> 'b mm
val map : ('a -> 'b) -> 'a mm -> 'b mm
val product : 'a mm -> 'b mm -> ('a * 'b) mm
val iterate : int -> int -> 'a -> (int -> 'a -> 'a mm) -> 'a mm

module type ERROR_TYPE = sig
  type mebi_error =
    | InvalidLTSArgsLength of int
    | InvalidLTSTermKind of Environ.env * Evd.evar_map * Constr.t
    | InvalidLTSSort of Sorts.family
    | InvalidTypeSort of Sorts.family
    | InvalidArity of Environ.env * Evd.evar_map * Constr.t
    | InvalidRefLTS of Names.GlobRef.t
    | InvalidRefType of Names.GlobRef.t
    | UnknownTermType of
        (Environ.env * Evd.evar_map * (term * term * term list))
    | PrimaryLTSNotFound of (Environ.env * Evd.evar_map * term * term list)
    | UnknownDecodeKey of (Environ.env * Evd.evar_map * E.t * term B.t)
    | ExpectedCoqIndDefOfLTSNotType of unit
    | InvalidCheckUpdatedCtx of
        (Environ.env
        * Evd.evar_map
        * EConstr.t list
        * EConstr.rel_declaration list)

  exception MEBI_exn of mebi_error

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
    -> term * term * term list
    -> exn

  val primary_lts_not_found
    :  Environ.env
    -> Evd.evar_map
    -> term
    -> term list
    -> exn

  val unknown_decode_key : Environ.env -> Evd.evar_map -> E.t -> term B.t -> exn

  val invalid_check_updated_ctx
    :  Environ.env
    -> Evd.evar_map
    -> EConstr.t list
    -> EConstr.rel_declaration list
    -> exn

end

module Error : ERROR_TYPE

val invalid_lts_args_length : int -> 'a mm
val invalid_lts_term_kind : Constr.t -> 'a mm
val invalid_sort_lts : Sorts.family -> 'a mm
val invalid_sort_type : Sorts.family -> 'a mm
val invalid_arity : Constr.t -> 'a mm
val invalid_ref_lts : Names.GlobRef.t -> 'a mm
val invalid_ref_type : Names.GlobRef.t -> 'a mm
val invalid_cindef_kind : 'b -> 'a mm

val unknown_term_type : term * term * term list -> 'a mm
val primary_lts_not_found : term * term list -> 'a mm
val unknown_decode_key : E.t * term B.t -> 'a mm
val invalid_check_updated_ctx : term list -> EConstr.rel_declaration list -> 'a mm

val get_env : wrapper ref -> Environ.env in_context
val get_sigma : wrapper ref -> Evd.evar_map in_context
val get_fwd_enc : wrapper ref -> E.t F.t in_context
val get_bck_enc : wrapper ref -> term B.t in_context

val state
  :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
  -> wrapper ref
  -> 'a in_context

val sandbox : 'a mm -> wrapper ref -> 'a in_context
val debug : (Environ.env -> Evd.evar_map -> Pp.t) -> unit mm

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

val encode : term -> E.t mm
val decode : E.t -> term mm
val encode_opt : term -> E.t option mm
val decode_opt : E.t -> term option mm
val has_encoding : term -> bool mm
val has_decoding : E.t -> bool mm
val encode_map : 'a F.t -> 'a B.t mm
val decode_map : 'a B.t -> 'a F.t mm
val constr_to_string : Constr.t -> string
val econstr_to_string : EConstr.t -> string
val constr_list_to_string : Constr.t list -> string
val econstr_list_to_string : EConstr.t list -> string

val constr_rel_decl_to_string : Constr.rel_declaration -> string
val econstr_rel_decl_to_string : EConstr.rel_declaration -> string
val constr_rel_decl_list_to_string : Constr.rel_declaration list -> string
val econstr_rel_decl_list_to_string : EConstr.rel_declaration list -> string

val debug_encoding : unit -> unit mm

module Constr_tree : sig
  type 'a tree = Node of 'a * 'a tree list
  type t = (E.t * int) tree

  val eq : t -> t -> bool
  val compare : t -> t -> int
  val pstr : t -> string
end

type decoded_tree = (string * int) Constr_tree.tree

val decode_constr_tree_lts : Constr_tree.t -> decoded_tree mm
val pstr_decoded_tree : decoded_tree -> string
val tref_to_econstr : Constrexpr.constr_expr -> term mm
val normalize_econstr : term -> term mm
val type_of_econstr : term -> term mm
val type_of_tref : Constrexpr.constr_expr -> term mm

val make_transition_tbl
  :  wrapper ref
  -> (module Hashtbl.S with type key = E.t) in_context

val make_state_set
  :  wrapper ref
  -> (module Set.S with type elt = E.t) in_context

val make_state_tree_pair_set
  :  wrapper ref
  -> (module Set.S with type elt = E.t * Constr_tree.t) in_context
