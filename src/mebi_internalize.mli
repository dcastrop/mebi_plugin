type term = Evd.econstr

module type MEBI_MONAD = sig
  type coq_context =
    { coq_env : Environ.env
    ; coq_ctx : Evd.evar_map
    }

  type 'a in_coq_context =
    { state : coq_context ref
    ; value : 'a
    }

  type 'a cm = coq_context ref -> 'a in_coq_context

  val coq_run : 'a cm -> 'a
  val coq_return : 'a -> 'a cm
  val coq_bind : 'a cm -> ('a -> 'b cm) -> 'b cm
  val ( let|* ) : 'a cm -> ('a -> 'b cm) -> 'b cm
  val get_env : Environ.env cm
  val get_sigma : Evd.evar_map cm
  val init : coq_context ref
end

module type MEBI_WRAPPER = sig
  module M : MEBI_MONAD

  module MkF : sig
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
end

module type WRAPPER = sig
  module MW : MEBI_WRAPPER
  module M : MEBI_MONAD

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
    val eq : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val to_string : t -> string

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

  module E : ENCODING_TYPE

  module B : sig
    type key = E.t
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

  module type ERROR_TYPE = sig
    type mebi_error =
      | InvalidLTSSort of Sorts.family
      | InvalidArity of Environ.env * Evd.evar_map * Constr.t
      | InvalidLTSRef of Names.GlobRef.t
      | UnknownTermType of
          (Environ.env * Evd.evar_map * (term * term * term list))
      | PrimaryLTSNotFound of (Environ.env * Evd.evar_map * term * term list)
      | UnknownDecodeKey of (Environ.env * Evd.evar_map * E.t * term B.t)

    exception MEBI_exn of mebi_error

    val invalid_sort : Sorts.family -> exn
    val invalid_arity : Environ.env -> Evd.evar_map -> Constr.t -> exn
    val invalid_ref : Names.GlobRef.t -> exn

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

    val unknown_decode_key
      :  Environ.env
      -> Evd.evar_map
      -> E.t
      -> term B.t
      -> exn
  end

  module Error : ERROR_TYPE

  type wrapper =
    { coq_ref : M.coq_context ref
    ; fwd_enc : E.t F.t
    ; bck_enc : term E.Tbl.t
    }

  type 'a in_context =
    { state : wrapper ref
    ; value : 'a
    }

  type 'a mm = wrapper ref -> 'a in_context

  val run : 'a mm -> 'a
  val return : 'a -> 'a mm
  val bind : 'a mm -> ('a -> 'b mm) -> 'b mm
  val map : ('a -> 'b) -> 'a mm -> 'b mm
  val product : 'a mm -> 'b mm -> ('a * 'b) mm
  val iterate : int -> int -> 'a -> (int -> 'a -> 'a mm) -> 'a mm
  val get_env : wrapper ref -> Environ.env in_context
  val get_sigma : wrapper ref -> Evd.evar_map in_context
  val get_fwd_enc : wrapper ref -> E.t MW.MkF.t in_context
  val get_bck_enc : wrapper ref -> term E.Tbl.t in_context

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

    val ( let$+ )
      :  (Environ.env -> Evd.evar_map -> 'a)
      -> ('a -> 'b mm)
      -> 'b mm

    val ( and+ ) : 'a mm -> 'b mm -> ('a * 'b) mm
  end

  module Syntax : MEBI_MONAD_SYNTAX

  val invalid_arity : Constr.t -> 'a mm
  val invalid_sort : Sorts.family -> 'a mm
  val invalid_ref : Names.GlobRef.t -> 'a mm
  val unknown_term_type : term * term * term list -> 'a mm
  val primary_lts_not_found : term * term list -> 'a mm
  val unknown_decode_key : E.t * term B.t -> 'a mm
  val encode : wrapper ref -> term -> E.t mm
  val decode : wrapper ref -> E.t -> term mm
  val constr_to_string : Constr.t -> string
  val econstr_to_string : term -> string
  val tref_to_econstr : Constrexpr.constr_expr -> term mm
  val normalize_econstr : term -> term mm
  val type_of_econstr : term -> term mm
  val type_of_tref : Constrexpr.constr_expr -> term mm
  val make_transition_tbl : (module Hashtbl.S with type key = E.t) mm
  val make_state_set : (module Set.S with type elt = E.t) mm

  val make_state_tree_pair_set
    : (module Set.S with type elt = E.t * Constr_tree.t) mm
end

module MebiMonad : MEBI_MONAD
module MebiWrapper : functor (_ : MEBI_MONAD) -> MEBI_WRAPPER
module Wrapper : functor (_ : MEBI_WRAPPER) -> WRAPPER
module MM = MebiMonad

module W : sig
  module MW : sig
    module M : sig
      type coq_context = Wrapper(MebiWrapper(MM)).MW.M.coq_context =
        { coq_env : Environ.env
        ; coq_ctx : Evd.evar_map
        }

      type 'a in_coq_context = 'a Wrapper(MebiWrapper(MM)).MW.M.in_coq_context =
        { state : coq_context ref
        ; value : 'a
        }

      type 'a cm = coq_context ref -> 'a in_coq_context

      val coq_run : 'a cm -> 'a
      val coq_return : 'a -> 'a cm
      val coq_bind : 'a cm -> ('a -> 'b cm) -> 'b cm
      val ( let|* ) : 'a cm -> ('a -> 'b cm) -> 'b cm
      val get_env : Environ.env cm
      val get_sigma : Evd.evar_map cm
      val init : coq_context ref
    end

    module MkF : sig
      type key = term
      type 'a t = 'a Wrapper(MebiWrapper(MM)).MW.MkF.t

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
  end

  module M : sig
    type coq_context = Wrapper(MebiWrapper(MM)).M.coq_context =
      { coq_env : Environ.env
      ; coq_ctx : Evd.evar_map
      }

    type 'a in_coq_context = 'a Wrapper(MebiWrapper(MM)).M.in_coq_context =
      { state : coq_context ref
      ; value : 'a
      }

    type 'a cm = coq_context ref -> 'a in_coq_context

    val coq_run : 'a cm -> 'a
    val coq_return : 'a -> 'a cm
    val coq_bind : 'a cm -> ('a -> 'b cm) -> 'b cm
    val ( let|* ) : 'a cm -> ('a -> 'b cm) -> 'b cm
    val get_env : Environ.env cm
    val get_sigma : Evd.evar_map cm
    val init : coq_context ref
  end

  module F : sig
    type key = term
    type 'a t = 'a Wrapper(MebiWrapper(MM)).F.t

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
    val eq : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val to_string : t -> string

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

  module E : sig
    type t = Wrapper(MebiWrapper(MM)).E.t

    val init : t
    val eq : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val to_string : t -> string

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

    module Tbl : sig
      type key = t
      type 'a t = 'a Wrapper(MebiWrapper(MM)).E.Tbl.t

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

    val encode : t F.t -> term Tbl.t -> term -> t

    exception InvalidDecodeKey of (t * term Tbl.t)

    val decode : term Tbl.t -> t -> term
  end

  module B : sig
    type key = E.t
    type 'a t = 'a Wrapper(MebiWrapper(MM)).B.t

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

  module type ERROR_TYPE = sig
    type mebi_error =
      | InvalidLTSSort of Sorts.family
      | InvalidArity of Environ.env * Evd.evar_map * Constr.t
      | InvalidLTSRef of Names.GlobRef.t
      | UnknownTermType of
          (Environ.env * Evd.evar_map * (term * term * term list))
      | PrimaryLTSNotFound of (Environ.env * Evd.evar_map * term * term list)
      | UnknownDecodeKey of (Environ.env * Evd.evar_map * E.t * term B.t)

    exception MEBI_exn of mebi_error

    val invalid_sort : Sorts.family -> exn
    val invalid_arity : Environ.env -> Evd.evar_map -> Constr.t -> exn
    val invalid_ref : Names.GlobRef.t -> exn

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

    val unknown_decode_key
      :  Environ.env
      -> Evd.evar_map
      -> E.t
      -> term B.t
      -> exn
  end

  module Error : sig
    type mebi_error = Wrapper(MebiWrapper(MM)).Error.mebi_error =
      | InvalidLTSSort of Sorts.family
      | InvalidArity of Environ.env * Evd.evar_map * Constr.t
      | InvalidLTSRef of Names.GlobRef.t
      | UnknownTermType of
          (Environ.env * Evd.evar_map * (term * term * term list))
      | PrimaryLTSNotFound of (Environ.env * Evd.evar_map * term * term list)
      | UnknownDecodeKey of (Environ.env * Evd.evar_map * E.t * term B.t)

    exception MEBI_exn of mebi_error

    val invalid_sort : Sorts.family -> exn
    val invalid_arity : Environ.env -> Evd.evar_map -> Constr.t -> exn
    val invalid_ref : Names.GlobRef.t -> exn

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

    val unknown_decode_key
      :  Environ.env
      -> Evd.evar_map
      -> E.t
      -> term B.t
      -> exn
  end

  type wrapper = Wrapper(MebiWrapper(MM)).wrapper =
    { coq_ref : M.coq_context ref
    ; fwd_enc : E.t F.t
    ; bck_enc : term E.Tbl.t
    }

  type 'a in_context = 'a Wrapper(MebiWrapper(MM)).in_context =
    { state : wrapper ref
    ; value : 'a
    }

  type 'a mm = wrapper ref -> 'a in_context

  val run : 'a mm -> 'a
  val return : 'a -> 'a mm
  val bind : 'a mm -> ('a -> 'b mm) -> 'b mm
  val map : ('a -> 'b) -> 'a mm -> 'b mm
  val product : 'a mm -> 'b mm -> ('a * 'b) mm
  val iterate : int -> int -> 'a -> (int -> 'a -> 'a mm) -> 'a mm
  val get_env : wrapper ref -> Environ.env in_context
  val get_sigma : wrapper ref -> Evd.evar_map in_context
  val get_fwd_enc : wrapper ref -> E.t MW.MkF.t in_context
  val get_bck_enc : wrapper ref -> term E.Tbl.t in_context

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

    val ( let$+ )
      :  (Environ.env -> Evd.evar_map -> 'a)
      -> ('a -> 'b mm)
      -> 'b mm

    val ( and+ ) : 'a mm -> 'b mm -> ('a * 'b) mm
  end

  module Syntax : sig
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

    val ( let$+ )
      :  (Environ.env -> Evd.evar_map -> 'a)
      -> ('a -> 'b mm)
      -> 'b mm

    val ( and+ ) : 'a mm -> 'b mm -> ('a * 'b) mm
  end

  val invalid_arity : Constr.t -> 'a mm
  val invalid_sort : Sorts.family -> 'a mm
  val invalid_ref : Names.GlobRef.t -> 'a mm
  val unknown_term_type : term * term * term list -> 'a mm
  val primary_lts_not_found : term * term list -> 'a mm
  val unknown_decode_key : E.t * term B.t -> 'a mm
  val encode : wrapper ref -> term -> E.t mm
  val decode : wrapper ref -> E.t -> term mm
  val constr_to_string : Constr.t -> string
  val econstr_to_string : term -> string
  val tref_to_econstr : Constrexpr.constr_expr -> term mm
  val normalize_econstr : term -> term mm
  val type_of_econstr : term -> term mm
  val type_of_tref : Constrexpr.constr_expr -> term mm
  val make_transition_tbl : (module Hashtbl.S with type key = E.t) mm
  val make_state_set : (module Set.S with type elt = E.t) mm

  val make_state_tree_pair_set
    : (module Set.S with type elt = E.t * Constr_tree.t) mm
end
