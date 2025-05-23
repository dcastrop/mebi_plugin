module type ENCODING_TYPE = sig
  type t

  val init : t
  val eq : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val next : t -> t

  module Tbl : sig
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
end

module IntEncoding : ENCODING_TYPE

module type MEBI_MONAD = sig
  module E : ENCODING_TYPE

  type term = Evd.econstr

  type coq_context =
    { coq_env : Environ.env
    ; coq_ctx : Evd.evar_map
    }

  type 'a in_coq_context =
    { state : coq_context ref
    ; value : 'a
    }

  type 'a cm = coq_context ref -> 'a in_coq_context

  val coq_return : 'a -> 'a cm

  val make_constr_tbl
    :  coq_context ref
    -> (module Hashtbl.S with type key = Evd.econstr) cm

  val coq_bind : 'a cm -> ('a -> 'b cm) -> 'b cm
  val ( let|* ) : 'a cm -> ('a -> 'b cm) -> 'b cm
  val init : coq_context ref
  val coq_run : 'a cm -> 'a
end

module type MEBI_WRAPPER = sig
  module M : MEBI_MONAD

  module MkF : sig
    type key = Evd.econstr
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
  module W : MEBI_WRAPPER
  module M : MEBI_MONAD

  module F : sig
    type key = Evd.econstr
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

  type wrapper =
    { coq_ref : M.coq_context ref
    ; fwd_enc : M.E.t F.t
    ; bck_enc : Evd.econstr M.E.Tbl.t
    ; counter : M.E.t ref
    }

  type 'a in_context =
    { state : wrapper ref
    ; value : 'a
    }

  type 'a mm = wrapper ref -> 'a in_context

  val return : 'a -> 'a mm
  val bind : 'a mm -> ('a -> 'b mm) -> 'b mm
  val map : ('a -> 'b) -> 'a mm -> 'b mm
  val product : 'a mm -> 'b mm -> ('a * 'b) mm
  val iterate : int -> int -> 'a -> (int -> 'a -> 'a mm) -> 'a mm
  val get_env : wrapper ref -> Environ.env in_context
  val get_sigma : wrapper ref -> Evd.evar_map in_context
  val get_fwd_enc : wrapper ref -> M.E.t W.MkF.t in_context
  val get_bck_enc : wrapper ref -> M.term M.E.Tbl.t in_context
  val get_counter : wrapper ref -> M.E.t ref in_context

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

  val next : wrapper ref -> M.E.t mm
  val encode : wrapper ref -> M.term -> M.E.t mm
  val decode : wrapper ref -> M.E.t -> M.term mm
end

module MebiMonad : functor (_ : ENCODING_TYPE) -> MEBI_MONAD
module MebiWrapper : functor (_ : MEBI_MONAD) -> MEBI_WRAPPER
module Wrapper : functor (_ : MEBI_WRAPPER) -> WRAPPER

module MM : sig
  module E : sig
    type t = MebiMonad(IntEncoding).E.t

    val init : t
    val eq : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val next : t -> t

    module Tbl : sig
      type key = t
      type 'a t = 'a MebiMonad(IntEncoding).E.Tbl.t

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

  type term = Evd.econstr

  type coq_context = MebiMonad(IntEncoding).coq_context =
    { coq_env : Environ.env
    ; coq_ctx : Evd.evar_map
    }

  type 'a in_coq_context = 'a MebiMonad(IntEncoding).in_coq_context =
    { state : coq_context ref
    ; value : 'a
    }

  type 'a cm = coq_context ref -> 'a in_coq_context

  val coq_return : 'a -> 'a cm

  val make_constr_tbl
    :  coq_context ref
    -> (module Hashtbl.S with type key = Evd.econstr) cm

  val coq_bind : 'a cm -> ('a -> 'b cm) -> 'b cm
  val ( let|* ) : 'a cm -> ('a -> 'b cm) -> 'b cm
  val init : coq_context ref
  val coq_run : 'a cm -> 'a
end

module MW : sig
  module M : sig
    module E : sig
      type t = MebiWrapper(MM).M.E.t

      val init : t
      val eq : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
      val next : t -> t

      module Tbl : sig
        type key = t
        type 'a t = 'a MebiWrapper(MM).M.E.Tbl.t

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

    type term = Evd.econstr

    type coq_context = MebiWrapper(MM).M.coq_context =
      { coq_env : Environ.env
      ; coq_ctx : Evd.evar_map
      }

    type 'a in_coq_context = 'a MebiWrapper(MM).M.in_coq_context =
      { state : coq_context ref
      ; value : 'a
      }

    type 'a cm = coq_context ref -> 'a in_coq_context

    val coq_return : 'a -> 'a cm

    val make_constr_tbl
      :  coq_context ref
      -> (module Hashtbl.S with type key = Evd.econstr) cm

    val coq_bind : 'a cm -> ('a -> 'b cm) -> 'b cm
    val ( let|* ) : 'a cm -> ('a -> 'b cm) -> 'b cm
    val init : coq_context ref
    val coq_run : 'a cm -> 'a
  end

  module MkF : sig
    type key = Evd.econstr
    type 'a t = 'a MebiWrapper(MM).MkF.t

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

module W : sig
  module W : sig
    module M : sig
      module E : sig
        type t = Wrapper(MW).W.M.E.t

        val init : t
        val eq : t -> t -> bool
        val compare : t -> t -> int
        val hash : t -> int
        val next : t -> t

        module Tbl : sig
          type key = t
          type 'a t = 'a Wrapper(MW).W.M.E.Tbl.t

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

      type term = Evd.econstr

      type coq_context = Wrapper(MW).W.M.coq_context =
        { coq_env : Environ.env
        ; coq_ctx : Evd.evar_map
        }

      type 'a in_coq_context = 'a Wrapper(MW).W.M.in_coq_context =
        { state : coq_context ref
        ; value : 'a
        }

      type 'a cm = coq_context ref -> 'a in_coq_context

      val coq_return : 'a -> 'a cm

      val make_constr_tbl
        :  coq_context ref
        -> (module Hashtbl.S with type key = Evd.econstr) cm

      val coq_bind : 'a cm -> ('a -> 'b cm) -> 'b cm
      val ( let|* ) : 'a cm -> ('a -> 'b cm) -> 'b cm
      val init : coq_context ref
      val coq_run : 'a cm -> 'a
    end

    module MkF : sig
      type key = Evd.econstr
      type 'a t = 'a Wrapper(MW).W.MkF.t

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
    module E : sig
      type t = Wrapper(MW).M.E.t

      val init : t
      val eq : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
      val next : t -> t

      module Tbl : sig
        type key = t
        type 'a t = 'a Wrapper(MW).M.E.Tbl.t

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

    type term = Evd.econstr

    type coq_context = Wrapper(MW).M.coq_context =
      { coq_env : Environ.env
      ; coq_ctx : Evd.evar_map
      }

    type 'a in_coq_context = 'a Wrapper(MW).M.in_coq_context =
      { state : coq_context ref
      ; value : 'a
      }

    type 'a cm = coq_context ref -> 'a in_coq_context

    val coq_return : 'a -> 'a cm

    val make_constr_tbl
      :  coq_context ref
      -> (module Hashtbl.S with type key = Evd.econstr) cm

    val coq_bind : 'a cm -> ('a -> 'b cm) -> 'b cm
    val ( let|* ) : 'a cm -> ('a -> 'b cm) -> 'b cm
    val init : coq_context ref
    val coq_run : 'a cm -> 'a
  end

  module F : sig
    type key = Evd.econstr
    type 'a t = 'a Wrapper(MW).F.t

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

  type wrapper = Wrapper(MW).wrapper =
    { coq_ref : M.coq_context ref
    ; fwd_enc : M.E.t F.t
    ; bck_enc : Evd.econstr M.E.Tbl.t
    ; counter : M.E.t ref
    }

  type 'a in_context = 'a Wrapper(MW).in_context =
    { state : wrapper ref
    ; value : 'a
    }

  type 'a mm = wrapper ref -> 'a in_context

  val return : 'a -> 'a mm
  val bind : 'a mm -> ('a -> 'b mm) -> 'b mm
  val map : ('a -> 'b) -> 'a mm -> 'b mm
  val product : 'a mm -> 'b mm -> ('a * 'b) mm
  val iterate : int -> int -> 'a -> (int -> 'a -> 'a mm) -> 'a mm
  val get_env : wrapper ref -> Environ.env in_context
  val get_sigma : wrapper ref -> Evd.evar_map in_context
  val get_fwd_enc : wrapper ref -> M.E.t W.MkF.t in_context
  val get_bck_enc : wrapper ref -> M.term M.E.Tbl.t in_context
  val get_counter : wrapper ref -> M.E.t ref in_context

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

  val next : wrapper ref -> M.E.t mm
  val encode : wrapper ref -> M.term -> M.E.t mm
  val decode : wrapper ref -> M.E.t -> M.term mm
end
