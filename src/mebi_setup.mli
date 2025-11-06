val enable_logging : bool ref

type unif_problem =
  { termL : Evd.econstr
  ; termR : Evd.econstr
  }

type coq_context =
  { coq_env : Environ.env
  ; coq_ctx : Evd.evar_map
  }

val the_coq_env_opt : Environ.env ref option ref
val new_coq_env : unit -> Environ.env ref
val the_coq_env : ?fresh:bool -> unit -> Environ.env ref
val the_coq_ctx_opt : Evd.evar_map ref option ref
val new_coq_ctx : ?fresh:bool -> unit -> Evd.evar_map ref
val the_coq_ctx : ?fresh:bool -> unit -> Evd.evar_map ref

module FwdMap : sig
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

module IntEncoding : Mebi_enc.S

module Enc : sig
  module F : sig
    type key = Evd.econstr
    type 'a t = 'a Mebi_enc.Make(IntEncoding).F.t

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

  type t = Mebi_enc.Make(IntEncoding).t

  val init : t
  val cache : t ref
  val reset : unit -> unit
  val eq : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val to_string : t -> string

  module B : sig
    type key = t
    type 'a t = 'a Mebi_enc.Make(IntEncoding).B.t

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

  exception InvalidDecodeKey of (t * Evd.econstr B.t)

  val encode : t F.t -> Evd.econstr B.t -> Evd.econstr -> t
  val decode_opt : Evd.econstr B.t -> t -> Evd.econstr option
  val decode : Evd.econstr B.t -> t -> Evd.econstr
  val fwd_to_list : t F.t -> (Evd.econstr * t) list
  val bck_to_list : Evd.econstr B.t -> (t * Evd.econstr) list
end

module Eq : sig
  val enc : Enc.t -> Enc.t -> bool
  val econstr : Evd.evar_map -> Evd.econstr -> Evd.econstr -> bool
  val constr : Constr.t -> Constr.t -> bool
end
