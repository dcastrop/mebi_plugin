val enable_logging : bool ref

type proof_context = {
  mutable proof : Declare.Proof.t option;
  mutable names : Names.Id.Set.t option;
}

type coq_context = {
  coq_env : Environ.env;
  coq_ctx : Evd.evar_map;
  proofv : proof_context;
}

val the_proofv_opt : proof_context ref option ref

val new_proofv :
  Declare.Proof.t option ->
  Names.Id.Set.t option ->
  proof_context ref

val the_coq_proofv :
  ?new_proof:bool ->
  ?proof:Declare.Proof.t option ->
  ?names:Names.Id.Set.t option ->
  unit ->
  proof_context ref

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

  val filter_map_inplace :
    (key -> 'a -> 'a option) -> 'a t -> unit

  val fold :
    (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

  val length : 'a t -> int
  val stats : 'a t -> Hashtbl.statistics
  val to_seq : 'a t -> (key * 'a) Seq.t
  val to_seq_keys : 'a t -> key Seq.t
  val to_seq_values : 'a t -> 'a Seq.t
  val add_seq : 'a t -> (key * 'a) Seq.t -> unit
  val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
  val of_seq : (key * 'a) Seq.t -> 'a t
end

module F = FwdMap

module Enc : sig
  module F : sig
    type key = Evd.econstr
    type 'a t = 'a F.t

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

    val filter_map_inplace :
      (key -> 'a -> 'a option) -> 'a t -> unit

    val fold :
      (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

    val length : 'a t -> int
    val stats : 'a t -> Hashtbl.statistics
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_keys : 'a t -> key Seq.t
    val to_seq_values : 'a t -> 'a Seq.t
    val add_seq : 'a t -> (key * 'a) Seq.t -> unit
    val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
    val of_seq : (key * 'a) Seq.t -> 'a t
  end

  type t = int

  val init : t
  val cache : t ref
  val counter : t ref
  val reset : unit -> unit
  val eq : Int.t -> Int.t -> bool
  val compare : Int.t -> Int.t -> int
  val hash : Int.t -> int
  val to_string : int -> string
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

    val filter_map_inplace :
      (key -> 'a -> 'a option) -> 'a t -> unit

    val fold :
      (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

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

    type 'a t =
      'a Mebi_enc.IntEncoding(FwdMap).Tbl.t

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

    val filter_map_inplace :
      (key -> 'a -> 'a option) -> 'a t -> unit

    val fold :
      (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

    val length : 'a t -> int
    val stats : 'a t -> Hashtbl.statistics
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_keys : 'a t -> key Seq.t
    val to_seq_values : 'a t -> 'a Seq.t
    val add_seq : 'a t -> (key * 'a) Seq.t -> unit
    val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
    val of_seq : (key * 'a) Seq.t -> 'a t
  end

  val encode : t F.t -> Evd.econstr Tbl.t -> Evd.econstr -> t

  exception InvalidDecodeKey of (t * Evd.econstr Tbl.t)

  val decode_opt : Evd.econstr Tbl.t -> t -> Evd.econstr option
  val decode : Evd.econstr Tbl.t -> t -> Evd.econstr
  val fwd_to_list : t F.t -> (Evd.econstr * t) list
  val bck_to_list : Evd.econstr Tbl.t -> (t * Evd.econstr) list
end

module B = Enc.Tbl

module Eq : sig
  val enc : Int.t -> Int.t -> bool

  val econstr :
    Evd.evar_map -> Evd.econstr -> Evd.econstr -> bool
end
