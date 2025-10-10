module type ENCODING_TYPE = sig
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

  module Tbl : ENC_TBL

  val encode : t F.t -> Evd.econstr Tbl.t -> Evd.econstr -> t

  exception InvalidDecodeKey of (t * Evd.econstr Tbl.t)

  val decode_opt : Evd.econstr Tbl.t -> t -> Evd.econstr option
  val decode : Evd.econstr Tbl.t -> t -> Evd.econstr
end

(* module Make : (Enc : ENCODING_TYPE) -> sig
  module F : sig
    type key = Evd.econstr
    type 'a t = 'a Enc.F.t

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

  type t = Enc.t

  val init : Enc.t
  val cache : Enc.t ref
  val reset : unit -> unit
  val eq : Enc.t -> Enc.t -> bool
  val compare : Enc.t -> Enc.t -> int
  val hash : Enc.t -> int
  val to_string : Enc.t -> string
  val of_int : int -> Enc.t

  module Tbl : sig
    type key = t
    type 'a t = 'a Enc.Tbl.t

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

  val encode :
    Enc.t Enc.F.t ->
    Evd.econstr Enc.Tbl.t ->
    Evd.econstr ->
    Enc.t

  exception InvalidDecodeKey of (Enc.t * Evd.econstr Enc.Tbl.t)

  val decode_opt :
    Evd.econstr Enc.Tbl.t -> Enc.t -> Evd.econstr option

  val decode : Evd.econstr Enc.Tbl.t -> Enc.t -> Evd.econstr
end *)

module IntEncoding : functor (FwdMap : sig
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

                        val iter :
                          (key -> 'a -> unit) -> 'a t -> unit

                        val filter_map_inplace :
                          (key -> 'a -> 'a option) ->
                          'a t ->
                          unit

                        val fold :
                          (key -> 'a -> 'acc -> 'acc) ->
                          'a t ->
                          'acc ->
                          'acc

                        val length : 'a t -> int
                        val stats : 'a t -> Hashtbl.statistics
                        val to_seq : 'a t -> (key * 'a) Seq.t
                        val to_seq_keys : 'a t -> key Seq.t
                        val to_seq_values : 'a t -> 'a Seq.t

                        val add_seq :
                          'a t -> (key * 'a) Seq.t -> unit

                        val replace_seq :
                          'a t -> (key * 'a) Seq.t -> unit

                        val of_seq : (key * 'a) Seq.t -> 'a t
                      end) -> ENCODING_TYPE with module F = FwdMap
