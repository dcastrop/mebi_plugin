module type ENCODING_TYPE = sig
  module F : sig
    type key = EConstr.t
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

  val encode : t F.t -> EConstr.t Tbl.t -> EConstr.t -> t

  exception InvalidDecodeKey of (t * EConstr.t Tbl.t)

  val decode_opt : EConstr.t Tbl.t -> t -> EConstr.t option
  val decode : EConstr.t Tbl.t -> t -> EConstr.t
  val fwd_to_list : t F.t -> (EConstr.t * t) list
  val bck_to_list : EConstr.t Tbl.t -> (t * EConstr.t) list
end

module IntEncoding : (FwdMap : sig
                        type key = EConstr.t
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
                      end)
  -> sig
  module F : sig
    type key = EConstr.t
    type 'a t = 'a FwdMap.t

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

  module Tbl : ENC_TBL

  val encode : t F.t -> EConstr.t Tbl.t -> EConstr.t -> t

  exception InvalidDecodeKey of (t * EConstr.t Tbl.t)

  val decode_opt : EConstr.t Tbl.t -> t -> EConstr.t option
  val decode : EConstr.t Tbl.t -> t -> EConstr.t
  val fwd_to_list : t F.t -> (EConstr.t * t) list
  val bck_to_list : EConstr.t Tbl.t -> (t * EConstr.t) list
end
