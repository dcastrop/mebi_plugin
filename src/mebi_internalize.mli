module type INTERNAL_TYPE = sig
  type t

  val eq : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
end

module type ENCODING_TYPE = sig
  module T : INTERNAL_TYPE

  type t = T.t

  val eq : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val next : t -> t
end

module type INTERNAL_PAIR = sig
  module O : INTERNAL_TYPE
  module E : ENCODING_TYPE

  type origin_t = O.t
  type encode_t = E.t
end

module type S = sig
  module P : INTERNAL_PAIR

  module F : sig
    type key = P.origin_t
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

  module B : sig
    type key = P.encode_t
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
    { fwd_enc : P.encode_t F.t
    ; bck_enc : P.origin_t B.t
    ; counter : P.encode_t ref
    ; coq : Mebi_monad.coq_context ref
    }

  type 'a in_wrapper =
    { state : wrapper ref
    ; value : 'a
    }

  type 'a ii = wrapper ref -> 'a in_wrapper

  val return : 'a -> 'a ii
  val bind : 'a ii -> ('a -> 'b ii) -> 'b ii
  val counter : wrapper ref -> P.encode_t ii
  val next : wrapper ref -> P.encode_t ii
end

module Internals : functor
    (Pair : INTERNAL_PAIR)
    (FwdMap : sig
       type key = Pair.origin_t
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
     end)
    (BckMap : sig
       type key = Pair.encode_t
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
     end)
    -> S

module type Wrapper = sig
  module I : S

  type origin_t = I.F.key
  type encode_t = I.B.key
  type wrapper = I.wrapper
  type 'a iw = 'a I.in_wrapper
  type 'a ii = 'a I.ii

  val return : 'a -> 'a ii
  val bind : 'a ii -> ('a -> 'b ii) -> 'b ii
  val ( let|* ) : 'a ii -> ('a -> 'b ii) -> 'b ii
end

module W : functor (Internals : S) -> Wrapper
