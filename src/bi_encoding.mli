module type S = sig
  module Enc : Encoding.SEncoding

  module FwdMap : sig
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

  module BckMap : sig
    type key = Enc.t
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

  type fwdmap = Enc.t FwdMap.t
  type bckmap = EConstr.t BckMap.t

  type maps =
    { fwd : fwdmap
    ; bck : bckmap
    }

  val the_maps : unit -> maps ref
  val reset : unit -> unit
  val encode : EConstr.t -> Enc.t
  val encoded : EConstr.t -> bool

  exception CannotDecode of Enc.t

  val decode : Enc.t -> EConstr.t
  val decode_opt : Enc.t -> EConstr.t option
  val encode_map : 'a FwdMap.t -> 'a BckMap.t
  val decode_map : 'a BckMap.t -> 'a FwdMap.t
  val to_list : unit -> (Enc.t * EConstr.t) list
  val make_hashtbl : (module Hashtbl.S with type key = Enc.t)
  val make_set : (module Set.S with type elt = Enc.t)
end

module Make : (_ : Rocq_context.SRocq_context) (E : Encoding.SEncoding) -> sig
  module Enc : sig
    type t = E.t

    val init : t
    val next : t -> t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val to_string : t -> string
    val counter : t ref
    val reset : unit -> unit
    val incr : unit -> t
  end

  module FwdMap : sig
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

  module BckMap : sig
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

  type fwdmap = E.t FwdMap.t
  type bckmap = EConstr.t BckMap.t

  type maps =
    { fwd : fwdmap
    ; bck : bckmap
    }

  val the_maps : unit -> maps ref
  val reset : unit -> unit
  val encode : EConstr.t -> Enc.t
  val encoded : EConstr.t -> bool

  exception CannotDecode of Enc.t

  val decode : Enc.t -> EConstr.t
  val decode_opt : Enc.t -> EConstr.t option
  val encode_map : 'a FwdMap.t -> 'a BckMap.t
  val decode_map : 'a BckMap.t -> 'a FwdMap.t
  val to_list : unit -> (Enc.t * EConstr.t) list
  val make_hashtbl : (module Hashtbl.S with type key = Enc.t)
  val make_set : (module Set.S with type elt = Enc.t)
end
