
  type t =
    { env : Environ.env
    ; sigma : Evd.evar_map
    }

  module type SRocq_context = sig
    val get : unit -> t ref
    val env : unit -> Environ.env ref
    val sigma : unit -> Evd.evar_map ref
    val update : Environ.env ref -> Evd.evar_map ref -> unit
  end

  module type S = sig
    val env : unit -> Environ.env ref
    val sigma : unit -> Evd.evar_map ref
  end

  module Make : (_ : S) -> SRocq_context
  module Default : SRocq_context


module MakeEConstrMap : (_ : SRocq_context) -> sig
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