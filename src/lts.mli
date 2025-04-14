type raw_flat_lts = (string * string * string * string option) list
type raw_nested_lts = (string * (string * string list) list) list

type raw_transitions =
  | Flat of raw_flat_lts
  | Nested of raw_nested_lts

type transition =
  { id : int
  ; from : string
  ; label : string
  ; destination : string
  ; info : string option
  }

module Transitions : sig
  type elt = transition
  type t

  val empty : t
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val disjoint : t -> t -> bool
  val diff : t -> t -> t
  val cardinal : t -> int
  val elements : t -> elt list
  val min_elt : t -> elt
  val min_elt_opt : t -> elt option
  val max_elt : t -> elt
  val max_elt_opt : t -> elt option
  val choose : t -> elt
  val choose_opt : t -> elt option
  val find : elt -> t -> elt
  val find_opt : elt -> t -> elt option
  val find_first : (elt -> bool) -> t -> elt
  val find_first_opt : (elt -> bool) -> t -> elt option
  val find_last : (elt -> bool) -> t -> elt
  val find_last_opt : (elt -> bool) -> t -> elt option
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val map : (elt -> elt) -> t -> t
  val filter : (elt -> bool) -> t -> t
  val filter_map : (elt -> elt option) -> t -> t
  val partition : (elt -> bool) -> t -> t * t
  val split : elt -> t -> t * bool * t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val subset : t -> t -> bool
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val to_list : t -> elt list
  val of_list : elt list -> t
  val to_seq_from : elt -> t -> elt Seq.t
  val to_seq : t -> elt Seq.t
  val to_rev_seq : t -> elt Seq.t
  val add_seq : elt Seq.t -> t -> t
  val of_seq : elt Seq.t -> t
end

type lts =
  { init : string option
  ; transitions : Transitions.t
  ; info : Utils.model_info option
  }

module PStr : sig
  val transition : ?params:Utils.Formatting.pstr_params -> transition -> string

  val transitions
    :  ?params:Utils.Formatting.pstr_params
    -> Transitions.t
    -> string

  val lts : ?params:Utils.Formatting.pstr_params -> lts -> string
end

module Create : sig
  type transition_params =
    | Of of (int * string * string * string * string option)

  val transition : transition_params -> transition
  val lts : ?init:string -> ?info:Utils.model_info -> raw_transitions -> lts
end
