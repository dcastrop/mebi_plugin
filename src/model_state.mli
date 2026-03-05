module Make : (Log : Logger.S)
  (Enc : Encoding.SEncoding)
  -> sig
  module type S = sig
    type t = { term : Enc.t; pp : string option }

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
  end

  module State : S

  module States : (State : S) -> sig
    module Set : sig
      type elt = State.t
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

    val empty : Set.t
    val add : State.t -> Set.t -> Set.t
    val singleton : State.t -> Set.t
    val remove : State.t -> Set.t -> Set.t
    val union : Set.t -> Set.t -> Set.t
    val inter : Set.t -> Set.t -> Set.t
    val disjoint : Set.t -> Set.t -> bool
    val diff : Set.t -> Set.t -> Set.t
    val cardinal : Set.t -> int
    val elements : Set.t -> State.t list
    val min_elt : Set.t -> State.t
    val min_elt_opt : Set.t -> State.t option
    val max_elt : Set.t -> State.t
    val max_elt_opt : Set.t -> State.t option
    val choose : Set.t -> State.t
    val choose_opt : Set.t -> State.t option
    val find : State.t -> Set.t -> State.t
    val find_opt : State.t -> Set.t -> State.t option
    val find_first : (State.t -> bool) -> Set.t -> State.t

    val find_first_opt :
      (State.t -> bool) -> Set.t -> State.t option

    val find_last : (State.t -> bool) -> Set.t -> State.t

    val find_last_opt :
      (State.t -> bool) -> Set.t -> State.t option

    val iter : (State.t -> unit) -> Set.t -> unit

    val fold :
      (State.t -> 'acc -> 'acc) -> Set.t -> 'acc -> 'acc

    val map : (State.t -> State.t) -> Set.t -> Set.t
    val filter : (State.t -> bool) -> Set.t -> Set.t

    val filter_map :
      (State.t -> State.t option) -> Set.t -> Set.t

    val partition : (State.t -> bool) -> Set.t -> Set.t * Set.t
    val split : State.t -> Set.t -> Set.t * bool * Set.t
    val is_empty : Set.t -> bool
    val mem : State.t -> Set.t -> bool
    val equal : Set.t -> Set.t -> bool
    val compare : Set.t -> Set.t -> int
    val subset : Set.t -> Set.t -> bool
    val for_all : (State.t -> bool) -> Set.t -> bool
    val exists : (State.t -> bool) -> Set.t -> bool
    val to_list : Set.t -> State.t list
    val of_list : State.t list -> Set.t
    val to_seq_from : State.t -> Set.t -> State.t Seq.t
    val to_seq : Set.t -> State.t Seq.t
    val to_rev_seq : Set.t -> State.t Seq.t
    val add_seq : State.t Seq.t -> Set.t -> Set.t
    val of_seq : State.t Seq.t -> Set.t
    val add_to_opt : State.t -> Set.t option -> Set.t

    exception StateHasNoOrigin of (State.t * Set.t * Set.t)

    val origin_of_state : State.t -> Set.t -> Set.t -> int
    val has_shared_origin : Set.t -> Set.t -> Set.t -> bool

    type t = Set.t
    type elt = State.t

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end
end
