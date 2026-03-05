module Make : (Log : Logger.S)
  (Enc : Encoding.SEncoding)
  -> sig
  module type NodeType = sig
    type t = Enc.t * int

    val compare : t -> t -> int
    val equal : t -> t -> bool
    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module Node : sig
    type t = Enc.t * int

    val compare : t -> t -> int
    val equal : t -> t -> bool
    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module type S = sig
    type 'a tree = N of 'a * 'a tree list
    type t = Node.t tree

    val add : t -> t -> t
    val add_list : t -> t list -> t list
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val minimize : t -> Node.t list

    exception CannotMinimizeEmptyList of unit

    val min : t list -> Node.t list

    type k = t

    val json : ?as_elt:bool -> k -> Yojson.t
    val to_string : ?pretty:bool -> k -> string
    val log : ?__FUNCTION__:string -> ?s:string -> k -> unit
  end

  module Tree : S

  module Trees : (Tree : S) -> sig
    module Set : sig
      type elt = Tree.t
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
    val add : Tree.t -> Set.t -> Set.t
    val singleton : Tree.t -> Set.t
    val remove : Tree.t -> Set.t -> Set.t
    val union : Set.t -> Set.t -> Set.t
    val inter : Set.t -> Set.t -> Set.t
    val disjoint : Set.t -> Set.t -> bool
    val diff : Set.t -> Set.t -> Set.t
    val cardinal : Set.t -> int
    val elements : Set.t -> Tree.t list
    val min_elt : Set.t -> Tree.t
    val min_elt_opt : Set.t -> Tree.t option
    val max_elt : Set.t -> Tree.t
    val max_elt_opt : Set.t -> Tree.t option
    val choose : Set.t -> Tree.t
    val choose_opt : Set.t -> Tree.t option
    val find : Tree.t -> Set.t -> Tree.t
    val find_opt : Tree.t -> Set.t -> Tree.t option
    val find_first : (Tree.t -> bool) -> Set.t -> Tree.t

    val find_first_opt :
      (Tree.t -> bool) -> Set.t -> Tree.t option

    val find_last : (Tree.t -> bool) -> Set.t -> Tree.t

    val find_last_opt :
      (Tree.t -> bool) -> Set.t -> Tree.t option

    val iter : (Tree.t -> unit) -> Set.t -> unit

    val fold :
      (Tree.t -> 'acc -> 'acc) -> Set.t -> 'acc -> 'acc

    val map : (Tree.t -> Tree.t) -> Set.t -> Set.t
    val filter : (Tree.t -> bool) -> Set.t -> Set.t

    val filter_map :
      (Tree.t -> Tree.t option) -> Set.t -> Set.t

    val partition : (Tree.t -> bool) -> Set.t -> Set.t * Set.t
    val split : Tree.t -> Set.t -> Set.t * bool * Set.t
    val is_empty : Set.t -> bool
    val mem : Tree.t -> Set.t -> bool
    val equal : Set.t -> Set.t -> bool
    val compare : Set.t -> Set.t -> int
    val subset : Set.t -> Set.t -> bool
    val for_all : (Tree.t -> bool) -> Set.t -> bool
    val exists : (Tree.t -> bool) -> Set.t -> bool
    val to_list : Set.t -> Tree.t list
    val of_list : Tree.t list -> Set.t
    val to_seq_from : Tree.t -> Set.t -> Tree.t Seq.t
    val to_seq : Set.t -> Tree.t Seq.t
    val to_rev_seq : Set.t -> Tree.t Seq.t
    val add_seq : Tree.t Seq.t -> Set.t -> Set.t
    val of_seq : Tree.t Seq.t -> Set.t

    exception EmptyHasNoMin

    val min : Set.t -> Tree.t
    val min_opt : Set.t -> Tree.t option

    type t = Set.t
    type elt = Tree.t

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end
end
