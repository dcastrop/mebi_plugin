module Make : (Log : Logger.SLogger)
  (Enc : Encoding.SEncoding)
  -> sig
  module Tree : sig
    module type STreeNode = sig
      type t = Enc.t * int

      val to_string : t -> string
    end

    module TreeNode : sig
      type t = Enc.t * int

      val to_string : t -> string
    end

    type 'a tree = 'a Enc_tree.Make(Enc).tree =
      | Node of 'a * 'a tree list

    type t = TreeNode.t tree

    val add : t -> t -> t
    val add_list : t -> t list -> t list
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val minimize : t -> TreeNode.t list

    exception CannotMinimizeEmptyList of unit

    val min : t list -> TreeNode.t list
    val to_string : t -> string

    val list_to_string :
      ?args:Utils.Strfy.style_args -> t list -> string
  end

  module Trees : sig
    type elt = Tree.t
    type t = Set.Make(Tree).t

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

    exception EmptyHasNoMin

    val min : t -> elt
    val min_opt : t -> elt option
    val json : t -> Yojson.t
    val to_string : t -> string
    val _log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module State_ : sig
    module type S = sig
      type t = { term : Enc.t; pp : string option }

      val json : ?as_elt:bool -> t -> Yojson.t
      val to_string : ?pretty:bool -> t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
    end

    module State : sig
      type t = Model_state.Make(Log)(Enc).State.t = {
        term : Enc.t;
        pp : string option;
      }

      val json : ?as_elt:bool -> t -> Yojson.t
      val to_string : ?pretty:bool -> t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
    end

    module States : (State : S) -> sig
      module Set : sig
        type elt = State.t
        type t = Model_state.Make(Log)(Enc).States(State).Set.t

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

      val partition :
        (State.t -> bool) -> Set.t -> Set.t * Set.t

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

  module State : State_.S

  module States : sig
    module Set : sig
      type elt = State.t
      type t = Model_state.Make(Log)(Enc).States(State).Set.t

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

  module Label_ : sig
    module type S = sig
      type t = {
        term : Enc.t;
        pp : string option;
        is_silent : bool option;
      }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
      val is_silent : t -> bool

      type k = t

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?s:string -> k -> unit
    end

    module Label : sig
      type t = Model_label.Make(Log)(Enc).Label.t = {
        term : Enc.t;
        pp : string option;
        is_silent : bool option;
      }

      val equal : t -> t -> bool
      val compare : t -> t -> int
      val hash : t -> int
      val is_silent : t -> bool

      type k = t

      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?s:string -> k -> unit
    end

    module Labels : (Label : S) -> sig
      module Set : sig
        type elt = Label.t
        type t = Model_label.Make(Log)(Enc).Labels(Label).Set.t

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
      val add : Label.t -> Set.t -> Set.t
      val singleton : Label.t -> Set.t
      val remove : Label.t -> Set.t -> Set.t
      val union : Set.t -> Set.t -> Set.t
      val inter : Set.t -> Set.t -> Set.t
      val disjoint : Set.t -> Set.t -> bool
      val diff : Set.t -> Set.t -> Set.t
      val cardinal : Set.t -> int
      val elements : Set.t -> Label.t list
      val min_elt : Set.t -> Label.t
      val min_elt_opt : Set.t -> Label.t option
      val max_elt : Set.t -> Label.t
      val max_elt_opt : Set.t -> Label.t option
      val choose : Set.t -> Label.t
      val choose_opt : Set.t -> Label.t option
      val find : Label.t -> Set.t -> Label.t
      val find_opt : Label.t -> Set.t -> Label.t option
      val find_first : (Label.t -> bool) -> Set.t -> Label.t

      val find_first_opt :
        (Label.t -> bool) -> Set.t -> Label.t option

      val find_last : (Label.t -> bool) -> Set.t -> Label.t

      val find_last_opt :
        (Label.t -> bool) -> Set.t -> Label.t option

      val iter : (Label.t -> unit) -> Set.t -> unit

      val fold :
        (Label.t -> 'acc -> 'acc) -> Set.t -> 'acc -> 'acc

      val map : (Label.t -> Label.t) -> Set.t -> Set.t
      val filter : (Label.t -> bool) -> Set.t -> Set.t

      val filter_map :
        (Label.t -> Label.t option) -> Set.t -> Set.t

      val partition :
        (Label.t -> bool) -> Set.t -> Set.t * Set.t

      val split : Label.t -> Set.t -> Set.t * bool * Set.t
      val is_empty : Set.t -> bool
      val mem : Label.t -> Set.t -> bool
      val equal : Set.t -> Set.t -> bool
      val compare : Set.t -> Set.t -> int
      val subset : Set.t -> Set.t -> bool
      val for_all : (Label.t -> bool) -> Set.t -> bool
      val exists : (Label.t -> bool) -> Set.t -> bool
      val to_list : Set.t -> Label.t list
      val of_list : Label.t list -> Set.t
      val to_seq_from : Label.t -> Set.t -> Label.t Seq.t
      val to_seq : Set.t -> Label.t Seq.t
      val to_rev_seq : Set.t -> Label.t Seq.t
      val add_seq : Label.t Seq.t -> Set.t -> Set.t
      val of_seq : Label.t Seq.t -> Set.t
      val non_silent : Set.t -> Set.t

      type t = Set.t
      type elt = Label.t

      val json : ?as_elt:bool -> t -> Yojson.t
      val to_string : ?pretty:bool -> t -> string
      val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    end
  end

  module Label : Label_.S

  module Labels : sig
    module Set : sig
      type elt = Label.t
      type t = Model_label.Make(Log)(Enc).Labels(Label).Set.t

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
    val add : Label.t -> Set.t -> Set.t
    val singleton : Label.t -> Set.t
    val remove : Label.t -> Set.t -> Set.t
    val union : Set.t -> Set.t -> Set.t
    val inter : Set.t -> Set.t -> Set.t
    val disjoint : Set.t -> Set.t -> bool
    val diff : Set.t -> Set.t -> Set.t
    val cardinal : Set.t -> int
    val elements : Set.t -> Label.t list
    val min_elt : Set.t -> Label.t
    val min_elt_opt : Set.t -> Label.t option
    val max_elt : Set.t -> Label.t
    val max_elt_opt : Set.t -> Label.t option
    val choose : Set.t -> Label.t
    val choose_opt : Set.t -> Label.t option
    val find : Label.t -> Set.t -> Label.t
    val find_opt : Label.t -> Set.t -> Label.t option
    val find_first : (Label.t -> bool) -> Set.t -> Label.t

    val find_first_opt :
      (Label.t -> bool) -> Set.t -> Label.t option

    val find_last : (Label.t -> bool) -> Set.t -> Label.t

    val find_last_opt :
      (Label.t -> bool) -> Set.t -> Label.t option

    val iter : (Label.t -> unit) -> Set.t -> unit

    val fold :
      (Label.t -> 'acc -> 'acc) -> Set.t -> 'acc -> 'acc

    val map : (Label.t -> Label.t) -> Set.t -> Set.t
    val filter : (Label.t -> bool) -> Set.t -> Set.t

    val filter_map :
      (Label.t -> Label.t option) -> Set.t -> Set.t

    val partition : (Label.t -> bool) -> Set.t -> Set.t * Set.t
    val split : Label.t -> Set.t -> Set.t * bool * Set.t
    val is_empty : Set.t -> bool
    val mem : Label.t -> Set.t -> bool
    val equal : Set.t -> Set.t -> bool
    val compare : Set.t -> Set.t -> int
    val subset : Set.t -> Set.t -> bool
    val for_all : (Label.t -> bool) -> Set.t -> bool
    val exists : (Label.t -> bool) -> Set.t -> bool
    val to_list : Set.t -> Label.t list
    val of_list : Label.t list -> Set.t
    val to_seq_from : Label.t -> Set.t -> Label.t Seq.t
    val to_seq : Set.t -> Label.t Seq.t
    val to_rev_seq : Set.t -> Label.t Seq.t
    val add_seq : Label.t Seq.t -> Set.t -> Set.t
    val of_seq : Label.t Seq.t -> Set.t
    val non_silent : Set.t -> Set.t

    type t = Set.t
    type elt = Label.t

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module Note : sig
    type t = {
      from : State.t;
      label : Label.t;
      using : Trees.t;
      goto : State.t;
    }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val is_silent : t -> bool
    val json : t -> Yojson.t
    val to_string : t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module Annotation : sig
    type t = { this : Note.t; next : t option }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val is_empty : t -> bool
    val length : t -> int
    val shorter : t -> t -> t
    val exists : Note.t -> t -> bool
    val exists_label : Label.t -> t -> bool
    val append : Note.t -> t -> t
    val last : t -> Note.t

    exception CannotDropLastOfSingleton of t

    val drop_last : t -> t
    val json : t -> Yojson.t
    val to_string : t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module Annotations : sig
    module S : sig
      type elt = Annotation.t
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

    type elt = Annotation.t
    type t = S.t

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
    val json : t -> Yojson.t
    val to_string : t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    val extrapolate : Annotation.t -> t
  end

  module Transition : sig
    type t = {
      from : State.t;
      goto : State.t;
      label : Label.t;
      annotation : Annotation.t option;
      constructor_tree : Tree.t option;
    }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val is_silent : t -> bool
    val annotation_is_empty : t -> bool
    val json : t -> Yojson.t
    val to_string : t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module Transitions : sig
    module S : sig
      type elt = Transition.t
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

    type elt = Transition.t
    type t = S.t

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
    val labels : t -> Labels.t
    val json : t -> Yojson.t
    val to_string : t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module Action : sig
    type t = {
      label : Label.t;
      annotation : Annotation.t option;
      constructor_trees : Trees.t;
    }

    val wk_equal : t -> t -> bool
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val is_silent : t -> bool
    val annotation_is_empty : t -> bool
    val annotation_length : t -> int
    val json : t -> Yojson.t
    val to_string : t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module ActionPair : sig
    type t = Action.t * States.t

    val compare : t -> t -> int
    val json : t -> Yojson.t
    val to_string : t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    val try_update : t -> t list -> t option * t list
    val merge_lists : t list -> t list -> t list
  end

  module ActionPairs : sig
    module S : sig
      type elt = ActionPair.t
      type t = Set.Make(ActionPair).t

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

    type elt = ActionPair.t
    type t = Set.Make(ActionPair).t

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
    val destinations : t -> States.t

    exception IsEmpty

    val shorest_annotation : t -> ActionPair.t
    val merge_list : t -> ActionPair.t list -> t
    val json : t -> Yojson.t
    val to_string : t -> string
    val _log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module Actions : sig
    module S : sig
      type elt = Action.t
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

    type elt = Action.t
    type t = S.t

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
    val labelled : t -> Label.t -> t
    val to_string : t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module ActionMap : sig
    module M : sig
      type key = Action.t
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

    type key = Action.t
    type 'a t = 'a M.t

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

    type t' = States.t t

    val update : t' -> Action.t -> States.t -> unit
    val destinations : t' -> States.t
    val reduce_by_label : t' -> Label.t -> t'
    val to_actions : t' -> Actions.t
    val to_actionpairs : t' -> ActionPairs.t
    val of_actionpairs : ActionPairs.t -> t'
    val merge : t' -> t' -> t'
    val to_string : t' -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t' -> unit
  end

  module Edge : sig
    type t = {
      from : State.t;
      goto : State.t;
      action : Action.t;
    }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val is_silent : t -> bool
    val to_string : t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module Edges : sig
    module S : sig
      type elt = Edge.t
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

    type elt = Edge.t
    type t = S.t

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
    val labelled : t -> Label.t -> t
    val to_string : t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module EdgeMap : sig
    module M : sig
      type key = State.t
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

    type key = State.t
    type 'a t = 'a M.t

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

    type t' = ActionMap.t' t

    val update : t' -> State.t -> Action.t -> States.t -> unit
    val destinations : t' -> State.t -> States.t
    val get_actions : t' -> State.t -> Actions.t
    val reduce_by_label : t' -> Label.t -> t'
    val get_edges : t' -> State.t -> Edges.t
    val to_edges : t' -> Edges.t
    val of_edges : Edges.t -> t'
    val merge : t' -> t' -> t'
    val to_string : t' -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t' -> unit
  end

  module Partition : sig
    module S : sig
      type elt = States.t
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

    type elt = S.elt
    type t = S.t

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
    val get_bisimilar : State.t -> t -> States.t
    val filter_reachable : States.t -> t -> t
    val reachable : State.t -> EdgeMap.t' -> t -> t

    val reachable_by_label :
      State.t -> Label.t -> EdgeMap.t' -> t -> t

    val to_string : t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module Info : sig
    type t = { meta : meta option; weak_labels : Labels.t }

    and meta = {
      is_complete : bool;
      is_merged : bool;
      bounds : bounds;
      lts : lts list;
    }

    and bounds = States of int | Transitions of int

    and lts = {
      enc : Enc.t;
      constructors : Rocq_bindings.constructor list;
    }

    val merge : t -> t -> t
    val to_string : t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module LTS : sig
    type t = {
      init : State.t option;
      terminals : Partition.elt;
      alphabet : Labels.t;
      states : Partition.elt;
      transitions : Transitions.t;
      info : Info.t;
    }

    val to_string : t -> string
  end

  module FSM : sig
    type t = {
      init : State.t option;
      terminals : Partition.elt;
      alphabet : Labels.t;
      states : Partition.elt;
      edges : EdgeMap.t';
      info : Info.t;
    }

    val merge : t -> t -> t
    val is_weak_mode : t -> bool
    val to_string : t -> string
  end

  module Convert : sig
    val transitions_to_edgemap : Transitions.t -> EdgeMap.t'
    val lts_to_fsm : LTS.t -> FSM.t
  end

  module Saturate : sig
    module Log : sig
      module Config : sig
        val get : Output_config.t ref
        val reset : unit -> unit
        val enable_output : unit -> unit
        val disable_output : unit -> unit
        val configure_output : Output_kind.t -> bool -> unit

        val do_output :
          ?__FUNCTION__:string ->
          ?prefix:string option ->
          ?override:bool ->
          Output_kind.t ->
          string ->
          unit

        val show_mode : unit -> unit
        val show_enabled : unit -> unit
        val show_kind : Output_kind.t -> unit
      end

      val enabled : bool ref
      val prefix : string option
      val debug : ?__FUNCTION__:string -> string -> unit
      val info : ?__FUNCTION__:string -> string -> unit
      val notice : ?__FUNCTION__:string -> string -> unit
      val warning : ?__FUNCTION__:string -> string -> unit
      val error : ?__FUNCTION__:string -> string -> unit
      val trace : ?__FUNCTION__:string -> string -> unit
      val result : ?__FUNCTION__:string -> string -> unit
      val show : ?__FUNCTION__:string -> string -> unit

      val thing :
        ?__FUNCTION__:string ->
        ?args:Utils.Strfy.style_args ->
        Output_kind.t ->
        string ->
        'a ->
        'a Utils.Strfy.to_string ->
        unit

      val things :
        ?__FUNCTION__:string ->
        ?args:Utils.Strfy.style_args ->
        Output_kind.t ->
        string ->
        'a list ->
        'a Utils.Strfy.to_string ->
        unit

      val option :
        ?__FUNCTION__:string ->
        ?args:Utils.Strfy.style_args ->
        Output_kind.t ->
        string ->
        'a option ->
        'a Utils.Strfy.to_string ->
        unit

      val options :
        ?__FUNCTION__:string ->
        ?args:Utils.Strfy.style_args ->
        Output_kind.t ->
        string ->
        'a list option ->
        'a Utils.Strfy.to_string ->
        unit
    end

    module WIP : sig
      type t = {
        from : State.t;
        via : Label.t;
        trees : Trees.t;
      }

      val to_string : t -> string
      val is_silent : t -> bool
      val is_named : t -> bool
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val create : State.t -> Action.t -> t

      exception IsEmptyList

      val list_to_annotation :
        State.t -> t list -> Annotation.t
    end

    module Trace : sig
      module NT : sig end

      type t = { this : WIP.t; next : next option }
      and next = Next of t | Goto of State.t

      val to_string : t -> string
      val create : WIP.t -> t
      val compare : t -> t -> int
      val compare_next : next -> next -> int

      exception Invalid

      val has_named : ?validate:bool -> t -> bool
      val validate : t -> unit

      exception CouldNotFindGoto

      val get_goto : t -> State.t

      exception CouldNotFindNamed

      val get_named : t -> Label.t
      val get_named_opt : t -> Label.t option

      exception FailAdd_AlreadyNamed
      exception FailAdd_AlreadyHasGoto

      val add : WIP.t -> t -> t

      exception FailSetGoto_AlreadyHasGoto

      val set_goto : State.t -> t -> t

      exception FailSeq_AlreadyNamed
      exception FailSeq_AlreadyHasGoto

      val seq : t -> t -> t
      val seq_opt : t option -> t -> t
      val get : WIP.t -> t -> t
      val upto_named : t -> t

      exception GotoNotSet

      val to_annotation : t -> Annotation.t
    end

    module Traces : sig
      type elt = Trace.t
      type t = Set.Make(Trace).t

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
      val get : WIP.t -> t -> t
      val to_string : t -> string
    end

    type data = {
      named : Label.t option;
      current : Trace.t option;
      visited : States.t;
      traces : Traces.t ref;
      can_collect_traces : bool ref;
      old_edges : EdgeMap.t';
    }

    val initial_data : Traces.t ref -> EdgeMap.t' -> data
    val has_named : data -> bool
    val update_traces : data -> Trace.t -> unit
    val update_named : Action.t -> data -> data
    val update_current : WIP.t -> data -> data
    val update_visited : State.t -> data -> data
    val already_visited : State.t -> data -> bool
    val skip_action : Action.t -> data -> bool

    val get_old_actions :
      State.t -> data -> ActionMap.t' option

    val update_acc :
      Trace.t -> Label.t -> ActionPairs.t -> ActionPairs.t

    val stop :
      data -> State.t -> ActionPairs.t -> ActionPairs.t

    val finish_with_trace :
      Trace.t ->
      data ->
      Label.t ->
      ActionPairs.t ->
      ActionPairs.t

    val finish_with_trace_upto :
      Trace.t ->
      data ->
      Label.t ->
      ActionPairs.t ->
      ActionPairs.t

    val check_from :
      data -> State.t -> ActionPairs.t -> ActionPairs.t

    val check_actions :
      data ->
      State.t ->
      ActionMap.t' ->
      ActionPairs.t ->
      ActionPairs.t

    val collect_from_traces :
      data ->
      State.t ->
      Action.t ->
      States.t ->
      ActionPairs.t ->
      ActionPairs.t

    val continue_check_destinations :
      data ->
      State.t ->
      Action.t ->
      States.t ->
      ActionPairs.t ->
      ActionPairs.t

    val check_destinations :
      data ->
      State.t ->
      States.t ->
      ActionPairs.t ->
      ActionPairs.t

    val edge_action_destinations :
      data -> State.t -> States.t -> ActionPairs.t

    val edge_actions :
      State.t ->
      ActionMap.t' ->
      EdgeMap.t' ->
      Traces.t ref ->
      ActionPairs.t

    val edge :
      ActionMap.t' ->
      State.t ->
      ActionMap.t' ->
      EdgeMap.t' ->
      Traces.t ref ->
      unit

    val edges :
      Labels.t -> States.t -> EdgeMap.t' -> EdgeMap.t'

    val fsm : ?only_if_weak:bool -> FSM.t -> FSM.t
  end

  module Minimize : sig
    type t = { fsm : FSM.t; pi : Partition.t }

    exception CannotSplitEmptyBlock of unit

    val ensure_nonempty : States.t -> unit

    val split_block :
      Partition.t ->
      State.t ->
      EdgeMap.t' ->
      States.t ->
      States.t * States.t option

    exception
      Split_OnlyReturnedOneBlock_ButNeqBlock of
        (States.t * States.t)

    val ensure_equal : States.t -> States.t -> unit

    val for_each_label :
      Partition.t ref ->
      bool ref ->
      EdgeMap.t' ->
      States.t ref ->
      Label.t ->
      unit

    val for_each_block :
      Partition.t ref ->
      bool ref ->
      Labels.t ->
      EdgeMap.t' ->
      States.t ->
      unit

    val partition_states : FSM.t -> Partition.t
    val fsm : FSM.t -> t
    val to_string : t -> string
  end

  module Bisimilar : sig
    type t = {
      fsm_a : fsm_pair;
      fsm_b : fsm_pair;
      merged : FSM.t;
      result : result;
    }

    and result = {
      bisim_states : Partition.t;
      non_bisim_states : Partition.t;
    }

    and fsm_pair = { original : FSM.t; saturated : FSM.t }

    val fsm_pair : FSM.t -> fsm_pair
    val are_bisimilar : result -> bool
    val the_cached_result : t option ref
    val set_the_result : t -> unit

    exception NoCachedResult of unit

    val get_the_result : unit -> t
    val split : Partition.t -> States.t -> States.t -> result
    val fsm : FSM.t -> FSM.t -> t
    val fsm_pair_to_string : fsm_pair -> string
    val result_to_string : result -> string
    val to_string : t -> string
  end
end
