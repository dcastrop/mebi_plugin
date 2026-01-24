module Context : sig
  type t =
    { env : Environ.env
    ; sigma : Evd.evar_map
    }

  module type TYPE = sig
    val get : unit -> t ref
    val env : unit -> Environ.env ref
    val sigma : unit -> Evd.evar_map ref
  end

  module type S = sig
    val env : unit -> Environ.env ref
    val sigma : unit -> Evd.evar_map ref
  end

  module Make : (_ : S) -> TYPE
  module Default : TYPE
end

module MakeEConstrMap : (_ : Context.TYPE) -> sig
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

module Encoding : sig
  module type TYPE = sig
    type t

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

  module type S = sig
    type t

    val init : t
    val next : t -> t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val to_string : t -> string
  end

  module Make : (_ : S) -> TYPE
  module Int : TYPE
end

module Constructor : sig
  module Tree : sig
    module type TYPE = sig
      module Node : sig
        type t

        val to_string : t -> string
      end

      type 'a tree = Node of 'a * 'a tree list
      type t

      val add : t -> t -> t
      val add_list : t -> t list -> t list
      val equal : t -> t -> bool
      val compare : t -> t -> int
      val minimize : t -> Node.t list

      exception Mebi_constr_Tree_EmptyList of unit

      val min : t list -> Node.t list
      val to_string : t -> string
      val list_to_string : ?args:Utils.Strfy.style_args -> t list -> string
    end

    module Make : (_ : Encoding.TYPE) -> TYPE
  end

  module Triple : sig
    module type TYPE = sig
      module Tree : Tree.TYPE

      type t = Evd.econstr * Evd.econstr * Tree.t

      val to_string
        :  Environ.env
        -> Evd.evar_map
        -> ?args:Utils.Strfy.style_args
        -> t
        -> string
    end

    module Make : (Enc : Encoding.TYPE) -> sig
      module Tree : sig
        module Node : sig
          type t = Tree.Make(Enc).Node.t

          val to_string : t -> string
        end

        type 'a tree = 'a Tree.Make(Enc).tree = Node of 'a * 'a tree list
        type t = Tree.Make(Enc).t

        val add : t -> t -> t
        val add_list : t -> t list -> t list
        val equal : t -> t -> bool
        val compare : t -> t -> int
        val minimize : t -> Node.t list

        exception Mebi_constr_Tree_EmptyList of unit

        val min : t list -> Node.t list
        val to_string : t -> string
        val list_to_string : ?args:Utils.Strfy.style_args -> t list -> string
      end

      type t = EConstr.t * EConstr.t * Tree.t

      val to_string
        :  Environ.env
        -> Evd.evar_map
        -> ?args:Utils.Strfy.style_args
        -> t
        -> string
    end
  end

  module type TYPE = sig
    module Tree : Tree.TYPE
    module Triple : Triple.TYPE
  end

  module Make : (_ : Encoding.TYPE) -> TYPE
end

module BiEncoding : sig
  module type TYPE = sig
    module Enc : Encoding.TYPE

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
    val decode : Enc.t -> EConstr.t
    val decode_opt : Enc.t -> EConstr.t option
    val encode_map : 'a FwdMap.t -> 'a BckMap.t
    val decode_map : 'a BckMap.t -> 'a FwdMap.t
    val to_list : unit -> (Enc.t * EConstr.t) list
  end

  module Make : (_ : Context.TYPE) (_ : Encoding.TYPE) -> TYPE
end

module Errors : sig
  module type TYPE = sig
    type t =
      | Invalid_Sort_LTS of Sorts.family
      | Invalid_Sort_Type of Sorts.family

    exception MEBI_exn of t

    val invalid_sort_lts : Sorts.family -> exn
    val invalid_sort_type : Sorts.family -> exn
  end

  module Make : (_ : Context.TYPE) -> TYPE
end

module type TYPE = sig
  module Enc : Encoding.TYPE
  module Constructor : Constructor.TYPE

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

  module Error : Errors.TYPE

  type fwdmap = Enc.t FwdMap.t
  type bckmap = EConstr.t BckMap.t

  type maps =
    { fwd : fwdmap
    ; bck : bckmap
    }

  val encode : EConstr.t -> Enc.t
  val encoded : EConstr.t -> bool
  val decode : Enc.t -> EConstr.t
  val decode_opt : Enc.t -> EConstr.t option
  val to_list : unit -> (Enc.t * EConstr.t) list

  type 'a mm = wrapper ref -> 'a in_wrapper

  and wrapper =
    { ctx : Context.t ref
    ; maps : maps ref
    }

  and 'a in_wrapper =
    { state : wrapper ref
    ; value : 'a
    }

  val run : ?reset_encoding:bool -> 'a mm -> 'a
  val return : 'a -> 'a mm
  val bind : 'a mm -> ('a -> 'b mm) -> 'b mm
  val map : ('a -> 'b) -> 'a mm -> 'b mm
  val product : 'a mm -> 'b mm -> ('a * 'b) mm
  val iterate : int -> int -> 'a -> (int -> 'a -> 'a mm) -> 'a mm

  val state
    :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
    -> wrapper ref
    -> 'a in_wrapper

  val sandbox : ?sigma:Evd.evar_map -> 'a mm -> wrapper ref -> 'a in_wrapper
  val get_env : wrapper ref -> Environ.env in_wrapper
  val get_sigma : wrapper ref -> Evd.evar_map in_wrapper
  val get_fwdmap : wrapper ref -> fwdmap in_wrapper
  val get_bckmap : wrapper ref -> bckmap in_wrapper

  module type SYNTAX = sig
    val ( let+ ) : 'a mm -> ('a -> 'b) -> 'b mm
    val ( let* ) : 'a mm -> ('a -> 'b mm) -> 'b mm

    val ( let$ )
      :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
      -> ('a -> 'b mm)
      -> 'b mm

    val ( let$* )
      :  (Environ.env -> Evd.evar_map -> Evd.evar_map)
      -> (unit -> 'b mm)
      -> 'b mm

    val ( let$+ )
      :  (Environ.env -> Evd.evar_map -> 'a)
      -> ('a -> 'b mm)
      -> 'b mm

    val ( and+ ) : 'a mm -> 'b mm -> ('a * 'b) mm
  end

  module Syntax : SYNTAX

  val fstring : (Environ.env -> Evd.evar_map -> 'a -> string) -> 'a -> string

  val make_transition_tbl
    :  wrapper ref
    -> (module Hashtbl.S with type key = Enc.t) in_wrapper

  val make_state_set
    :  wrapper ref
    -> (module Set.S with type elt = Enc.t) in_wrapper

  val make_state_tree_pair_set
    :  wrapper ref
    -> (module Set.S with type elt = Enc.t * Constructor.Tree.t) in_wrapper
end

module Make : (_ : Context.TYPE) (_ : Encoding.TYPE) -> TYPE
