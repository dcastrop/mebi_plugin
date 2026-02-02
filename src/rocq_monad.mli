module Make : (Log : Logger.SLogger)
  (_ : Rocq_context.SRocq_context)
  (_ : Encoding.SEncoding)
  -> sig
  module Ctx : Rocq_context.SRocq_context
  module Enc : Encoding.SEncoding

  module F : sig
    type key = Evd.econstr

    type 'a t =
      'a Bi_encoding.Make(Log)(Ctx)(Enc).F.t

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

  module B : sig
    type key = Enc.t

    type 'a t =
      'a Bi_encoding.Make(Log)(Ctx)(Enc).B.t

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

  type maps = 
        Bi_encoding.Make(Log)(Ctx)(Enc).maps = {
    fwd : Enc.t F.t;
    bck : Evd.econstr B.t;
  }

  val the_maps : maps ref option ref
  val reset : unit -> unit

  exception MapsNotInitialised of unit

  val get_the_maps : unit -> maps ref
  val fwdmap : unit -> Enc.t F.t
  val bckmap : unit -> Evd.econstr B.t

  exception EncodingNotFound of Evd.econstr

  val get_encoding : Evd.econstr -> Enc.t
  val encode : Evd.econstr -> Enc.t
  val encoded : Evd.econstr -> bool

  exception DecodingNotFound of Enc.t

  val get_econstr : Enc.t -> Evd.econstr

  exception CannotDecode of Enc.t

  val decode : Enc.t -> Evd.econstr
  val decode_opt : Enc.t -> Evd.econstr option
  val decode_map : 'a B.t -> 'a F.t
  val encode_map : 'a F.t -> 'a B.t
  val to_list : unit -> (Enc.t * Evd.econstr) list
  val bienc_to_list : unit -> (Enc.t * EConstr.t) list

  type 'a mm = wrapper ref -> 'a in_wrapper
  and wrapper = { ctx : Rocq_context.t ref; maps : maps ref }
  and 'a in_wrapper = { state : wrapper ref; value : 'a }

  val run : ?reset_encoding:bool -> 'a mm -> 'a
  val return : 'a -> 'a mm
  val bind : 'a mm -> ('a -> 'b mm) -> 'b mm
  val map : ('a -> 'b) -> 'a mm -> 'b mm
  val product : 'a mm -> 'b mm -> ('a * 'b) mm

  val iterate :
    int -> int -> 'a -> (int -> 'a -> 'a mm) -> 'a mm

  val state :
    (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a) ->
    wrapper ref ->
    'a in_wrapper

  val sandbox :
    ?sigma:Evd.evar_map ->
    'a mm ->
    wrapper ref ->
    'a in_wrapper

  module type SYNTAX = sig
    val ( let+ ) : 'a mm -> ('a -> 'b) -> 'b mm
    val ( let* ) : 'a mm -> ('a -> 'b mm) -> 'b mm

    val ( let$ ) :
      (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a) ->
      ('a -> 'b mm) ->
      'b mm

    val ( let$* ) :
      (Environ.env -> Evd.evar_map -> Evd.evar_map) ->
      (unit -> 'b mm) ->
      'b mm

    val ( let$+ ) :
      (Environ.env -> Evd.evar_map -> 'a) ->
      ('a -> 'b mm) ->
      'b mm

    val ( and+ ) : 'a mm -> 'b mm -> ('a * 'b) mm
  end

  module Syntax : SYNTAX

  val get_ctx : wrapper ref -> Rocq_context.t in_wrapper
  val get_env : wrapper ref -> Environ.env in_wrapper
  val get_sigma : wrapper ref -> Evd.evar_map in_wrapper
  val get_maps : wrapper ref -> maps in_wrapper
  val get_fwdmap : wrapper ref -> Enc.t F.t in_wrapper
  val get_bckmap : wrapper ref -> EConstr.t B.t in_wrapper

  val fstring :
    (Environ.env -> Evd.evar_map -> 'a -> string) ->
    'a ->
    string

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

  module Constructor : sig
    type t = EConstr.t * EConstr.t * Tree.t

    val to_string : Environ.env -> Evd.evar_map -> t -> string
  end

  val make_state_tree_pair_set : unit ->
    (module Set.S with type elt = Enc.t * Tree.t)
  val make_hashtbl : unit -> (module Hashtbl.S with type key = Enc.t)
  val make_set : unit -> (module Set.S with type elt = Enc.t)
end
