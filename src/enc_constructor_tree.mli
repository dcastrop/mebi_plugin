module Make : (Log : Logger.S)
  (Enc : Encoding.SEncoding)
  -> sig
module Tree_ : module type of Enc_tree.Make (Log) (Enc)
  module Tree = Tree_.Tree

  module type S = sig
    type t = Enc.t * Enc.t * Tree.t

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module Constructor : S

  module Constructors : (Constructor : S) -> sig
    type t = Constructor.t list
    type k = (Enc.t * Enc.t * Tree.t) list

    val json : ?as_elt:bool -> k -> Yojson.t
    val to_string : ?pretty:bool -> k -> string
    val log : ?__FUNCTION__:string -> ?s:string -> k -> unit
  end
end
