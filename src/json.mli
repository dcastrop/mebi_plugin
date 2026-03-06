module Thing : sig
  module type S = sig
    type k

    val name : string
    val json : ?as_elt:bool -> k -> Yojson.t
  end

  module type Type = sig
    type k

    val json : ?as_elt:bool -> k -> Yojson.t
    val to_string : ?pretty:bool -> k -> string
    val log : ?__FUNCTION__:string -> ?s:string -> k -> unit
  end

  module Make : (Log : Logger.S) (X : S) -> sig
    type k = X.k

    val json : ?as_elt:bool -> k -> Yojson.t
    val to_string : ?pretty:bool -> k -> string
    val log : ?__FUNCTION__:string -> ?s:string -> k -> unit
  end
end

module List : sig
  module type S = sig
    type t

    val json : ?as_elt:bool -> t -> Yojson.t
    val name : string
  end

  module type Type = sig
    type k

    val json : ?as_elt:bool -> k -> Yojson.t
    val to_string : ?pretty:bool -> k -> string
    val log : ?__FUNCTION__:string -> ?s:string -> k -> unit
  end

  module Make : (Log : Logger.S) (X : S) -> sig
    type k = X.t list

    val json : ?as_elt:bool -> k -> Yojson.t
    val to_string : ?pretty:bool -> k -> string
    val log : ?__FUNCTION__:string -> ?s:string -> k -> unit
  end
end

module Set : sig
  module type S = sig
    module Set : Set.S

    val name : string
    val json : ?as_elt:bool -> Set.elt -> Yojson.t
  end

  module type Type = sig
    type t
    type elt

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module Make : (Log : Logger.S) (X : S) -> sig
    type t = X.Set.t
    type elt = X.Set.elt

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end
end

module Map : sig
  module type S = sig
    module Map : Hashtbl.S

    type value

    val name : string
    val kname : string
    val vname : string
    val kjson : ?as_elt:bool -> Map.key -> Yojson.t
    val vjson : ?as_elt:bool -> value -> Yojson.t
  end

  module type Type = sig
    type k

    val json : ?as_elt:bool -> k -> Yojson.t
    val to_string : ?pretty:bool -> k -> string
    val log : ?__FUNCTION__:string -> ?s:string -> k -> unit
  end

  module Make : (Log : Logger.S) (X : S) -> sig
    type k = X.value X.Map.t

    val json : ?as_elt:bool -> k -> Yojson.t
    val to_string : ?pretty:bool -> k -> string
    val log : ?__FUNCTION__:string -> ?s:string -> k -> unit
  end
end
