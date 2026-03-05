module Make : (Log : Logger.S)
  (Enc : Encoding.SEncoding)
  -> sig
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
    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module Label : S

  module Labels : (Label : S) -> sig
    include module type of Set.Make (Label) with type elt = Label.t
    val non_silent : t -> t


    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end
end
