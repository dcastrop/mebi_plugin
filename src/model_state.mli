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
    include module type of Set.Make (State) with type elt = State.t

    val add_to_opt : State.t -> t option -> t
    exception StateHasNoOrigin of (State.t * t * t)

    val origin_of_state : State.t -> t -> t -> int
    val has_shared_origin : t -> t -> t -> bool


    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end
end
