module type JsonArgs =
  sig type k val name : string val json : k -> Yojson.t end
module type JsonKind =
  sig
    type k
    val json : ?as_elt:bool -> k -> Yojson.t
    val to_string : ?pretty:bool -> k -> string
    val log : ?__FUNCTION__:string -> ?s:string -> k -> unit
  end
module JsonThing :
  (Log : Logger.SLogger) (X : JsonArgs) ->
    sig
      type k = X.k
      val json : ?as_elt:bool -> k -> Yojson.t
      val to_string : ?pretty:bool -> k -> string
      val log : ?__FUNCTION__:string -> ?s:string -> k -> unit
    end
module Make :
  (Log : Logger.SLogger) (Enc : Encoding.SEncoding)
    ->
    sig
      module type S =
        sig
          type t = { term : Enc.t; pp : string option; }
          val json : ?as_elt:bool -> t -> Yojson.t
          val to_string : ?pretty:bool -> t -> string
          val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
          val equal : t -> t -> bool
          val compare : t -> t -> int
          val hash : t -> int
        end
      module State : S
    end
