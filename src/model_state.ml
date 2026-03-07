module Make (Log : Logger.S) (Enc : Encoding.S) : sig
  type t = { enc : Enc.t }

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
end = struct
  type t = { enc : Enc.t }

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "State"

        let json ?as_elt (x : t) : Yojson.t =
          `Assoc [ "enc", Enc.json ~as_elt:true x.enc ]
        ;;
      end)

  let equal (a : t) (b : t) : bool = Enc.equal a.enc b.enc
  let compare (a : t) (b : t) : int = Enc.compare a.enc b.enc
  let hash (x : t) : int = Enc.hash x.enc
end
