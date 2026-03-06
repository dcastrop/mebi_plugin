module Make (Log : Logger.S) (Enc : Encoding.S) : sig
  type t =
    { term : Enc.t
    ; pp : string option
    }

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
end = struct
  type t =
    { term : Enc.t
    ; pp : string option
    }

  let equal (a : t) (b : t) : bool = Enc.equal a.term b.term
  let compare (a : t) (b : t) : int = Enc.compare a.term b.term
  let hash (x : t) : int = Enc.hash x.term

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "State"

        let json ?as_elt (x : t) : Yojson.t =
          `Assoc
            [ "enc", Enc.json ~as_elt:true x.term
            ; "pp", `String (Utils.Strfy.option (Args Utils.Strfy.string) x.pp)
            ]
        ;;
      end)
end
