module type S = sig
  type t

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
end

module Make (Log : Logger.S) (Base : Base_term.S) : S with type t = Base.t =
struct
  type t = Base.t

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "State"
        let json ?as_elt (x : t) : Yojson.t = Base.json ~as_elt:false x
      end)

  let equal = Base.equal
  let compare = Base.compare
  let hash = Base.hash
end
