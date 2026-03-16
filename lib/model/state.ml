module type S = sig
  type base
  type t = { base : base }

  include Json.S with type k = t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
end

module Make (Log : Logger.S) (Base : Base_term.S) : S with type base = Base.t =
struct
  type base = Base.t
  type t = { base : base }

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "State"
        let json ?as_elt (x : t) : Yojson.t = Base.json ~as_elt:false x.base
      end)

  let equal a b = Base.equal a.base b.base
  let compare a b = Base.compare a.base b.base
  let hash x = Base.hash x.base
end
