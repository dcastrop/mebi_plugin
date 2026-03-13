module Make : (Log : Logger.S)
    (Constructor : sig
       type t

       val json : ?as_elt:bool -> t -> Yojson.t
     end)
    -> sig
  type t = Constructor.t list

  include Json.S with type k = t
end
