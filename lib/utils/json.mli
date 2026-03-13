val option
  :  ?as_elt:bool
  -> (?as_elt:bool -> 'a -> Yojson.t)
  -> 'a option
  -> Yojson.t

module type S = sig
  type k

  val json : ?as_elt:bool -> k -> Yojson.t
  val to_string : ?pretty:bool -> k -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit
  val write : ?dir:string -> string -> k -> unit
end

module Make
    (Log : Logger.S)
    (X : sig
       type k

       val name : string
       val json : ?as_elt:bool -> k -> Yojson.t
     end) : S with type k = X.k

module Thing : sig
  module Make : (Log : Logger.S)
      (X : sig
         type k

         val name : string
         val json : ?as_elt:bool -> k -> Yojson.t
       end)
      -> S with type k = X.k
end

module Map : sig
  module Make : (Log : Logger.S)
      (X : sig
         module Map : Hashtbl.S

         type value

         val name : string
         val kname : string
         val vname : string
         val kjson : ?as_elt:bool -> Map.key -> Yojson.t
         val vjson : ?as_elt:bool -> value -> Yojson.t
       end)
      -> S with type k = X.value X.Map.t
end

module List : sig
  module Make : (Log : Logger.S)
      (X : sig
         type k

         val json : ?as_elt:bool -> k -> Yojson.t
         val name : string
       end)
      -> S with type k = X.k list
end

module Set : sig
  module Make : (Log : Logger.S)
      (X : sig
         module Set : Set.S

         val name : string
         val json : ?as_elt:bool -> Set.elt -> Yojson.t
       end)
      -> S with type k = X.Set.t
end
