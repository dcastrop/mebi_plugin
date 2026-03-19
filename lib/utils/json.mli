val option
  :  ?as_elt:bool
  -> (?as_elt:bool -> 'a -> Yojson.t)
  -> 'a option
  -> Yojson.t

module type S = sig
  type k

  val name : string
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
  module Make
      (Log : Logger.S)
      (X : sig
         type k

         val name : string
         val json : ?as_elt:bool -> k -> Yojson.t
       end) : S with type k = X.k
end

module Map : sig
  module type S' = sig
    include S

    val compare : k -> k -> int
  end

  module Make
      (Log : Logger.S)
      (X : sig
         module Map : Hashtbl.S

         type value

         val name : string
       end)
      (K : S' with type k = X.Map.key)
      (V : S' with type k = X.value) : S with type k = X.value X.Map.t
end

module Set : sig
  module Make
      (Log : Logger.S)
      (X : sig
         module Set : Set.S

         val name : string
         val json : ?as_elt:bool -> Set.elt -> Yojson.t
       end) : S with type k = X.Set.t
end

module List : sig
  module Make
      (Log : Logger.S)
      (X : sig
         type k

         val name : string
         val json : ?as_elt:bool -> k -> Yojson.t
       end) : S with type k = X.k list
end
