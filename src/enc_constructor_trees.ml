module Make
    (Log : Logger.S)
    (Constructor : sig
       type t

       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end) =
struct
  type t = Constructor.t list

  include
    Json.List.Make
      (Log)
      (struct
        include Constructor

        let name = "Constructors"
      end)
end
