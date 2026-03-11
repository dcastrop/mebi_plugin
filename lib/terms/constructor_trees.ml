module Make
    (Log : Logger.S)
    (Constructor : sig
       type t

       val json : ?as_elt:bool -> t -> Yojson.t
     end) =
struct
  type t = Constructor.t list

  include
    Json.List.Make
      (Log)
      (struct
        type k = Constructor.t

        let name = "Constructors"
        let json = Constructor.json
      end)
end
