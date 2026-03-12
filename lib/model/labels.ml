module type S = sig
  include Set.S

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
  val non_silent : t -> t
end

module Make (Log : Logger.S) (Label : Label.S) : S with type elt = Label.t =
struct
  module Set_ : Set.S with type elt = Label.t = Set.Make (Label)
  include Set_

  include
    Json.Set.Make
      (Log)
      (struct
        module Set = Set_

        let name = "Labels"
        let json = Label.json
      end)

  let non_silent (xs : t) : t =
    filter (fun (x : Label.t) -> Bool.not (Label.is_silent x)) xs
  ;;
end
