module Make
    (Log : Logger.S)
    (Label : sig
       type t

       val json : ?as_elt:bool -> t -> Yojson.t

       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val equal : t -> t -> bool *)
       val compare : t -> t -> int

       (* val hash : t -> int *)
       val is_silent : t -> bool
     end) : sig
  include Set.S with type elt = Label.t

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val non_silent : t -> t
end = struct
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
