module Make
    (Log : Logger.S)
    (State : sig
       type t

       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
       val equal : t -> t -> bool
       val compare : t -> t -> int
       val hash : t -> int
     end)
    (Label : sig
       type t

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val hash : t -> int
       val is_silent : t -> bool
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (Action : sig
       type t

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val hash : t -> int
       val wk_equal : t -> t -> bool
       val is_silent : t -> bool
       val is_labelled : Label.t -> t -> bool
       val shorter_annotation : t -> t -> t
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (Edge : sig
       type t =
         { from : State.t
         ; goto : State.t
         ; action : Action.t
         }

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val is_silent : t -> bool
       val is_labelled : Label.t -> t -> bool
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end) =
struct
  module Set_ : Set.S with type elt = Edge.t = Set.Make (Edge)
  include Set_

  let labelled (xs : t) (y : Label.t) : t =
    Log.trace __FUNCTION__;
    filter (Edge.is_labelled y) xs
  ;;

  include
    Json.Set.Make
      (Log)
      (struct
        module Set = Set_

        let name = "Edge"
        let json = Edge.json
      end)
end
