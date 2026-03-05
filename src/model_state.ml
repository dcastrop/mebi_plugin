module type JsonArgs = sig
  type k

  val name : string
  val json : k -> Yojson.t
end

module type JsonKind = sig
  type k

  val json : ?as_elt:bool -> k -> Yojson.t
  val to_string : ?pretty:bool -> k -> string
  val log : ?__FUNCTION__:string -> ?s:string -> k -> unit
end

module JsonThing (Log : Logger.SLogger) (X : JsonArgs) :
  JsonKind with type k = X.k = struct
  (* type t = X.t *)
  (* include X *)
  (* module X = X *)
  type k = X.k

  let json ?(as_elt : bool = false) (x : X.k) : Yojson.t =
    let y : Yojson.t = X.json x in
    if as_elt then y else `Assoc [ X.name, y ]
  ;;

  let to_string ?(pretty : bool = true) (x : X.k) : string =
    if pretty
    then json x |> Yojson.pretty_to_string
    else json x |> Yojson.to_string
  ;;

  let log ?(__FUNCTION__ : string = "") ?(s : string = X.name) (x : X.k) : unit =
    Log.thing ~__FUNCTION__ Debug s x (Of to_string)
  ;;
end

(* module JsonList (Log : Logger.SLogger) (X : JsonArgs) :
   JsonKind with type t = X.t = struct
   type t = X.t

   let json ?(as_elt : bool = false) (x : t) : Yojson.t =
   let y : Yojson.t = X.json x in
   `Assoc [ X.name, y ]
   ;;

   let to_string ?(pretty : bool = true) (x : t) : string =
   if pretty
   then json x |> Yojson.pretty_to_string
   else json x |> Yojson.to_string
   ;;

   let log ?(__FUNCTION__ : string = "") ?(s : string = X.name) (x : t) : unit =
   Log.thing ~__FUNCTION__ Debug s x (Of to_string)
   ;;
   end *)

(*******************)

module Make (Log : Logger.SLogger) (Enc : Encoding.SEncoding) = struct
  module type S = sig
    type t =
      { term : Enc.t
      ; pp : string option
      }

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
  end

  module State : S = struct
    type t =
      { term : Enc.t
      ; pp : string option
      }

    let equal (a : t) (b : t) : bool = Enc.equal a.term b.term
    let compare (a : t) (b : t) : int = Enc.compare a.term b.term
    let hash (x : t) : int = Enc.hash x.term

    include
      JsonThing
        (Log)
        (struct
          type k = t

          let name = "State"

          let json (x : t) : Yojson.t =
            `Assoc
              [ "enc", `String (Enc.to_string x.term)
              ; ( "pp"
                , `String (Utils.Strfy.option (Args Utils.Strfy.string) x.pp) )
              ]
          ;;
        end)
  end
end
