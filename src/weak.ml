module type S = sig
  type enc

  type t =
    | Option of enc
    | Custom of enc * enc

  type k = t

  val json : ?as_elt:bool -> k -> Yojson.t
  val to_string : ?pretty:bool -> k -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit
  val eq : t -> t -> bool
end

module Make
    (Log : Logger.S)
    (Enc : Encoding.S)
    (M : Rocq_monad_utils.S with type enc = Enc.t) : S with type enc = Enc.t =
struct
  type enc = Enc.t

  type t =
    | Option of Enc.t
    | Custom of Enc.t * Enc.t

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "Weak"

        let json ?(as_elt : bool = false) : t -> Yojson.t =
          let f (x : Enc.t) : Yojson.t =
            `Assoc
              [ "enc", Enc.json ~as_elt:true x
              ; "econstr", `String (M.decode x |> M.Strfy.econstr)
              ]
          in
          function
          | Option label -> `Assoc [ "option label", f label ]
          | Custom (tau, label) ->
            `Assoc [ "custom", `Assoc [ "tau", f tau; "label", f label ] ]
        ;;
      end)

  let eq x y : bool =
    match x, y with
    | Option x, Option y -> Enc.equal x y
    | Custom (x1, x2), Custom (y1, y2) -> Enc.equal x1 y1 && Enc.equal x2 y2
    | _, _ -> false
  ;;
end
