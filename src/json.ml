module Thing = struct
  module type S = sig
    type k

    val name : string
    val json : ?as_elt:bool -> k -> Yojson.t
  end

  module type Type = sig
    type k

    val json : ?as_elt:bool -> k -> Yojson.t
    val to_string : ?pretty:bool -> k -> string
    val log : ?__FUNCTION__:string -> ?s:string -> k -> unit
  end

  module Make (Log : Logger.S) (X : S) : Type with type k = X.k = struct
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

    let log ?(__FUNCTION__ : string = "") ?(s : string = X.name) (x : X.k)
      : unit
      =
      Log.thing ~__FUNCTION__ Debug s x (Of to_string)
    ;;
  end
end

module List = struct
  module type S = sig
    (* type elt *)
    include sig
      type t

      val json : ?as_elt:bool -> t -> Yojson.t
    end

    val name : string
  end

  module type Type = sig
    type k

    val json : ?as_elt:bool -> k -> Yojson.t
    val to_string : ?pretty:bool -> k -> string
    val log : ?__FUNCTION__:string -> ?s:string -> k -> unit
  end

  module Make (Log : Logger.S) (X : S) : Type with type k = X.t list = struct
    type elt = X.t
    type k = elt list

    (** ... *)
    let json ?(as_elt : bool = false) (xs : k) : Yojson.t =
      let y : Yojson.t =
        `List
          (List.rev xs
           |> List.fold_left
                (fun (acc : Yojson.t list) (x : elt) ->
                  X.json ~as_elt:true x :: acc)
                [])
      in
      if as_elt then y else `Assoc [ X.name, y ]
    ;;

    let to_string ?(pretty : bool = true) (x : k) : string =
      if pretty
      then json x |> Yojson.pretty_to_string
      else json x |> Yojson.to_string
    ;;

    let log ?(__FUNCTION__ : string = "") ?(s : string = X.name) (x : k) : unit =
      Log.thing ~__FUNCTION__ Debug s x (Of to_string)
    ;;
  end
end

module Set = struct
  module type S = sig
    module Set : Set.S

    val name : string
    val json : ?as_elt:bool -> Set.elt -> Yojson.t
  end

  module type Type = sig
    type t
    type elt

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module Make (Log : Logger.S) (X : S) :
    Type with type t = X.Set.t and type elt = X.Set.elt = struct
    type t = X.Set.t
    type elt = X.Set.elt

    (** ... *)
    let json ?(as_elt : bool = false) (x : X.Set.t) : Yojson.t =
      let y : Yojson.t =
        `List
          (X.Set.fold
             (fun (x : X.Set.elt) (acc : Yojson.t list) ->
               X.json ~as_elt:true x :: acc)
             x
             [])
      in
      if as_elt then y else `Assoc [ X.name, y ]
    ;;

    let to_string ?(pretty : bool = true) (x : X.Set.t) : string =
      if pretty
      then json x |> Yojson.pretty_to_string
      else json x |> Yojson.to_string
    ;;

    let log ?(__FUNCTION__ : string = "") ?(s : string = X.name) (x : X.Set.t)
      : unit
      =
      Log.thing ~__FUNCTION__ Debug s x (Of to_string)
    ;;
  end
end
