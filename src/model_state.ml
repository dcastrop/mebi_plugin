module Make (Log : Logger.S) (Enc : Encoding.SEncoding) = struct
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
      Json.Thing.Make
        (Log)
        (struct
          type k = t

          let name = "State"

          let json ?as_elt (x : t) : Yojson.t =
            `Assoc
              [ "enc", `String (Enc.to_string x.term)
              ; ( "pp"
                , `String (Utils.Strfy.option (Args Utils.Strfy.string) x.pp) )
              ]
          ;;
        end)
  end

  module States (State : S) = struct
    (* module Set_ : Set.S with type elt = State.t = Set.Make (State) *)
    (* include Set_ *)
    include Set.Make (State)

    let add_to_opt (x : State.t) (ys : t option) : t =
      add x (Option.default empty ys)
    ;;

    exception StateHasNoOrigin of (State.t * t * t)

    (** [origin_of_state x a b] returns [-1] if [x] is only in [a], [1] if [x] is only in [b], [0] if [x] is in both [a] and [b], and raises [StateHasNoOrigin] otherwise.
    *)
    let origin_of_state (x : State.t) (a : t) (b : t) : int =
      match mem x a, mem x b with
      | true, true -> 0
      | true, false -> -1
      | false, true -> 1
      | false, false -> raise (StateHasNoOrigin (x, a, b))
    ;;

    (** [has_shared_origin a b c] returns [true] if any of the states in [a] are present in both [b] and [c], otherwise [false].
    *)
    let has_shared_origin (a : t) (b : t) (c : t) : bool =
      let f (i : int) (x : State.t) : bool =
        match origin_of_state x b c with 0 -> true | j -> Int.equal i j
      in
      exists (f (-1)) a && exists (f 1) a
    ;;

    include
      Json.Set.Make
        (Log)
        (struct
          module Set = Set.Make (State)

          let name = "States"
          let json = State.json
        end)
  end
end
