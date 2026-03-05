module Make (Log : Logger.S) (Enc : Encoding.SEncoding) = struct
  module type S = sig
    type t =
      { term : Enc.t
      ; pp : string option
      ; is_silent : bool option
      }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val is_silent : t -> bool
    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module Label : S = struct
    type t =
      { term : Enc.t
      ; pp : string option
      ; is_silent : bool option
      }

    let equal (a : t) (b : t) : bool = Enc.equal a.term b.term

    (** [compare a b] first compares the [term] of each, and if [0] then only compares the contents of [is_silent] if both are [Some _]. This allows us to [find] a label using only the [term] if we do not know if [is_silent] (which is acceptable since in the full model either all [Labels] are [None] or [Some _], and a comparison such as this would only be in the case where we are trying to find a label by [term]).
    *)
    let compare (a : t) (b : t) : int =
      Utils.compare_chain
        [ Enc.compare a.term b.term
        ; Option.cata
            (fun (a : bool) -> Option.cata (Bool.compare a) 0 b.is_silent)
            0
            a.is_silent
        ]
    ;;

    let hash (x : t) : int = Enc.hash x.term

    (* *)
    let is_silent (x : t) : bool = Option.default false x.is_silent

    (* *)
    include
      Json.Thing.Make
        (Log)
        (struct
          type k = t

          let name = "Label"

          let json ?as_elt (x : t) : Yojson.t =
            `Assoc
              [ "enc", `String (Enc.to_string x.term)
              ; ( "pp"
                , `String (Utils.Strfy.option (Args Utils.Strfy.string) x.pp) )
              ; ( "is_silent"
                , `String
                    (Utils.Strfy.option (Args Utils.Strfy.bool) x.is_silent) )
              ]
          ;;
        end)
  end

  module Labels (Label : S) = struct
    include Set.Make (Label)

    let non_silent (xs : t) : t =
      filter (fun (x : Label.t) -> Bool.not (Label.is_silent x)) xs
    ;;

    (* *)
    include
      Json.Set.Make
        (Log)
        (struct
          module Set = Set.Make (Label)

          let name = "Labels"
          let json = Label.json
        end)
  end
end
