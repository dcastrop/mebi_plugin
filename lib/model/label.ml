module type S = sig
  type t

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val is_silent : t -> bool
end

type 'a t' =
  { enc : 'a
  ; is_silent : bool option
  }

module Make (Log : Logger.S) (Base : Base_term.S) : S with type t = Base.t t' =
struct
  type t = Base.t t'

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "Label"

        let json ?as_elt (x : t) : Yojson.t =
          `Assoc
            [ "enc", Base.json ~as_elt:true x.enc
            ; ( "is_silent"
              , Json.option ~as_elt:true (fun ?as_elt x -> `Bool x) x.is_silent
              )
            ]
        ;;
      end)

  let equal (a : t) (b : t) : bool = Base.equal a.enc b.enc

  (** [compare a b] first compares the [enc] of each, and if [0] then only compares the contents of [is_silent] if both are [Some _]. This allows us to [find] a label using only the [enc] if we do not know if [is_silent] (which is acceptable since in the full model either all [Labels] are [None] or [Some _], and a comparison such as this would only be in the case where we are trying to find a label by [enc]).
  *)
  let compare (a : t) (b : t) : int =
    Utils.compare_chain
      [ Base.compare a.enc b.enc
      ; Option.cata
          (fun (a : bool) -> Option.cata (Bool.compare a) 0 b.is_silent)
          0
          a.is_silent
      ]
  ;;

  let hash (x : t) : int = Base.hash x.enc
  let is_silent (x : t) : bool = Option.default false x.is_silent
end
