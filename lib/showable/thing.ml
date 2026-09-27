(* Unifies [Showable]'s pp/show/equal/compare with [Json.S]'s
   json/to_string/log/write, so a component supplies {name; json; equal;
   compare} once instead of separately hand-writing equal/compare and
   calling [Json.Thing.Make]. [pp]/[show] are derived from the existing
   [json] function (via [to_string]) rather than requiring a second,
   independently-written pretty-printer per component. *)

module type S = sig
  type t

  include Showable.Ordered with type t := t
  include Json.S with type k := t
end

(* The result deliberately introduces no [type t]/[type k] of its own --
   every value is phrased directly in terms of [X.t]. An ascription like
   ": S with type t = X.t" would still hide [X.t]'s field/constructor labels
   from callers (S declares [type t] abstractly; a manifest equation on an
   otherwise-abstract name doesn't reveal what's behind it), forcing a
   record type declared via [Thing.Make] to be unusable with its own field
   syntax ([x.base]). Naming [X.t] directly sidesteps the issue: nothing
   here ever needs its own name for the type, so nothing is hidden. *)
module Make (X : sig
    type t

    val name : string
    val json : ?as_elt:bool -> t -> Yojson.t
    val equal : t -> t -> bool
    val compare : t -> t -> int
  end) : sig
  (* Kept (not eliminated, unlike [t]) so a component can still be passed
     directly as a Key/Value argument to [Json.Map.Make] (e.g. for a
     Hashtbl-based component elsewhere) -- nothing ever field-accesses
     through [k], so exposing it under its own name is safe. *)
  type k = X.t

  val name : string
  val json : ?as_elt:bool -> X.t -> Yojson.t
  val to_string : ?pretty:bool -> X.t -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> X.t -> unit
  val write : ?dir:string -> string -> X.t -> unit
  val equal : X.t -> X.t -> bool
  val compare : X.t -> X.t -> int
  val show : X.t -> string
  val pp : Format.formatter -> X.t -> unit
end = struct
  module J = struct
    type k = X.t

    let name = X.name
    let json = X.json
  end

  include Json.Thing.Make (J)

  let equal = X.equal
  let compare = X.compare
  let show : X.t -> string = to_string ~pretty:false

  let pp (ppf : Format.formatter) (x : X.t) : unit =
    Format.pp_print_string ppf (show x)
  ;;
end

(* [Set (X) (N)] is the Set-of-[X] companion, carrying both [Showable]'s
   pp/show/subset/random/etc and the JSON-dump mechanism together. [N.name]
   is separate from [X.name] because the existing dump format names a set
   differently from its element (e.g. "States", not "State"). *)
module Set
    (X : S)
    (N : sig
       val name : string
     end) =
struct
  module Set_ = Showable.Set.Make (X)
  include Set_

  include Json.Set.Make (struct
      module Set = Set_

      let name = N.name
      let json = X.json
    end)
end
