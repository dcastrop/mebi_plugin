module type S = sig
  type t

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module type Sa = sig
  type 'a t

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
end

module type Ordered = sig
  type t

  include Set.OrderedType with type t := t

  val equal : t -> t -> bool

  include S with type t := t
end

(* Hand-written rather than [@@deriving show, eq]: _CoqProject's make-based
   build has no equivalent of dune's per-library (preprocess (pps ...)), so
   these ppx-derived functions silently never existed under `make` -- only
   `dune build` ever ran them. These five types are small enough that
   writing pp/show/equal by hand avoids the dependency entirely. *)

module Unit : Ordered with type t = unit = struct
  type t = unit

  let compare () () = 0
  let equal () () = true
  let pp (ppf : Format.formatter) () : unit = Format.pp_print_string ppf "()"
  let show () : string = "()"
end

module String : Ordered with type t = string = struct
  type t = string

  let compare = String.compare
  let equal = String.equal
  let pp (ppf : Format.formatter) (x : t) : unit = Format.fprintf ppf "%S" x
  let show (x : t) : string = Format.asprintf "%S" x
end

module Int : Ordered with type t = int = struct
  type t = int

  let compare = Int.compare
  let equal = Int.equal
  let pp (ppf : Format.formatter) (x : t) : unit = Format.pp_print_int ppf x
  let show (x : t) : string = string_of_int x
end

module Bool : Ordered with type t = bool = struct
  type t = bool

  let compare = Bool.compare
  let equal = Bool.equal
  let pp (ppf : Format.formatter) (x : t) : unit = Format.pp_print_bool ppf x
  let show (x : t) : string = string_of_bool x
end

(* [Yojson.t] (included into [Yojson_compare]) already provides pp/show/equal
   directly -- no need to derive or hand-write them here. *)
module Json : Ordered with type t = Yojson_compare.t = struct
  type t = Yojson_compare.t

  let compare = Yojson_compare.compare
  let equal = Yojson_compare.equal
  let pp = Yojson_compare.pp
  let show = Yojson_compare.show
end
