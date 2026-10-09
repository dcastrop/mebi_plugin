(** Types the plugin can print, and order. Plain OCaml, written by hand:
    [make]'s build has no equivalent of dune's per-library
    [(preprocess (pps ...))], so [ppx]-derived functions would exist only
    under [dune build]. *)

(** A type that can be printed. *)
module type S = sig
  type t

  (** [pp ppf x] prints [x] on [ppf]. *)
  val pp : Format.formatter -> t -> unit

  (** [show x] is [x], printed. *)
  val show : t -> string
end

(** A type that can be ordered, compared for equality and printed: what a
    set ({!Set_.Make}) needs of its elements. *)
module type Ordered = sig
  type t

  include Set.OrderedType with type t := t

  val equal : t -> t -> bool

  include S with type t := t
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
