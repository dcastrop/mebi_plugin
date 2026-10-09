(** The standard library's [Set], with sets that can be printed. *)
include Set

module type Ordered = Type_.Ordered

(** A set that can be printed ({!Type_.S}), with its element module. *)
module type S = sig
  include Set.S
  include Type_.S with type t := t
  module Elt : Ordered with type t := elt
end

(** [Make (X)] is the set of [X]s, printed as [{ x1; x2; ... }] in order. *)
module Make (X : Ordered) :
  S with type elt = X.t and module Elt = X and type Elt.t = X.t = struct
  include Set.Make (X)
  module Elt = X

  (* See [S]: [{ }] when empty, else the elements between braces, broken
     across lines as needed. *)
  let pp ppf s =
    if is_empty s
    then Format.pp_print_string ppf "{ }"
    else (
      let pp_sep ppf () = Format.fprintf ppf ";@ " in
      Format.fprintf
        ppf
        "@[<hv 2>{ %a }@]"
        (Format.pp_print_list ~pp_sep X.pp)
        (elements s))
  ;;

  let show s = Format.asprintf "%a" pp s
end
