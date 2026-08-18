include Set

module type Ordered = Showable_.Type_.Ordered

module type S = sig
  include Set.S
  include Showable_.Type_.S with type t := t
  module Elt : Ordered with type t := elt

  val subset : t -> t -> bool
  val subseteq : t -> t -> bool
  val supset : t -> t -> bool
  val supseteq : t -> t -> bool
  val bfold : (elt -> bool -> bool) -> t -> bool -> bool
  val disjunction : (elt -> bool -> bool) -> t -> bool
  val conjunction : (elt -> bool -> bool) -> t -> bool
  val make : (int -> elt) -> int -> t
  val random : t -> elt
end

module Make (X : Ordered) :
  S with type elt = X.t and module Elt = X and type Elt.t = X.t = struct
  include Set.Make (X)
  module Elt = X

  let subset a b = subset a b
  let subseteq a b = subset a b || equal a b
  let supset a b = subseteq b a
  let supseteq a b = subset b a
  let bfold : (elt -> bool -> bool) -> t -> bool -> bool = fold
  let disjunction f xs = bfold f xs false
  let conjunction f xs = bfold f xs true
  let make f n : t = List.init n f |> of_list

  (** todo: make Things = Set of Thing so it can use cardinal range for id match
  *)
  let random x =
    Random.self_init ();
    try
      if is_empty x
      then raise (Failure "empty set")
      else
        Random.int_in_range ~min:1 ~max:(cardinal x) - 1 |> List.nth (to_list x)
    with
    | Failure _ -> raise (Failure "empty set")
  ;;

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
