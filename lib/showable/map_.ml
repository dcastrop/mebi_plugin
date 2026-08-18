include Map

module type Ordered = Showable_.Type_.Ordered

module type S = sig
  include Map.S
  include Showable_.Type_.Sa with type 'a t := 'a t
  module Key : Ordered with type t := key
end

module Make (X : Ordered) : S with type key = X.t and module Key = X = struct
  include Map.Make (X)
  module Key = X

  let pp pp_v ppf m =
    if is_empty m
    then Format.pp_print_string ppf "{ }"
    else (
      let pp_sep ppf () = Format.fprintf ppf ";@ " in
      let pp_binding ppf (k, v) =
        Format.fprintf ppf "@[<hov 2>%a ->@ %a@]" X.pp k pp_v v
      in
      Format.fprintf
        ppf
        "@[<hv 2>{ %a }@]"
        (Format.pp_print_list ~pp_sep pp_binding)
        (bindings m))
  ;;

  let show pp_v m = Format.asprintf "%a" (pp pp_v) m
end
