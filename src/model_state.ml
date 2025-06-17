type t = Mebi_wrapper.E.t

let eq (s1 : t) (s2 : t) = Mebi_wrapper.E.eq s1 s2
let compare (s1 : t) (s2 : t) = Mebi_wrapper.E.compare s1 s2
let to_string (t : t) = Mebi_wrapper.E.to_string t

let pstr ?(indents : int = 0) (t : t) =
  Printf.sprintf "%s%s" (Utils.str_tabs indents) (to_string t)
;;
