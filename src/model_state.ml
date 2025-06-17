type t = Mebi_wrapper.E.t * string option

let eq (s1 : t) (s2 : t) = Mebi_wrapper.E.eq (fst s1) (fst s2)
let compare (s1 : t) (s2 : t) = Mebi_wrapper.E.compare (fst s1) (fst s2)
let hash (t : t) = Mebi_wrapper.E.hash (fst t)
let to_string (t : t) = Mebi_wrapper.E.to_string (fst t)

let pstr ?(indents : int = 0) (t : t) =
  Printf.sprintf "%s%s" (Utils.str_tabs indents) (to_string t)
;;
