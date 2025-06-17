type meta =
  { is_silent : bool option
  ; info : string option
  }

type t = Mebi_wrapper.E.t * string option

let eq (t1 : t) (t2 : t) = Mebi_wrapper.E.eq (fst t1) (fst t2)
let compare (t1 : t) (t2 : t) = Mebi_wrapper.E.compare (fst t1) (fst t2)
let hash (t : t) = Mebi_wrapper.E.hash (fst t)
let to_string (t : t) = Mebi_wrapper.E.to_string (fst t)

let pstr ?(indents : int = 0) (t : t) =
  Printf.sprintf "%s%s" (Utils.str_tabs indents) (to_string t)
;;
