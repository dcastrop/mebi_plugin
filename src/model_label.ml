type meta =
  { is_silent : bool option
  ; info : string option
  }

type t = Mebi_wrapper.E.t

let eq (t1 : t) (t2 : t) = Mebi_wrapper.E.eq t1 t2
let compare (t1 : t) (t2 : t) = Mebi_wrapper.E.compare t1 t2
let hash (t : t) = Mebi_wrapper.E.hash t
