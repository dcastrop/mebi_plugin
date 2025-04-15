type action =
  { id : int
  ; label : string
  ; is_tau : bool
  }

val eq : action -> action -> bool
