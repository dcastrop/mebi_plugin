type action =
  { 
   label : string
  ; is_tau : bool
  }

val eq : action -> action -> bool
