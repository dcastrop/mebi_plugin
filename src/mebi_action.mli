type action = { label : string; is_tau : bool option }

val eq : action -> action -> bool
