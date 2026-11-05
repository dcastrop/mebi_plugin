(** The prototype on CUDD ({!Bdd_cudd}); built only where [mlcuddidl] is
    installed. *)
module R = Driver.Run (Bdd_cudd)

let () = R.main ()
