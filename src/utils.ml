(** [print ?show to_print] is a wrapper for [Printf.printf].
    @param ?show determines if [to_print] is outputted. *)
let print ?(show : bool = false) (to_print : string) : unit =
  match show with
  | true -> Printf.printf "%s" to_print
  | false -> ()
;;
