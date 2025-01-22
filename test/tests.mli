val ks90_exas
  :  ?show:bool
  -> ?details:bool
  -> ?debug:bool
  -> Mebi_plugin.Examples.example list
  -> (string * bool * bool) list

val run_all_ks90
  :  ?show:bool
  -> ?details:bool
  -> ?debug:bool
  -> unit
  -> (string * bool * bool) list

val run_all : ?show:bool -> ?details:bool -> ?debug:bool -> unit -> unit
