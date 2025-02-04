val pstr_results
  :  ?params:Mebi_plugin.Utils.logging_params
  -> (string * (string * bool * bool) list) list
  -> string

val ks90_exas
  :  ?params:Mebi_plugin.Utils.logging_params
  -> Mebi_plugin.Examples.example list
  -> (string * bool * bool) list

val run_all_ks90
  :  ?params:Mebi_plugin.Utils.logging_params
  -> unit
  -> (string * bool * bool) list

val run_all : ?params:Mebi_plugin.Utils.logging_params -> unit -> unit
