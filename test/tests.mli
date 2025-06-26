(* exception
   UnexpectedExampleKind of Mebi_plugin.Examples.exa_kind

   val pstr_results :
   ?params:Mebi_plugin.Utils.Logging.params ->
   (string * (string * bool * bool) list) list ->
   string

   val pstr_exa_bisim :
   ?params:Mebi_plugin.Utils.Logging.params ->
   string ->
   Mebi_plugin.Fsm.t ->
   Mebi_plugin.Fsm.t ->
   string

   val run_all :
   ?params:Mebi_plugin.Utils.Logging.params -> unit -> unit

   exception QuickTestFailed of Mebi_plugin.Examples.t

   val quick_test_saturate_fsm :
   ?params:Mebi_plugin.Utils.Logging.params ->
   Mebi_plugin.Examples.t ->
   unit ->
   unit

   val quick_test_saturate_fsm_states :
   ?params:Mebi_plugin.Utils.Logging.params ->
   Mebi_plugin.Examples.t ->
   unit ->
   unit *)
