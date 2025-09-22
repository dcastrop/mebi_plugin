
val default_bound : int
val the_bounds : (int * (int option)) ref
val reset_bounds : unit -> unit
val fst_bound : unit -> int
val snd_bound : unit -> int
val printout_bounds : unit -> unit 
val set_bounds : (int * (int option)) -> unit 

val the_dump_to_file : bool ref
val reset_dump_to_file : unit -> unit
val printout_dump_to_file : unit -> unit 
val set_dump_to_file : bool -> unit 

val reset_show_debug : unit -> unit
val printout_show_debug : unit -> unit 
val set_show_debug : bool -> unit 

val reset_show_details : unit -> unit
val printout_show_details : unit -> unit 
val set_show_details : bool -> unit 

val the_weak_mode : bool ref
val reset_weak_mode : unit -> unit
val printout_weak_mode : unit -> unit 
val set_weak_mode : bool -> unit 

val printout_all : unit -> unit

