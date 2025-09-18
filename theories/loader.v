Set Debug "backtrace".
Declare ML Module "coq-mebi.mebi".

(* Inductive printout_mode : Set :=
| RunCheck : printout_mode (* only warning printouts *)
| ShowInfo : printout_mode (* show more printouts *)
| Verbose  : printout_mode (* show more detailed printouts *)
.
Definition DefaultPrintoutMode := RunCheck.

Require Export String.
Inductive output_mode : Set := 
| OutputMode : option string -> printout_mode -> output_mode.
Definition DefaultOutputMode := OutputMode None DefaultPrintoutMode.

(** debugging commands *)
Inductive single_model_commands (A B : Type) : Type :=
| MakeLTS : A -> B -> B -> option nat -> output_mode -> single_model_commands A B 
| MakeFSM : A -> B -> B -> option nat -> output_mode -> single_model_commands A B 
| CheckSaturation : A -> B -> B -> option nat -> output_mode -> single_model_commands A B 
.

Inductive plugin_command (A B C D : Type) : Type :=
| CheckBisimilarity : A -> B -> B -> option nat -> 
                      C -> D -> D -> option nat -> 
                      output_mode -> plugin_command A B C D. *)


(* Check (MakeLTS nat bool 0 false true None DefaultOutputMode). *)
(* Check (MakeLTS nat (option bool) 0 None (Some _) None DefaultOutputMode). *)
