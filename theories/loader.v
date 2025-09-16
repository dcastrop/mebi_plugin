Set Debug "backtrace".
Declare ML Module "coq-mebi.mebi".

Inductive output_mode : Set :=
| RunCheck : output_mode (* only warning printouts *)
| ShowInfo : output_mode (* show more printouts *)
| Verbose  : output_mode (* show more detailed printouts *)
.
Definition DefaultOutputMode := RunCheck.

Inductive plugin_model_kind : Set :=
| AsLTS : plugin_model_kind
| AsFSM : plugin_model_kind
.
Definition DefaultPluginModelKind := AsLTS.

Section Definitions.
  Context (M : Type) (A : Type).
  Definition LTS : Type := M -> option A -> M -> Prop.
  Inductive term_to_model : Type := ModelInfo : A -> LTS -> term_to_model.
End Definitions.
About ModelInfo.
Arguments term_to_model {M A}.

(* Section ModelCommands.
  Context (M : Type) (A : Type).
  Inductive plugin_model_params : Type := 
  | Model : plugin_model_kind -> option nat -> term_to_model -> plugin_model_params
.
Definition DefaultPluginModel := Model DefaultPluginModelKind None.

Inductive dev_plugin_command : Type :=
| Saturate : plugin_model_params -> dev_plugin_command
.

Inductive plugin_command : Type :=
| Bisim : plugin_model_params -> plugin_model_params


End API. *)




(* Definition  *)
(* Record coq_lts := { term_type  : A; label_type : B }. *)

(* Definition term {A B : Type} :=  *)

(* Inductive build_model {A B : Type} (p:A) (ls:list coq_lts) := *)
(* | *)

(* Inductive build_model (A:Type) (B:Type) (a:A) (l:(A -> option B -> A -> Prop)) : Type :=
| LTS : build_model A B a l
| FSM : build_model A B a l
. *)


(* Inductive run_mode : Set :=
|  *)
