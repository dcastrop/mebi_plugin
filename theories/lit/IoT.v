
From Coq Require Export String.

(* following: https://doi.org/10.1145/2480362.2480615 *)


Definition Name := string.
Definition RefSensor := string.
Definition RefActuator := string.
Definition Channel := string.
Definition Value := string.
Definition Var := string.

Definition Label := string.

Record Sensor := {
  sname : RefSensor;
  svalue : Value
}.

Record Actuator := {
  aname : RefActuator;
  avalue : Value
}.

Inductive Process : Set :=
  | output : Channel -> Value -> Process
  | input : Channel -> Var -> Process
  | rinput : Channel -> Var -> Process
  | read : RefSensor -> Var -> Process
  | write : Value -> RefActuator -> Process
  | pnil : Process
  .

Inductive Body :=
  (* | sensor : Sensor *)
  (* | actuator : Actuator *)
  | process : Process -> Body
  .

(* Print (output "c" "x" (pnil)). *)

Record Node := {
  name : Name;
  body : list Body
}.

Record Link := {
  l : Name;
  r : Name
}.

Inductive System := {
  links : list Link;
  nodes : list Node
}.


Definition transition (s:System) (a:Label) : option System :=
  match s, a with
  | _, _ => None
  end.