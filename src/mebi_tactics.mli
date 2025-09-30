type unfold_kind =
  | Gr of Names.GlobRef.t
  | Id of Names.variable
  | Ex of EConstr.t
  | Ce of Constrexpr.constr_expr

val unfold : ?pre:unit Proofview.tactic option -> unit ->
  unfold_kind -> unit Proofview.tactic Mebi_wrapper.mm

val unknown_counter : int ref
val new_auto_name : ?prefix:string -> unit -> string
val handle_name : string option -> string

type id_kind =
  | Id of Names.variable
  | Str of string
  | Unk of unit

val intro : ?pre:unit Proofview.tactic option -> unit -> id_kind -> unit Proofview.tactic

val intros :
  ?all:bool -> ?pre:unit Proofview.tactic option -> unit -> id_kind list -> unit Proofview.tactic
