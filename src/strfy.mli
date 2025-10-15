val str_tabs : ?size:int -> int -> string
val nlsep : ?force_newline:bool -> ?indent:int -> unit -> string

val list
  :  ?force_newline:bool
  -> ?label:string
  -> ?indent:int
  -> ?use:string * string
  -> ('a -> string)
  -> 'a list
  -> string

val str : string -> string
val int : int -> string
val bool : bool -> string
val option : ('a -> string) -> 'a option -> string

val tuple
  :  ?force_newline:bool
  -> ?is_keyval:bool
  -> ?indent:int
  -> ('a -> string)
  -> ('b -> string)
  -> 'a * 'b
  -> string

val pp : ?clean:bool -> Pp.t -> string
val evar : Evar.t -> string
val evar' : Environ.env -> Evd.evar_map -> Evar.t -> string
val constr : Environ.env -> Evd.evar_map -> Constr.t -> string
val constr_opt : Environ.env -> Evd.evar_map -> Constr.t option -> string

val constr_rel_decl
  :  Environ.env
  -> Evd.evar_map
  -> Constr.rel_declaration
  -> string

val constr_kind
  :  ?indent:int
  -> Environ.env
  -> Evd.evar_map
  -> Constr.t
  -> string

val econstr : Environ.env -> Evd.evar_map -> EConstr.t -> string

val econstr_rel_decl
  :  Environ.env
  -> Evd.evar_map
  -> EConstr.rel_declaration
  -> string

val econstr_types
  :  ?indent:int
  -> Environ.env
  -> Evd.evar_map
  -> EConstr.t
  -> string

val econstr_kind
  :  ?indent:int
  -> Environ.env
  -> Evd.evar_map
  -> EConstr.t
  -> string

val name_id : Names.variable -> string
val global : Names.GlobRef.t -> string
val concl : ?indent:int -> Environ.env -> Evd.evar_map -> EConstr.t -> string
val erel : 'a -> Evd.evar_map -> Evd.erelevance -> string

val hyp
  :  ?force_newline:bool
  -> ?indent:int
  -> Environ.env
  -> Evd.evar_map
  -> Mebi_setup.hyp
  -> string

val goal : ?indent:int -> Proofview.Goal.t -> string
val enc : Mebi_setup.Enc.t -> string

val enc_econstr_pair
  :  ?indent:int
  -> Environ.env
  -> Evd.evar_map
  -> Mebi_setup.Enc.t * EConstr.t
  -> string

val enc_econstr_opt_pair
  :  ?indent:int
  -> Environ.env
  -> Evd.evar_map
  -> Mebi_setup.Enc.t option * EConstr.t
  -> string

val coq_info : ?indent:int -> Model.Info.Coq.t -> string
val info : ?indent:int -> Model.Info.t -> string
val state : ?indent:int -> Model.State.t -> string
val states : ?indent:int -> Model.States.t -> string
val action_label : ?indent:int -> Model.Alphabet.elt -> string
val action_metadata : ?indent:int -> Model.Action.MetaData.t -> string

val action_annotation_pair
  :  ?indent:int
  -> Model.Action.annotation_pair
  -> string

val action_annotation : ?indent:int -> Model.Action.annotation -> string
val action_annotations : ?indent:int -> Model.Action.annotations -> string
val action : ?indent:int -> Model.Action.t -> string
