val ind_constr :
  ?indent:int ->
  Environ.env ->
  Evd.evar_map ->
  Rocq_utils.ind_constr ->
  string

val ind_constrs :
  Environ.env ->
  Evd.evar_map ->
  Rocq_utils.ind_constrs ->
  string

val enc : Mebi_setup.Enc.t -> string

val enc_econstr_pair :
  ?indent:int ->
  Environ.env ->
  Evd.evar_map ->
  Mebi_setup.Enc.t * Evd.econstr ->
  string

val enc_econstr_opt_pair :
  ?indent:int ->
  Environ.env ->
  Evd.evar_map ->
  Mebi_setup.Enc.t option * Evd.econstr ->
  string

val coq_info : ?indent:int -> Model.Info.Coq.t -> string
val info : ?indent:int -> Model.Info.t -> string
val state : ?indent:int -> Model.State.t -> string
val states : ?indent:int -> Model.States.t -> string
val action_label : ?indent:int -> Model.Alphabet.elt -> string

val action_metadata :
  ?indent:int -> Model.Action.MetaData.t -> string

val action_annotation_pair :
  ?indent:int -> Model.Action.annotation_pair -> string

val action_annotation :
  ?indent:int -> Model.Action.annotation -> string

val action_annotations :
  ?indent:int -> Model.Action.annotations -> string

val action : ?indent:int -> Model.Action.t -> string
val constr_tree : ?indent:int -> Mebi_constr.Tree.t -> string

val lts_constr :
  ?indent:int ->
  Environ.env ->
  Evd.evar_map ->
  Mebi_constr.t ->
  string
