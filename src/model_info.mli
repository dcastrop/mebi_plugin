type t =
  { mebi_info : mebi_info list option
  ; rocq_info : rocq_info list option
  ; weak_info : string list option
  }

and mebi_info =
  { is_complete : bool
  ; is_merged : bool
  ; bounds : bound_config
  }

and bound_config =
  { bound : int
  ; bound_for : boundable
  }

and boundable =
  | States
  | Transitions

and rocq_info =
  { enc : Mebi_setup.Enc.t
  ; pp : string
  ; constructors : rocq_constructor list
  }

and rocq_constructor =
  { index : int
  ; name : string
  ; bindings : EConstr.t Tactypes.bindings
  }

val mebi_info_to_string : ?args:Utils.Strfy.style_args -> mebi_info -> string

val mebi_info_list_option_to_string
  :  ?args:Utils.Strfy.style_args
  -> mebi_info list option
  -> string

val rocq_constructor_bindings_to_string
  :  ?args:Utils.Strfy.style_args
  -> EConstr.t Tactypes.bindings
  -> string

val rocq_constructor_to_string
  :  ?args:Utils.Strfy.style_args
  -> rocq_constructor
  -> string

val rocq_info_to_string : ?args:Utils.Strfy.style_args -> rocq_info -> string

val rocq_info_list_option_to_string
  :  ?args:Utils.Strfy.style_args
  -> rocq_info list option
  -> string

val to_string : ?args:Utils.Strfy.style_args -> t -> string
