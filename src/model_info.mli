type t = {
  mebi_info : mebi_info list option;
  rocq_info : rocq_info list option;
  weak_info : string list option;
}

and mebi_info = {
  is_complete : bool;
  is_merged : bool;
  bound : int;
}

and rocq_info = {
  enc : Mebi_setup.Enc.t;
  pp : string;
  constructor_names : string list;
}

val mebi_info_to_string :
  ?args:Utils.Strfy.style_args -> mebi_info -> string

val mebi_info_list_option_to_string :
  ?args:Utils.Strfy.style_args ->
  mebi_info list option ->
  string

val rocq_info_to_string :
  ?args:Utils.Strfy.style_args -> rocq_info -> string

val rocq_info_list_option_to_string :
  ?args:Utils.Strfy.style_args ->
  rocq_info list option ->
  string

val to_string : ?args:Utils.Strfy.style_args -> t -> string
