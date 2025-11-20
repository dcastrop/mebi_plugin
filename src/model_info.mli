type t = {
  is_complete : bool;
  is_merged : bool;
  bound : int;
  num_terminals : int;
  num_labels : int;
  num_states : int;
  num_edges : int;
  rocq_info : rocq_info list option;
  weak_info : string list option;
}

and rocq_info = {
  enc : Mebi_setup.Enc.t;
  pp : string;
  constructor_names : string list;
}

val to_string : ?args:Utils.Strfy.style_args -> t -> string

val rocq_info_list_option_to_string :
  ?args:Utils.Strfy.style_args ->
  rocq_info list option ->
  string

val rocq_info_to_string :
  ?args:Utils.Strfy.style_args -> rocq_info -> string


