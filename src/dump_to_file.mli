val perm : int
val default_output_dir : string
val clean : string -> string

type output_dir_kind =
  | Default of unit
  | Exact of string

val get_local_timestamp : string

type filename_kind =
  | Auto of unit
  | Just of string
  | LTS of string
  | FSM of string

val get_name : filename_kind -> string
val get_filename : filename_kind -> bool option -> string

type filetype_kind = JSON of unit

val build_filename : filename_kind -> filetype_kind -> bool option -> string

val build_filepath
  :  output_dir_kind
  -> filename_kind
  -> filetype_kind
  -> bool option
  -> string

val create_parent_dir : string -> unit

type dumpable_kind = LTS of Lts.lts
type json_action_name = string
type json_action_silent = bool
type json_action_annotations = json_action_name list

type json_action =
  json_action_name * (json_action_silent * json_action_annotations)

type json_state_name = json_action_name
type json_state_info = json_state_name
type json_state = json_state_info * json_state_info
type json_edge_info = json_state_info

type json_edge =
  (json_edge_info * json_edge_info) * (json_edge_info * json_edge_info)

type model_info =
  { name : json_edge_info
  ; kind : json_edge_info
  ; extra : Utils.model_info option
  }

type json_model =
  { info : model_info
  ; alphabet : json_action Queue.t
  ; initial_state : json_action_name
  ; state_list : json_state Queue.t
  ; edge_list : json_edge Queue.t
  }

val string_opt : string option -> string
val is_model_complete : json_model -> bool option
val to_json_model : string -> dumpable_kind -> json_model
val handle_if_first : bool ref -> string
val write_json_extra_to_file : out_channel -> Utils.model_info option -> unit
val write_json_info_to_file : out_channel -> model_info -> unit
val write_json_alphabet_to_file : out_channel -> json_action Queue.t -> unit
val write_xl_string_to_file : out_channel -> string -> unit
val write_json_states_to_file : out_channel -> json_state Queue.t -> unit
val write_json_edges_to_file : out_channel -> json_edge Queue.t -> unit
val write_json_to_file : json_model -> string -> unit

val write_to_file
  :  output_dir_kind
  -> filename_kind
  -> filetype_kind
  -> dumpable_kind
  -> string
