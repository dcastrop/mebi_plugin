val perm : int
val default_output_dir : string

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

module JSON_Alphabet : sig
  type elt = json_action_name * (json_action_silent * json_action_annotations)
  type t

  val empty : t
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val disjoint : t -> t -> json_action_silent
  val diff : t -> t -> t
  val cardinal : t -> int
  val elements : t -> elt list
  val min_elt : t -> elt
  val min_elt_opt : t -> elt option
  val max_elt : t -> elt
  val max_elt_opt : t -> elt option
  val choose : t -> elt
  val choose_opt : t -> elt option
  val find : elt -> t -> elt
  val find_opt : elt -> t -> elt option
  val find_first : (elt -> json_action_silent) -> t -> elt
  val find_first_opt : (elt -> json_action_silent) -> t -> elt option
  val find_last : (elt -> json_action_silent) -> t -> elt
  val find_last_opt : (elt -> json_action_silent) -> t -> elt option
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val map : (elt -> elt) -> t -> t
  val filter : (elt -> json_action_silent) -> t -> t
  val filter_map : (elt -> elt option) -> t -> t
  val partition : (elt -> json_action_silent) -> t -> t * t
  val split : elt -> t -> t * json_action_silent * t
  val is_empty : t -> json_action_silent
  val mem : elt -> t -> json_action_silent
  val equal : t -> t -> json_action_silent
  val compare : t -> t -> int
  val subset : t -> t -> json_action_silent
  val for_all : (elt -> json_action_silent) -> t -> json_action_silent
  val exists : (elt -> json_action_silent) -> t -> json_action_silent
  val to_list : t -> elt list
  val of_list : elt list -> t
  val to_seq_from : elt -> t -> elt Seq.t
  val to_seq : t -> elt Seq.t
  val to_rev_seq : t -> elt Seq.t
  val add_seq : elt Seq.t -> t -> t
  val of_seq : elt Seq.t -> t
end

type json_state_name = json_action_name
type json_state_info = json_state_name

module JSON_States : sig
  type elt = json_state_info * json_state_info
  type t

  val empty : t
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val disjoint : t -> t -> json_action_silent
  val diff : t -> t -> t
  val cardinal : t -> int
  val elements : t -> elt list
  val min_elt : t -> elt
  val min_elt_opt : t -> elt option
  val max_elt : t -> elt
  val max_elt_opt : t -> elt option
  val choose : t -> elt
  val choose_opt : t -> elt option
  val find : elt -> t -> elt
  val find_opt : elt -> t -> elt option
  val find_first : (elt -> json_action_silent) -> t -> elt
  val find_first_opt : (elt -> json_action_silent) -> t -> elt option
  val find_last : (elt -> json_action_silent) -> t -> elt
  val find_last_opt : (elt -> json_action_silent) -> t -> elt option
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val map : (elt -> elt) -> t -> t
  val filter : (elt -> json_action_silent) -> t -> t
  val filter_map : (elt -> elt option) -> t -> t
  val partition : (elt -> json_action_silent) -> t -> t * t
  val split : elt -> t -> t * json_action_silent * t
  val is_empty : t -> json_action_silent
  val mem : elt -> t -> json_action_silent
  val equal : t -> t -> json_action_silent
  val compare : t -> t -> int
  val subset : t -> t -> json_action_silent
  val for_all : (elt -> json_action_silent) -> t -> json_action_silent
  val exists : (elt -> json_action_silent) -> t -> json_action_silent
  val to_list : t -> elt list
  val of_list : elt list -> t
  val to_seq_from : elt -> t -> elt Seq.t
  val to_seq : t -> elt Seq.t
  val to_rev_seq : t -> elt Seq.t
  val add_seq : elt Seq.t -> t -> t
  val of_seq : elt Seq.t -> t
end

module JSON_Edges : sig
  type elt =
    (json_state_info * json_state_info) * (json_state_info * json_state_info)

  type t

  val empty : t
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val disjoint : t -> t -> json_action_silent
  val diff : t -> t -> t
  val cardinal : t -> int
  val elements : t -> elt list
  val min_elt : t -> elt
  val min_elt_opt : t -> elt option
  val max_elt : t -> elt
  val max_elt_opt : t -> elt option
  val choose : t -> elt
  val choose_opt : t -> elt option
  val find : elt -> t -> elt
  val find_opt : elt -> t -> elt option
  val find_first : (elt -> json_action_silent) -> t -> elt
  val find_first_opt : (elt -> json_action_silent) -> t -> elt option
  val find_last : (elt -> json_action_silent) -> t -> elt
  val find_last_opt : (elt -> json_action_silent) -> t -> elt option
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val map : (elt -> elt) -> t -> t
  val filter : (elt -> json_action_silent) -> t -> t
  val filter_map : (elt -> elt option) -> t -> t
  val partition : (elt -> json_action_silent) -> t -> t * t
  val split : elt -> t -> t * json_action_silent * t
  val is_empty : t -> json_action_silent
  val mem : elt -> t -> json_action_silent
  val equal : t -> t -> json_action_silent
  val compare : t -> t -> int
  val subset : t -> t -> json_action_silent
  val for_all : (elt -> json_action_silent) -> t -> json_action_silent
  val exists : (elt -> json_action_silent) -> t -> json_action_silent
  val to_list : t -> elt list
  val of_list : elt list -> t
  val to_seq_from : elt -> t -> elt Seq.t
  val to_seq : t -> elt Seq.t
  val to_rev_seq : t -> elt Seq.t
  val add_seq : elt Seq.t -> t -> t
  val of_seq : elt Seq.t -> t
end

type model_info =
  { name : json_state_info
  ; kind : json_state_info
  ; extra : Utils.model_info option
  }

type json_model =
  { info : model_info
  ; alphabet : JSON_Alphabet.t
  ; initial_state : json_state_info
  ; state_list : JSON_States.t
  ; edge_list : JSON_Edges.t
  }

val string_opt : string option -> string
val is_model_complete : json_model -> bool option
val to_json_model : string -> dumpable_kind -> json_model
val write_json_extra_to_file : out_channel -> Utils.model_info option -> unit
val write_json_info_to_file : out_channel -> model_info -> unit
val write_json_alphabet_to_file : out_channel -> JSON_Alphabet.t -> unit
val write_json_states_to_file : out_channel -> JSON_States.t -> unit
val write_json_edges_to_file : out_channel -> JSON_Edges.t -> unit
val write_json_to_file : out_channel -> json_model -> unit

val write_to_file
  :  output_dir_kind
  -> filename_kind
  -> filetype_kind
  -> dumpable_kind
  -> string
