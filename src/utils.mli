val is_unit_option : unit option -> bool

type model_info =
  { is_complete : bool
  ; bound : int
  }

val is_complete : model_info option -> bool option

module PStr : sig
  val model_info : model_info option -> string
end

module Logging : sig
  type output_modes =
    | Coq of unit
    | OCaml of unit

  type output_kind =
    | Normal of unit
    | Details of unit
    | Debug of unit
    | Warning of unit

  val pstr_output_kind_head : output_modes -> output_kind -> string

  type output_options =
    { mutable output_enabled : bool
    ; mutable show_normal_output : bool
    ; mutable show_detailed_output : bool
    ; mutable show_debug_output : bool
    ; mutable show_warning_output : bool
    }

  type params =
    { mode : output_modes
    ; mutable kind : output_kind
    ; mutable options : output_options
    ; mutable scope : string Stack.t
    ; mutable override : unit option
    }

  val default_params : ?mode:output_modes -> unit -> params
  val log_options : output_options -> params -> unit
  val log_kind : output_kind -> params -> unit
  val override : params -> params
  val push_scope : string -> params -> unit
  val peek_scope : string -> params -> string
  val pop_scope : string -> params -> string
  val pstr_scope : string Stack.t -> string
  val is_output_kind_enabled : params -> bool
  val log : ?params:params -> string -> unit

  module Log : sig
    val warning : ?params:params -> string -> unit
    val debug : ?params:params -> string -> unit
    val details : ?params:params -> string -> unit
  end
end

module Formatting : sig
  type params =
    { tabs : int
    ; no_leading_tab : bool
    ; params : Logging.params
    }

  val default_params
    :  ?params:Logging.params
    -> ?mode:Logging.output_modes
    -> unit
    -> params

  type pstr_params =
    | Log of Logging.params
    | Fmt of params

  val handle_params : pstr_params -> params
end

module Params : sig
  type fmt = Formatting.params
  type log = Logging.params
  type pstr = Formatting.pstr_params

  module Default : sig
    val fmt : ?params:log -> ?mode:Logging.output_modes -> unit -> fmt
    val log : ?mode:Logging.output_modes -> unit -> log
  end

  val handle : Formatting.pstr_params -> Formatting.params
end

val inc_tab : ?by:int -> Params.fmt -> Params.fmt
val dec_tab : ?by:int -> Params.fmt -> Params.fmt
val no_tab : Params.fmt -> Params.fmt
val no_leading_tab : bool -> Params.fmt -> Params.fmt
val print : ?show:bool -> string -> unit
val default_indent_val : int
val str_tabs : ?size:int -> int -> string
val get_key_of_val : ('a, 'b) Hashtbl.t -> 'b -> 'a option
val new_int_counter : unit -> unit -> int

type keys_kind = OfEConstr of Evd.econstr Seq.t

val pstr_keys : keys_kind -> string
