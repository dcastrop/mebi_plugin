val swap : 'a * 'b -> 'b * 'a
val split_at : int -> 'a list -> 'a list -> 'a list * 'a list
val strip_snd : ('a * 'a) list -> 'a list
val is_unit_option : unit option -> bool
val bool_opt_to_string : string -> bool option -> string
val string_opt_to_string : string -> string option -> string
val clean_string : string -> string
val print : ?show:bool -> string -> unit
val default_indent_val : int
val str_tabs : ?size:int -> int -> string
val infix : string -> string
val prefix : string -> string
val suffix : string -> string
val get_key_of_val : ('a, 'b) Hashtbl.t -> 'b -> 'a option

val new_int_counter :
  ?start:int ->
  unit ->
  ((unit -> int) * (unit -> int)) * int ref

module Strfy : sig
  type collection_delimiter =
    | Comma
    | Semi
    | Colon
    | Line
    | Use of string

  val collection_delimiter : collection_delimiter -> string

  type collection_marker =
    | Brace
    | Squig
    | Square
    | Angle
    | Use of string * string

  val collection_marker : collection_marker -> string * string

  type collection_style = {
    marker : collection_marker;
    delimiter : collection_delimiter;
    inline : bool;
    size : bool;
  }

  val delimiter : collection_style -> string
  val marker : collection_style -> string * string

  type collection_kind =
    | Tuple
    | Record
    | List
    | Use of collection_style

  val collection_style : collection_kind -> collection_style

  type style_args = {
    mutable indent : int;
    mutable newline : bool;
    mutable nested : bool;
    name : string option;
    style : collection_style option;
  }

  val style_args :
    ?indent:int ->
    ?newline:bool ->
    ?nested:bool ->
    ?name:string ->
    ?style:collection_style option ->
    unit ->
    style_args

  val keyval_style : unit -> collection_style
  val inline_tuple_style : unit -> collection_style
  val nlindent : int -> string
  val mkindent : int -> bool -> string
  val empty_msg : style_args -> string
  val size_msg : int -> style_args -> string
  val prefix : style_args -> string
  val push : style_args -> style_args
  val _pull : style_args -> style_args
  val nest : style_args -> style_args
  val show_empty_string : string
  val wrap : style_args -> string list -> string
  val string : ?args:style_args -> string -> string
  val int : ?args:style_args -> int -> string
  val bool : ?args:style_args -> bool -> string

  val option :
    (?args:style_args -> 'a -> string) ->
    ?args:style_args ->
    'a option ->
    string

  val tuple :
    (?args:style_args -> 'a -> string) ->
    (?args:style_args -> 'b -> string) ->
    ?args:style_args ->
    'a * 'b ->
    string

  val inline_tuple :
    (?args:style_args -> 'a -> string) ->
    (?args:style_args -> 'b -> string) ->
    ?args:style_args ->
    'a * 'b ->
    string

  val keyval :
    (?args:style_args -> 'a -> string) ->
    ?args:style_args ->
    string * 'a ->
    string

  val list :
    (?args:style_args -> 'a -> string) ->
    ?args:style_args ->
    'a list ->
    string

  val array :
    (?args:style_args -> 'a -> string) ->
    ?args:style_args ->
    'a array ->
    string

  val record :
    ?args:style_args ->
    ?args:style_args ->
    (string * string) list ->
    string
end
