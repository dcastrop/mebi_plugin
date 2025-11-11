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

val list_of_constr_kinds : Constr.t -> (string * bool) list

val list_of_econstr_kinds :
  Evd.evar_map -> Evd.econstr -> (string * bool) list

module Strfy : sig
  val nlsep :
    ?force_newline:bool -> ?indent:int -> unit -> string

  val list :
    ?force_newline:bool ->
    ?label:string ->
    ?indent:int ->
    ?use:string * string ->
    ('a -> string) ->
    'a list ->
    string

  val array :
    ?force_newline:bool ->
    ?label:string ->
    ?indent:int ->
    ?use:string * string ->
    ('a -> string) ->
    'a array ->
    string

  val str : string -> string
  val int : int -> string
  val bool : bool -> string
  val option : ('a -> string) -> 'a option -> string

  val tuple :
    ?force_newline:bool ->
    ?is_keyval:bool ->
    ?indent:int ->
    ('a -> string) ->
    ('b -> string) ->
    'a * 'b ->
    string
end
