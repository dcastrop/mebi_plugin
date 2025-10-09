val split_at : int -> 'a list -> 'a list -> 'a list * 'a list
val strip_snd : ('a * 'a) list -> 'a list
val is_unit_option : unit option -> bool
val bool_opt_to_string : string -> bool option -> string
val string_opt_to_string : string -> string option -> string
val clean_string : string -> string
val print : ?show:bool -> string -> unit
val default_indent_val : int
val str_tabs : ?size:int -> int -> string
val get_key_of_val : ('a, 'b) Hashtbl.t -> 'b -> 'a option
val new_int_counter : ?start:int -> unit -> unit -> int
val ppstr : Pp.t -> string

val pstr_list :
  ?force_newline:bool ->
  ?empty_msg:string ->
  ?indent:int ->
  ('a -> string) ->
  'a list ->
  string

val pstr_list2 :
  ?empty_msgA:string ->
  ?empty_msgB:string ->
  ?indent:int ->
  ('b -> string) ->
  ('c -> string) ->
  ('b list * 'c list) list ->
  string

module Strfy : sig
  val str : string -> string
  val option : ('a -> string) -> 'a option -> string
  val evar : Evar.t -> string

  val constr :
    Environ.env -> Evd.evar_map -> Constr.t -> string

  val constr_opt :
    Environ.env -> Evd.evar_map -> Constr.t option -> string

  val constr_rel_decl :
    Environ.env ->
    Evd.evar_map ->
    Constr.rel_declaration ->
    string

  val econstr :
    Environ.env -> Evd.evar_map -> Evd.econstr -> string

  val econstr_rel_decl :
    Environ.env ->
    Evd.evar_map ->
    EConstr.rel_declaration ->
    string

  val goal : Environ.env -> Evd.evar_map -> Evar.t -> string
  val global : Names.GlobRef.t -> string
end
