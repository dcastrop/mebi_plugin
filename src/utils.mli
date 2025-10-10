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
val ppstr : ?clean:bool -> Pp.t -> string

val nlsep :
  ?force_newline:bool -> ?indent:int -> unit -> string

val pstr_list :
  ?force_newline:bool ->
  ?label:string ->
  ?indent:int ->
  ?use:string * string ->
  ('a -> string) ->
  'a list ->
  string

module Strfy : sig
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

  val evar : Evar.t -> string
  val evar' : Environ.env -> Evd.evar_map -> Evar.t -> string

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

  val econstr_types :
    ?indent:int ->
    Environ.env ->
    Evd.evar_map ->
    Evd.econstr ->
    string

  val name_id : Names.variable -> string
  val global : Names.GlobRef.t -> string

  val concl :
    Environ.env -> Evd.evar_map -> Evd.econstr -> string

  val erel : 'a -> Evd.evar_map -> Evd.erelevance -> string

  type hyp =
    ( Evd.econstr,
      Evd.econstr,
      Evd.erelevance )
    Context.Named.Declaration.pt

  val hyp :
    ?force_newline:bool ->
    ?indent:int ->
    Environ.env ->
    Evd.evar_map ->
    hyp ->
    string

  val goal : ?indent:int -> Proofview.Goal.t -> string
end
