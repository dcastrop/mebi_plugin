module Instructions : sig
  type t =
    | Undefined
    | Done
    | Arg of
        { root : Constr.t
        ; index : int
        ; cont : t
        }

  exception Rocq_bindings_CannotAppendDone of unit

  val append : t -> t -> t
  val length : t -> int
  val to_string : Environ.env -> Evd.evar_map -> t -> string
end

module C = Rocq_utils.C

type t =
  | No_Bindings
  | Use_Bindings of arg_maps

and arg_maps =
  { from : map option
  ; action : map option
  ; goto : map option
  }

and map = extractor_binding C.t
and extractor_binding = Names.Name.t * Instructions.t

val update_map : map -> Constr.t -> extractor_binding -> unit

val to_string
  :  ?args:Utils.Strfy.style_args
  -> ?envsigma:(Environ.env * Evd.evar_map) option
  -> t
  -> string
