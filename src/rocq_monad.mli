module Make : (Log : Logger.S) (Ctx : Rocq_context.S) (Enc : Encoding.S) -> sig
  include module type of Bi_encoding.Make (Log) (Ctx) (Enc)

  val bienc_to_list : unit -> (Enc.t * EConstr.t) list

  type 'a mm = wrapper ref -> 'a in_wrapper

  and wrapper =
    { ctx : Rocq_context.t ref
    ; maps : maps ref
    }

  and 'a in_wrapper =
    { state : wrapper ref
    ; value : 'a
    }

  val run : ?reset_encoding:bool -> 'a mm -> 'a
  val return : 'a -> 'a mm
  val bind : 'a mm -> ('a -> 'b mm) -> 'b mm
  val map : ('a -> 'b) -> 'a mm -> 'b mm
  val product : 'a mm -> 'b mm -> ('a * 'b) mm
  val iterate : int -> int -> 'a -> (int -> 'a -> 'a mm) -> 'a mm

  val state
    :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
    -> wrapper ref
    -> 'a in_wrapper

  val sandbox : ?sigma:Evd.evar_map -> 'a mm -> wrapper ref -> 'a in_wrapper

  module type SYNTAX = sig
    val ( let+ ) : 'a mm -> ('a -> 'b) -> 'b mm
    val ( let* ) : 'a mm -> ('a -> 'b mm) -> 'b mm

    val ( let$ )
      :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
      -> ('a -> 'b mm)
      -> 'b mm

    val ( let$* )
      :  (Environ.env -> Evd.evar_map -> Evd.evar_map)
      -> (unit -> 'b mm)
      -> 'b mm

    val ( let$+ )
      :  (Environ.env -> Evd.evar_map -> 'a)
      -> ('a -> 'b mm)
      -> 'b mm

    val ( and+ ) : 'a mm -> 'b mm -> ('a * 'b) mm
  end

  module Syntax : SYNTAX

  val econstr_normalize : EConstr.t -> EConstr.t mm
  val encode : EConstr.t -> Enc.t
  val get_ctx : wrapper ref -> Rocq_context.t in_wrapper
  val get_env : wrapper ref -> Environ.env in_wrapper
  val get_sigma : wrapper ref -> Evd.evar_map in_wrapper
  val get_maps : wrapper ref -> maps in_wrapper
  val get_fwdmap : wrapper ref -> Enc.t F.t in_wrapper
  val get_bckmap : wrapper ref -> EConstr.t B.t in_wrapper
  val fstring : (Environ.env -> Evd.evar_map -> 'a -> string) -> 'a -> string
end
