type 'a t

(********************************************)
(****** CORE DEFINITIONS ********************)
(********************************************)
val run : 'a t -> 'a
val return : 'a -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val map : ('a -> 'b) -> 'a t -> 'b t
val product : 'a t -> 'b t -> ('a * 'b) t
val iterate : int -> int -> 'a -> (int -> 'a -> 'a t) -> 'a t

(********************************************)
(****** ERRORS ******************************)
(********************************************)
val invalid_arity : Constr.types -> 'a t
val invalid_sort : Sorts.family -> 'a t
val invalid_ref : Names.GlobRef.t -> 'a t

(********************************************)
(****** GET & PUT STATE *********************)
(********************************************)
val get_env : Environ.env t
val get_sigma : Evd.evar_map t
val state : (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a) -> 'a t
val sandbox : 'a t -> 'a t

(********************************************)
(****** CTX-dependent wrappers **************)
(********************************************)
val make_constr_tbl : (module Hashtbl.S with type key = EConstr.t) t
val make_constr_set : (module Set.S with type elt = EConstr.t) t

(* val debug : ?params:logging_params -> (Environ.env -> Evd.evar_map -> Pp.t) -> unit t *)
val feedback
  :  ?params:Utils.logging_params
  -> (Environ.env -> Evd.evar_map -> Pp.t)
  -> unit t

module type Monad = sig
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let$ )
    :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
    -> ('a -> 'b t)
    -> 'b t

  val ( let$* )
    :  (Environ.env -> Evd.evar_map -> Evd.evar_map)
    -> (unit -> 'b t)
    -> 'b t

  val ( let$+ ) : (Environ.env -> Evd.evar_map -> 'a) -> ('a -> 'b t) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

module Monad_syntax : Monad
