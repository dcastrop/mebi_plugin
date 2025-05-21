type 'a mm

(* needs to be exposed for [mebi_internalize.ml] *)
type coq_context =
  { coq_env : Environ.env
  ; coq_ctx : Evd.evar_map
  ; coq_enc : EConstr.t list (* ; coq_enc : (EConstr.t, int) Hashtbl.t *)
  }

(********************************************)
(****** CORE DEFINITIONS ********************)
(********************************************)
val run : 'a mm -> 'a
val return : 'a -> 'a mm
val bind : 'a mm -> ('a -> 'b mm) -> 'b mm
val map : ('a -> 'b) -> 'a mm -> 'b mm
val product : 'a mm -> 'b mm -> ('a * 'b) mm
val iterate : int -> int -> 'a -> (int -> 'a -> 'a mm) -> 'a mm

(********************************************)
(****** ERRORS ******************************)
(********************************************)
val invalid_arity : Constr.types -> 'a mm
val invalid_sort : Sorts.family -> 'a mm
val invalid_ref : Names.GlobRef.t -> 'a mm
val unknown_term_type : EConstr.t * EConstr.t * EConstr.t list -> 'a mm
val primary_lts_not_found : EConstr.t * EConstr.t list -> 'a mm

(********************************************)
(****** GET & PUT STATE *********************)
(********************************************)
val get_env : Environ.env mm
val get_sigma : Evd.evar_map mm
val state : (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a) -> 'a mm
val sandbox : 'a mm -> 'a mm

(********************************************)
(****** CTX-dependent wrappers **************)
(********************************************)
val make_constr_tbl : (module Hashtbl.S with type key = EConstr.t) mm
val make_constr_set : (module Set.S with type elt = EConstr.t) mm

val make_constr_tree_set
  : (module Set.S with type elt = EConstr.t * Constr_tree.t) mm

val debug : (Environ.env -> Evd.evar_map -> Pp.t) -> unit mm

module type Monad = sig
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

  val ( let$+ ) : (Environ.env -> Evd.evar_map -> 'a) -> ('a -> 'b mm) -> 'b mm
  val ( and+ ) : 'a mm -> 'b mm -> ('a * 'b) mm
end

module Monad_syntax : Monad
