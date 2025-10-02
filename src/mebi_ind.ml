type lts =
  { trm_type : EConstr.t
  ; lbl_type : EConstr.t
  ; constr_transitions : (Constr.rel_context * Constr.t) array
  }

type kind =
  | Type of EConstr.t option
  | LTS of lts

type info =
  { name : EConstr.t
  ; constr_names : Names.Id.t array
  }

type t =
  { index : int
  ; info : info
  ; kind : kind
  }

open Logging
open Mebi_wrapper

let get_lts_trm_type (c : t) : EConstr.t mm =
  Log.debug "mebi_ind.get_lts_trm_type";
  match c.kind with LTS l -> return l.trm_type | _ -> invalid_cindef_kind ()
;;

let get_constr_transitions (c : t) : (Constr.rel_context * Constr.t) array mm =
  Log.debug "mebi_ind.get_constr_transitions";
  match c.kind with
  | LTS l -> return l.constr_transitions
  | _ -> invalid_cindef_kind ()
;;
