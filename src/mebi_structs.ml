(** [coq_lts] is a type containing the coq-based [lts] of a typed term. *)
type coq_lts =
  { env : Environ.env
  ; sigma : Evd.evar_map
  ; lts_type : Evd.econstr
  ; term_type : Evd.econstr
  ; type_lbls : Evd.econstr
  ; constr_names : Names.variable array
  ; constr_transitions : (Constr.rel_context * Constr.t) array
  ; start_term : Evd.econstr
  ; states : Evd.econstr list
  ; edges : Evd.econstr list
  }

(** [coq_lts ...] is a constructor for type [coq_lts], initialising with empty lists of [states] and [edges]. *)
let coq_lts
  ?(states : Evd.econstr list = [])
  ?(edges : Evd.econstr list = [])
  (env : Environ.env)
  (sigma : Evd.evar_map)
  (lts_type : Evd.econstr)
  (term_type : Evd.econstr)
  (type_lbls : Evd.econstr)
  (constr_names : Names.variable array)
  (constr_transitions : (Constr.rel_context * Constr.t) array)
  (start_term : Evd.econstr)
  : coq_lts
  =
  { env
  ; sigma
  ; lts_type
  ; term_type
  ; type_lbls
  ; constr_names
  ; constr_transitions
  ; start_term
  ; states
  ; edges
  }
;;

(** [coq_lts_update_sigma lts sigma] is [lts] with an updated [sigma]. *)
let coq_lts_update_sigma (lts : coq_lts) (sigma : Evd.evar_map) : coq_lts =
  match lts with
  | { env
    ; sigma = sigma'
    ; lts_type
    ; term_type
    ; type_lbls
    ; constr_names
    ; constr_transitions
    ; start_term
    ; states
    ; edges
    ; _
    } ->
    { env
    ; sigma
    ; lts_type
    ; term_type
    ; type_lbls
    ; constr_names
    ; constr_transitions
    ; start_term
    ; states
    ; edges
    }
;;

(** [coq_lts_update_states lts states] is [lts] with updated [states]. *)
let coq_lts_update_states (lts : coq_lts) (states : Evd.econstr list) : coq_lts =
  match lts with
  | { env
    ; sigma
    ; lts_type
    ; term_type
    ; type_lbls
    ; constr_names
    ; constr_transitions
    ; start_term
    ; states = states'
    ; edges
    ; _
    } ->
    { env
    ; sigma
    ; lts_type
    ; term_type
    ; type_lbls
    ; constr_names
    ; constr_transitions
    ; start_term
    ; states
    ; edges
    }
;;

(** [coq_lts_update_edges lts edges] is [lts] with updated [edges]. *)
let coq_lts_update_edges (lts : coq_lts) (edges : Evd.econstr list) : coq_lts =
  match lts with
  | { env
    ; sigma
    ; lts_type
    ; term_type
    ; type_lbls
    ; constr_names
    ; constr_transitions
    ; start_term
    ; states
    ; edges = edges'
    ; _
    } ->
    { env
    ; sigma
    ; lts_type
    ; term_type
    ; type_lbls
    ; constr_names
    ; constr_transitions
    ; start_term
    ; states
    ; edges
    }
;;
