type lts =
  { trm_type : EConstr.t
  ; lbl_type : EConstr.t
  ; constr_transitions : Rocq_utils.ind_constrs
  }

type kind =
  | Type of EConstr.t option
  | LTS of lts

type info =
  { name : EConstr.t
  ; constr_names : Names.Id.t array
  }

type t =
  { enc : Mebi_setup.Enc.t
  ; info : info
  ; kind : kind
  }

open Mebi_wrapper

module Log : Logger.LOGGER_TYPE =
  Logger.Make
    (Logger.Output.Rocq)
    (struct
      let prefix : string option = None

      let is_level_enabled : Logger.level -> bool =
        Logger.make_level_fun ~debug:false ()
      ;;
    end)

let get_lts_trm_type (c : t) : EConstr.t mm =
  Log.trace __FUNCTION__;
  match c.kind with LTS l -> return l.trm_type | _ -> invalid_cindef_kind ()
;;

let get_lts_label_type (c : t) : EConstr.t mm =
  Log.trace __FUNCTION__;
  match c.kind with LTS l -> return l.lbl_type | _ -> invalid_cindef_kind ()
;;

let get_constr_transitions (c : t) : Rocq_utils.ind_constrs mm =
  Log.trace __FUNCTION__;
  match c.kind with
  | LTS l -> return l.constr_transitions
  | _ -> invalid_cindef_kind ()
;;
