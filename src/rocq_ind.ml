(***********************************************************************)
module Log : Logger.LOGGER_TYPE = Logger.MkDefault ()

let () = Log.Config.enable_output ()
let () = Log.Config.configure_output Debug true
let () = Log.Config.configure_output Trace true
(***********************************************************************)

module LTS = struct
  type t =
    { term_type : EConstr.t
    ; label_type : EConstr.t
    ; constructor_types : constructor array
    }

  and constructor =
    { name : Names.Id.t
    ; constructor : Rocq_utils.ind_constr
    }
end

(***********************************************************************)

type 'a t =
  { enc : 'a
  ; ind : EConstr.t
  ; kind : kind
  }

and kind =
  | Type of EConstr.t option
  | LTS of LTS.t

(***********************************************************************)

exception Rocq_ind_UnexpectedKind of kind

let get_lts ({ kind; _ } : 'a t) : LTS.t =
  match kind with
  | LTS l -> l
  (* | _ -> invalid_cindef_kind () *)
  | x -> raise (Rocq_ind_UnexpectedKind x)
;;

let get_lts_term_type (x : 'a t) : EConstr.t = (get_lts x).term_type
let get_lts_label_type (x : 'a t) : EConstr.t = (get_lts x).label_type

let get_lts_constructor_types (x : 'a t) : LTS.constructor array =
  (get_lts x).constructor_types
;;

(***********************************************************************)

let get_lts_constructor_names (x : 'a t) : Names.Id.t array =
  get_lts_constructor_types x
  |> Array.map (fun ({ name; _ } : LTS.constructor) -> name)
;;

let get_lts_constructors (x : 'a t) : Rocq_utils.ind_constr array =
  get_lts_constructor_types x
  |> Array.map (fun ({ constructor; _ } : LTS.constructor) -> constructor)
;;

(***********************************************************************)

exception
  Rocq_ind_mip_InconsistentNumConstructors of Declarations.one_inductive_body

let mip_to_lts_constructors (mip : Declarations.one_inductive_body)
  : LTS.constructor array
  =
  Log.trace __FUNCTION__;
  try
    Array.combine mip.mind_consnames mip.mind_nf_lc
    |> Array.fold_left
         (fun (acc : LTS.constructor list)
           ((name, constructor) : Names.Id.t * Rocq_utils.ind_constr) ->
           { name; constructor } :: acc)
         []
    |> List.rev
    |> Array.of_list
  with
  | Invalid_argument _ -> raise (Rocq_ind_mip_InconsistentNumConstructors mip)
;;

(***********************************************************************)
