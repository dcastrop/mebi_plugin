(***********************************************************************)
module Log : Logger.LOGGER_TYPE = Logger.MkDefault ()

let () = Log.Config.enable_output ()
let () = Log.Config.configure_output Debug true
let () = Log.Config.configure_output Trace true
(***********************************************************************)

type t =
  { enc : Mebi_setup.Enc.t
  ; ind : EConstr.t
  ; kind : kind
  }

(* and info =
  { name : EConstr.t
  ; constr_names : Names.Id.t array
  } *)
and kind =
  | Type of EConstr.t option
  | LTS of lts

and lts =
  { term_type : EConstr.t
  ; label_type :
      EConstr.t (* ; constr_transitions : Rocq_utils.ind_constr array *)
  ; constructor_types : lts_constructor array
  }

and lts_constructor =
  { name : Names.Id.t
  ; constructor : Rocq_utils.ind_constr
  }

(* TODO: remove the unnecessary Mebi_wrapper.mm from these, just add a [try with] around the outside *)
(* open Mebi_wrapper *)
exception Mebi_ind_ExpectedLTSNotType of t

let get_lts_term_type : t -> EConstr.t =
  Log.trace __FUNCTION__;
  function
  | { kind = LTS l; _ } -> l.term_type
  (* | _ -> invalid_cindef_kind () *)
  | x -> raise (Mebi_ind_ExpectedLTSNotType x)
;;

let get_lts_label_type : t -> EConstr.t =
  Log.trace __FUNCTION__;
  function
  | { kind = LTS l; _ } -> l.label_type
  (* | _ -> invalid_cindef_kind () *)
  | x -> raise (Mebi_ind_ExpectedLTSNotType x)
;;

let get_lts_constructor_types : t -> lts_constructor array =
  Log.trace __FUNCTION__;
  function
  | { kind = LTS l; _ } -> l.constructor_types
  (* | _ -> invalid_cindef_kind () *)
  | x -> raise (Mebi_ind_ExpectedLTSNotType x)
;;

let get_lts_constructor_names (x : t) : Names.Id.t array =
  Log.trace __FUNCTION__;
  get_lts_constructor_types x |> Array.map (fun { name; _ } -> name)
;;

let get_lts_constructors (x : t) : Rocq_utils.ind_constr array =
  Log.trace __FUNCTION__;
  get_lts_constructor_types x
  |> Array.map (fun { constructor; _ } -> constructor)
;;

(***********************************************************************)

exception
  Mebi_Utils_mip_InconsistentNumConstructors of Declarations.one_inductive_body

let mip_to_lts_constructors (mip : Declarations.one_inductive_body)
  : lts_constructor array
  =
  Log.trace __FUNCTION__;
  try
    Array.combine mip.mind_consnames mip.mind_nf_lc
    |> Array.fold_left
         (fun (acc : lts_constructor list)
           ((name, constructor) : Names.Id.t * Rocq_utils.ind_constr) ->
           { name; constructor } :: acc)
         []
    |> List.rev
    |> Array.of_list
  with
  | Invalid_argument _ -> raise (Mebi_Utils_mip_InconsistentNumConstructors mip)
;;
