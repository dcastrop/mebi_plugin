(***********************************************************************)
module Log : Logger.S = Logger.MkDefault ()

let () = Log.Config.enable_output ()
let () = Log.Config.configure_output Debug false
let () = Log.Config.configure_output Trace false
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

let to_string
      (f : 'a -> string)
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (x : 'a t)
  : string
  =
  Utils.Strfy.record
    [ "enc", f x.enc
    ; "ind", Rocq_utils.Strfy.econstr env sigma x.ind
    ; ("kind", match x.kind with Type _ -> "Type" | LTS _ -> "LTS")
    ]
;;

(***********************************************************************)

exception UnexpectedKind of kind

let get_lts ({ kind; _ } : 'a t) : LTS.t =
  Log.trace __FUNCTION__;
  match kind with
  | LTS l -> l
  (* | _ -> invalid_cindef_kind () *)
  | x -> raise (UnexpectedKind x)
;;

let get_lts_term_type (x : 'a t) : EConstr.t =
  Log.trace __FUNCTION__;
  (get_lts x).term_type
;;

let get_lts_label_type (x : 'a t) : EConstr.t =
  Log.trace __FUNCTION__;
  (get_lts x).label_type
;;

let get_lts_constructor_types (x : 'a t) : LTS.constructor array =
  Log.trace __FUNCTION__;
  (get_lts x).constructor_types
;;

(***********************************************************************)

let get_lts_constructor_names (x : 'a t) : Names.Id.t array =
  Log.trace __FUNCTION__;
  get_lts_constructor_types x
  |> Array.map (fun ({ name; _ } : LTS.constructor) -> name)
;;

let get_lts_constructors (x : 'a t) : Rocq_utils.ind_constr array =
  Log.trace __FUNCTION__;
  get_lts_constructor_types x
  |> Array.map (fun ({ constructor; _ } : LTS.constructor) -> constructor)
;;

(***********************************************************************)

exception Mip_InconsistentNumConstructors of Declarations.one_inductive_body

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
  | Invalid_argument _ -> raise (Mip_InconsistentNumConstructors mip)
;;

(***********************************************************************)
