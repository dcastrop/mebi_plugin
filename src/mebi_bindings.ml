(***********************************************************************)
module Log : Logger.LOGGER_TYPE = Logger.MkDefault ()

let () = Log.Config.configure_output Debug true
let () = Log.Config.configure_output Trace true
(***********************************************************************)

(* NOTE: used locally -- DO NOT EXPORT *)
module C = Rocq_utils.C

type 'a mm = 'a Mebi_wrapper.mm

let return = Mebi_wrapper.return

(***********************************************************************)

type t = Rocq_bindings.t
and map = Rocq_bindings.map
and extractor_binding = Rocq_bindings.extractor_binding

module Instructions = Rocq_bindings.Instructions

let update_map = Rocq_bindings.update_map
let add_instruction = Rocq_bindings.Instructions.append

(***********************************************************************)
exception Mebi_bindings_CannotFindBindingName of EConstr.t

let find_name (name_pairs : (EConstr.t * Names.Name.t) list) (x : EConstr.t)
  : Names.Name.t mm
  =
  Log.trace __FUNCTION__;
  let open Mebi_wrapper.Syntax in
  let filter (i : int) : Names.Name.t option -> Names.Name.t option mm =
    Log.trace __FUNCTION__;
    function
    | None ->
      let ((y, z) : EConstr.t * Names.Name.t) = List.nth name_pairs i in
      let* is_eq : bool = Mebi_utils.econstr_eq x y in
      if is_eq then return (Some z) else return None
    | acc -> return acc
  in
  let* name_opt : Names.Name.t option =
    Mebi_wrapper.iterate 0 (List.length name_pairs - 1) None filter
  in
  match name_opt with
  | None -> raise (Mebi_bindings_CannotFindBindingName x)
  | Some n -> return n
;;

let extract_binding_map
      (name_pairs : (EConstr.t * Names.Name.t) list)
      (x : EConstr.t)
      (y : Constr.t)
  : map mm
  =
  Log.trace __FUNCTION__;
  let open Mebi_wrapper.Syntax in
  let cmap : map = C.create 0 in
  let rec f
            (acc : (Constr.t * extractor_binding) list)
            (b : Instructions.t)
            ((x, y) : EConstr.t * Constr.t)
    : unit mm
    =
    Log.trace __FUNCTION__;
    let* xkind = Mebi_utils.econstr_kind x in
    match xkind, Constr.kind y with
    | App (xty, xtys), App (yty, ytys) ->
      let* is_eq = Mebi_utils.econstr_eq xty (EConstr.of_constr yty) in
      if is_eq
      then (
        (* NOTE: set to [-1] so that it is [0] on first use. *)
        let (tysindex, _), _ = Utils.new_int_counter ~start:(-1) () in
        let xytys = Array.combine xtys ytys in
        let iter (i : int) (_ : unit) : unit mm =
          Log.trace __FUNCTION__;
          let (xy : EConstr.t * Constr.t) = xytys.(i) in
          let b' =
            add_instruction
              (Arg { root = yty; index = tysindex (); cont = Undefined })
              b
          in
          f acc b' xy
        in
        Mebi_wrapper.iterate 0 (Array.length xytys - 1) () iter)
      else return ()
    | _, Rel _ ->
      let* name = find_name name_pairs x in
      update_map cmap y (name, add_instruction Done b);
      return ()
    | _, _ -> return ()
  in
  let* _ = f [] Undefined (x, y) in
  return cmap
;;

let make_map
      (name_pairs : (EConstr.t * Names.Name.t) list)
      ((evar, rel) : EConstr.t * Constr.t)
  : map option mm
  =
  Log.trace __FUNCTION__;
  let open Mebi_wrapper.Syntax in
  let* cmap = extract_binding_map name_pairs evar rel in
  match C.to_seq_values cmap |> List.of_seq with
  | [] -> return None
  | [ (_, Done) ] -> return None
  | _ -> return (Some cmap)
;;

(***********************************************************************)

let use_no_bindings (xs : map option list) : bool =
  Log.trace __FUNCTION__;
  List.filter (function None -> false | _ -> true) xs |> List.is_empty
;;

(** [] ...
    @param name_map is a tuple list of evars and corresponding binding_names *)
let extract
      (name_pairs : (EConstr.t * Names.Name.t) list)
      (from : EConstr.t * Constr.t)
      (action : EConstr.t * Constr.t)
      (goto : EConstr.t * Constr.t)
  : t mm
  =
  Log.trace __FUNCTION__;
  let open Mebi_wrapper.Syntax in
  let make_map : EConstr.t * Constr.t -> map option mm = make_map name_pairs in
  let* from : map option = make_map from in
  let* action : map option = make_map action in
  let* goto : map option = make_map goto in
  if use_no_bindings [ from; action; goto ]
  then return Rocq_bindings.No_Bindings
  else
    (* raise (Invalid_argument "TODO:") *)
    return (Rocq_bindings.Use_Bindings { from; action; goto })
;;

let extract_info (x : Mebi_ind.t) : Model_info.rocq_constructor list mm =
  Log.trace __FUNCTION__;
  let open Mebi_wrapper.Syntax in
  (* NOTE: constructor tactic index starts from 1 -- ignore 0 below *)
  let (get_constructor_index, _), _ = Utils.new_int_counter ~start:0 () in
  let tys : Mebi_ind.lts_constructor array =
    Mebi_ind.get_lts_constructor_types x
  in
  let fold (i : int) (acc : Model_info.rocq_constructor list) =
    Log.trace __FUNCTION__;
    let ({ name; constructor = ctx, c } : Mebi_ind.lts_constructor) = tys.(i) in
    let index : int = get_constructor_index () in
    let name : string = Names.Id.to_string name in
    let decls : Rocq_utils.econstr_decl list =
      Rocq_utils.get_econstr_decls ctx
    in
    let* substl = Mebi_utils.mk_ctx_substl [] (List.rev decls) in
    let name_pairs = Mebi_utils.map_decl_evar_pairs decls substl in
    let* args : Mebi_utils.constructor_args =
      Mebi_utils.extract_args ~substl c
    in
    let from, action, goto =
      Mebi_utils.get_constr_app c |> Mebi_utils.unpack_constr_args
    in
    let* bindings : t =
      extract name_pairs (args.lhs, from) (args.act, action) (args.rhs, goto)
    in
    let h : Model_info.rocq_constructor = { index; name; bindings } in
    return (h :: acc)
  in
  Mebi_wrapper.iterate 0 (Array.length tys - 1) [] fold
;;

(***********************************************************************)

let get_quantified_hyp : Names.Name.t -> Tactypes.quantified_hypothesis =
  Log.trace __FUNCTION__;
  function
  | Names.Name.Anonymous -> Tactypes.AnonHyp (* FIXME: *) 0
  | Names.Name.Name v -> Tactypes.NamedHyp (CAst.make v)
;;

exception Mebi_bindings_BindingInstruction_NotApp of EConstr.t
exception Mebi_bindings_BindingInstruction_Undefined of EConstr.t * EConstr.t
exception Mebi_bindings_BindingInstruction_IndexOutOfBounds of EConstr.t * int
exception Mebi_bindings_BindingInstruction_NEQ of EConstr.t * Constr.t

let rec get_bound_term (x : EConstr.t) : Instructions.t -> EConstr.t mm =
  Log.trace __FUNCTION__;
  function
  | Undefined -> raise (Mebi_bindings_BindingInstruction_Undefined (x, x))
  | Done -> return x
  | Arg { root; index; cont } ->
    let open Mebi_wrapper.Syntax in
    (try
       let* xkind = Mebi_utils.econstr_kind x in
       match xkind with
       | App (xty, xtys) ->
         Log.thing ~__FUNCTION__ Debug "x" x (Of Mebi_utils.Strfy.econstr);
         Log.thing ~__FUNCTION__ Debug "xty" xty (Of Mebi_utils.Strfy.econstr);
         Log.thing ~__FUNCTION__ Debug "root" root (Of Mebi_utils.Strfy.constr);
         let* xeq : bool = Mebi_utils.econstr_eq xty (EConstr.of_constr root) in
         if xeq
         then (
           try get_bound_term xtys.(index) cont with
           | Invalid_argument _ ->
             raise
               (Mebi_bindings_BindingInstruction_IndexOutOfBounds (x, index)))
         else raise (Mebi_bindings_BindingInstruction_NEQ (xty, root))
       | _ -> raise (Mebi_bindings_BindingInstruction_NotApp x)
     with
     | Mebi_bindings_BindingInstruction_Undefined (_, y) ->
       raise (Mebi_bindings_BindingInstruction_Undefined (x, y)))
;;

let get_explicit_bindings
  : EConstr.t * map option -> EConstr.t Tactypes.explicit_bindings mm
  =
  Log.trace __FUNCTION__;
  function
  | _, None -> return []
  | x, Some xmap ->
    let open Mebi_wrapper.Syntax in
    let keyvals = C.to_seq xmap |> Array.of_seq in
    let fold (i : int) (acc : EConstr.t Tactypes.explicit_bindings) =
      Log.trace __FUNCTION__;
      let rel, (name, inst) = keyvals.(i) in
      let q = get_quantified_hyp name in
      let* bs = get_bound_term x inst in
      return (CAst.make (q, bs) :: acc)
    in
    Mebi_wrapper.iterate 0 (C.length xmap - 1) [] fold
;;

let get
      (the_from : EConstr.t)
      (the_action : EConstr.t option)
      (the_goto : EConstr.t option)
  : t -> EConstr.t Tactypes.bindings mm
  =
  Log.trace __FUNCTION__;
  function
  | No_Bindings -> return Tactypes.NoBindings
  | Use_Bindings { from; action; goto } ->
    let open Mebi_wrapper.Syntax in
    let acc_some (acc : (EConstr.t * map option) list) (x : map option)
      : EConstr.t option -> (EConstr.t * map option) list
      = function
      | None -> acc
      | Some y -> (y, x) :: acc
    in
    let to_iter : (EConstr.t * map option) list =
      acc_some (acc_some [ the_from, from ] action the_action) goto the_goto
    in
    let map (i : int) (acc : EConstr.t Tactypes.explicit_bindings) =
      Log.trace __FUNCTION__;
      let* bindings = List.nth to_iter i |> get_explicit_bindings in
      return (List.flatten [ bindings; acc ])
    in
    let* bindings : EConstr.t Tactypes.explicit_bindings =
      Mebi_wrapper.iterate 0 (List.length to_iter - 1) [] map
    in
    (match bindings with
     | [] -> return Tactypes.NoBindings
     | xs -> return (Tactypes.ExplicitBindings xs))
;;
