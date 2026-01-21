(***********************************************************************)
module Log : Logger.LOGGER_TYPE = Logger.MkDefault ()

let () = Log.Config.configure_output Debug true
let () = Log.Config.configure_output Trace true
(***********************************************************************)

type t = Rocq_bindings.t
and map = Rocq_bindings.map
and extractor_binding = Rocq_bindings.extractor_binding

module Instructions = Rocq_bindings.Instructions

let update_map = Rocq_bindings.update_map
let add_instruction = Rocq_bindings.Instructions.append

(***********************************************************************)
exception Mebi_bindings_CannotFindBindingName of EConstr.t

let find_name
      (sigma : Evd.evar_map)
      (name_pairs : (EConstr.t * Names.Name.t) list)
      (x : EConstr.t)
  : Names.Name.t
  =
  Log.trace __FUNCTION__;
  match
    List.find_opt (fun (y, z) -> EConstr.eq_constr sigma x y) name_pairs
  with
  | None -> raise (Mebi_bindings_CannotFindBindingName x)
  | Some (_, n) -> n
;;

let extract_binding_map
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (name_pairs : (EConstr.t * Names.Name.t) list)
      (x : EConstr.t)
      (y : Constr.t)
  : map
  =
  Log.trace __FUNCTION__;
  let cmap : map = Rocq_bindings.C.create 0 in
  let rec f
            (acc : (Constr.t * extractor_binding) list)
            (b : Instructions.t)
            ((x, y) : EConstr.t * Constr.t)
    : unit
    =
    Log.trace __FUNCTION__;
    match EConstr.kind sigma x, Constr.kind y with
    | App (xty, xtys), App (yty, ytys) ->
      if EConstr.eq_constr sigma xty (EConstr.of_constr yty)
      then (
        (* NOTE: set to [-1] so that it is [0] on first use. *)
        let (tysindex, _), _ = Utils.new_int_counter ~start:(-1) () in
        Array.combine xtys ytys
        |> Array.iter (fun (xy : EConstr.t * Constr.t) ->
          let b' =
            add_instruction
              (Arg { root = yty; index = tysindex (); cont = Undefined })
              b
          in
          f acc b' xy))
      else ()
    | _, Rel _ ->
      let name = find_name sigma name_pairs x in
      update_map cmap y (name, add_instruction Done b);
      ()
    | _, _ -> ()
  in
  let () = f [] Undefined (x, y) in
  cmap
;;

let make_map
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (name_pairs : (EConstr.t * Names.Name.t) list)
      ((evar, rel) : EConstr.t * Constr.t)
  : map option
  =
  Log.trace __FUNCTION__;
  let cmap = extract_binding_map env sigma name_pairs evar rel in
  match Rocq_bindings.C.to_seq_values cmap |> List.of_seq with
  | [] -> None
  | [ (_, Done) ] -> None
  | _ -> Some cmap
;;

(***********************************************************************)

let use_no_bindings (xs : map option list) : bool =
  Log.trace __FUNCTION__;
  List.filter (function None -> false | _ -> true) xs |> List.is_empty
;;

(** [] ...
    @param name_map is a tuple list of evars and corresponding binding_names *)
let extract
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (name_pairs : (EConstr.t * Names.Name.t) list)
      (from : EConstr.t * Constr.t)
      (action : EConstr.t * Constr.t)
      (goto : EConstr.t * Constr.t)
  : t
  =
  Log.trace __FUNCTION__;
  let make_map : EConstr.t * Constr.t -> map option =
    make_map env sigma name_pairs
  in
  let from : map option = make_map from in
  let action : map option = make_map action in
  let goto : map option = make_map goto in
  if use_no_bindings [ from; action; goto ]
  then Rocq_bindings.No_Bindings
  else Rocq_bindings.Use_Bindings { from; action; goto }
;;

let extract_info (env : Environ.env) (sigma : Evd.evar_map) (x : Mebi_ind.t)
  : Evd.evar_map * Model_info.rocq_constructor list
  =
  Log.trace __FUNCTION__;
  (* NOTE: constructor tactic index starts from 1 -- ignore 0 below *)
  let (get_constructor_index, _), _ = Utils.new_int_counter ~start:0 () in
  let tys : Mebi_ind.lts_constructor array =
    Mebi_ind.get_lts_constructor_types x
  in
  Array.fold_left
    (fun ((sigma, acc) : Evd.evar_map * Model_info.rocq_constructor list)
      ({ name; constructor = ctx, c } : Mebi_ind.lts_constructor) ->
      let index : int = get_constructor_index () in
      let name : string = Names.Id.to_string name in
      let decls : Rocq_utils.econstr_decl list =
        Rocq_utils.get_econstr_decls ctx
      in
      let sigma, substl =
        Rocq_utils.mk_ctx_substl env sigma [] (List.rev decls)
      in
      let name_pairs = Rocq_utils.map_decl_evar_pairs decls substl in
      let args : Rocq_utils.constructor_args =
        Rocq_utils.extract_args ~substl c
      in
      let from, action, goto =
        Rocq_utils.constr_to_app c |> Rocq_utils.unpack_constr_args
      in
      let bindings : t =
        extract
          env
          sigma
          name_pairs
          (args.lhs, from)
          (args.act, action)
          (args.rhs, goto)
      in
      let h : Model_info.rocq_constructor = { index; name; bindings } in
      sigma, h :: acc)
    (sigma, [])
    tys
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

let rec get_bound_term
          (env : Environ.env)
          (sigma : Evd.evar_map)
          (x : EConstr.t)
  : Instructions.t -> EConstr.t
  =
  Log.trace __FUNCTION__;
  function
  | Undefined -> raise (Mebi_bindings_BindingInstruction_Undefined (x, x))
  | Done -> x
  | Arg { root; index; cont } ->
    (try
       match EConstr.kind sigma x with
       | App (xty, xtys) ->
         (* Log.thing
            ~__FUNCTION__
            Debug
            "x"
            x
            (Of (Rocq_utils.Strfy.econstr env sigma));
            Log.thing
            ~__FUNCTION__
            Debug
            "xty"
            xty
            (Of (Rocq_utils.Strfy.econstr env sigma));
            Log.thing
            ~__FUNCTION__
            Debug
            "root"
            root
            (Of (Rocq_utils.Strfy.constr env sigma)); *)
         if EConstr.eq_constr sigma xty (EConstr.of_constr root)
         then (
           try get_bound_term env sigma xtys.(index) cont with
           | Invalid_argument _ ->
             raise
               (Mebi_bindings_BindingInstruction_IndexOutOfBounds (x, index)))
         else raise (Mebi_bindings_BindingInstruction_NEQ (xty, root))
       | _ -> raise (Mebi_bindings_BindingInstruction_NotApp x)
     with
     | Mebi_bindings_BindingInstruction_Undefined (_, y) ->
       raise (Mebi_bindings_BindingInstruction_Undefined (x, y)))
;;

let get_explicit_bindings (env : Environ.env) (sigma : Evd.evar_map)
  : EConstr.t * map option -> EConstr.t Tactypes.explicit_bindings
  =
  Log.trace __FUNCTION__;
  function
  | _, None -> []
  | x, Some xmap ->
    Log.thing
      ~__FUNCTION__
      Debug
      "x"
      x
      (Of (Rocq_utils.Strfy.econstr env sigma));
    Rocq_utils.C.fold
      (fun (rel : Constr.t)
        ((name, inst) : Names.Name.t * Instructions.t)
        (acc : EConstr.t Tactypes.explicit_bindings) ->
        let q = get_quantified_hyp name in
        let bs = get_bound_term env sigma x inst in
        CAst.make (q, bs) :: acc)
      xmap
      []
;;

let get
      (env : Environ.env)
      (sigma : Evd.evar_map)
      (the_from : EConstr.t)
      (the_action : EConstr.t option)
      (the_goto : EConstr.t option)
  : t -> EConstr.t Tactypes.bindings
  =
  Log.trace __FUNCTION__;
  function
  | No_Bindings -> Tactypes.NoBindings
  | Use_Bindings { from; action; goto } ->
    (* let open Mebi_wrapper.Syntax in *)
    (* let* env = Mebi_wrapper.get_env in *)
    (* let* sigma = Mebi_wrapper.get_sigma in *)
    Log.thing
      ~__FUNCTION__
      Debug
      "the_from"
      the_from
      (Of (Rocq_utils.Strfy.econstr env sigma));
    Log.thing
      ~__FUNCTION__
      Debug
      "the_action"
      the_action
      (Of (Utils.Strfy.option (Of (Rocq_utils.Strfy.econstr env sigma))));
    Log.thing
      ~__FUNCTION__
      Debug
      "the_goto"
      the_goto
      (Of (Utils.Strfy.option (Of (Rocq_utils.Strfy.econstr env sigma))));
    let acc_some (acc : (EConstr.t * map option) list) (x : map option)
      : EConstr.t option -> (EConstr.t * map option) list
      =
      Log.trace __FUNCTION__;
      function None -> acc | Some y -> (y, x) :: acc
    in
    let to_iter : (EConstr.t * map option) list =
      acc_some (acc_some [ the_from, from ] action the_action) goto the_goto
    in
    (* let map (i : int) (acc : EConstr.t Tactypes.explicit_bindings) =
       Log.trace __FUNCTION__;
       Log.thing
       ~__FUNCTION__
       Debug
       "term"
       (List.nth to_iter i |> fst)
       Mebi_utils.Strfy.feconstr;
       let bindings = List.nth to_iter i |> get_explicit_bindings env sigma in
       List.flatten [ bindings; acc ]
       in*)
    let bindings : EConstr.t Tactypes.explicit_bindings =
      (* Mebi_wrapper.iterate 0 (List.length to_iter - 1) [] map *)
      List.map (get_explicit_bindings env sigma) to_iter |> List.flatten
    in
    (match bindings with
     | [] -> Tactypes.NoBindings
     | xs -> Tactypes.ExplicitBindings xs)
;;
