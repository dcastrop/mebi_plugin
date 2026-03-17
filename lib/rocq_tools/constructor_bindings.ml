module type S = sig
  type 'a mm
  type ind
  type instructions
  type bindings
  type constrmap

  type t =
    { index : int
    ; name : string
    ; bindings : bindings
    }

  include Json.S with type k = t

  val extract_info : ind -> t list mm
  val get_quantified_hyp : Names.Name.t -> Tactypes.quantified_hypothesis

  exception BindingInstruction_NotApp of EConstr.t
  exception BindingInstruction_Undefined of EConstr.t * EConstr.t
  exception BindingInstruction_IndexOutOfBounds of EConstr.t * int
  exception BindingInstruction_NEQ of EConstr.t * Constr.t

  val get_bound_term : EConstr.t -> instructions -> EConstr.t mm

  val get_explicit_bindings
    :  EConstr.t * constrmap option
    -> EConstr.t Tactypes.explicit_bindings mm

  val get
    :  EConstr.t
    -> EConstr.t option
    -> EConstr.t option
    -> bindings
    -> EConstr.t Tactypes.bindings mm
end

module Make
    (Log : Logger.S)
    (M : Rocq_monad_utils.S)
    (Bindings : Bindings.S with type 'a mm = 'a M.mm) :
  S
  with type 'a mm = 'a M.mm
   and type ind = M.Ind.t
   and type instructions = Bindings.Instructions.t
   and type bindings = Bindings.t
   and type constrmap = Bindings.ConstrMap.t' = struct
  type 'a mm = 'a M.mm

  open M

  type ind = Ind.t
  type instructions = Bindings.Instructions.t
  type bindings = Bindings.t
  type constrmap = Bindings.ConstrMap.t'

  type t =
    { index : int
    ; name : string
    ; bindings : Bindings.t
    }

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "ConstructorBindings"

        let json ?as_elt (x : t) : Yojson.t =
          `Assoc
            [ "index", `Int x.index
            ; "name", `String x.name
            ; "bindings", Bindings.json ~as_elt:true x.bindings
            ]
        ;;
      end)

  let extract_info (x : Ind.t) : t list mm =
    Log.trace __FUNCTION__;
    let open Syntax in
    (* NOTE: constructor tactic index starts from 1 -- ignore 0 below *)
    let (get_constructor_index, _), _ = Utils.new_int_counter ~start:0 () in
    let tys : Ind.LTS.constructor array = Ind.get_lts_constructor_types x in
    let f (i : int) (acc : t list) : t list mm =
      Log.trace __FUNCTION__;
      let { name; constructor = ctx, c } : Ind.LTS.constructor = tys.(i) in
      let index : int = get_constructor_index () in
      let name : string = Names.Id.to_string name in
      let decls : Rocq_utils.econstr_decl list =
        Rocq_utils.get_econstr_decls ctx
      in
      let* substl = mk_ctx_substl [] (List.rev decls) in
      let name_pairs = Rocq_utils.map_decl_evar_pairs decls substl in
      let args : Rocq_utils.constructor_args =
        Rocq_utils.extract_args ~substl c
      in
      let from, action, goto =
        Rocq_utils.constr_to_app c |> Rocq_utils.unpack_constr_args
      in
      let* bindings : Bindings.t =
        Bindings.extract
          name_pairs
          (args.lhs, from)
          (args.act, action)
          (args.rhs, goto)
      in
      let h : t = { index; name; bindings } in
      h :: acc |> return
    in
    iterate 0 (Array.length tys - 1) [] f
  ;;

  (***********************************************************************)

  let get_quantified_hyp : Names.Name.t -> Tactypes.quantified_hypothesis =
    Log.trace __FUNCTION__;
    function
    | Names.Name.Anonymous -> Tactypes.AnonHyp (* FIXME: *) 0
    | Names.Name.Name v -> Tactypes.NamedHyp (CAst.make v)
  ;;

  exception BindingInstruction_NotApp of EConstr.t
  exception BindingInstruction_Undefined of EConstr.t * EConstr.t
  exception BindingInstruction_IndexOutOfBounds of EConstr.t * int
  exception BindingInstruction_NEQ of EConstr.t * Constr.t

  let rec get_bound_term (x : EConstr.t)
    : Bindings.Instructions.t -> EConstr.t mm
    =
    Log.trace __FUNCTION__;
    log_econstr ~__FUNCTION__ ~s:"x" x;
    function
    | Undefined -> raise (BindingInstruction_Undefined (x, x))
    | Done -> return x
    | Arg { root; index; cont } ->
      Bindings.Instructions.log ~__FUNCTION__ (Arg { root; index; cont });
      (try
         let open Syntax in
         let* kind = econstr_kind x in
         match kind with
         | App (xty, xtys) ->
           let* eq = econstr_eq ~enc:false xty (EConstr.of_constr root) in
           if eq
           then (
             try get_bound_term xtys.(index) cont with
             | Invalid_argument _ ->
               raise (BindingInstruction_IndexOutOfBounds (x, index)))
           else (
             log_econstr ~__FUNCTION__ ~s:"xty" xty;
             log_constr ~__FUNCTION__ ~s:"root" root;
             raise (BindingInstruction_NEQ (xty, root)))
         | _ -> raise (BindingInstruction_NotApp x)
       with
       | BindingInstruction_Undefined (_, y) ->
         raise (BindingInstruction_Undefined (x, y)))
  ;;

  let get_explicit_bindings
    :  EConstr.t * Bindings.ConstrMap.t' option
    -> EConstr.t Tactypes.explicit_bindings mm
    =
    Log.trace __FUNCTION__;
    function
    | _, None -> return []
    | x, Some xmap ->
      let open Syntax in
      let ys = Bindings.ConstrMap.to_seq_values xmap |> Array.of_seq in
      let f (i : int) (acc : EConstr.t Tactypes.explicit_bindings) =
        Log.trace __FUNCTION__;
        let name, inst = ys.(i) in
        Log.thing ~__FUNCTION__ Debug "name" name (Of Rocq_utils.Strfy.name);
        Bindings.Instructions.log ~__FUNCTION__ inst;
        let q = get_quantified_hyp name in
        let* bs = get_bound_term x inst in
        CAst.make (q, bs) :: acc |> return
      in
      iterate 0 (Array.length ys - 1) [] f
  ;;

  let get
        (from' : EConstr.t)
        (action' : EConstr.t option)
        (goto' : EConstr.t option)
    : Bindings.t -> EConstr.t Tactypes.bindings mm
    =
    Log.trace __FUNCTION__;
    log_econstr ~__FUNCTION__ ~s:"from'" from';
    function
    | No_Bindings -> return Tactypes.NoBindings
    | Use_Bindings { from; action; goto } ->
      let acc_some
            (acc : (EConstr.t * Bindings.ConstrMap.t' option) list)
            (x : Bindings.ConstrMap.t' option)
        : EConstr.t option -> (EConstr.t * Bindings.ConstrMap.t' option) list
        = function
        | None -> acc
        | Some y -> (y, x) :: acc
      in
      let to_iter : (EConstr.t * Bindings.ConstrMap.t' option) list =
        acc_some (acc_some [ from', from ] action action') goto goto'
      in
      let open Syntax in
      let* bindings : EConstr.t Tactypes.explicit_bindings =
        let f (i : int) acc =
          Log.trace __FUNCTION__;
          let* x = get_explicit_bindings (List.nth to_iter i) in
          x :: acc |> return
        in
        let* xs = iterate 0 (List.length to_iter - 1) [] f in
        List.flatten xs |> return
      in
      (match bindings with
       | [] -> return Tactypes.NoBindings
       | xs -> return (Tactypes.ExplicitBindings xs))
  ;;
end
