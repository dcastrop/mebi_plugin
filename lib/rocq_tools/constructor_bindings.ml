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

  include Json.Thing.Make (struct
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

  (** [constructor_info index c] is the binder paths of the LTS constructor
      [c] ({!Bindings.extract}), with [index], [c]'s position for the
      [constructor] tactic (from 1), and its name. Each binder is first
      given a fresh evar, so that the evar term can be walked alongside the
      de Bruijn one.

      Raises, when run, whatever {!Bindings.extract} raises, and, if [c]'s
      type is not an application of its LTS, the errors of
      {!Rocq_utils.extract_args} and {!Rocq_utils.constr_to_app} (all
      propagated). *)
  let constructor_info (index : int) (c : Ind.LTS.constructor) : t mm =
    Logger.trace __FUNCTION__;
    let open Syntax in
    let { name; constructor = ctx, c } : Ind.LTS.constructor = c in
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
    return { index; name; bindings }
  ;;

  (* See the [.mli]. One {!constructor_info} per constructor, numbered from
     1, the last first. *)
  let extract_info (x : Ind.t) : t list mm =
    Logger.trace __FUNCTION__;
    let open Syntax in
    (* NOTE: constructor tactic index starts from 1 -- ignore 0 below *)
    let (get_constructor_index, _), _ = Utils.new_int_counter ~start:0 () in
    let tys : Ind.LTS.constructor array = Ind.get_lts_constructor_types x in
    iterate
      0
      (Array.length tys - 1)
      []
      (fun i acc ->
        let* h = constructor_info (get_constructor_index ()) tys.(i) in
        return (h :: acc))
  ;;

  (* See the [.mli]. *)
  let get_quantified_hyp : Names.Name.t -> Tactypes.quantified_hypothesis =
    Logger.trace __FUNCTION__;
    function
    | Names.Name.Anonymous -> Tactypes.AnonHyp (* FIXME: *) 0
    | Names.Name.Name v -> Tactypes.NamedHyp (CAst.make v)
  ;;

  exception BindingInstruction_NotApp of EConstr.t
  exception BindingInstruction_Undefined of EConstr.t * EConstr.t
  exception BindingInstruction_IndexOutOfBounds of EConstr.t * int
  exception BindingInstruction_NEQ of EConstr.t * Constr.t

  (** [naming_outer x m] is [m], but a [BindingInstruction_Undefined] it
      raises when run names [x] as the outer term. The handler is inside
      the computation, so it runs with [m], not around building it.

      @raise BindingInstruction_Undefined
        when run, as [m] raises it, naming [x] (raised here). Also raises
        whatever else [m] raises (propagated). *)
  let naming_outer (x : EConstr.t) (m : EConstr.t mm) : EConstr.t mm =
    fun st ->
    try m st with
    | BindingInstruction_Undefined (_, y) ->
      raise (BindingInstruction_Undefined (x, y))
  ;;

  (* See the [.mli]. Each [Arg] step names its own term in an [Undefined]
     from deeper in, so the outermost one is what the caller sees. *)
  let rec get_bound_term (x : EConstr.t)
    : Bindings.Instructions.t -> EConstr.t mm
    =
    Logger.trace __FUNCTION__;
    log_econstr ~__FUNCTION__ ~s:"x" x;
    function
    | Undefined -> raise (BindingInstruction_Undefined (x, x))
    | Done -> return x
    | Arg { root; index; cont } ->
      Bindings.Instructions.log ~__FUNCTION__ (Arg { root; index; cont });
      naming_outer
        x
        (let open Syntax in
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
         | _ -> raise (BindingInstruction_NotApp x))
  ;;

  (** [explicit_binding x (name, path)] is the [with] binding of the binder
      [name] to its subterm of [x] at [path] ({!get_bound_term}).

      Raises whatever {!get_bound_term} raises (propagated). *)
  let explicit_binding
        (x : EConstr.t)
        ((name, path) : Bindings.NamedInstructions.t)
    : (Tactypes.quantified_hypothesis * EConstr.t) CAst.t mm
    =
    Logger.trace __FUNCTION__;
    Logger.thing ~__FUNCTION__ Debug "name" name Rocq_utils.Strfy.name;
    Bindings.Instructions.log ~__FUNCTION__ path;
    let open Syntax in
    let q = get_quantified_hyp name in
    let* bs = get_bound_term x path in
    return (CAst.make (q, bs))
  ;;

  (* See the [.mli]. One {!explicit_binding} per binder in [xmap], in
     reverse order. *)
  let get_explicit_bindings
    :  EConstr.t * Bindings.ConstrMap.t' option
    -> EConstr.t Tactypes.explicit_bindings mm
    =
    Logger.trace __FUNCTION__;
    function
    | _, None -> return []
    | x, Some xmap ->
      let open Syntax in
      let ys = Bindings.ConstrMap.to_seq_values xmap |> Array.of_seq in
      iterate
        0
        (Array.length ys - 1)
        []
        (fun i acc ->
          let* b = explicit_binding x ys.(i) in
          return (b :: acc))
  ;;

  (** [add_if_known acc map term] is [acc] with [(t, map)] in front if
      [term] is [Some t], else [acc] unchanged: a step's label or target
      is bound only when it is known. Raises nothing. *)
  let add_if_known
        (acc : (EConstr.t * Bindings.ConstrMap.t' option) list)
        (map : Bindings.ConstrMap.t' option)
    : EConstr.t option -> (EConstr.t * Bindings.ConstrMap.t' option) list
    = function
    | None -> acc
    | Some t -> (t, map) :: acc
  ;;

  (** [binding_key b] is the binder [b] binds, as a comparable value: its
      name, or its index if anonymous. Raises nothing. *)
  let binding_key (b : (Tactypes.quantified_hypothesis * EConstr.t) CAst.t) =
    match fst b.CAst.v with
    | Tactypes.NamedHyp id -> `Named (Names.Id.to_string id.CAst.v)
    | Tactypes.AnonHyp i -> `Anon i
  ;;

  (** [bind_once bs] is [bs] with each binder bound only by its first
      binding, in order. A binder can be reachable from more than one of
      source, label and target; in a consistent proof every occurrence gives
      it the same value, so which is kept does not matter. Quadratic, on
      a constructor's few binders. Raises nothing. *)
  let bind_once (bs : EConstr.t Tactypes.explicit_bindings)
    : EConstr.t Tactypes.explicit_bindings
    =
    List.fold_left
      (fun acc b ->
        if List.exists (fun b' -> binding_key b' = binding_key b) acc
        then acc
        else acc @ [ b ])
      []
      bs
  ;;

  (* See the [.mli]. *)
  let get
        (from' : EConstr.t)
        (action' : EConstr.t option)
        (goto' : EConstr.t option)
    : Bindings.t -> EConstr.t Tactypes.bindings mm
    =
    Logger.trace __FUNCTION__;
    log_econstr ~__FUNCTION__ ~s:"from'" from';
    function
    | No_Bindings -> return Tactypes.NoBindings
    | Use_Bindings { from; action; goto } ->
      let to_iter : (EConstr.t * Bindings.ConstrMap.t' option) list =
        add_if_known (add_if_known [ from', from ] action action') goto goto'
      in
      let open Syntax in
      let* bindings : EConstr.t Tactypes.explicit_bindings =
        let f (i : int) acc =
          Logger.trace __FUNCTION__;
          let* x = get_explicit_bindings (List.nth to_iter i) in
          x :: acc |> return
        in
        let* xs = iterate 0 (List.length to_iter - 1) [] f in
        List.flatten xs |> return
      in
      (match bind_once bindings with
       | [] -> return Tactypes.NoBindings
       | xs -> return (Tactypes.ExplicitBindings xs))
  ;;
end
