open Logging
open Mebi_wrapper
open Mebi_wrapper.Syntax

let default_debug : bool = true
let debugerr : bool = true

type constructor_args =
  { lhs : EConstr.t
  ; act : EConstr.t
  ; rhs : EConstr.t
  }

module Constructor_arg = struct
  type t =
    | Normal of EConstr.t
    | Fresh of fresh

  and fresh =
    { sigma : Evd.evar_map
    ; term : EConstr.t
    ; original : EConstr.t
    }

  let to_string env sigma' : t -> string = function
    | Normal x -> Printf.sprintf "(Normal: %s)" (Strfy.econstr env sigma' x)
    | Fresh { sigma; term; original } ->
      Printf.sprintf
        "(Fresh: %s) (Original: %s)"
        (Strfy.econstr env sigma term)
        (Strfy.econstr env sigma' original)
  ;;
end

module Pair = struct
  (** [snd] is a term (e.g., destination) that we want to check unifies with [fst] (which we have already reached).
  @see Mebi_setup.unif_problem where [type unif_problem = {termL:EConstr.t;termR:EConstr.t}] *)
  type t =
    { a : Constructor_arg.t
    ; b : EConstr.t
    }

  let normal (a : EConstr.t) (b : EConstr.t) : t = { a = Normal a; b }

  let _debug_fresh env sigma sigma' fresh a b : unit =
    let fstr : string = Strfy.econstr env sigma' fresh in
    let astr : string = Strfy.econstr env sigma' a in
    let bstr : string = Strfy.econstr env sigma' b in
    Logging.Log.debug
      (Printf.sprintf
         "created new fresh a: %s\nto replace a: %s\npaired with b: %s"
         fstr
         astr
         bstr)
  ;;

  let fresh env sigma (original : EConstr.t) (b : EConstr.t) : t =
    let sigma', fresh = Evarutil.new_evar env sigma original in
    (* let () = _debug_fresh env sigma sigma' fresh original b in *)
    let a : Constructor_arg.t =
      Fresh { sigma = sigma'; term = fresh; original }
    in
    { a; b }
  ;;

  let to_string ?(indent : int = 0) env sigma ({ a; b } : t) : string =
    let f' = Constructor_arg.to_string env sigma in
    let f = Strfy.tuple ~is_keyval:true ~indent Strfy.str f' in
    let g' = Strfy.econstr env sigma in
    let g = Strfy.tuple ~is_keyval:true ~indent Strfy.str g' in
    Strfy.tuple ~force_newline:true ~indent f g (("a", a), ("b", b))
  ;;

  let debug_unify env sigma (x : t) =
    Log.debug (Printf.sprintf "unified:\n%s" (to_string env sigma x))
  ;;

  let debug_unifyerr env sigma (x : t) c d =
    let s1 = Printf.sprintf "cannot unify:\n%s" (to_string env sigma x) in
    let f = Strfy.econstr env sigma in
    let s2 = Printf.sprintf "specifically, \"%s\" with: %s" (f c) (f d) in
    Log.debug (Printf.sprintf "%s\n%s\n" s1 s2)
  ;;

  let w_unify env sigma ({ a; b } : t) : Evd.evar_map =
    match a with
    | Normal a' -> Unification.w_unify env sigma Conversion.CUMUL a' b
    | Fresh { sigma; term; _ } ->
      Unification.w_unify env sigma Conversion.CUMUL term b
  ;;

  (** [unify a b] tries to unify [a] and [b] within the context of the [env] and [sigma] of [mm]. @returns [true] if successful, [false] otherwise. *)
  let unify ?(debug : bool = default_debug) ({ a; b } : t) : bool mm =
    state (fun (env : Environ.env) (sigma : Evd.evar_map) ->
      let open Pretype_errors in
      try
        let sigma : Evd.evar_map = w_unify env sigma { a; b } in
        if debug then debug_unify env sigma { a; b };
        sigma, true
      with
      | PretypeError (_, _, CannotUnify (c, d, _e)) ->
        if debugerr then debug_unifyerr env sigma { a; b } c d;
        sigma, false)
  ;;
end

module Problem = struct
  (** if [fst] is sucessfully unified then [snd] represents a tree of constructors that lead to that term (from some previously visited term).
  *)
  type t = Pair.t * Mebi_constr.Tree.t

  let to_string ?(indent : int = 0) env sigma ((p, t) : t) : string =
    let f = Pair.to_string ~indent:(indent + 4) env sigma in
    let fs =
      Strfy.tuple ~is_keyval:true ~indent:(indent + 3) Strfy.str f ("pair", p)
    in
    let g = Mebi_constr.Tree.to_string in
    let gs =
      Strfy.tuple ~is_keyval:true ~indent:(indent + 3) Strfy.str g ("tree", t)
    in
    Strfy.list ~force_newline:true ~use:("{", "}") ~indent Strfy.str [ fs; gs ]
  ;;

  let unify_opt ?(debug : bool = false) : t -> Mebi_constr.Tree.t option mm
    = function
    | unification_problem, constructor_tree ->
      let* success = Pair.unify ~debug unification_problem in
      if success then return (Some constructor_tree) else return None
  ;;
end

module Problems = struct
  type t = Problem.t list

  let to_string ?(indent : int = 0) env sigma : t -> string =
    Strfy.list
      ~force_newline:true
      ~indent
      (Problem.to_string ~indent:(indent + 1) env sigma)
  ;;

  let list_to_string ?(indent : int = 0) env sigma : t list -> string =
    Strfy.list
      ~force_newline:true
      ~indent
      (to_string ~indent:(indent + 1) env sigma)
  ;;

  let rec unify_opt ?(debug : bool = false)
    : t -> Mebi_constr.Tree.t list option mm
    = function
    | [] -> return (Some [])
    | h :: tl ->
      let* success_opt = Problem.unify_opt ~debug h in
      (match success_opt with
       | None ->
         Logging.Log.debug (Printf.sprintf "UP1");
         return None
       | Some constructor_tree ->
         let* unified_opt = unify_opt ~debug tl in
         (match unified_opt with
          | None ->
            Logging.Log.debug (Printf.sprintf "UP2");
            return None
          | Some acc ->
            Logging.Log.debug (Printf.sprintf "UP3");
            return (Some (constructor_tree :: acc))))
  ;;
end

module Constructors = struct
  type t = Mebi_constr.t list

  (* unified_tgt, ctor_tree *)
  type r = EConstr.t * Mebi_constr.Tree.t list

  let sandbox_unify_all_opt
        ?(debug : bool = false)
        (tgt : EConstr.t)
        (problems : Problems.t)
    : r option mm
    =
    sandbox
      (let* unified_opt = Problems.unify_opt ~debug problems in
       match unified_opt with
       | None ->
         Logging.Log.debug (Printf.sprintf "S1: %i" (List.length problems));
         return None
       | Some constructor_trees ->
         Logging.Log.debug (Printf.sprintf "S2: %i" (List.length problems));
         let* term = Mebi_utils.econstr_normalize tgt in
         let* is_undefined = Mebi_utils.econstr_is_evar term in
         if is_undefined
         then return None
         else return (Some (term, constructor_trees)))
  ;;

  let rec retrieve
            ?(debug : bool = true)
            (constructor_index : int)
            (acc : Mebi_constr.t list)
            (act : EConstr.t)
            (tgt : EConstr.t)
    : Enc.t * Problems.t list -> t mm
    = function
    | _, [] ->
      Logging.Log.debug (Printf.sprintf "R0: %i" (List.length acc));
      return acc
    | lts_enc, problems :: tl ->
      let* success = sandbox_unify_all_opt ~debug tgt problems in
      (match success with
       | None ->
         Logging.Log.debug (Printf.sprintf "R1: %i" (List.length acc));
         retrieve ~debug constructor_index acc act tgt (lts_enc, tl)
       | Some (unified_tgt, ctor_trees) ->
         let* unified_tgt = Mebi_utils.econstr_normalize unified_tgt in
         let* act = Mebi_utils.econstr_normalize act in
         let open Mebi_constr.Tree in
         let tree = Node ((lts_enc, constructor_index), ctor_trees) in
         let ctor = act, unified_tgt, tree in
         let acc' = ctor :: acc in
         Logging.Log.debug (Printf.sprintf "R2: %i" (List.length acc'));
         retrieve ~debug constructor_index acc' act tgt (lts_enc, tl))
  ;;
end
