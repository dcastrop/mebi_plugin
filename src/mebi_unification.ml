(* TODO: rename to [unifification_structs] since the mebi_wrapper isn't used here *)

open Logging
open Mebi_wrapper
open Mebi_wrapper.Syntax

let default_debug : bool = true
let debugerr : bool = false

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
    ; evar : EConstr.t
    ; original : EConstr.t
    }

  let to_string env sigma' : t -> string = function
    | Normal x ->
      Printf.sprintf "(Normal: %s)" (Rocq_utils.Strfy.econstr env sigma' x)
    | Fresh { sigma; evar; original } ->
      Printf.sprintf
        "(Fresh: %s) (Original: %s)"
        (Rocq_utils.Strfy.econstr env sigma evar)
        (Rocq_utils.Strfy.econstr env sigma' original)
  ;;
end

module Pair = struct
  (** [fst] is a term (e.g., destination) that we want to check unifies with [snd] (which we have already reached).
  @see Mebi_setup.unif_problem where [type unif_problem = {termL:EConstr.t;termR:EConstr.t}] *)
  type t =
    { a : Constructor_arg.t
    ; b : EConstr.t
    }

  let to_string ?(indent : int = 0) env sigma ({ a; b } : t) : string =
    let f' = Constructor_arg.to_string env sigma in
    let f = Utils.Strfy.tuple ~is_keyval:true ~indent Utils.Strfy.str f' in
    let g' = Rocq_utils.Strfy.econstr env sigma in
    let g = Utils.Strfy.tuple ~is_keyval:true ~indent Utils.Strfy.str g' in
    Utils.Strfy.tuple ~force_newline:true ~indent f g (("a", a), ("b", b))
  ;;

  let _debug_fresh env sigma sigma' fresh a b : unit =
    let fstr : string = Rocq_utils.Strfy.econstr env sigma' fresh in
    let astr : string = Rocq_utils.Strfy.econstr env sigma' a in
    let bstr : string = Rocq_utils.Strfy.econstr env sigma' b in
    Logging.Log.debug
      (Printf.sprintf
         "created new fresh a: %s\nto replace a: %s\npaired with b: %s"
         fstr
         astr
         bstr)
  ;;

  let fresh env sigma' (a : EConstr.t) (b : EConstr.t) : t =
    let sigma, evar = Evarutil.new_evar env sigma' a in
    (* let () = _debug_fresh env sigma sigma' fresh original b in *)
    let a : Constructor_arg.t = Fresh { sigma; evar; original = a } in
    { a; b }
  ;;

  let normal (a : EConstr.t) (b : EConstr.t) : t = { a = Normal a; b }

  let make env sigma (a : EConstr.t) (b : EConstr.t) : t =
    if EConstr.isEvar sigma a then fresh env sigma a b else normal a b
  ;;

  let debug_unify env sigma (x : t) =
    Log.debug (Printf.sprintf "unified:\n%s" (to_string env sigma x))
  ;;

  let debug_unifyerr env sigma (x : t) c d =
    let f = Rocq_utils.Strfy.econstr env sigma in
    let s1 = Printf.sprintf "cannot unify \"%s\" with \"%s\"" (f c) (f d) in
    Log.debug (Printf.sprintf "%s:\n%s" s1 (to_string env sigma x))
  ;;

  let w_unify env sigma' ({ a; b } : t) : Evd.evar_map =
    match a with
    | Normal a' -> Unification.w_unify env sigma' Conversion.CUMUL a' b
    | Fresh { sigma; evar; original } ->
      let f =
        (* Unification.w_unify env sigma' Conversion.CUMUL original *)
        Unification.w_unify env sigma Conversion.CUMUL evar
      in
      f b
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
        if debug || debugerr then debug_unifyerr env sigma { a; b } c d;
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
      Utils.Strfy.tuple
        ~is_keyval:true
        ~indent:(indent + 3)
        Utils.Strfy.str
        f
        ("pair", p)
    in
    let g = Mebi_constr.Tree.to_string in
    let gs =
      Utils.Strfy.tuple
        ~is_keyval:true
        ~indent:(indent + 3)
        Utils.Strfy.str
        g
        ("tree", t)
    in
    Utils.Strfy.list
      ~force_newline:true
      ~use:("{", "}")
      ~indent
      Utils.Strfy.str
      [ fs; gs ]
  ;;

  let unify_opt ?(debug : bool = false) : t -> Mebi_constr.Tree.t option mm
    = function
    | unification_problem, constructor_tree ->
      let* success = Pair.unify ~debug unification_problem in
      if success
      then (
        Logging.Log.debug (Printf.sprintf "PR0: SOME");
        return (Some constructor_tree))
      else (
        Logging.Log.debug (Printf.sprintf "PR1: NONE");
        return None)
  ;;
end

module Problems = struct
  type t = Problem.t list

  let to_string ?(indent : int = 0) env sigma : t -> string =
    Utils.Strfy.list
      ~force_newline:true
      ~indent
      ~label:"unification problems"
      (Problem.to_string ~indent:(indent + 1) env sigma)
  ;;

  let list_to_string ?(indent : int = 0) env sigma : t list -> string =
    Utils.Strfy.list
      ~force_newline:true
      ~indent
      ~label:"list of unification problems"
      (to_string ~indent:(indent + 1) env sigma)
  ;;

  let rec unify_opt ?(debug : bool = false)
    : t -> Mebi_constr.Tree.t list option mm
    = function
    | [] ->
      Logging.Log.debug (Printf.sprintf "UP0: RETURN");
      return (Some [])
    | h :: tl ->
      let* success_opt = Problem.unify_opt ~debug h in
      (match success_opt with
       | None ->
         Logging.Log.debug (Printf.sprintf "UP1: NONE");
         return None
       | Some constructor_tree ->
         let* unified_opt = unify_opt ~debug tl in
         (match unified_opt with
          | None ->
            Logging.Log.debug (Printf.sprintf "UP2: NONE");
            return None
          | Some acc ->
            Logging.Log.debug (Printf.sprintf "UP3: RETURN");
            return (Some (constructor_tree :: acc))))
  ;;
end

module Constructors = struct
  type t = Mebi_constr.t list

  let to_string ?(indent : int = 0) env sigma : t -> string =
    Utils.Strfy.list
      ~force_newline:true
      ~label:"constructors"
      ~indent
      (Mebi_constr.to_string ~indent:(indent + 1) env sigma)
  ;;

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
         Logging.Log.debug (Printf.sprintf "S1: NONE %i" (List.length problems));
         return None
       | Some constructor_trees ->
         let* term = Mebi_utils.econstr_normalize tgt in
         let* is_undefined = Mebi_utils.econstr_is_evar term in
         if is_undefined
         then (
           Logging.Log.debug
             (Printf.sprintf "S2: NONE %i" (List.length problems));
           return None)
         else (
           Logging.Log.debug
             (Printf.sprintf "S3: RETURN %i" (List.length problems));
           return (Some (term, constructor_trees))))
  ;;

  let rec retrieve
            ?(debug : bool = false)
            (constructor_index : int)
            (acc : Mebi_constr.t list)
            (act : EConstr.t)
            (tgt : EConstr.t)
    : Enc.t * Problems.t list -> t mm
    = function
    | _, [] ->
      Logging.Log.debug (Printf.sprintf "R0: RETURN %i" (List.length acc));
      return acc
    | lts_enc, problems :: tl ->
      let* success = sandbox_unify_all_opt ~debug tgt problems in
      (match success with
       | None ->
         Logging.Log.debug (Printf.sprintf "R1: NONE %i" (List.length acc));
         retrieve ~debug constructor_index acc act tgt (lts_enc, tl)
       | Some (unified_tgt, ctor_trees) ->
         let* unified_tgt = Mebi_utils.econstr_normalize unified_tgt in
         let* act = Mebi_utils.econstr_normalize act in
         let open Mebi_constr.Tree in
         let tree = Node ((lts_enc, constructor_index), ctor_trees) in
         let ctor = act, unified_tgt, tree in
         let acc' = ctor :: acc in
         Logging.Log.debug (Printf.sprintf "R2: SOME %i" (List.length acc'));
         retrieve ~debug constructor_index acc' act tgt (lts_enc, tl))
  ;;
end
