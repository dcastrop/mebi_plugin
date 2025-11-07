open Logging
open Mebi_wrapper
open Mebi_wrapper.Syntax

module Pair = struct
  (** [snd] is a term (e.g., destination) that we want to check unifies with [fst] (which we have already reached).
  @see Mebi_setup.unif_problem where [type unif_problem = {termL:EConstr.t;termR:EConstr.t}] *)
  type t = EConstr.t * EConstr.t

  let to_string env sigma ((x, y) : t) : string =
    let f = Strfy.econstr env sigma in
    let g : string * EConstr.t -> string =
      Strfy.tuple ~is_keyval:true Strfy.str f
    in
    Strfy.tuple ~force_newline:true g g (("lhs", x), ("rhs", y))
  ;;

  let debug_unify f a b =
    Log.debug (Printf.sprintf "unified:\n-: %s\n-: %s\n" (f a) (f b))
  ;;

  let debug_unifyerr f a b c d =
    let s1 = Printf.sprintf "cannot unify:\n-: %s\n-: %s" (f a) (f b) in
    let s2 = Printf.sprintf "specifically, \"%s\" with: %s" (f c) (f d) in
    Log.debug (Printf.sprintf "%s\n%s\n" s1 s2)
  ;;

  (** [unify a b] tries to unify [a] and [b] within the context of the [env] and [sigma] of [mm]. @returns [true] if successful, [false] otherwise. *)
  let unify ?(debug : bool = false) ((a, b) : t) : bool mm =
    state (fun (env : Environ.env) (sigma : Evd.evar_map) ->
      let open Pretype_errors in
      try
        let sigma = Unification.w_unify env sigma Conversion.CUMUL a b in
        let f : EConstr.t -> string = Strfy.econstr env sigma in
        if debug then debug_unify f a b;
        sigma, true
      with
      | PretypeError (_, _, CannotUnify (c, d, _e)) ->
        let f : EConstr.t -> string = Strfy.econstr env sigma in
        let () = debug_unifyerr f a b c d in
        sigma, false)
  ;;
end

module Problem = struct
  (** if [fst] is sucessfully unified then [snd] represents a tree of constructors that lead to that term (from some previously visited term).
  *)
  type t = Pair.t * Mebi_constr.Tree.t

  let to_string env sigma ((p, t) : t) : string =
    let f = Pair.to_string env sigma in
    let fs = Strfy.tuple ~is_keyval:true Strfy.str f ("pair", p) in
    let g = Mebi_constr.Tree.to_string in
    let gs = Strfy.tuple ~is_keyval:true Strfy.str g ("tree", t) in
    Strfy.list ~force_newline:true ~use:("{", "}") Strfy.str [ fs; gs ]
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

  let to_string env sigma : t -> string =
    Strfy.list ~force_newline:true (Problem.to_string env sigma)
  ;;

  let rec unify_opt ?(debug : bool = false)
    : t -> Mebi_constr.Tree.t list option mm
    = function
    | [] -> return (Some [])
    | h :: tl ->
      let* success_opt = Problem.unify_opt ~debug h in
      (match success_opt with
       | None -> return None
       | Some constructor_tree ->
         let* unified_opt = unify_opt ~debug tl in
         (match unified_opt with
          | None -> return None
          | Some acc -> return (Some (constructor_tree :: acc))))
  ;;
end

module Constructor = struct
  (* unified_tgt, ctor_tree *)
  type t = EConstr.t * Mebi_constr.Tree.t list

  let sandbox_unify_all_opt
        ?(debug : bool = false)
        (tgt : EConstr.t)
        (problems : Problems.t)
    : t option mm
    =
    sandbox
      (let* unified_opt = Problems.unify_opt ~debug problems in
       match unified_opt with
       | None -> return None
       | Some constructor_trees ->
         let* term = Mebi_utils.econstr_normalize tgt in
         let* is_undefined = Mebi_utils.econstr_is_evar term in
         if is_undefined
         then return None
         else return (Some (term, constructor_trees)))
  ;;
end

module Constructors = struct
  type t = Mebi_constr.t list

  let rec retrieve
            ?(debug : bool = false)
            (constructor_index : int)
            (acc : Mebi_constr.t list)
            (act : EConstr.t)
            (tgt : EConstr.t)
    : Enc.t * Problems.t list -> t mm
    = function
    | _, [] -> return acc
    | lts_enc, problems :: tl ->
      let* success = Constructor.sandbox_unify_all_opt ~debug tgt problems in
      (match success with
       | None -> retrieve ~debug constructor_index acc act tgt (lts_enc, tl)
       | Some (unified_tgt, ctor_trees) ->
         let* unified_tgt = Mebi_utils.econstr_normalize unified_tgt in
         let* act = Mebi_utils.econstr_normalize act in
         let open Mebi_constr.Tree in
         let tree = Node ((lts_enc, constructor_index), ctor_trees) in
         let ctor = act, unified_tgt, tree in
         let acc' = ctor :: acc in
         retrieve ~debug constructor_index acc' act tgt (lts_enc, tl))
  ;;
end
