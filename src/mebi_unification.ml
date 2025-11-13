(* TODO: rename to [unifification_structs] since the mebi_wrapper isn't used here *)

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
  module Fresh = struct
    type t =
      { sigma : Evd.evar_map
      ; evar : EConstr.t
      ; original : EConstr.t
      }

    exception CouldNotGetNextFreshEvarName of unit

    let get_next (env : Environ.env) (sigma : Evd.evar_map) (x : EConstr.t)
      : Evd.evar_map * t
      =
      let sigma', evar = Rocq_utils.get_next env sigma x in
      let a : t = { sigma = sigma'; evar; original = x } in
      sigma, a
    ;;
  end

  type t =
    | Normal of EConstr.t
    | Fresh of Fresh.t

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
    let astr : string = Rocq_utils.Strfy.econstr env sigma a in
    let bstr : string = Rocq_utils.Strfy.econstr env sigma b in
    Logging.Log.debug
      (Printf.sprintf
         "created new fresh a: %s\nto replace a: %s\npaired with b: %s"
         fstr
         astr
         bstr)
  ;;

  let fresh env sigma (a : EConstr.t) (b : EConstr.t) : Evd.evar_map * t =
    let sigma, a = Constructor_arg.Fresh.get_next env sigma a in
    sigma, { a = Fresh a; b }
  ;;

  let normal (a : EConstr.t) (b : EConstr.t) : t = { a = Normal a; b }

  let make env sigma (a : EConstr.t) (b : EConstr.t) : Evd.evar_map * t =
    if EConstr.isEvar sigma a then fresh env sigma a b else sigma, normal a b
  ;;

  let debug_unify env sigma (x : t) =
    Log.debug (Printf.sprintf "unified:\n%s" (to_string env sigma x))
  ;;

  let debug_unifyerr env sigma (x : t) c d =
    let f = Rocq_utils.Strfy.econstr env sigma in
    let s1 = Printf.sprintf "cannot unify \"%s\" with \"%s\"" (f c) (f d) in
    Log.debug (Printf.sprintf "%s:\n%s" s1 (to_string env sigma x))
  ;;

  (** [unify a b] tries to unify [a] and [b] within the context of the [env] and [sigma] of [mm]. @returns [true] if successful, [false] otherwise. *)
  let unify ?(debug : bool = default_debug) env sigma' ({ a; b } : t)
    : Evd.evar_map * Constructor_arg.Fresh.t option * bool
    =
    let open Pretype_errors in
    try
      match a with
      | Normal a ->
        let sigma = Unification.w_unify env sigma' Conversion.CUMUL a b in
        if debug then debug_unify env sigma { a = Normal a; b };
        sigma, None, true
      | Fresh { sigma; evar; original } ->
        let sigma = Unification.w_unify env sigma Conversion.CUMUL evar b in
        let a : Constructor_arg.Fresh.t = { sigma; evar; original } in
        if debug then debug_unify env sigma { a = Fresh a; b };
        sigma', Some a, true
    with
    | PretypeError (_, _, CannotUnify (c, d, _e)) ->
      if debug || debugerr then debug_unifyerr env sigma' { a; b } c d;
      sigma', None, false
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

  let unify_opt ?(debug : bool = default_debug)
    : t -> (Constructor_arg.Fresh.t option * Mebi_constr.Tree.t) option mm
    = function
    | unification_problem, constructor_tree ->
      state (fun env sigma ->
        match Pair.unify ~debug env sigma unification_problem with
        | sigma, _, false -> sigma, None
        | sigma, None, true -> sigma, Some (None, constructor_tree)
        | sigma, Some fresh, true -> sigma, Some (Some fresh, constructor_tree))
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

  let append_fresh_opt (fresh : Constructor_arg.Fresh.t list)
    : Constructor_arg.Fresh.t option -> Constructor_arg.Fresh.t list
    = function
    | None -> fresh
    | Some a -> a :: fresh
  ;;

  let rec unify_opt ?(debug : bool = default_debug)
    : t -> (Constructor_arg.Fresh.t option * Mebi_constr.Tree.t) list option mm
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
       | Some (fresh_opt, constructor_tree) ->
         let* unified_opt = unify_opt ~debug tl in
         (match unified_opt with
          | None ->
            Logging.Log.debug (Printf.sprintf "UP2: NONE");
            return None
          | Some acc ->
            Logging.Log.debug (Printf.sprintf "UP3: RETURN");
            return (Some ((fresh_opt, constructor_tree) :: acc))))
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

  exception NotApp of unit

  let _debug_unbox_fresh
        (tgt : EConstr.t)
        ({ sigma; evar; original } : Constructor_arg.Fresh.t)
    : unit mm
    =
    state (fun env sigma' ->
      let tgtstr : string = Rocq_utils.Strfy.econstr env sigma' tgt in
      let evarstr : string = Rocq_utils.Strfy.econstr env sigma evar in
      let o1str : string = Rocq_utils.Strfy.econstr env sigma' original in
      let o2str : string = Rocq_utils.Strfy.econstr env sigma original in
      let s : EConstr.t = EConstr.Vars.substl [ original; evar ] tgt in
      let sstr1 : string = Rocq_utils.Strfy.econstr env sigma' s in
      let sstr2 : string = Rocq_utils.Strfy.econstr env sigma s in
      match EConstr.kind sigma' tgt with
      | App (name, args) ->
        let tgt' : EConstr.t =
          EConstr.mkApp
            ( name
            , Array.map
                (fun x ->
                  if Mebi_setup.Eq.econstr sigma' x original then evar else x)
                args )
        in
        let tgt'str1 : string = Rocq_utils.Strfy.econstr env sigma' tgt' in
        let tgt'str2 : string = Rocq_utils.Strfy.econstr env sigma tgt' in
        Logging.Log.debug
          (Printf.sprintf
             "unbox:\n\
              - tgt: %s\n\
              - evar: %s\n\
              - o1: %s\n\
              - o2: %s\n\
              - s1: %s\n\
              - s2: %s\n\
              - t1: %s\n\
              - t2: %s\n"
             tgtstr
             evarstr
             o1str
             o2str
             sstr1
             sstr2
             tgt'str1
             tgt'str2);
        sigma', ()
      | _ -> raise (NotApp ()))
  ;;

  let sandbox_unbox_fresh
        (tgt : EConstr.t)
        ({ sigma; evar; original } : Constructor_arg.Fresh.t)
    : EConstr.t mm
    =
    (* let* () = debug_unbox_fresh tgt { sigma; evar; original } in *)
    (* return evar *)
    (* TODO: is it as simple as jsut returning the evar sigma? *)
    state (fun env sigma' ->
      match EConstr.kind sigma' tgt with
      | App (name, args) ->
        let tgt' : EConstr.t =
          EConstr.mkApp
            ( name
            , Array.map
                (fun x ->
                  if Mebi_setup.Eq.econstr sigma' x original then evar else x)
                args )
        in
        (* sigma, evar *)
        sigma, tgt'
      | _ -> raise (NotApp ()))
  ;;

  (** iterate through and remove the Fresh.t part of the tuple. For any None we jsut use the [tgt] provided. For any Some x we sandbox unify with the tgt to obtain what the term should be, and then return it with the tree. The fst of the return tupe is just the head of the list of unboxed-fresh [r], but we use a tuple so that we can easily add any that have None.
  *)
  let rec unbox_fresh (tgt : EConstr.t)
    : (Constructor_arg.Fresh.t option * Mebi_constr.Tree.t) list -> r mm
    = function
    | [] -> return (tgt, [])
    | (None, tree) :: tl ->
      let* unified_tgt, constructor_trees = unbox_fresh tgt tl in
      let* () = Rocq_debug.debug_econstr_mm "A" unified_tgt in
      return (unified_tgt, tree :: constructor_trees)
    | (Some fresh, tree) :: tl ->
      let* unified_tgt, constructor_trees = unbox_fresh tgt tl in
      let* () = Rocq_debug.debug_econstr_mm "B1" unified_tgt in
      let* unified_tgt = sandbox_unbox_fresh unified_tgt fresh in
      let* () = Rocq_debug.debug_econstr_mm "B2" unified_tgt in
      return (unified_tgt, tree :: constructor_trees)
  ;;

  let sandbox_unify_all_opt
        ?(debug : bool = default_debug)
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
       | Some fresh_opt_and_constructor_trees ->
         Logging.Log.debug "unbox:";
         let* unified_term, constructor_trees =
           unbox_fresh tgt fresh_opt_and_constructor_trees
         in
         let* is_undefined = Mebi_utils.econstr_is_evar unified_term in
         if is_undefined
         then (
           Logging.Log.debug
             (Printf.sprintf "S2: NONE %i" (List.length problems));
           return None)
         else (
           Logging.Log.debug
             (Printf.sprintf "S3: RETURN %i" (List.length problems));
           return (Some (unified_term, constructor_trees))))
  ;;

  let rec retrieve
            ?(debug : bool = default_debug)
            (constructor_index : int)
            (acc : t)
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
       | Some (unified_tgt, constructor_trees) ->
         let* unified_tgt = Mebi_utils.econstr_normalize unified_tgt in
         let* act = Mebi_utils.econstr_normalize act in
         (* let acc =
            List.map
            (fun (unified_tgt, trees) ->
            ( act
            , unified_tgt
            , Mebi_constr.Tree.Node ((lts_enc, constructor_index), trees) ))
            constructor_pairs
            in *)
         let open Mebi_constr.Tree in
         let tree = Node ((lts_enc, constructor_index), constructor_trees) in
         let constructor = act, unified_tgt, tree in
         let acc = constructor :: acc in
         Logging.Log.debug (Printf.sprintf "R2: SOME %i" (List.length acc));
         retrieve ~debug constructor_index acc act tgt (lts_enc, tl))
  ;;
end
