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

(* module Constructor_arg = struct
  module Fresh = struct
    type t =
      { sigma : Evd.evar_map
      ; evar : EConstr.t
      ; original : EConstr.t
      }

    let get_next (env : Environ.env) (sigma : Evd.evar_map) (x : EConstr.t)
      : Evd.evar_map * t
      =Rocq_utils.get_next env sigma (TypeOf x) in
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
end *)

module Pair = struct
  (** [fst] is a term (e.g., destination) that we want to check unifies with [snd] (which we have already reached).
  @see Mebi_setup.unif_problem where [type unif_problem = {termL:EConstr.t;termR:EConstr.t}] *)
  type t =
    { a : EConstr.t
    ; b : EConstr.t
    }

  let to_string
        env
        sigma
        ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
        ({ a; b } : t)
    : string
    =
    let f = Rocq_utils.Strfy.econstr env sigma in
    let g = Utils.Strfy.tuple ~args Utils.Strfy.str f in
    let a : string = g ("a", a) in
    let b : string = g ("b", b) in
    Utils.Strfy.tuple ~args Utils.Strfy.str Utils.Strfy.str (a, b)
  ;;

  (* let _debug_fresh env sigma sigma' fresh a b : unit =
     let fstr : string = Rocq_utils.Strfy.econstr env sigma' fresh in
     let astr : string = Rocq_utils.Strfy.econstr env sigma a in
     let bstr : string = Rocq_utils.Strfy.econstr env sigma b in
     Logging.Log.debug
     (Printf.sprintf
     "created new fresh a: %s\nto replace a: %s\npaired with b: %s"
     fstr
     astr
     bstr)
     ;; *)

  let fresh env sigma (a : EConstr.t) (b : EConstr.t) : Evd.evar_map * t =
    let sigma, a = Rocq_utils.get_next env sigma (TypeOf a) in
    sigma, { a; b }
  ;;

  let normal (a : EConstr.t) (b : EConstr.t) : t = { a; b }

  let make env sigma (a : EConstr.t) (b : EConstr.t) : Evd.evar_map * t =
    if EConstr.isEvar sigma a then fresh env sigma a b else sigma, normal a b
  ;;

  let debug_unify
        env
        sigma
        ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
        a
        b
    =
    let f = Rocq_utils.Strfy.econstr env sigma in
    let g = Utils.Strfy.tuple ~args Utils.Strfy.str f in
    let a : string = g ("a", a) in
    let b : string = g ("b", b) in
    let s = Utils.Strfy.tuple ~args Utils.Strfy.str Utils.Strfy.str (a, b) in
    Log.debug (Printf.sprintf "unified:\n%s" s)
  ;;

  let debug_unifyerr
        env
        sigma
        ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
        a
        b
        c
        d
    =
    let f = Rocq_utils.Strfy.econstr env sigma in
    let s1 = Printf.sprintf "cannot unify \"%s\" with \"%s\"" (f c) (f d) in
    let g = Utils.Strfy.tuple ~args Utils.Strfy.str f in
    let a : string = g ("a", a) in
    let b : string = g ("b", b) in
    let s2 = Utils.Strfy.tuple ~args Utils.Strfy.str Utils.Strfy.str (a, b) in
    Log.debug (Printf.sprintf "%s:\n%s" s1 s2)
  ;;

  let w_unify
        ?(debug : bool = default_debug)
        env
        sigma
        (a : EConstr.t)
        (b : EConstr.t)
    : Evd.evar_map * bool
    =
    let open Pretype_errors in
    try
      Logging.Log.debug "UW 0: ENTER";
      let sigma = Unification.w_unify env sigma Conversion.CUMUL a b in
      sigma, true
    with
    | PretypeError (_, _, CannotUnify (c, d, _e)) ->
      Logging.Log.debug "UW 0: ERR";
      if debug || debugerr then debug_unifyerr env sigma a b c d;
      sigma, false
  ;;

  (** [unify a b] tries to unify [a] and [b] within the context of the [env] and [sigma] of [mm]. @returns [true] if successful, [false] otherwise. *)
  let unify ?(debug : bool = default_debug) env sigma ({ a; b } : t)
    : Evd.evar_map * bool
    =
    Logging.Log.debug "UF 0: ENTER";
    w_unify ~debug env sigma a b
  ;;
end

module Problem = struct
  (** if [fst] is sucessfully unified then [snd] represents a tree of constructors that lead to that term (from some previously visited term).
  *)
  type t =
    { act : Pair.t
    ; dest : Pair.t
    ; tree : Mebi_constr.Tree.t
    }

  let to_string
        env
        sigma
        ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
        ({ act; dest; tree } : t)
    : string
    =
    let f = Pair.to_string env sigma ~args:(Utils.Strfy.nest args) in
    let act = f act in
    let dest = f dest in
    let tree = Mebi_constr.Tree.to_string ~args:(Utils.Strfy.nest args) tree in
    Utils.Strfy.record ~args [ "act", act; "dest", dest; "tree", tree ]
  ;;

  let unify_pair_opt ?(debug : bool = default_debug) (pair : Pair.t) : bool mm =
    state (fun env sigma -> Pair.unify ~debug env sigma pair)
  ;;

  let unify_opt ?(debug : bool = default_debug)
    : t -> Mebi_constr.Tree.t option mm
    =
    Logging.Log.debug "UO 0: ENTER";
    function
    | { act; dest; tree } ->
      Logging.Log.debug "UO 1: ENTER";
      let* () =
        state (fun env sigma ->
          let s : string = Pair.to_string env sigma act in
          Logging.Log.debug (Printf.sprintf "unify_opt act: %s" s);
          sigma, ())
      in
      let* () =
        state (fun env sigma ->
          let s : string = Pair.to_string env sigma dest in
          Logging.Log.debug (Printf.sprintf "unify_opt dest: %s" s);
          sigma, ())
      in
      let* unified_act_opt = unify_pair_opt ~debug act in
      Logging.Log.debug "UO 2: ENTER";
      let* unified_dest_opt = unify_pair_opt ~debug dest in
      Logging.Log.debug "UO 3: ENTER";
      (match unified_act_opt, unified_dest_opt with
       | true, true -> return (Some tree)
       | _, _ -> return None)
  ;;
end

module Problems = struct
  type t =
    { sigma : Evd.evar_map
    ; to_unify : Problem.t list
    }

  let empty () : t mm =
    let* sigma = get_sigma in
    return { sigma; to_unify = [] }
  ;;

  let is_empty : t -> bool = function
    | { to_unify = []; _ } -> true
    | _ -> false
  ;;

  let list_is_empty : t list -> bool = function
    | [] -> true
    | [ p ] -> is_empty p
    | _ -> false
  ;;

  let to_string env ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
    : t -> string
    = function
    | { sigma; to_unify } ->
      let open Utils.Strfy in
      list
        ~args:{ (nest args) with name = Some "unification problems" }
        (Problem.to_string env sigma)
        to_unify
  ;;

  let list_to_string
        env
        ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
    : t list -> string
    =
    let open Utils.Strfy in
    list
      ~args:{ (nest args) with name = Some "unification problems" }
      (to_string env)
  ;;

  let rec unify_list_opt ?(debug : bool = default_debug)
    : Problem.t list -> Mebi_constr.Tree.t list option mm
    =
    Logging.Log.debug "UP 0: ENTER";
    function
    | [] ->
      Logging.Log.debug (Printf.sprintf "UP 1: RETURN");
      return (Some [])
    | h :: tl ->
      let* success_opt = Problem.unify_opt ~debug h in
      (match success_opt with
       | None ->
         Logging.Log.debug (Printf.sprintf "UP 2.1: NONE");
         return None
       | Some constructor_tree ->
         let* unified_opt = unify_list_opt ~debug tl in
         (match unified_opt with
          | None ->
            Logging.Log.debug (Printf.sprintf "UP 2.2: NONE");
            return None
          | Some acc ->
            Logging.Log.debug (Printf.sprintf "UP 2.3: RETURN");
            return (Some (constructor_tree :: acc))))
  ;;

  (* exception NotApp of unit *)

  (* let _debug_unbox_fresh
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
      | _ ->
        let k : string = Rocq_utils.Strfy.econstr_kind env sigma' tgt in
        Logging.Log.warning
          (Printf.sprintf
             "unbox, NOT APP:\n\
              - tgt: %s\n\
              - evar: %s\n\
              - o1: %s\n\
              - o2: %s\n\
              - s1: %s\n\
              - s2: %s\n\n\
              - kind: %s"
             tgtstr
             evarstr
             o1str
             o2str
             sstr1
             sstr2
             k);
        sigma', ())
  ;; *)

  (* let sandbox_unbox_fresh
        (tgt : EConstr.t)
        ({ sigma; evar; original } : Constructor_arg.Fresh.t)
    : EConstr.t mm
    =
    (* let* () = _debug_unbox_fresh tgt { sigma; evar; original } in *)
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
      | Evar _ -> sigma, evar
      | _ -> raise (NotApp ()))
  ;; *)

  (** iterate through and remove the Fresh.t part of the tuple. For any None we jsut use the [tgt] provided. For any Some x we sandbox unify with the tgt to obtain what the term should be, and then return it with the tree. The fst of the return tupe is just the head of the list of unboxed-fresh [r], but we use a tuple so that we can easily add any that have None.
      (* TODO: redocument this *)

      @param act
        is the action from the outer-scope, and is only here to act as a default return parameter in the case that the list provided is empty -- which will not be the case here since we already checked for this in [Mebi_unify.check_for_next_constructors]. This is just here to provide a base-case return parameter for this recursive function.
      @param tgt
        is similar to act, except that it is actually used. In the case that we have [Some fresh] we determine which one should be returned via [sandbox_unbox_fresh]
  *)
  (* let rec unbox_fresh (act : EConstr.t) (tgt : EConstr.t)
     :  (EConstr.t * Constructor_arg.Fresh.t option * Mebi_constr.Tree.t) list
     -> (EConstr.t * EConstr.t * Mebi_constr.Tree.t list) mm
     = function
     | [] -> return (act, tgt, [])
     | (action, None, tree) :: tl ->
     let* _, unified_tgt, constructor_trees = unbox_fresh act tgt tl in
     let* () = Rocq_debug.debug_econstr_mm "A act" unified_tgt in
     let* () = Rocq_debug.debug_econstr_mm "A tgt" unified_tgt in
     return (action, unified_tgt, tree :: constructor_trees)
     | (action, Some fresh, tree) :: tl ->
     let* _, unified_tgt, constructor_trees = unbox_fresh act tgt tl in
     let* () = Rocq_debug.debug_econstr_mm "B1 act" action in
     let* () = Rocq_debug.debug_econstr_mm "B1 tgt" unified_tgt in
     let* unified_tgt = sandbox_unbox_fresh unified_tgt fresh in
     let* () = Rocq_debug.debug_econstr_mm "B2 tgt" unified_tgt in
     return (action, unified_tgt, tree :: constructor_trees)
     ;; *)

  let sandbox_unify_all_opt
        ?(debug : bool = default_debug)
        (act : EConstr.t)
        (dest : EConstr.t)
    : t -> (EConstr.t * EConstr.t * Mebi_constr.Tree.t list) option mm
    =
    Logging.Log.debug "SB 0: ENTER";
    function
    | { sigma = psigma; to_unify } ->
      Logging.Log.debug "SB 0.1: ENTER";
      let* () =
        state (fun env sigma ->
          let s : string = Rocq_utils.Strfy.econstr env sigma act in
          Logging.Log.debug (Printf.sprintf "sandbox act: %s" s);
          sigma, ())
      in
      let* () =
        state (fun env sigma ->
          let s : string = Rocq_utils.Strfy.econstr env sigma dest in
          Logging.Log.debug (Printf.sprintf "sandbox dest: %s" s);
          sigma, ())
      in
      sandbox
        ~using:psigma
        (Logging.Log.debug "SB 0.2: ENTER";
         let* unified_opt = unify_list_opt ~debug to_unify in
         Logging.Log.debug "SB 0.3: ENTER";
         match unified_opt with
         | None ->
           Logging.Log.debug
             (Printf.sprintf "SB 1: NONE %i" (List.length to_unify));
           return None
         | Some constructor_trees ->
           (* Logging.Log.debug "unbox:"; *)
           (* let* action, unified_term, constructor_trees =
              unbox_fresh act tgt fresh_opt_and_constructor_trees
              in *)
           let$+ act env sigma = Reductionops.nf_all env sigma act in
           let$+ dest env sigma = Reductionops.nf_all env sigma dest in
           let* is_act_undefined = Mebi_utils.econstr_is_evar act in
           let* is_dest_undefined = Mebi_utils.econstr_is_evar dest in
           if is_act_undefined && is_dest_undefined
           then (
             Logging.Log.debug
               (Printf.sprintf "SB 2.1: NONE %i" (List.length to_unify));
             return None)
           else (
             Logging.Log.debug
               (Printf.sprintf "SB 2.2: RETURN %i" (List.length to_unify));
             return (Some (act, dest, constructor_trees))))
  ;;
end

module Constructors = struct
  type t = Mebi_constr.t list

  let to_string
        env
        sigma
        ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
    : t -> string
    =
    Utils.Strfy.list ~args (Mebi_constr.to_string env sigma)
  ;;

  (* unified_tgt, ctor_tree *)
  (* type r = EConstr.t * EConstr.t * Mebi_constr.Tree.t list *)
  (* type r = Mebi_constr.t *)

  let rec retrieve
            ?(debug : bool = default_debug)
            (constructor_index : int)
            (acc : t)
            (act : EConstr.t)
            (tgt : EConstr.t)
    : Enc.t * Problems.t list -> t mm
    =
    Logging.Log.debug "RT 0: ENTER";
    function
    | _, [] ->
      Logging.Log.debug (Printf.sprintf "RT 1: RETURN %i" (List.length acc));
      return acc
    | lts_enc, problems :: tl ->
      Logging.Log.debug (Printf.sprintf "RT 2: ENTER %i" (List.length acc));
      let* success = Problems.sandbox_unify_all_opt ~debug act tgt problems in
      (match success with
       | None ->
         Logging.Log.debug (Printf.sprintf "RT 2.1: NONE %i" (List.length acc));
         retrieve ~debug constructor_index acc act tgt (lts_enc, tl)
       | Some (act, dest, constructor_trees) ->
         (* let* unified_act = Mebi_utils.econstr_normalize act in *)
         (* let* unified_tgt = Mebi_utils.econstr_normalize tgt in *)
         (* let* unified_tgt = Mebi_utils.econstr_normalize unified_tgt in *)
         (* let* act = Mebi_utils.econstr_normalize act in *)
         let open Mebi_constr.Tree in
         let tree = Node ((lts_enc, constructor_index), constructor_trees) in
         let constructor = act, dest, tree in
         let acc = constructor :: acc in
         Logging.Log.debug (Printf.sprintf "RT 2.2: SOME %i" (List.length acc));
         retrieve ~debug constructor_index acc act tgt (lts_enc, tl))
  ;;
end
