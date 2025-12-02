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
    let g = Utils.Strfy.tuple ~args Utils.Strfy.string f in
    let a : string = g ("a", a) in
    let b : string = g ("b", b) in
    Utils.Strfy.tuple ~args Utils.Strfy.string Utils.Strfy.string (a, b)
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
    let g = Utils.Strfy.tuple ~args Utils.Strfy.string f in
    let a : string = g ("a", a) in
    let b : string = g ("b", b) in
    let s =
      Utils.Strfy.tuple ~args Utils.Strfy.string Utils.Strfy.string (a, b)
    in
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
    let g = Utils.Strfy.tuple ~args Utils.Strfy.string f in
    let a : string = g ("a", a) in
    let b : string = g ("b", b) in
    let s2 =
      Utils.Strfy.tuple ~args Utils.Strfy.string Utils.Strfy.string (a, b)
    in
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
    let open Utils.Strfy in
    let args : style_args =
      { args with name = None; style = Some (tuple_style ()) }
    in
    let f = Pair.to_string env sigma ~args:(nest args) in
    let act = f act in
    let dest = f dest in
    let tree = Mebi_constr.Tree.to_string ~args:(nest args) tree in
    let args : style_args = { args with style = Some (record_style ()) } in
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
        ~args:
          { (nest args) with
            style = Some (list_style ())
          ; name = Some "unification problems"
          }
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
      ~args:
        { (nest args) with
          style = Some (list_style ())
        ; name = Some "unification problems list"
        }
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
           Logging.Log.debug
             (Printf.sprintf
                "SB 2.0.1:\nact: %s\ndest: %s"
                (econstr_to_string act)
                (econstr_to_string dest));
           let$+ act env sigma = Reductionops.nf_all env sigma act in
           let$+ dest env sigma = Reductionops.nf_all env sigma dest in
           (* let* env = get_env in
              let act = Reductionops.nf_all env psigma act in
              let dest = Reductionops.nf_all env psigma dest in *)
           Logging.Log.debug
             (Printf.sprintf
                "SB 2.0.2:\nact: %s\ndest: %s"
                (econstr_to_string act)
                (econstr_to_string dest));
           let$+ is_act_undefined _ sigma = EConstr.isEvar sigma act in
           let$+ is_dest_undefined _ sigma = EConstr.isEvar sigma dest in
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
      let* acc = retrieve ~debug constructor_index acc act tgt (lts_enc, tl) in
      let* constructor_opt : Mebi_constr.t option =
        sandbox
          (let* success =
             Problems.sandbox_unify_all_opt ~debug act tgt problems
           in
           match success with
           | None ->
             (* Logging.Log.debug
               (Printf.sprintf "RT 2.1: NONE %i" (List.length acc)); *)
             (* retrieve ~debug constructor_index acc act tgt (lts_enc, tl) *)
             return None
           | Some (act, dest, constructor_trees) ->
             let open Mebi_constr.Tree in
             let tree =
               Node ((lts_enc, constructor_index), constructor_trees)
             in
             let constructor = act, dest, tree in
             return (Some constructor))
      in
      (match constructor_opt with
       | None ->
         Logging.Log.debug (Printf.sprintf "RT 2.1: NONE %i" (List.length acc));
         (* retrieve ~debug constructor_index acc act tgt (lts_enc, tl) *)
         return acc
       | Some constructor ->
         let act, dest, tree = constructor in
         let acc = constructor :: acc in
         Logging.Log.debug (Printf.sprintf "RT 2.2: SOME %i" (List.length acc));
         Logging.Log.debug
           (Printf.sprintf
              "RT 2.3:\nact: %s\ndest: %s\ntree: %s"
              (econstr_to_string act)
              (econstr_to_string dest)
              (Mebi_constr.Tree.to_string tree));
         (* retrieve ~debug constructor_index acc act tgt (lts_enc, tl) *)
         return acc)
  ;;
end
