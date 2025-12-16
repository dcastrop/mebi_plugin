(* TODO: rename to [unifification_structs] since the mebi_wrapper isn't used here *)

(* open Logging *)
open Mebi_wrapper
open Mebi_wrapper.Syntax
module Tree = Mebi_constr.Tree

module UnifLog : Logger.LOGGER_TYPE =
  Logger.Make
    (Logger.Output.Rocq)
    (struct
      let prefix : string option = None

      let is_level_enabled : Logger.level -> bool =
        Logger.make_level_fun ~debug:false ~info:false ~error:false ()
      ;;
    end)

let debug_econstr
      ?(__FUNCTION__ : string = "")
      (prefix : string)
      (x : EConstr.t)
  : unit mm
  =
  state (fun env sigma ->
    UnifLog.thing
      ~__FUNCTION__
      Debug
      prefix
      x
      (Args (Rocq_utils.Strfy.econstr env sigma));
    sigma, ())
;;

(***********************************************************************)

type constructor_args =
  { lhs : EConstr.t
  ; act : EConstr.t
  ; rhs : EConstr.t
  }

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
    UnifLog.trace __FUNCTION__;
    let f = Rocq_utils.Strfy.econstr env sigma in
    let g = Utils.Strfy.tuple ~args Utils.Strfy.string f in
    let a : string = g ("a", a) in
    let b : string = g ("b", b) in
    Utils.Strfy.tuple ~args Utils.Strfy.string Utils.Strfy.string (a, b)
  ;;

  let fresh env sigma (a : EConstr.t) (b : EConstr.t) : Evd.evar_map * t =
    UnifLog.trace __FUNCTION__;
    let sigma, a = Rocq_utils.get_next env sigma (TypeOf a) in
    sigma, { a; b }
  ;;

  let normal (a : EConstr.t) (b : EConstr.t) : t =
    UnifLog.trace __FUNCTION__;
    { a; b }
  ;;

  let make env sigma (a : EConstr.t) (b : EConstr.t) : Evd.evar_map * t =
    UnifLog.trace __FUNCTION__;
    if EConstr.isEvar sigma a then fresh env sigma a b else sigma, normal a b
  ;;

  let debug_unify
        env
        sigma
        ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
        a
        b
    =
    UnifLog.trace __FUNCTION__;
    let f = Rocq_utils.Strfy.econstr env sigma in
    let g = Utils.Strfy.tuple ~args Utils.Strfy.string f in
    let a : string = g ("a", a) in
    let b : string = g ("b", b) in
    Of (Utils.Strfy.tuple ~args Utils.Strfy.string Utils.Strfy.string)
    |> UnifLog.thing ~__FUNCTION__ Debug "unified" (a, b)
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
    UnifLog.trace __FUNCTION__;
    let f = Rocq_utils.Strfy.econstr env sigma in
    let g = Utils.Strfy.tuple ~args Utils.Strfy.string f in
    let a : string = g ("a", a) in
    let b : string = g ("b", b) in
    Of (Utils.Strfy.tuple ~args Utils.Strfy.string Utils.Strfy.string)
    |> UnifLog.thing ~__FUNCTION__ Error "tried to unify" (a, b);
    Printf.sprintf "cannot unify \"%s\" with \"%s\"" (f c) (f d)
    |> UnifLog.error ~__FUNCTION__
  ;;

  let w_unify env sigma (a : EConstr.t) (b : EConstr.t) : Evd.evar_map * bool =
    UnifLog.trace __FUNCTION__;
    let open Pretype_errors in
    try
      let sigma = Unification.w_unify env sigma Conversion.CUMUL a b in
      sigma, true
    with
    | PretypeError (_, _, CannotUnify (c, d, _e)) ->
      UnifLog.error ~__FUNCTION__ "PretypeError";
      debug_unifyerr env sigma a b c d;
      sigma, false
  ;;

  (** [unify a b] tries to unify [a] and [b] within the context of the [env] and [sigma] of [mm]. @returns [true] if successful, [false] otherwise. *)
  let unify env sigma ({ a; b } : t) : Evd.evar_map * bool =
    UnifLog.trace __FUNCTION__;
    w_unify env sigma a b
  ;;
end

let debug_pair ?(__FUNCTION__ : string = "") (prefix : string) (x : Pair.t)
  : unit mm
  =
  state (fun env sigma ->
    UnifLog.thing ~__FUNCTION__ Debug prefix x (Args (Pair.to_string env sigma));
    sigma, ())
;;

module Problem = struct
  (** if [fst] is sucessfully unified then [snd] represents a tree of constructors that lead to that term (from some previously visited term).
  *)
  type t =
    { act : Pair.t
    ; goto : Pair.t
    ; tree : Mebi_constr.Tree.t
    }

  let to_string
        env
        sigma
        ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
        ({ act; goto; tree } : t)
    : string
    =
    UnifLog.trace __FUNCTION__;
    let open Utils.Strfy in
    let args : style_args =
      { args with name = None; style = Some (tuple_style ()) }
    in
    let f = Pair.to_string env sigma ~args:(nest args) in
    let act : string = f act in
    let goto : string = f goto in
    let tree : string = Tree.to_string ~args:(nest args) tree in
    let args : style_args = { args with style = Some (record_style ()) } in
    Utils.Strfy.record ~args [ "act", act; "goto", goto; "tree", tree ]
  ;;

  let unify_pair_opt (pair : Pair.t) : bool mm =
    UnifLog.trace __FUNCTION__;
    state (fun env sigma -> Pair.unify env sigma pair)
  ;;

  let unify_opt ({ act; goto; tree } : t) : Mebi_constr.Tree.t option mm =
    UnifLog.trace __FUNCTION__;
    let* () = debug_pair ~__FUNCTION__ "act" act in
    let* () = debug_pair ~__FUNCTION__ "goto" goto in
    let* unified_act_opt = unify_pair_opt act in
    UnifLog.trace ~__FUNCTION__ "unified act opt";
    let* unified_goto_opt = unify_pair_opt goto in
    UnifLog.trace ~__FUNCTION__ "unified goto opt";
    match unified_act_opt, unified_goto_opt with
    | true, true -> return (Some tree)
    | _, _ -> return None
  ;;
end

module Problems = struct
  type t =
    { sigma : Evd.evar_map
    ; to_unify : Problem.t list
    }

  let empty () : t mm =
    UnifLog.trace __FUNCTION__;
    let* sigma = get_sigma in
    return { sigma; to_unify = [] }
  ;;

  let is_empty : t -> bool =
    UnifLog.trace __FUNCTION__;
    function { to_unify = []; _ } -> true | _ -> false
  ;;

  let list_is_empty : t list -> bool =
    UnifLog.trace __FUNCTION__;
    function [] -> true | [ p ] -> is_empty p | _ -> false
  ;;

  let to_string
        env
        ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
        ({ sigma; to_unify } : t)
    : string
    =
    UnifLog.trace __FUNCTION__;
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
    UnifLog.trace __FUNCTION__;
    let open Utils.Strfy in
    list
      ~args:
        { (nest args) with
          style = Some (list_style ())
        ; name = Some "unification problems list"
        }
      (to_string env)
  ;;

  let rec unify_list_opt : Problem.t list -> Mebi_constr.Tree.t list option mm =
    UnifLog.trace __FUNCTION__;
    function
    | [] ->
      UnifLog.trace ~__FUNCTION__ "return";
      return (Some [])
    | h :: tl ->
      let* success_opt = Problem.unify_opt h in
      (match success_opt with
       | None ->
         UnifLog.trace ~__FUNCTION__ "unify success opt -> None";
         return None
       | Some constructor_tree ->
         UnifLog.trace ~__FUNCTION__ "unify success opt -> Some tree";
         let* unified_opt = unify_list_opt tl in
         (match unified_opt with
          | None ->
            UnifLog.trace ~__FUNCTION__ "tail unify success opt -> None";
            return None
          | Some acc ->
            UnifLog.trace ~__FUNCTION__ "tail unify success opt -> Some acc";
            return (Some (constructor_tree :: acc))))
  ;;

  let sandbox_unify_all_opt
        (act : EConstr.t)
        (goto : EConstr.t)
        ({ sigma = psigma; to_unify } : t)
    : (EConstr.t * EConstr.t * Mebi_constr.Tree.t list) option mm
    =
    UnifLog.trace __FUNCTION__;
    let* () = debug_econstr ~__FUNCTION__ "act" act in
    let* () = debug_econstr ~__FUNCTION__ "goto" goto in
    sandbox
      ~using:psigma
      (let* unified_opt = unify_list_opt to_unify in
       match unified_opt with
       | None ->
         UnifLog.trace ~__FUNCTION__ "unified opt -> None";
         return None
       | Some constructor_trees ->
         UnifLog.trace ~__FUNCTION__ "unified opt -> Some trees";
         let* () = debug_econstr ~__FUNCTION__ "(unified) act" act in
         let* () = debug_econstr ~__FUNCTION__ "(unified) goto" goto in
         let$+ act env sigma = Reductionops.nf_all env sigma act in
         let$+ goto env sigma = Reductionops.nf_all env sigma goto in
         let* () = debug_econstr ~__FUNCTION__ "(nf) act" act in
         let* () = debug_econstr ~__FUNCTION__ "(nf) goto" goto in
         let$+ is_act_undefined _ sigma = EConstr.isEvar sigma act in
         let$+ is_goto_undefined _ sigma = EConstr.isEvar sigma goto in
         if is_act_undefined && is_goto_undefined
         then (
           UnifLog.debug ~__FUNCTION__ "act or goto is undefined (return None)";
           return None)
         else (
           UnifLog.debug ~__FUNCTION__ "return Some (act, goto, trees)";
           return (Some (act, goto, constructor_trees))))
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
    UnifLog.trace __FUNCTION__;
    Utils.Strfy.list ~args (Mebi_constr.to_string env sigma)
  ;;

  let rec retrieve
            (constructor_index : int)
            (acc : t)
            (act : EConstr.t)
            (tgt : EConstr.t)
    : Enc.t * Problems.t list -> t mm
    =
    UnifLog.trace __FUNCTION__;
    function
    | _, [] ->
      UnifLog.trace ~__FUNCTION__ "return";
      return acc
    | lts_enc, problems :: tl ->
      UnifLog.trace ~__FUNCTION__ (Printf.sprintf "(rem %i)" (List.length acc));
      let* acc = retrieve constructor_index acc act tgt (lts_enc, tl) in
      let* constructor_opt : Mebi_constr.t option =
        sandbox
          (let* success = Problems.sandbox_unify_all_opt act tgt problems in
           match success with
           | None ->
             UnifLog.trace ~__FUNCTION__ "success -> None. return None";
             return None
           | Some (act, goto, constructor_trees) ->
             let open Mebi_constr.Tree in
             let tree =
               Node ((lts_enc, constructor_index), constructor_trees)
             in
             let constructor = act, goto, tree in
             UnifLog.trace ~__FUNCTION__ "success -> Some. return constructor";
             return (Some constructor))
      in
      (match constructor_opt with
       | None ->
         UnifLog.trace ~__FUNCTION__ "constructor -> None. (cont.)";
         (* retrieve ~debug constructor_index acc act tgt (lts_enc, tl) *)
         return acc
       | Some constructor ->
         UnifLog.trace ~__FUNCTION__ "constructor -> Some. (cont.)";
         let act, goto, tree = constructor in
         let acc = constructor :: acc in
         let* () = debug_econstr ~__FUNCTION__ "act" act in
         let* () = debug_econstr ~__FUNCTION__ "goto" goto in
         UnifLog.thing ~__FUNCTION__ Debug "tree" tree (Args Tree.to_string);
         (* retrieve ~debug constructor_index acc act tgt (lts_enc, tl) *)
         return acc)
  ;;
end
