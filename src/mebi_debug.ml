open Debug
open Mebi_wrapper
(* open Mebi_wrapper.Syntax *)

(* let debug_problems env sigma (problems : Mebi_unification.Problems.t) : unit =
   Logging.Log.debug
   (Printf.sprintf
   "unification problems:\n%s\n"
   (Mebi_unification.Problems.to_string env sigma problems))
   ;; *)

(* let debug_problems_mm (problems : Mebi_unification.Problems.t) : unit mm =
   state (fun env sigma ->
   let () = debug_problems env sigma problems in
   sigma, ())
   ;; *)

(* let debug_problems_list env sigma (problems : Mebi_unification.Problems.t list)
   : unit
   =
   Logging.Log.debug
   (Printf.sprintf
   "unification problems list:\n%s\n"
   (Mebi_unification.Problems.list_to_string env sigma problems))
   ;; *)

(* let debug_problems_list_mm (problems : Mebi_unification.Problems.t list)
   : unit mm
   =
   state (fun env sigma ->
   let () = debug_problems_list env sigma problems in
   sigma, ())
   ;; *)

(****************************************************)

(* let debug_labelled_problem_list
   env
   sigma
   (labelled_problems : (EConstr.t * Mebi_unification.Problem.t) list)
   : unit
   =
   List.iter
   (fun ((action, problem) : EConstr.t * Mebi_unification.Problem.t) ->
   let action : string = Rocq_utils.Strfy.econstr env sigma action in
   Logging.Log.debug
   (Printf.sprintf
   "labelled unification problem: %s\n%s\n"
   action
   (Mebi_unification.Problem.to_string env sigma problem)))
   labelled_problems
   ;; *)

(* let debug_labelled_problem_list_mm
   (labelled_problems : (EConstr.t * Mebi_unification.Problem.t) list)
   : unit mm
   =
   state (fun env sigma ->
   let () = debug_labelled_problem_list env sigma labelled_problems in
   sigma, ())
   ;; *)

(* let debug_labelled_problems_list
   env
   sigma
   (labelled_problems : (EConstr.t * Mebi_unification.Problems.t) list)
   : unit
   =
   List.iter
   (fun ((action, problems) : EConstr.t * Mebi_unification.Problems.t) ->
   let action : string = Rocq_utils.Strfy.econstr env sigma action in
   Logging.Log.debug
   (Printf.sprintf
   "labelled unification problem: %s\n%s\n"
   action
   (Mebi_unification.Problems.to_string env sigma problems)))
   labelled_problems
   ;; *)

(* let debug_labelled_problems_list_mm
   (labelled_problems : (EConstr.t * Mebi_unification.Problems.t) list)
   : unit mm
   =
   Logging.Log.debug "POST CROSS PRODUCT: B";
   state (fun env sigma ->
   let () = debug_labelled_problems_list env sigma labelled_problems in
   sigma, ())
   ;; *)

let debug_labelled_cross_product env sigma xact yact xproblems yproblem : unit =
  let f : EConstr.t -> string = Rocq_utils.Strfy.econstr env sigma in
  let xactevar : bool = EConstr.isEvar sigma xact in
  let xact : string = f xact in
  let yactevar : bool = EConstr.isEvar sigma yact in
  let yact : string = f yact in
  Logging.Log.debug
    (Printf.sprintf
       "labelled cross product:\n- xact (%b): %s\n- yact (%b): %s"
       xactevar
       xact
       yactevar
       yact)
;;

let debug_labelled_cross_product_mm xact yact xproblems yproblem : unit mm =
  state (fun env sigma ->
    let () =
      debug_labelled_cross_product env sigma xact yact xproblems yproblem
    in
    sigma, ())
;;

let debug_constructors
      env
      sigma
      (constructors : Mebi_unification.Constructors.t)
  : unit
  =
  Logging.Log.debug
    (Printf.sprintf
       "next constructors:\n%s\n"
       (Mebi_unification.Constructors.to_string env sigma constructors))
;;

let debug_constructors_mm (constructors : Mebi_unification.Constructors.t)
  : unit mm
  =
  state (fun env sigma ->
    let () = debug_constructors env sigma constructors in
    sigma, ())
;;

(***************************************)

(** Mebi_unify.check_valid_constructors START *)
let debug_validconstrs_start (from : EConstr.t) : unit mm =
  state (fun env sigma ->
    let infix : string = "valid constructors" in
    let from : string = Rocq_utils.Strfy.econstr env sigma from in
    let () = Scope.start ~infix ~suffix:from () in
    sigma, ())
;;

(** Mebi_unify.check_valid_constructors CLOSE *)
let debug_validconstrs_close
      (from : EConstr.t)
      (constructors : Mebi_unification.Constructors.t)
  : unit mm
  =
  state (fun env sigma ->
    let l : int = List.length constructors in
    let infix : string = Printf.sprintf "valid constructors |%i|" l in
    let from : string = Rocq_utils.Strfy.econstr env sigma from in
    let () = Scope.close ~infix ~suffix:from () in
    sigma, ())
;;

(***************************************)

(** Mebi_unify.check_valid_constructors.iter_body START *)
let debug_validconstrs_iter_start
      (i : int)
      (constructors : Mebi_unification.Constructors.t)
  : unit mm
  =
  state (fun env sigma ->
    (* Logging.Log.notice "\n"; *)
    let infix : string = Printf.sprintf "check valid constructor (%i)" i in
    let l : int = List.length constructors in
    let suffix : string = Printf.sprintf "(acc: |%i|)" l in
    let () = Scope.start ~infix ~suffix () in
    sigma, ())
;;

(** Mebi_unify.check_valid_constructors.iter_body CLOSE *)
let debug_validconstrs_iter_close
      (i : int)
      (constructors : Mebi_unification.Constructors.t)
  : unit mm
  =
  state (fun env sigma ->
    let infix : string = Printf.sprintf "check valid constructor (%i)" i in
    let l : int = List.length constructors in
    let suffix : string = Printf.sprintf "(acc: |%i|)" l in
    let () = Scope.close ~infix ~suffix () in
    sigma, ())
;;

(***************************************)

(** Mebi_unify.check_valid_constructors.iter_body START *)
let debug_validconstrs_iter_success_start
      (from : EConstr.t)
      (act : EConstr.t option)
      (_args : Mebi_unification.constructor_args)
  : unit mm
  =
  state (fun env sigma ->
    (* Logging.Log.notice "\n"; *)
    let from : string = Rocq_utils.Strfy.econstr env sigma from in
    let _act : string =
      Option.cata (fun act -> Rocq_utils.Strfy.econstr env sigma act) "None" act
    in
    let infix : string = "explore valid constructor" in
    (* let suffix : string = Printf.sprintf "(from: %s) (act: %s)" from act in *)
    let suffix : string = Printf.sprintf "(from: %s)" from in
    let () = Scope.start ~infix ~suffix () in
    sigma, ())
;;

(** Mebi_unify.check_valid_constructors.iter_body CLOSE *)
let debug_validconstrs_iter_success_close
      (from : EConstr.t)
      (act : EConstr.t option)
      (args : Mebi_unification.constructor_args)
  : unit mm
  =
  state (fun env sigma ->
    (* Logging.Log.notice "\n"; *)
    let from : string = Rocq_utils.Strfy.econstr env sigma from in
    let _act : string =
      Option.cata (fun act -> Rocq_utils.Strfy.econstr env sigma act) "None" act
    in
    let infix : string = "explore valid constructor" in
    (* let suffix : string = Printf.sprintf "(from: %s) (act: %s)" from act in *)
    let suffix : string = Printf.sprintf "(from: %s)" from in
    let () = Scope.close ~infix ~suffix () in
    sigma, ())
;;

(***************************************)

(** Mebi_unify.check_for_next_constructors START *)
let debug_nextconstrs_start () : unit mm =
  state (fun env sigma ->
    let infix : string = "checking next" in
    let () = Scope.start ~infix () in
    sigma, ())
;;

(** Mebi_unify.check_for_next_constructors CLOSE *)
let debug_nextconstrs_close
      (problems : Mebi_unification.Problems.t list)
      (is_evar : bool option)
      (constructors : Mebi_unification.Constructors.t)
  : unit mm
  =
  state (fun env sigma ->
    let l : int = List.length problems in
    let m : int = List.length constructors in
    let infix = "checking next" in
    let f = fun e -> Printf.sprintf " (tgt is evar: %b)" e in
    let e : string = Option.cata f "" is_evar in
    let suffix =
      Printf.sprintf "(problems: |%i|) (constructors: |%i|)%s" l m e
    in
    let () = Scope.close ~infix ~suffix () in
    (* let () = debug_problems_list env sigma problems in *)
    (* let () = debug_constructors env sigma constructors in *)
    sigma, ())
;;

(** Mebi_unify.check_for_next_constructors RETURN *)
let debug_nextconstrs_return () : unit mm =
  state (fun env sigma ->
    let () = Scope.return ~infix:"next constructors" () in
    sigma, ())
;;

(***************************************)

(** Mebi_unify.check_updated_ctx START *)
let debug_updtcontext_start () : unit mm =
  state (fun env sigma ->
    let infix : string = "updating context" in
    let () = Scope.start ~infix () in
    sigma, ())
;;

(** Mebi_unify.check_updated_ctx CLOSE *)
let debug_updtcontext_close
      (upd_t : EConstr.t)
      (ind : Mebi_ind.t option)
      (constructors : Mebi_unification.Constructors.t)
  : unit mm
  =
  state (fun env sigma ->
    let infix : string = "updating context" in
    let upd_t : string = Rocq_utils.Strfy.econstr env sigma upd_t in
    let suffix : string = Printf.sprintf "(is not App: %s)" upd_t in
    let () = Scope.close ~infix ~suffix () in
    sigma, ())
;;

let debug_updtcontext_close_app (name : EConstr.t) : unit mm =
  state (fun env sigma ->
    let infix : string = "updating context" in
    let name : string = Rocq_utils.Strfy.econstr env sigma name in
    let suffix : string = Printf.sprintf "(unrecognized App: %s)" name in
    let () = Scope.close ~infix ~suffix () in
    sigma, ())
;;

let debug_updtcontext_close_app_known
      (name : EConstr.t)
      (ind : Mebi_ind.t)
      (constructors : Mebi_unification.Constructors.t)
  : unit mm
  =
  state (fun env sigma ->
    let infix : string = "updating context" in
    let name : string = Rocq_utils.Strfy.econstr env sigma name in
    let suffix : string =
      let l : int = List.length constructors in
      Printf.sprintf "(App: %s) (next: |%i|)" name l
    in
    let () = Scope.close ~infix ~suffix () in
    sigma, ())
;;

(** Mebi_unify.check_updated_ctx RETURN *)
let debug_updtcontext_return () : unit mm =
  state (fun env sigma ->
    let () = Scope.return ~infix:"updated context" () in
    sigma, ())
;;
