open Debug
open Mebi_wrapper
(* open Mebi_wrapper.Syntax *)

(** Mebi_unify.check_valid_constructors START *)
let debug_validconstrs_start (from : EConstr.t) : unit mm =
  state (fun env sigma ->
    let infix : string = "valid constructors" in
    let from : string = Strfy.econstr env sigma from in
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
    let infix : string = Printf.sprintf "valid constructors (%i)" l in
    let from : string = Strfy.econstr env sigma from in
    let () = Scope.close ~infix ~suffix:from () in
    sigma, ())
;;

(***************************************)

(** Mebi_unify.check_valid_constructors.iter_body START *)
let debug_validconstrs_iter_start () : unit mm =
  state (fun env sigma ->
    Logging.Log.notice "\n";
    let infix : string = "check valid constructor" in
    let () = Scope.start ~infix () in
    sigma, ())
;;

(** Mebi_unify.check_valid_constructors.iter_body CLOSE *)
let debug_validconstrs_iter_close () : unit mm =
  state (fun env sigma ->
    let infix : string = "check valid constructor" in
    let () = Scope.close ~infix () in
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
    Logging.Log.notice "\n";
    let from : string = Strfy.econstr env sigma from in
    let _act : string =
      Option.cata (fun act -> Strfy.econstr env sigma act) "None" act
    in
    let infix : string = "valid constructor" in
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
    Logging.Log.notice "\n";
    let from : string = Strfy.econstr env sigma from in
    let _act : string =
      Option.cata (fun act -> Strfy.econstr env sigma act) "None" act
    in
    let infix : string = "valid constructor" in
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
  : unit mm
  =
  state (fun env sigma ->
    let l : int = List.length problems in
    let infix = "checking next" in
    let f = fun e -> Printf.sprintf ", (tgt is evar: %b)" e in
    let e : string = Option.cata f "" is_evar in
    let suffix = Printf.sprintf "(problems: %i)%s" l e in
    let info = Mebi_unification.Problems.list_to_string env sigma problems in
    let () = Scope.close ~infix ~suffix ~info:(Some info) () in
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
    let upd_t : string = Strfy.econstr env sigma upd_t in
    let suffix : string = Printf.sprintf "(is not App: %s)" upd_t in
    let () = Scope.close ~infix ~suffix () in
    sigma, ())
;;

let debug_updtcontext_close_app (name : EConstr.t) : unit mm =
  state (fun env sigma ->
    let infix : string = "updating context" in
    let name : string = Strfy.econstr env sigma name in
    let suffix : string = Printf.sprintf "(is unrecognized App: %s)" name in
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
    let name : string = Strfy.econstr env sigma name in
    let suffix : string =
      let l : int = List.length constructors in
      Printf.sprintf "(is App: %s) (next constructors: %i)" name l
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
