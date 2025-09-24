open Mebi_wrapper
open Mebi_wrapper.Syntax
(* open Vernac *)
(* open Command *)

open Logging

(* Libnames.qualid *)

let check (args : Constrexpr.constr_expr) : unit mm =
  Log.warning "mebi_api.check";
  let* (t : EConstr.t) = constrexpr_to_econstr args in
  Log.warning (Printf.sprintf "mebi_api.check, t = %s" (econstr_to_string t));
  return ()
;;

let run
      (args : Constrexpr.constr_expr)
        (* (api : Constrexpr.constr_expr) *)
        (* (rs : Libnames.qualid list) *)
  : unit mm
  =
  Log.warning "mebi_api.run";
  let* (t : EConstr.t) = constrexpr_to_econstr args in
  Log.warning (Printf.sprintf "mebi_api.run, t = %s" (econstr_to_string t));
  return ()
;;
