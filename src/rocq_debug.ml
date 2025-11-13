(* open Debug *)
open Logging

let debug_econstr p env sigma (x : EConstr.t) : unit =
  Log.debug
    (Printf.sprintf
       "%seconstr: %s"
       (Utils.prefix p)
       (Rocq_utils.Strfy.econstr env sigma x))
;;

let debug_econstr_mm p (x : EConstr.t) : unit Mebi_wrapper.mm =
  Mebi_wrapper.state (fun env sigma ->
    let () = debug_econstr p env sigma x in
    sigma, ())
;;
