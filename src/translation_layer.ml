open Fsm
(* open Stringify *)

(** [default_table_size] is the default size of the [translation_table]. *)
let default_table_size = 10

(** [] *)
type fsm_table =
  { term_type : string
  ; lbls_type : string
  ; type_name : string
  ; state_map : (Evd.econstr, int) Hashtbl.t
  }

(** [] *)
let fsm_table
  ?(state_map = Hashtbl.create default_table_size)
  (term_type : string)
  (lbls_type : string)
  (type_name : string)
  =
  { term_type; lbls_type; type_name; state_map }
;;

(** [] *)
let econstr_to_string
  (env : Environ.env)
  (sigma : Evd.evar_map)
  (target : Evd.econstr)
  : string
  =
  Pp.string_of_ppcmds (Printer.pr_econstr_env env sigma target)
;;

(** [] *)
let lts_to_fsm
  (env : Environ.env)
  (sigma : Evd.evar_map)
  (lts_type : Evd.econstr)
  (term_type : Evd.econstr)
  (lbls_type : Evd.econstr)
  (start_term : Evd.econstr)
  (lts : (Evd.econstr * Evd.econstr) list)
  (transitions : (Constr.rel_context * Constr.t) array)
  : fsm_table * fsm
  =
  (*** [tbl] is an [fsm_table]. *)
  let tbl =
    fsm_table
      (econstr_to_string env sigma term_type)
      (econstr_to_string env sigma lbls_type)
      (econstr_to_string env sigma lts_type)
  in
  (* let tbl = tbl.state_map = fsm [] [] in *)
  (* add meta data *)
  tbl, fsm [] []
;;

(* (** [] *)
   let _ =

   ;; *)

(* (** [] *)
   let _ =

   ;; *)

(* (** [] *)
   let _ =

   ;; *)

(* let () =

   ;; *)
