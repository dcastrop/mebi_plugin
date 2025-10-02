open Logging
open Mebi_wrapper

let apply_In_sim () : unit Proofview.tactic mm =
  let open Mebi_wrapper.Syntax in
  let* t = Mebi_theories.c_In_sim () in
  return (Proofview.Goal.enter (fun gl -> Tactics.apply t))
;;

let apply_Pack_sim () : unit Proofview.tactic mm =
  let open Mebi_wrapper.Syntax in
  let* t = Mebi_theories.c_Pack_sim () in
  return (Proofview.Goal.enter (fun gl -> Tactics.apply t))
;;

(****************************************************************************)

let unfold_econstr : EConstr.t -> unit Proofview.tactic mm = function
  | x ->
    Log.trace "mebi_tactics.unfold_econstr";
    let open Mebi_wrapper.Syntax in
    let* z = econstr_to_constr x in
    (match Constr.kind z with
     | Const (c, _) ->
       Log.debug "mebi_tactics.unfold_econstr, const";
       return (Tactics.unfold_constr (Names.GlobRef.ConstRef c))
     | _ ->
       Log.warning "mebi_tactics.unfold_econstr -- not Constr!";
       return (Proofview.tclUNIT ()))
;;

let unfold_constrexpr : Constrexpr.constr_expr -> unit Proofview.tactic mm
  = function
  | x ->
    Log.trace "mebi_tactics.unfold_constrexpr";
    let open Mebi_wrapper.Syntax in
    let* y = constrexpr_to_econstr x in
    let* z = econstr_to_constr y in
    (match Constr.kind z with
     | Const (c, _) ->
       Log.debug "mebi_tactics.unfold_constrexpr, const";
       return (Tactics.unfold_constr (Names.GlobRef.ConstRef c))
     | _ ->
       Log.warning "mebi_tactics.unfold_constrexpr -- not Constr!";
       return (Proofview.tclUNIT ()))
;;

let rec unfold_constrexpr_list
  : Constrexpr.constr_expr list -> unit Proofview.tactic mm
  = function
  | [] -> return (Proofview.tclUNIT ())
  | h :: [] -> unfold_constrexpr h
  | h :: t ->
    let open Syntax in
    let* h' = unfold_constrexpr h in
    let* t' = unfold_constrexpr_list t in
    return (Proofview.tclTHEN h' t')
;;

let cofix ?(name : Names.Id.t option = None) () : unit Proofview.tactic mm =
  Log.trace "mebi_tactics.cofix";
  let open Mebi_wrapper.Syntax in
  let* name =
    match name with
    | None -> Mebi_wrapper.new_name_of_string ~add:true "Cofix0"
    | Some name -> return name
  in
  return (Tactics.cofix name)
;;

let intros_all () : unit Proofview.tactic mm =
  Log.trace "mebi_tactics.intros_all";
  return Tactics.intros
;;

let intro_of_string (s : string) : unit Proofview.tactic mm =
  Log.trace "mebi_tactics.intro_of_string";
  let open Mebi_wrapper.Syntax in
  let* name = Mebi_wrapper.new_name_of_string ~add:true s in
  return (Tactics.introduction name)
;;

(*
   type unfold_kind =
   | Gr of Names.GlobRef.t
   | Id of Names.Id.t
   (* | Ln of Libnames.qualid *)
   | Ex of EConstr.t
   | Ce of Constrexpr.constr_expr

   let unfold ?(pre : 'a Proofview.tactic option = None) ()
   : unfold_kind -> unit Proofview.tactic mm
   = function
   | Gr x ->
   Log.debug "mebi_tactics.unfold Gr _";
   return (seq pre (Tactics.unfold_constr x))
   | Id x ->
   Log.debug "mebi_tactics.unfold Id _";
   return (seq pre (Tactics.unfold_body x))
   | Ex x ->
   Log.debug "mebi_tactics.unfold Ex _";
   let open Mebi_wrapper.Syntax in
   let* _ = Mebi_wrapper.debug_term_kind x in
   let* _ = Mebi_wrapper.debug_term_constr_kind x in
   (* return (Proofview.tclUNIT ()) *)
   let* y = econstr_to_constr x in
   (match Constr.kind y with
   | App (z, _) ->
   Log.debug "mebi_tactics.unfold Ex _ App";
   return (seq pre (Proofview.tclUNIT ()))
   (* return (Tactics.unfold_body y) *)
   | _ ->
   Log.debug "mebi_tactics.unfold Ex _ Other";
   return (seq pre (Proofview.tclUNIT ())))
   | Ce x ->
   Log.debug "mebi_tactics.unfold Ce _";
   let open Mebi_wrapper.Syntax in
   let* y = constrexpr_to_econstr x in
   let* _ = Mebi_wrapper.debug_term_kind y in
   let* _ = Mebi_wrapper.debug_term_constr_kind y in
   let* z = econstr_to_constr y in
   (match Constr.kind z with
   | Const (z, _) ->
   Log.debug "mebi_tactics.unfold Ce _ Const";
   return (Tactics.unfold_constr (Names.GlobRef.ConstRef z))
   | _ ->
   Log.debug "mebi_tactics.unfold Ce _ Other";
   return (seq pre (Proofview.tclUNIT ())))
   ;; *)

(* let* z = econstr_to_constr y in
   (match Constr.kind z with
   | App (z, _) ->
   Log.debug "mebi_tactics.unfold Ce _ App";
   return (Proofview.tclUNIT ())
   (* return (Tactics.unfold_body x) *)
   | _ ->
   Log.debug "mebi_tactics.unfold Ce _ Other";
   return (Proofview.tclUNIT ())) *)

(* | Ln l ->
   Log.debug "mebi_tactics.unfold Ln _";
   Tactics.unfold_constr (Mebi_utils.ref_to_glob l) *)

(* | Ce e -> Mebi_wrapper.constr *)

(*************************************************)

(* let unknown_counter = ref 0

   let new_auto_name ?(prefix : string = "H") () : string =
   let s = Printf.sprintf "%s%i" prefix !unknown_counter in
   unknown_counter := !unknown_counter + 1;
   s
   ;;

   let handle_name : string option -> string = function
   | None -> new_auto_name ()
   | Some n -> new_auto_name ~prefix:n ()
   ;;

   type id_kind =
   | Id of Names.Id.t
   | Str of string
   | Unk of unit

   let intro ?(pre : 'a Proofview.tactic option = None) ()
   : id_kind -> unit Proofview.tactic
   = function
   | Id i ->
   Log.debug "mebi_tactics.intro Id _";
   seq pre (Tactics.introduction i)
   | Str s ->
   Log.debug "mebi_tactics.intro Str _";
   let i = Names.Id.of_string (handle_name (Some s)) in
   seq pre (Tactics.introduction i)
   | Unk _ ->
   Log.debug "mebi_tactics.intro Unk _";
   seq pre Tactics.intro
   ;;

   let rec intros
   ?(all : bool = false)
   ?(pre : 'a Proofview.tactic option = None)
   ()
   : id_kind list -> unit Proofview.tactic
   = function
   | [] -> seq pre (if all then Tactics.intros else Proofview.tclUNIT ())
   | h :: t -> seq pre (Proofview.tclTHEN (intro () h) (intros ~all () t))
   ;; *)
