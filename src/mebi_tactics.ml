open Logging
open Mebi_wrapper

(* let seq (first:unit Proofview.tactic mm) : 'a Proofview.tactic mm -> unit Proofview.tactic mm = function sec ->
   let open Mebi_wrapper.Syntax in
   let* first = first in
   return (Proofview.tclTHEN first sec ) *)

let seq (pre : 'a Proofview.tactic option) (x : unit Proofview.tactic)
  : unit Proofview.tactic
  =
  match pre with None -> x | Some y -> Proofview.tclTHEN x y
;;

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
;;

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

let unknown_counter = ref 0

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
;;
