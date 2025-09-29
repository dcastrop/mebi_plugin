open Logging
open Mebi_wrapper

type unfold_kind =
  | Gr of Names.GlobRef.t
  | Id of Names.Id.t
  (* | Ln of Libnames.qualid *)
  | Ex of EConstr.t
  | Ce of Constrexpr.constr_expr

let unfold : unfold_kind -> unit Proofview.tactic mm = function
  | Gr x ->
    Log.debug "mebi_tactics.unfold Gr _";
    return (Tactics.unfold_constr x)
  | Id x ->
    Log.debug "mebi_tactics.unfold Id _";
    return (Tactics.unfold_body x)
  | Ex x ->
    Log.debug "mebi_tactics.unfold Ex _";
    let open Mebi_wrapper.Syntax in
    let* _ = Mebi_wrapper.debug_term_kind x in
    let* _ = Mebi_wrapper.debug_term_constr_kind x in
    return (Proofview.tclUNIT ())
    (* (match Constr.kind z with
       | App (z, _) ->
       Log.debug "mebi_tactics.unfold Ex _ App";
       return (Proofview.tclUNIT ())
       (* return (Tactics.unfold_body x) *)
       | _ ->
       Log.debug "mebi_tactics.unfold Ex _ Other";
       return (Proofview.tclUNIT ())) *)
  | Ce x ->
    Log.debug "mebi_tactics.unfold Ce _";
    let open Mebi_wrapper.Syntax in
    let* y = constrexpr_to_econstr x in
    let* _ = Mebi_wrapper.debug_term_kind y in
    let* _ = Mebi_wrapper.debug_term_constr_kind y in
    return (Proofview.tclUNIT ())
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

let intro : id_kind -> unit Proofview.tactic = function
  | Id i ->
    Log.debug "mebi_tactics.intro Id _";
    Tactics.introduction i
  | Str s ->
    Log.debug "mebi_tactics.intro Str _";
    let i = Names.Id.of_string (handle_name (Some s)) in
    Tactics.introduction i
  | Unk _ ->
    Log.debug "mebi_tactics.intro Unk _";
    Tactics.intro
;;

let rec intros ?(all : bool = false) () : id_kind list -> unit Proofview.tactic
  = function
  | [] -> if all then Tactics.intros else Proofview.tclUNIT ()
  | h :: t -> Proofview.tclTHEN (intro h) (intros ~all () t)
;;
