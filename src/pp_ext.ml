open Pp
open Mebi_structs

(** [pp_list l] is a pretty printed list ([l]). *)
let pp_list (l : 'a list) =
  if List.is_empty l
  then str "[] (empty)\n"
  else
    (* ! use [fnl()] for newlines (will only be used if necessary). *)
    str "[\n"
    ++ Pp.prlist_with_sep
         (* sep *) (fun _ -> str ", " ++ fnl ())
         (* fun *) (fun i -> str "  " ++ i)
         (* list *) l
    ++ str "\n]\n"
;;

(** [pp_transition env sigma transition] is a pretty printed [transition]. *)
let pp_transition env sigma (transition : Constr.rel_context * Constr.t) =
  Printer.pr_constr_env env sigma (snd transition)
;;

(** [pp_transitions_to_list env sigma transitions] is an list of pretty-printed [transitions]. *)
let pp_transitions_to_list env sigma transitions =
  let rec transitions_to_list i res =
    if i < 0
    then res
    else
      transitions_to_list
        (i - 1)
        (pp_transition env sigma (Array.unsafe_get transitions i) :: res)
  in
  transitions_to_list (Array.length transitions - 1) []
;;

(** [pp_transitions env sigma transitions] is an array of [transitions] pretty-printed as a list. *)
let pp_transitions env sigma transitions =
  pp_list (pp_transitions_to_list env sigma transitions)
;;

(** [pp_edge env sigma edge] is a pretty-printed [edge]. *)
let pp_edge env sigma (edge : Evd.econstr) =
  Printer.pr_econstr_env env sigma edge
;;

(** [pp_edges_to_list env sigma edges] is a pretty-printed list of [edges] (i.e., [constrs]). *)
let rec pp_edges_to_list env sigma (edges : Evd.econstr list) =
  match edges with
  | [] -> []
  | h_edge :: t_edges ->
    pp_edge env sigma h_edge :: pp_edges_to_list env sigma t_edges
;;

(** [pp_edges env sigma edges] is a [t] (str) of pretty-printed [edges] (i.e., [constrs]). *)
let pp_edges env sigma (edges : Evd.econstr list) =
  pp_list (pp_edges_to_list env sigma edges)
;;

(** [pp_edges env sigma edges] is a [t] (str) of pretty-printed [edges] (i.e., [constrs]). *)
let pp_edges' env sigma (edges : (Evd.econstr * Evd.econstr) list) =
  pp_list (pp_edges_to_list env sigma (Mebi_utils.strip_snd edges))
;;

(** [pp_state env sigma state] is a pretty-printed [state]. *)
let pp_state env sigma (state : Evd.econstr) =
  Printer.pr_econstr_env env sigma state
;;

(** [pp_states_to_list env sigma constrs] is a pretty-printed list of [states]. *)
let rec pp_states_to_list env sigma (states : Evd.econstr list) =
  match states with
  | [] -> []
  | h_state :: t_states ->
    pp_state env sigma h_state :: pp_states_to_list env sigma t_states
;;

(** [pp_states env sigma states] is a [t] (str) of pretty-printed states. *)
let pp_states
  (env : Environ.env)
  (sigma : Evd.evar_map)
  (states : Evd.econstr list)
  =
  pp_list (pp_states_to_list env sigma states)
;;

(** [pp_coq_lts lts] is a [t] (str) of pretty-printed coq-based [lts]. *)
let pp_coq_lts (lts : Mebi_structs.coq_lts) : Pp.t =
  match lts with
  | { env
    ; sigma
    ; lts_type
    ; term_type
    ; type_lbls
    ; constr_names
    ; constr_transitions
    ; start_term
    ; states
    ; edges
    ; _
    } ->
    let header_pp =
      str "lts of type: \""
      ++ Printer.pr_econstr_env env sigma lts_type
      ++ str "\";\n"
    in
    let terms_pp =
      str "has terms of: \""
      ++ Printer.pr_econstr_env env sigma term_type
      ++ str "\";\n"
    in
    let labels_pp =
      str "has labels of: \""
      ++ Printer.pr_econstr_env env sigma type_lbls
      ++ str "\";\n"
    in
    let constrs_pp =
      str "constructors: \""
      ++ Pp.prvect_with_sep (fun _ -> str "\", \"") Names.Id.print constr_names
      ++ str "\";\n"
    in
    let transitions_pp =
      str "transitions: "
      ++ pp_transitions env sigma constr_transitions
      ++ strbrk ""
      (* ++ strbrk "\n" *)
    in
    let states_pp = str "states: " ++ pp_states env sigma states in
    let edges_pp = str "edges: " ++ pp_edges env sigma edges in
    (* return all joined together *)
    header_pp
    ++ str "{\n  "
    ++ terms_pp
    ++ str "  "
    ++ labels_pp
    ++ str "  "
    ++ constrs_pp
    ++ str "}\n"
    ++ transitions_pp
    ++ states_pp
    ++ edges_pp
;;
