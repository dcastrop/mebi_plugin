open Pp

(** [pp_list l] is a pretty printed list ([l]). *)
let pp_list l =
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

(** [pp_transitions_to_list env sigma constrs] is. *)
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

(** [pp_transitions env sigma transitions] is an [array] of [transitions] pretty printed as a [list]. *)
let pp_transitions env sigma transitions =
  pp_list (pp_transitions_to_list env sigma transitions)
;;

(** [pp_edge env sigma edge] is a pretty printed [edge]. *)
let pp_edge env sigma (edge : Evd.econstr) =
  Printer.pr_econstr_env env sigma edge
;;

(** [pp_edges_to_list env sigma constrs] is a pretty printed list of edges ([constrs]). *)
let rec pp_edges_to_list env sigma (edges : Evd.econstr list) =
  match edges with
  | [] -> []
  | h_edge :: t_edges ->
    pp_edge env sigma h_edge :: pp_edges_to_list env sigma t_edges
;;

(** [pp_edges env sigma edges] is a [t] (str) of pretty printed edges ([constrs]). *)
let pp_edges env sigma (edges : Evd.econstr list) =
  pp_list (pp_edges_to_list env sigma edges)
;;

(** [pp_state env sigma state] is a pretty printed [state]. *)
let pp_state env sigma (state : Evd.econstr) =
  Printer.pr_econstr_env env sigma state
;;

(** [pp_states_to_list env sigma constrs] is a pretty printed list of states. *)
let rec pp_states_to_list env sigma (states : Evd.econstr list) =
  match states with
  | [] -> []
  | h_state :: t_states ->
    pp_state env sigma h_state :: pp_states_to_list env sigma t_states
;;

(** [pp_states env sigma states] is a [t] (str) of pretty printed states. *)
let pp_states
  (env : Environ.env)
  (sigma : Evd.evar_map)
  (states : Evd.econstr list)
  =
  pp_list (pp_states_to_list env sigma states)
;;

(** [] *)
let pp_coq_fsm
  (env : Environ.env)
  (sigma : Evd.evar_map)
  (fsm : Evd.econstr list * Evd.econstr list)
  : Pp.t
  =
  match fsm with
  | states, edges ->
    str "states:\n"
    ++ pp_states env sigma states
    ++ str "\n\nedges:\n"
    ++ pp_edges env sigma edges
;;
