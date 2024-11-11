(* open Pp *)
open Fsm
open Stringify

(** [default_table_size] is the default size of the [translation_table]. *)
let default_table_size = 10

(** [] *)
type fsm_table =
  { env : Environ.env
  ; sigma : Evd.evar_map
  ; term_type : string
  ; lbls_type : string
  ; type_name : string
  ; state_map : (Evd.econstr, int) Hashtbl.t
  }

(** [] *)
let fsm_table
  ?(state_map = Hashtbl.create default_table_size)
  (env : Environ.env)
  (sigma : Evd.evar_map)
  (term_type : string)
  (lbls_type : string)
  (type_name : string)
  =
  { env; sigma; term_type; lbls_type; type_name; state_map }
;;

type translate_map = (Evd.econstr * id) list

(** [econstr_to_int] is [e] converted to [int] (via [econstr_to_string] and [int_of_string]). *)
let econstr_to_int env sigma e : int =
  int_of_string (econstr_to_string env sigma e)
;;

(** [get_states env sigma s] is the list of [Fsm.state]s (i.e., [Fsm.states]) derived from a list [s] of [Evd.econstr]. *)
let get_states (env : Environ.env) (sigma : Evd.evar_map) (s : Evd.econstr list)
  : states * translate_map
  =
  let rec states'
    (s' : Evd.econstr list)
    (acc : states)
    (map : (Evd.econstr * id) list)
    (i : int)
    : states * (Evd.econstr * id) list
    =
    match s' with
    | [] -> acc, map
    | h :: t ->
      states'
        t
        (List.concat
           [ [ state
                 ~name:(Printf.sprintf "s%s" (econstr_to_string env sigma h))
                 i
             ]
           ; acc
           ])
        (List.concat [ [ h, i ]; map ])
        (i + 1)
  in
  states' s [] [] 0
;;

(** [] *)
let get_edges
  (env : Environ.env)
  (sigma : Evd.evar_map)
  (es : Evd.econstr list)
  (state_map : (Evd.econstr, int) Hashtbl.t)
  : edges * translate_map
  =
  let rec edges'
    (es' : Evd.econstr list)
    (acc : edges)
    (map : (Evd.econstr * id) list)
    (i : int)
    : edges * translate_map
    =
    match es' with
    | [] -> acc, map
    | h :: t ->
      let lhs_id, label, rhs_id =
        match EConstr.decompose_app sigma h with
        | h' ->
          (match h' with
           | _lhs, rhs ->
             (* Feedback.msg_info
                (str (Printf.sprintf "edge #%d lhs': " i)
                ++ Printer.pr_econstr_env env sigma _lhs
                ++ str "\n  rhs': "
                ++ str "[\n"
                ++ Pp.prlist_with_sep
                     (* sep *) (fun _ -> str ", " ++ fnl ())
                     (* fun *) (fun i -> str "  " ++ i)
                     (* list *)
                     (let rec rhs_to_list i res =
                        if i < 0
                        then res
                        else
                          rhs_to_list
                            (i - 1)
                            (Printer.pr_econstr_env
                               env
                               sigma
                               (Array.unsafe_get rhs i)
                             :: res)
                      in
                      rhs_to_list (Array.length rhs - 1) [])
                ++ str "\n]\n"); *)
             let rhs' = Array.to_list rhs in
             List.nth rhs' 0, List.nth rhs' 1, List.nth rhs' 2)
        (* Hashtbl.find
           state_map
           lhs
           , Hashtbl.find state_map rhs *)
      in
      edges'
        t
        (List.concat
           [ [ edge
                 ~label:(econstr_to_string env sigma label)
                 i
                 (ID (econstr_to_int env sigma lhs_id))
                 (ID (econstr_to_int env sigma rhs_id))
             ]
           ; acc
           ])
        (List.concat [ [ h, i ]; map ])
        (i + 1)
    (* edges'
       t
       acc (* (List.concat [ [ edge i - 1 - 1 ]; acc ]) *)
       (List.concat [ [ h, i ]; map ])
       (i + 1))) *)
  in
  edges' es [] [] 0
;;

(* (** [] *)
   let _ =

   ;; *)

(** [] *)
let lts_to_fsm
  (env : Environ.env)
  (sigma : Evd.evar_map)
  (lts_type : Evd.econstr)
  (term_type : Evd.econstr)
  (lbls_type : Evd.econstr)
  (start_term : Evd.econstr)
  (transitions : (Constr.rel_context * Constr.t) array)
  (raw_states : Evd.econstr list)
  (lts : Evd.econstr list)
  : fsm_table * fsm
  =
  (*** [tbl] is an [fsm_table]. *)
  let tbl =
    fsm_table
      env
      sigma
      (econstr_to_string env sigma term_type)
      (econstr_to_string env sigma lbls_type)
      (econstr_to_string env sigma lts_type)
  in
  (* translate [raw_states] from coq api to ocaml [Fsm.states], and get the [state_map] from [Evd.econstr] to [id]. *)
  let states, state_map = get_states env sigma raw_states in
  (* update the [state_map] *)
  Hashtbl.add_seq tbl.state_map (List.to_seq state_map);
  (* translate [lts] from coq api to ocaml [Fsm.edges]. *)
  let edges, edge_map = get_edges env sigma lts tbl.state_map in
  (* add meta data *)
  tbl, fsm ~init:(Hashtbl.find tbl.state_map start_term) states edges
;;

(* let () =

   ;; *)
