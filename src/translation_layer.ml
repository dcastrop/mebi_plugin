open Pp
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

type translate_from =
  (* State of Evd.econstr *)
  | States of Evd.econstr list
  (* | Edge of Evd.econstr *)
  | Edges of (Evd.econstr * Evd.econstr) list

type translate_to =
  | State of state
  | States of states
  | Edge of edge
  | Edges of edges

type translate_map = (Evd.econstr * id) list
(*
   let get_from_econstr
   (env : Environ.env)
   (sigma : Evd.evar_map)
   (l : translate_from)
   : translate_to * translate_map
   =
   (* determine acc and map *)
   let acc, map =
   match l with
   | States _ -> States [], []
   | Edges _ -> Edges [], []
   in
   (* recurse over l *)
   let rec get_from
   (l : translate_from)
   (acc : translate_to)
   (map : translate_map)
   (index : int)
   : translate_to * translate_map
   =
   match l with
   | States s ->
   (match s with
   | [] -> acc, map
   | h :: t -> get_from
   (States t)
   (States (List.concat [ [ state ~name:(econstr_to_string env sigma h) index ]; acc ]))
   (List.concat [ [ h, index ]; map ])
   (index + 1)
   )
   | Edges e ->
   (match e with | [] -> acc, map | h::t -> get_from (Edges t) (Edges (List.concat [[edge ~]])) )
   in
   get_from' l acc map 0
   ;; *)

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
        (List.concat [ [ state ~name:(econstr_to_string env sigma h) i ]; acc ])
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
      (* let lhs_id,rhs_id = *)
      (match EConstr.decompose_app sigma h with
       | h' ->
         (match h' with
          | lhs', rhs' ->
            Feedback.msg_info
              (str (Printf.sprintf "edge #%d lhs': " i)
               ++ Printer.pr_econstr_env env sigma lhs'
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
                              (Array.unsafe_get rhs' i)
                            :: res)
                     in
                     rhs_to_list (Array.length rhs' - 1) [])
               ++ str "\n]\n");
            (* Hashtbl.find state_map lhs, Hashtbl.find state_map rhs
               in
               edges' t (List.concat [ [ edge i lhs_id rhs_id]; acc ]) (List.concat [ [ h, i ]; map ]) (i + 1) *)
            edges'
              t
              acc (* (List.concat [ [ edge i - 1 - 1 ]; acc ]) *)
              (List.concat [ [ h, i ]; map ])
              (i + 1)))
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
  tbl, fsm states edges
;;

(* let () =

   ;; *)
