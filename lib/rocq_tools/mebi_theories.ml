(***********************************************************************)
module Log : Logger.S = Logger.MkDefault ()

let () = Log.Config.configure_output Debug true
let () = Log.Config.configure_output Trace true
(***********************************************************************)

(* let rec tactics : unit Proofview.tactic list -> unit Proofview.tactic = function
   | [] -> Proofview.tclUNIT ()
   | h :: [] -> h
   | h :: t -> Proofview.tclTHEN h (tactics t)
   ;; *)

(*****************************************************************************)

(* source: https://github.com/rocq-prover/rocq/blob/master/doc/plugin_tutorial/tuto3/src/tuto_tactic.ml *)

let constants : EConstr.t list ref = ref ([] : EConstr.t list)
let constants' : (string, EConstr.t) Hashtbl.t ref option ref = ref None

exception ErrorWithGlobalOfPath

let find_reference (path : string list) (id : string) : Names.GlobRef.t =
  let path = Names.DirPath.make (List.rev_map Names.Id.of_string path) in
  let fp = Libnames.make_path path (Names.Id.of_string id) in
  try Nametab.global_of_path fp with
  | Not_found ->
    Log.thing ~__FUNCTION__ Error "fp" fp Libnames.string_of_path;
    raise ErrorWithGlobalOfPath
;;

(* let find_reference = Coqlib.find_reference [@ocaml.warning "-3"] *)

(****************************************************************************)

(****************************************************************************)

let reference_paths_to_load : (string, string list) Hashtbl.t =
  [ "LTS", [ "MEBI"; "Bisimilarity" ]
  ; "tau", [ "MEBI"; "Bisimilarity" ]
  ; "silent", [ "MEBI"; "Bisimilarity" ]
  ; "silent1", [ "MEBI"; "Bisimilarity" ]
  ; "weak", [ "MEBI"; "Bisimilarity" ]
  ; "wk_some", [ "MEBI"; "Bisimilarity" ]
  ; "wk_none", [ "MEBI"; "Bisimilarity" ]
  ; "simF", [ "MEBI"; "Bisimilarity" ]
  ; "Pack_sim", [ "MEBI"; "Bisimilarity" ]
  ; "sim_weak", [ "MEBI"; "Bisimilarity" ]
  ; "weak_sim", [ "MEBI"; "Bisimilarity" ]
  ; "In_sim", [ "MEBI"; "Bisimilarity" ]
  ; "out_sim", [ "MEBI"; "Bisimilarity" ]
  ; "weak_bisim", [ "MEBI"; "Bisimilarity" ]
  ; "weak_sim_refl", [ "MEBI"; "Bisimilarity" ]
  ; "wk_bisim_refl", [ "MEBI"; "Bisimilarity" ]
  ; "option", [ "Corelib"; "Init"; "Datatypes" ]
  ; "None", [ "Corelib"; "Init"; "Datatypes" ]
  ; "Some", [ "Corelib"; "Init"; "Datatypes" ]
  ; "ex", [ "Corelib"; "Init"; "Logic" ]
  ; "ex_intro", [ "Corelib"; "Init"; "Logic" ]
  ; "prod", [ "Corelib"; "Init"; "Datatypes" ]
  ; "pair", [ "Corelib"; "Init"; "Datatypes" ]
  ; "list", [ "Corelib"; "Init"; "Datatypes" ]
  ; "cons", [ "Corelib"; "Init"; "Datatypes" ]
  ; "nil", [ "Corelib"; "Init"; "Datatypes" ]
  ; "relation", [ "Corelib"; "Relations"; "Relation_Definitions" ]
  ; "clos_trans_1n", [ "Stdlib"; "Relations"; "Relation_Operators" ]
  ; "rt1n_refl", [ "Stdlib"; "Relations"; "Relation_Operators" ]
  ; "rt1n_trans", [ "Stdlib"; "Relations"; "Relation_Operators" ]
  ; "clos_trans_1n", [ "Stdlib"; "Relations"; "Relation_Operators" ]
  ]
  |> List.to_seq
  |> Hashtbl.of_seq
;;

let get_constant_to_load (x : Names.GlobRef.t) : EConstr.t =
  EConstr.of_constr (UnivGen.constr_of_monomorphic_global (Global.env ()) x)
;;

(*
   let load_econstr_constant (k : string) : EConstr.t =
   let cs = get_constants () in
   Hashtbl.replace
   ;; *)

let get_constants () : (string, EConstr.t) Hashtbl.t =
  match !constants' with
  | None ->
    let x = Hashtbl.create 0 in
    Hashtbl.iter
      (fun k v -> Hashtbl.add x k (find_reference v k |> get_constant_to_load))
      reference_paths_to_load;
    constants' := Some (ref x);
    x
  | Some x -> !x
;;

let constant_not_found (k : string) : 'a =
  match Hashtbl.find_opt reference_paths_to_load k with
  | Some v ->
    failwith
      (Printf.sprintf
         "could not obtain an internal representation of %s.%s"
         (List.fold_left String.cat "" v)
         k)
  | None ->
    failwith
      (Printf.sprintf "could not obtain an internal representation of %s" k)
;;

let get (k : string) : EConstr.t =
  match Hashtbl.find_opt (get_constants ()) k with
  | Some v -> v
  | None -> constant_not_found k
;;

(* This is a pattern to collect terms from the Coq memory of valid terms
   and proofs.  This pattern extends all the way to the definition of function
   c_U *)
(* let collect_bisimilarity_theories () : EConstr.t list =
  Log.trace __FUNCTION__;
  match !constants with
  | [] ->
    Log.trace ~__FUNCTION__ "mapping constants";
    (* TODO: why does error "Not_found" occur if line below is removed? *)
    let new_constants : EConstr.t list =
      List.map
        (fun (x : Names.GlobRef.t) ->
          EConstr.of_constr
            (UnivGen.constr_of_monomorphic_global (Global.env ()) x))
        (Hashtbl.fold
           (fun k v acc -> find_reference v k :: acc)
           references_args
           [])
    in
    Log.trace ~__FUNCTION__ "post new constants";
    Hashtbl.clear constants';
    Hashtbl.iter
      (fun k v ->
        Hashtbl.add
          constants'
          k
          (EConstr.of_constr
             (UnivGen.constr_of_monomorphic_global
                (Global.env ())
                (find_reference v k))))
      references_args;
    constants := new_constants;
    !constants
  | _ -> !constants
;;

let rec indexed_c : int * EConstr.t list -> EConstr.t option = function
  | i, [] -> None
  | 0, h :: _ -> Some h
  | i, _ :: t -> indexed_c (i - 1, t)
;;

let c_LTS () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (0, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of MEBI.Bisimilarity.LTS"
  | Some c -> c
;;

let c_tau () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (1, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of MEBI.Bisimilarity.tau"
  | Some c -> c
;;

let c_silent () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (2, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of MEBI.Bisimilarity.silent"
  | Some c -> c
;;

let c_silent1 () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (3, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of MEBI.Bisimilarity.silent1"
  | Some c -> c
;;

let c_weak () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (4, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of MEBI.Bisimilarity.weak"
  | Some c -> c
;;

let c_wk_some () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (5, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of MEBI.Bisimilarity.wk_some"
  | Some c -> c
;;

let c_wk_none () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (6, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of MEBI.Bisimilarity.wk_none"
  | Some c -> c
;;

let c_simF () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (7, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of MEBI.Bisimilarity.simF"
  | Some c -> c
;;

let c_Pack_sim () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (8, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       MEBI.Bisimilarity.Pack_sim"
  | Some c -> c
;;

let c_sim_weak () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (9, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       MEBI.Bisimilarity.sim_weak"
  | Some c -> c
;;

let c_weak_sim () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (10, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       MEBI.Bisimilarity.weak_sim"
  | Some c -> c
;;

let c_In_sim () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (11, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of MEBI.Bisimilarity.In_sim"
  | Some c -> c
;;

let c_out_sim () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (12, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of MEBI.Bisimilarity.out_sim"
  | Some c -> c
;;

let c_weak_bisim () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (13, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       MEBI.Bisimilarity.weak_bisim"
  | Some c -> c
;;

let c_relation () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (14, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Corelib.Relations.Relation_Definitions.relations"
  | Some c -> c
;;

let c_clos_refl_trans_1n () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (15, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Stdlib.Relations.Relation_Operators.clos_refl_trans_1n"
  | Some c -> c
;;

let c_rt1n_refl () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (16, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Stdlib.Relations.Relation_Operators.rt1n_refl"
  | Some c -> c
;;

let c_rt1n_trans () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (17, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Stdlib.Relations.Relation_Operators.rt1n_trans"
  | Some c -> c
;;

let c_clos_trans_1n () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (18, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Stdlib.Relations.Relation_Operators.clos_trans_1n"
  | Some c -> c
;;

let c_option () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (19, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Corelib.Init.Datatypes.option"
  | Some c -> c
;;

let c_None () : EConstr.t =
  Log.trace __FUNCTION__;
  let _cs = collect_bisimilarity_theories () in
  (* match indexed_c (20, cs) with *)
  match Hashtbl.find_opt constants' "None" with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Corelib.Init.Datatypes.None"
  | Some c -> c
;;

let c_Some () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (21, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Corelib.Init.Datatypes.Some"
  | Some c -> c
;;

let c_ex () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (22, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of Corelib.Init.Logic.ex"
  | Some c -> c
;;

let c_ex_intro () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (23, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Corelib.Init.Logic.ex.ex_intro"
  | Some c -> c
;;

let c_prod () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (24, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Corelib.Init.Datatypes.prod"
  | Some c -> c
;;

let c_pair () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (25, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       Corelib.Init.Datatypes.pair"
  | Some c -> c
;;

let c_weak_sim_refl () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (26, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       MEBI.Bisimilarity.weak_sim_refl"
  | Some c -> c
;;

let c_wk_bisim_refl () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (27, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of \
       MEBI.Bisimilarity.wk_bisim_refl"
  | Some c -> c
;;

let c_cons () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (28, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of Stdlib.Lists.List.cons"
  | Some c -> c
;;

let c_nil () : EConstr.t =
  Log.trace __FUNCTION__;
  let cs = collect_bisimilarity_theories () in
  match indexed_c (29, cs) with
  | None ->
    failwith
      "could not obtain an internal representation of Stdlib.Lists.List.nil"
  | Some c -> c
;; *)

(* let c_ () : EConstr.t =
   Log.trace __FUNCTION__;
   let cs = collect_bisimilarity_theories () in
   match indexed_c (23, cs) with
   | None ->
   failwith
   "could not obtain an internal representation of "
   | Some c -> c
   ;; *)

(*****************************************************************************)

(******)

(*****************************************************************************)

let get_proof_from_pstate : Declare.Proof.t -> Proof.t = Declare.Proof.get
let get_partial_proof : Proof.t -> EConstr.t list = Proof.partial_proof

(*****************************************************************************)
