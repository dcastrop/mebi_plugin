(* This module deliberately forces Debug and Trace on for itself, independently
   of the user-facing configuration -- Logger.Scoped keeps that local rather
   than making it a functor parameter threaded through the rest of lib/. *)
module Log = Logger.Scoped (struct
    let overrides = [ Output.Kind.Debug, true; Output.Kind.Trace, true ]
  end)

(** {!get_constants}' cache. *)
let constants' : (string, EConstr.t) Hashtbl.t ref option ref = ref None

(* See the [.mli]. *)
exception ErrorWithGlobalOfPath

(* See the [.mli]. *)
let find_reference (path : string list) (id : string) : Names.GlobRef.t =
  let path = Names.DirPath.make (List.rev_map Names.Id.of_string path) in
  let fp = Libnames.make_path path (Names.Id.of_string id) in
  try Nametab.global_of_path fp with
  | Not_found ->
    Log.thing ~__FUNCTION__ Error "fp" fp Libnames.string_of_path;
    raise ErrorWithGlobalOfPath
;;

(** The theory terms the plugin uses: each name, and the library module it
    is defined in. *)
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
  ; "bisimF", [ "MEBI"; "Bisimilarity" ]
  ; "Pack_bisim", [ "MEBI"; "Bisimilarity" ]
  ; "weak_bisimilar", [ "MEBI"; "Bisimilarity" ]
  ; "In_bisim", [ "MEBI"; "Bisimilarity" ]
  ; "weak_bisimilar_refl", [ "MEBI"; "Bisimilarity" ]
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
    (* "clos_trans_1n" was listed a second time here (collapsed by
       [Hashtbl.of_seq], so harmless). Possibly [clos_refl_trans_1n] was
       meant -- the relation the solver's weak-transition goals use -- but
       adding it would make those goals count as theory in
       [Theories.is_any_theory] (unfolding, the premise-goal check), so that is
       left as an open question, not changed. *)
  ]
  |> List.to_seq
  |> Hashtbl.of_seq
;;

(** [get_constant_to_load r] is the term of the global reference [r] in the
    global environment. Raises nothing. *)
let get_constant_to_load (x : Names.GlobRef.t) : EConstr.t =
  EConstr.of_constr (UnivGen.constr_of_monomorphic_global (Global.env ()) x)
;;

(* See the [.mli]. *)
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

(** [constant_not_found k] fails, naming [k] and its module if known.

    @raise Failure always (raised here). *)
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

(* See the [.mli]. *)
let get (k : string) : EConstr.t =
  match Hashtbl.find_opt (get_constants ()) k with
  | Some v -> v
  | None -> constant_not_found k
;;
