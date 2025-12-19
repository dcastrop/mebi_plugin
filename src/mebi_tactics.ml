(***********************************************************************)
module Log : Logger.LOGGER_TYPE = Logger.MkDefault ()

let () = Log.Config.configure_output Debug false
let () = Log.Config.configure_output Trace false
(***********************************************************************)

(**********************************)
(****** COQ PROOF TACTICS *********)
(**********************************)

let update_proof_by_tactic (pstate : Declare.Proof.t)
  : unit Proofview.tactic -> Declare.Proof.t
  =
  fun x ->
  let new_pstate, is_safe_tactic = Declare.Proof.by x pstate in
  if Bool.not is_safe_tactic
  then Log.warning "mebi_tactics.update_proof_by_tactic, unsafe tactic used";
  new_pstate
;;

let rec update_proof_by_tactics (pstate : Declare.Proof.t)
  : unit Proofview.tactic list -> Declare.Proof.t
  = function
  | [] -> pstate
  | h :: t -> update_proof_by_tactics (update_proof_by_tactic pstate h) t
;;

(*************************)

let do_inversion (h : Rocq_utils.hyp) : unit Proofview.tactic =
  Inv.inv_tac (Context.Named.Declaration.get_id h)
;;

let subst_all () : unit Proofview.tactic = Equality.subst_all ()

let simplify_all ?(gl : Proofview.Goal.t option) () : unit Proofview.tactic =
  let the_fun =
    fun gl ->
    List.fold_left
      (fun acc (h : Rocq_utils.hyp) ->
        Mebi_theories.tactics
          [ acc
          ; Tactics.simpl_in_hyp
              (Context.Named.Declaration.get_id h, Locus.InHyp)
          ])
      Tactics.simpl_in_concl
      (Proofview.Goal.hyps gl)
  in
  match gl with None -> Proofview.Goal.enter the_fun | Some gl -> the_fun gl
;;

let simplify_and_subst_all ?(gl : Proofview.Goal.t option) ()
  : unit Proofview.tactic
  =
  match gl with
  | None -> Mebi_theories.tactics [ simplify_all (); subst_all () ]
  | Some gl -> Mebi_theories.tactics [ simplify_all ~gl (); subst_all () ]
;;

let the_goals : (int, Proofview.Goal.t) Hashtbl.t ref = ref (Hashtbl.create 0)

let reset_the_goals () : unit =
  Args Utils.Strfy.int
  |> Log.thing ~__FUNCTION__ Debug "pre-reset num" (Hashtbl.length !the_goals);
  Hashtbl.clear !the_goals
;;

let add_goal (g : Proofview.Goal.t) : unit =
  Hashtbl.add !the_goals (Evar.hash (Proofview.Goal.goal g)) g
;;

(****************************************************************************)

let apply ?(gl : Proofview.Goal.t option) (c : EConstr.t)
  : unit Proofview.tactic
  =
  let the_fun = fun gl -> Tactics.apply c in
  match gl with None -> Proofview.Goal.enter the_fun | Some gl -> the_fun gl
;;

let eapply ?(gl : Proofview.Goal.t option) (c : EConstr.t)
  : unit Proofview.tactic
  =
  let the_fun = fun gl -> Tactics.eapply c in
  match gl with None -> Proofview.Goal.enter the_fun | Some gl -> the_fun gl
;;

(****************************************************************************)

let unfold_econstr (gl : Proofview.Goal.t) : EConstr.t -> unit Proofview.tactic
  = function
  | x ->
    let sigma = Proofview.Goal.sigma gl in
    let y = Rocq_convert.econstr_to_constr sigma x in
    (match Constr.kind y with
     | Const (c, _) ->
       Log.debug "mebi_tactics.unfold_econstr, const";
       Tactics.unfold_constr (Names.GlobRef.ConstRef c)
     | _ ->
       Log.warning "mebi_tactics.unfold_econstr -- not Constr!";
       Proofview.tclUNIT ())
;;

let unfold_constrexpr (gl : Proofview.Goal.t)
  : Constrexpr.constr_expr -> unit Proofview.tactic
  = function
  | x ->
    let env = Proofview.Goal.env gl in
    let sigma = Proofview.Goal.sigma gl in
    let sigma, y = Rocq_convert.constrexpr_to_econstr env sigma x in
    let z = Rocq_convert.econstr_to_constr sigma y in
    (match Constr.kind z with
     | Const (c, _) ->
       Log.debug "mebi_tactics.unfold_constrexpr, const";
       Tactics.unfold_constr (Names.GlobRef.ConstRef c)
     | _ ->
       Log.warning "mebi_tactics.unfold_constrexpr -- not Constr!";
       Proofview.tclUNIT ())
;;

let rec unfold_constrexpr_list (gl : Proofview.Goal.t)
  : Constrexpr.constr_expr list -> unit Proofview.tactic
  = function
  | [] -> Proofview.tclUNIT ()
  | h :: [] -> unfold_constrexpr gl h
  | h :: t ->
    Mebi_theories.tactics
      [ unfold_constrexpr gl h; unfold_constrexpr_list gl t ]
;;

let cofix (gl : Proofview.Goal.t) : unit Proofview.tactic =
  Tactics.cofix (Mebi_theories.new_cofix_name gl)
;;

let intros_all () : unit Proofview.tactic = Tactics.intros

let intro_of_string (gl : Proofview.Goal.t) (s : string) : unit Proofview.tactic
  =
  Tactics.introduction (Mebi_theories.new_name_of_string gl s)
;;
