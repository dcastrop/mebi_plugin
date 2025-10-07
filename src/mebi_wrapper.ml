open Logging

type term = EConstr.t

let enable_logging : bool ref = ref true

(********************************************)
(****** COQ ENVIRONMENT/CONTEXT *************)
(********************************************)

(* TODO: move proof stuff to own monad/wrapper *)
type proof_context =
  { mutable proof : Declare.Proof.t option
  ; mutable names : Names.Id.Set.t option
  }

type coq_context =
  { coq_env : Environ.env
  ; coq_ctx : Evd.evar_map
  ; proofv : proof_context
  }

(* *)
let the_proofv_opt : proof_context ref option ref = ref None

let new_proofv (proof : Declare.Proof.t option) (names : Names.Id.Set.t option)
  : proof_context ref
  =
  Log.trace "mebi_wrapper.new_proofv: Created new proofv.";
  let the_proofv : proof_context ref = ref { proof; names } in
  the_proofv_opt := Some the_proofv;
  the_proofv
;;

let the_coq_proofv
      ?(new_proof : bool = false)
      ?(proof : Declare.Proof.t option = None)
      ?(names : Names.Id.Set.t option = None)
      ()
  : proof_context ref
  =
  Log.trace "mebi_wrapper.the_coq_proofv";
  match !the_proofv_opt with
  | None ->
    Log.debug "mebi_wrapper.the_coq_proofv: proofv is None, using args";
    new_proofv proof names
  | Some proofv ->
    if new_proof
    then (
      Log.debug "mebi_wrapper.the_coq_proofv: new proof";
      new_proofv proof names)
    else (
      let the_proof =
        match !proofv.proof with
        | None ->
          Log.debug
            "mebi_wrapper.the_coq_proofv: proofv.proof is None, using proof arg";
          proof
        | Some _ ->
          (match proof with
           | None ->
             Log.debug
               "mebi_wrapper.the_coq_proofv: proofv.proof is Some and proof \
                arg is None, preserving proofv.proof";
             !proofv.proof
           | Some q ->
             Log.debug
               "mebi_wrapper.the_coq_proofv: proofv.proof and proof arg are \
                Some, overriding, using new proof arg";
             proof)
      in
      let the_names =
        match !proofv.names with
        | None ->
          Log.debug
            "mebi_wrapper.the_coq_proofv: proofv.names is None, using names arg";
          names
        | Some _ ->
          (match names with
           | None ->
             Log.debug
               "mebi_wrapper.the_coq_proofv: proofv.names is Some and names \
                arg is None, preserving proofv.names";
             !proofv.names
           | Some q ->
             Log.debug
               "mebi_wrapper.the_coq_proofv: proofv.names and names arg are \
                Some, overriding, using new names arg";
             names)
      in
      new_proofv the_proof the_names)
;;

(** *)
let the_coq_env_opt : Environ.env ref option ref = ref None

let new_coq_env () : Environ.env ref =
  if !enable_logging
  then Log.debug "mebi_wrapper.new_coq_env: Created new coq env.";
  let env : Environ.env ref = ref (Global.env ()) in
  the_coq_env_opt := Some env;
  env
;;

let the_coq_env ?(fresh : bool = false) () : Environ.env ref =
  match !the_coq_env_opt with
  | None -> new_coq_env ()
  | Some env -> if fresh then new_coq_env () else env
;;

(** *)
let the_coq_ctx_opt : Evd.evar_map ref option ref = ref None

let new_coq_ctx ?(fresh : bool = false) () : Evd.evar_map ref =
  if !enable_logging
  then Log.debug "mebi_wrapper.new_coq_ctx: Created new coq ctx.";
  let ctx = ref (Evd.from_env !(the_coq_env ~fresh ())) in
  the_coq_ctx_opt := Some ctx;
  ctx
;;

let the_coq_ctx ?(fresh : bool = false) () : Evd.evar_map ref =
  match !the_coq_ctx_opt with
  | None -> new_coq_ctx ~fresh ()
  | Some ctx -> if fresh then new_coq_ctx ~fresh () else ctx
;;

(********************************************)
(****** FORWARD ENCODING MAP ****************)
(********************************************)

module F : Hashtbl.S with type key = term = Hashtbl.Make (struct
    type t = term

    let equal t1 t2 = EConstr.eq_constr !(the_coq_ctx ()) t1 t2

    let hash t =
      Constr.hash
        (EConstr.to_constr
           ?abort_on_undefined_evars:(Some false)
           !(the_coq_ctx ())
           t)
    ;;
  end)

(********************************************)
(****** ENCODINGS ***************************)
(********************************************)

module type ENCODING_TYPE = sig
  type t

  val init : t
  val cache : t ref
  val reset : unit -> unit
  val eq : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val to_string : t -> string
  val of_int : int -> t

  module type ENC_TBL = Hashtbl.S with type key = t

  module Tbl : ENC_TBL

  val encode : t F.t -> term Tbl.t -> term -> t

  exception InvalidDecodeKey of (t * term Tbl.t)

  val decode_opt : term Tbl.t -> t -> term option
  val decode : term Tbl.t -> t -> term
end

(**********************************)
(****** INTEGER ENCODING **********)
(**********************************)

module IntEncoding : ENCODING_TYPE = struct
  type t = int

  let init : t = 0
  let cache : t ref = ref init
  let counter = cache

  let reset () =
    cache := init;
    ()
  ;;

  let eq t1 t2 = Int.equal t1 t2
  let compare t1 t2 = Int.compare t1 t2
  let hash t = Int.hash t
  let to_string t : string = Printf.sprintf "%i" t
  let of_int (i : int) : t = i

  module type ENC_TBL = Hashtbl.S with type key = t

  module Tbl : ENC_TBL = Hashtbl.Make (struct
      type t = int

      let equal t1 t2 = eq t1 t2
      let hash t = hash t
    end)

  let encode (fwd : t F.t) (bck : term Tbl.t) (k : term) : t =
    Log.trace "Mebi_wrapper.IntEncoding.encode";
    match F.find_opt fwd k with
    | None ->
      (* map to next encoding and return *)
      let next_enc : t = !counter in
      counter := !counter + 1;
      F.add fwd k next_enc;
      Tbl.add bck next_enc k;
      Log.debug
        (Printf.sprintf
           "Mebi_wrapper.IntEncoding.encode, new encoding: %s"
           (to_string next_enc));
      next_enc
    | Some enc ->
      Log.debug
        (Printf.sprintf
           "Mebi_wrapper.IntEncoding.encode -- already encoded as (%s)"
           (to_string enc));
      enc
  ;;

  exception InvalidDecodeKey of (t * term Tbl.t)

  let decode_opt (bck : term Tbl.t) (k : t) : term option = Tbl.find_opt bck k

  let decode (bck : term Tbl.t) (k : t) : term =
    match Tbl.find_opt bck k with
    | None -> raise (InvalidDecodeKey (k, bck))
    | Some enc -> enc
  ;;
end

module E = IntEncoding
module B = E.Tbl

(********************************************)
(****** WRAPPER & CONTEXT *******************)
(********************************************)

type wrapper =
  { coq_ref : coq_context ref
  ; fwd_enc : E.t F.t
  ; bck_enc : term B.t
  }

type 'a in_context =
  { state : wrapper ref
  ; value : 'a
  }

type 'a mm = wrapper ref -> 'a in_context

(** [run x] initializes the monad, and runs [x].
    @param ?keep_encoding
      is [true] when this is called mid-run.
      E.g., via [econstr_to_string]
    @param x is the command to run inside the [wrapper] state monad. *)
let run
      ?(keep_encoding : bool = false)
      ?(fresh : bool = true)
      ?(new_proof : bool = false)
      ?(proof : Declare.Proof.t option = None)
      (x : 'a mm)
  : 'a
  =
  Log.trace "mebi_wrapper.run";
  let coq_env : Environ.env = !(the_coq_env ~fresh ()) in
  let coq_ctx : Evd.evar_map = !(the_coq_ctx ()) in
  let proofv : proof_context = !(the_coq_proofv ~new_proof ~proof ()) in
  let coq_ref : coq_context ref = ref { coq_env; coq_ctx; proofv } in
  if keep_encoding then () else E.reset ();
  let fwd_enc : E.t F.t = F.create 0 in
  let bck_enc = B.create 0 in
  let a = x (ref { coq_ref; fwd_enc; bck_enc }) in
  (* enable_logging := false; *)
  a.value
;;

let string_mm (s : string mm) : string = run ~keep_encoding:true s

let return (x : 'a) : 'a mm =
  fun (st : wrapper ref) -> { state = st; value = x }
[@@inline always]
;;

let bind (x : 'a mm) (f : 'a -> 'b mm) : 'b mm =
  fun (st : wrapper ref) ->
  let a = x st in
  f a.value a.state
[@@inline always]
;;

let map (f : 'a -> 'b) (x : 'a mm) : 'b mm =
  fun (st : wrapper ref) ->
  let x_st = x st in
  { x_st with value = f x_st.value }
[@@inline always]
;;

let product (x : 'a mm) (y : 'b mm) : ('a * 'b) mm =
  bind x (fun a -> bind y (fun b -> return (a, b)))
[@@inline always]
;;

(** Monadic for loop *)
let rec iterate
          (from_idx : int)
          (to_idx : int)
          (acc : 'a)
          (f : int -> 'a -> 'a mm)
  : 'a mm
  =
  if from_idx > to_idx
  then return acc
  else bind (f from_idx acc) (fun acc' -> iterate (from_idx + 1) to_idx acc' f)
;;

(********************************************)
(****** GET & PUT STATE *********************)
(********************************************)

let set_proof (new_proof : Declare.Proof.t) (st : wrapper ref) : unit in_context
  =
  Log.trace "mebi_wrapper.set_proof";
  let coq_st = !st.coq_ref in
  let names = !coq_st.proofv.names in
  let proofv = !(the_coq_proofv ~proof:(Some new_proof) ~names ()) in
  coq_st := { !coq_st with proofv };
  st := { !st with coq_ref = coq_st };
  { state = st; value = () }
;;

let get_env (st : wrapper ref) : Environ.env in_context =
  let coq_st = !st.coq_ref in
  { state = st; value = !coq_st.coq_env }
;;

let get_sigma (st : wrapper ref) : Evd.evar_map in_context =
  let coq_st = !st.coq_ref in
  { state = st; value = !coq_st.coq_ctx }
;;

let get_proofv (st : wrapper ref) : proof_context in_context =
  let coq_st = !st.coq_ref in
  { state = st; value = !coq_st.proofv }
;;

let get_fwd_enc (st : wrapper ref) : E.t F.t in_context =
  { state = st; value = !st.fwd_enc }
;;

let get_bck_enc (st : wrapper ref) : term B.t in_context =
  { state = st; value = !st.bck_enc }
;;

let state
      (f : Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
      (st : wrapper ref)
  : 'a in_context
  =
  let coq_st = !st.coq_ref in
  let sigma, a = f !coq_st.coq_env !coq_st.coq_ctx in
  coq_st := { !coq_st with coq_ctx = sigma };
  { state = st; value = a }
;;

let sandbox (m : 'a mm) (st : wrapper ref) : 'a in_context =
  let st_contents = !st in
  let res = m st in
  st := st_contents;
  { state = st; value = res.value }
;;

(**********************************)
(****** COQ TERMS *****************)
(**********************************)

let econstr_eq (a : term) (b : term) : bool mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  { state = st; value = EConstr.eq_constr !coq_st.coq_ctx a b }
;;

let term_eq (a : term) (b : term) : bool mm = econstr_eq a b

let econstr_to_constr ?(abort_on_undefined_evars : bool = false) (x : EConstr.t)
  : Constr.t mm
  =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  { state = st
  ; value = EConstr.to_constr ~abort_on_undefined_evars !coq_st.coq_ctx x
  }
;;

let term_to_constr ?(abort_on_undefined_evars : bool = false) (x : term)
  : Constr.t mm
  =
  econstr_to_constr x
;;

let econstr_to_constr_opt (x : EConstr.t) : Constr.t option mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  { state = st; value = EConstr.to_constr_opt !coq_st.coq_ctx x }
;;

let term_to_constr_opt (x : term) : Constr.t option mm = econstr_to_constr_opt x

let constrexpr_to_econstr (t : Constrexpr.constr_expr) : EConstr.t mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  let sigma, t =
    Constrintern.interp_constr_evars !coq_st.coq_env !coq_st.coq_ctx t
  in
  coq_st := { !coq_st with coq_ctx = sigma };
  { state = st; value = t }
;;

let constrexpr_to_term (t : Constrexpr.constr_expr) : term mm =
  constrexpr_to_econstr t
;;

let normalize_econstr (t : EConstr.t) : EConstr.t mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  let t = Reductionops.nf_all !coq_st.coq_env !coq_st.coq_ctx t in
  { state = st; value = t }
;;

let normalize_term (t : term) : term mm = normalize_econstr t

let type_of_econstr (t : EConstr.t) : EConstr.t mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  let t = Reductionops.nf_all !coq_st.coq_env !coq_st.coq_ctx t in
  let sigma, t = Typing.type_of !coq_st.coq_env !coq_st.coq_ctx t in
  coq_st := { !coq_st with coq_ctx = sigma };
  { state = st; value = t }
;;

let type_of_term (t : term) : term mm = type_of_econstr t

let new_evar_of_econstr (t : EConstr.t) : EConstr.t mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  let sigma, instance = Evarutil.new_evar !coq_st.coq_env !coq_st.coq_ctx t in
  coq_st := { !coq_st with coq_ctx = sigma };
  { state = st; value = instance }
;;

let new_evar_of_term (t : term) : term mm = new_evar_of_econstr t

(* let evar_to_econstr (t:Evar.t) : EConstr.t mm = 
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  (* let sigma, t = EConstr.mkLEvar sigma *)
  let x = Evarutil. in
  { state = st; value = t }
  ;; *)

(********************************************)
(****** SYNTAX ******************************)
(********************************************)

module type MEBI_MONAD_SYNTAX = sig
  val ( let+ ) : 'a mm -> ('a -> 'b) -> 'b mm
  val ( let* ) : 'a mm -> ('a -> 'b mm) -> 'b mm

  val ( let$ )
    :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
    -> ('a -> 'b mm)
    -> 'b mm

  val ( let$* )
    :  (Environ.env -> Evd.evar_map -> Evd.evar_map)
    -> (unit -> 'b mm)
    -> 'b mm

  val ( let$+ ) : (Environ.env -> Evd.evar_map -> 'a) -> ('a -> 'b mm) -> 'b mm
  val ( and+ ) : 'a mm -> 'b mm -> ('a * 'b) mm
end

module Syntax : MEBI_MONAD_SYNTAX = struct
  let ( let+ ) x f = map f x
  let ( let* ) = bind
  let ( let$ ) f g = bind (state f) g
  let ( let$* ) f g = bind (state (fun e s -> f e s, ())) g
  let ( let$+ ) f g = bind (state (fun e s -> s, f e s)) g
  let ( and+ ) x y = product x y
end

(**********************************)
(****** UTILS *********************)
(**********************************)

let type_of_constrexpr (tref : Constrexpr.constr_expr) : term mm =
  let open Syntax in
  let* t : term = constrexpr_to_econstr tref in
  type_of_econstr t
;;

let rec econstr_list_to_constr ?(abort_on_undefined_evars : bool = false)
  : EConstr.t list -> Constr.t list mm
  = function
  | [] -> return []
  | h :: t ->
    let open Syntax in
    let* h = econstr_to_constr ~abort_on_undefined_evars h in
    let* t = econstr_list_to_constr ~abort_on_undefined_evars t in
    return (h :: t)
;;

let rec econstr_list_to_constr_opt ?(abort_on_undefined_evars : bool = false)
  : EConstr.t list -> Constr.t option list mm
  = function
  | [] -> return []
  | h :: t ->
    let open Syntax in
    let* h = econstr_to_constr_opt h in
    let* t = econstr_list_to_constr_opt t in
    return (h :: t)
;;

(**********************************)
(****** COQ TERM TO STRING ********)
(**********************************)

let constr_to_string (x : Constr.t) : string =
  let s_mm : string mm =
    let open Syntax in
    let* env = get_env in
    let* sigma = get_sigma in
    return (Utils.ppstr (Printer.pr_constr_env env sigma x))
  in
  string_mm s_mm
;;

let econstr_to_string (x : EConstr.t) : string =
  let s_mm : string mm =
    let open Syntax in
    let* env = get_env in
    let* sigma = get_sigma in
    return (Utils.ppstr (Printer.pr_econstr_env env sigma x))
  in
  string_mm s_mm
;;

let term_to_string (x : term) : string = econstr_to_string x

let constr_rel_decl_to_string (rd : Constr.rel_declaration) : string =
  let s_mm : string mm =
    let open Syntax in
    let* env = get_env in
    let* sigma = get_sigma in
    return (Utils.ppstr (Printer.pr_rel_decl env sigma rd))
  in
  string_mm s_mm
;;

let econstr_rel_decl_to_string (rd : EConstr.rel_declaration) : string =
  let s_mm : string mm =
    let open Syntax in
    let* env = get_env in
    let* sigma = get_sigma in
    return (Utils.ppstr (Printer.pr_erel_decl env sigma rd))
  in
  string_mm s_mm
;;

let constr_list_to_string (xs : Constr.t list) : string =
  match xs with
  | [] -> "[]"
  | h :: [] -> Printf.sprintf "[%s]" (constr_to_string h)
  | h :: t ->
    Printf.sprintf
      "[%s]"
      (List.fold_left
         (fun (acc : string) (x : Constr.t) ->
           Printf.sprintf "%s, %s" acc (constr_to_string x))
         (constr_to_string h)
         t)
;;

let constr_opt_list_to_string (xs : Constr.t option list) : string =
  match xs with
  | [] -> "[]"
  | None :: [] -> "[None]"
  | Some h :: [] -> Printf.sprintf "[%s]" (constr_to_string h)
  | h :: t ->
    Printf.sprintf
      "[%s]"
      (List.fold_left
         (fun (acc : string) (x : Constr.t option) ->
           Printf.sprintf
             "%s, %s"
             acc
             (match x with None -> "None" | Some x -> constr_to_string x))
         (match h with None -> "None" | Some h -> constr_to_string h)
         t)
;;

let econstr_list_to_string (xs : EConstr.t list) : string =
  match xs with
  | [] -> "[]"
  | h :: [] -> Printf.sprintf "[%s]" (econstr_to_string h)
  | h :: t ->
    Printf.sprintf
      "[%s]"
      (List.fold_left
         (fun (acc : string) (x : EConstr.t) ->
           Printf.sprintf "%s, %s" acc (econstr_to_string x))
         (econstr_to_string h)
         t)
;;

let constr_rel_decl_list_to_string (xs : Constr.rel_declaration list) : string =
  match xs with
  | [] -> "[]"
  | h :: [] -> Printf.sprintf "[%s]" (constr_rel_decl_to_string h)
  | h :: t ->
    Printf.sprintf
      "[%s]"
      (List.fold_left
         (fun (acc : string) (x : Constr.rel_declaration) ->
           Printf.sprintf "%s, %s" acc (constr_rel_decl_to_string x))
         (constr_rel_decl_to_string h)
         t)
;;

let econstr_rel_decl_list_to_string (xs : EConstr.rel_declaration list) : string
  =
  match xs with
  | [] -> "[]"
  | h :: [] -> Printf.sprintf "[%s]" (econstr_rel_decl_to_string h)
  | h :: t ->
    Printf.sprintf
      "[%s]"
      (List.fold_left
         (fun (acc : string) (x : EConstr.rel_declaration) ->
           Printf.sprintf "%s, %s" acc (econstr_rel_decl_to_string x))
         (econstr_rel_decl_to_string h)
         t)
;;

let econstr_list_to_constr_opt_string (es : EConstr.t list) : string mm =
  let open Syntax in
  let* es = econstr_list_to_constr_opt es in
  return (constr_opt_list_to_string es)
;;

(**********************************)
(****** DEBUG PRINTOUTS ***********)
(**********************************)

let debug (f : Environ.env -> Evd.evar_map -> Pp.t) : unit mm =
  state (fun env sigma ->
    Feedback.msg_debug (f env sigma);
    sigma, ())
;;

let show_proof_data () : unit mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  (match !coq_st.proofv.proof with
   | None -> Log.debug "mebi_wrapper.show_proof, proofv.proof is None"
   | Some proof ->
     let goals : Proofview.Goal.t Proofview.tactic list Proofview.tactic =
       Proofview.Goal.goals
     in
     let _goals = goals in
     let the_proof : Proof.t = Declare.Proof.get proof in
     let the_data = Proof.data the_proof in
     let goals_string = Utils.pstr_evar_list the_data.goals in
     let all_goals_string =
       Utils.pstr_evar_list (Evar.Set.to_list (Proof.all_goals the_proof))
     in
     let _partial_proof : EConstr.constr list =
       Proof.partial_proof the_proof
       (* Proofview.partial_proof the_data.entry *)
     in
     let _x = econstr_list_to_constr_opt_string _partial_proof in
     (* let _y = string_mm _x in *)
     let partial_proof_string =
       "TODO -- (obtaining string from wrapper causes \"Anomaly Uncaught \
        exception Not_found\")"
       (* _y *)
     in
     (* let _pv = match the_proof with | { proofview; focus_stack; entry; name; poly } -> proofview
    in *)
     let stack_string =
       if List.is_empty the_data.stack
       then "[ ] (empty)"
       else
         Printf.sprintf
           "[%s]"
           (List.fold_left
              (fun (acc : string) ((a, b) : Evar.t list * Evar.t list) ->
                Printf.sprintf
                  "%s\n%s%s,\n%s%s\n"
                  acc
                  (Utils.str_tabs 1)
                  (Utils.pstr_evar_list ~indent:2 a)
                  (Utils.str_tabs 1)
                  (Utils.pstr_evar_list ~indent:2 b))
              ""
              the_data.stack)
     in
     Log.debug
       (Printf.sprintf
          "mebi_wrapper.show_proof_data, name: %s\n\
           - is done: %b\n\
           - no focused goal: %b\n\
           - unfocused: %b\n\
           - goals: %s\n\
           - all goals: %s\n\
           - stack: %s\n\
           - partial proof: %s\n\
           - pr_proof: %s\n"
          (Names.Id.to_string the_data.name)
          (Proof.is_done the_proof)
          (Proof.no_focused_goal the_proof)
          (Proof.unfocused the_proof)
          goals_string
          all_goals_string
          stack_string
          partial_proof_string
          (Utils.ppstr (Proof.pr_proof the_proof))));
  (* *)
  { state = st; value = () }
;;

let show_proof () : unit mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  (match !coq_st.proofv.proof with
   | None -> Log.debug "mebi_wrapper.show_proof, proofv.proof is None"
   | Some proof ->
     let the_proof : Proof.t = Declare.Proof.get proof in
     let proof_string = Utils.ppstr (Proof.pr_proof the_proof) in
     Log.debug
       (Printf.sprintf
          "mebi_wrapper.show_proof, Proof.pr_proof: %s"
          proof_string));
  { state = st; value = () }
;;

let show_names () : unit mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  (match !coq_st.proofv.names with
   | None -> Log.debug "mebi_wrapper.show_names, proofv.names is None"
   | Some names ->
     Log.debug
       (Printf.sprintf
          "mebi_wrapper.show_names: %s"
          (if Names.Id.Set.is_empty names
           then "[ ] (empty)"
           else
             Printf.sprintf
               "[%s\n]"
               (Names.Id.Set.fold
                  (fun (n : Names.Id.t) (acc : string) ->
                    Printf.sprintf
                      "%s\n\t%s : %s"
                      acc
                      (Names.Id.to_string n)
                      (Utils.ppstr (Names.Id.print n)))
                  names
                  ""))));
  { state = st; value = () }
;;

(********************************************)
(****** ERRORS ******************************)
(********************************************)

module type ERROR_TYPE = sig
  type mebi_error =
    | ProofvIsNone of unit
    | ParamsFailIfIncomplete of unit
    | ParamsFailIfNotBisim of unit
    | InvalidLTSArgsLength of int
    | InvalidLTSTermKind of Environ.env * Evd.evar_map * Constr.t
    | InvalidLTSSort of Sorts.family
    | InvalidTypeSort of Sorts.family
    | InvalidArity of Environ.env * Evd.evar_map * Constr.types
    | InvalidRefLTS of Names.GlobRef.t
    | InvalidRefType of Names.GlobRef.t
    | UnknownTermType of
        (Environ.env * Evd.evar_map * (term * term * term list))
    | PrimaryLTSNotFound of (Environ.env * Evd.evar_map * term * term list)
    | UnknownDecodeKey of (Environ.env * Evd.evar_map * E.t * term B.t)
    | ExpectedCoqIndDefOfLTSNotType of unit
    | InvalidCheckUpdatedCtx of
        (Environ.env
        * Evd.evar_map
        * EConstr.t list
        * EConstr.rel_declaration list)

  exception MEBI_exn of mebi_error

  val proofv_is_none : unit -> exn
  val params_fail_if_incomplete : unit -> exn
  val params_fail_if_not_bisim : unit -> exn
  val invalid_lts_args_length : int -> exn
  val invalid_lts_term_kind : Environ.env -> Evd.evar_map -> Constr.t -> exn
  val invalid_sort_lts : Sorts.family -> exn
  val invalid_sort_type : Sorts.family -> exn
  val invalid_arity : Environ.env -> Evd.evar_map -> Constr.types -> exn
  val invalid_ref_lts : Names.GlobRef.t -> exn
  val invalid_ref_type : Names.GlobRef.t -> exn
  val invalid_cindef_kind : unit -> exn

  val unknown_term_type
    :  Environ.env
    -> Evd.evar_map
    -> term * term * term list
    -> exn

  val primary_lts_not_found
    :  Environ.env
    -> Evd.evar_map
    -> term
    -> term list
    -> exn

  val unknown_decode_key : Environ.env -> Evd.evar_map -> E.t -> term B.t -> exn

  val invalid_check_updated_ctx
    :  Environ.env
    -> Evd.evar_map
    -> EConstr.t list
    -> EConstr.rel_declaration list
    -> exn
end

module Error : ERROR_TYPE = struct
  type mebi_error =
    | ProofvIsNone of unit
    | ParamsFailIfIncomplete of unit
    | ParamsFailIfNotBisim of unit
    | InvalidLTSArgsLength of int
    | InvalidLTSTermKind of Environ.env * Evd.evar_map * Constr.t
    | InvalidLTSSort of Sorts.family
    | InvalidTypeSort of Sorts.family
    | InvalidArity of Environ.env * Evd.evar_map * Constr.types
    | InvalidRefLTS of Names.GlobRef.t
    | InvalidRefType of Names.GlobRef.t
    | UnknownTermType of
        (Environ.env * Evd.evar_map * (term * term * term list))
    | PrimaryLTSNotFound of (Environ.env * Evd.evar_map * term * term list)
    | UnknownDecodeKey of (Environ.env * Evd.evar_map * E.t * term B.t)
    | ExpectedCoqIndDefOfLTSNotType of unit
    | InvalidCheckUpdatedCtx of
        (Environ.env
        * Evd.evar_map
        * EConstr.t list
        * EConstr.rel_declaration list)

  exception MEBI_exn of mebi_error

  let proofv_is_none () = MEBI_exn (ProofvIsNone ())
  let params_fail_if_incomplete () = MEBI_exn (ParamsFailIfIncomplete ())
  let params_fail_if_not_bisim () = MEBI_exn (ParamsFailIfNotBisim ())

  (** Assert args length == 3 in [Command.extract_args]. *)
  let invalid_lts_args_length i = MEBI_exn (InvalidLTSArgsLength i)

  (** Assert Constr.kind tm is App _ in [Command.extract_args]. *)
  let invalid_lts_term_kind ev sg x = MEBI_exn (InvalidLTSTermKind (ev, sg, x))

  (** Error when input LTS has the wrong arity *)
  let invalid_sort_lts f = MEBI_exn (InvalidLTSSort f)

  (** Error when input Type has the wrong arity *)
  let invalid_sort_type f = MEBI_exn (InvalidTypeSort f)

  (** Error when input LTS has the wrong Sort *)
  let invalid_arity ev sg t = MEBI_exn (InvalidArity (ev, sg, t))

  (** Error when input LTS reference is invalid (e.g. non existing) *)
  let invalid_ref_lts r = MEBI_exn (InvalidRefLTS r)

  let invalid_ref_type r = MEBI_exn (InvalidRefType r)

  (** Error when input LTS reference is invalid (e.g. non existing) *)
  let invalid_cindef_kind () = MEBI_exn (ExpectedCoqIndDefOfLTSNotType ())

  (** Error when term is of unknown type *)
  let unknown_term_type ev sg tmty = MEBI_exn (UnknownTermType (ev, sg, tmty))

  (** Error when multiple coq-LTS provided, but none of them match term. *)
  let primary_lts_not_found ev sg t names =
    MEBI_exn (PrimaryLTSNotFound (ev, sg, t, names))
  ;;

  (** Error when multiple coq-LTS provided, but none of them match term. *)
  let unknown_decode_key ev sg k bckmap =
    MEBI_exn (UnknownDecodeKey (ev, sg, k, bckmap))
  ;;

  let invalid_check_updated_ctx ev sg x y =
    MEBI_exn (InvalidCheckUpdatedCtx (ev, sg, x, y))
  ;;

  open Pp

  let mebi_handler = function
    | ProofvIsNone () -> str "Tried to access contents of proofv which is None."
    | ParamsFailIfIncomplete () ->
      str
        "Params are configured to fail if cannot construct complete LTS from \
         term.\n\n\
         Use command \"MeBi Set FailIfIncomplete False\" to disable this \
         behaviour."
    | ParamsFailIfNotBisim () ->
      str
        "Params are configured to fail if terms not bisim.\n\n\
         Use command \"MeBi Set FailIfNotBisim False\" to disable this \
         behaviour."
    | ExpectedCoqIndDefOfLTSNotType () ->
      str
        "cindef (Coq Inductive Definition) of LTS was expected, but Type was \
         used."
    | InvalidLTSArgsLength i ->
      str
        (Printf.sprintf
           "Command.extract_args, assertion: Array.length args == 3 failed. \
            Got %i"
           i)
    | InvalidLTSTermKind (ev, sg, tm) ->
      str
        "Command.extract_args, assertion: Constr.kind tm matches App _ failed. \
         Got "
      ++ Printer.pr_constr_env ev sg tm
      ++ str " which matches with "
      ++ str
           (match Constr.kind tm with
            | Rel _ -> "Rel"
            | Var _ -> "Var"
            | Meta _ -> "Meta"
            | Evar _ -> "EVar"
            | Sort _ -> "Sort"
            | Cast _ -> "Cast"
            | Prod _ -> "Prod"
            | Lambda _ -> "Lambda"
            | LetIn _ -> "LetIn"
            | App _ -> "App"
            | Const _ -> "Const"
            | Ind _ -> "Ind"
            | Construct _ -> "Construct"
            | Case _ -> "Case"
            | Fix _ -> "Fix"
            | CoFix _ -> "CoFix"
            | Proj _ -> "Proj"
            | Int _ -> "Int"
            | Float _ -> "Float"
            | String _ -> "String"
            | Array _ -> "Array")
      ++ str "."
    | InvalidCheckUpdatedCtx (ev, sg, x, y) ->
      str
        "Invalid Args to check_updated_ctx. Should both be empty, or both have \
         some."
      ++ strbrk "\n"
      ++ str (Printf.sprintf "substls: %s." (econstr_list_to_string x))
      ++ strbrk "\n"
      ++ str (Printf.sprintf "ctx_tys: %s." (econstr_rel_decl_list_to_string y))
    | InvalidLTSSort f ->
      str "Invalid LTS Sort: expecting Prop, got " ++ Sorts.pr_sort_family f
    | InvalidTypeSort f ->
      str "Invalid Type Sort: expecting Type or Set, got "
      ++ Sorts.pr_sort_family f
    | InvalidArity (ev, sg, t) ->
      str "Invalid arity for LTS: "
      ++ Printer.pr_constr_env ev sg t
      ++ strbrk "\n"
      ++ str "Expecting: forall params, ?terms -> ?labels -> ?terms -> Prop"
    | InvalidRefLTS r -> str "Invalid ref LTS: " ++ Printer.pr_global r
    | InvalidRefType r -> str "Invalid ref Type: " ++ Printer.pr_global r
    | UnknownTermType (ev, sg, (tm, ty, trkeys)) ->
      str
        "None of the constructors provided matched type of term to visit. \
         (unknown_term_type) "
      ++ strbrk "\n\n"
      ++ str "Term: "
      ++ Printer.pr_econstr_env ev sg tm
      ++ strbrk "\n\n"
      ++ str "Type: "
      ++ Printer.pr_econstr_env ev sg ty
      ++ strbrk "\n\n"
      ++ str (Printf.sprintf "Keys: %s" (econstr_list_to_string trkeys))
      ++ strbrk "\n\n"
      ++ str
           (Printf.sprintf
              "Does Type match EConstr of any Key? = %b"
              (List.exists (fun (k : term) -> EConstr.eq_constr sg ty k) trkeys))
      ++ strbrk "\n"
      ++ str
           (let tystr = Utils.ppstr (Printer.pr_econstr_env ev sg ty) in
            Printf.sprintf
              "Does Type match String of any Key? = %b"
              (List.exists
                 (fun (k : term) ->
                   String.equal
                     tystr
                     (Utils.ppstr (Printer.pr_econstr_env ev sg k)))
                 trkeys))
    | PrimaryLTSNotFound (ev, sg, t, names) ->
      str "Primary LTS Not found for term: "
      ++ Printer.pr_econstr_env ev sg t
      ++ strbrk "\n\n"
      ++ str "constructor names: "
      ++ List.fold_left
           (fun (acc : Pp.t) (name : EConstr.t) ->
             acc ++ strbrk "\n\n" ++ Printer.pr_econstr_env ev sg name)
           (Printer.pr_econstr_env ev sg (List.hd names))
           (List.tl names)
    | UnknownDecodeKey (ev, sg, k, bckmap) ->
      str "Unknown decode key: "
      ++ str (E.to_string k)
      ++ strbrk "\n\n"
      ++ str "Decode map: ["
      ++
      if Int.equal (B.length bckmap) 0
      then str " ] (empty)"
      else
        B.fold
          (fun (t : E.t) (v : term) (acc : Pp.t) ->
            acc
            ++ strbrk "\n\n"
            ++ str (E.to_string t)
            ++ str " => "
            ++ Printer.pr_econstr_env ev sg v)
          bckmap
          (str "")
        ++ str " ]"
  ;;

  let _ =
    CErrors.register_handler (fun e ->
      match e with MEBI_exn e -> Some (mebi_handler e) | _ -> None)
  ;;
end

(**********************************)
(****** ERROR FUNCTIONS ***********)
(**********************************)

let proofv_is_none () : 'a mm =
  fun (st : wrapper ref) -> raise (Error.proofv_is_none ())
;;

let params_fail_if_incomplete () : 'a mm =
  fun (st : wrapper ref) -> raise (Error.params_fail_if_incomplete ())
;;

let params_fail_if_not_bisim () : 'a mm =
  fun (st : wrapper ref) -> raise (Error.params_fail_if_not_bisim ())
;;

let invalid_check_updated_ctx x y : 'a mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  raise (Error.invalid_check_updated_ctx !coq_st.coq_env !coq_st.coq_ctx x y)
;;

let invalid_lts_args_length (x : int) : 'a mm =
  fun (st : wrapper ref) -> raise (Error.invalid_lts_args_length x)
;;

let invalid_lts_term_kind (x : Constr.t) : 'a mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  raise (Error.invalid_lts_term_kind !coq_st.coq_env !coq_st.coq_ctx x)
;;

(** Error when input LTS has the wrong arity *)
let invalid_arity (x : Constr.types) : 'a mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  raise (Error.invalid_arity !coq_st.coq_env !coq_st.coq_ctx x)
;;

(** Error when input LTS has the wrong sort *)
let invalid_sort_lts (x : Sorts.family) : 'a mm =
  fun (st : wrapper ref) -> raise (Error.invalid_sort_lts x)
;;

(** Error when input Type has the wrong sort *)
let invalid_sort_type (x : Sorts.family) : 'a mm =
  fun (st : wrapper ref) -> raise (Error.invalid_sort_type x)
;;

(** Error when input LTS reference is invalid (e.g. non existing) *)
let invalid_ref_lts (x : Names.GlobRef.t) : 'a mm =
  fun (st : wrapper ref) -> raise (Error.invalid_ref_lts x)
;;

(** Error when input Type reference is invalid (e.g. non existing) *)
let invalid_ref_type (x : Names.GlobRef.t) : 'a mm =
  fun (st : wrapper ref) -> raise (Error.invalid_ref_type x)
;;

(** Error when input LTS reference is invalid (e.g. non existing) *)
let invalid_cindef_kind unit : 'a mm =
  fun (st : wrapper ref) -> raise (Error.invalid_cindef_kind ())
;;

(** Error when term is of unknown type *)
let unknown_term_type (tmty : term * term * term list) : 'a mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  raise (Error.unknown_term_type !coq_st.coq_env !coq_st.coq_ctx tmty)
;;

(** Error when multiple coq-LTS provided, but none of them match term. *)
let primary_lts_not_found ((t, names) : term * term list) : 'a mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  raise (Error.primary_lts_not_found !coq_st.coq_env !coq_st.coq_ctx t names)
;;

(** Error when try to decode key that does not exist in decode map. *)
let unknown_decode_key ((k, bckmap) : E.t * term B.t) : 'a mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  raise (Error.unknown_decode_key !coq_st.coq_env !coq_st.coq_ctx k bckmap)
;;

(********************************************)
(****** ENCODE/DECODE ***********************)
(********************************************)

let encode (k : term) : E.t mm =
  fun (st : wrapper ref) ->
  let encoding : E.t = E.encode !st.fwd_enc !st.bck_enc k in
  Logging.Log.debug
    (Printf.sprintf
       "mebi_wrapper.encode, \"%s\" into (%s)"
       (econstr_to_string k)
       (E.to_string encoding));
  assert (F.mem !st.fwd_enc k);
  assert (B.mem !st.bck_enc encoding);
  { state = st; value = encoding }
;;

(** dual to [encode] except we cannot handle new values *)
let decode (k : E.t) : term mm =
  fun (st : wrapper ref) ->
  match E.decode_opt !st.bck_enc k with
  | Some decoding ->
    Logging.Log.debug
      (Printf.sprintf
         "mebi_wrapper.decode, \"%s\" into (%s)"
         (E.to_string k)
         (econstr_to_string decoding));
    { state = st; value = decoding }
  | None ->
    let coq_st = !st.coq_ref in
    raise
      (Error.unknown_decode_key !coq_st.coq_env !coq_st.coq_ctx k !st.bck_enc)
;;

let decode_to_string (x : E.t) : string =
  let s_mm : string mm =
    let open Syntax in
    let* y = decode x in
    let* env = get_env in
    let* sigma = get_sigma in
    return (Utils.ppstr (Printer.pr_econstr_env env sigma y))
  in
  string_mm s_mm
;;

(********************************************)
(****** ENCODE/DECODE OPT *******************)
(********************************************)

let get_encoding_opt (k : term) : E.t option mm =
  fun (st : wrapper ref) ->
  match F.find_opt !st.fwd_enc k with
  | None -> { state = st; value = None }
  | Some e -> { state = st; value = Some e }
;;

(** dual to [encode] except we cannot handle new values *)
let get_decoding_opt (k : E.t) : term option mm =
  fun (st : wrapper ref) ->
  match B.find_opt !st.bck_enc k with
  | None -> { state = st; value = None }
  | Some e -> { state = st; value = Some e }
;;

(********************************************)
(****** ENCODE/DECODE CHECKs ****************)
(********************************************)

let has_encoding (k : term) : bool mm =
  fun (st : wrapper ref) ->
  match F.find_opt !st.fwd_enc k with
  | None -> { state = st; value = false }
  | Some _ -> { state = st; value = true }
;;

(** dual to [encode] except we cannot handle new values *)
let has_decoding (k : E.t) : bool mm =
  fun (st : wrapper ref) ->
  match B.find_opt !st.bck_enc k with
  | None -> { state = st; value = false }
  | Some _ -> { state = st; value = true }
;;

(**********************************)
(****** ENCODE/DECODE MAPs ********)
(**********************************)

let encode_map (m : 'a F.t) : 'a B.t mm =
  fun (st : wrapper ref) ->
  let encoded_map : 'a B.t = B.create (F.length m) in
  F.iter
    (fun (k : term) (v : 'a) ->
      let encoding : E.t = E.encode !st.fwd_enc !st.bck_enc k in
      B.add encoded_map encoding v)
    m;
  { state = st; value = encoded_map }
;;

(** *)
let decode_map (m : 'a B.t) : 'a F.t mm =
  fun (st : wrapper ref) ->
  let decoded_map : 'a F.t = F.create (B.length m) in
  B.iter
    (fun (k : E.t) (v : 'a) ->
      let decoding : term = E.decode !st.bck_enc k in
      F.add decoded_map decoding v)
    m;
  { state = st; value = decoded_map }
;;

(********************************************)
(****** COQ CONSTR TREE *********************)
(********************************************)

(* TODO: generalize this, and use a functor to map from E.g., [E.t*int tree] to
   [string*int tree]. *)

module Constr_tree = struct
  type 'a tree = Node of 'a * 'a tree list
  type t = (E.t * int) tree

  let eq (t1 : t) (t2 : t) : bool =
    let rec tree_eq (t1 : t) (t2 : t) : bool =
      match t1, t2 with
      | Node (a1, b1), Node (a2, b2) ->
        fst a1 == fst a2 && snd a1 == snd a2 && tree_list_eq b1 b2
    and tree_list_eq (l1 : t list) (l2 : t list) : bool =
      match l1, l2 with
      | [], [] -> true
      | h1 :: t1, h2 :: t2 -> tree_eq h1 h2 && tree_list_eq t1 t2
      | [], _ :: _ -> false
      | _ :: _, [] -> false
    in
    tree_eq t1 t2
  ;;

  let compare (t1 : t) (t2 : t) : int =
    let rec tree_compare (t1 : t) (t2 : t) : int =
      match t1, t2 with
      | Node (i1, l1), Node (i2, l2) ->
        (match E.compare (fst i1) (fst i2) with
         | 0 ->
           (match Int.compare (snd i1) (snd i2) with
            | 0 -> tree_list_compare l1 l2
            | n -> n)
         | n -> n)
    and tree_list_compare (l1 : t list) (l2 : t list) : int =
      match l1, l2 with
      | [], [] -> 0
      | h1 :: t1, h2 :: t2 ->
        (match tree_compare h1 h2 with
         | 0 -> tree_list_compare t1 t2 (* these should always be empty *)
         | n -> n (* prioritise the main node when comparing *))
      | [], _ :: _ -> -1
      | _ :: _, [] -> 1
    in
    tree_compare t1 t2
  ;;

  let rec pstr (t1 : t) : string =
    match t1 with
    | Node (lhs_int, rhs_int_tree_list) ->
      Printf.sprintf
        "(%s:%i) [%s]"
        (E.to_string (fst lhs_int))
        (snd lhs_int)
        (match List.length rhs_int_tree_list with
         | 0 -> ""
         | 1 -> pstr (List.hd rhs_int_tree_list)
         | _ ->
           List.fold_left
             (fun (acc : string) (rhs_int_tree : t) ->
               Printf.sprintf "%s, %s" acc (pstr rhs_int_tree))
             (pstr (List.hd rhs_int_tree_list))
             (List.tl rhs_int_tree_list))
  ;;
end

(**********************************)
(****** DECODE CONSTR TREE ********)
(**********************************)

type decoded_tree = (string * int) Constr_tree.tree

(** decodes the parts of the tree corresponding to the LTS into string form. *)
let decode_constr_tree_lts (tree : Constr_tree.t) : decoded_tree mm =
  let open Syntax in
  let rec decode_tree (t : Constr_tree.t) : decoded_tree mm =
    match t with
    | Node (leaf, stem) ->
      let* decoded_leaf_lts : term = decode (fst leaf) in
      let decoded_leaf = econstr_to_string decoded_leaf_lts, snd leaf in
      let* decoded_stem = decode_tree_list stem in
      return (Constr_tree.Node (decoded_leaf, decoded_stem))
  and decode_tree_list (l : Constr_tree.t list) : decoded_tree list mm =
    match l with
    | [] -> return []
    | h :: t ->
      let* decoded_h = decode_tree h in
      let* decoded_l = decode_tree_list t in
      return (decoded_h :: decoded_l)
  in
  decode_tree tree
;;

let rec pstr_decoded_tree (t1 : decoded_tree) : string =
  match t1 with
  | Node (lhs_int, rhs_int_tree_list) ->
    Printf.sprintf
      "(%s:%i) [%s]"
      (fst lhs_int)
      (snd lhs_int)
      (match List.length rhs_int_tree_list with
       | 0 -> ""
       | 1 -> pstr_decoded_tree (List.hd rhs_int_tree_list)
       | _ ->
         List.fold_left
           (fun (acc : string) (rhs_int_tree : decoded_tree) ->
             Printf.sprintf "%s, %s" acc (pstr_decoded_tree rhs_int_tree))
           (pstr_decoded_tree (List.hd rhs_int_tree_list))
           (List.tl rhs_int_tree_list))
;;

(**********************************)
(****** COQ NONE TYPE *************)
(**********************************)

let the_none_ref () : Names.GlobRef.t = Coqlib.lib_ref "core.option.None"

let the_none_term () : term mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  let sigma, the_none =
    Evd.fresh_global !coq_st.coq_env !coq_st.coq_ctx (the_none_ref ())
  in
  coq_st := { !coq_st with coq_ctx = sigma };
  { state = st; value = the_none }
;;

let is_none_term (t : term) : bool mm =
  let open Syntax in
  let* t : Constr.t = term_to_constr t in
  match Constr.kind t with
  | App (t, _) ->
    let* none : term = the_none_term () in
    let* n : Constr.t = term_to_constr none in
    return (Constr.equal n t)
  | _ -> return false
;;

(**********************************)
(****** COQ PROOF THEORIES ********)
(**********************************)
(* source: https://github.com/rocq-prover/rocq/blob/master/doc/plugin_tutorial/tuto3/src/tuto_tactic.ml *)

(* In the environment of the goal, we can get the type of an assumption
   directly by a lookup.  The other solution is to call a low-cost retyping
   function like *)
let get_type_of_hyp (id : Names.Id.t) : EConstr.t mm =
  let open Syntax in
  let* env = get_env in
  match EConstr.lookup_named id env with
  | Context.Named.Declaration.LocalAssum (_pbinder_annot, ty) ->
    Log.debug
      (Printf.sprintf
         "mebi_wrapper.get_type_of_hyp, LocalAssum: %s"
         (econstr_to_string ty));
    return ty
  | Context.Named.Declaration.LocalDef (_pbinder_annot, _constr, ty) ->
    Log.debug
      (Printf.sprintf
         "mebi_wrapper.get_type_of_hyp, LocalDef: %s := %s"
         (econstr_to_string ty)
         (econstr_to_string _constr));
    return ty
;;

(**********************************)
(****** COQ PROOF CONTEXT *********)
(**********************************)

let get_proof () : Declare.Proof.t mm =
  Log.trace "mebi_wrapper.get_proof";
  let open Syntax in
  let* proofv : proof_context = get_proofv in
  match proofv.proof with
  | None -> proofv_is_none ()
  | Some proof -> return proof
;;

let get_proof_names () : Names.Id.Set.t mm =
  Log.trace "mebi_wrapper.get_proof_names";
  let open Syntax in
  let* proofv : proof_context = get_proofv in
  match proofv.names with
  | None -> return Names.Id.Set.empty
  | Some names -> return names
;;

let update_names
      ?(replace : bool = false)
      (new_names : Names.Id.Set.t)
      (st : wrapper ref)
  : unit in_context
  =
  let coq_st = !st.coq_ref in
  match !coq_st.proofv.names with
  | None ->
    Log.warning "mebi_wrapper.update_names, proofv.names is None (create new)";
    !coq_st.proofv.names <- Some new_names;
    { state = st; value = () }
  | Some names ->
    !coq_st.proofv.names
    <- Some (if replace then new_names else Names.Id.Set.union names new_names);
    { state = st; value = () }
;;

let add_name (name : Names.Id.t) (st : wrapper ref) : unit in_context =
  let coq_st = !st.coq_ref in
  match !coq_st.proofv.names with
  | None ->
    Log.warning "mebi_wrapper.add_name, proofv.names is None (create new)";
    !coq_st.proofv.names <- Some (Names.Id.Set.singleton name);
    { state = st; value = () }
  | Some names ->
    !coq_st.proofv.names <- Some (Names.Id.Set.add name names);
    { state = st; value = () }
;;

(**********************************)
(****** COQ PROOF NAMES ***********)
(**********************************)

let next_name_of (n : Names.Id.t) : Names.Id.t mm =
  let open Syntax in
  let* names = get_proof_names () in
  return (Namegen.next_ident_away n names)
;;

let new_name_of_string ?(add : bool = true) (s : string) : Names.Id.t mm =
  Log.trace "mebi_wrapper.new_name_of_string";
  let open Syntax in
  let* name = next_name_of (Names.Id.of_string s) in
  let* _ = show_names () in
  let* _ = if add then add_name name else return () in
  let* _ = show_names () in
  return name
;;

(**********************************)
(****** COQ PROOF TACTICS *********)
(**********************************)

let update_proof_by_tactic (t : unit Proofview.tactic) : unit mm =
  Log.trace "mebi_wrapper.update_proof_by_tactic";
  let open Syntax in
  let* the_proof : Declare.Proof.t = get_proof () in
  let new_proof, is_safe_tactic = Declare.Proof.by t the_proof in
  if Bool.not is_safe_tactic
  then Log.warning "mebi_wrapper.update_proof_by_tactic, unsafe tactic used";
  set_proof new_proof
;;

let update_proof_by_tactic_mm (t : unit Proofview.tactic mm) : unit mm =
  let open Syntax in
  let* t = t in
  update_proof_by_tactic t
;;

let rec update_proof_by_tactics : unit Proofview.tactic list -> unit mm
  = function
  | [] -> return ()
  | h :: t ->
    let open Syntax in
    let* _ = update_proof_by_tactic h in
    update_proof_by_tactics t
;;

(* NOTE: same as above, but for list of elems wrapped in [mm] *)
let rec update_proof_by_tactics_mm : unit Proofview.tactic mm list -> unit mm
  = function
  | [] -> return ()
  | h :: t ->
    let open Syntax in
    let* _ = update_proof_by_tactic_mm h in
    update_proof_by_tactics_mm t
;;

(* TODO: [Declare.Proof.by tac proofv.proof] *)
(* TODO: [Declare.Proof.get proofv.proof -> Proof.] *)

(* TODO: [Proofview.tactic?] swap between them all *)

(********************************************)
(****** GRAPH *******************************)
(********************************************)

let make_transition_tbl (st : wrapper ref)
  : (module Hashtbl.S with type key = E.t) in_context
  =
  let eqf = E.eq in
  let hashf = E.hash in
  let module TransitionTbl =
    Hashtbl.Make (struct
      type t = E.t

      let equal t1 t2 = eqf t1 t2
      let hash t = hashf t
    end)
  in
  { state = st; value = (module TransitionTbl : Hashtbl.S with type key = E.t) }
;;

let make_state_set (st : wrapper ref)
  : (module Set.S with type elt = E.t) in_context
  =
  let comparef = E.compare in
  let module StateSet =
    Set.Make (struct
      type t = E.t

      let compare t1 t2 = comparef t1 t2
    end)
  in
  { state = st; value = (module StateSet : Set.S with type elt = E.t) }
;;

let make_state_tree_pair_set (st : wrapper ref)
  : (module Set.S with type elt = E.t * Constr_tree.t) in_context
  =
  let module PairSet =
    Set.Make (struct
      type t = E.t * Constr_tree.t

      let compare t1 t2 =
        match E.compare (fst t1) (fst t2) with
        | 0 -> Constr_tree.compare (snd t1) (snd t2)
        | c -> c
      ;;
    end)
  in
  { state = st
  ; value = (module PairSet : Set.S with type elt = E.t * Constr_tree.t)
  }
;;

(********************************************)
(****** DEBUG *******************************)
(********************************************)

let debug_encoding () : unit mm =
  fun (st : wrapper ref) ->
  if Int.equal 0 (F.length !st.fwd_enc)
  then (
    Logging.Log.debug "mebi_wrapper.debug_encoding, fwd encoding is empty";
    if Int.equal 0 (B.length !st.bck_enc)
    then Logging.Log.debug "mebi_wrapper.debug_encoding, bck encoding is empty"
    else
      B.iter
        (fun (enc : E.t) (t : term) ->
          Logging.Log.debug
            (Printf.sprintf
               "(%s) => %s "
               (E.to_string enc)
               (econstr_to_string t)))
        !st.bck_enc)
  else
    F.iter
      (fun (t : term) (enc : E.t) ->
        Logging.Log.debug
          (Printf.sprintf "(%s) => %s " (E.to_string enc) (econstr_to_string t)))
      !st.fwd_enc;
  { state = st; value = () }
;;

let debug_econstr_kind (t : EConstr.t) : unit mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  Log.debug
    (Printf.sprintf
       "mebi_wrapper.debug_econstr_kind:\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n"
       (Printf.sprintf
          "isRel %s = %b"
          (econstr_to_string t)
          (EConstr.isRel !coq_st.coq_ctx t))
       (Printf.sprintf
          "isVar %s = %b"
          (econstr_to_string t)
          (EConstr.isVar !coq_st.coq_ctx t))
       (Printf.sprintf
          "isInd %s = %b"
          (econstr_to_string t)
          (EConstr.isInd !coq_st.coq_ctx t))
       (Printf.sprintf
          "isRef %s = %b"
          (econstr_to_string t)
          (EConstr.isRef !coq_st.coq_ctx t))
       (Printf.sprintf
          "isEvar %s = %b"
          (econstr_to_string t)
          (EConstr.isEvar !coq_st.coq_ctx t))
       (Printf.sprintf
          "isMeta %s = %b"
          (econstr_to_string t)
          (EConstr.isMeta !coq_st.coq_ctx t))
       (Printf.sprintf
          "isSort %s = %b"
          (econstr_to_string t)
          (EConstr.isSort !coq_st.coq_ctx t))
       (Printf.sprintf
          "isCast %s = %b"
          (econstr_to_string t)
          (EConstr.isCast !coq_st.coq_ctx t))
       (Printf.sprintf
          "isApp %s = %b"
          (econstr_to_string t)
          (EConstr.isApp !coq_st.coq_ctx t))
       (Printf.sprintf
          "isLambda %s = %b"
          (econstr_to_string t)
          (EConstr.isLambda !coq_st.coq_ctx t))
       (Printf.sprintf
          "isLetIn %s = %b"
          (econstr_to_string t)
          (EConstr.isLetIn !coq_st.coq_ctx t))
       (Printf.sprintf
          "isProd %s = %b"
          (econstr_to_string t)
          (EConstr.isProd !coq_st.coq_ctx t))
       (Printf.sprintf
          "isConst %s = %b"
          (econstr_to_string t)
          (EConstr.isConst !coq_st.coq_ctx t))
       (Printf.sprintf
          "isConstruct %s = %b"
          (econstr_to_string t)
          (EConstr.isConstruct !coq_st.coq_ctx t))
       (Printf.sprintf
          "isFix %s = %b"
          (econstr_to_string t)
          (EConstr.isFix !coq_st.coq_ctx t))
       (Printf.sprintf
          "isCoFix %s = %b"
          (econstr_to_string t)
          (EConstr.isCoFix !coq_st.coq_ctx t))
       (Printf.sprintf
          "isCase %s = %b"
          (econstr_to_string t)
          (EConstr.isCase !coq_st.coq_ctx t))
       (Printf.sprintf
          "isProj %s = %b"
          (econstr_to_string t)
          (EConstr.isProj !coq_st.coq_ctx t))
       (Printf.sprintf
          "isType %s = %b"
          (econstr_to_string t)
          (EConstr.isType !coq_st.coq_ctx t)));
  { state = st; value = () }
;;

let debug_term_kind (t : term) : unit mm = debug_econstr_kind t

let debug_constr_kind (t : Constr.t) : unit mm =
  fun (st : wrapper ref) ->
  Log.debug
    (Printf.sprintf
       "mebi_wrapper.debug_constr_kind:\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n\
       \ - %s\n"
       (Printf.sprintf "isRel %s = %b" (constr_to_string t) (Constr.isRel t))
       (Printf.sprintf "isVar %s = %b" (constr_to_string t) (Constr.isVar t))
       (Printf.sprintf "isInd %s = %b" (constr_to_string t) (Constr.isInd t))
       (Printf.sprintf "isRef %s = %b" (constr_to_string t) (Constr.isRef t))
       (Printf.sprintf "isEvar %s = %b" (constr_to_string t) (Constr.isEvar t))
       (Printf.sprintf "isMeta %s = %b" (constr_to_string t) (Constr.isMeta t))
       (Printf.sprintf "isSort %s = %b" (constr_to_string t) (Constr.isSort t))
       (Printf.sprintf "isCast %s = %b" (constr_to_string t) (Constr.isCast t))
       (Printf.sprintf "isApp %s = %b" (constr_to_string t) (Constr.isApp t))
       (Printf.sprintf
          "isLambda %s = %b"
          (constr_to_string t)
          (Constr.isLambda t))
       (Printf.sprintf
          "isLetIn %s = %b"
          (constr_to_string t)
          (Constr.isLetIn t))
       (Printf.sprintf "isProd %s = %b" (constr_to_string t) (Constr.isProd t))
       (Printf.sprintf
          "isConst %s = %b"
          (constr_to_string t)
          (Constr.isConst t))
       (Printf.sprintf
          "isConstruct %s = %b"
          (constr_to_string t)
          (Constr.isConstruct t))
       (Printf.sprintf "isFix %s = %b" (constr_to_string t) (Constr.isFix t))
       (Printf.sprintf
          "isCoFix %s = %b"
          (constr_to_string t)
          (Constr.isCoFix t))
       (Printf.sprintf "isCase %s = %b" (constr_to_string t) (Constr.isCase t))
       (Printf.sprintf "isProj %s = %b" (constr_to_string t) (Constr.isProj t))
       (Printf.sprintf
          "is_Prop %s = %b"
          (constr_to_string t)
          (Constr.is_Prop t))
       (Printf.sprintf
          "is_Type %s = %b"
          (constr_to_string t)
          (Constr.is_Type t))
       (Printf.sprintf "is_Set %s = %b" (constr_to_string t) (Constr.is_Set t)));
  { state = st; value = () }
;;

let debug_term_constr_kind (t : term) : unit mm =
  let open Syntax in
  let* t = term_to_constr t in
  debug_constr_kind t
;;

(****************************************************************************)

let proof_query (pstate : Declare.Proof.t) : Proof.t = Declare.Proof.get pstate
let proof_partial (p : Proof.t) : EConstr.t list = Proof.partial_proof p

let proof_test () : unit Proofview.tactic mm =
  fun (st : wrapper ref) ->
  Log.trace "mebi_wrapper.proof_test";
  let _h_hyps_id = Names.Id.of_string "TestPacked" in
  (* *)
  { state = st
  ; value =
      Proofview.Goal.enter (fun gl ->
        let _hyps = Environ.named_context_val (Proofview.Goal.env gl) in
        Proofview.tclUNIT ())
      (* let x = Proofview.Goal.goal gl in

         if Termops.mem_named_context_val h_hyps_id hyps then
         Proofview.tclTHEN (repackage i h_hyps_id)
         (Proofview.tclTHEN (Tactics.clear [h_hyps_id; i])
         (Tactics.introduction h_hyps_id))
         else
         Proofview.tclTHEN (package i)
         (Proofview.tclTHEN (Tactics.rename_hyp [i, h_hyps_id])
         (Tactics.move_hyp h_hyps_id Logic.MoveLast)) *)
  }
;;
