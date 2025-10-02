open Logging

type term = EConstr.t

let enable_logging : bool ref = ref true

(********************************************)
(****** COQ ENVIRONMENT/CONTEXT *************)
(********************************************)

type proof_context =
  { mutable proof : Declare.Proof.t
  ; mutable names : Names.Id.Set.t
  }

type coq_context =
  { coq_env : Environ.env
  ; coq_ctx : Evd.evar_map
  ; proofv : proof_context option
  }

(* *)
let the_proofv_opt : proof_context option ref option ref = ref None

let new_proofv
      ?(names : Names.Id.Set.t = Names.Id.Set.empty)
      (proof : Declare.Proof.t)
  : proof_context option ref
  =
  Log.debug "mebi_wrapper.new_proofv: Created new proofv.";
  let the_proofv : proof_context option ref = ref (Some { proof; names }) in
  the_proofv_opt := Some the_proofv;
  the_proofv
;;

let the_coq_proofv ?(proof : Declare.Proof.t option = None) ()
  : proof_context option ref
  =
  match proof with
  | None ->
    (match !the_proofv_opt with
     | None -> ref None
     | Some the_proofv -> the_proofv)
  | Some proof ->
    (match !the_proofv_opt with
     | None ->
       Log.debug "mebi_wrapper.the_coq_proofv, is Some and have None";
       new_proofv proof
     | Some _old_proof ->
       Log.warning "mebi_wrapper.the_coq_proofv, both Some, overriding.";
       Log.debug "mebi_wrapper.the_coq_proofv, carry over names?";
       new_proofv proof)
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
    match F.find_opt fwd k with
    | None ->
      (* map to next encoding and return *)
      let next_enc : t = !counter in
      counter := !counter + 1;
      F.add fwd k next_enc;
      Tbl.add bck next_enc k;
      next_enc
    | Some enc -> enc
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
      ?(proof : Declare.Proof.t option = None)
      (x : 'a mm)
  : 'a
  =
  let coq_env : Environ.env = !(the_coq_env ~fresh ()) in
  let coq_ctx : Evd.evar_map = !(the_coq_ctx ()) in
  let proofv : proof_context option = !(the_coq_proofv ~proof ()) in
  let coq_ref : coq_context ref = ref { coq_env; coq_ctx; proofv } in
  if keep_encoding then () else E.reset ();
  let fwd_enc : E.t F.t = F.create 0 in
  let bck_enc = B.create 0 in
  let a = x (ref { coq_ref; fwd_enc; bck_enc }) in
  (* enable_logging := false; *)
  a.value
;;

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

let get_env (st : wrapper ref) : Environ.env in_context =
  let coq_st = !st.coq_ref in
  { state = st; value = !coq_st.coq_env }
;;

let get_sigma (st : wrapper ref) : Evd.evar_map in_context =
  let coq_st = !st.coq_ref in
  { state = st; value = !coq_st.coq_ctx }
;;

let get_proofv (st : wrapper ref) : proof_context option in_context =
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

let debug (f : Environ.env -> Evd.evar_map -> Pp.t) : unit mm =
  state (fun env sigma ->
    Feedback.msg_debug (f env sigma);
    sigma, ())
;;

let show_names () : unit mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  (match !coq_st.proofv with
   | None -> Log.debug "mebi_wrapper.show_names, proofv is None"
   | Some proofv ->
     Log.debug
       (Printf.sprintf
          "mebi_wrapper.show_names: %s"
          (if Names.Id.Set.is_empty proofv.names then "[ ] (empty)" else ""));
     Names.Id.Set.iter
       (fun (n : Names.Id.t) ->
         Log.debug
           (Printf.sprintf
              "%s : %s"
              (Names.Id.to_string n)
              (Pp.string_of_ppcmds (Names.Id.print n))))
       proofv.names);
  { state = st; value = () }
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

(* names *)
let new_name_of_string (s : string) : Names.Id.t mm =
  fun (st : wrapper ref) ->
  let coq_st = !st.coq_ref in
  match !coq_st.proofv with
  | None ->
    Log.warning "mebi_wrapper.new_name_of_string st.coq_st.proofv is None";
    assert false
  | Some proofv ->
    let n : Names.Id.t = Names.Id.of_string s in
    let n' : Names.Id.t = Namegen.next_ident_away n proofv.names in
    Log.debug
      (Printf.sprintf
         "mebi_wrapper.new_name_of_string, A:\n\
          - (mem:%b) n  = %s\n\
          - (mem:%b) n' = %s\n\
          - n = n' => %b\n"
         (Names.Id.Set.mem n proofv.names)
         (Names.Id.to_string n)
         (Names.Id.Set.mem n' proofv.names)
         (Names.Id.to_string n')
         (Names.Id.equal n n'));
    (* let names =  in *)
    proofv.names <- Names.Id.Set.add n' proofv.names;
    coq_st := { !coq_st with proofv = Some proofv };
    Log.debug
      (Printf.sprintf
         "mebi_wrapper.new_name_of_string, B:\n\
          - (mem:%b) n  = %s\n\
          - (mem:%b) n' = %s\n\
          - n = n' => %b\n"
         (Names.Id.Set.mem n proofv.names)
         (Names.Id.to_string n)
         (Names.Id.Set.mem n' proofv.names)
         (Names.Id.to_string n')
         (Names.Id.equal n n'));
    let m : Names.Id.t = Namegen.next_ident_away n proofv.names in
    Log.debug
      (Printf.sprintf
         "mebi_wrapper.new_name_of_string, C:\n\
          - (mem:%b) n = %s\n\
          - (mem:%b) m = %s\n\
          - n = m => %b\n"
         (Names.Id.Set.mem n proofv.names)
         (Names.Id.to_string n)
         (Names.Id.Set.mem m proofv.names)
         (Names.Id.to_string m)
         (Names.Id.equal n m));
    (* st := { !st with coq_ref = coq_st }; *)
    { state = st; value = n' }
;;

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

(**********************************)
(****** COQ TERM TO STRING ********)
(**********************************)

let constr_to_string (x : Constr.t) : string =
  let s_mm : string mm =
    let open Syntax in
    let* env = get_env in
    let* sigma = get_sigma in
    return (Pp.string_of_ppcmds (Printer.pr_constr_env env sigma x))
  in
  run ~keep_encoding:true s_mm
;;

let econstr_to_string (x : EConstr.t) : string =
  let s_mm : string mm =
    let open Syntax in
    let* env = get_env in
    let* sigma = get_sigma in
    return (Pp.string_of_ppcmds (Printer.pr_econstr_env env sigma x))
  in
  run ~keep_encoding:true s_mm
;;

let term_to_string (x : term) : string = econstr_to_string x

let constr_rel_decl_to_string (rd : Constr.rel_declaration) : string =
  let s_mm : string mm =
    let open Syntax in
    let* env = get_env in
    let* sigma = get_sigma in
    return (Pp.string_of_ppcmds (Printer.pr_rel_decl env sigma rd))
  in
  run ~keep_encoding:true s_mm
;;

let econstr_rel_decl_to_string (rd : EConstr.rel_declaration) : string =
  let s_mm : string mm =
    let open Syntax in
    let* env = get_env in
    let* sigma = get_sigma in
    return (Pp.string_of_ppcmds (Printer.pr_erel_decl env sigma rd))
  in
  run ~keep_encoding:true s_mm
;;

let constr_list_to_string (xs : Constr.t list) : string =
  match xs with
  | [] -> "[]"
  | h :: [] -> constr_to_string h
  | h :: t ->
    List.fold_left
      (fun (acc : string) (x : Constr.t) ->
        Printf.sprintf "%s, %s" acc (constr_to_string x))
      (constr_to_string h)
      t
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

(********************************************)
(****** ERRORS ******************************)
(********************************************)

module type ERROR_TYPE = sig
  type mebi_error =
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
           (let tystr = Pp.string_of_ppcmds (Printer.pr_econstr_env ev sg ty) in
            Printf.sprintf
              "Does Type match String of any Key? = %b"
              (List.exists
                 (fun (k : term) ->
                   String.equal
                     tystr
                     (Pp.string_of_ppcmds (Printer.pr_econstr_env ev sg k)))
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
    return (Pp.string_of_ppcmds (Printer.pr_econstr_env env sigma y))
  in
  run ~keep_encoding:true s_mm
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
(****** UTILS *******************************)
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

(****************************************************************************)

let proof_query (pstate : Declare.Proof.t) : Proof.t = Declare.Proof.get pstate
let proof_partial (p : Proof.t) : EConstr.t list = Proof.partial_proof p

let proof_test () : unit Proofview.tactic mm =
  fun (st : wrapper ref) ->
  Log.debug "mebi_wrapper.proof_test";
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

(****************************************************************************)

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

(****************************************************************************)

(* source: https://github.com/rocq-prover/rocq/blob/master/doc/plugin_tutorial/tuto3/src/tuto_tactic.ml *)

(* todo: move to monad *)
let constants = ref ([] : EConstr.t list)

(* This is a pattern to collect terms from the Coq memory of valid terms
   and proofs.  This pattern extends all the way to the definition of function
   c_U *)
let collect_bisimilarity_theories () =
  match !constants with
  | [] ->
    let open Names in
    let open EConstr in
    let open UnivGen in
    let find_reference path id =
      let path = DirPath.make (List.rev_map Id.of_string path) in
      let fp = Libnames.make_path path (Id.of_string id) in
      Nametab.global_of_path fp
    in
    (* let gr_M = find_reference ["theories"; "Bisimilarity"] "M" in *)
    (* let gr_A = find_reference ["theories"; "Bisimilarity"] "A" in *)
    let gr_LTS = find_reference [ "theories"; "Bisimilarity" ] "LTS" in
    let gr_tau = find_reference [ "theories"; "Bisimilarity" ] "tau" in
    let gr_silent = find_reference [ "theories"; "Bisimilarity" ] "silent" in
    let gr_silent1 = find_reference [ "theories"; "Bisimilarity" ] "silent1" in
    let gr_weak = find_reference [ "theories"; "Bisimilarity" ] "weak" in
    let gr_simF = find_reference [ "theories"; "Bisimilarity" ] "simF" in
    let gr_weak_sim =
      find_reference [ "theories"; "Bisimilarity" ] "weak_sim"
    in
    let gr_Pack_sim =
      find_reference [ "theories"; "Bisimilarity" ] "Pack_sim"
    in
    let gr_out_sim = find_reference [ "theories"; "Bisimilarity" ] "out_sim" in
    let gr_weak_bisim =
      find_reference [ "theories"; "Bisimilarity" ] "weak_bisim"
    in
    constants
    := List.map
         (fun x -> of_constr (constr_of_monomorphic_global (Global.env ()) x))
         [ (* gr_M; gr_A; *)
           gr_LTS
         ; gr_tau
         ; gr_silent
         ; gr_silent1
         ; gr_weak
         ; gr_simF
         ; gr_weak_sim
         ; gr_Pack_sim
         ; gr_out_sim
         ; gr_weak_bisim
         ];
    !constants
  | _ -> !constants
;;

let _c_LTS () =
  match collect_bisimilarity_theories () with
  | it :: _ -> it
  | _ ->
    failwith
      "could not obtain an internal representation of Theories.Bisimilarity.LTS"
;;

let _c_tau () =
  match collect_bisimilarity_theories () with
  | _ :: it :: _ -> it
  | _ ->
    failwith
      "could not obtain an internal representation of Theories.Bisimilarity.tau"
;;

let _c_silent () =
  match collect_bisimilarity_theories () with
  | _ :: _ :: it :: _ -> it
  | _ ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.silent"
;;

let _c_silent1 () =
  match collect_bisimilarity_theories () with
  | _ :: _ :: _ :: it :: _ -> it
  | _ ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.silent1"
;;

let _c_weak () =
  match collect_bisimilarity_theories () with
  | _ :: _ :: _ :: _ :: it :: _ -> it
  | _ ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.weak"
;;

let _c_simF () =
  match collect_bisimilarity_theories () with
  | _ :: _ :: _ :: _ :: _ :: it :: _ -> it
  | _ ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.simF"
;;

let _c_weak_sim () =
  match collect_bisimilarity_theories () with
  | _ :: _ :: _ :: _ :: _ :: _ :: it :: _ -> it
  | _ ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.weak_sim"
;;

let _c_Pack_sim () =
  match collect_bisimilarity_theories () with
  | _ :: _ :: _ :: _ :: _ :: _ :: _ :: it :: _ -> it
  | _ ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.Pack_sim"
;;

let _c_out_sim () =
  match collect_bisimilarity_theories () with
  | _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: it :: _ -> it
  | _ ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.out_sim"
;;

let _c_weak_bisim () =
  match collect_bisimilarity_theories () with
  | _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: it :: _ -> it
  | _ ->
    failwith
      "could not obtain an internal representation of \
       Theories.Bisimilarity.weak_bisim"
;;

(* The following tactic is meant to pack an hypothesis when no other
   data is already packed.

   The main difficulty in defining this tactic is to understand how to
   construct the input expected by apply_in. *)
let _package i =
  Proofview.Goal.enter (fun gl ->
    Tactics.apply_in
      true
      false
      i
      [ (* this means that the applied theorem is not to be cleared. *)
        (* None, (CAst.make (c_M (), *)
        (* we don't specialize the theorem with extra values. *)
        (* Tactypes.NoBindings)) *) ]
      (* we don't destruct the result according to any intro_pattern *)
      None)
;;

(* In the environment of the goal, we can get the type of an assumption
   directly by a lookup.  The other solution is to call a low-cost retyping
   function like *)
let _get_type_of_hyp env id =
  match EConstr.lookup_named id env with
  | Context.Named.Declaration.LocalAssum (_, ty) -> ty
  | _ ->
    CErrors.user_err
      (let open Pp in
       str (Names.Id.to_string id) ++ str " is not a plain hypothesis")
;;
(* 
let proof_test () : unit Proofview.tactic mm =
  fun (st : wrapper ref) ->
  Log.debug "mebi_wrapper.proof_test";
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
;; *)
(* let coq_st = !st.coq_ref in
  (* *)
  let ((_en, pv) : Proofview.entry * Proofview.proofview) =
    Proofview.init !coq_st.coq_ctx []
    (* [!coq_st.coq_env, _] *)
  in
  Log.debug
    (Printf.sprintf
       "mebi_wrapper.proof_test: is finished => %b"
       (Proofview.finished pv));
  (* *)
  let rel_ctx : EConstr.rel_context = EConstr.rel_context !coq_st.coq_env in
  Log.debug
    (Printf.sprintf
       "mebi_wrapper.proof_test: rel_ctx => \"%s\""
       (Pp.string_of_ppcmds
          (Printer.pr_rel_context
             !coq_st.coq_env
             !coq_st.coq_ctx
             (EConstr.to_rel_context !coq_st.coq_ctx rel_ctx))));
  (* *)
  let named_ctx : EConstr.named_context =
    EConstr.named_context !coq_st.coq_env
  in
  Log.debug
    (Printf.sprintf
       "mebi_wrapper.proof_test: named_ctx => \"%s\""
       (Pp.string_of_ppcmds
          (Printer.pr_named_context
             !coq_st.coq_env
             !coq_st.coq_ctx
             (EConstr.to_named_context !coq_st.coq_ctx named_ctx))));
  (* *)
  { state = st; value = (Proofview.Goal.enter begin fun gl ->
    let hyps = Environ.named_context_val (Proofview.Goal.env gl) in
    
  end
    ) } *)
(*  *)
(* Log.debug
    (Printf.sprintf
       "mebi_wrapper.proof_test: default_goal => %s"
       (Pp.string_of_ppcmds
          (Goal_select.pr_goal_selector
             (Goal_select.get_default_goal_selector ())))); *)
(*  *)
(* let p : Proof.t =
     Proof.start
     ~name:(Names.Id.of_string "test_proof")
     ~poly:false
     !coq_st.coq_ctx
     []
     in
     Log.debug
     (Printf.sprintf "mebi_wrapper.proof_test: is done => %b" (Proof.is_done p)); *)
