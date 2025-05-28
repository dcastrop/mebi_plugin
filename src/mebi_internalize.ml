module M = Mebi_monad
module F = Mebi_wrapper.MkF

(********************************************)
(****** ENCODINGS ***************************)
(********************************************)

module type ENCODING_TYPE = sig
  type t

  val init : t
  val eq : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val to_string : t -> string

  module type ENC_TBL = Hashtbl.S with type key = t

  module Tbl : ENC_TBL

  val encode : t F.t -> M.term Tbl.t -> M.term -> t

  exception InvalidDecodeKey of (t * M.term Tbl.t)

  val decode : M.term Tbl.t -> t -> M.term
end

(**********************************)
(****** INTEGER ENCODING **********)
(**********************************)

module IntEncoding : ENCODING_TYPE = struct
  type t = int

  let init = 0
  let counter : t ref = ref init
  let eq t1 t2 = Int.equal t1 t2
  let compare t1 t2 = Int.compare t1 t2
  let hash t = Int.hash t
  let to_string t : string = Printf.sprintf "%i" t

  module type ENC_TBL = Hashtbl.S with type key = t

  module Tbl : ENC_TBL = Hashtbl.Make (struct
      type t = int

      let equal t1 t2 = eq t1 t2
      let hash t = hash t
    end)

  let encode (fwd : t F.t) (bck : M.term Tbl.t) (k : M.term) : t =
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

  exception InvalidDecodeKey of (t * M.term Tbl.t)

  let decode (bck : M.term Tbl.t) (k : t) : M.term =
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
  { coq_ref : M.coq_context ref
  ; fwd_enc : E.t F.t
  ; bck_enc : M.term B.t
  }

type 'a in_context =
  { state : wrapper ref
  ; value : 'a
  }

type 'a mm = wrapper ref -> 'a in_context

(** [run x] initializes the monad, and runs [x]. *)
let run (x : 'a mm) : 'a =
  let coq_ref = M.init in
  let fwd_enc : E.t F.t = F.create 0 in
  let bck_enc = B.create 0 in
  let a = x (ref { coq_ref; fwd_enc; bck_enc }) in
  a.value
;;

let return (x : 'a) : 'a mm = fun st -> { state = st; value = x }
[@@inline always]
;;

let bind (x : 'a mm) (f : 'a -> 'b mm) : 'b mm =
  fun st ->
  let a = x st in
  f a.value a.state
[@@inline always]
;;

let map (f : 'a -> 'b) (x : 'a mm) : 'b mm =
  fun st ->
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
(****** ERRORS ******************************)
(********************************************)

module type ERROR_TYPE = sig
  type mebi_error =
    | InvalidLTSSort of Sorts.family
    | InvalidArity of Environ.env * Evd.evar_map * Constr.types
    | InvalidLTSRef of Names.GlobRef.t
    | UnknownTermType of
        (Environ.env * Evd.evar_map * (M.term * M.term * M.term list))
    | PrimaryLTSNotFound of (Environ.env * Evd.evar_map * M.term * M.term list)
    | UnknownDecodeKey of (Environ.env * Evd.evar_map * E.t * M.term B.t)

  exception MEBI_exn of mebi_error

  val invalid_sort : Sorts.family -> exn
  val invalid_arity : Environ.env -> Evd.evar_map -> Constr.types -> exn
  val invalid_ref : Names.GlobRef.t -> exn

  val unknown_term_type
    :  Environ.env
    -> Evd.evar_map
    -> M.term * M.term * M.term list
    -> exn

  val primary_lts_not_found
    :  Environ.env
    -> Evd.evar_map
    -> M.term
    -> M.term list
    -> exn

  val unknown_decode_key
    :  Environ.env
    -> Evd.evar_map
    -> E.t
    -> M.term B.t
    -> exn
end

module Error : ERROR_TYPE = struct
  type mebi_error =
    | InvalidLTSSort of Sorts.family
    | InvalidArity of Environ.env * Evd.evar_map * Constr.types
    | InvalidLTSRef of Names.GlobRef.t
    | UnknownTermType of
        (Environ.env * Evd.evar_map * (M.term * M.term * M.term list))
    | PrimaryLTSNotFound of (Environ.env * Evd.evar_map * M.term * M.term list)
    | UnknownDecodeKey of (Environ.env * Evd.evar_map * E.t * M.term B.t)

  exception MEBI_exn of mebi_error

  (** Error when input LTS has the wrong arity *)
  let invalid_sort f = MEBI_exn (InvalidLTSSort f)

  (** Error when input LTS has the wrong Sort *)
  let invalid_arity ev sg t = MEBI_exn (InvalidArity (ev, sg, t))

  (** Error when input LTS reference is invalid (e.g. non existing) *)
  let invalid_ref r = MEBI_exn (InvalidLTSRef r)

  (** Error when M.term is of unknown type *)
  let unknown_term_type ev sg tmty = MEBI_exn (UnknownTermType (ev, sg, tmty))

  (** Error when multiple coq-LTS provided, but none of them match term. *)
  let primary_lts_not_found ev sg t names =
    MEBI_exn (PrimaryLTSNotFound (ev, sg, t, names))
  ;;

  (** Error when multiple coq-LTS provided, but none of them match term. *)
  let unknown_decode_key ev sg k bckmap =
    MEBI_exn (UnknownDecodeKey (ev, sg, k, bckmap))
  ;;

  open Pp

  let mebi_handler = function
    | InvalidLTSSort f ->
      str "Invalid LTS Sort: expecting Prop, got " ++ Sorts.pr_sort_family f
    | InvalidArity (ev, sg, t) ->
      str "Invalid arity for LTS: "
      ++ Printer.pr_constr_env ev sg t
      ++ strbrk "\n"
      ++ str "Expecting: forall params, ?terms -> ?labels -> ?terms -> Prop"
    | InvalidLTSRef r -> str "Invalid LTS ref: " ++ Printer.pr_global r
    | UnknownTermType (ev, sg, (tm, ty, trkeys)) ->
      str
        "None of the constructors provided matched type of M.term to visit. \
         (unknown_term_type) "
      ++ strbrk "\n\n"
      ++ str "Term: "
      ++ Printer.pr_econstr_env ev sg tm
      ++ strbrk "\n\n"
      ++ str "Type: "
      ++ Printer.pr_econstr_env ev sg ty
      ++ strbrk "\n\n"
      ++ str
           (Printf.sprintf
              "Keys: %s"
              (if List.is_empty trkeys
               then "[ ] (empty)"
               else
                 Printf.sprintf
                   "[%s ]"
                   (List.fold_left
                      (fun (acc : string) (k : M.term) ->
                        Printf.sprintf
                          "%s '%s'"
                          acc
                          (Pp.string_of_ppcmds (Printer.pr_econstr_env ev sg k)))
                      ""
                      trkeys)))
      ++ strbrk "\n\n"
      ++ str
           (Printf.sprintf
              "Does Type match EConstr of any Key? = %b"
              (List.exists
                 (fun (k : M.term) -> EConstr.eq_constr sg ty k)
                 trkeys))
      ++ strbrk "\n"
      ++ str
           (let tystr = Pp.string_of_ppcmds (Printer.pr_econstr_env ev sg ty) in
            Printf.sprintf
              "Does Type match String of any Key? = %b"
              (List.exists
                 (fun (k : M.term) ->
                   String.equal
                     tystr
                     (Pp.string_of_ppcmds (Printer.pr_econstr_env ev sg k)))
                 trkeys))
    | PrimaryLTSNotFound (ev, sg, t, names) ->
      str "(TODO: primary lts not found error)"
    | UnknownDecodeKey (ev, sg, k, bckmap) ->
      str "(TODO: unknown decode key error)"
  ;;

  let _ =
    CErrors.register_handler (fun e ->
      match e with MEBI_exn e -> Some (mebi_handler e) | _ -> None)
  ;;
end

(**********************************)
(****** ERROR FUNCTIONS ***********)
(**********************************)

(** Error when input LTS has the wrong arity *)
let invalid_arity (x : Constr.types) : 'a mm =
  fun st ->
  let coq_st = !st.coq_ref in
  raise (Error.invalid_arity !coq_st.coq_env !coq_st.coq_ctx x)
;;

(** Error when input LTS has the wrong Sort *)
let invalid_sort (x : Sorts.family) : 'a mm =
  fun st -> raise (Error.invalid_sort x)
;;

(** Error when input LTS reference is invalid (e.g. non existing) *)
let invalid_ref (x : Names.GlobRef.t) : 'a mm =
  fun st -> raise (Error.invalid_ref x)
;;

(** Error when M.term is of unknown type *)
let unknown_term_type (tmty : M.term * M.term * M.term list) : 'a mm =
  fun st ->
  let coq_st = !st.coq_ref in
  raise (Error.unknown_term_type !coq_st.coq_env !coq_st.coq_ctx tmty)
;;

(** Error when multiple coq-LTS provided, but none of them match term. *)
let primary_lts_not_found ((t, names) : M.term * M.term list) : 'a mm =
  fun st ->
  let coq_st = !st.coq_ref in
  raise (Error.primary_lts_not_found !coq_st.coq_env !coq_st.coq_ctx t names)
;;

(** Error when try to decode key that does not exist in decode map. *)
let unknown_decode_key ((k, bckmap) : E.t * M.term B.t) : 'a mm =
  fun st ->
  let coq_st = !st.coq_ref in
  raise (Error.unknown_decode_key !coq_st.coq_env !coq_st.coq_ctx k bckmap)
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

let get_fwd_enc (st : wrapper ref) : E.t F.t in_context =
  { state = st; value = !st.fwd_enc }
;;

let get_bck_enc (st : wrapper ref) : M.term B.t in_context =
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

(********************************************)
(****** ENCODE/DECODE ***********************)
(********************************************)

let encode (k : M.term) : E.t mm =
  fun (st : wrapper ref) ->
  let encoding : E.t = E.encode !st.fwd_enc !st.bck_enc k in
  { state = st; value = encoding }
;;

(** dual to [encode] except we cannot handle new values *)
let decode (k : E.t) : M.term mm =
  fun (st : wrapper ref) ->
  let decoding : M.term = E.decode !st.bck_enc k in
  { state = st; value = decoding }
;;

(**********************************)
(****** ENCODE/DECODE MAPs ********)
(**********************************)

let encode_map (m : 'a F.t) : 'a B.t mm =
  fun (st : wrapper ref) ->
  let encoded_map : 'a B.t = B.create (F.length m) in
  F.iter
    (fun (k : M.term) (v : 'a) ->
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
      let decoding : M.term = E.decode !st.bck_enc k in
      F.add decoded_map decoding v)
    m;
  { state = st; value = decoded_map }
;;

(********************************************)
(****** UTILS *******************************)
(********************************************)

(**********************************)
(****** COQ TERM TO STRING ********)
(**********************************)

(** *)
let constr_to_string (x : Constr.t) : string =
  let s_mm : string mm =
    let open Syntax in
    let* env = get_env in
    let* sigma = get_sigma in
    return (Pp.string_of_ppcmds (Printer.pr_constr_env env sigma x))
  in
  run s_mm
;;

let econstr_to_string (x : EConstr.t) : string =
  let s_mm : string mm =
    let open Syntax in
    let* env = get_env in
    let* sigma = get_sigma in
    return (Pp.string_of_ppcmds (Printer.pr_econstr_env env sigma x))
  in
  run s_mm
;;

(**********************************)
(****** COQ TERMS *****************)
(**********************************)

let tref_to_econstr (tref : Constrexpr.constr_expr) : M.term mm =
  let open Syntax in
  let$ t env sigma = Constrintern.interp_constr_evars env sigma tref in
  return t
;;

let normalize_econstr (t' : M.term) : M.term mm =
  let open Syntax in
  let$+ t env sigma = Reductionops.nf_all env sigma t' in
  return t
;;

let type_of_econstr (t' : M.term) : M.term mm =
  let open Syntax in
  let* (t : M.term) = normalize_econstr t' in
  let$ ty env sigma = Typing.type_of env sigma t in
  return ty
;;

(** *)
let type_of_tref (tref : Constrexpr.constr_expr) : M.term mm =
  let open Syntax in
  let* (t : M.term) = tref_to_econstr tref in
  type_of_econstr t
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
