type term = EConstr.t

let default_params () : Utils.Params.log =
  Utils.Params.Default.log ~mode:(Coq ()) ()
;;

let enable_logging : bool ref = ref true

(********************************************)
(****** COQ ENVIRONMENT *********************)
(********************************************)

type coq_context =
  { coq_env : Environ.env
  ; coq_ctx : Evd.evar_map
  }

let coq_env_wrapper : Environ.env ref option ref = ref None

let new_coq_env () : Environ.env ref =
  if !enable_logging
  then
    Utils.Logging.Log.override
      ~params:(default_params ())
      "Created new coq env.";
  let env = ref (Global.env ()) in
  coq_env_wrapper := Some env;
  env
;;

let the_coq_env ?(fresh : bool = false) () : Environ.env ref =
  match !coq_env_wrapper with
  | None -> new_coq_env ()
  | Some env -> if fresh then new_coq_env () else env
;;

let coq_ctx_wrapper : Evd.evar_map ref option ref = ref None

let new_coq_ctx ?(fresh : bool = false) () : Evd.evar_map ref =
  if !enable_logging
  then
    Utils.Logging.Log.override
      ~params:(default_params ())
      "Created new coq ctx.";
  let ctx = ref (Evd.from_env !(the_coq_env ~fresh ())) in
  coq_ctx_wrapper := Some ctx;
  ctx
;;

let the_coq_ctx ?(fresh : bool = false) () : Evd.evar_map ref =
  match !coq_ctx_wrapper with
  | None -> new_coq_ctx ()
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
let run ?(keep_encoding : bool = false) (x : 'a mm) : 'a =
  let env = !(the_coq_env ~fresh:true ()) in
  let sigma = !(the_coq_ctx ()) in
  let coq_ref : coq_context ref = ref { coq_env = env; coq_ctx = sigma } in
  if keep_encoding then () else E.reset ();
  let fwd_enc : E.t F.t = F.create 0 in
  let bck_enc = B.create 0 in
  let a = x (ref { coq_ref; fwd_enc; bck_enc }) in
  enable_logging := false;
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
        (Environ.env * Evd.evar_map * (term * term * term list))
    | PrimaryLTSNotFound of (Environ.env * Evd.evar_map * term * term list)
    | UnknownDecodeKey of (Environ.env * Evd.evar_map * E.t * term B.t)

  exception MEBI_exn of mebi_error

  val invalid_sort : Sorts.family -> exn
  val invalid_arity : Environ.env -> Evd.evar_map -> Constr.types -> exn
  val invalid_ref : Names.GlobRef.t -> exn

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
end

module Error : ERROR_TYPE = struct
  type mebi_error =
    | InvalidLTSSort of Sorts.family
    | InvalidArity of Environ.env * Evd.evar_map * Constr.types
    | InvalidLTSRef of Names.GlobRef.t
    | UnknownTermType of
        (Environ.env * Evd.evar_map * (term * term * term list))
    | PrimaryLTSNotFound of (Environ.env * Evd.evar_map * term * term list)
    | UnknownDecodeKey of (Environ.env * Evd.evar_map * E.t * term B.t)

  exception MEBI_exn of mebi_error

  (** Error when input LTS has the wrong arity *)
  let invalid_sort f = MEBI_exn (InvalidLTSSort f)

  (** Error when input LTS has the wrong Sort *)
  let invalid_arity ev sg t = MEBI_exn (InvalidArity (ev, sg, t))

  (** Error when input LTS reference is invalid (e.g. non existing) *)
  let invalid_ref r = MEBI_exn (InvalidLTSRef r)

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
        "None of the constructors provided matched type of term to visit. \
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
                      (fun (acc : string) (k : term) ->
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

(** Error when term is of unknown type *)
let unknown_term_type (tmty : term * term * term list) : 'a mm =
  fun st ->
  let coq_st = !st.coq_ref in
  raise (Error.unknown_term_type !coq_st.coq_env !coq_st.coq_ctx tmty)
;;

(** Error when multiple coq-LTS provided, but none of them match term. *)
let primary_lts_not_found ((t, names) : term * term list) : 'a mm =
  fun st ->
  let coq_st = !st.coq_ref in
  raise (Error.primary_lts_not_found !coq_st.coq_env !coq_st.coq_ctx t names)
;;

(** Error when try to decode key that does not exist in decode map. *)
let unknown_decode_key ((k, bckmap) : E.t * term B.t) : 'a mm =
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

let encode (k : term) : E.t mm =
  fun (st : wrapper ref) ->
  let encoding : E.t = E.encode !st.fwd_enc !st.bck_enc k in
  { state = st; value = encoding }
;;

(** dual to [encode] except we cannot handle new values *)
let decode (k : E.t) : term mm =
  fun (st : wrapper ref) ->
  let decoding : term = E.decode !st.bck_enc k in
  { state = st; value = decoding }
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
      let* (decoded_leaf_lts : term) = decode (fst leaf) in
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
(****** COQ TERMS *****************)
(**********************************)

let tref_to_econstr (tref : Constrexpr.constr_expr) : term mm =
  let open Syntax in
  let$ t env sigma = Constrintern.interp_constr_evars env sigma tref in
  return t
;;

let normalize_econstr (t' : term) : term mm =
  let open Syntax in
  let$+ t env sigma = Reductionops.nf_all env sigma t' in
  return t
;;

let type_of_econstr (t' : term) : term mm =
  let open Syntax in
  let* (t : term) = normalize_econstr t' in
  let$ ty env sigma = Typing.type_of env sigma t in
  return ty
;;

(** *)
let type_of_tref (tref : Constrexpr.constr_expr) : term mm =
  let open Syntax in
  let* (t : term) = tref_to_econstr tref in
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
