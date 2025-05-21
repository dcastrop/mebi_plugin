open Mebi_errors

type coq_context =
  { coq_env : Environ.env
  ; coq_ctx : Evd.evar_map
  ; coq_enc : EConstr.t list (* ; coq_enc : (EConstr.t, int) Hashtbl.t *)
  }

(* Essentially, a state monad *)
type 'a in_context =
  { state : coq_context ref
  ; value : 'a
  }

type 'a t = coq_context ref -> 'a in_context

let run (x : 'a t) : 'a =
  let env = Global.env () in
  let sigma = Evd.from_env env in
  (* let a = x (ref { coq_env = env; coq_ctx = sigma }) in *)
  let a = x (ref { coq_env = env; coq_ctx = sigma; coq_enc = [] }) in
  (* let a = x (ref { coq_env = env; coq_ctx = sigma; coq_enc = Hashtbl.create 0
     }) in *)
  a.value
;;

let return (x : 'a) : 'a t = fun st -> { state = st; value = x }
[@@inline always]
;;

let bind (x : 'a t) (f : 'a -> 'b t) : 'b t =
  fun st ->
  let a = x st in
  f a.value a.state
[@@inline always]
;;

let map (f : 'a -> 'b) (x : 'a t) : 'b t =
  fun st ->
  let x_st = x st in
  { x_st with value = f x_st.value }
[@@inline always]
;;

let product (x : 'a t) (y : 'b t) : ('a * 'b) t =
  bind x (fun a -> bind y (fun b -> return (a, b)))
[@@inline always]
;;

let econstr_hash st () t =
  Constr.hash
    (EConstr.to_constr ?abort_on_undefined_evars:(Some false) !st.coq_ctx t)
;;

let _econstr_to_string st () (target : EConstr.t) : string =
  Pp.string_of_ppcmds (Printer.pr_econstr_env !st.coq_env !st.coq_ctx target)
;;

(* let eq_constr st () = EConstr.eq_constr !st.coq_ctx *)
(** [] used to determine a match in a [Hashtbl] with [EConstr.t] keys. *)
let eq_constr st () t1 t2 : bool = EConstr.eq_constr !st.coq_ctx t1 t2

(** same as above, but with sanity checks *)
let _eq_constr ?(prefix : string = "eq_constr") st () t1 t2 : bool =
  let is_eq_constr : bool = EConstr.eq_constr !st.coq_ctx t1 t2 in
  let t1_str : string = _econstr_to_string st () t1 in
  let t2_str : string = _econstr_to_string st () t2 in
  let is_eq_str : bool = String.equal t1_str t2_str in
  (match is_eq_constr, is_eq_str with
   | false, true ->
     Utils.Logging.Log.warning
       ~params:(Utils.Params.Default.log ~mode:(Coq ()) ())
       (Printf.sprintf
          "%s,\n\
          \ EConstr.eq_constr t1 t2: %b\n\
          \ String.equal t1 t2     : %b\n\
          \ t1: %s\n\
          \ t2: %s"
          prefix
          is_eq_constr
          is_eq_str
          t1_str
          t2_str)
   | _, _ -> ());
  is_eq_constr
;;

(** [] used for comparing [EConstr.t] in a set. *)
let _compare_constr_using_eq st () t1 t2 =
  (* if eq_constr ~prefix:"compare_constr" st () t1 t2 then 0 else 1 *)
  if EConstr.eq_constr !st.coq_ctx t1 t2 then 0 else 1
;;

let _compare_constr_using_int st () t1 t2 =
  Int.compare (econstr_hash st () t1) (econstr_hash st () t2)
;;

let _compare_constr_using_str st () t1 t2 =
  let t1_str : string = _econstr_to_string st () t1 in
  let t2_str : string = _econstr_to_string st () t2 in
  String.compare t1_str t2_str
;;

(** _compare_constr_using_enc_list st () t1 t2 compares the encodings of t1, t2.
    Terms are stored in a list in the order they are encountered, with new terms
    being added to the front of the list. If a term is not found in the list,
    then its encoding must be the length of the list. When a term is found in
    the list, since we add terms to front, the [int] index does not represent
    its encoding and we must minus this from the total length of the list.
    (E.g., for term [b] in list [e, d, c, b, a] which has length [5], the index
    of [b] is [3], in order to obtain the encoding of [b] we must [(5-1)-3]
    which yields an encoding of [1].) *)
let _compare_constr_using_enc_list st () t1 t2 =
  let (t1_enc, offset2) : int * int =
    let offset1 : int = List.length !st.coq_enc - 1 in
    match
      List.find_index
        (fun (t : EConstr.t) -> EConstr.eq_constr !st.coq_ctx t1 t)
        !st.coq_enc
    with
    | None ->
      (* add to cache *)
      st := { !st with coq_enc = t1 :: !st.coq_enc };
      (* t1 is at the head of the queue*)
      offset1 + 1, offset1 + 1
    | Some enc -> offset1 - enc, offset1
  in
  let t2_enc : int =
    match
      List.find_index
        (fun (t : EConstr.t) -> EConstr.eq_constr !st.coq_ctx t2 t)
        !st.coq_enc
    with
    | None ->
      (* add to cache *)
      st := { !st with coq_enc = t2 :: !st.coq_enc };
      offset2 + 1
    | Some enc -> offset2 - enc
  in
  (* sanity check *)
  (* let comp = Int.compare t1_enc t2_enc in if Int.equal comp 0 then ( let
     t1_str : string = _econstr_to_string st () t1 in let t2_str : string =
     _econstr_to_string st () t2 in if Bool.not (String.equal t1_str t2_str)
     then Utils.Logging.Log.warning ~params:(Utils.Params.Default.log ~mode:(Coq
     ()) ()) (Printf.sprintf "_compare_constr_using_enc_list, equal but str not
     equal:\n\ \ t1: %s\n\n\ \ t2:\n\ \ %s\n" t1_str t2_str)); comp *)
  Int.compare t1_enc t2_enc
;;

(** let _compare_constr_using_enc_tbl st () t1 t2 compares encodings of t1 t2.
    The [(EConstr.t, int) Hashtbl.t] maps terms to integers. When a new term is
    encountered, it is mapped to the current length of the table. *)
(* let _compare_constr_using_enc_tbl st () t1 t2 = let t1_enc : int = match
   Hashtbl.find_opt !st.coq_enc t1 with | None -> let enc : int = Hashtbl.length
   !st.coq_enc in Hashtbl.add !st.coq_enc t1 enc; enc | Some enc -> enc in let
   t2_enc : int = match Hashtbl.find_opt !st.coq_enc t2 with | None -> let enc :
   int = Hashtbl.length !st.coq_enc in Hashtbl.add !st.coq_enc t2 enc; enc |
   Some enc -> enc in (* sanity check *) (* let comp = Int.compare t1_enc t2_enc
   in if Int.equal comp 0 then ( let t1_str : string = _econstr_to_string st ()
   t1 in let t2_str : string = _econstr_to_string st () t2 in if Bool.not
   (String.equal t1_str t2_str) then Utils.Logging.Log.warning
   ~params:(Utils.Params.Default.log ~mode:(Coq ()) ()) (Printf.sprintf
   "_compare_constr_using_enc_tbl, equal but str\n\ \ not equal:\n\ \ t1:
   %s\n\n\ \ t2: %s\n" t1_str t2_str)); comp *) Int.compare t1_enc t2_enc ;; *)

(* temporary *)
(* let compare_constr st () t1 t2 = _compare_constr_using_eq st () t1 t2 *)
(* let compare_constr st () t1 t2 = _compare_constr_using_str st () t1 t2 *)
let compare_constr st () t1 t2 = _compare_constr_using_enc_list st () t1 t2
(* let compare_constr st () t1 t2 = _compare_constr_using_enc_tbl st () t1 t2 *)

(** [make_constr_tbl st] is used to create Hashtbl that map from [EConstr.t] *)
let make_constr_tbl st =
  let cmp_eq = eq_constr st in
  let hashf = econstr_hash st in
  let module Constrtbl =
    Hashtbl.Make (struct
      type t = EConstr.t

      let equal t1 t2 = cmp_eq () t1 t2
      let hash t = hashf () t
    end)
  in
  { state = st
  ; value = (module Constrtbl : Hashtbl.S with type key = EConstr.t)
  }
;;

(** [make_constr_set st] is used to create Set of [EConstr.t] *)
let make_constr_set (st : coq_context ref)
  : (module Set.S with type elt = EConstr.t) in_context
  =
  let cmp = compare_constr st in
  let module Constrset =
    Set.Make (struct
      type t = EConstr.t

      let compare t1 t2 = cmp () t1 t2
    end)
  in
  { state = st; value = (module Constrset : Set.S with type elt = EConstr.t) }
;;

(** compare_constr_tree st () t1 t2 compares destination pairs t1 and t2.
    Prioritises the comparison of terms over trees. Only if terms look identical
    do we then compare the trees. (See [Constr_tree.compare].)*)
let compare_constr_tree
      st
      ()
      (t1 : EConstr.t * Constr_tree.t)
      (t2 : EConstr.t * Constr_tree.t)
  =
  match compare_constr st () (fst t1) (fst t2) with
  | 0 -> Constr_tree.compare (snd t1) (snd t2)
  | n -> n
;;

(* if EConstr.eq_constr !st.coq_ctx (fst t1) (fst t2) && Constr_tree.eq (snd t1)
   (snd t2) then 0 else 1 *)

(** [make_constr_set st] is used to create Set of [EConstr.t * Constr_tree.t] *)
let make_constr_tree_set (st : coq_context ref)
  : (module Set.S with type elt = EConstr.t * Constr_tree.t) in_context
  =
  let cmp = compare_constr_tree st in
  let module Constrset =
    Set.Make (struct
      type t = EConstr.t * Constr_tree.t

      let compare t1 t2 = cmp () t1 t2
    end)
  in
  { state = st
  ; value = (module Constrset : Set.S with type elt = EConstr.t * Constr_tree.t)
  }
;;

(** Monadic for loop *)
let rec iterate
          (from_idx : int)
          (to_idx : int)
          (acc : 'a)
          (f : int -> 'a -> 'a t)
  : 'a t
  =
  if from_idx > to_idx
  then return acc
  else bind (f from_idx acc) (fun acc' -> iterate (from_idx + 1) to_idx acc' f)
;;

let get_env (st : coq_context ref) : Environ.env in_context =
  { state = st; value = !st.coq_env }
;;

let get_sigma (st : coq_context ref) : Evd.evar_map in_context =
  { state = st; value = !st.coq_ctx }
;;

let state
      (f : Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
      (st : coq_context ref)
  : 'a in_context
  =
  let sigma, a = f !st.coq_env !st.coq_ctx in
  st := { !st with coq_ctx = sigma };
  { state = st; value = a }
;;

let sandbox (m : 'a t) (st : coq_context ref) : 'a in_context =
  let st_contents = !st in
  let res = m st in
  st := st_contents;
  { state = st; value = res.value }
;;

let debug (f : Environ.env -> Evd.evar_map -> Pp.t) : unit t =
  state (fun env sigma ->
    Feedback.msg_debug (f env sigma);
    sigma, ())
;;

(** Error when input LTS has the wrong arity *)
let invalid_arity (x : Constr.types) : 'a t =
  fun st -> raise (invalid_arity !st.coq_env !st.coq_ctx x)
;;

(** Error when input LTS has the wrong Sort *)
let invalid_sort (x : Sorts.family) : 'a t = fun st -> raise (invalid_sort x)

(** Error when input LTS reference is invalid (e.g. non existing) *)
let invalid_ref (x : Names.GlobRef.t) : 'a t = fun st -> raise (invalid_ref x)

(** Error when term is of unknown type *)
let unknown_term_type (tmty : EConstr.t * EConstr.t * EConstr.t list) : 'a t =
  fun st -> raise (unknown_term_type !st.coq_env !st.coq_ctx tmty)
;;

let primary_lts_not_found ((t, names) : EConstr.t * EConstr.t list) : 'a t =
  fun st -> raise (primary_lts_not_found !st.coq_env !st.coq_ctx t names)
;;

module type Monad = sig
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let$ )
    :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
    -> ('a -> 'b t)
    -> 'b t

  val ( let$* )
    :  (Environ.env -> Evd.evar_map -> Evd.evar_map)
    -> (unit -> 'b t)
    -> 'b t

  val ( let$+ ) : (Environ.env -> Evd.evar_map -> 'a) -> ('a -> 'b t) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

module Monad_syntax : Monad = struct
  let ( let+ ) x f = map f x
  let ( let* ) = bind
  let ( let$ ) f g = bind (state f) g
  let ( let$* ) f g = bind (state (fun e s -> f e s, ())) g
  let ( let$+ ) f g = bind (state (fun e s -> s, f e s)) g
  let ( and+ ) x y = product x y
end
