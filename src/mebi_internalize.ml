open Mebi_errors
open Mebi_monad
(* open Mebi_monad.Monad_syntax *)

type internals =
  { int_enc : (EConstr.t, int) Hashtbl.t
  ; coq_enc : (int, EConstr.t) Hashtbl.t
  ; counter : int
  ; context : coq_context
  }

type 'a internalized =
  { state : internals ref
  ; value : 'a
  }

type 'a ii = internals ref -> 'a internalized

(********************************************)
(****** CORE DEFINITIONS ********************)
(********************************************)
(* let run (x : 'a ii) : 'a = *)

let return (x : 'a) : 'a ii = fun st -> { state = st; value = x }
[@@inline always]
;;

let bind (x : 'a ii) (f : 'a -> 'b ii) : 'b ii =
  fun st ->
  let a = x st in
  f a.value a.state
[@@inline always]
;;

let map (f : 'a -> 'b) (x : 'a ii) : 'b ii =
  fun st ->
  let x_st = x st in
  { x_st with value = f x_st.value }
[@@inline always]
;;

let product (x : 'a ii) (y : 'b ii) : ('a * 'b) ii =
  bind x (fun a -> bind y (fun b -> return (a, b)))
[@@inline always]
;;

(** Monadic for loop *)
let rec iterate
          (from_idx : int)
          (to_idx : int)
          (acc : 'a)
          (f : int -> 'a -> 'a ii)
  : 'a ii
  =
  if from_idx > to_idx
  then return acc
  else bind (f from_idx acc) (fun acc' -> iterate (from_idx + 1) to_idx acc' f)
;;

(********************************************)
(****** ERRORS ******************************)
(********************************************)

(********************************************)
(****** GET & PUT STATE *********************)
(********************************************)

let get_int_enc (st : internals ref) : (EConstr.t, int) Hashtbl.t internalized =
  { state = st; value = !st.int_enc }
;;

let get_coq_enc (st : internals ref) : (int, EConstr.t) Hashtbl.t internalized =
  { state = st; value = !st.coq_enc }
;;

let get_counter (st : internals ref) : int internalized =
  { state = st; value = !st.counter }
;;

let get_context (st : internals ref) : coq_context internalized =
  { state = st; value = !st.context }
;;

let state
      (f : Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
      (st : internals ref)
  : 'a internalized
  =
  let sigma, a = f !st.context.coq_env !st.context.coq_ctx in
  st := { !st with context = { !st.context with coq_ctx = sigma } };
  { state = st; value = a }
;;

let sandbox (m : 'a ii) (st : internals ref) : 'a internalized =
  let st_contents = !st in
  let res = m st in
  st := st_contents;
  { state = st; value = res.value }
;;
