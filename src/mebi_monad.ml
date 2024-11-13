open Mebi_errors

type coq_context =
  { coq_env : Environ.env
  ; coq_ctx : Evd.evar_map
  }

(* Essentially, a state monad *)
type 'a in_context =
  { state : coq_context
  ; value : 'a
  }

type 'a t = coq_context -> 'a in_context

let run (x : 'a t) : 'a =
  let env = Global.env () in
  let sigma = Evd.from_env env in
  let a = x { coq_env = env; coq_ctx = sigma } in
  a.value
;;

let return (x : 'a) : 'a t = fun st -> { state = st; value = x }

let bind (x : 'a t) (f : 'a -> 'b t) : 'b t =
  fun st ->
  let a = x st in
  f a.value a.state
;;

let map (f : 'a -> 'b) (x : 'a t) : 'b t =
  fun st ->
  let x_st = x st in
  { x_st with value = f x_st.value }
;;

let product (x : 'a t) (y : 'b t) : ('a * 'b) t =
  bind x (fun a -> bind y (fun b -> return (a, b)))
;;

(** Monadic for loop *)
let rec iterate
  (from_idx : int)
  (to_idx : int)
  (acc : 'a)
  (f : int -> 'a -> 'a t)
  =
  if from_idx > to_idx
  then return acc
  else bind (f from_idx acc) (fun acc' -> iterate (from_idx + 1) to_idx acc' f)
;;

let get_env st = { state = st; value = st.coq_env }
let get_sigma st = { state = st; value = st.coq_ctx }

let with_state f st =
  let sigma, a = f st.coq_env st.coq_ctx in
  { state = { st with coq_ctx = sigma }; value = a }
;;

let with_state' f = with_state (fun ev sg -> f ev sg, ())

(** Error when input LTS has the wrong arity *)
let invalid_arity (x : Constr.types) : 'a t =
  fun st -> raise (invalid_arity st.coq_env st.coq_ctx x)
;;

(** Error when input LTS has the wrong Sort *)
let invalid_sort (x : Sorts.family) : 'a t = fun st -> raise (invalid_sort x)

(** Error when input LTS reference is invalid (e.g. non existing) *)
let invalid_ref (x : Names.GlobRef.t) : 'a t = fun st -> raise (invalid_ref x)

module type Monad = sig
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let$ )
    :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
    -> ('a -> 'b t)
    -> 'b t

  val ( let$+ )
    :  (Environ.env -> Evd.evar_map -> Evd.evar_map)
    -> (unit -> 'b t)
    -> 'b t

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

module Monad_syntax : Monad = struct
  let ( let+ ) x f = map f x
  let ( let* ) = bind
  let ( let$ ) f g = bind (with_state f) g
  let ( let$+ ) f g = bind (with_state' f) g
  let ( and+ ) x y = product x y
end
