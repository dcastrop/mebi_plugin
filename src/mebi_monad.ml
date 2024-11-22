open Mebi_errors

type coq_context =
  { coq_env : Environ.env
  ; coq_ctx : Evd.evar_map
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
  let a = x (ref { coq_env = env; coq_ctx = sigma }) in
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

let eq_constr st () = EConstr.eq_constr !st.coq_ctx

let econstr_hash st () t =
  Constr.hash
    (EConstr.to_constr ?abort_on_undefined_evars:(Some false) !st.coq_ctx t)
;;

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

let get_env st = { state = st; value = !st.coq_env }
let get_sigma st = { state = st; value = !st.coq_ctx }

let state f st =
  let sigma, a = f !st.coq_env !st.coq_ctx in
  st := { !st with coq_ctx = sigma };
  { state = st; value = a }
;;

let sandbox (m : 'a t) (st : coq_context ref) =
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
