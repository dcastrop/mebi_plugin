(*********************************************************************)
(*** Internals *******************************************************)
(*********************************************************************)

module type ENCODING_TYPE = sig
  type t

  val init : t
  val eq : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val next : t -> t
  (* val make_hashtbl : (module Hashtbl.S with type key = t) *)

  module Tbl : Hashtbl.S with type key = t
end

module IntEncoding : ENCODING_TYPE = struct
  type t = int

  let init = 0
  let eq t1 t2 = Int.equal t1 t2
  let compare t1 t2 = Int.compare t1 t2
  let hash t = Int.hash t
  let next t = t + 1

  module Tbl = Hashtbl.Make (struct
      type t = int

      let equal t1 t2 = eq t1 t2
      let hash t = hash t
    end)
end

(*********************************************************************)
(*** Mebi Monad ******************************************************)
(*********************************************************************)

module type MEBI_MONAD = sig
  module E : ENCODING_TYPE

  type term = EConstr.t

  type coq_context =
    { coq_env : Environ.env
    ; coq_ctx : Evd.evar_map
    ; fwd_enc : (term, E.t) Hashtbl.t
    ; bck_enc : term E.Tbl.t
    ; counter : E.t ref
    }

  type 'a in_context =
    { state : coq_context ref
    ; value : 'a
    }

  type 'a mm = coq_context ref -> 'a in_context

  (* core definitions *)
  val run : 'a mm -> 'a
  val return : 'a -> 'a mm
  val bind : 'a mm -> ('a -> 'b mm) -> 'b mm
  val map : ('a -> 'b) -> 'a mm -> 'b mm
  val product : 'a mm -> 'b mm -> ('a * 'b) mm
  val iterate : int -> int -> 'a -> (int -> 'a -> 'a mm) -> 'a mm

  (* get and put state *)
  val get_env : coq_context ref -> Environ.env in_context
  val get_sigma : coq_context ref -> Evd.evar_map in_context
  val get_fwd_enc : coq_context ref -> (term, E.t) Hashtbl.t in_context
  val get_bck_enc : coq_context ref -> term E.Tbl.t in_context
  val get_counter : coq_context ref -> E.t ref in_context

  val state
    :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
    -> coq_context ref
    -> 'a in_context

  val sandbox : 'a mm -> coq_context ref -> 'a in_context
  val debug : (Environ.env -> Evd.evar_map -> Pp.t) -> unit mm

  (* syntax *)
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

    val ( let$+ )
      :  (Environ.env -> Evd.evar_map -> 'a)
      -> ('a -> 'b mm)
      -> 'b mm

    val ( and+ ) : 'a mm -> 'b mm -> ('a * 'b) mm
  end

  module Syntax : MEBI_MONAD_SYNTAX

  val next : coq_context ref -> E.t mm
  val encode : coq_context ref -> term -> E.t mm
  val decode : coq_context ref -> E.t -> term mm
end

module MebiMonad (Enc : ENCODING_TYPE) : MEBI_MONAD = struct
  module E = Enc

  (* FIXME: must be custom module that uses state *)
  module F = Hashtbl
  module B = E.Tbl

  type term = EConstr.t

  type coq_context =
    { coq_env : Environ.env
    ; coq_ctx : Evd.evar_map
    ; fwd_enc : (term, E.t) F.t
    ; bck_enc : term B.t
    ; counter : E.t ref
    }

  type 'a in_context =
    { state : coq_context ref
    ; value : 'a
    }

  (* let make_constr_tbl (st : coq_context ref) : (module Hashtbl.S with type
     key = EConstr.t) in_context = let module Constrtbl = Hashtbl.Make (struct
     type t = EConstr.t

     let equal t1 t2 = EConstr.eq_constr !st.coq_ctx t1 t2

     let hash t = Constr.hash (EConstr.to_constr ?abort_on_undefined_evars:(Some
     false) !st.coq_ctx t) ;; end) in { state = st ; value = (module Constrtbl :
     Hashtbl.S with type key = EConstr.t) } ;; *)

  type 'a mm = coq_context ref -> 'a in_context

  (** [run x] initializes the monad, and runs [x]. *)
  let run (x : 'a mm) : 'a =
    let env = Global.env () in
    let sigma = Evd.from_env env in
    let fwd_enc = F.create 0 in
    let bck_enc = B.create 0 in
    let counter : E.t ref = ref E.init in
    let a =
      x (ref { coq_env = env; coq_ctx = sigma; fwd_enc; bck_enc; counter })
    in
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
    else
      bind (f from_idx acc) (fun acc' -> iterate (from_idx + 1) to_idx acc' f)
  ;;

  let get_env (st : coq_context ref) : Environ.env in_context =
    { state = st; value = !st.coq_env }
  ;;

  let get_sigma (st : coq_context ref) : Evd.evar_map in_context =
    { state = st; value = !st.coq_ctx }
  ;;

  let get_fwd_enc (st : coq_context ref) : (term, E.t) Hashtbl.t in_context =
    { state = st; value = !st.fwd_enc }
  ;;

  let get_bck_enc (st : coq_context ref) : term B.t in_context =
    { state = st; value = !st.bck_enc }
  ;;

  let get_counter (st : coq_context ref) : E.t ref in_context =
    { state = st; value = !st.counter }
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

  let sandbox (m : 'a mm) (st : coq_context ref) : 'a in_context =
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

  (* syntax *)
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

    val ( let$+ )
      :  (Environ.env -> Evd.evar_map -> 'a)
      -> ('a -> 'b mm)
      -> 'b mm

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

  (** increments and returns counter *)
  let next (st : coq_context ref) : E.t mm =
    let c : E.t ref = !st.counter in
    let new_val : E.t = E.next !c in
    !st.counter := new_val;
    return new_val
  ;;

  let encode (st : coq_context ref) (k : term) : E.t mm =
    match F.find_opt !st.fwd_enc k with
    | None ->
      (* map to next encoding and return *)
      let open Syntax in
      let* (next_enc : E.t) = next st in
      F.add !st.fwd_enc k next_enc;
      B.add !st.bck_enc next_enc k;
      return next_enc
    | Some enc -> return enc
  ;;

  (* FIXME: need to parameterize this error too *)
  exception InvalidDecodeKey of E.t

  (** dual to [encode] except we cannot handle new values *)
  let decode (st : coq_context ref) (k : E.t) : term mm =
    match B.find_opt !st.bck_enc k with
    (* FIXME: need to parameterize this error too *)
    | None -> raise (InvalidDecodeKey k)
    | Some enc -> return enc
  ;;
end

(* let make_wrapper : MebiMonad.MkWrapper.wrapper MebiMonad.in_context = *)

(*********************************************************************)
(*** Wrappper ********************************************************)
(*********************************************************************)

module type S = sig
  module MM : MEBI_MONAD

  (* val fwd_map : (module Hashtbl.S with type key = EConstr.t) *)

  type wrapper =
    { fwd_enc : (module Hashtbl.S with type key = EConstr.t)
    ; bck_enc : (module Hashtbl.S with type key = MM.E.t)
    }

  type 'a in_wrapper =
    { state : wrapper ref
    ; value : 'a
    }

  type 'a ii = wrapper ref -> 'a in_wrapper
end

module Wrapper (Mebi : MEBI_MONAD) : S = struct
  module MM = Mebi

  (* let fwd_map = let f = MM.make_constr_tbl in f MM.in_context *)

  type wrapper =
    { fwd_enc : (module Hashtbl.S with type key = EConstr.t)
    ; bck_enc : (module Hashtbl.S with type key = MM.E.t)
    }

  type 'a in_wrapper =
    { state : wrapper ref
    ; value : 'a
    }

  type 'a ii = wrapper ref -> 'a in_wrapper

  (** [run] initializes the wrapper and is called at the beginning. *)
  (* let run (x : 'a ii) : 'a = let fwd_enc = MM.make_constr_tbl in *)
end
