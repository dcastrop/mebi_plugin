(*********************************************************************)
(*** Mebi Monad ******************************************************)
(*********************************************************************)

module type MEBI_MONAD = sig
  type coq_context =
    { coq_env : Environ.env
    ; coq_ctx : Evd.evar_map
    }

  type 'a in_context =
    { state : coq_context ref
    ; value : 'a
    }

  type 'a mm = coq_context ref -> 'a in_context

  (* hashtbl *)
  val make_constr_tbl
    :  coq_context ref
    -> (module Hashtbl.S with type key = EConstr.t) in_context

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
end

(*********************************************************************)
(*** Internalize *****************************************************)
(*********************************************************************)

(** Same as [INTERNAL_TYPE] but with a method returning the [next] encoding. *)
module type ENCODING_TYPE = sig
  type t

  val eq : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val next : t -> t
  val make_hashtbl : (module Hashtbl.S with type key = t)
end

(** Signature of the functor [MkInternals]. *)
module type S = sig
  module MM : MEBI_MONAD
  module E : ENCODING_TYPE
  module F : Hashtbl.S with type key = EConstr.t

  type t = E.t
  type term = F.key

  type wrapper =
    { fwd_enc : t F.t
    ; bck_enc : (t, term) Hashtbl.t
    ; counter : t ref
    ; coq : MM.coq_context ref
    }

  type 'a in_wrapper =
    { state : wrapper ref
    ; value : 'a
    }

  type 'a ii = wrapper ref -> 'a in_wrapper

  val return : 'a -> 'a ii
  val bind : 'a ii -> ('a -> 'b ii) -> 'b ii
  val ( let|* ) : 'a ii -> ('a -> 'b ii) -> 'b ii
  val counter : wrapper ref -> t ii
  val next : wrapper ref -> t ii
  val encode : wrapper ref -> term -> t ii
  val decode : wrapper ref -> t -> term ii

  (******************)
  (** Mebi monad ****)
  (******************)

  type 'a in_context = 'a MM.in_context
  type 'a mm = 'a MM.mm

  (* val unpack : 'a MM.mm -> 'a ii *)

  (* TODO: add functions that elevate to mm *)

  val origin_eq : wrapper ref -> term -> term -> bool ii
  val origin_compare : wrapper ref -> term -> term -> int ii
  val origin_hash : wrapper ref -> term -> int ii
end

(** Signature of the functor [MkWrapper].
    Wraps around modules produced by functors of sig [S]. *)
module type Wrapper = sig
  module I : S

  type t = I.t
  type wrapper = I.wrapper
  type 'a iw = 'a I.in_wrapper
  type 'a ii = 'a I.ii
end

(*********************************************************************)
(*** Functors ********************************************************)
(*********************************************************************)

module MkInternals
    (Mebi : MEBI_MONAD)
    (Enc : ENCODING_TYPE)
    (FwdMap : Hashtbl.S with type key = EConstr.t) : S = struct
  module MM = Mebi
  module E = Enc
  module F = FwdMap

  type t = E.t
  type term = F.key

  type wrapper =
    { fwd_enc : t F.t
    ; bck_enc : (t, term) Hashtbl.t
    ; counter : t ref
    ; coq : MM.coq_context ref
    }

  type 'a in_wrapper =
    { state : wrapper ref
    ; value : 'a
    }

  type 'a ii = wrapper ref -> 'a in_wrapper

  let return (x : 'a) : 'a ii = fun st -> { state = st; value = x }
  [@@inline always]
  ;;

  let bind (x : 'a ii) (f : 'a -> 'b ii) : 'b ii =
    fun st ->
    let a = x st in
    f a.value a.state
  [@@inline always]
  ;;

  let ( let|* ) = bind

  (** returns the value of the counter *)
  let counter (w : wrapper ref) : t ii =
    let c : t ref = !w.counter in
    return !c
  ;;

  (** increments and returns counter *)
  let next (w : wrapper ref) : t ii =
    let c : t ref = !w.counter in
    let new_val : t = E.next !c in
    !w.counter := new_val;
    return new_val
  ;;

  let encode (w : wrapper ref) (k : term) : t ii =
    match F.find_opt !w.fwd_enc k with
    | None ->
      (* map to next encoding and return *)
      let|* (next_enc : t) = next w in
      F.add !w.fwd_enc k next_enc;
      Hashtbl.add !w.bck_enc next_enc k;
      return next_enc
    | Some enc -> return enc
  ;;

  (* FIXME: need to parameterize this error too *)
  exception InvalidDecodeKey of t

  (** dual to [encode] except we cannot handle new values *)
  let decode (w : wrapper ref) (k : t) : term ii =
    match Hashtbl.find_opt !w.bck_enc k with
    (* FIXME: need to parameterize this error too *)
    | None -> raise (InvalidDecodeKey k)
    | Some enc -> return enc
  ;;

  (******************)
  (** Mebi monad ****)
  (******************)

  type 'a in_context = 'a MM.in_context
  type 'a mm = 'a MM.mm

  (* let unpack (x:'a MM.mm) : 'a ii = let a = x (ref {}) in return a.value
     ;; *)

  let origin_eq (w : wrapper ref) t1 t2 =
    let|* enc1 = encode w t1 in
    let|* enc2 = encode w t2 in
    return (E.eq enc1 enc2)
  ;;

  (** compare terms using their encodings *)
  let origin_compare (w : wrapper ref) t1 t2 =
    let|* enc1 = encode w t1 in
    let|* enc2 = encode w t2 in
    return (E.compare enc1 enc2)
  ;;

  let origin_hash (w : wrapper ref) t =
    let|* enc = encode w t in
    return (E.hash enc)
  ;;
end

(* module MkWrapper (Internals : S) : Wrapper = struct module I = Internals (*
   module F = I.F *) (* module MM = I.MM *)

   type t = I.t type wrapper = I.wrapper type 'a iw = 'a I.in_wrapper type 'a ii
   = 'a I.ii end *)

(*********************************************************************)
(*** Implementations *************************************************)
(*********************************************************************)

module MebiMonad : MEBI_MONAD = struct
  type coq_context =
    { coq_env : Environ.env
    ; coq_ctx : Evd.evar_map
    }

  type 'a in_context =
    { state : coq_context ref
    ; value : 'a
    }

  type 'a mm = coq_context ref -> 'a in_context

  let make_constr_tbl (st : coq_context ref)
    : (module Hashtbl.S with type key = EConstr.t) in_context
    =
    let module Constrtbl =
      Hashtbl.Make (struct
        type t = EConstr.t

        let equal t1 t2 = EConstr.eq_constr !st.coq_ctx t1 t2

        let hash t =
          Constr.hash
            (EConstr.to_constr
               ?abort_on_undefined_evars:(Some false)
               !st.coq_ctx
               t)
        ;;
      end)
    in
    { state = st
    ; value = (module Constrtbl : Hashtbl.S with type key = EConstr.t)
    }
  ;;

  let run (x : 'a mm) : 'a =
    let env = Global.env () in
    let sigma = Evd.from_env env in
    let a = x (ref { coq_env = env; coq_ctx = sigma }) in
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
end

module IntEncoding : ENCODING_TYPE = struct
  type t = int

  let eq t1 t2 = Int.equal t1 t2
  let compare t1 t2 = Int.compare t1 t2
  let hash t = Int.hash t
  let next t = t + 1

  let make_hashtbl : (module Hashtbl.S with type key = t) =
    let eqf = eq in
    let hashf = hash in
    let module Tbl =
      Hashtbl.Make (struct
        type t = int

        let equal t1 t2 = eqf t1 t2
        let hash t = hashf t
      end)
    in
    (module Tbl : Hashtbl.S with type key = t)
  ;;
end

let make_internals (module MM : MEBI_MONAD) (module E : ENCODING_TYPE)
  : (module S)
  =
  (* let mk_fwd_map : (module Hashtbl.S with type key = EConstr.t) MM.mm = let
     open MM.Syntax in let* (f : (module Hashtbl.S with type key = EConstr.t)) =
     MM.make_constr_tbl in MM.return f in *)
  (* in *)
  (* let fwd_map =  *)
  let open MM.Syntax in
  let (f : (module Hashtbl.S with type key = EConstr.t)) = MM.make_constr_tbl in
  let module I = MkInternals (MM) (E) in
  (module I : S)
;;

(* let make_wrapper (module MM:MEBI_MONAD) (module E:ENCODING_TYPE) : (module
   Wrapper) =

   let module W : Wrapper = (struct module I = end) *)

(* module TermInternal (Mebi:MEBI_MONAD): INTERNAL_TYPE = struct module MM =
   Mebi

   type t = EConstr.t

   let eq st t1 t2 = EConstr.eq_constr (MM.get_sigma st) t1 t2 let eq st t1 t2 =
   EConstr.eq_constr (MM.get_sigma st) t1 t2 end *)
