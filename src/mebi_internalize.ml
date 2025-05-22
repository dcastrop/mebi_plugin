(** An [INTERNAL_TYPE] must have functions for [eq], [compare] and [hash]. *)
module type INTERNAL_TYPE = sig
  type t

  val eq : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
end

(** Same as [INTERNAL_TYPE] but with a method returning the [next] encoding. *)
module type ENCODING_TYPE = sig
  type t

  val eq : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val next : t -> t
end

(** Consists of an [INTERNAL_TYPE] and an [ENCODING_TYPE]. *)
module type INTERNAL_PAIR = sig
  module O : INTERNAL_TYPE
  module E : ENCODING_TYPE

  type origin_t = O.t
  type encode_t = E.t
end

(** Signature of the functor [MkInternals]. *)
module type S = sig
  (* module MM : Mebi_monad *)
  module P : INTERNAL_PAIR
  module F : Hashtbl.S with type key = P.origin_t
  module B : Hashtbl.S with type key = P.encode_t

  type wrapper =
    { fwd_enc : P.encode_t F.t
    ; bck_enc : P.origin_t B.t
    ; counter : P.encode_t ref
    ; coq : Mebi_monad.coq_context ref
    }

  type 'a in_wrapper =
    { state : wrapper ref
    ; value : 'a
    }

  type 'a ii = wrapper ref -> 'a in_wrapper

  val return : 'a -> 'a ii
  val bind : 'a ii -> ('a -> 'b ii) -> 'b ii
  val counter : wrapper ref -> P.encode_t ii
  val next : wrapper ref -> P.encode_t ii

  (******************)
  (** Mebi monad ****)
  (******************)
  (* FIXME: this probably isn't correct *)
  (* type 'a mm = wrapper ref -> 'a Mebi_monad.mm *)

  (* TODO: add functions that elevate to mm *)
end

(** Signature of the functor [MkWrapper].
    Wraps around modules produced by functors of sig [S]. *)
module type Wrapper = sig
  module I : S

  type origin_t = I.P.origin_t
  type encode_t = I.P.encode_t
  type wrapper = I.wrapper
  type 'a iw = 'a I.in_wrapper
  type 'a ii = 'a I.ii

  val return : 'a -> 'a ii
  val bind : 'a ii -> ('a -> 'b ii) -> 'b ii
  val ( let|* ) : 'a ii -> ('a -> 'b ii) -> 'b ii
end

(*********************************************************************)
(*** Functors ********************************************************)
(*********************************************************************)

(* module MkInternalType : INTERNAL_TYPE = struct type t

   let eq t1 t2 = failwith "method 'eq' must be provided" let compare t1 t2 =
   failwith "method 'compare' must be provided" let hash t = failwith "method
   'hash' must be provided" end *)

(* module MkEncodingType (InternalType : INTERNAL_TYPE) : ENCODING_TYPE = struct
   module T = InternalType

   type t = T.t

   let eq t1 t2 = T.eq t1 t2 let compare t1 t2 = T.compare t1 t2 let hash t =
   T.hash t let next t = failwith "method 'next' must be provided" end *)

(* let make_int_internal_type (fn_eq : int -> int -> bool) (fn_compare : int ->
   int -> int) (fn_hash : int -> int) : (module INTERNAL_TYPE with type t = int)
   = let module NewInternal = struct type t = int

   let eq t1 t2 = Int.equal t1 t2 let compare t1 t2 = Int.compare t1 t2 let hash
   t = Int.hash t end in (module NewInternal : INTERNAL_TYPE with type t = int)
   ;; *)

(* let make_int_encode_type (fn_eq : int -> int -> bool) (fn_compare : int ->
   int -> int) (fn_hash : int -> int) (fn_next : int -> int) : (module
   ENCODING_TYPE with type t = int) = let module NewEncoding = struct type t =
   int

   let eq t1 t2 = Int.equal t1 t2 let compare t1 t2 = Int.compare t1 t2 let hash
   t = Int.hash t let next t = t + 1 end in (module NewEncoding : ENCODING_TYPE
   with type t = int) ;; *)

(** *)
module MkInternalPair (Original : INTERNAL_TYPE) (Encoding : ENCODING_TYPE) :
  INTERNAL_PAIR = struct
  module O = Original
  module E = Encoding

  type origin_t = O.t
  type encode_t = E.t
end

module MkInternals
    (Pair : INTERNAL_PAIR)
    (FwdMap : Hashtbl.S with type key = Pair.origin_t)
    (BckMap : Hashtbl.S with type key = Pair.encode_t) : S = struct
  module P = Pair
  module F = FwdMap
  module B = BckMap

  type wrapper =
    { fwd_enc : P.encode_t F.t
    ; bck_enc : P.origin_t B.t
    ; counter : P.encode_t ref
    ; coq : Mebi_monad.coq_context ref
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

  (** returns the value of the counter *)
  let counter (w : wrapper ref) : P.encode_t ii =
    let c : P.encode_t ref = !w.counter in
    return !c
  ;;

  (** increments and returns counter *)
  let next (w : wrapper ref) : P.encode_t ii =
    let c : P.encode_t ref = !w.counter in
    let new_val : P.encode_t = P.E.next !c in
    !w.counter := new_val;
    (* { state = w; value = new_val } *)
    return new_val
  ;;
end

module MkWrapper (Internals : S) : Wrapper = struct
  module I = Internals
  module F = I.F
  module B = I.B

  type origin_t = I.P.origin_t
  type encode_t = I.P.encode_t
  type wrapper = I.wrapper
  type 'a iw = 'a I.in_wrapper
  type 'a ii = 'a I.ii

  let return (x : 'a) : 'a ii = I.return x
  let bind (x : 'a ii) (f : 'a -> 'b ii) : 'b ii = I.bind x f
  let ( let|* ) = I.bind

  let encode (w : wrapper ref) (k : origin_t) : encode_t ii =
    match F.find_opt !w.fwd_enc k with
    | None ->
      (* map to next encoding and return *)
      let|* (next_enc : encode_t) = I.next w in
      F.add !w.fwd_enc k next_enc;
      B.add !w.bck_enc next_enc k;
      return next_enc
    | Some enc -> return enc
  ;;

  (* FIXME: need to parameterize this error too *)
  exception InvalidDecodeKey of encode_t

  (** dual to [encode] except we cannot handle new values *)
  let decode (w : wrapper ref) (k : encode_t) : origin_t ii =
    match B.find_opt !w.bck_enc k with
    (* FIXME: need to parameterize this error too *)
    | None -> raise (InvalidDecodeKey k)
    | Some enc -> return enc
  ;;
end

(*********************************************************************)
(*** Implementations *************************************************)
(*********************************************************************)

(* module TermInternal : INTERNAL_TYPE = struct type t = EConstr.t

   let eq t1 t2 = EConstr.eq_constr end *)

module IntEncoding : ENCODING_TYPE = struct
  type t = int

  let eq t1 t2 = Int.equal t1 t2
  let compare t1 t2 = Int.compare t1 t2
  let hash t = Int.hash t
  let next t = t + 1
end
