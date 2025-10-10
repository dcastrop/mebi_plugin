open Logging
(* open Mebi_setup *)

(********************************************)
(****** ENCODINGS ***************************)
(********************************************)

module type ENCODING_TYPE = sig
  module F : Hashtbl.S with type key = EConstr.t

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

  val encode : t F.t -> EConstr.t Tbl.t -> EConstr.t -> t

  exception InvalidDecodeKey of (t * EConstr.t Tbl.t)

  val decode_opt : EConstr.t Tbl.t -> t -> EConstr.t option
  val decode : EConstr.t Tbl.t -> t -> EConstr.t
end

(**********************************)
(****** INTEGER ENCODING **********)
(**********************************)

module IntEncoding =
functor
  (FwdMap : Hashtbl.S with type key = EConstr.t)
  ->
  struct
    module F = FwdMap

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

    let encode (fwd : t F.t) (bck : EConstr.t Tbl.t) (k : EConstr.t) : t =
      Log.trace "Mebi_wrapper.IntEncoding.encode";
      match F.find_opt fwd k with
      | None ->
        (* map to next encoding and return *)
        let next_enc : t = !counter in
        counter := !counter + 1;
        F.add fwd k next_enc;
        Tbl.add bck next_enc k;
        Log.debug
          (Printf.sprintf
             "Mebi_wrapper.IntEncoding.encode, new encoding: %s"
             (to_string next_enc));
        next_enc
      | Some enc ->
        Log.debug
          (Printf.sprintf
             "Mebi_wrapper.IntEncoding.encode -- already encoded as (%s)"
             (to_string enc));
        enc
    ;;

    exception InvalidDecodeKey of (t * EConstr.t Tbl.t)

    let decode_opt (bck : EConstr.t Tbl.t) (k : t) : EConstr.t option =
      Tbl.find_opt bck k
    ;;

    let decode (bck : EConstr.t Tbl.t) (k : t) : EConstr.t =
      match Tbl.find_opt bck k with
      | None -> raise (InvalidDecodeKey (k, bck))
      | Some enc -> enc
    ;;
  end
