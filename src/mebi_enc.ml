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

  (* val counter : t ref *)
  (* val next : t -> t *)
  (* val get_next : unit -> t *)
  val reset : unit -> unit
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val to_string : t -> string

  module B : Hashtbl.S with type key = t

  exception InvalidDecodeKey of (t * EConstr.t B.t)

  val encode : t F.t -> EConstr.t B.t -> EConstr.t -> t
  val decode_opt : EConstr.t B.t -> t -> EConstr.t option
  val decode : EConstr.t B.t -> t -> EConstr.t

  (* *)
  val fwd_to_list : t F.t -> (EConstr.t * t) list
  val bck_to_list : EConstr.t B.t -> (t * EConstr.t) list
end

module type S = sig
  module F : Hashtbl.S with type key = EConstr.t

  type t

  val init : t
  val next : t -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val to_string : t -> string
end

module Make (Enc : S) : ENCODING_TYPE = struct
  include Enc

  let cache : t ref = ref init
  let counter : t ref = cache

  let get_next () : t =
    let x = !counter in
    counter := next !counter;
    x
  ;;

  let reset () = cache := init

  module B : Hashtbl.S with type key = t = Hashtbl.Make (struct
      include Enc
    end)

  let encode (fwd : t F.t) (bck : EConstr.t B.t) (k : EConstr.t) : t =
    Log.trace "Mebi_wrapper.IntEncoding.encode";
    match F.find_opt fwd k with
    | None ->
      (* map to next encoding and return *)
      let next_enc : t = get_next () in
      F.add fwd k next_enc;
      B.add bck next_enc k;
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

  exception InvalidDecodeKey of (t * EConstr.t B.t)

  let decode_opt (bck : EConstr.t B.t) (k : t) : EConstr.t option =
    B.find_opt bck k
  ;;

  let decode (bck : EConstr.t B.t) (k : t) : EConstr.t =
    match B.find_opt bck k with
    | None -> raise (InvalidDecodeKey (k, bck))
    | Some enc -> enc
  ;;

  let fwd_to_list : t F.t -> (EConstr.t * t) list =
    fun (x : t F.t) ->
    List.sort (fun (_, a) (_, b) -> compare a b) (List.of_seq (F.to_seq x))
  ;;

  let bck_to_list : EConstr.t B.t -> (t * EConstr.t) list =
    fun (x : EConstr.t B.t) ->
    List.sort (fun (a, _) (b, _) -> compare a b) (List.of_seq (B.to_seq x))
  ;;
end
