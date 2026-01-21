(***********************************************************************)
module Log : Logger.LOGGER_TYPE = Logger.MkDefault ()

let () = Log.Config.configure_output Debug false
let () = Log.Config.configure_output Trace false
(***********************************************************************)

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
    Log.trace __FUNCTION__;
    let x : t = !counter in
    counter := next !counter;
    x
  ;;

  let reset () =
    Log.trace __FUNCTION__;
    cache := init
  ;;

  module B : Hashtbl.S with type key = t = Hashtbl.Make (struct
      include Enc
    end)

  let encode (fwd : t F.t) (bck : EConstr.t B.t) (k : EConstr.t) : t =
    Log.trace __FUNCTION__;
    match F.find_opt fwd k with
    | None ->
      (* NOTE: map to next encoding and return *)
      let next_enc : t = get_next () in
      F.add fwd k next_enc;
      B.add bck next_enc k;
      Log.thing ~__FUNCTION__ Debug "new enc" next_enc (Of to_string);
      next_enc
    | Some enc ->
      Log.thing ~__FUNCTION__ Debug "found enc" enc (Of to_string);
      enc
  ;;

  exception InvalidDecodeKey of (t * EConstr.t B.t)

  let decode_opt (bck : EConstr.t B.t) (k : t) : EConstr.t option =
    Log.trace __FUNCTION__;
    B.find_opt bck k
  ;;

  let decode (bck : EConstr.t B.t) (k : t) : EConstr.t =
    Log.trace __FUNCTION__;
    match B.find_opt bck k with
    | None -> raise (InvalidDecodeKey (k, bck))
    | Some enc -> enc
  ;;

  let fwd_to_list (x : t F.t) : (EConstr.t * t) list =
    Log.trace __FUNCTION__;
    List.sort (fun (_, a) (_, b) -> compare a b) (List.of_seq (F.to_seq x))
  ;;

  let bck_to_list (x : EConstr.t B.t) : (t * EConstr.t) list =
    Log.trace __FUNCTION__;
    List.sort (fun (a, _) (b, _) -> compare a b) (List.of_seq (B.to_seq x))
  ;;
end
