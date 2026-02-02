(* module type S = sig
  module Enc : Encoding.SEncoding
  module FwdMap : Hashtbl.S with type key = EConstr.t
  module BckMap : Hashtbl.S with type key = Enc.t

  type fwdmap = Enc.t FwdMap.t
  type bckmap = EConstr.t BckMap.t

  type maps =
    { fwd : fwdmap
    ; bck : bckmap
    }

  val the_maps : unit -> maps ref
  val reset : unit -> unit

  (* *)
  val encode : EConstr.t -> Enc.t
  val encoded : EConstr.t -> bool

  exception CannotDecode of Enc.t

  val decode : Enc.t -> EConstr.t
  val decode_opt : Enc.t -> EConstr.t option
  val encode_map : 'a FwdMap.t -> 'a BckMap.t
  val decode_map : 'a BckMap.t -> 'a FwdMap.t
  val to_list : unit -> (Enc.t * EConstr.t) list

  (* *)

  val make_hashtbl : (module Hashtbl.S with type key = Enc.t)
  val make_set : (module Set.S with type elt = Enc.t)
end *)

module Make
    (Log : Logger.SLogger)
    (Ctx : Rocq_context.SRocq_context)
    (Enc : Encoding.SEncoding)
     (* :
        S with module Enc = E and type Enc.t = E.t *) =
struct
  (* module Enc : Encoding.SEncoding with type t = E.t = E *)

  module F : Hashtbl.S with type key = EConstr.t =
    Rocq_context.MakeEConstrMap (Ctx)

  module B : Hashtbl.S with type key = Enc.t = Hashtbl.Make (Enc)

  type maps =
    { fwd : Enc.t F.t
    ; bck : EConstr.t B.t
    }

  let the_maps : maps ref option ref = ref None

  let reset () : unit =
    Log.trace __FUNCTION__;
    Enc.reset ();
    let fwd : Enc.t F.t = F.create 0 in
    let bck : EConstr.t B.t = B.create 0 in
    the_maps := Some (ref { fwd; bck })
  ;;

  let initialize () : unit =
    match !the_maps with None -> reset () | Some _ -> ()
  ;;

  exception MapsNotInitialised of unit

  let get_the_maps () : maps ref =
    Log.trace __FUNCTION__;
    match !the_maps with None -> raise (MapsNotInitialised ()) | Some x -> x
  ;;

  let fwdmap () : Enc.t F.t =
    Log.trace __FUNCTION__;
    !(get_the_maps ()).fwd
  ;;

  let bckmap () : EConstr.t B.t =
    Log.trace __FUNCTION__;
    !(get_the_maps ()).bck
  ;;

  (* *)
  let to_list () : (Enc.t * EConstr.t) list =
    Log.trace __FUNCTION__;
    B.to_seq (bckmap ())
    |> List.of_seq
    |> List.sort (fun (a, _) (b, _) -> Enc.compare a b)
  ;;

  exception EncodingNotFound of EConstr.t

  (* *)
  let get_encoding (x : EConstr.t) : Enc.t =
    Log.trace __FUNCTION__;
    try F.find (fwdmap ()) x with Not_found -> raise (EncodingNotFound x)
  ;;

  let encode (x : EConstr.t) : Enc.t =
    Log.trace __FUNCTION__;
    try get_encoding x with
    | EncodingNotFound x ->
      Log.trace ~__FUNCTION__ "Err: EncodingNotFound";
      (* NOTE: map to the next encoding and return *)
      let new_enc : Enc.t = Enc.incr () in
      F.add (fwdmap ()) x new_enc;
      B.add (bckmap ()) new_enc x;
      (* NOTE: make sure to update the maps (keep progress) *)
      Log.thing ~__FUNCTION__ Trace "new enc" new_enc (Of Enc.to_string);
      new_enc
  ;;

  let encoded (x : EConstr.t) : bool =
    Log.trace __FUNCTION__;
    F.mem (fwdmap ()) x
  ;;

  exception DecodingNotFound of Enc.t

  (* *)
  let get_econstr (x : Enc.t) : EConstr.t =
    Log.trace __FUNCTION__;
    try B.find (bckmap ()) x with Not_found -> raise (DecodingNotFound x)
  ;;

  exception CannotDecode of Enc.t

  let decode (x : Enc.t) : EConstr.t =
    Log.trace __FUNCTION__;
    try get_econstr x with
    | DecodingNotFound x ->
      Log.thing ~__FUNCTION__ Trace "Err: DecodingNotFound" x (Of Enc.to_string);
      raise (CannotDecode x)
  ;;

  let decode_opt (x : Enc.t) : EConstr.t option =
    Log.trace __FUNCTION__;
    try Some (decode x) with CannotDecode _ -> None
  ;;

  (* *)
  let decode_map (bmap : 'a B.t) : 'a F.t =
    Log.trace __FUNCTION__;
    let fmap : 'a F.t = F.create (B.length bmap) in
    B.iter (fun (k : Enc.t) (v : 'a) -> F.add fmap (decode k) v) bmap;
    fmap
  ;;

  let encode_map (fmap : 'a F.t) : 'a B.t =
    Log.trace __FUNCTION__;
    let bmap : 'a B.t = B.create (F.length fmap) in
    F.iter (fun (k : EConstr.t) (v : 'a) -> B.add bmap (encode k) v) fmap;
    bmap
  ;;
end
