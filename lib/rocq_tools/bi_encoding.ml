module type S = sig
  type enc

  module F : Hashtbl.S with type key = EConstr.t
  module B : Hashtbl.S with type key = enc

  type maps =
    { fwd : enc F.t
    ; bck : EConstr.t B.t
    }

  val the_maps : maps ref option ref
  val reset : unit -> unit
  val initialize : unit -> unit

  exception MapsNotInitialised of unit

  val get_the_maps : unit -> maps ref
  val fwdmap : unit -> enc F.t
  val bckmap : unit -> EConstr.t B.t

  exception EncodingNotFound of EConstr.t

  val get_encoding : EConstr.t -> enc
  val encode : EConstr.t -> enc
  val encoded : EConstr.t -> bool

  exception DecodingNotFound of enc

  val get_econstr : enc -> EConstr.t

  exception CannotDecode of enc

  val decode : enc -> EConstr.t
  val decode_opt : enc -> EConstr.t option
  val opt_decode : enc option -> EConstr.t option
  val decode_map : 'a B.t -> 'a F.t
  val encode_map : 'a F.t -> 'a B.t
  val to_list : unit -> (enc * EConstr.t) list
end

module Make (Log : Logger.S) (Ctx : Rocq_context.S) (Enc : Encoding.S) :
  S with type enc = Enc.t = struct
  type enc = Enc.t

  module F : Hashtbl.S with type key = EConstr.t = Hashtbl.Make (struct
      type t = EConstr.t

      let equal (a : t) (b : t) : bool =
        Log.trace __FUNCTION__;
        EConstr.eq_constr !(Ctx.sigma ()) a b
      ;;

      let hash (x : t) : int =
        Log.trace __FUNCTION__;
        Constr.hash
          (EConstr.to_constr ~abort_on_undefined_evars:false !(Ctx.sigma ()) x)
      ;;
    end)

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
    Log.trace __FUNCTION__;
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
      (* Log.trace ~__FUNCTION__ "Err: EncodingNotFound"; *)
      (* NOTE: map to the next encoding and return *)
      let new_enc : Enc.t = Enc.incr () in
      F.add (fwdmap ()) x new_enc;
      B.add (bckmap ()) new_enc x;
      (* NOTE: make sure to update the maps (keep progress) *)
      (* Log.thing ~__FUNCTION__ Trace "new enc" new_enc (Of Enc.to_string); *)
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

  let opt_decode : Enc.t option -> EConstr.t option = function
    | None -> None
    | Some x -> (try Some (decode x) with CannotDecode _ -> None)
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

  (* *)
  let to_list () : (Enc.t * EConstr.t) list =
    Log.trace __FUNCTION__;
    B.to_seq (bckmap ())
    |> List.of_seq
    |> List.sort (fun (a, _) (b, _) -> Enc.compare a b)
  ;;
end
