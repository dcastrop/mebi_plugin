module type S = sig
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
end

module Make (Ctx : Rocq_context.SRocq_context) (E : Encoding.SEncoding) :
  S with module Enc = E and type Enc.t = E.t = struct
  module Enc : Encoding.SEncoding with type t = E.t = E

  module FwdMap : Hashtbl.S with type key = EConstr.t =
    Rocq_context.MakeEConstrMap (Ctx)

  module BckMap : Hashtbl.S with type key = Enc.t = Hashtbl.Make (Enc)

  type fwdmap = Enc.t FwdMap.t
  type bckmap = EConstr.t BckMap.t

  type maps =
    { fwd : fwdmap
    ; bck : bckmap
    }

  let the_maps () : maps ref =
    ref { fwd = FwdMap.create 0; bck = BckMap.create 0 }
  ;;

  let reset () : unit =
    Enc.reset ();
    the_maps () := { fwd = FwdMap.create 0; bck = BckMap.create 0 }
  ;;

  let fwdmap () : Enc.t FwdMap.t = !(the_maps ()).fwd
  let bckmap () : EConstr.t BckMap.t = !(the_maps ()).bck

  module F = FwdMap
  module B = BckMap

  (* *)
  let get_encoding : EConstr.t -> Enc.t = F.find (fwdmap ())

  let encode (x : EConstr.t) : Enc.t =
    try get_encoding x with
    | Not_found ->
      (* NOTE: map to the next encoding and return *)
      let next_enc : Enc.t = Enc.incr () in
      F.add (fwdmap ()) x next_enc;
      B.add (bckmap ()) next_enc x;
      next_enc
  ;;

  let encoded (x : EConstr.t) : bool = F.mem (fwdmap ()) x

  (* *)
  let get_econstr : Enc.t -> EConstr.t = B.find (bckmap ())

  exception CannotDecode of Enc.t

  let decode (x : Enc.t) : EConstr.t =
    try get_econstr x with Not_found -> raise (CannotDecode x)
  ;;

  let decode_opt (x : Enc.t) : EConstr.t option =
    try Some (decode x) with CannotDecode _ -> None
  ;;

  (* *)
  let decode_map (bmap : 'a B.t) : 'a F.t =
    let fmap : 'a F.t = F.create (B.length bmap) in
    B.iter (fun (k : Enc.t) (v : 'a) -> F.add fmap (decode k) v) bmap;
    fmap
  ;;

  let encode_map (fmap : 'a F.t) : 'a B.t =
    let bmap : 'a B.t = B.create (F.length fmap) in
    F.iter (fun (k : EConstr.t) (v : 'a) -> B.add bmap (encode k) v) fmap;
    bmap
  ;;

  (* *)
  let to_list () : (Enc.t * EConstr.t) list =
    B.to_seq (bckmap ())
    |> List.of_seq
    |> List.sort (fun (a, _) (b, _) -> Enc.compare a b)
  ;;

  (* *)
  let make_hashtbl : (module Hashtbl.S with type key = Enc.t) =
    (module Hashtbl.Make (struct
         include Enc
       end))
  ;;

  let make_set : (module Set.S with type elt = Enc.t) =
    (module Set.Make (struct
         include Enc
       end))
  ;;
end
