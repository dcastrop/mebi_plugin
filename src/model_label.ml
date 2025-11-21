(** (encoding, (?cached_decoding, ?is_silent)) *)
type t =
  { enc : Mebi_setup.Enc.t
  ; pp : string option
  ; is_silent : bool option
  }

(** [is_silent x] returns [true] if [x.silent] is [Some true], else [false]. *)
let is_silent (x : t) : bool = Option.cata (fun x -> x) false x.is_silent

let equal (a : t) (b : t) = Mebi_setup.Enc.equal a.enc b.enc
let compare (a : t) (b : t) = Mebi_setup.Enc.compare a.enc b.enc
let hash (x : t) = Mebi_setup.Enc.hash x.enc

open Utils.Strfy

let to_string ?(args : style_args = record_args ()) (x : t) : string =
  let enc : string = Mebi_setup.Enc.to_string x.enc in
  let open Utils.Strfy in
  let is_silent : string option =
    Option.cata (fun y -> Some (bool y)) None x.is_silent
  in
  match x.pp, is_silent with
  | None, None -> record ~args [ "enc", enc ]
  | Some pp, None -> record ~args [ "enc", enc; "pp", pp ]
  | None, Some y -> record ~args [ "enc", enc; "is_silent", y ]
  | Some pp, Some y -> record ~args [ "enc", enc; "pp", pp; "is_silent", y ]
;;
