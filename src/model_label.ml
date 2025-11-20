(** (encoding, (?cached_decoding, ?is_silent)) *)
type t =
  { enc : Mebi_setup.Enc.t
  ; pp : string option
  ; is_silent : bool option
  }

let equal (a : t) (b : t) = Mebi_setup.Enc.equal a.enc b.enc
let compare (a : t) (b : t) = Mebi_setup.Enc.compare a.enc b.enc
let hash (x : t) = Mebi_setup.Enc.hash x.enc

let to_string
      ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
      (x : t)
  : string
  =
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
