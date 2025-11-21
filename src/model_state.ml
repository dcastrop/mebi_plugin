type t =
  { enc : Mebi_setup.Enc.t
  ; pp : string option
  }

let equal (a : t) (b : t) = Mebi_setup.Enc.equal a.enc b.enc
let compare (a : t) (b : t) = Mebi_setup.Enc.compare a.enc b.enc
let hash (x : t) : int = Mebi_setup.Enc.hash x.enc

open Utils.Strfy

let to_string ?(args : style_args = record_args ()) (x : t) : string =
  let enc : string = Mebi_setup.Enc.to_string x.enc in
  let open Utils.Strfy in
  match x.pp with
  | None -> record ~args [ "enc", enc ]
  | Some pp -> record ~args [ "enc", enc; "pp", pp ]
;;
