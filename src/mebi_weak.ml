type t =
  | Option of Mebi_wrapper.Enc.t
  | Custom of Mebi_wrapper.Enc.t * Mebi_wrapper.Enc.t

let eq x y : bool =
  match x, y with
  | Option x, Option y -> Mebi_wrapper.Enc.equal x y
  | Custom (x1, x2), Custom (y1, y2) ->
    Mebi_wrapper.Enc.equal x1 y1 && Mebi_wrapper.Enc.equal x2 y2
  | _, _ -> false
;;

let to_string (x : t) : string Mebi_wrapper.mm =
  let open Mebi_wrapper in
  let open Mebi_wrapper.Syntax in
  match x with
  | Option label_enc ->
    let* label_dec : EConstr.t = decode label_enc in
    let label_enc : string = Enc.to_string label_enc in
    let label_dec : string = econstr_to_string label_dec in
    let s : string = Printf.sprintf "Option (%s) -> %s" label_enc label_dec in
    return s
  | Custom (tau_enc, label_enc) ->
    let* tau_dec : EConstr.t = decode tau_enc in
    let tau_enc : string = Enc.to_string tau_enc in
    let tau_dec : string = econstr_to_string tau_dec in
    let* label_dec : EConstr.t = decode label_enc in
    let label_enc : string = Enc.to_string label_enc in
    let label_dec : string = econstr_to_string label_dec in
    let a : string = Printf.sprintf "- tau (%s) -> %s" tau_enc tau_dec in
    let b : string = Printf.sprintf "- label (%s) -> %s" label_enc label_dec in
    let s : string = Printf.sprintf "Custom\n%s\n%s" a b in
    return s
;;
