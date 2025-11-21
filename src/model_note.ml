type t =
  { from : Model_state.t
  ; via : Model_label.t
  }

let equal (a : t) (b : t) : bool =
  let f = Model_state.equal in
  let g = Model_label.equal in
  f a.from b.from && g a.via b.via
;;

let compare (a : t) (b : t) : int =
  let f = Model_state.compare in
  let g = Model_label.compare in
  Int.compare (f a.from b.from) (g a.via b.via)
;;

(***********************************************************************)
(*** Annotation ********************************************************)
(***********************************************************************)

type annotation = t list

let annotation_equal (a : annotation) (b : annotation) : bool =
  List.equal equal a b
;;

let annotation_compare (a : annotation) (b : annotation) : int =
  List.compare compare a b
;;

let annotation_is_empty : annotation -> bool = function
  | [] -> true
  | _ -> false
;;

let add_note (a : t) (bs : annotation) : annotation =
  if List.mem a bs then bs else a :: bs
;;

let union_annotation (a : annotation) (b : annotation) : annotation =
  List.fold_left
    (fun (acc : annotation) (x : t) -> if List.mem x acc then acc else x :: acc)
    a
    b
;;

(***********************************************************************)
(*** Annotations *******************************************************)
(***********************************************************************)

type annotations = annotation list

let annotations_equal (a : annotations) (b : annotations) : bool =
  List.equal annotation_equal a b
;;

let annotations_compare (a : annotations) (b : annotations) : int =
  List.compare annotation_compare a b
;;

let annotations_is_empty : annotations -> bool = function
  | [] -> true
  | [ [] ] -> true
  | _ -> false
;;

let union_annotations (a : annotations) (b : annotations) : annotations =
  List.fold_left
    (fun (acc : annotations) (x : annotation) ->
      if List.mem x acc then acc else x :: acc)
    a
    b
;;

let add_annotation (a : annotation) (bs : annotations) : annotations =
  if List.mem a bs then bs else a :: bs
;;

open Utils.Strfy

let to_string ?(args : style_args = style_args ()) (x : t) : string =
  let goto : string = Mebi_setup.Enc.to_string x.from.enc in
  let via : string = Mebi_setup.Enc.to_string x.via.enc in
  Printf.sprintf "<State (%s) Via (%s)>" goto via
;;

let annotation_to_string ?(args : style_args = style_args ()) (x : annotation)
  : string
  =
  list ~args:{ args with name = Some "Annotation" } to_string x
;;

let annotations_to_string ?(args : style_args = style_args ()) (x : annotations)
  : string
  =
  list ~args:{ args with name = Some "Annotations" } annotation_to_string x
;;
