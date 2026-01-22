type t =
  { from : Model_state.t
  ; via : Model_label.t
  ; using : Mebi_constr.Tree.t list
  ; goto : Model_state.t
  }

let create
      (from : Model_state.t)
      (via : Model_label.t)
      (using : Mebi_constr.Tree.t list)
      (goto : Model_state.t)
  : t
  =
  { from; via; using; goto }
;;

let equal (a : t) (b : t) : bool =
  let f = Model_state.equal in
  let g = Model_label.equal in
  let h = List.equal Mebi_constr.Tree.equal in
  f a.from b.from && g a.via b.via && h a.using b.using && f a.goto b.goto
;;

let compare (a : t) (b : t) : int =
  let f = Model_state.compare in
  let g = Model_label.compare in
  let h = List.compare Mebi_constr.Tree.compare in
  Utils.compare_chain
    [ f a.from b.from; g a.via b.via; h a.using b.using; f a.goto b.goto ]
;;

(***********************************************************************)
(*** Annotation ********************************************************)
(***********************************************************************)

type annotation =
  { this : t
  ; next : annotation option
  }

let rec annotation_equal
          ({ this = athis; next = anext } : annotation)
          ({ this = bthis; next = bnext } : annotation)
  : bool
  =
  equal athis bthis && Option.equal annotation_equal anext bnext
;;

let rec annotation_compare
          ({ this = athis; next = anext } : annotation)
          ({ this = bthis; next = bnext } : annotation)
  : int
  =
  match compare athis bthis with
  | 0 -> Option.compare annotation_compare anext bnext
  | n -> n
;;

(** assumes the annotation is the "root" i.e., in the base case just mirrors a transition
*)
let annotation_is_empty : annotation -> bool = function
  | { this; next = None } -> true
  | _ -> false
;;

let rec annotation_depth : annotation -> int = function
  | { this; next = None } -> 1
  | { this; next = Some next } -> 1 + annotation_depth next
;;

let shorter_annotation (a : annotation) (b : annotation) : annotation =
  match Int.compare (annotation_depth a) (annotation_depth b) with
  | -1 -> a
  | _ -> b
;;

let rec exists (x : t) : annotation -> bool = function
  | { this; next = None } -> false
  | { this; next = Some next } -> if equal x this then true else exists x next
;;

let add_note (x : t) (y : annotation) : annotation =
  let rec add_note (a : t) : annotation -> annotation = function
    | { this; next = None } -> { this; next = Some { this = a; next = None } }
    | { this; next = Some next } -> { this; next = Some (add_note a next) }
  in
  if exists x y then y else add_note x y
;;

let rec exists_label (x : Model_label.t) : annotation -> bool = function
  | { this; next = None } -> false
  | { this; next = Some next } ->
    if Model_label.equal x this.via then true else exists_label x next
;;

let rec last : annotation -> t = function
  | { this; next = None } -> this
  | { next = Some next; _ } -> last next
;;

exception Model_note_CannotDropLast of annotation

let rec drop_last : annotation -> annotation = function
  | { this; next = None } ->
    raise (Model_note_CannotDropLast { this; next = None })
  | { this; next = Some { next = None; _ } } -> { this; next = None }
  | { this = a; next = Some next } -> { this = a; next = Some (drop_last next) }
;;

(* | { this; next = Some next; } -> drop_last next *)

(***********************************************************************)
(*** Annotations *******************************************************)
(***********************************************************************)

module Annotations = Set.Make (struct
    type t = annotation

    let compare = annotation_compare
  end)

(* type annotations = annotation list

   let annotations_equal (a : annotations) (b : annotations) : bool =
   List.equal annotation_equal a b
   ;;

   let annotations_compare (a : annotations) (b : annotations) : int =
   List.compare annotation_compare a b
   ;;

   let annotations_is_empty : annotations -> bool = function
   | [] -> true
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
   ;; *)

open Utils.Strfy

let to_string ?(args : style_args = style_args ()) (x : t) : string =
  let f : Mebi_setup.Enc.t -> string = Mebi_setup.Enc.to_string in
  let from : string = f x.from.enc in
  let goto : string = f x.goto.enc in
  let via : string = f x.via.enc in
  (* let constructors : string =
    Utils.Strfy.list
      Utils.Strfy.string
      (List.map Mebi_constr.Tree.to_string x.constructors)
  in *)
  (* Printf.sprintf "<State (%s) Via (%s) :: %s>" goto via constructors *)
  Printf.sprintf "<State (%s) Via (%s) Goto (%s)>" from via goto
;;

let annotation_to_string ?(args : style_args = style_args ()) (x : annotation)
  : string
  =
  let rec annotation_to_string ?(args : style_args = style_args ())
    : annotation -> string list
    = function
    | { this; next = None } -> [ to_string ~args this ]
    | { this; next = Some next } ->
      to_string ~args this :: annotation_to_string ~args next
  in
  list
    ~args:{ args with name = Some "Annotation" }
    (Of string)
    (annotation_to_string ~args x)
;;

let annotations_to_string
      ?(args : style_args = style_args ())
      (x : Annotations.t)
  : string
  =
  list
    ~args:{ args with name = Some "Annotations" }
    (Args annotation_to_string)
    (Annotations.to_list x)
;;

(* let annotations_to_string ?(args : style_args = style_args ()) (x : annotations)
  : string
  =
  list ~args:{ args with name = Some "Annotations" } annotation_to_string x
;; *)
