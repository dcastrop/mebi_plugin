open Logging

module Info = struct
  module Coq = struct
    type t = Mebi_wrapper.E.t * (string * string list)

    let to_string ((enc, (dec, names)) : t) : string =
      Printf.sprintf
        "(%s => %s, [%s])"
        (Mebi_wrapper.E.to_string enc)
        dec
        (match names with
         | [] -> ""
         | nh :: nt ->
           List.fold_left
             (fun (acc : string) (nt_str : string) ->
               Printf.sprintf "%s, %s" acc nt_str)
             (Printf.sprintf "%s" nh)
             nt)
    ;;
  end

  type t =
    { is_complete : bool
    ; bound : int
    ; num_terminals : int
    ; num_labels : int
    ; num_states : int
    ; num_edges : int
    ; coq_info : Coq.t list option
    ; weak_info : string list option
    }

  let merge
        ?(bound : int = -1)
        ?(num_terminals : int = -1)
        ?(num_labels : int = -1)
        ?(num_states : int = -1)
        ?(num_edges : int = -1)
        (i1 : t)
        (i2 : t)
    : t
    =
    Log.debug "model.Info.merge";
    { is_complete = i1.is_complete && i2.is_complete
    ; bound
    ; num_terminals
    ; num_labels
    ; num_states
    ; num_edges
    ; coq_info =
        (match i1.coq_info, i2.coq_info with
         | Some c1, Some c2 -> Some c1
         | _, _ -> None)
    ; weak_info =
        (match i1.weak_info, i2.weak_info with
         | Some s1, None -> Some s1
         | None, Some s2 -> Some s2
         | Some s1, Some s2 ->
           Some (List.merge (fun a b -> String.compare a b) s1 s2)
         | None, None -> None)
    }
  ;;

  let to_string ?(pstr : bool = false) ?(indents : int = 0) (i : t) : string =
    let outer, sep =
      if pstr
      then
        ( Printf.sprintf "\n%s " (Utils.str_tabs indents)
        , Printf.sprintf "\n%s" (Utils.str_tabs (indents + 1)) )
      else "", ""
    in
    Printf.sprintf
      "%s{| is complete: %b%s; bound: %i%s; num labels: %i%s; num states: \
       %i%s; num edges: %i%s; coq info: %s%s; silent info: %s%s|}"
      outer
      i.is_complete
      sep
      i.bound
      sep
      i.num_labels
      sep
      i.num_states
      sep
      i.num_edges
      sep
      (match i.coq_info with
       | None -> "None"
       | Some [] -> "[]"
       | Some (h :: t) ->
         if List.is_empty t
         then Coq.to_string h
         else
           List.fold_left
             (fun (acc : string) c_info ->
               Printf.sprintf "%s, %s" acc (Coq.to_string c_info))
             (Coq.to_string h)
             t)
      sep
      (match i.weak_info with
       | None -> "None"
       | Some ws -> Utils.pstr_string_list ws)
      outer
  ;;

  let opt_to_string ?(pstr : bool = false) ?(indents : int = 0) (i : t option)
    : string
    =
    match i with None -> "None" | Some i -> to_string ~pstr ~indents i
  ;;

  let pstr ?(skip_leading_tab : bool = false) ?(indents : int = 0) (t : t) =
    Printf.sprintf
      "%s%s"
      (if skip_leading_tab then "" else Utils.str_tabs indents)
      (to_string ~pstr:true ~indents t)
  ;;

  let opt_pstr
        ?(skip_leading_tab : bool = false)
        ?(indents : int = 0)
        (t : t option)
    =
    Printf.sprintf
      "%s%s"
      (if skip_leading_tab then "" else Utils.str_tabs indents)
      (opt_to_string ~pstr:true ~indents t)
  ;;

  let opt_is_complete (i : t option) : bool option =
    match i with None -> None | Some i -> Some i.is_complete
  ;;
end

module State = struct
  type t = Mebi_wrapper.E.t * string option

  let eq (s1 : t) (s2 : t) = Mebi_wrapper.E.eq (fst s1) (fst s2)
  let compare (s1 : t) (s2 : t) = Mebi_wrapper.E.compare (fst s1) (fst s2)
  let hash (t : t) = Mebi_wrapper.E.hash (fst t)

  let to_string
        ?(skip_leading_tab : bool = false)
        ?(indents : int = 0)
        ?(pstr : bool = false)
        (t : t)
    =
    let state_str = Mebi_wrapper.E.to_string (fst t) in
    match pstr with
    | false -> state_str
    | true ->
      let str_indent = Utils.str_tabs indents in
      Printf.sprintf
        "%s%s( encodingSTATE: %s\n%s; decodingSTATE: %s\n%s)"
        (if pstr then "\n" else "")
        (if skip_leading_tab then "" else str_indent)
        state_str
        str_indent
        (match snd t with None -> "None" | Some decoding -> decoding)
        str_indent
  ;;

  let pstr ?(skip_leading_tab : bool = false) ?(indents : int = 0) (t : t) =
    to_string ~pstr:true ~skip_leading_tab ~indents t
  ;;
end

module Action = struct
  module Label = struct
    (** (encoding, (?cached_decoding, ?is_silent)) *)
    type t = Mebi_wrapper.E.t * (string option * bool option)

    let to_string (t : t) = Mebi_wrapper.E.to_string (fst t)

    let pstr ?(indents : int = 0) (t : t) =
      Printf.sprintf "\n%s%s" (Utils.str_tabs indents) (to_string t)
    ;;

    let is_silent t = snd (snd t)
    let eq (t1 : t) (t2 : t) = Mebi_wrapper.E.eq (fst t1) (fst t2)
    let compare (t1 : t) (t2 : t) = Mebi_wrapper.E.compare (fst t1) (fst t2)
    let hash (t : t) = Mebi_wrapper.E.hash (fst t)
  end

  module MetaData = struct
    type t = string list

    let merge (m1 : t) (m2 : t) : t = List.append m1 m2
    let from_opt (t : t option) = match t with None -> None | Some t -> Some t

    let to_string (m : t) : string =
      match m with
      | [] -> "(No MetaData)"
      | h :: t ->
        Printf.sprintf
          "(%s)"
          (Printf.sprintf
             "[%s]"
             (List.fold_left
                (fun (acc : string) (i : string) ->
                  Printf.sprintf "%s; %s" acc i)
                h
                t))
    ;;

    let eq (m1 : t) (m2 : t) : bool =
      if Int.equal (List.length m1) (List.length m2)
      then
        List.for_all
          (fun ((i1, i2) : string * string) -> String.equal i1 i2)
          (List.combine m1 m2)
      else false
    ;;

    let eq_opt (m1 : t option) (m2 : t option) : bool =
      match m1, m2 with
      | None, None -> true
      | Some i1, Some i2 -> eq i1 i2
      | _, _ -> false
    ;;

    let compare (m1 : t) (m2 : t) : int =
      List.compare (fun a b -> String.compare a b) m1 m2
    ;;

    let compare_opt (m1 : t option) (m2 : t option) : int =
      match m1, m2 with
      | None, None -> 0
      | None, Some _ -> -1
      | Some _, None -> 1
      | Some i1, Some i2 -> compare i1 i2
    ;;
  end

  type t =
    { label : Label.t
    ; meta : MetaData.t
    ; mutable annos : annotations
    }

  and annotation_pair = State.t * t
  and annotation = annotation_pair list
  and annotations = annotation list

  let saturated ?(anno : annotation = []) (a : t) : t =
    { label = a.label
    ; meta = a.meta (* { a.meta with is_silent = Some true } *)
    ; annos = List.rev anno :: a.annos
    }
  ;;

  exception CannotMergeActionWithDifferentLabels of (t * t)

  let merge (a1 : t) (a2 : t) : t =
    if Label.eq a1.label a2.label
    then
      { a1 with
        meta = MetaData.merge a1.meta a2.meta
      ; annos =
          List.fold_left
            (fun (acc : annotations) (anno : annotation) ->
              if List.mem anno acc then acc else anno :: acc)
            a1.annos
            a2.annos
      }
    else raise (CannotMergeActionWithDifferentLabels (a1, a2))
  ;;

  let rec annotation_pair_to_string (p : annotation_pair) : string =
    Printf.sprintf "(%s, %s)" (State.to_string (fst p)) (to_string (snd p))

  and annotation_to_string (anno : annotation) : string =
    if List.is_empty anno
    then "[ ] (empty)"
    else
      Printf.sprintf
        "[%s]"
        (List.fold_left
           (fun (acc : string) (p : annotation_pair) ->
             Printf.sprintf "%s; %s" acc (annotation_pair_to_string p))
           (annotation_pair_to_string (List.hd anno))
           (List.tl anno))

  and annotations_to_string (annos : annotations) : string =
    if List.is_empty annos
    then "[ ] (empty)"
    else
      Printf.sprintf
        "[%s]"
        (List.fold_left
           (fun (acc : string) (anno : annotation) ->
             Printf.sprintf "%s; %s" acc (annotation_to_string anno))
           (annotation_to_string (List.hd annos))
           (List.tl annos))

  and to_string
        ?(skip_leading_tab : bool = false)
        ?(indents : int = 0)
        ?(pstr : bool = false)
        (a : t)
    =
    let label_str = Label.to_string a.label in
    match pstr with
    | false -> label_str
    | true ->
      let str_indent = Utils.str_tabs indents in
      Printf.sprintf
        "%s%s( encoding: %s\n\
         %s; decoding: %s\n\
         %s; silent: %s\n\
         %s; meta: %s\n\
         %s; annotations: %s\n\
         %s)"
        (if pstr then "\n" else "")
        (if skip_leading_tab then "" else str_indent)
        label_str
        str_indent
        (match fst (snd a.label) with
         | None -> "None"
         | Some decoding -> decoding)
        str_indent
        (match snd (snd a.label) with
         | None -> "None"
         | Some silent -> Bool.to_string silent)
        str_indent
        (MetaData.to_string a.meta)
        str_indent
        (annotations_to_string a.annos)
        str_indent
  ;;

  let pstr ?(indents : int = 0) (t : t) =
    Printf.sprintf
      "%s%s"
      (Utils.str_tabs indents)
      (to_string ~pstr:true ~indents t)
  ;;

  (* let annotate (a : t) (anno : annotation) : unit = a.annos <- anno :: a.annos *)

  exception ActionSilenceIsNone of t

  let is_silent (a : t) : bool =
    match snd (snd a.label) with
    | None -> raise (ActionSilenceIsNone a)
    | Some b -> b
  ;;

  let has_label (l : Label.t) (a : t) : bool = Label.eq l a.label

  let rec eq ?(annos : bool = true) ?(meta : bool = true) (a1 : t) (a2 : t)
    : bool
    =
    match a1, a2 with
    | ( { label = v1; meta = m1; annos = a1 }
      , { label = v2; meta = m2; annos = a2 } ) ->
      Label.eq v1 v2
      &&
      if meta
      then MetaData.eq m1 m2
      else if annos
      then annos_eq ~annos ~meta a1 a2
      else true

  and anno_eq
        ?(annos : bool = true)
        ?(meta : bool = true)
        (a1 : annotation)
        (a2 : annotation)
    : bool
    =
    match a1, a2 with
    | [], [] -> true
    | [], h :: t -> false
    | h :: t, [] -> false
    | (e1, s1) :: t1, (e2, s2) :: t2 ->
      State.eq e1 e2 && eq ~annos ~meta s1 s2 && anno_eq ~annos ~meta t1 t2

  and annos_eq
        ?(annos : bool = true)
        ?(meta : bool = true)
        (a1 : annotations)
        (a2 : annotations)
    : bool
    =
    match a1, a2 with
    | [], [] -> true
    | [], h :: t -> false
    | h :: t, [] -> false
    | h1 :: t1, h2 :: t2 ->
      anno_eq ~annos ~meta h1 h2 && annos_eq ~annos ~meta t1 t2
  ;;

  let rec compare ?(annos : bool = true) ?(meta : bool = true) (a1 : t) (a2 : t)
    : int
    =
    match a1, a2 with
    | ( { label = v1; meta = m1; annos = a1 }
      , { label = v2; meta = m2; annos = a2 } ) ->
      if Label.eq v1 v2
      then
        if meta
        then
          if MetaData.eq m1 m2
          then 0
          else if annos
          then
            if annos_eq ~annos ~meta a1 a2
            then 0
            else annos_compare ~annos ~meta a1 a2
          else MetaData.compare m1 m2
        else Label.compare v1 v2
      else Label.compare v1 v2

  and anno_compare
        ?(annos : bool = true)
        ?(meta : bool = true)
        (a1 : annotation)
        (a2 : annotation)
    : int
    =
    match a1, a2 with
    | [], [] -> 0
    | [], h :: t -> -1
    | h :: t, [] -> 1
    | (e1, s1) :: t1, (e2, s2) :: t2 ->
      Int.compare (State.compare e1 e2) (compare ~annos ~meta s1 s2)

  and annos_compare
        ?(annos : bool = true)
        ?(meta : bool = true)
        (a1 : annotations)
        (a2 : annotations)
    : int
    =
    match a1, a2 with
    | [], [] -> 0
    | [], h :: t -> -1
    | h :: t, [] -> 1
    | h1 :: t1, h2 :: t2 ->
      Int.compare
        (anno_compare ~annos ~meta h1 h2)
        (annos_compare ~annos ~meta t1 t2)
  ;;

  let hash (a : t) : int = match a with { label; _ } -> Label.hash label
end

module Transition = struct
  type t = State.t * Action.Label.t * State.t * Action.MetaData.t option

  let to_string (t : t) =
    match t with
    | from, l, dest, meta ->
      Printf.sprintf
        "(| %s ==( %s )=> %s // %s |)"
        (State.to_string from)
        (Action.Label.to_string l)
        (State.to_string dest)
        (match meta with None -> "-" | Some m -> Action.MetaData.to_string m)
  ;;

  let pstr ?(indents : int = 0) (t : t) =
    Printf.sprintf "%s%s" (Utils.str_tabs indents) (to_string t)
  ;;

  let eq (t1 : t) (t2 : t) =
    match t1, t2 with
    | (from1, a1, dest1, meta1), (from2, a2, dest2, meta2) ->
      State.eq from1 from2
      && Action.Label.eq a1 a2
      && State.eq dest1 dest2
      &&
        (match meta1, meta2 with
        | None, None -> true
        | None, Some _ -> false
        | Some _, None -> false
        | Some m1, Some m2 -> Action.MetaData.eq m1 m2)
  ;;

  let compare (t1 : t) (t2 : t) =
    match t1, t2 with
    | (from1, a1, dest1, meta1), (from2, a2, dest2, meta2) ->
      if State.eq from1 from2
      then
        if Action.Label.eq a1 a2
        then
          if State.eq dest1 dest2
          then
            if Action.MetaData.eq_opt meta1 meta2
            then 0
            else Action.MetaData.compare_opt meta1 meta2
          else State.compare dest1 dest2
        else Action.Label.compare a1 a2
      else State.compare from1 from2
  ;;
end

module Edge = struct
  type t = State.t * Action.t * State.t

  exception CannotMergeEdgeWithDifferentStates of (t * t)

  let merge (e1 : t) (e2 : t) : t =
    match e1, e2 with
    | (from1, action1, dest1), (from2, action2, dest2) ->
      if State.eq from1 from2 && State.eq dest1 dest2
      then from1, Action.merge action1 action2, dest2
      else raise (CannotMergeEdgeWithDifferentStates (e1, e2))
  ;;

  let to_string
        ?(skip_leading_tab : bool = false)
        ?(indents : int = 0)
        ?(pstr : bool = false)
        (t : t)
    =
    match t with
    | from, a, dest ->
      let from_str =
        State.to_string
          ~skip_leading_tab:false
          ~indents:(indents + 3)
          ~pstr
          from
      in
      let a_str =
        Action.to_string ~skip_leading_tab:false ~indents:(indents + 3) ~pstr a
      in
      let dest_str =
        State.to_string
          ~skip_leading_tab:false
          ~indents:(indents + 3)
          ~pstr
          dest
      in
      let str_indent = Utils.str_tabs (indents + 1) in
      let sep = if pstr then Printf.sprintf "\n%s ; " str_indent else "; " in
      Printf.sprintf
        "%s(| from: %s%saction: %s%sdest: %s%s|)"
        (if skip_leading_tab then "" else str_indent)
        from_str
        sep
        a_str
        sep
        dest_str
        (if pstr then Printf.sprintf "\n%s" str_indent else " ")
  ;;

  let pstr ?(skip_leading_tab : bool = false) ?(indents : int = 0) (t : t) =
    to_string ~skip_leading_tab ~indents ~pstr:true t
  ;;
end

(*********************************************************************)
(****** Collections **************************************************)
(*********************************************************************)

module States = Set.Make (struct
    type t = State.t

    let compare a b = State.compare a b
  end)

module Partition = Set.Make (States)

module Alphabet = Set.Make (struct
    type t = Action.Label.t

    let compare (a : t) (b : t) = Action.Label.compare a b
  end)

(* module ActionSet = Set.Make (struct
   type t = Action.t

   let compare (a : t) (b : t) = Action.compare ~ignore_annos:true a b
   end) *)

module Actions = Hashtbl.Make (struct
    type t = Action.t

    let equal t1 t2 = Action.eq t1 t2
    let hash t = Action.hash t
  end)

module Edges = Hashtbl.Make (struct
    type t = State.t

    let equal t1 t2 = State.eq t1 t2
    let hash t = State.hash t
  end)

(** used by LTS *)
module Transitions = Set.Make (struct
    type t = Transition.t

    let compare a b = Transition.compare a b
  end)

(*********************************************************************)
(****** Pairs ********************************************************)
(*********************************************************************)

module ActionPair = struct
  type t = Action.t * States.t

  let compare a b =
    match Action.compare (fst a) (fst b) with
    | 0 -> States.compare (snd a) (snd b)
    | n -> n
  ;;
end

module ActionPairs = Set.Make (struct
    type t = ActionPair.t

    let compare a b = ActionPair.compare a b
  end)

let action_pairs_to_list (aps : ActionPairs.t) : ActionPair.t list =
  ActionPairs.fold
    (fun (ap : ActionPair.t) (acc : ActionPair.t list) -> ap :: acc)
    aps
    []
;;

(*********************************************************************)
(****** Model Kinds **************************************************)
(*********************************************************************)

type t =
  | LTS of
      (State.t option
      * States.t
      * Alphabet.t
      * States.t
      * Transitions.t
      * Info.t option)
  | FSM of
      (State.t option
      * States.t
      * Alphabet.t
      * States.t
      * States.t Actions.t Edges.t
      * Info.t option)
(* | NestedStrList of
   (string option
   * string option
   * (string * (string * string list) list) list) *)

(*********************************************************************)
(****** Labels <-> Actions *******************************************)
(*********************************************************************)

let label_to_action
      ?(meta : Action.MetaData.t option = None)
      (l : Action.Label.t)
  : Action.t
  =
  match meta with
  | None -> { label = l; meta = []; annos = [] }
  | Some m -> { label = l; meta = m; annos = [] }
;;

let action_to_label (a : Action.t) : Action.Label.t = a.label

let action_list_to_label_list (aa : Action.t list) : Action.Label.t list =
  List.rev
    (List.fold_left
       (fun (acc : Action.Label.t list) (a : Action.t) ->
         action_to_label a :: acc)
       []
       aa)
;;

(*********************************************************************)
(****** Edges <-> Edge List ******************************************)
(*********************************************************************)

let edges_to_list (es : States.t Actions.t Edges.t) : Edge.t list =
  Edges.fold
    (fun (from : State.t) (aa : States.t Actions.t) (acc0 : Edge.t list) ->
      Actions.fold
        (fun (a : Action.t) (dests : States.t) (acc1 : Edge.t list) ->
          States.fold
            (fun (dest : State.t) (acc2 : Edge.t list) ->
              (from, a, dest) :: acc2)
            dests
            acc1)
        aa
        acc0)
    es
    []
;;

(*********************************************************************)
(****** Edges <-> Transitions ****************************************)
(*********************************************************************)

let edges_to_transitions (es : States.t Actions.t Edges.t) : Transitions.t =
  Edges.fold
    (fun (from : State.t) (aa : States.t Actions.t) (acc0 : Transitions.t) ->
      Actions.fold
        (fun (a : Action.t) (dests : States.t) (acc1 : Transitions.t) ->
          States.fold
            (fun (dest : State.t) (acc2 : Transitions.t) ->
              Transitions.add (from, a.label, dest, Some a.meta) acc2)
            dests
            acc1)
        aa
        acc0)
    es
    Transitions.empty
;;

let transitions_to_edges (ts : Transitions.t) : States.t Actions.t Edges.t =
  let es = Edges.create 0 in
  Transitions.iter
    (fun ((from, label, dest, meta) : Transition.t) ->
      let a = label_to_action ~meta label in
      match Edges.find_opt es from with
      | None ->
        Edges.add
          es
          from
          (Actions.of_seq (List.to_seq [ a, States.singleton dest ]))
      | Some aa ->
        (match Actions.find_opt aa a with
         | None -> Actions.add aa a (States.singleton dest)
         | Some dests -> Actions.replace aa a (States.add dest dests)))
    ts;
  es
;;

(*********************************************************************)
(****** Edges <-> Transition List ************************************)
(*********************************************************************)

let edges_to_transition_list (es : States.t Actions.t Edges.t)
  : Transition.t list
  =
  Edges.fold
    (fun (from : State.t)
      (aa : States.t Actions.t)
      (acc0 : Transition.t list) ->
      Actions.fold
        (fun (a : Action.t) (dests : States.t) (acc1 : Transition.t list) ->
          States.fold
            (fun (dest : State.t) (acc2 : Transition.t list) ->
              (from, a.label, dest, Some a.meta) :: acc2)
            dests
            acc1)
        aa
        acc0)
    es
    []
;;

let transition_list_to_edges (ts : Transition.t list)
  : States.t Actions.t Edges.t
  =
  let es = Edges.create 0 in
  List.iter
    (fun ((from, label, dest, meta) : Transition.t) ->
      let a = label_to_action ~meta label in
      match Edges.find_opt es from with
      | None ->
        Edges.add
          es
          from
          (Actions.of_seq (List.to_seq [ a, States.singleton dest ]))
      | Some aa ->
        (match Actions.find_opt aa a with
         | None -> Actions.add aa a (States.singleton dest)
         | Some dests -> Actions.replace aa a (States.add dest dests)))
    ts;
  es
;;

(*********************************************************************)
(****** Actions <-> Alphabet *****************************************)
(*********************************************************************)

let alphabet_from_actions
      ?(acc : Alphabet.t = Alphabet.empty)
      (aa : States.t Actions.t)
  : Alphabet.t
  =
  Actions.fold
    (fun (a : Action.t) (_ : States.t) (acc : Alphabet.t) ->
      Alphabet.add a.label acc)
    aa
    acc
;;

let alphabet_from_edges (es : States.t Actions.t Edges.t) : Alphabet.t =
  Edges.fold
    (fun (_ : State.t) (aa : States.t Actions.t) (acc : Alphabet.t) ->
      alphabet_from_actions ~acc aa)
    es
    Alphabet.empty
;;

let alphabet_from_transitions (ts : Transitions.t) : Alphabet.t =
  Transitions.fold
    (fun ((_from, a, _dest, meta) : Transition.t) (acc : Alphabet.t) ->
      Alphabet.add
        (* { label = a; meta = Action.MetaData.from_opt meta; annos = [] } *)
        a
        acc)
    ts
    Alphabet.empty
;;

(*********************************************************************)
(****** Add **********************************************************)
(*********************************************************************)

let add_action (aa : States.t Actions.t) (a : Action.t) (dest : State.t) : unit =
  match Actions.find_opt aa a with
  | None -> Actions.add aa a (States.singleton dest)
  | Some dests -> Actions.replace aa a (States.add dest dests)
;;

let add_edge
      (es : States.t Actions.t Edges.t)
      (from : State.t)
      (a : Action.t)
      (dest : State.t)
  : unit
  =
  match Edges.find_opt es from with
  | None ->
    (* let aa : States.t Actions.t = Actions.create 0 in
       Actions.add aa a (States.singleton dest);
       Edges.add es from aa *)
    Edges.add
      es
      from
      (Actions.of_seq (List.to_seq [ a, States.singleton dest ]))
  | Some aa -> add_action aa a dest
;;

(*********************************************************************)
(****** Get **********************************************************)
(*********************************************************************)

let get_num_labels ?(num : int = 0) (aa : States.t Actions.t) : int =
  Actions.fold
    (fun (_ : Action.t) (dests : States.t) (num : int) ->
      num + States.cardinal dests)
    aa
    num
;;

let get_num_edges ?(num : int = 0) (es : States.t Actions.t Edges.t) : int =
  Edges.fold
    (fun (_ : State.t) (aa : States.t Actions.t) (num : int) ->
      get_num_labels ~num aa)
    es
    num
;;

let get_actions (es : States.t Actions.t Edges.t) : States.t Actions.t =
  let aa : States.t Actions.t = Actions.create 0 in
  List.iter
    (fun ((_from, a, dests) : Edge.t) -> add_action aa a dests)
    (edges_to_list es);
  aa
;;

let get_actions_from (from : State.t) (es : States.t Actions.t Edges.t)
  : States.t Actions.t
  =
  match Edges.find_opt es from with None -> Actions.create 0 | Some aa -> aa
;;

let get_actions_of (a : Action.t) (es : States.t Actions.t Edges.t)
  : States.t Actions.t
  =
  let result : States.t Actions.t = Actions.create 0 in
  (match Actions.find_opt (get_actions es) a with
   | None -> ()
   | Some dests -> Actions.add result a dests);
  result
;;

let get_actions_with_label
      (l : Action.Label.t)
      (es : States.t Actions.t Edges.t)
  : States.t Actions.t
  =
  let result : States.t Actions.t = Actions.create 0 in
  List.iter
    (fun ((_from, a, dests) : Edge.t) ->
      if Action.has_label l a then add_action result a dests)
    (edges_to_list es);
  result
;;

let get_edges_with_label (l : Action.Label.t) (es : States.t Actions.t Edges.t)
  : States.t Actions.t Edges.t
  =
  let result : States.t Actions.t Edges.t = Edges.create 0 in
  List.iter
    (fun ((from, a, dests) : Edge.t) -> add_edge result from a dests)
    (List.filter
       (fun ((_from, a, _dests) : Edge.t) -> Action.has_label l a)
       (edges_to_list es));
  result
;;

let get_edges_from (from : State.t) (es : States.t Actions.t Edges.t)
  : States.t Actions.t Edges.t
  =
  let result : States.t Actions.t Edges.t = Edges.create 0 in
  (match Edges.find_opt es from with
   | None -> ()
   | Some aa -> Edges.add result from aa);
  result
;;

let get_destinations (aa : States.t Actions.t) : States.t =
  Actions.fold
    (fun (_ : Action.t) (dests : States.t) (acc : States.t) ->
      States.union acc dests)
    aa
    States.empty
;;

let get_reachable_blocks (aa : States.t Actions.t) (pi : Partition.t)
  : Partition.t
  =
  let dests = get_destinations aa in
  Partition.filter
    (fun (block : States.t) ->
      Bool.not (States.is_empty (States.inter block dests)))
    pi
;;

let get_reachable_blocks_opt (aa : States.t Actions.t) (pi : Partition.t)
  : Partition.t option
  =
  let blocks = get_reachable_blocks aa pi in
  if Partition.is_empty blocks then None else Some blocks
;;

let get_num_blocks (pi : Partition.t) : int =
  Partition.fold
    (fun (ss : States.t) (num : int) -> States.cardinal ss + num)
    pi
    0
;;

(*********************************************************************)
(****** Makers *******************************************************)
(*********************************************************************)

(* let handle_nested_str_list_to_edges ((init,silent,nested_edges):(string option
   * string option
   * (string * (string * string list) list) list)) : (State.t option * Alphabet.t * States.t * (States.t Actions.t Edges.t)) =

   ;; *)

(*********************************************************************)
(****** Merge ********************************************************)
(*********************************************************************)

let merge_actions (aa1 : States.t Actions.t) (aa2 : States.t Actions.t)
  : States.t Actions.t
  =
  let a1_keys = List.of_seq (Actions.to_seq_keys aa1) in
  let a2_keys = List.of_seq (Actions.to_seq_keys aa2) in
  (* let action_keys = List.append a1_keys a2_keys in *)
  let labels =
    Alphabet.union (alphabet_from_actions aa1) (alphabet_from_actions aa2)
  in
  let actions : States.t Actions.t = Actions.create 0 in
  Alphabet.iter
    (fun (l : Action.Label.t) ->
      match
        ( List.find_opt
            (fun (akey : Action.t) -> Action.Label.eq l akey.label)
            a1_keys
        , List.find_opt
            (fun (akey : Action.t) -> Action.Label.eq l akey.label)
            a2_keys )
      with
      | None, None -> (* impossible *) ()
      | None, Some a2 -> Actions.add actions a2 (Actions.find aa2 a2)
      | Some a1, None -> Actions.add actions a1 (Actions.find aa1 a1)
      | Some a1, Some a2 ->
        (* should me able to merge *)
        let dests = States.union (Actions.find aa1 a1) (Actions.find aa2 a2) in
        let merged_action = Action.merge a1 a2 in
        Actions.add actions merged_action dests)
    labels;
  actions
;;

let merge_edges
      (e1 : States.t Actions.t Edges.t)
      (e2 : States.t Actions.t Edges.t)
  : States.t Actions.t Edges.t
  =
  let from_states =
    States.union
      (States.of_seq (Edges.to_seq_keys e1))
      (States.of_seq (Edges.to_seq_keys e2))
  in
  let edges : States.t Actions.t Edges.t =
    Edges.create (States.cardinal from_states)
  in
  States.iter
    (fun (from : State.t) ->
      match Edges.find_opt e1 from, Edges.find_opt e2 from with
      | None, None -> () (* impossible ? *)
      | None, Some a2 -> Edges.add edges from a2
      | Some a1, None -> Edges.add edges from a1
      | Some a1, Some a2 ->
        let merged_actions = merge_actions a1 a2 in
        Edges.add edges from merged_actions)
    from_states;
  edges
;;

(*********************************************************************)
(****** Pretty-Strings ***********************************************)
(*********************************************************************)

let pstr_info ?(indents : int = 0) (info : Info.t) : string =
  Info.pstr ~indents info
;;

let pstr_info_opt ?(indents : int = 0) (info : Info.t option) : string =
  Info.opt_pstr ~indents info
;;

let pstr_label ?(indents : int = 0) (l : Action.Label.t) : string =
  Action.Label.pstr ~indents l
;;

let pstr_action ?(indents : int = 0) (a : Action.t) : string =
  Action.pstr ~indents a
;;

let pstr_alphabet
      ?(skip_leading_tab : bool = false)
      ?(indents : int = 0)
      ?(details : bool = false)
      (ls : Alphabet.t)
  : string
  =
  match Alphabet.is_empty ls with
  | true -> "[ ] (empty)"
  | false ->
    let str_indent = Utils.str_tabs indents in
    Printf.sprintf
      "%s[%s%s]"
      (if skip_leading_tab then "" else str_indent)
      (Alphabet.fold
         (fun (l : Action.Label.t) (acc : string) ->
           Printf.sprintf "%s%s\n" acc (pstr_label ~indents:(indents + 1) l))
         ls
         "")
      str_indent
;;

let pstr_state
      ?(skip_leading_tab : bool = false)
      ?(indents : int = 0)
      (s : State.t)
  : string
  =
  State.pstr ~skip_leading_tab ~indents s
;;

let pstr_state_opt
      ?(skip_leading_tab : bool = false)
      ?(indents : int = 0)
      (s : State.t option)
  : string
  =
  match s with
  | None -> "None"
  | Some s -> State.pstr ~skip_leading_tab ~indents s
;;

let pstr_states
      ?(skip_leading_tab : bool = false)
      ?(indents : int = 0)
      ?(details : bool = false)
      (ss : States.t)
  : string
  =
  match States.is_empty ss with
  | true -> "[ ] (empty)"
  | false ->
    let str_indent = Utils.str_tabs indents in
    Printf.sprintf
      "%s[%s%s]"
      (if skip_leading_tab then "" else str_indent)
      (States.fold
         (fun (s : State.t) (acc : string) ->
           Printf.sprintf
             "%s%s\n"
             acc
             (pstr_state ~skip_leading_tab:false ~indents:(indents + 1) s))
         ss
         "")
      str_indent
;;

let pstr_partition
      ?(skip_leading_tab : bool = false)
      ?(indents : int = 0)
      ?(details : bool = false)
      (ps : Partition.t)
  : string
  =
  match Partition.is_empty ps with
  | true -> "[ ] (empty)"
  | false ->
    let str_indent = Utils.str_tabs indents in
    Printf.sprintf
      "%s[%s%s]"
      (if skip_leading_tab then "" else str_indent)
      (Partition.fold
         (fun (ss : States.t) (acc : string) ->
           Printf.sprintf
             "%s%s\n"
             acc
             (pstr_states ~skip_leading_tab ~indents:(indents + 1) ss))
         ps
         "\n")
      str_indent
;;

let pstr_transition ?(indents : int = 0) (tr : Transition.t) : string =
  Transition.pstr ~indents tr
;;

let pstr_transitions
      ?(skip_leading_tab : bool = false)
      ?(indents : int = 0)
      ?(details : bool = false)
      (ts : Transitions.t)
  : string
  =
  match Transitions.is_empty ts with
  | true -> "[ ] (empty)"
  | false ->
    let str_indent = Utils.str_tabs indents in
    Printf.sprintf
      "%s[%s%s]"
      (if skip_leading_tab then "" else str_indent)
      (Transitions.fold
         (fun (tr : Transition.t) (acc : string) ->
           Printf.sprintf
             "%s%s\n"
             acc
             (pstr_transition ~indents:(indents + 1) tr))
         ts
         "\n")
      str_indent
;;

let pstr_edge
      ?(skip_leading_tab : bool = false)
      ?(indents : int = 0)
      (e : Edge.t)
  : string
  =
  Edge.pstr ~skip_leading_tab ~indents e
;;

let pstr_edges_from_a
      ?(skip_leading_tab : bool = false)
      ?(indents : int = 0)
      (from : State.t)
      (a : Action.t)
      (dests : States.t)
  : string
  =
  match States.is_empty dests with
  | true ->
    Printf.sprintf
      "%s => {[ %s => {[ (empty) ]} ]}\n"
      (pstr_state from)
      (pstr_action a)
  | false ->
    Printf.sprintf
      "%s"
      (States.fold
         (fun (dest : State.t) (acc : string) ->
           Printf.sprintf
             "%s%s\n"
             acc
             (pstr_edge ~skip_leading_tab:false ~indents (from, a, dest)))
         dests
         "")
;;

let pstr_edges_from
      ?(skip_leading_tab : bool = false)
      ?(indents : int = 0)
      (from : State.t)
      (aa : States.t Actions.t)
  : string
  =
  match Int.equal 0 (Actions.length aa) with
  | true ->
    Log.warning
      (Printf.sprintf
         "model.pstr_edges_from, FSM has empty outgoing edges from: %s"
         (pstr_state from));
    Printf.sprintf
      "(| from:%s => {| |} (empty)\n%s|)\n\n"
      (pstr_state ~skip_leading_tab:false ~indents:(indents + 1) from)
      (Utils.str_tabs (indents - 1))
  | false ->
    Actions.fold
      (fun (a : Action.t) (dests : States.t) (acc : string) ->
        Printf.sprintf
          "%s%s"
          acc
          (pstr_edges_from_a ~skip_leading_tab:false ~indents from a dests))
      aa
      ""
;;

let pstr_edges
      ?(skip_leading_tab : bool = false)
      ?(indents : int = 0)
      (es : States.t Actions.t Edges.t)
  : string
  =
  match Int.equal 0 (Edges.length es) with
  | true -> "{| |} (empty)"
  | false ->
    let str_indent = Utils.str_tabs indents in
    Printf.sprintf
      "%s{[\n%s%s]}"
      (if skip_leading_tab then "" else str_indent)
      (Edges.fold
         (fun (from : State.t) (aa : States.t Actions.t) (acc : string) ->
           Printf.sprintf
             "%s%s"
             acc
             (pstr_edges_from ~skip_leading_tab:false ~indents from aa))
         es
         "")
      str_indent
;;

(*********************************************************************)
(****** Info Utils ***************************************************)
(*********************************************************************)

let check_info (m : t) : unit =
  match
    match m with
    | LTS (_init, terminals, alphabet, states, transitions, info) ->
      (match info with
       | None -> None
       | Some info ->
         Some
           (List.combine
              [ "terminals"; "alphabet"; "states"; "transitions" ]
              (List.combine
                 [ States.cardinal terminals
                 ; Alphabet.cardinal alphabet
                 ; States.cardinal states
                 ; Transitions.cardinal transitions
                 ]
                 [ info.num_terminals
                 ; info.num_labels
                 ; info.num_states
                 ; info.num_edges
                 ])))
    | FSM (_init, terminals, alphabet, states, edges, info) ->
      (match info with
       | None -> None
       | Some info ->
         Some
           (List.combine
              [ "terminals"; "alphabet"; "states"; "edges" ]
              (List.combine
                 [ States.cardinal terminals
                 ; Alphabet.cardinal alphabet
                 ; States.cardinal states
                 ; get_num_edges edges
                 ]
                 [ info.num_terminals
                 ; info.num_labels
                 ; info.num_states
                 ; info.num_edges
                 ])))
    (* | NestedStrList _ -> None *)
  with
  | None -> ()
  | Some info_list ->
    List.iter
      (fun (name, (real_num, info_num)) ->
        if Int.equal real_num info_num
        then ()
        else
          Logging.Log.warning
            (Printf.sprintf
               "Model.check_info, discrepency found in num of \"%s\": expected \
                (%i) but counted (%i)"
               name
               info_num
               real_num))
      info_list
;;

let merge_action_pairs (ap : ActionPairs.t) (bp : ActionPairs.t) : ActionPairs.t
  =
  ActionPairs.fold
    (fun ((b, dests) : ActionPair.t) (acc0 : ActionPairs.t) ->
      match
        ActionPairs.fold
          (fun ((a, dests') : ActionPair.t)
            ((acc1a, acc1b) : Action.t list * ActionPairs.t) ->
            if
              Action.eq ~annos:false ~meta:false b a
              && States.equal
                   (States.union dests dests')
                   (States.inter dests dests')
            then a :: acc1a, acc1b
            else acc1a, ActionPairs.add (a, dests') acc1b)
          acc0
          ([], ActionPairs.empty)
      with
      | [], acc0 ->
        (* no other matches, add *)
        Logging.Log.debug
          (Printf.sprintf
             "model.merge_action_pairs, adding new (%s) "
             (Action.to_string b));
        ActionPairs.add (b, dests) acc0
      | a :: [], acc0 ->
        (* single match found, merge annotations *)
        Logging.Log.debug
          (Printf.sprintf
             "model.merge_action_pairs, merging (%s) with (%s)"
             (Action.to_string b)
             (Action.to_string a));
        ActionPairs.add
          ({ a with annos = List.append a.annos b.annos }, dests)
          acc0
      | multi, acc0 ->
        (* many matches found *)
        Logging.Log.debug
          (Printf.sprintf
             "model.merge_action_pairs, found (%i) action pairs matching named \
              action (%s).\n\
              TEMP: dropping dupes and adding as fresh"
             (List.length multi)
             (Action.to_string b));
        ActionPairs.add (b, dests) acc0)
    bp
    ap
;;
