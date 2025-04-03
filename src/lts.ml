type raw_flat_lts = (string * string * string) list
type raw_nested_lts = (string * (string * string list) list) list

type raw_transitions =
  | Flat of raw_flat_lts
  | Nested of raw_nested_lts

type transition =
  { id : int
  ; from : string
  ; label : string
  ; destination : string
  }

module Transitions = Set.Make (struct
    type t = transition

    let compare a b = compare a.id b.id
  end)

type lts =
  { init : string option
  ; transitions : Transitions.t
  ; is_complete : bool
  }

module PStr = struct
  open Utils.Logging
  open Utils.Formatting
  open Utils

  let transition
    ?(params : Params.pstr = Fmt (Params.Default.fmt ()))
    (t : transition)
    : string
    =
    let _params : Params.fmt = Params.handle params in
    let tabs : string = str_tabs _params.tabs in
    Printf.sprintf
      "%s%s"
      (if _params.no_leading_tab then "" else tabs)
      (let normal_pstr : string =
         Printf.sprintf "(%s --<%s>--> %s)" t.from t.label t.destination
       and detail_pstr : string =
         Printf.sprintf
           "#%d (%s --<%s>--> %s)"
           t.id
           t.from
           t.label
           t.destination
       in
       match _params.params.kind with
       | Normal () -> normal_pstr
       | Details () -> detail_pstr
       | Debug () -> detail_pstr
       | Warning () -> detail_pstr)
  ;;

  let transitions
    ?(params : Params.pstr = Fmt (Params.Default.fmt ()))
    (ts : Transitions.t)
    : string
    =
    if Transitions.is_empty ts
    then "[ ] (empty)"
    else (
      (* increment tab of inner elements of set *)
      let _params : Params.fmt = Params.handle params in
      let _params' : Params.fmt = inc_tab _params
      and tabs : string = str_tabs _params.tabs in
      Printf.sprintf
        "%s[%s%s]"
        (if _params.no_leading_tab then "" else tabs)
        (Transitions.fold
           (fun (t : transition) (acc : string) ->
             Printf.sprintf
               "%s%s\n"
               acc
               (transition ~params:(Fmt (no_leading_tab false _params')) t))
           ts
           "\n")
        tabs)
  ;;

  let lts ?(params : Params.pstr = Fmt (Params.Default.fmt ())) (the_lts : lts)
    : string
    =
    (* increment tab of inner elements of fsm *)
    let _params : Params.fmt = Params.handle params in
    let _params' : Params.fmt = inc_tab ~by:2 _params
    and tabs : string = str_tabs _params.tabs
    and tabs' : string = str_tabs (_params.tabs + 1) in
    Printf.sprintf
      "{ %s; %s; %s\n%s}"
      (Printf.sprintf
         "\n%sinitial state: %s"
         tabs'
         (match the_lts.init with
          | None -> "None"
          | Some init' -> init'))
      (Printf.sprintf "\n%sis complete: %b" tabs' the_lts.is_complete)
      (Printf.sprintf
         "\n%stransitions: %s"
         tabs'
         (transitions ~params:(Fmt _params') the_lts.transitions))
      tabs
  ;;
end

module Create = struct
  type transition_params = Of of (int * string * string * string)

  let transition (params : transition_params) : transition =
    match params with
    | Of (id, from, label, destination) -> { id; from; label; destination }
  ;;

  let lts
    ?(init : string option)
    ?(is_complete : bool = true)
    (raw : raw_transitions)
    : lts
    =
    let transitions : Transitions.t =
      match raw with
      | Flat raw' ->
        List.fold_left
          (fun (acc : Transitions.t)
            ((from, label, destination) : string * string * string) ->
            Transitions.add
              (transition
                 (Of (Transitions.cardinal acc, from, label, destination)))
              acc)
          Transitions.empty
          raw'
      | Nested raw' ->
        List.fold_left
          (fun (acc : Transitions.t)
            ((from, actions) : string * (string * string list) list) ->
            List.fold_left
              (fun (acc' : Transitions.t)
                ((label, destinations) : string * string list) ->
                List.fold_left
                  (fun (acc'' : Transitions.t) (destination : string) ->
                    Transitions.add
                      (transition
                         (Of
                            ( Transitions.cardinal acc''
                            , from
                            , label
                            , destination )))
                      acc'')
                  acc'
                  destinations)
              acc
              actions)
          Transitions.empty
          raw'
    in
    { init; transitions; is_complete }
  ;;
end
