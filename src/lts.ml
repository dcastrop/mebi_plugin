type raw_flat_lts = (string * string * string * string option) list
type raw_nested_lts = (string * (string * string list) list) list

type raw_transitions =
  | Flat of (raw_flat_lts * string list option)
  | Nested of (raw_nested_lts * string list option)

type transition =
  { id : int
  ; from : string
  ; label : string
  ; destination : string
  ; info : string option
  }

module Transitions = Set.Make (struct
    type t = transition

    let compare a b = compare a.id b.id
  end)

module States = Set.Make (struct
    type t = string

    let compare a b = String.compare a b
  end)

type lts =
  { init : string option
  ; transitions : Transitions.t
  ; states : States.t
  ; info : Utils.model_info option
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
    let tabs : string = str_tabs _params.tabs
    and tabs' : string = str_tabs (_params.tabs + 2) in
    Printf.sprintf
      "%s%s"
      (if _params.no_leading_tab then "" else tabs)
      (let normal_pstr : string =
         (* Printf.sprintf "(%s --<%s>--> %s)" t.from t.label t.destination *)
         Printf.sprintf
           "%s\n%s--<%s>-->\n%s%s\n"
           t.from
           tabs'
           t.label
           tabs'
           t.destination
       and detail_pstr : string =
         Printf.sprintf
           "#%d %s\n%s--<%s>-->\n%s%s\n"
           t.id
           t.from
           tabs'
           t.label
           tabs'
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

  let states
    ?(params : Params.pstr = Fmt (Params.Default.fmt ()))
    (ss : States.t)
    : string
    =
    if States.is_empty ss
    then "[ ] (empty)"
    else (
      (* increment tab of inner elements of set *)
      let _params : Params.fmt = Params.handle params in
      let _params' : Params.fmt = inc_tab _params
      and tabs : string = str_tabs _params.tabs in
      let tabs' : string = str_tabs _params'.tabs in
      Printf.sprintf
        "%s[%s%s]"
        (if _params.no_leading_tab then "" else tabs)
        (States.fold
           (fun (s : string) (acc : string) ->
             Printf.sprintf "%s%s%s\n" acc tabs' s)
           ss
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
      "{ %s; %s; %s; %s\n%s}"
      (Printf.sprintf
         "\n%sinitial state: %s"
         tabs'
         (match the_lts.init with
          | None -> "None"
          | Some init' -> init'))
      (Printf.sprintf
         "\n%smeta info: %s"
         tabs'
         (Utils.PStr.model_info the_lts.info))
      (Printf.sprintf
         "\n%sstates: %s"
         tabs'
         (states ~params:(Fmt _params') the_lts.states))
      (Printf.sprintf
         "\n%stransitions: %s"
         tabs'
         (transitions ~params:(Fmt _params') the_lts.transitions))
      tabs
  ;;
end

module Create = struct
  type transition_params =
    | Of of (int * string * string * string * string option)

  let transition (params : transition_params) : transition =
    match params with
    | Of (id, from, label, destination, info) ->
      { id; from; label; destination; info }
  ;;

  let lts
    ?(init : string option)
    ?(info : Utils.model_info option)
    (raw : raw_transitions)
    : lts
    =
    let (transitions, states) : Transitions.t * States.t =
      match raw with
      | Flat (raw', states) ->
        let states' : States.t =
          match states with
          | None -> States.empty
          | Some states' -> States.of_list states'
        in
        ( List.fold_left
            (fun (acc : Transitions.t)
              ((from, label, destination, constr_info) :
                string * string * string * string option) ->
              Transitions.add
                (transition
                   (Of
                      ( Transitions.cardinal acc
                      , from
                      , label
                      , destination
                      , constr_info )))
                acc)
            Transitions.empty
            raw'
        , states' )
      | Nested (raw', states) ->
        let states' : States.t =
          match states with
          | None -> States.empty
          | Some states' -> States.of_list states'
        in
        ( List.fold_left
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
                              , destination
                              , None )))
                        acc'')
                    acc'
                    destinations)
                acc
                actions)
            Transitions.empty
            raw'
        , states' )
    in
    { init; transitions; states; info }
  ;;
end
