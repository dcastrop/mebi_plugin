type raw_transitions =
  | Flat of (string * string * string) list
  | Nested of (string * (string * string list) list) list

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
  }

module Make = struct
  type transition_params = Of of (int * string * string * string)

  let transition (params : transition_params) : transition =
    match params with
    | Of (id, from, label, destination) -> { id; from; label; destination }
  ;;

  let lts ?(init : string option) (raw : raw_transitions) : lts =
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
    { init; transitions }
  ;;
end
