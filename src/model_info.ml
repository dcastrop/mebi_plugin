module Make
    (Log : Logger.S)
    (Enc : Encoding.S)
    (State : sig
       type t

       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
       val equal : t -> t -> bool
       val compare : t -> t -> int
       val hash : t -> int
     end)
    (States : sig
       include Set.S with type elt = State.t

       val add_to_opt : State.t -> t option -> t

       exception StateHasNoOrigin of (State.t * t * t)

       val origin_of_state : State.t -> t -> t -> int
       val has_shared_origin : t -> t -> t -> bool
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (Label : sig
       type t

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val hash : t -> int
       val is_silent : t -> bool
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (Labels : sig
       include Set.S with type elt = Label.t

       val non_silent : t -> t
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (Action : sig
       type t

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val hash : t -> int
       val wk_equal : t -> t -> bool
       val is_silent : t -> bool
       val is_labelled : Label.t -> t -> bool
       val shorter_annotation : t -> t -> t
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (Actions : sig
       include Set.S with type elt = Action.t

       val labelled : t -> Label.t -> t
       val labels : t -> Labels.t
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (ActionPair : sig
       type t = Action.t * States.t

       val compare : t -> t -> int
       val shorter_annotation : t -> t -> t
       val try_update : t -> t list -> t option * t list
       val merge_lists : t list -> t list -> t list
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (ActionPairs : sig
       include Set.S with type elt = ActionPair.t

       val destinations : t -> States.t

       exception IsEmpty

       val shortest_annotation : t -> ActionPair.t
       val merge_list : t -> ActionPair.t list -> t
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (ActionMap : sig
       include Hashtbl.S with type key = Action.t

       type t' = States.t t

       val update : t' -> Action.t -> States.t -> unit
       val destinations : t' -> States.t
       val reduce_by_label : t' -> Label.t -> t'
       val to_actions : t' -> Actions.t
       val to_actionpairs : t' -> ActionPairs.t
       val of_actionpairs : ActionPairs.t -> t'
       val merge : t' -> t' -> t'
       val json : ?as_elt:bool -> t' -> Yojson.t
       val to_string : ?pretty:bool -> t' -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t' -> unit
     end)
    (Edge : sig
       type t =
         { from : State.t
         ; goto : State.t
         ; action : Action.t
         }

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val is_silent : t -> bool
       val is_labelled : Label.t -> t -> bool
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end)
    (Edges : sig
       include Set.S with type elt = Edge.t

       val labelled : t -> Label.t -> t
       val json : ?as_elt:bool -> t -> Yojson.t
       val to_string : ?pretty:bool -> t -> string
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
     end) =
struct
  module Meta = struct
    type t =
      { is_complete : bool
      ; is_merged : bool
      ; bounds : bounds
      ; lts : lts list
      }

    and bounds =
      | States of int
      | Transitions of int

    and lts =
      { enc : Enc.t
      ; constructors : Rocq_bindings.constructor list
      }

    include
      Json.Thing.Make
        (Log)
        (struct
          type k = t

          let name = "Meta"

          let json ?as_elt (x : t) : Yojson.t =
            `Assoc
              [ "complete", `Bool x.is_complete
              ; "merged", `Bool x.is_merged
              ; ( "bounds"
                , match x.bounds with
                  | States i -> `Assoc [ "by", `String "states"; "num", `Int i ]
                  | Transitions i ->
                    `Assoc [ "by", `String "transitions"; "num", `Int i ] )
              ; ( "lts"
                , `List
                    ((* List.map Rocq_bindings.json x.lts *)
                     List.fold_left
                       (fun acc ({ enc; constructors } : lts) ->
                         `Assoc
                           [ "enc", Enc.json enc
                           ; ( "constructors"
                             , (* List.map Rocq_bindings.json constructors *)
                               `String "TODO" )
                           ]
                         :: acc)
                       []
                       x.lts) )
              ]
          ;;
        end)
  end

  type t =
    { meta : Meta.t option
    ; weak_labels : Labels.t
    }

  (** [merge a b] returns a new [t] with a union of [weak_labels] and [meta=None].
  *)
  let merge (a : t) (b : t) : t =
    { meta = None; weak_labels = Labels.union a.weak_labels b.weak_labels }
  ;;

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "Info"

        let json ?as_elt (x : t) : Yojson.t =
          `Assoc [ "meta", `Assoc []; "weak labels", `List [] ]
        ;;
      end)
end
