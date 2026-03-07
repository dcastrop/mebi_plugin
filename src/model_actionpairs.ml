module Make
    (Log : Logger.S)
    (State : sig
       type t

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val equal : t -> t -> bool *)
       (* val compare : t -> t -> int *)
       (* val hash : t -> int *)
     end)
    (States : sig
       include Set.S with type elt = State.t

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val add_to_opt : State.t -> t option -> t *)

       exception StateHasNoOrigin of (State.t * t * t)

       (* val origin_of_state : State.t -> t -> t -> int *)
       (* val has_shared_origin : t -> t -> t -> bool *)
     end)
    (Action : sig
       type t

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       val equal : t -> t -> bool
       (* val compare : t -> t -> int *)
       (* val hash : t -> int *)
       (* val wk_equal : t -> t -> bool *)
       (* val is_silent : t -> bool *)
       (* val shorter_annotation : t -> t -> t *)
     end)
    (ActionPair : sig
       type t = Action.t * States.t

       val json : ?as_elt:bool -> t -> Yojson.t

       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       val compare : t -> t -> int
       val shorter_annotation : t -> t -> t
       (* val try_update : t -> t list -> t option * t list *)
       (* val merge_lists : t list -> t list -> t list *)
     end) : sig
  include Set.S with type elt = ActionPair.t

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val destinations : t -> States.t

  exception IsEmpty

  val shortest_annotation : t -> ActionPair.t
  val merge_list : t -> ActionPair.t list -> t
end = struct
  module Set_ : Set.S with type elt = ActionPair.t = Set.Make (ActionPair)
  include Set_

  include
    Json.Set.Make
      (Log)
      (struct
        module Set = Set_

        let name = "ActionPairs"
        let json = ActionPair.json
      end)

  let destinations (x : t) : States.t =
    to_list x
    |> List.fold_left
         (fun (acc : States.t) ((a, b) : ActionPair.t) -> States.union acc b)
         States.empty
  ;;

  exception IsEmpty

  (** returns the action in [x] that has the {e shortest} annotation (where [None] is treated as 0).
  *)
  let shortest_annotation (x : t) : ActionPair.t =
    match to_list x with
    | [] -> raise IsEmpty
    | h :: tl -> List.fold_left ActionPair.shorter_annotation h tl
  ;;

  let merge_list : t -> ActionPair.t list -> t =
    List.fold_left (fun (acc : t) ((a, s) : ActionPair.t) ->
      (* NOTE: merge destinations of equal actions *)
      let matching =
        filter (fun ((b, t) : ActionPair.t) -> Action.equal a b) acc
      in
      if is_empty matching
      then add (a, s) acc
      else (
        (* NOTE: update states of each matching *)
        let acc = diff acc matching in
        matching
        |> to_list
        |> List.map (fun (_, t) -> a, States.union s t)
        |> of_list
        |> union acc))
  ;;
end
