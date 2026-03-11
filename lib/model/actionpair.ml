module type S = sig
  type action
  type states
  type t = action * states

  val compare : t -> t -> int
  val shorter_annotation : t -> t -> t
  val try_update : t -> t list -> t option * t list
  val merge_lists : t list -> t list -> t list
  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
end

module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (States : States.S)
    (Annotation : Annotation.S)
    (Action :
       Action.S
       with type annotation = Annotation.t
        and type trees = Base.Trees.t) :
  S
  with type action = Action.t
   and type states = States.t
   and type t = Action.t * States.t = struct
  type action = Action.t
  type states = States.t
  type t = action * states

  let compare ((a, x) : t) ((b, y) : t) : int =
    Utils.compare_chain [ Action.compare a b; States.compare x y ]
  ;;

  let shorter_annotation ((a, xs) : t) ((b, ys) : t) : t =
    match
      Int.compare
        (Annotation.opt_length a.annotation)
        (Annotation.opt_length b.annotation)
    with
    | 1 -> b, ys
    | _ -> a, xs
  ;;

  (** [try_update x a] returns [None, a] when [x] cannot be used to update a pre-existing tuple in [a], and [Some z, a'] where [z] is the updated tuple in [a] which has been removed in [a'].
      (* TODO:REFACTOR -- this is the reason so many functor params *) *)
  let try_update ((xaction, xdestinations) : t) (a : t list) : t option * t list
    =
    Log.trace __FUNCTION__;
    let f : Annotation.t option * Annotation.t option -> Annotation.t option =
      function
      | None, None -> None
      | None, y -> y
      | x, None -> x
      | Some x, Some y -> Some (Annotation.shorter x y)
    in
    List.fold_left
      (fun ((updated_opt, acc) :
             (Action.t * States.t) option * (Action.t * States.t) list)
        ((yaction, ydestinations) : Action.t * States.t) ->
        match updated_opt with
        | Some opt -> Some opt, (yaction, ydestinations) :: acc
        | None ->
          if
            Action.wk_equal xaction yaction
            && States.equal xdestinations ydestinations
          then (
            let annotation : Annotation.t option =
              f (yaction.annotation, xaction.annotation)
            in
            let zaction : Action.t =
              { label = yaction.label
              ; annotation
              ; trees = Base.Trees.union yaction.trees xaction.trees
              }
            in
            Some (zaction, ydestinations), acc)
          else None, (yaction, ydestinations) :: acc)
      (None, [])
      a
  ;;

  (** [merge_lists a b] merges elements of [b] into [a], either by updating an element in [a] with additional annotation for a saturation tuple that describes the same action-destination, or in the case that the saturation tuple is not described within [a] by inserting it within [a].
  *)
  let rec merge_lists (a : t list) : t list -> t list =
    Log.trace __FUNCTION__;
    function
    | [] -> a
    | h :: tl ->
      let (a : (Action.t * States.t) list) =
        match try_update h a with
        | None, a -> h :: a
        | Some updated, a -> updated :: a
      in
      merge_lists a tl
  ;;

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "ActionPair"

        let json ?as_elt (x : t) : Yojson.t =
          `Assoc
            [ "action", Action.json (fst x)
            ; "destinations", States.json (snd x)
            ]
        ;;
      end)
end
