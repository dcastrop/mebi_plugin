module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (Label : Label.S with type t = Base.t Label.t')
    (Labels : sig
       include Set.S with type elt = Label.t
     end)
    (Annotation : sig
       type t
     end)
    (Action : sig
       type t =
         { label : Label.t
         ; annotation : Annotation.t option
         ; constructor_trees : Base.Trees.t
         }

       val json : ?as_elt:bool -> t -> Yojson.t
       val compare : t -> t -> int
     end) : sig
  include Set.S with type elt = Action.t

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val labelled : t -> Label.t -> t
  val labels : t -> Labels.t
end = struct
  module Set_ : Set.S with type elt = Action.t = Set.Make (Action)
  include Set_

  include
    Json.Set.Make
      (Log)
      (struct
        module Set = Set_

        let name = "Actions"
        let json = Action.json
      end)

  let labelled (xs : t) (y : Label.t) : t =
    Log.trace __FUNCTION__;
    filter (fun ({ label; _ } : Action.t) -> Label.equal label y) xs
  ;;

  let labels (xs : t) : Labels.t =
    Log.trace __FUNCTION__;
    fold
      (fun ({ label; _ } : Action.t) : (Labels.t -> Labels.t) ->
        Labels.add label)
      xs
      Labels.empty
  ;;
end
