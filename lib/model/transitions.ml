module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type t = Base.t)
    (Label : Label.S with type t = Base.t Label.t')
    (Labels : sig
       include Set.S with type elt = Label.t
     end)
    (Annotation : sig
       type t
     end)
    (Transition :
       Transition.S
       with type t = (State.t, Label.t, Base.Tree.t, Annotation.t) Transition.t') : sig
  include Set.S with type elt = Transition.t

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val labels : t -> Labels.t
end = struct
  module Set_ : Set.S with type elt = Transition.t = Set.Make (Transition)
  include Set_

  include
    Json.Set.Make
      (Log)
      (struct
        module Set = Set_

        let name = "Transitions"
        let json = Transition.json
      end)

  let labels (xs : t) : Labels.t =
    Log.trace __FUNCTION__;
    fold
      (fun ({ label; _ } : Transition.t) : (Labels.t -> Labels.t) ->
        Labels.add label)
      xs
      Labels.empty
  ;;
end
