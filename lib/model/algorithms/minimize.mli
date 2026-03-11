module Make : (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type base = Base.t)
    (States : States.S with type elt = State.t)
    (Label : Label.S with type base = Base.t)
    (Labels : Labels.S with type elt = Label.t)
    (Action : sig
       type t
     end)
    (ActionMap : sig
       include Hashtbl.S with type key = Action.t

       type t' = States.t t
     end)
    (EdgeMap : sig
       include Hashtbl.S with type key = State.t

       type t' = ActionMap.t' t

       val reduce_by_label : t' -> Label.t -> t'
     end)
    (Partition : sig
       include Set.S with type elt = States.t

       val json : ?as_elt:bool -> t -> Yojson.t
       val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
       val reachable : State.t -> EdgeMap.t' -> t -> t
     end)
    (Info : sig
       type t
     end)
    (FSM : sig
       type t =
         { init : EdgeMap.key option
         ; terminals : Partition.elt
         ; alphabet : Labels.t
         ; states : Partition.elt
         ; edges : EdgeMap.t'
         ; info : Info.t
         }

       val json : ?as_elt:bool -> t -> Yojson.t
       val saturate : ?only_if_weak:bool -> t -> t
     end)
    -> sig
  type t =
    { fsm : FSM.t
    ; pi : Partition.t
    }

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit

  exception CannotSplitEmptyBlock of unit

  val ensure_nonempty : States.t -> unit

  val split_block
    :  Partition.t
    -> State.t
    -> EdgeMap.t'
    -> States.t
    -> States.t * States.t option

  exception Split_OnlyReturnedOneBlock_ButNeqBlock of (States.t * States.t)

  val ensure_equal : States.t -> States.t -> unit

  val for_each_label
    :  Partition.t ref
    -> bool ref
    -> EdgeMap.t'
    -> States.t ref
    -> Label.t
    -> unit

  val for_each_block
    :  Partition.t ref
    -> bool ref
    -> Labels.t
    -> EdgeMap.t'
    -> States.t
    -> unit

  val partition_states : FSM.t -> Partition.t
  val fsm : FSM.t -> t
end
