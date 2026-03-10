module Make : (Log : Logger.S)
    (Base : Base_term.S)
    (Bindings : sig
       module Instructions : sig
         type t =
           | Undefined
           | Done
           | Arg of
               { root : Constr.t
               ; index : int
               ; cont : t
               }
       end

       module ConstrMap : sig
         include Hashtbl.S with type key = Constr.t

         type v = Names.Name.t * Instructions.t
         type t' = v t
       end

       type t =
         | No_Bindings
         | Use_Bindings of
             { from : ConstrMap.t' option
             ; action : ConstrMap.t' option
             ; goto : ConstrMap.t' option
             }
     end)
    (ConstructorBindings : sig
       type t =
         { index : int
         ; name : string
         ; bindings : Bindings.t
         }

       val json : ?as_elt:bool -> t -> Yojson.t
     end)
    -> sig
  module State : State.S with type t = Base.t
  module States : States.S with type elt = State.t
  module Label : Label.S with type t = Base.t Label.t'
  module Labels : Labels.S with type elt = Label.t
  module Note : module type of Annotation_note.Make (Log) (Base) (State) (Label)
  module Annotation : module type of Annotation.Make (Log) (Label) (Note)
  module Annotations : module type of Annotations.Make (Log) (Note) (Annotation)

  module Transition :
      module type of
        Transition.Make (Log) (Base) (State) (Label) (Note) (Annotation)

  module Transitions :
      module type of
        Transitions.Make (Log) (Base) (State) (Label) (Labels) (Annotation)
          (Transition)

  module Action : module type of Action.Make (Log) (Base) (Label) (Annotation)

  module Actions :
      module type of
        Actions.Make (Log) (Base) (Label) (Labels) (Annotation) (Action)

  module ActionPair :
      module type of
        Actionpair.Make (Log) (Base) (State) (States) (Label) (Annotation)
          (Action)

  module ActionPairs :
      module type of
        Actionpairs.Make (Log) (Base) (State) (States) (Action) (ActionPair)

  module ActionMap :
      module type of
        Actionmap.Make (Log) (Base) (State) (States) (Label) (Annotation)
          (Action)
          (Actions)
          (ActionPair)
          (ActionPairs)

  module Edge : module type of Edge.Make (Log) (State) (Label) (Action)
  module Edges : module type of Edges.Make (Log) (State) (Label) (Action) (Edge)

  module EdgeMap :
      module type of
        Edgemap.Make (Log) (Base) (State) (States) (Label) (Annotation)
          (Transition)
          (Transitions)
          (Action)
          (Actions)
          (ActionPair)
          (ActionPairs)
          (ActionMap)
          (Edge)
          (Edges)

  module Partition :
      module type of
        State_partition.Make (Log) (State) (States) (Label) (Action) (ActionMap)
          (EdgeMap)

  module Info :
      module type of
        Info.Make (Log) (Base) (Label) (Labels) (Bindings) (ConstructorBindings)

  (* module Info : sig
    type t =
      { meta : meta option
      ; weak_labels : Labels.t
      }

    and meta =
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

    val merge : t -> t -> t
    val to_string : t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end *)

  module LTS : sig
    type t =
      { init : State.t option
      ; terminals : Partition.elt
      ; alphabet : Labels.t
      ; states : Partition.elt
      ; transitions : Transitions.t
      ; info : Info.t
      }

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module FSM : sig
    type t =
      { init : State.t option
      ; terminals : Partition.elt
      ; alphabet : Labels.t
      ; states : Partition.elt
      ; edges : EdgeMap.t'
      ; info : Info.t
      }

    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
    val of_lts : LTS.t -> t
    val merge : t -> t -> t
    val is_weak_mode : t -> bool

    (* include module Saturate :
       module type of
       Saturate.Make (Log) (Base) (State) (States) (Label) (Labels) (Note)
       (Annotation)
       (Annotations)
       (Action)
       (ActionPair)
       (ActionPairs)
       (ActionMap)
       (EdgeMap) *)

    val saturate : ?only_if_weak:bool -> t -> t
  end

  module Minimize :
      module type of
        Minimize.Make (Log) (Base) (State) (States) (Label) (Labels) (Action)
          (ActionMap)
          (EdgeMap)
          (Partition)
          (Info)
          (FSM)

  module Bisimilarity :
      module type of
        Bisimilarity.Make (Log) (State) (States) (Label) (Labels) (Action)
          (ActionMap)
          (EdgeMap)
          (Partition)
          (Info)
          (FSM)
          (Minimize)
end
