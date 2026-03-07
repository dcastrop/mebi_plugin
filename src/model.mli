module Make : (Log : Logger.S)
    (Enc : Encoding.S)
    (Tree : sig
              module Node : sig
                  type t = Enc.t * int

                  (* val compare : t -> t -> int *)
                  (* val equal : t -> t -> bool *)
                  (* val json : ?as_elt:bool -> t -> Yojson.t *)
                  (* val to_string : ?pretty:bool -> t -> string *)
                  (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
                end
                with type t = Enc.t * int

              type 'a tree = N of 'a * 'a tree list
              type t = Node.t tree

              (* val add : t -> t -> t *)
              (* val add_list : t -> t list -> t list *)
              val equal : t -> t -> bool
              val compare : t -> t -> int
              (* val minimize : t -> Node.t list *)

              exception CannotMinimizeEmptyList of unit

              (* val min : t list -> Node.t list *)
              val json : ?as_elt:bool -> t -> Yojson.t
              (* val to_string : ?pretty:bool -> t -> string *)
              (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
            end
            with type Node.t = Enc.t * int)
    (Trees : sig
       include Set.S with type elt = Tree.t

       exception EmptyHasNoMin

       (* val min : t -> Tree.t *)
       (* val min_opt : t -> Tree.t option *)
       val json : ?as_elt:bool -> t -> Yojson.t
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
     end)
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

         (* val json : ?as_elt:bool -> t -> Yojson.t *)
         (* val to_string : ?pretty:bool -> t -> string *)
         (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)

         exception Rocq_bindings_CannotAppendDone of unit

         (* val append : t -> t -> t *)
         (* val length : t -> int *)
       end

       module ConstrMap : sig
         include Hashtbl.S with type key = Constr.t

         type v = Names.Name.t * Instructions.t
         type t' = v t

         (* val json : ?as_elt:bool -> t' -> Yojson.t *)
         (* val to_string : ?pretty:bool -> t' -> string *)
         (* val log : ?__FUNCTION__:string -> ?s:string -> t' -> unit *)
         (* val update : t' -> Constr.t -> v -> unit *)

         exception Rocq_bindings_CannotFindBindingName of Evd.econstr

         (* val find_name
            :  (Evd.econstr * Names.Name.t) list
            -> Evd.econstr
            -> Names.Name.t

            val extract_binding_map
            :  (Evd.econstr * Names.Name.t) list
            -> Evd.econstr
            -> Constr.t
            -> t' mm

            val make_opt
            :  (Evd.econstr * Names.Name.t) list
            -> Evd.econstr * Constr.t
            -> t' option mm *)
       end

       type t =
         | No_Bindings
         | Use_Bindings of
             { from : ConstrMap.t' option
             ; action : ConstrMap.t' option
             ; goto : ConstrMap.t' option
             }

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val use_no_bindings : ConstrMap.t' option list -> bool

    val extract
      :  (Evd.econstr * Names.Name.t) list
      -> Evd.econstr * Constr.t
      -> Evd.econstr * Constr.t
      -> Evd.econstr * Constr.t
      -> t mm *)
     end)
    (ConstructorBindings : sig
       type t =
         { index : int
         ; name : string
         ; bindings : Bindings.t
         }

       val json : ?as_elt:bool -> t -> Yojson.t
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val extract_info : 'a Rocq_ind.t -> t list mm *)
       (* val get_quantified_hyp : Names.Name.t -> Tactypes.quantified_hypothesis

    exception Rocq_bindings_BindingInstruction_NotApp of Evd.econstr

    exception
      Rocq_bindings_BindingInstruction_Undefined of Evd.econstr * Evd.econstr

    exception
      Rocq_bindings_BindingInstruction_IndexOutOfBounds of Evd.econstr * int

    exception Rocq_bindings_BindingInstruction_NEQ of Evd.econstr * Constr.t

    val get_bound_term
      :  Evd.econstr
      -> Bindings.Instructions.t
      -> Evd.econstr mm

    val get_explicit_bindings
      :  Evd.econstr * Bindings.ConstrMap.t' option
      -> Evd.econstr Tactypes.explicit_bindings mm

    val get
      :  Enc.t
      -> Enc.t option
      -> Enc.t option
      -> Bindings.t
      -> Evd.econstr Tactypes.bindings mm *)
     end)
    -> sig
  module State : module type of Model_state.Make (Log) (Enc)
  module States : module type of Model_states.Make (Log) (State)
  module Label : module type of Model_label.Make (Log) (Enc)
  module Labels : module type of Model_labels.Make (Log) (Label)

  module Note :
      module type of
        Model_annotation_note.Make (Log) (State) (Label) (Tree) (Trees)

  module Annotation : module type of Model_annotation.Make (Log) (Label) (Note)

  module Annotations :
      module type of Model_annotations.Make (Log) (Note) (Annotation)

  module Transition :
      module type of
        Model_transition.Make (Log) (State) (Label) (Tree) (Note) (Annotation)

  module Transitions :
      module type of
        Model_transitions.Make (Log) (State) (Label) (Labels) (Tree)
          (Annotation)
          (Transition)

  module Action :
      module type of
        Model_action.Make (Log) (Label) (Tree) (Trees) (Note) (Annotation)

  module Actions :
      module type of
        Model_actions.Make (Log) (Label) (Labels) (Tree) (Trees) (Annotation)
          (Action)

  module ActionPair :
      module type of
        Model_actionpair.Make (Log) (State) (States) (Label) (Tree) (Trees)
          (Note)
          (Annotation)
          (Action)

  module ActionPairs :
      module type of
        Model_actionpairs.Make (Log) (State) (States) (Action) (ActionPair)

  module ActionMap :
      module type of
        Model_actionmap.Make (Log) (State) (States) (Label) (Tree) (Trees)
          (Annotation)
          (Action)
          (Actions)
          (ActionPair)
          (ActionPairs)

  module Edge : module type of Model_edge.Make (Log) (State) (Label) (Action)

  module Edges :
      module type of Model_edges.Make (Log) (State) (Label) (Action) (Edge)

  module EdgeMap :
      module type of
        Model_edgemap.Make (Log) (State) (States) (Label) (Action) (Actions)
          (ActionPair)
          (ActionPairs)
          (ActionMap)
          (Edge)
          (Edges)

  module Partition :
      module type of
        Model_state_partition.Make (Log) (State) (States) (Label) (Action)
          (ActionMap)
          (EdgeMap)

  module Info :
      module type of
        Model_info.Make (Log) (Enc) (Label) (Labels) (Bindings)
          (ConstructorBindings)

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
    val merge : t -> t -> t
    val is_weak_mode : t -> bool

    module Saturate :
        module type of
          Algorithm_saturate.Make (Log) (State) (States) (Label) (Labels) (Tree)
            (Trees)
            (Note)
            (Annotation)
            (Annotations)
            (Action)
            (ActionPair)
            (ActionPairs)
            (ActionMap)
            (EdgeMap)

    val saturate : ?only_if_weak:bool -> t -> t
  end

  module Convert : sig
    val transitions_to_edgemap : Transitions.t -> EdgeMap.t'
    val lts_to_fsm : LTS.t -> FSM.t
  end

  module Minimize :
      module type of
        Algorithm_minimize.Make (Log) (State) (States) (Label) (Labels) (Action)
          (ActionMap)
          (EdgeMap)
          (Partition)
          (Info)
          (FSM)

  module Bisimilar :
      module type of
        Algorithm_bisimilarity.Make (Log) (State) (States) (Label) (Labels)
          (Action)
          (ActionMap)
          (EdgeMap)
          (Partition)
          (Info)
          (FSM)
          (Minimize)
end
