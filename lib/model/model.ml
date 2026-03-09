module Make
    (Log : Logger.S)
    (Enc : Encoding.S)
    (Tree : sig
              module Node : sig
                  type t = Enc.t * int

                  (* val json : ?as_elt:bool -> t -> Yojson.t *)
                  (* val to_string : ?pretty:bool -> t -> string *)
                  (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
                  (* val compare : t -> t -> int *)
                  (* val equal : t -> t -> bool *)
                end
                with type t = Enc.t * int

              type 'a tree = N of 'a * 'a tree list
              type t = Node.t tree

              val json : ?as_elt:bool -> t -> Yojson.t

              (* val to_string : ?pretty:bool -> t -> string *)
              (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
              (* val add : t -> t -> t *)
              (* val add_list : t -> t list -> t list *)
              val equal : t -> t -> bool
              val compare : t -> t -> int
              (* val minimize : t -> Node.t list *)

              exception CannotMinimizeEmptyList of unit

              (* val min : t list -> Node.t list *)
            end
            with type Node.t = Enc.t * int)
    (Trees : sig
       include Set.S with type elt = Tree.t

       val json : ?as_elt:bool -> t -> Yojson.t

       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       exception EmptyHasNoMin

       (* val min : t -> Tree.t *)
       (* val min_opt : t -> Tree.t option *)
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
     end) =
struct
  module State = State.Make (Log) (Enc)
  module States = States.Make (Log) (State)
  module Label = Label.Make (Log) (Enc)
  module Labels = Labels.Make (Log) (Label)
  module Note = Annotation_note.Make (Log) (State) (Label) (Tree) (Trees)
  module Annotation = Annotation.Make (Log) (Label) (Note)
  module Annotations = Annotations.Make (Log) (Note) (Annotation)

  module Transition =
    Transition.Make (Log) (State) (Label) (Tree) (Note) (Annotation)

  module Transitions =
    Transitions.Make (Log) (State) (Label) (Labels) (Tree) (Annotation)
      (Transition)

  module Action = Action.Make (Log) (Label) (Tree) (Trees) (Note) (Annotation)

  module Actions =
    Actions.Make (Log) (Label) (Labels) (Tree) (Trees) (Annotation) (Action)

  module ActionPair =
    Actionpair.Make (Log) (State) (States) (Label) (Tree) (Trees) (Note)
      (Annotation)
      (Action)

  module ActionPairs =
    Actionpairs.Make (Log) (State) (States) (Action) (ActionPair)

  module ActionMap =
    Actionmap.Make (Log) (State) (States) (Label) (Tree) (Trees) (Annotation)
      (Action)
      (Actions)
      (ActionPair)
      (ActionPairs)

  module Edge = Edge.Make (Log) (State) (Label) (Action)
  module Edges = Edges.Make (Log) (State) (Label) (Action) (Edge)

  module EdgeMap =
    Edgemap.Make (Log) (State) (States) (Label) (Action) (Actions) (ActionPair)
      (ActionPairs)
      (ActionMap)
      (Edge)
      (Edges)

  module Partition =
    State_partition.Make (Log) (State) (States) (Label) (Action) (ActionMap)
      (EdgeMap)

  module Info =
    Info.Make (Log) (Enc) (Label) (Labels) (Bindings) (ConstructorBindings)

  module LTS = struct
    type t =
      { init : State.t option
      ; terminals : States.t
      ; alphabet : Labels.t
      ; states : States.t
      ; transitions : Transitions.t
      ; info : Info.t
      }

    include
      Json.Thing.Make
        (Log)
        (struct
          type k = t

          let name = "LTS"

          let json ?as_elt (x : t) : Yojson.t =
            `Assoc
              [ "init", Json.option ~as_elt:true State.json x.init
              ; "info", Info.json ~as_elt:true x.info
              ; "terminals", States.json ~as_elt:true x.terminals
              ; "alphabet", Labels.json ~as_elt:true x.alphabet
              ; "states", States.json ~as_elt:true x.states
              ; "transitions", Transitions.json ~as_elt:true x.transitions
              ]
          ;;
        end)
  end

  module FSM = struct
    type t =
      { init : State.t option
      ; terminals : States.t
      ; alphabet : Labels.t
      ; states : States.t
      ; edges : EdgeMap.t'
      ; info : Info.t
      }

    include
      Json.Thing.Make
        (Log)
        (struct
          type k = t

          let name = "FSM"

          let json ?as_elt (x : t) : Yojson.t =
            `Assoc
              [ "init", Json.option ~as_elt:true State.json x.init
              ; "info", Info.json ~as_elt:true x.info
              ; "terminals", States.json ~as_elt:true x.terminals
              ; "alphabet", Labels.json ~as_elt:true x.alphabet
              ; "states", States.json ~as_elt:true x.states
              ; "edges", EdgeMap.json ~as_elt:true x.edges
              ]
          ;;
        end)

    let merge (a : t) (b : t) : t =
      let init : State.t option = None in
      let terminals : States.t = States.union a.terminals b.terminals in
      let alphabet : Labels.t = Labels.union a.alphabet b.alphabet in
      let states : States.t = States.union a.states b.states in
      let edges : EdgeMap.t' = EdgeMap.merge a.edges b.edges in
      let info : Info.t = Info.merge a.info b.info in
      { init; terminals; alphabet; states; edges; info }
    ;;

    let is_weak_mode (x : t) : bool =
      Bool.not (Labels.is_empty x.info.weak_labels)
    ;;

    (** [module Saturate ] ...
        (* TODO: the idea of [Traces] needs to be revisited. It does provide optimizations to examples with a lot of silent actions, where the saturated FSM is considerably larger, but i believe that there are areas where this can still be improved. *)
    *)
    module Saturate =
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

    let saturate ?(only_if_weak : bool = true) (x : t) : t =
      Log.trace __FUNCTION__;
      if only_if_weak && Bool.not (is_weak_mode x)
      then (
        Log.debug ~__FUNCTION__ "Not weak, returning unchanged";
        x)
      else
        { x with
          edges = Saturate.edges x.alphabet x.states (EdgeMap.copy x.edges)
        }
    ;;
  end

  module Convert = struct
    let transitions_to_edgemap (xs : Transitions.t) : EdgeMap.t' =
      Log.trace __FUNCTION__;
      let edges : EdgeMap.t' = EdgeMap.create 0 in
      Transitions.iter
        (fun ({ from; goto; label; annotation; tree } : Transition.t) ->
          (* NOTE: [ActionMap.update] handles merging of [constructor_trees] for [actions] with matching [labels] *)
          EdgeMap.update
            edges
            from
            { label
            ; annotation
            ; constructor_trees = Option.cata Trees.singleton Trees.empty tree
            }
            (States.singleton goto))
        xs;
      edges
    ;;

    let lts_to_fsm (x : LTS.t) : FSM.t =
      Log.trace __FUNCTION__;
      { init = x.init
      ; terminals = x.terminals
      ; alphabet = x.alphabet
      ; states = x.states
      ; edges = transitions_to_edgemap x.transitions
      ; info = x.info
      }
    ;;
  end

  module Minimize =
    Algorithm_minimize.Make (Log) (State) (States) (Label) (Labels) (Action)
      (ActionMap)
      (EdgeMap)
      (Partition)
      (Info)
      (FSM)

  module Bisimilar =
    Algorithm_bisimilarity.Make (Log) (State) (States) (Label) (Labels) (Action)
      (ActionMap)
      (EdgeMap)
      (Partition)
      (Info)
      (FSM)
      (Minimize)
end
