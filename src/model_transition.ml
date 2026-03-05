module Make
    (Log : Logger.S)
    (Enc : Encoding.SEncoding)
    (State : Model_state.Make(Log)(Enc).S)
    (Label_ : module type of Model_label.Make (Log) (Enc))
       (Labels : module type of Label_.Labels (Label_.Label))
          (Tree_ : module type of Enc_tree.Make (Log) (Enc))
             (* (Tree : Tree_.S) *)
              (Annotation :
                Model_annotation.Make(Log)(Enc)(State)(Label_.Label)(Tree_)
                  (Tree_.Tree)
                .S) =
        struct
       module Label = Label_.Label

       (* module Labels = Label_.Labels (Label) *)
       (* module Tree = Tree_.Tree *)

       module type S = sig
         type t =
           { from : State.t
           ; goto : State.t
           ; label : Label.t
           ; annotation : Annotation.t option
           ; constructor_tree : Tree_.Tree.t option
           }

         val equal : t -> t -> bool
         val compare : t -> t -> int
         val is_silent : t -> bool
         val annotation_is_empty : t -> bool
         val json : ?as_elt:bool -> t -> Yojson.t
         val to_string : ?pretty:bool -> t -> string
         val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
       end

       module Transition : S = struct
         type t =
           { from : State.t
           ; goto : State.t
           ; label : Label.t
           ; annotation : Annotation.t option
           ; constructor_tree : Tree_.Tree.t option
           }

         let equal (a : t) (b : t) : bool =
           State.equal a.from b.from
           && State.equal a.goto b.goto
           && Label.equal a.label b.label
           && Option.equal Annotation.equal a.annotation b.annotation
           && Option.equal
                Tree_.Tree.equal
                a.constructor_tree
                b.constructor_tree
         ;;

         let compare (a : t) (b : t) : int =
           Utils.compare_chain
             [ State.compare a.from b.from
             ; State.compare a.goto b.goto
             ; Label.compare a.label b.label
             ; Option.compare Annotation.compare a.annotation b.annotation
             ; Option.compare
                 Tree_.Tree.compare
                 a.constructor_tree
                 b.constructor_tree
             ]
         ;;

         let is_silent (x : t) : bool = Label.is_silent x.label

         let annotation_is_empty : t -> bool = function
           | { annotation = None; _ } -> true
           | { annotation = Some annotation; _ } ->
             Annotation.is_empty annotation
         ;;

         (* *)
         include
           Json.Thing.Make
             (Log)
             (struct
               type k = t

               let name = "Transition"

               let json ?(as_elt : bool = false) (x : t) : Yojson.t =
                 `Assoc
                   [ "from", State.json ~as_elt:true x.from
                   ; "goto", State.json ~as_elt:true x.goto
                   ; "label", Label.json ~as_elt:true x.label
                   ; ( "annotation"
                     , match x.annotation with
                       | None -> `String "None"
                       | Some x -> Annotation.json ~as_elt:true x )
                   ; ( "constructor_tree"
                     , match x.constructor_tree with
                       | None -> `String "None"
                       | Some x -> Tree_.Tree.json ~as_elt:true x )
                   ]
               ;;
             end)
       end

       module Transitions (Transition : S) = struct
         include Set.Make (Transition)

         let labels (xs : t) : Labels.t =
           Log.trace __FUNCTION__;
           fold
             (fun ({ label; _ } : elt) : (Labels.t -> Labels.t) ->
               Labels.add label)
             xs
             Labels.empty
         ;;

         include
           Json.Set.Make
             (Log)
             (struct
               module Set = Set.Make (Transition)

               let name = "Transitions"
               let json = Transition.json
             end)
       end
     end
