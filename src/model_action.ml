module Make
    (Log : Logger.S)
    (Label : sig
       type t

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val hash : t -> int
       val is_silent : t -> bool
       val json : ?as_elt:bool -> t -> Yojson.t
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
     end)
    (Tree : sig
       module Node : sig
         type t

         (* val compare : t -> t -> int *)
         (* val equal : t -> t -> bool *)
         (* val json : ?as_elt:bool -> t -> Yojson.t *)
         (* val to_string : ?pretty:bool -> t -> string *)
         (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       end

       type 'a tree = N of 'a * 'a tree list
       type t = Node.t tree

       (* val add : t -> t -> t *)
       (* val add_list : t -> t list -> t list *)
       (* val equal : t -> t -> bool *)
       (* val compare : t -> t -> int *)
       (* val minimize : t -> Node.t list *)

       exception CannotMinimizeEmptyList of unit

       (* val min : t list -> Node.t list *)
       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
     end)
    (Trees : sig
       include Set.S with type elt = Tree.t

       exception EmptyHasNoMin

       (* val min : t -> Tree.t *)
       (* val min_opt : t -> Tree.t option *)
       val json : ?as_elt:bool -> t -> Yojson.t
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
     end)
    (Note : sig
       type t

       (* val equal : t -> t -> bool *)
       (* val compare : t -> t -> int *)
       (* val is_silent : t -> bool *)
       (* val has_label : Label.t -> t -> bool *)
       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
     end)
    (Annotation : sig
       type t =
         { this : Note.t
         ; next : t option
         }

       val equal : t -> t -> bool
       val compare : t -> t -> int

       (* val is_empty : t -> bool *)
       (* val opt_is_empty : ?fail_if_none:bool -> t option -> bool *)
       (* val length : t -> int *)
       val opt_length : ?fail_if_none:bool -> t option -> int
       (* val shorter : t -> t -> t *)
       (* val exists : Note.t -> t -> bool *)
       (* val exists_label : Label.t -> t -> bool *)
       (* val append : Note.t -> t -> t *)
       (* val last : t -> Note.t *)

       exception CannotDropLastOfSingleton of t

       (* val drop_last : t -> t *)
       val json : ?as_elt:bool -> t -> Yojson.t
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
     end) : sig
  type t =
    { label : Label.t
    ; annotation : Annotation.t option
    ; constructor_trees : Trees.t
    }

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val wk_equal : t -> t -> bool
  val is_silent : t -> bool
  val is_labelled : Label.t -> t -> bool
  val shorter_annotation : t -> t -> t
end = struct
  type t =
    { label : Label.t
    ; annotation : Annotation.t option
    ; constructor_trees : Trees.t
    }

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "Action"

        let json ?(as_elt : bool = false) (x : t) : Yojson.t =
          `Assoc
            [ "label", Label.json ~as_elt:true x.label
            ; ( "annotation"
              , Json.option ~as_elt:true Annotation.json x.annotation )
            ; "constructor_trees", Trees.json ~as_elt:true x.constructor_trees
            ]
        ;;
      end)

  let equal (a : t) (b : t) : bool =
    Label.equal a.label b.label
    && Option.equal Annotation.equal a.annotation b.annotation
    && Trees.equal a.constructor_trees b.constructor_trees
  ;;

  let compare (a : t) (b : t) : int =
    Utils.compare_chain
      [ Label.compare a.label b.label
      ; Option.compare Annotation.compare a.annotation b.annotation
      ; Trees.compare a.constructor_trees b.constructor_trees
      ]
  ;;

  let hash (x : t) : int = Label.hash x.label
  let wk_equal (a : t) (b : t) : bool = Label.equal a.label b.label
  let is_silent (x : t) : bool = Label.is_silent x.label
  let is_labelled (x : Label.t) (y : t) : bool = Label.equal x y.label

  let shorter_annotation (a : t) (b : t) : t =
    match
      Int.compare
        (Annotation.opt_length a.annotation)
        (Annotation.opt_length b.annotation)
    with
    | 1 -> b
    | _ -> a
  ;;
end
