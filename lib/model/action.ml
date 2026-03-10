module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (Label : Label.S with type t = Base.t Label.t')
    (Annotation : sig
       type t

       val equal : t -> t -> bool
       val compare : t -> t -> int
       val opt_length : ?fail_if_none:bool -> t option -> int
       val json : ?as_elt:bool -> t -> Yojson.t
     end) : sig
  type t =
    { label : Label.t
    ; annotation : Annotation.t option
    ; constructor_trees : Base.Trees.t
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
    ; constructor_trees : Base.Trees.t
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
            ; ( "constructor_trees"
              , Base.Trees.json ~as_elt:true x.constructor_trees )
            ]
        ;;
      end)

  let equal (a : t) (b : t) : bool =
    Label.equal a.label b.label
    && Option.equal Annotation.equal a.annotation b.annotation
    && Base.Trees.equal a.constructor_trees b.constructor_trees
  ;;

  let compare (a : t) (b : t) : int =
    Utils.compare_chain
      [ Label.compare a.label b.label
      ; Option.compare Annotation.compare a.annotation b.annotation
      ; Base.Trees.compare a.constructor_trees b.constructor_trees
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
