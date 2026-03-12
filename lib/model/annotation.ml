module type S = sig
  type label
  type note

  type t =
    { this : note
    ; next : t option
    }

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val is_empty : t -> bool
  val opt_is_empty : ?fail_if_none:bool -> t option -> bool
  val length : t -> int
  val opt_length : ?fail_if_none:bool -> t option -> int
  val shorter : t -> t -> t
  val exists : note -> t -> bool
  val exists_label : label -> t -> bool
  val append : note -> t -> t
  val last : t -> note

  exception CannotDropLastOfSingleton of t

  val drop_last : t -> t
end

module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (Label : Label.S with type base = Base.t)
    (Note : Annotation_note.S with type label = Label.t) :
  S with type label = Label.t and type note = Note.t = struct
  type label = Label.t
  type note = Note.t

  type t =
    { this : note
    ; next : t option
    }

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "Annotation"

        let rec json ?(as_elt : bool = false) (x : t) : Yojson.t =
          `Assoc
            [ "this", Note.json ~as_elt:true x.this
            ; ( "next"
              , match x.next with
                | None -> `String "None"
                | Some next -> json ~as_elt:true next )
            ]
        ;;
      end)

  let rec equal (a : t) (b : t) : bool =
    Note.equal a.this b.this && Option.equal equal a.next b.next
  ;;

  let rec compare (a : t) (b : t) : int =
    Utils.compare_chain
      [ Note.compare a.this b.this; Option.compare compare a.next b.next ]
  ;;

  let is_empty : t -> bool = function
    | { this; next = None } -> true
    | _ -> false
  ;;

  let opt_is_empty ?(fail_if_none : bool = false) : t option -> bool = function
    | None -> if fail_if_none then raise Option.IsNone else true
    | Some x -> is_empty x
  ;;

  let rec length : t -> int = function
    | { next = None; _ } -> 1
    | { next = Some next; _ } -> 1 + length next
  ;;

  let opt_length ?(fail_if_none : bool = false) : t option -> int = function
    | None -> if fail_if_none then raise Option.IsNone else 0
    | Some x -> length x
  ;;

  let shorter (a : t) (b : t) : t =
    match Int.compare (length a) (length b) with -1 -> a | _ -> b
  ;;

  let rec exists (x : Note.t) : t -> bool = function
    | { this; next = None } -> Note.equal x this
    | { this; next = Some next } ->
      if Note.equal x this then true else exists x next
  ;;

  let rec exists_label (x : Label.t) : t -> bool = function
    | { this; next = None } -> Note.has_label x this
    | { this; next = Some next } ->
      if Note.has_label x this then true else exists_label x next
  ;;

  let rec append (x : Note.t) : t -> t = function
    | { this; next = None } -> { this; next = Some { this = x; next = None } }
    | { this; next = Some next } -> { this; next = Some (append x next) }
  ;;

  let rec last : t -> Note.t = function
    | { this; next = None } -> this
    | { next = Some next; _ } -> last next
  ;;

  exception CannotDropLastOfSingleton of t

  let rec drop_last : t -> t = function
    | { this; next = None } ->
      raise (CannotDropLastOfSingleton { this; next = None })
    | { this; next = Some { next = None; _ }; _ } -> { this; next = None }
    | { this; next = Some next } -> { this; next = Some (drop_last next) }
  ;;
end
