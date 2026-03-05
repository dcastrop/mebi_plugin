module Make
    (Log : Logger.S)
    (Enc : Encoding.SEncoding)
    (State : Model_state.Make(Log)(Enc).S)
    (Label : Model_label.Make(Log)(Enc).S)
    (Tree_ : module type of Enc_tree.Make (Log) (Enc)) (Tree : Tree_.S) =
  struct
  module Trees = Tree_.Trees (Tree)

  module type NoteType = sig
    type t =
      { from : State.t
      ; label : Label.t
      ; using : Trees.t
      ; goto : State.t
      }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val is_silent : t -> bool
    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module Note : NoteType = struct
    type t =
      { from : State.t
      ; label : Label.t
      ; using : Trees.t
      ; goto : State.t
      }

    let equal (a : t) (b : t) : bool =
      State.equal a.from b.from
      && State.equal a.goto b.goto
      && Label.equal a.label b.label
      && Trees.equal a.using b.using
    ;;

    let compare (a : t) (b : t) : int =
      Utils.compare_chain
        [ State.compare a.from b.from
        ; State.compare a.goto b.goto
        ; Label.compare a.label b.label
        ; Trees.compare a.using b.using
        ]
    ;;

    let is_silent (x : t) : bool = Label.is_silent x.label

    include
      Json.Thing.Make
        (Log)
        (struct
          type k = t

          let name = "Note"

          let json ?(as_elt : bool = false) (x : t) : Yojson.t =
            `Assoc
              [ "from", State.json ~as_elt:true x.from
              ; "label", Label.json ~as_elt:true x.label
              ; "goto", State.json ~as_elt:true x.goto
              ; "using", Trees.json ~as_elt:true x.using
              ]
          ;;
        end)
  end

  module type S = sig
    type t =
      { this : Note.t
      ; next : t option
      }

    val equal : t -> t -> bool
    val compare : t -> t -> int
    val is_empty : t -> bool
    val length : t -> int
    val shorter : t -> t -> t
    val exists : Note.t -> t -> bool
    val exists_label : Label.t -> t -> bool
    val append : Note.t -> t -> t
    val last : t -> Note.t

    exception CannotDropLastOfSingleton of t

    val drop_last : t -> t
    val json : ?as_elt:bool -> t -> Yojson.t
    val to_string : ?pretty:bool -> t -> string
    val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  end

  module Annotation : S = struct
    type t =
      { this : Note.t
      ; next : t option
      }

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

    let rec length : t -> int = function
      | { next = None; _ } -> 1
      | { next = Some next; _ } -> 1 + length next
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
      | { this; next = None } -> Label.equal x this.label
      | { this; next = Some next } ->
        if Label.equal x this.label then true else exists_label x next
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

    (* *)
    include
      Json.Thing.Make
        (Log)
        (struct
          type k = t

          let name = "Annotation"

          let json ?(as_elt : bool = false) (x : t) : Yojson.t =
            let rec f ?(as_elt : bool = false) (x : t) : Yojson.t =
              `Assoc
                [ "this", Note.json ~as_elt:true x.this
                ; ( "next"
                  , match x.next with
                    | None -> `String "None"
                    | Some next -> f ~as_elt:true next )
                ]
            in
            f ~as_elt x
          ;;
        end)
  end

  module Annotations (Annotation : S) = struct
    include Set.Make (Annotation)

    (** returns all of the possible actions after the named action *)
    let extrapolate (x : Annotation.t) : t =
      Log.trace __FUNCTION__;
      (* NOTE: skip pre-named action *)
      let rec skip ({ this; next } : Annotation.t) : t =
        (* Log.trace __FUNCTION__; *)
        let xs =
          Option.cata (if Note.is_silent this then skip else get) empty next
          |> map (fun (y : Annotation.t) -> { this; next = Some y })
        in
        (* NOTE: don't forget to add this action if named *)
        if Note.is_silent this then xs else add { this; next = None } xs
      (* NOTE: get every annotation from named action onwards *)
      and get : Annotation.t -> t =
        (* Log.trace __FUNCTION__; *)
        function
        | { this; next = None } -> singleton { this; next = None }
        | { this; next = Some next } ->
          get next
          |> map (fun (y : Annotation.t) -> { this; next = Some y })
          |> add { this; next = None }
      in
      add x (skip x)
    ;;

    include
      Json.Set.Make
        (Log)
        (struct
          module Set = Set.Make (Annotation)

          let name = "Annotations"
          let json = Annotation.json
        end)
  end
end
