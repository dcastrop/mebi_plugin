(** [module Trace] ... we keep track of the total sum of traces we have already checked. This is useful for checking if, from a state and action, we have already explored the rest of this trace and so can just use what we have already learned, e.g., if we are in some "subtrace".
*)
module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type t = Base.t)
    (Label : Label.S with type t = Base.t Label.t')
    (Note : sig
       type t =
         { from : State.t
         ; label : Label.t
         ; using : Base.Trees.t
         ; goto : State.t
         }
     end)
    (Annotation : sig
       type t =
         { this : Note.t
         ; next : t option
         }
     end)
    (WIP : sig
       type t =
         { from : State.t
         ; via : Label.t
         ; trees : Base.Trees.t
         }

       val json : ?as_elt:bool -> t -> Yojson.t
       val is_named : t -> bool
       val equal : t -> t -> bool
       val compare : t -> t -> int
     end) : sig
  type t =
    { this : WIP.t
    ; next : next option
    }

  and next =
    | Next of t
    | Goto of State.t

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val create : WIP.t -> t
  val compare : t -> t -> int
  val compare_next : next -> next -> int

  exception Invalid

  val has_named : ?validate:bool -> t -> bool
  val validate : t -> unit

  exception CouldNotFindGoto

  val get_goto : t -> State.t

  exception CouldNotFindNamed

  val get_named : t -> Label.t
  val get_named_opt : t -> Label.t option

  exception FailAdd_AlreadyNamed
  exception FailAdd_AlreadyHasGoto

  val add : WIP.t -> t -> t

  exception FailSetGoto_AlreadyHasGoto

  val set_goto : State.t -> t -> t

  exception FailSeq_AlreadyNamed
  exception FailSeq_AlreadyHasGoto

  val seq : t -> t -> t
  val seq_opt : t option -> t -> t
  val get : WIP.t -> t -> t
  val upto_named : t -> t

  exception GotoNotSet

  val to_annotation : t -> Annotation.t
end = struct
  type t =
    { this : WIP.t
    ; next : next option
    }

  and next =
    | Next of t
    | Goto of State.t

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "WIP Trace"

        let json ?as_elt (x : t) : Yojson.t =
          let rec f (x : t) : Yojson.t =
            `Assoc
              [ "this", WIP.json ~as_elt:true x.this
              ; ( "next"
                , match x.next with
                  | None -> `Null
                  | Some (Next x) -> f x
                  | Some (Goto x) -> State.json ~as_elt:true x )
              ]
          in
          f x
        ;;
      end)

  let create (this : WIP.t) : t = { this; next = None }

  let rec compare (a : t) (b : t) : int =
    Utils.compare_chain
      [ WIP.compare a.this b.this; Option.compare compare_next a.next b.next ]

  and compare_next (a : next) (b : next) : int =
    match a, b with
    | Next a, Next b -> compare a b
    | Next a, Goto _ -> -1
    | Goto a, Goto b -> State.compare a b
    | Goto a, Next _ -> 1
  ;;

  exception Invalid

  (** [had_named ?validate x] is [true] if for an element in x has a label that is non-silent.
      @param ?validate
        is a optional flag that if [true] continues the search through the [Some next] that follows a named term, and will raise [Invalid] if another named element is found in x.
      @raise Invalid
        if more than one named element is found in [x] and [~validate:true]. *)
  let rec has_named ?(validate : bool = false) : t -> bool = function
    | { this; next = Some (Next x) } ->
      if WIP.is_named this
      then (
        if validate then if has_named x then raise Invalid;
        true)
      else has_named ~validate x
    | { this; next = _ } -> WIP.is_named this
  ;;

  let validate (x : t) : unit =
    let _ = has_named ~validate:true x in
    ()
  ;;

  exception CouldNotFindGoto

  let rec get_goto : t -> State.t = function
    | { this; next = Some (Goto x) } -> x
    | { this; next = Some (Next next) } -> get_goto next
    | { this; next = None } -> raise CouldNotFindGoto
  ;;

  exception CouldNotFindNamed

  let rec get_named : t -> Label.t = function
    | { this; next = Some (Next x) } ->
      if WIP.is_named this then this.via else get_named x
    | { this; next = _ } ->
      if WIP.is_named this then this.via else raise CouldNotFindNamed
  ;;

  let get_named_opt (x : t) : Label.t option =
    try Some (get_named x) with CouldNotFindNamed -> None
  ;;

  exception FailAdd_AlreadyNamed
  exception FailAdd_AlreadyHasGoto

  let add (x : WIP.t) (ys : t) : t =
    (* NOTE: cannot have more than one non-silent action *)
    if WIP.is_named x && has_named ys then raise FailAdd_AlreadyHasGoto;
    let rec add : t -> t = function
      | { this; next = None } ->
        { this; next = Some (Next { this = x; next = None }) }
      | { this; next = Some (Next y) } -> { this; next = Some (Next (add y)) }
      | { this; next = Some (Goto _) } -> raise FailAdd_AlreadyHasGoto
    in
    add ys
  ;;

  exception FailSetGoto_AlreadyHasGoto

  let set_goto (x : State.t) (ys : t) : t =
    let rec set_goto : t -> t = function
      | { this; next = None } -> { this; next = Some (Goto x) }
      | { this; next = Some (Next y) } ->
        { this; next = Some (Next (set_goto y)) }
      | { this; next = Some (Goto _) } -> raise FailSetGoto_AlreadyHasGoto
    in
    set_goto ys
  ;;

  exception FailSeq_AlreadyNamed
  exception FailSeq_AlreadyHasGoto

  (** [seq a b] appends [b] to the end of [a]. *)
  let seq (a : t) (b : t) : t =
    (* NOTE: cannot have more than one non-silent action *)
    if has_named a && has_named b then raise FailSeq_AlreadyNamed;
    let rec seq : t -> t = function
      | { this; next = None } -> { this; next = Some (Next b) }
      | { this; next = Some (Next x) } -> { this; next = Some (Next (seq x)) }
      | { this; next = Some (Goto _) } -> raise FailAdd_AlreadyHasGoto
    in
    seq a
  ;;

  (** [seq_opt a b] is [seq a b] of [Some a] else [b] *)
  let seq_opt (a : t option) (b : t) : t =
    match a with None -> b | Some a -> seq a b
  ;;

  (** [get x ys] ...
      @raise Not_found if [x] does not match any in [ys]. *)
  let rec get (x : WIP.t) : t -> t = function
    | { this; next = Some (Next y) } ->
      if WIP.equal x this then { this; next = Some (Next y) } else get x y
    | { this; next } ->
      if WIP.equal x this then { this; next } else raise Not_found
  ;;

  (** [upto_named x] ...
      @raise Not_found if [x] begins with a named label. *)
  let upto_named (x : t) : t =
    Log.trace __FUNCTION__;
    match x with
    | { this; next } ->
      if WIP.is_named this
      then raise Not_found
      else (
        let rec f : next option -> next option = function
          | None -> None
          | Some (Goto y) -> Some (Goto y)
          | Some (Next { this; next }) ->
            if WIP.is_named this
            then Some (Goto this.from)
            else Some (Next { this; next = f next })
        in
        { this; next = f next })
  ;;

  exception GotoNotSet

  let rec to_annotation : t -> Annotation.t =
    Log.trace __FUNCTION__;
    function
    | { this = { from; via; trees }; next = None } ->
      (* { this = { from; label = via; using = trees; goto }; next = None } *)
      raise GotoNotSet
    | { this = { from; via; trees }; next = Some (Next x) } ->
      let next : Annotation.t = to_annotation x in
      { this = { from; label = via; using = trees; goto = next.this.from }
      ; next = Some next
      }
    | { this = { from; via; trees }; next = Some (Goto goto) } ->
      { this = { from; label = via; using = trees; goto }; next = None }
  ;;
end
