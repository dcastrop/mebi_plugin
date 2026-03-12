module type S = sig
  include Set.S

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
  val extrapolate : elt -> t
end

module Make
    (Log : Logger.S)
    (Note : Annotation_note.S)
    (Annotation : Annotation.S with type note = Note.t) :
  S with type elt = Annotation.t = struct
  module Set_ : Set.S with type elt = Annotation.t = Set.Make (Annotation)
  include Set_

  include
    Json.Set.Make
      (Log)
      (struct
        module Set = Set_

        let name = "Annotations"
        let json = Annotation.json
      end)

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
end
