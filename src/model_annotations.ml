module Make
    (Log : Logger.S)
    (* (Label : sig
       type t

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val equal : t -> t -> bool *)
       (* val compare : t -> t -> int *)
       (* val hash : t -> int *)
       (* val is_silent : t -> bool *)
       end) *)
     (Note : sig
       type t

       (* val json : ?as_elt:bool -> t -> Yojson.t *)
       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val equal : t -> t -> bool *)
       (* val compare : t -> t -> int *)
       val is_silent : t -> bool
       (* val has_label : Label.t -> t -> bool *)
     end)
    (Annotation : sig
       type t =
         { this : Note.t
         ; next : t option
         }

       val json : ?as_elt:bool -> t -> Yojson.t

       (* val to_string : ?pretty:bool -> t -> string *)
       (* val log : ?__FUNCTION__:string -> ?s:string -> t -> unit *)
       (* val equal : t -> t -> bool *)
       val compare : t -> t -> int
       (* val is_empty : t -> bool *)
       (* val length : t -> int *)
       (* val shorter : t -> t -> t *)
       (* val exists : Note.t -> t -> bool *)
       (* val exists_label : Label.t -> t -> bool *)
       (* val append : Note.t -> t -> t *)
       (* val last : t -> Note.t *)

       exception CannotDropLastOfSingleton of t

       (* val drop_last : t -> t *)
     end) : sig
  include Set.S with type elt = Annotation.t

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val extrapolate : elt -> t
end = struct
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
