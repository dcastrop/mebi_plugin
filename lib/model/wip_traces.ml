module type S = sig
  type wip

  include Set.S

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> t -> unit
  val get : wip -> t -> t
end

module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (State : State.S with type base = Base.t)
    (WIP : Wip_annotation.S with type state = State.t)
    (Trace : Wip_trace.S with type state = State.t and type wip = WIP.t) :
  S with type elt = Trace.t and type wip = WIP.t = struct
  type wip = WIP.t

  module Set_ : Set.S with type elt = Trace.t = Set.Make (Trace)
  include Set_

  include
    Json.Set.Make
      (Log)
      (struct
        module Set = Set_

        let name = "WIP Traces"
        let json = Trace.json
      end)

  (** [get x ys] returns a subset subtraces [ys] that begin with [x]. This includes elements in [ys] that begin with [x], in addition to the trailing-subtraces that begin with [x] for elements in [ys].
      @raise Not_found if the set would return empty. *)
  let get (x : WIP.t) (ys : t) : t =
    let xs : t =
      fold
        (fun (y : Trace.t) (acc : t) ->
          if WIP.equal x y.this
          then add y acc
          else (
            match y.next with
            | Some (Next next) ->
              (try add (Trace.get x next) acc with Not_found -> acc)
            | _ -> acc))
        ys
        empty
    in
    if is_empty xs then raise Not_found else xs
  ;;
end
