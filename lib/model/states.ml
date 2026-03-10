module type S = sig
  include Set.S

  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
  val add_to_opt : elt -> t option -> t

  exception StateHasNoOrigin of (elt * t * t)

  val origin_of_state : elt -> t -> t -> int
  val has_shared_origin : t -> t -> t -> bool
end

module Make (Log : Logger.S) (State : State.S) : S with type elt = State.t =
struct
  module Set_ : Set.S with type elt = State.t = Set.Make (State)
  include Set_

  include
    Json.Set.Make
      (Log)
      (struct
        module Set = Set_

        let name = "States"
        let json = State.json
      end)

  let add_to_opt (x : State.t) (ys : t option) : t =
    add x (Option.default empty ys)
  ;;

  exception StateHasNoOrigin of (State.t * t * t)

  (** [origin_of_state x a b] returns [-1] if [x] is only in [a], [1] if [x] is only in [b], [0] if [x] is in both [a] and [b], and raises [StateHasNoOrigin] otherwise.
  *)
  let origin_of_state (x : State.t) (a : t) (b : t) : int =
    match mem x a, mem x b with
    | true, true -> 0
    | true, false -> -1
    | false, true -> 1
    | false, false -> raise (StateHasNoOrigin (x, a, b))
  ;;

  (** [has_shared_origin a b c] returns [true] if any of the states in [a] are present in both [b] and [c], otherwise [false].
  *)
  let has_shared_origin (a : t) (b : t) (c : t) : bool =
    let f (i : int) (x : State.t) : bool =
      match origin_of_state x b c with 0 -> true | j -> Int.equal i j
    in
    exists (f (-1)) a && exists (f 1) a
  ;;
end
