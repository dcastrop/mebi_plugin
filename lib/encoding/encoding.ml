module type S = sig
  type t

  val init : t
  val next : t -> t
  val reset : unit -> unit
  val incr : unit -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val json : ?as_elt:bool -> t -> Yojson.t
  val to_string : ?pretty:bool -> t -> string
  val log : ?__FUNCTION__:string -> ?s:string -> t -> unit
end

module Make
    (Log : Logger.S)
    (X : sig
       type t

       val init : t
       val next : t -> t
       val equal : t -> t -> bool
       val compare : t -> t -> int
       val hash : t -> int
       val to_string : t -> string
     end) : S with type t = X.t with type t = X.t = struct
  type t = X.t

  let init = X.init
  let next = X.next
  let equal = X.equal
  let compare = X.compare
  let hash = X.hash

  (* *)
  let counter : t ref = ref init
  let reset () : unit = counter := init

  let incr () : t =
    let x : t = !counter in
    counter := X.next !counter;
    x
  ;;

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name = "Enc"

        let json ?(as_elt : bool = false) (x : t) : Yojson.t =
          `String (X.to_string x)
        ;;
      end)
end

module Int (Log : Logger.S) : S with type t = Int.t =
  Make
    (Log)
    (struct
      include Int

      let init : t = 0
      let next : t -> t = fun x -> x + 1
      let to_string : t -> string = Printf.sprintf "%i"
    end)
