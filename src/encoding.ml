
  module type SEncoding = sig
    type t

    val init : t
    val next : t -> t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val to_string : t -> string
    val counter : t ref
    val reset : unit -> unit
    val incr : unit -> t
  end

module type S = sig
    type t

    val init : t
    val next : t -> t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val hash : t -> int
    val to_string : t -> string
  end

  module Make (X : S) : SEncoding = struct
    type t = X.t

    let init = X.init
    let next = X.next
    let equal = X.equal
    let compare = X.compare
    let hash = X.hash
    let to_string = X.to_string

    (* *)
    let counter : t ref = ref init
    let reset () : unit = counter := init

    let incr () : t =
      let x : t = !counter in
      counter := X.next !counter;
      x
    ;;
  end

  module Int : SEncoding = Make (struct
      include Int

      type t = int

      let init : t = 0
      let next : t -> t = fun x -> x + 1
      let to_string : t -> string = Printf.sprintf "%i"
    end)