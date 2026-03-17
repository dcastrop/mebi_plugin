module type S = sig
  include Base_term.S
  include Json.S with type k = t

  val init : t
  val next : t -> t
  val reset : unit -> unit
  val incr : unit -> t
end

module type Args = sig
  type t

  val init : t
  val next : t -> t
end

module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (X : Args with type t = Base.t) : S with type t = Base.t = struct
  include Base

  include
    Json.Thing.Make
      (Log)
      (struct
        type k = t

        let name : string = "Enc"

        let json ?(as_elt : bool = false) (x : t) : Yojson.t =
          Base.json ~as_elt:true x
        ;;
      end)

  let init = X.init
  let next = X.next
  let equal = Base.equal
  let compare = Base.compare
  let hash = Base.hash

  (* *)
  let counter : t ref = ref init
  let reset () : unit = counter := init

  let incr () : t =
    let x : t = !counter in
    counter := next !counter;
    x
  ;;
end

module Packed = struct
  module type PackedS = sig
    type t

    module BaseArgs : Base_term.Args with type t = t
    module EncodingArgs : Args with type t = t
  end

  module Int : PackedS with type t = Int.t = struct
    type t = Int.t

    module BaseArgs = struct
      include Int

      let to_string : t -> string = Printf.sprintf "%i"
    end

    module EncodingArgs : Args with type t = Int.t = struct
      type t = int

      let init : int = 0
      let next : int -> int = fun x -> x + 1
    end
  end

  module Unpack (Log : Logger.S) (Args : PackedS) : S with type t = Args.t =
  struct
    module Base : Base_term.S with type t = Args.t =
      Base_term.Make (Log) (Args.BaseArgs)

    include Make (Log) (Base) (Args.EncodingArgs)
  end
end
