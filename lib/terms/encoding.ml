module type S = sig
  include Base_term.S

  val init : t
  val next : t -> t
  val reset : unit -> unit
  val incr : unit -> t
end

module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (X : sig
       val init : Base.t
       val next : Base.t -> Base.t
     end) : S with type t = Base.t = struct
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

module ToBase (Enc : S) : Base_term.S = Enc

module Int (Log : Logger.S) : S with type t = Int.t =
  Make
    (Log)
    (Base_term.Make
       (Log)
       (struct
         include Int

         let to_string : t -> string = Printf.sprintf "%i"
       end))
    (struct
      let init : int = 0
      let next : int -> int = fun x -> x + 1
    end)
