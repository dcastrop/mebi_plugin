module type S = sig
  type t

  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module type Sa = sig
  type 'a t

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
end

module type Ordered = sig
  type t

  include Set.OrderedType with type t := t

  val equal : t -> t -> bool

  include S with type t := t
end

module Unit : Ordered with type t = unit = struct
  type t = unit [@@deriving show { with_path = false }, eq]

  let compare () () = 0
end

module String : Ordered with type t = string = struct
  type t = string [@@deriving show { with_path = false }, eq]

  let compare = String.compare
end

module Int : Ordered with type t = int = struct
  type t = int [@@deriving show { with_path = false }, eq]

  let compare = Int.compare
end

module Bool : Ordered with type t = bool = struct
  type t = bool [@@deriving show { with_path = false }, eq]

  let compare = Bool.compare
end

module Json : Ordered with type t = Json.t = struct
  type t = Json.t [@@deriving show { with_path = false }, eq]

  let compare = Json.compare
end
