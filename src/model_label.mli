type t = {
  enc : Mebi_setup.Enc.t;
  pp : string option;
  is_silent : bool option;
}

val is_silent : t -> bool
val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int
val to_string : ?args:Utils.Strfy.style_args -> t -> string
