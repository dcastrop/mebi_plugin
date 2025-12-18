type t =
  | Option of Mebi_setup.Enc.t
  | Custom of Mebi_setup.Enc.t * Mebi_setup.Enc.t

val eq : t -> t -> bool
val to_string : t -> string Mebi_wrapper.mm
