type t =
  Model_label.t
  * Model_label.t
  * Model_label.t
  * Model_label.meta option

val eq : t -> t -> bool
val compare : t -> t -> int
