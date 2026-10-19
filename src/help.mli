(** [MeBi Help]: topic paths, their text, and printing. *)

(** Every documented topic, as a path of words after [MeBi Help]. *)
val topics : string list list

(** The text for a topic path ([[]] is the overview), if there is one. *)
val text : string list -> string option

(** Print a topic, or the overview and topic list if it is unknown. *)
val show : string list -> unit
