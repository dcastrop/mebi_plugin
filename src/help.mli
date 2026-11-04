(** [MeBi Help]: topic paths, their text, and printing. *)

(** The text for a topic path ([[]] is the overview), if there is one. *)
val text : string list -> string option

(** Print a topic, or the overview and topic list if it is unknown. *)
val show : string list -> unit
