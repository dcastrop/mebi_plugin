(** The kinds of message the plugin can emit. Contains no reference to the Rocq
    API — see [Logger.set_sink] for how messages reach Rocq's [Feedback]. *)

module Kind : sig
  type t =
    | Debug
    | Info
    | Notice
    | Warning
    | Error
    | Trace
    | Result
    | Show

  val all : t list
  val to_string : t -> string
  val of_string : string -> t option

  (** Whether a kind is emitted when nothing has configured it. *)
  val default : t -> bool
end

(** One message, kept in parts rather than pre-composed, so a sink that can lay
    text out properly (Rocq's [Pp]) still has the pieces to do so. *)
type message =
  { kind : Kind.t
  ; fn : string (** [__FUNCTION__] of the emitting site, or [""]. *)
  ; prefix : string option
  ; body : string
  }
