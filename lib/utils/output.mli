(** The kinds of message the plugin can emit. Contains no reference to the Rocq
    API — see [Logger.set_sink] for how messages reach Rocq's [Feedback]. *)

module Kind : sig
  (** [Debug] .. [Error] mirror Rocq's [Feedback.level]; [Trace], [Result]
      and [Show] are the plugin's own. *)
  type t =
    | Debug
    | Info
    | Notice
    | Warning
    | Error
    | Trace
    | Result
    | Show

  (** Every kind. *)
  val all : t list

  (** [to_string k] is [k]'s constructor name ([Debug] is ["Debug"]).
      Raises nothing. *)
  val to_string : t -> string

  (** [of_string s] is the kind named [s] ({!to_string}), or [None]. Raises
      nothing. *)
  val of_string : string -> t option

  (** [default k] is whether [k] is emitted when nothing has configured it:
      all but [Debug], [Trace] and [Result]. Raises nothing. *)
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
