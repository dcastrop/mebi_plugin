(** Turning the plugin's values into JSON, for logging them and dumping
    results ([MeBi Config Output "DumpResults"]). A value's JSON is either
    an element ([as_elt]), or, at the top, wrapped as [{name: ...}]. *)

(** [option ?as_elt f x] is [null] for [None], else [f ~as_elt] of [x]'s
    value. Raises whatever [f] raises (propagated). *)
val option
  :  ?as_elt:bool
  -> (?as_elt:bool -> 'a -> Yojson.t)
  -> 'a option
  -> Yojson.t

(** JSON, printing, logging and dumping for one type, [k]. *)
module type S = sig
  (**/**)

  (** Necessary for signature but never intended to be used directly. Always
      refers to some [type t] that this handles for. *)
  type k

  (**/**)

  (** The name the value is wrapped in, at the top. *)
  val name : string

  (** [json ?as_elt x] is [x] as JSON: as an element if [as_elt], else
      wrapped as [{name: ...}] (the default). Raises nothing. *)
  val json : ?as_elt:bool -> k -> Yojson.t

  (** [to_string ?pretty x] is {!json} of [x], printed, indented if
      [pretty] (the default). Raises nothing. *)
  val to_string : ?pretty:bool -> k -> string

  (** [log ?__FUNCTION__ ?m ?s x] logs {!to_string} of [x] at kind [m]
      (default [Debug]), headed [s] (default {!name}). Raises nothing. *)
  val log : ?__FUNCTION__:string -> ?m:Output.Kind.t -> ?s:string -> k -> unit

  (** [write ?dir name x] writes {!json} of [x], indented, to a new file in
      [dir] (default {!Utils.FileWriter.default_dir}) named after the
      session's timestamp, the source location and [name], creating [dir]
      if needed.

      Raises [Sys_error] if the directory or file cannot be created or
      written (propagated; the file is closed first). *)
  val write : ?dir:string -> string -> k -> unit
end

(** [Make (X)] is {!S} for [X.k], from its name and JSON. *)

module Make (X : sig
    type k

    val name : string
    val json : ?as_elt:bool -> k -> Yojson.t
  end) : S with type k = X.k

(** For a single value. *)
module Thing : sig
  (** [Make (X)] is {!S} for [X.k]: [X.json x], called with its own
      default for [as_elt], wrapped as [{X.name: ...}] at the top. *)
  module Make (X : sig
      type k

      val name : string
      val json : ?as_elt:bool -> k -> Yojson.t
    end) : S with type k = X.k
end

(** For a hash table. *)
module Map : sig
  (** {!S} with an order, so a table's entries can be sorted. *)
  module type S' = sig
    include S

    val compare : k -> k -> int
  end

  (** [Make (X) (K) (V)] is {!S} for a table of [X.Map], as a list of
      [{K.name: key, V.name: value}] entries, sorted by key and then
      value. *)
  module Make
      (X : sig
         module Map : Hashtbl.S

         type value

         val name : string
       end)
      (K : S' with type k = X.Map.key)
      (V : S' with type k = X.value) : S with type k = X.value X.Map.t
end

(** For a set. *)
module Set : sig
  (** [Make (X)] is {!S} for an [X.Set], as a list of its elements in the
      set's order. *)
  module Make (X : sig
      module Set : Set.S

      val name : string
      val json : ?as_elt:bool -> Set.elt -> Yojson.t
    end) : S with type k = X.Set.t
end

(** For a list. *)
module List : sig
  (** [Make (X)] is {!S} for a list of [X.k], as a list of its elements in
      order. *)
  module Make (X : sig
      type k

      val name : string
      val json : ?as_elt:bool -> k -> Yojson.t
    end) : S with type k = X.k list
end
