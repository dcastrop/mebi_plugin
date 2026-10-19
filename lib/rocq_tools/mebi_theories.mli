(** Raised by {!find_reference}: no such reference is loaded. *)
exception ErrorWithGlobalOfPath

(** [find_reference path id] is the global reference [id] in the library
    module [path] (e.g. [["MEBI"; "Bisimilarity"]], ["weak_sim"]).

    @raise ErrorWithGlobalOfPath
      if no such reference is loaded (raised
      here, after logging the path). *)
val find_reference : string list -> string -> Names.GlobRef.t

(** [get_constants ()] is every theory term the plugin uses, by name
    ([weak_sim], [tau], [Some], ...), looked up once on first use and kept.

    @raise ErrorWithGlobalOfPath
      if one is not loaded
      (propagated from {!find_reference}). *)
val get_constants : unit -> (string, EConstr.t) Hashtbl.t

(** [get k] is the theory term named [k] ({!get_constants}).

    @raise Failure if [k] is not one of them (raised here).
    @raise ErrorWithGlobalOfPath as {!get_constants}
                                 (propagated). *)
val get : string -> EConstr.t
