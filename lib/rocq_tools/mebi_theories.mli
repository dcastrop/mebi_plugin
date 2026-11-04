
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
