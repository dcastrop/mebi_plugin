(** Unused: a leftover of an earlier way of loading the theory terms. *)
val constants : EConstr.t list ref

(** [find_reference path id] is the global reference [id] in the library
    module [path] (e.g. [["MEBI"; "Bisimilarity"]], ["weak_sim"]).

    @raise ErrorWithGlobalOfPath
      if no such reference is loaded (raised
      here, after logging the path). The exception is not exported, so a
      caller can only catch it generically. *)
val find_reference : string list -> string -> Names.GlobRef.t

(** [get_constants ()] is every theory term the plugin uses, by name
    ([weak_sim], [tau], [Some], ...), looked up once on first use and kept.

    @raise ErrorWithGlobalOfPath
      (unexported) if one is not loaded
      (propagated from {!find_reference}). *)
val get_constants : unit -> (string, EConstr.t) Hashtbl.t

(** [get k] is the theory term named [k] ({!get_constants}).

    @raise Failure if [k] is not one of them (raised here).
    @raise ErrorWithGlobalOfPath (unexported) as {!get_constants}
                                 (propagated). *)
val get : string -> EConstr.t

(** [get_proof_from_pstate p] is [Declare.Proof.get p]. Unused. *)
val get_proof_from_pstate : Declare.Proof.t -> Proof.t

(** [get_partial_proof p] is [Proof.partial_proof p]. Unused. *)
val get_partial_proof : Proof.t -> EConstr.t list
