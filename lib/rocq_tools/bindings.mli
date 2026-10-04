(** Where an LTS constructor's binders sit in its source, label and target
    terms, as paths into those terms; {!Constructor_bindings} follows the
    paths in a concrete step to bind them. *)
module type S = sig
  type 'a mm

  (** A path into a term: [Arg { root; index; cont }] steps into argument
      [index] of an application headed by [root] and continues with [cont];
      [Done] is the term reached; [Undefined] is a path still being built
      (its open end). *)
  module Instructions : sig
    type t =
      | Undefined
      | Done
      | Arg of
          { root : Constr.t
          ; index : int
          ; cont : t
          }

    include Json.S with type k = t (** @closed *)

    (** Raised by {!append} on a finished path. *)
    exception CannotAppendDone of unit

    (** [append x path] is [path] with [x] in place of its open end (its
        [Undefined]), so paths are built from the outside in.

        @raise CannotAppendDone if [path] already ends in [Done] (raised
                                here). *)
    val append : t -> t -> t

    (** [length path] is the number of [Arg] steps in [path], less one if
        it ends in [Done] (so of two finished paths, the shorter is the
        smaller). Raises nothing. *)
    val length : t -> int
  end

  (** A binder's name and the path to it. *)
  module NamedInstructions : sig
    type t = Names.Name.t * Instructions.t

    include Json.S with type k = t (** @closed *)
  end

  (** Per binder (keyed by its de Bruijn index term in the constructor's
      type), its name and path. *)
  module ConstrMap : sig
    include Hashtbl.S with type key = Constr.t

    type t' = NamedInstructions.t t

    include Json.S with type k = t' (** @closed *)

    (** [update m k (name, path)] records [(name, path)] for [k], unless [k]
        already has a path at least as short. Raises nothing. *)
    val update : t' -> Constr.t -> NamedInstructions.t -> unit

    (** Raised by {!find_name}: the term is none of the binders. *)
    exception Rocq_bindings_CannotFindBindingName of EConstr.t

    (** [find_name name_pairs x] is the name of the binder whose evar is [x],
        among [name_pairs] ((evar, name) per binder).

        @raise Rocq_bindings_CannotFindBindingName
          when run, if [x] is none
          of them (raised here). *)
    val find_name
      :  (EConstr.t * Names.Name.t) list
      -> EConstr.t
      -> Names.Name.t mm

    (** [extract_binding_map name_pairs x y] is the path to each binder in
        [x], found by walking [x] (the constructor's term with an evar per
        binder) alongside [y] (the same term with de Bruijn indices): at
        matching applications it descends into each argument, and where [y]
        is an index, the evar there names a binder ({!find_name}), the
        shortest path to it kept ({!update}).

        @raise Rocq_bindings_CannotFindBindingName
          when run, if an index's
          evar is not a binder (propagated from {!find_name}). *)
    val extract_binding_map
      :  (EConstr.t * Names.Name.t) list
      -> EConstr.t
      -> Constr.t
      -> t' mm

    (** [make_opt ?keep_var name_pairs (evar, rel)] is the binder paths in
        one of a constructor's source, label or target ({!extract_binding_map}),
        or [None] if there are none, or if the term is just a binder and
        [keep_var] is unset. Such a binding is redundant for the source
        (matching the goal fixes it) but not for the target, still open when
        the constructor is applied, which a premise may mention
        ([succ_rel n m -> lts n a m]; backlog I2, stage 2).

        @raise Rocq_bindings_CannotFindBindingName
          as
          {!extract_binding_map} (propagated). *)
    val make_opt
      :  ?keep_var:bool
      -> (EConstr.t * Names.Name.t) list
      -> EConstr.t * Constr.t
      -> t' option mm
  end

  (** A constructor's binder paths: none at all, or per source, label and
      target (each optional). *)
  type t =
    | No_Bindings
    | Use_Bindings of
        { from : ConstrMap.t' option
        ; action : ConstrMap.t' option
        ; goto : ConstrMap.t' option
        }

  include Json.S with type k = t (** @closed *)

  (** [use_no_bindings ms] is whether none of [ms] has paths. Raises
      nothing. *)
  val use_no_bindings : ConstrMap.t' option list -> bool

  (** [extract name_pairs from action goto] is a constructor's binder
      paths in its source, label and target ({!ConstrMap.make_opt}; the
      target keeps a bare binder), or [No_Bindings] if none has any. Each of
      [from], [action] and [goto] pairs the term with evars for binders and
      the term with de Bruijn indices.

      @raise ConstrMap.Rocq_bindings_CannotFindBindingName
        as
        {!ConstrMap.make_opt} (propagated). *)
  val extract
    :  (EConstr.t * Names.Name.t) list
    -> EConstr.t * Constr.t
    -> EConstr.t * Constr.t
    -> EConstr.t * Constr.t
    -> t mm
end

module Make (M : Rocq_monad_utils.S) : S with type 'a mm = 'a M.mm
