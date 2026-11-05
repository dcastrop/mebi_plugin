(** The signatures of {!Bindings}, declared once for its [.ml] and [.mli]. *)

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
