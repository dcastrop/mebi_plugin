module type S = sig
  type weak
  type 'a mm
  type t
  type lts

  (** [build ?weak t lts using] is the graph of every state reachable from
      the term [t] by the LTS [lts], whose constructors may use the LTSs
      [using] in their premises, explored breadth first until the bounds
      ({!Api.the_bounds_args}) stop it. [t] is typechecked against [lts]'s
      state type first. [weak] names the silent label.

      Raises, when run, Rocq's errors if [t] does not typecheck or a name is
      unknown, and whatever extracting a constructor raises (both
      propagated).

      @raise LTSMapDoesNotContainPrimaryLTS
        when run, if [lts] is not among [using] (propagated). *)
  val build
    :  ?weak:weak option
    -> Constrexpr.constr_expr
    -> Libnames.qualid
    -> Names.GlobRef.t list
    -> t mm

  (** [extract g] is the LTS of the graph [g]
      ({!Graph_extract_lts.S.extract}). Raises nothing when run. *)
  val extract : t -> lts mm
end

module Make
    (Enc : Encoding.S)
    (M : Rocq_monad_utils.S with type enc = Enc.t and type tree = Enc.Tree.t)
    (Weak : Weak.S with type enc = Enc.t)
    (Theory :
       Theories_enc.S
       with type enc = Enc.t
        and type 'a mm = 'a M.mm
        and type 'a im = 'a M.mm)
    (ConstructorBindings :
       Constructor_bindings.S with type 'a mm = 'a M.mm and type ind = M.Ind.t)
    (Model :
       Model.S
       with type base = Enc.t
        and type tree = Enc.Tree.t
        and type trees = Enc.Trees.t
        and type constructorbindings = ConstructorBindings.t)
    (X : Graph_type.Args with type enc = Enc.t and type tree = Enc.Tree.t) :
  S with type weak = Weak.t and type 'a mm = 'a M.mm and type lts = Model.LTS.t
