module type S = sig
  type 'a mm
  type ind
  type instructions
  type bindings
  type constrmap

  type t =
    { index : int
    ; name : string
    ; bindings : bindings
    }

  include Json.S with type k = t (** @closed *)

  val extract_info : ind -> t list mm
  val get_quantified_hyp : Names.Name.t -> Tactypes.quantified_hypothesis

  exception BindingInstruction_NotApp of EConstr.t
  exception BindingInstruction_Undefined of EConstr.t * EConstr.t
  exception BindingInstruction_IndexOutOfBounds of EConstr.t * int
  exception BindingInstruction_NEQ of EConstr.t * Constr.t

  val get_bound_term : EConstr.t -> instructions -> EConstr.t mm

  val get_explicit_bindings
    :  EConstr.t * constrmap option
    -> EConstr.t Tactypes.explicit_bindings mm

  val get
    :  EConstr.t
    -> EConstr.t option
    -> EConstr.t option
    -> bindings
    -> EConstr.t Tactypes.bindings mm
end

module Make
    (Log : Logger.S)
    (M : Rocq_monad_utils.S)
    (Bindings : Bindings.S with type 'a mm = 'a M.mm) :
  S
  with type 'a mm = 'a M.mm
   and type ind = M.Ind.t
   and type instructions = Bindings.Instructions.t
   and type bindings = Bindings.t
   and type constrmap = Bindings.ConstrMap.t'
