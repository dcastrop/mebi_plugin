module type S = sig
  type 'a mm

  module Instructions : sig
    type t =
      | Undefined
      | Done
      | Arg of
          { root : Constr.t
          ; index : int
          ; cont : t
          }

    include Json.S with type k = t

    exception CannotAppendDone of unit

    val append : t -> t -> t
    val length : t -> int
  end

  module NamedInstructions : sig
    type t = Names.Name.t * Instructions.t

    include Json.S with type k = t
  end

  module ConstrMap : sig
    include Hashtbl.S with type key = Constr.t

    type t' = NamedInstructions.t t

    include Json.S with type k = t'

    val update : t' -> Constr.t -> NamedInstructions.t -> unit

    exception Rocq_bindings_CannotFindBindingName of EConstr.t

    val find_name
      :  (EConstr.t * Names.Name.t) list
      -> EConstr.t
      -> Names.Name.t mm

    val extract_binding_map
      :  (EConstr.t * Names.Name.t) list
      -> EConstr.t
      -> Constr.t
      -> t' mm

    val make_opt
      :  (EConstr.t * Names.Name.t) list
      -> EConstr.t * Constr.t
      -> t' option mm
  end

  type t =
    | No_Bindings
    | Use_Bindings of
        { from : ConstrMap.t' option
        ; action : ConstrMap.t' option
        ; goto : ConstrMap.t' option
        }

  include Json.S with type k = t

  val use_no_bindings : ConstrMap.t' option list -> bool

  val extract
    :  (EConstr.t * Names.Name.t) list
    -> EConstr.t * Constr.t
    -> EConstr.t * Constr.t
    -> EConstr.t * Constr.t
    -> t mm
end

module Make (Log : Logger.S) (M : Rocq_monad_utils.S) :
  S with type 'a mm = 'a M.mm
