module type S = sig
  module M : Rocq_monad.Utils.S

  module type SPair = sig
    type t =
      { to_check : Evd.econstr
      ; acc : Evd.econstr
      }

    val to_string : Environ.env -> Evd.evar_map -> t -> string

    val make
      :  Environ.env
      -> Evd.evar_map
      -> Evd.econstr
      -> Evd.econstr
      -> Evd.evar_map * t

    val unify : Environ.env -> Evd.evar_map -> t -> Evd.evar_map * bool
  end

  module Pair : SPair

  module type SProblem = sig
    type t =
      { act : Pair.t
      ; goto : Pair.t
      ; tree : M.Constructor.Tree.t
      }

    val to_string : Environ.env -> Evd.evar_map -> t -> string
    val unify_opt : t -> M.Constructor.Tree.t option M.mm
  end

  module Problem : SProblem

  module type SProblems = sig
    type t =
      { sigma : Evd.evar_map
      ; to_unify : Problem.t list
      }

    val empty : unit -> t M.mm
    val list_is_empty : t list -> bool
    val to_string : Environ.env -> t -> string
    val list_to_string : Environ.env -> t list -> string

    val sandbox_unify_all_opt
      :  Evd.econstr
      -> Evd.econstr
      -> t
      -> (Evd.econstr * Evd.econstr * M.Constructor.Tree.t list) option M.mm
  end

  module Problems : SProblems

  module type SConstructors = sig
    type t = M.Constructor.t list

    val to_string : Environ.env -> Evd.evar_map -> t -> string

    val retrieve
      :  int
      -> t
      -> Evd.econstr
      -> Evd.econstr
      -> M.Enc.t * Problems.t list
      -> t M.mm
  end

  module Constructors : SConstructors

  val collect_valid_constructors
    :  Rocq_ind.LTS.constructor array
    -> M.Enc.t Rocq_ind.t M.F.t
    -> Evd.econstr
    -> Evd.econstr
    -> M.Enc.t
    -> Constructors.t M.mm
end

module Make : (_ : Rocq_monad.Utils.S) -> S
