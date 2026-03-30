module type S = sig
  type base
  type constructorbindings
  type labels

  module Meta : sig
    module Bounds : sig
      type t =
        | States of int
        | Transitions of int
        | Merged of t * t

      include Json.S with type k = t (** @closed *)
    end

    module RocqLTS : sig
      type t =
        { base : base
        ; constructors : constructorbindings list
        }

      include Json.S with type k = t (** @closed *)
    end

    type t =
      { is_complete : bool
      ; is_merged : bool
      ; bounds : Bounds.t
      ; lts : RocqLTS.t list
      }

    include Json.S with type k = t (** @closed *)

    val merge : t -> t -> t
    val merge_opt : t option -> t option -> t option
  end

  type t =
    { meta : Meta.t option
    ; weak_labels : labels
    ; nums : nums option
    }

  and nums =
    { states : int
    ; labels : int
    ; edges : int
    }

  include Json.S with type k = t (** @closed *)

  val merge : ?nums:nums option -> t -> t -> t
end

module Make
    (Log : Logger.S)
    (Base : Base_term.S)
    (Labels : Labels.S)
    (ConstructorBindings : Constructor_bindings.S) :
  S
  with type base = Base.t
   and type constructorbindings = ConstructorBindings.t
   and type labels = Labels.t
