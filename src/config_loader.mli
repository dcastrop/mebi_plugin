module type S = sig
  type weak
  type 'a mm

  val load_weak_arg : Api.weak_arg -> weak mm
  val load_weak_arg_opt : Api.weak_arg option -> weak option mm

  type weak_args =
    { a : weak option
    ; b : weak option
    }

  val the_weak_args : weak_args ref option ref
  val reset_the_weak_args : unit -> unit
  val load_weak_args : unit -> unit mm
  val get_the_weak_args : unit -> weak_args option
  val get_the_weak_arg1 : unit -> weak option
  val get_the_weak_arg2 : unit -> weak option
  val get_weak : weak option -> weak option

  (* val api_bounds_to_model_bounds : Api.bounds_args -> Model.Info.Meta.bounds *)
  val the_bounds_args : Api.bounds_args ref
  val load_the_bounds_args : unit -> unit
end

module Make
    (Log : Logger.S)
    (Enc : Encoding.S)
    (M : Rocq_monad_utils.S with type enc = Enc.t)
    (Weak : Weak.S with type enc = Enc.t) :
  S with type weak = Weak.t and type 'a mm = 'a M.mm
