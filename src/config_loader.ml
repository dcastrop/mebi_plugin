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
  S with type weak = Weak.t and type 'a mm = 'a M.mm = struct
  type weak = Weak.t
  type 'a mm = 'a M.mm

  let load_weak_arg : Api.weak_arg -> weak M.mm =
    let open M.Syntax in
    function
    | Api.Option label_tref ->
      let* label : EConstr.t = M.constrexpr_to_econstr label_tref in
      let label_enc : Enc.t = M.encode label in
      (* NOTE: sanity check we can decode these *)
      let _ : EConstr.t = M.decode label_enc in
      Weak.Option label_enc |> M.return
    | Api.Custom (tau_tref, label_ref) ->
      let* tau : EConstr.t = M.constrexpr_to_econstr tau_tref in
      let tau_enc : Enc.t = M.encode tau in
      let* ind, (mib, mip) = Nametab.global label_ref |> M.Ind.lts_type_mind in
      let label : EConstr.t = Rocq_utils.get_ind_ty ind mib in
      let label_enc : Enc.t = M.encode label in
      (* NOTE: sanity check we can decode these *)
      let _ : EConstr.t = M.decode tau_enc in
      let _ : EConstr.t = M.decode label_enc in
      Weak.Custom (tau_enc, label_enc) |> M.return
  ;;

  let load_weak_arg_opt : Api.weak_arg option -> weak option M.mm = function
    | None -> M.return None
    | Some x ->
      let open M.Syntax in
      let* y = load_weak_arg x in
      M.return (Some y)
  ;;

  type weak_args =
    { a : weak option
    ; b : weak option
    }

  let the_weak_args : weak_args ref option ref = ref None
  let reset_the_weak_args () : unit = the_weak_args := None

  let load_weak_args () : unit M.mm =
    Log.trace __FUNCTION__;
    let open M.Syntax in
    match !Api.the_weak_args with
    | None ->
      the_weak_args := None;
      M.return ()
    | Some x ->
      let* a = load_weak_arg_opt !x.a in
      let* b = load_weak_arg_opt !x.b in
      the_weak_args := Some (ref { a; b });
      M.return ()
  ;;

  let get_the_weak_args () : weak_args option =
    match !the_weak_args with None -> None | Some x -> Some !x
  ;;

  let get_the_weak_arg1 () : weak option =
    match get_the_weak_args () with None -> None | Some x -> x.a
  ;;

  let get_the_weak_arg2 () : weak option =
    match get_the_weak_args () with None -> None | Some x -> x.b
  ;;

  (** [get_weak x] will return [get_the_weak_arg1] in the case that [x] is [None], else returns [x].
  *)
  let get_weak : Weak.t option -> Weak.t option = function
    | None -> get_the_weak_arg1 ()
    | Some x -> Some x
  ;;

  (***********************************************************************)

  let the_bounds_args : Api.bounds_args ref = ref Api.default_bounds
  let load_the_bounds_args () : unit = the_bounds_args := !Api.the_bounds_args
end
