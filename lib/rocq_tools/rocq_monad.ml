module type S = Rocq_monad_intf.S

module Make (Enc : Encoding.S) : S with type enc = Enc.t = struct
  module BiEnc = Bi_encoding.Make (Enc)
  include BiEnc

  type 'a mm = wrapper ref -> 'a in_wrapper

  and wrapper =
    { ctx : Rocq_context.t ref
    ; maps : maps ref
    }

  and 'a in_wrapper =
    { state : wrapper ref
    ; value : 'a
    }

  (* *)

  (* See the [.mli]. The context is a value rather than a functor
     parameter, so installing it no longer rebuilds this module or discards
     the encoding table. It is still fixed per instance: [encode], [fstring]
     and [Rocq_monad_utils.get_encoding] all call [run] themselves, so a
     per-call [?ctx] could not have been honoured by them, and the table
     would end up hashing some entries under one [sigma] and looking them up
     under another. See [Proof_solver_wrapper.Make]. *)
  let run ?(reset_encoding : bool = false) (x : 'a mm) : 'a =
    if reset_encoding then reset () else initialize ();
    let a : 'a in_wrapper =
      x (ref { ctx = ref (current_ctx ()); maps = get_the_maps () })
    in
    a.value
  ;;

  (* See the [.mli] for [return], [bind], [map] and [product]. *)
  let return (x : 'a) : 'a mm =
    fun (st : wrapper ref) -> { state = st; value = x }
  [@@inline always]
  ;;

  let bind (x : 'a mm) (f : 'a -> 'b mm) : 'b mm =
    fun (st : wrapper ref) ->
    let a : 'a in_wrapper = x st in
    f a.value a.state
  [@@inline always]
  ;;

  let map (f : 'a -> 'b) (x : 'a mm) : 'b mm =
    fun (st : wrapper ref) ->
    let x_st : 'a in_wrapper = x st in
    { x_st with value = f x_st.value }
  [@@inline always]
  ;;

  let product (x : 'a mm) (y : 'b mm) : ('a * 'b) mm =
    bind x (fun a -> bind y (fun b -> return (a, b)))
  [@@inline always]
  ;;

  (* See the [.mli]. *)
  let rec iterate
            (index : int)
            (upper_bound : int)
            (acc : 'a)
            (f : int -> 'a -> 'a mm)
    : 'a mm
    =
    Logger.trace __FUNCTION__;
    if index > upper_bound
    then return acc
    else bind (f index acc) (fun acc' -> iterate (index + 1) upper_bound acc' f)
  ;;

  (* See the [.mli]. *)
  let rec fold (acc : 'a) (xs : 'b list) (f : 'b -> 'a -> 'a mm) : 'a mm =
    match xs with
    | [] -> return acc
    | x :: xs -> bind (f x acc) (fun acc' -> fold acc' xs f)
  ;;

  (* See the [.mli]. *)
  let state
        (f : Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
        (st : wrapper ref)
    : 'a in_wrapper
    =
    Logger.trace __FUNCTION__;
    let sigma, a = f !(!st.ctx).env !(!st.ctx).sigma in
    st := { !st with ctx = ref { !(!st.ctx) with sigma } };
    { state = st; value = a }
  ;;

  (* See the [.mli]. *)
  let sandbox ?(sigma : Evd.evar_map option) (m : 'a mm) (st : wrapper ref)
    : 'a in_wrapper
    =
    Logger.trace __FUNCTION__;
    let st_copy : wrapper = !st in
    let st =
      Option.cata
        (fun (sigma : Evd.evar_map) ->
          ref { !st with ctx = ref { !(!st.ctx) with sigma } })
        st
        sigma
    in
    let result : 'a in_wrapper = m st in
    { state = ref st_copy; value = result.value }
  ;;

  module type SYNTAX = sig
    val ( let+ ) : 'a mm -> ('a -> 'b) -> 'b mm
    val ( let* ) : 'a mm -> ('a -> 'b mm) -> 'b mm

    val ( let$ )
      :  (Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
      -> ('a -> 'b mm)
      -> 'b mm

    val ( let$* )
      :  (Environ.env -> Evd.evar_map -> Evd.evar_map)
      -> (unit -> 'b mm)
      -> 'b mm

    val ( let$+ )
      :  (Environ.env -> Evd.evar_map -> 'a)
      -> ('a -> 'b mm)
      -> 'b mm

    val ( and+ ) : 'a mm -> 'b mm -> ('a * 'b) mm
  end

  module Syntax : SYNTAX = struct
    let ( let+ ) x f = map f x
    let ( let* ) = bind
    let ( let$ ) f g = bind (state f) g
    let ( let$* ) f g = bind (state (fun e s -> f e s, ())) g
    let ( let$+ ) f g = bind (state (fun e s -> s, f e s)) g
    let ( and+ ) x y = product x y
  end

  (* See the [.mli]. *)
  let econstr_normalize (x : EConstr.t) : EConstr.t mm =
    Logger.trace __FUNCTION__;
    let open Syntax in
    let$+ t env sigma = Reductionops.nf_all env sigma x in
    return t
  ;;

  (* See the [.mli]. *)
  let encode (x : EConstr.t) : Enc.t = run (econstr_normalize x) |> encode

  let get_env (st : wrapper ref) : Environ.env in_wrapper =
    { state = st; value = !(!st.ctx).env }
  ;;

  let get_sigma (st : wrapper ref) : Evd.evar_map in_wrapper =
    { state = st; value = !(!st.ctx).sigma }
  ;;

  let get_fwdmap (st : wrapper ref) : Enc.t F.t in_wrapper =
    { state = st; value = !(!st.maps).fwd }
  ;;

  (* See the [.mli]. *)
  let fstring (f : Environ.env -> Evd.evar_map -> 'a -> string) : 'a -> string =
    run
      (let open Syntax in
       let* env = get_env in
       let* sigma = get_sigma in
       return (f env sigma))
  ;;
end
