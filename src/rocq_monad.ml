module Make (Ctx : Rocq_context.SRocq_context) (Enc : Encoding.SEncoding) =
struct
  module Ctx : Rocq_context.SRocq_context = Ctx
  module Enc : Encoding.SEncoding = Enc
  include Bi_encoding.Make (Ctx) (Enc)

  let bienc_to_list : unit -> (Enc.t * EConstr.t) list = to_list

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
  let run ?(reset_encoding : bool = false) (x : 'a mm) : 'a =
    reset ();
    let a : 'a in_wrapper = x (ref { ctx = Ctx.get (); maps = the_maps () }) in
    a.value
  ;;

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

  (** Monadic for loop *)
  let rec iterate
            (index : int)
            (upper_bound : int)
            (acc : 'a)
            (f : int -> 'a -> 'a mm)
    : 'a mm
    =
    if index > upper_bound
    then return acc
    else bind (f index acc) (fun acc' -> iterate (index + 1) upper_bound acc' f)
  ;;

  (** [state f] provides the [env] and [sigma] for [f] and returns the result.
  *)
  let state
        (f : Environ.env -> Evd.evar_map -> Evd.evar_map * 'a)
        (st : wrapper ref)
    : 'a in_wrapper
    =
    let sigma, a = f !(!st.ctx).env !(!st.ctx).sigma in
    st := { !st with ctx = ref { !(!st.ctx) with sigma } };
    { state = st; value = a }
  ;;

  (** [sandbox ?sigma m] evaluates [m] without updating the state of the monad.
      @param ?sigma
        allows a specific sigma to be used (instead of the one from the state).
  *)
  let sandbox ?(sigma : Evd.evar_map option) (m : 'a mm) (st : wrapper ref)
    : 'a in_wrapper
    =
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

  (* *)
  let get_ctx (st : wrapper ref) : Rocq_context.t in_wrapper =
    { state = st; value = !(!st.ctx) }
  ;;

  let get_env (st : wrapper ref) : Environ.env in_wrapper =
    { state = st; value = !(!st.ctx).env }
  ;;

  let get_sigma (st : wrapper ref) : Evd.evar_map in_wrapper =
    { state = st; value = !(!st.ctx).sigma }
  ;;

  (* *)
  let get_maps (st : wrapper ref) : maps in_wrapper =
    { state = st; value = !(!st.maps) }
  ;;

  let get_fwdmap (st : wrapper ref) : Enc.t F.t in_wrapper =
    { state = st; value = !(!st.maps).fwd }
  ;;

  let get_bckmap (st : wrapper ref) : EConstr.t B.t in_wrapper =
    { state = st; value = !(!st.maps).bck }
  ;;

  (* *)
  let fstring (f : Environ.env -> Evd.evar_map -> 'a -> string) : 'a -> string =
    run
      (let open Syntax in
       let* env = get_env in
       let* sigma = get_sigma in
       return (f env sigma))
  ;;

  module Tree = Enc_tree.Make (Enc)

  module Constructor = struct
    type t = EConstr.t * EConstr.t * Tree.t

    let to_string
          (env : Environ.env)
          (sigma : Evd.evar_map)
          ((action, destination, tree) : t)
      : string
      =
      Utils.Strfy.record
        [ "action", Rocq_utils.Strfy.econstr env sigma action
        ; "destination", Rocq_utils.Strfy.econstr env sigma destination
        ; "tree", Tree.to_string tree
        ]
    ;;
  end

  let make_state_tree_pair_set : (module Set.S with type elt = Enc.t * Tree.t) =
    (module Set.Make (struct
         type t = Enc.t * Tree.t

         let compare t1 t2 =
           Utils.compare_chain
             [ Enc.compare (fst t1) (fst t2); Tree.compare (snd t1) (snd t2) ]
         ;;
       end))
  ;;
end
