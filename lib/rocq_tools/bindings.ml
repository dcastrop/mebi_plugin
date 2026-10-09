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
      :  ?keep_var:bool
      -> (EConstr.t * Names.Name.t) list
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

module Make (M : Rocq_monad_utils.S) : S with type 'a mm = 'a M.mm = struct
  type 'a mm = 'a M.mm

  open M

  module Instructions = struct
    type t =
      | Undefined
      | Done
      | Arg of
          { root : Constr.t
          ; index : int
          ; cont : t
          }

    include Json.Thing.Make (struct
        type k = t

        let name = "Instructions"

        let json ?as_elt (x : t) : Yojson.t =
          let rec f : t -> Yojson.t = function
            | Undefined -> `String "Undefined"
            | Done -> `String "Done"
            | Arg { root; index; cont } ->
              `Assoc
                [ "root", `String (Strfy.constr root)
                ; "index", `Int index
                ; "cont", f cont
                ]
          in
          f x
        ;;
      end)

    exception CannotAppendDone of unit

    (* See the [.mli]. *)
    let rec append (x : t) : t -> t
      =
      (* Logger.trace __FUNCTION__; *)
      function
      | Arg { root; index; cont } -> Arg { root; index; cont = append x cont }
      | Undefined -> x
      | Done -> raise (CannotAppendDone ())
    ;;

    (* See the [.mli]. *)
    let rec length : t -> int =
      (* Logger.trace __FUNCTION__; *)
      function
      | Undefined -> 0
      | Done -> -1
      | Arg { cont; _ } -> 1 + length cont
    ;;
  end

  module NamedInstructions = struct
    type t = Names.Name.t * Instructions.t

    include Json.Thing.Make (struct
        type k = t

        let name = "NamedInstructions"

        let json ?(as_elt : bool = false) x =
          `Assoc
            [ "name", `String (Rocq_utils.Strfy.name (fst x))
            ; "instructions", Instructions.json ~as_elt:true (snd x)
            ]
        ;;
      end)
  end

  module ConstrMap = struct
    module Map_ = Hashtbl.Make (struct
        type t = Constr.t

        let equal : t -> t -> bool = Constr.equal
        let hash : t -> int = Constr.hash
      end)

    include Map_

    type t' = NamedInstructions.t t

    include
      Json.Map.Make
        (struct
          module Map = Map_

          type value = NamedInstructions.t

          let name = "ConstrMap"
        end)
        (struct
          include Json.Thing.Make (struct
              type k = Constr.t

              let name = "Constr"
              let json ?(as_elt : bool = false) x = `String (Strfy.constr x)
            end)

          let compare a b : int = Constr.compare a b
        end)
        (struct
          include NamedInstructions

          let compare a b : int = 0
        end)

    (* See the [.mli]. *)
    let update (cmap : t') (k : Constr.t) ((name, inst) : NamedInstructions.t)
      : unit
      =
      Logger.trace __FUNCTION__;
      match find_opt cmap k with
      | None -> add cmap k (name, inst)
      | Some (name', inst') ->
        let f = Instructions.length in
        (match Int.compare (f inst) (f inst') with
         | -1 -> replace cmap k (name, inst)
         | _ -> ())
    ;;

    exception Rocq_bindings_CannotFindBindingName of EConstr.t

    (** [first_named x name_pairs] is the name paired with the first evar
        in [name_pairs] equal to [x] (compared without encoding), or [None]
        if there is none. Raises nothing. *)
    let rec first_named (x : EConstr.t)
      : (EConstr.t * Names.Name.t) list -> Names.Name.t option mm
      =
      let open Syntax in
      function
      | [] -> return None
      | (y, z) :: name_pairs ->
        let* eq = econstr_eq ~enc:false x y in
        if eq
        then (
          Logger.thing ~__FUNCTION__ Trace "eq x" z Rocq_utils.Strfy.name;
          return (Some z))
        else first_named x name_pairs
    ;;

    (* See the [.mli]. {!first_named}, raising if there is no match. *)
    let find_name (name_pairs : (EConstr.t * Names.Name.t) list) (x : EConstr.t)
      : Names.Name.t mm
      =
      Logger.thing ~__FUNCTION__ Trace "x" x Strfy.econstr;
      let open Syntax in
      let* matches = first_named x name_pairs in
      match matches with
      | None ->
        Logger.trace
          ~__FUNCTION__
          "Raise (Rocq_bindings_CannotFindBindingName x)";
        raise (Rocq_bindings_CannotFindBindingName x)
      | Some n ->
        Logger.trace ~__FUNCTION__ "Some (_, n)";
        return n
    ;;

    (** [walk m name_pairs path (x, y)] records in [m] ({!update}) the path
        to each binder in [x], [path] being the path to [x] so far (its open
        end still [Undefined]): see {!extract_binding_map}. [x] is the
        constructor's term with an evar per binder, [y] the same term with de
        Bruijn indices. At applications with the same head it walks each
        argument pair, one [Arg] step deeper; where [y] is an index it
        records the binder whose evar [x] is; elsewhere it stops.

        @raise Rocq_bindings_CannotFindBindingName
          when run, if an index's evar is not a binder (propagated from
          {!find_name}). *)
    let rec walk
              (m : t')
              (name_pairs : (EConstr.t * Names.Name.t) list)
              (path : Instructions.t)
              ((x, y) : EConstr.t * Constr.t)
      : unit mm
      =
      Logger.trace __FUNCTION__;
      let open Syntax in
      let* x_kind = econstr_kind x in
      match x_kind, Constr.kind y with
      | App (xty, xtys), App (yty, ytys) ->
        let* eq = econstr_eq ~enc:false xty (EConstr.of_constr yty) in
        if eq
        then (
          let xytys = Array.combine xtys ytys in
          iterate
            0
            (Array.length xytys - 1)
            ()
            (fun index () ->
              let step =
                Instructions.Arg { root = yty; index; cont = Undefined }
              in
              walk m name_pairs (Instructions.append step path) xytys.(index)))
        else return ()
      | _, Rel _ ->
        let* name = find_name name_pairs x in
        update m y (name, Instructions.append Done path);
        return ()
      | _, _ -> return ()
    ;;

    (* See the [.mli]. {!walk} from the root, into a fresh table. *)
    let extract_binding_map
          (name_pairs : (EConstr.t * Names.Name.t) list)
          (x : EConstr.t)
          (y : Constr.t)
      : t' mm
      =
      Logger.trace __FUNCTION__;
      let open Syntax in
      let m : t' = create 0 in
      let* () = walk m name_pairs Undefined (x, y) in
      return m
    ;;

    (* See the [.mli]. *)
    let make_opt
          ?(keep_var : bool = false)
          (name_pairs : (EConstr.t * Names.Name.t) list)
          ((evar, rel) : EConstr.t * Constr.t)
      : t' option mm
      =
      Logger.trace __FUNCTION__;
      let open Syntax in
      let* m = extract_binding_map name_pairs evar rel in
      match to_seq_values m |> List.of_seq with
      | [] -> return None
      | [ (_, Instructions.Done) ] when Bool.not keep_var -> return None
      | _ :: _ -> return (Some m)
    ;;
  end

  type t =
    | No_Bindings
    | Use_Bindings of
        { from : ConstrMap.t' option
        ; action : ConstrMap.t' option
        ; goto : ConstrMap.t' option
        }

  include Json.Thing.Make (struct
      type k = t

      let name = "Bindings"

      let json ?as_elt : t -> Yojson.t = function
        | No_Bindings -> `String "NoBindings"
        | Use_Bindings { from; action; goto } ->
          `Assoc
            [ "from", Json.option ~as_elt:true ConstrMap.json from
            ; "action", Json.option ~as_elt:true ConstrMap.json action
            ; "goto", Json.option ~as_elt:true ConstrMap.json goto
            ]
      ;;
    end)

  (* See the [.mli]. *)
  let use_no_bindings (xs : ConstrMap.t' option list) : bool =
    Logger.trace __FUNCTION__;
    List.filter (function None -> false | _ -> true) xs |> List.is_empty
  ;;

  (* See the [.mli]. *)
  let extract
        (name_pairs : (EConstr.t * Names.Name.t) list)
        (from : EConstr.t * Constr.t)
        (action : EConstr.t * Constr.t)
        (goto : EConstr.t * Constr.t)
    : t mm
    =
    Logger.trace __FUNCTION__;
    let open Syntax in
    let f = ConstrMap.make_opt name_pairs in
    let* from : ConstrMap.t' option = f from in
    let* action : ConstrMap.t' option = f action in
    let* goto : ConstrMap.t' option =
      ConstrMap.make_opt ~keep_var:true name_pairs goto
    in
    if use_no_bindings [ from; action; goto ]
    then return No_Bindings
    else return (Use_Bindings { from; action; goto })
  ;;
end
