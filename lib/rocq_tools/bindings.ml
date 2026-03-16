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
  S with type 'a mm = 'a M.mm = struct
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

    include
      Json.Thing.Make
        (Log)
        (struct
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

    let rec append (x : t) : t -> t
      =
      (* Log.trace __FUNCTION__; *)
      function
      | Arg { root; index; cont } -> Arg { root; index; cont = append x cont }
      | Undefined -> x
      | Done -> raise (CannotAppendDone ())
    ;;

    let rec length : t -> int =
      (* Log.trace __FUNCTION__; *)
      function
      | Undefined -> 0
      | Done -> -1
      | Arg { cont; _ } -> 1 + length cont
    ;;
  end

  module NamedInstructions = struct
    type t = Names.Name.t * Instructions.t

    include
      Json.Thing.Make
        (Log)
        (struct
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
        (Log)
        (struct
          module Map = Map_

          type value = NamedInstructions.t

          let name = "ConstrMap"
        end)
        (struct
          include
            Json.Thing.Make
              (Log)
              (struct
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

    let update (cmap : t') (k : Constr.t) ((name, inst) : NamedInstructions.t)
      : unit
      =
      Log.trace __FUNCTION__;
      match find_opt cmap k with
      | None -> add cmap k (name, inst)
      | Some (name', inst') ->
        let f = Instructions.length in
        (match Int.compare (f inst) (f inst') with
         | -1 -> replace cmap k (name, inst)
         | _ -> ())
    ;;

    exception Rocq_bindings_CannotFindBindingName of EConstr.t

    let find_name (name_pairs : (EConstr.t * Names.Name.t) list) (x : EConstr.t)
      : Names.Name.t mm
      =
      (* Log.trace __FUNCTION__; *)
      Log.thing ~__FUNCTION__ Debug "x" x (Of Strfy.econstr);
      let open Syntax in
      let f (i : int) : Names.Name.t option -> Names.Name.t option mm = function
        | Some n ->
          Log.thing ~__FUNCTION__ Trace "Some" n (Of Rocq_utils.Strfy.name);
          Some n |> return
        | None ->
          Log.trace ~__FUNCTION__ "None";
          let y, z = List.nth name_pairs i in
          let* eq = econstr_eq ~enc:false x y in
          if eq
          then (
            Log.thing ~__FUNCTION__ Debug "eq x" z (Of Rocq_utils.Strfy.name);
            Some z |> return)
          else return None
      in
      let* matches = iterate 0 (List.length name_pairs - 1) None f in
      match matches with
      | None ->
        Log.trace ~__FUNCTION__ "Raise (Rocq_bindings_CannotFindBindingName x)";
        raise (Rocq_bindings_CannotFindBindingName x)
      | Some n ->
        Log.trace ~__FUNCTION__ "Some (_, n)";
        return n
    ;;

    let extract_binding_map
          (name_pairs : (EConstr.t * Names.Name.t) list)
          (x : EConstr.t)
          (y : Constr.t)
      : t' mm
      =
      Log.trace __FUNCTION__;
      let open Syntax in
      let m : t' = create 0 in
      let rec f
                (acc : (Constr.t * NamedInstructions.t) list)
                (b : Instructions.t)
                ((x, y) : EConstr.t * Constr.t)
        : unit mm
        =
        Log.trace __FUNCTION__;
        let* x_kind = econstr_kind x in
        match x_kind, Constr.kind y with
        | App (xty, xtys), App (yty, ytys) ->
          let* eq = econstr_eq ~enc:false xty (EConstr.of_constr yty) in
          if eq
          then (
            (* NOTE: set to [-1] so that it is [0] on first use. *)
            let (tysindex, _), _ = Utils.new_int_counter ~start:(-1) () in
            let xytys = Array.combine xtys ytys in
            let iter_body (i : int) () =
              Log.trace __FUNCTION__;
              let b' =
                Instructions.append
                  (Arg { root = yty; index = tysindex (); cont = Undefined })
                  b
              in
              f acc b' xytys.(i)
            in
            iterate 0 (Array.length xytys - 1) () iter_body)
          else return ()
        | _, Rel _ ->
          let* name = find_name name_pairs x in
          update m y (name, Instructions.append Done b);
          return ()
        | _, _ -> return ()
      in
      let* () = f [] Undefined (x, y) in
      return m
    ;;

    let make_opt
          (name_pairs : (EConstr.t * Names.Name.t) list)
          ((evar, rel) : EConstr.t * Constr.t)
      : t' option mm
      =
      Log.trace __FUNCTION__;
      let open Syntax in
      let* m = extract_binding_map name_pairs evar rel in
      match to_seq_values m |> List.of_seq with
      | [] -> return None
      | [ (_, Instructions.Done) ] -> return None
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

  include
    Json.Thing.Make
      (Log)
      (struct
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

  let use_no_bindings (xs : ConstrMap.t' option list) : bool =
    Log.trace __FUNCTION__;
    List.filter (function None -> false | _ -> true) xs |> List.is_empty
  ;;

  (** [] ...
      @param name_map is a tuple list of evars and corresponding binding_names
  *)
  let extract
        (name_pairs : (EConstr.t * Names.Name.t) list)
        (from : EConstr.t * Constr.t)
        (action : EConstr.t * Constr.t)
        (goto : EConstr.t * Constr.t)
    : t mm
    =
    Log.trace __FUNCTION__;
    let open Syntax in
    let f = ConstrMap.make_opt name_pairs in
    let* from : ConstrMap.t' option = f from in
    let* action : ConstrMap.t' option = f action in
    let* goto : ConstrMap.t' option = f goto in
    if use_no_bindings [ from; action; goto ]
    then return No_Bindings
    else return (Use_Bindings { from; action; goto })
  ;;
end
