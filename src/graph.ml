module type S = sig
  type weak
  type 'a mm
  type t
  type lts

  val build
    :  ?weak:weak option
    -> Constrexpr.constr_expr
    -> Libnames.qualid
    -> Names.GlobRef.t list
    -> t mm

  val extract : t -> lts mm
end

module Make
    (Log : Logger.S)
    (Enc : Encoding.S)
    (M : Rocq_monad_utils.S with type enc = Enc.t and type tree = Enc.Tree.t)
    (Weak : Weak.S with type enc = Enc.t)
    (Theory :
       Theories_enc.S
       with type enc = Enc.t
        and type 'a mm = 'a M.mm
        and type 'a im = 'a M.mm)
    (ConstructorBindings :
       Constructor_bindings.S with type 'a mm = 'a M.mm and type ind = M.Ind.t)
    (Model :
       Model_.S
       with type base = Enc.t
        and type tree = Enc.Tree.t
        and type trees = Enc.Trees.t
        and type constructorbindings = ConstructorBindings.t)
    (X : Graph_type.Args with type enc = Enc.t and type tree = Enc.Tree.t) :
  S with type weak = Weak.t and type 'a mm = 'a M.mm and type lts = Model.LTS.t =
struct
  type weak = Weak.t
  type 'a mm = 'a M.mm
  type lts = Model.LTS.t

  module G :
    Graph_type.S
    with type enc = Enc.t
     and type tree = Enc.Tree.t
     and type action = Model.Action.t
     and type weak = Weak.t
     and type ind = M.Ind.t
     and module B = M.B
     and module F = M.F
     and type indmap = M.Ind.t M.B.t
     and type 'a mm = 'a M.mm =
    Graph_type.Make (Log) (Enc) (M) (Weak) (Theory) (ConstructorBindings)
      (Model)
      (X)

  type t = G.t

  module Builder :
    Graph_builder.S
    with type t = G.t
     and type 'a mm = 'a M.mm
     and type enc = Enc.t
     and type tree = Enc.Tree.t
     and type action = Model.Action.t
     and type constructor = M.Constructor.t
     and type states = G.States.t =
    Graph_builder.Make (Log) (Enc) (M) (Model) (G)

  open G
  module LTS = Model.LTS

  let encode_indlts (x : Names.GlobRef.t) (ltsmap : G.indmap) : unit M.mm =
    let open M.Syntax in
    (* NOTE: [M.Ind.lts] automatically encodes [x.ind] into the bi-enc maps. *)
    let* x : M.Ind.t = M.Ind.lts x in
    M.Ind.log ~__FUNCTION__ ~m:Debug ~s:"x" x;
    (* NOTE: [ind_defs] is a separate map, so add again using same enc. *)
    M.B.replace ltsmap x.enc x;
    M.return ()
  ;;

  let build_ltsmap (grefs : Names.GlobRef.t list) : M.Ind.t M.B.t M.mm =
    Log.trace __FUNCTION__;
    let ltsmap : G.indmap = M.B.create (List.length grefs) in
    let open M.Syntax in
    let f (i : int) () = encode_indlts (List.nth grefs i) ltsmap in
    let* () = M.iterate 0 (List.length grefs - 1) () f in
    M.return ltsmap
  ;;

  exception LTSMapDoesNotContainPrimaryLTS of G.indmap * Libnames.qualid

  let get_primary_lts (ltsmap : G.indmap) (primary_lts : Libnames.qualid)
    : M.Ind.t M.mm
    =
    Log.trace __FUNCTION__;
    let open M.Syntax in
    let* x : M.Ind.t = Nametab.global primary_lts |> M.Ind.lts in
    M.Ind.log ~__FUNCTION__ ~m:Debug ~s:"primary lts" x;
    (* NOTE: below is a sanity check *)
    try M.encode x.ind |> M.B.find ltsmap |> M.return with
    | Not_found -> raise (LTSMapDoesNotContainPrimaryLTS (ltsmap, primary_lts))
  ;;

  (** normalize and encode the initial term *)
  let initialize_term (x : Constrexpr.constr_expr) (lts : M.Ind.t)
    : EConstr.t M.mm
    =
    Log.trace __FUNCTION__;
    let open M.Syntax in
    let* x : EConstr.t = M.constrexpr_to_econstr x in
    let* x : EConstr.t = M.econstr_normalize x in
    let$* _unit env sigma =
      M.Ind.get_lts_term_type lts |> Typing.check env sigma x
    in
    M.return x
  ;;

  let encode_initial_term (x : Constrexpr.constr_expr) (lts : M.Ind.t)
    : Enc.t M.mm
    =
    Log.trace __FUNCTION__;
    let open M.Syntax in
    let* x : EConstr.t = initialize_term x lts in
    let init : Enc.t = M.encode x in
    Enc.log ~__FUNCTION__ ~m:Debug ~s:"init enc" init;
    M.return init
  ;;

  let build
        ?(weak : Weak.t option = None)
        (starting_term : Constrexpr.constr_expr)
        (primary_lts : Libnames.qualid)
        (grefs : Names.GlobRef.t list)
    : G.t M.mm
    =
    Log.trace __FUNCTION__;
    let open M.Syntax in
    let* ltsmap : G.indmap = build_ltsmap grefs in
    let* primary_lts : M.Ind.t = get_primary_lts ltsmap primary_lts in
    let* init : Enc.t = encode_initial_term starting_term primary_lts in
    Log.info "Building the Graph...";
    let the_graph : t ref = ref (create init ltsmap primary_lts weak) in
    Queue.push init !the_graph.to_visit;
    let* the_graph : t = Builder.build !the_graph in
    Log.info "Finished Building Graph.";
    M.return the_graph
  ;;

  module Extract :
    Graph_extract_lts.S
    with type t = G.t
     and type lts = Model.LTS.t
     and type 'a mm = 'a M.mm =
    Graph_extract_lts.Make (Log) (Enc) (M) (Weak) (Theory) (ConstructorBindings)
      (Model)
      (X)
      (G)

  let extract (g : G.t) : LTS.t M.mm =
    Log.trace __FUNCTION__;
    Log.info "Extracting LTS from Graph...";
    let open M.Syntax in
    let* x : LTS.t = Extract.extract g in
    Log.info "Finished Extracting LTS.";
    M.return x
  ;;
end
