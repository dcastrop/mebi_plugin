module type S = sig
  type enc
  type node
  type tree
  type trees

  (** The monad and Rocq utilities over this encoding. *)
  module M : Rocq_monad_utils.S with type enc = enc and type tree = tree

  (** Where constructors' binders sit ({!Bindings}). *)
  module Bindings : Bindings.S with type 'a mm = 'a M.mm

  (** Constructors' explicit bindings for proofs ({!Constructor_bindings}). *)
  module ConstructorBindings :
    Constructor_bindings.S
    with type 'a mm = 'a M.mm
     and type ind = M.Ind.t
     and type instructions = Bindings.Instructions.t
     and type bindings = Bindings.t
     and type constrmap = Bindings.ConstrMap.t'

  (** The model: LTSs, FSMs and their algorithms ({!Model}). *)
  module Model :
    Model.S
    with type base = enc
     and type tree = tree
     and type trees = trees
     and type constructorbindings = ConstructorBindings.t

  (** The model printed with its Rocq terms ({!Decoder}). *)
  module Decode :
    Decoder.S
    with type enc = enc
     and type state = Model.State.t
     and type states = Model.State.Set.t
     and type partition = Model.Partition.t
     and type label = Model.Label.t
     and type labels = Model.Label.Set.t
     and type note = Model.Note.t
     and type annotation = Model.Annotation.t
     and type annotations = Model.Annotation.Set.t
     and type transition = Model.Transition.t
     and type transitions = Model.Transition.Set.t
     and type action = Model.Action.t
     and type actions = Model.Action.Set.t
     and type actionmap = Model.Action.Map.t'
     and type edgemap = Model.EdgeMap.t'
     and type rocqlts = Model.Info.Meta.RocqLTS.t
     and type info = Model.Info.t
     and type lts = Model.LTS.t
     and type fsm = Model.FSM.t
     and type result = Model.Bisimilarity.Result.t
     and type bisimilarity = Model.Bisimilarity.t

  (** The plugin's theory terms, by encoding ({!Theories_enc}). *)
  module Theory :
    Theories_enc.S
    with type enc = enc
     and type 'a mm = 'a M.mm
     and type 'a im = 'a M.mm

  (** Silent labels ({!Weak}). *)
  module Weak : Weak.S with type enc = enc

  (** The configuration loaded for a command ({!Config_loader}). *)
  module Config :
    Config_loader.S with type weak = Weak.t and type 'a mm = 'a M.mm

  (** [result_log ?decode enc dec] is the printer to log a result with:
      [dec], showing Rocq terms, when [decode] (the default) and
      [DecodeResults] are on; [enc], showing encodings, otherwise. Raises
      nothing. *)
  val result_log
    :  ?decode:bool
    -> (module Json.S with type k = 'a)
    -> (module Json.S with type k = 'a)
    -> (module Json.S with type k = 'a)

  (** [handle_results k s x printer] logs [x] at the kind [k] under the
      title [s], and, for a [Result] with [DumpResults] on, writes it as JSON
      into [./_dumps/]. Raises whatever [printer] raises (propagated). *)
  val handle_results
    :  Output.Kind.t
    -> string
    -> 'a
    -> (module Json.S with type k = 'a)
    -> unit

  (** [extract_lts lts t using weak] is the LTS of the term [t] by the
      relation [lts] ({!Graph.S.build}, then {!Graph.S.extract}), marked
      incomplete if any premise was approximated, and checked against the
      [FailIf] flags.

      @raise Rocq_monad_utils.S.Errors.MEBI_exn
        when run, for an empty LTS (with [FailIf Empty]) or an incomplete
        one (with [FailIf Incomplete], the default) (raised here, via
        {!Rocq_monad_utils.S.Err}). Also raises what {!Graph.S.build}
        raises (propagated). *)
  val extract_lts
    :  Libnames.qualid
    -> Constrexpr.constr_expr
    -> Libnames.qualid list
    -> Weak.t option
    -> Model.LTS.t M.mm

  (** [similarity r] is, for a bisimilarity check's result [r], the greatest
      weak simulation from FSM a to FSM b among the pairs reachable from
      their start states ({!Model.Product.simulation}), or [None] if either
      FSM has no start state. When an FSM is saturated on demand, that walk
      saturates state after state, so it needs [MeBi Config Bounds Game <n>]
      and stays within it.

      @raise CErrors.UserError
        when an FSM is saturated on demand and no game bound is set, or the
        game is larger than it (raised here). *)
  val similarity : Model.Bisimilarity.t -> Model.Product.Pair.Set.t option

  (** The [MeBi Run] commands. *)
  module Command : sig
    (** [build_lts ?weak lts t using] is {!extract_lts}, with the first
        system's silent label when [weak] is not given. Raises as
        {!extract_lts}, when run. *)
    val build_lts
      :  ?weak:Weak.t option
      -> Libnames.qualid
      -> Constrexpr.constr_expr
      -> Libnames.qualid list
      -> Model.LTS.t M.mm

    (** [build_fsm ?weak lts t using] is {!build_lts} as an FSM. Raises as
        {!extract_lts}, when run. *)
    val build_fsm
      :  ?weak:Weak.t option
      -> Libnames.qualid
      -> Constrexpr.constr_expr
      -> Libnames.qualid list
      -> Model.FSM.t M.mm

    (** A [MeBi Run] command: what to build or check, from which term and
        relation. *)
    type t =
      | MakeLTS of rocq_args
      | MakeFSM of rocq_args
      | Saturate of rocq_args
      | Minimize of rocq_args
      | Merge of rocq_pair
      | CheckBisim of rocq_pair
      | CheckSim of rocq_pair
      | BenchmarkGraph of (rocq_args * (int * int))

    and rocq_args = Constrexpr.constr_expr * Libnames.qualid

    and rocq_pair =
      { a : rocq_args
      ; b : rocq_args
      }

    (** [do_make_lts (t, lts) using] logs the LTS of [t] ([MeBi Run LTS]).
        [None]. Raises as {!extract_lts}, when run. *)
    val do_make_lts
      :  Constrexpr.constr_expr * Libnames.qualid
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    (** [do_make_fsm (t, lts) using] logs the FSM of [t] ([MeBi Run FSM]).
        [None]. Raises as {!extract_lts}, when run. *)
    val do_make_fsm
      :  Constrexpr.constr_expr * Libnames.qualid
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    (** [do_saturate (t, lts) using] logs the FSM of [t], saturated
        ([MeBi Run Saturate]). [None].

        @raise Rocq_monad_utils.S.Errors.MEBI_exn
          when run, if saturation would pass the saturation bound and
          [FailIf Oversaturated] is set (raised here). Also raises as
          {!extract_lts}. *)
    val do_saturate
      :  Constrexpr.constr_expr * Libnames.qualid
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    (** [do_minimize (t, lts) using] logs the FSM of [t] and its minimised
        partition ([MeBi Run Minimize]). [None]. Raises as {!do_saturate}, when
        run. *)
    val do_minimize
      :  Constrexpr.constr_expr * Libnames.qualid
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    (** [build_fsms a b using] is the FSMs of the two systems [a] and [b],
        each with its own silent label, logged. Raises as {!extract_lts},
        when run. *)
    val build_fsms
      :  rocq_args
      -> rocq_args
      -> Libnames.qualid list
      -> (Model.FSM.t * Model.FSM.t) M.mm

    (** [do_merge {a; b} using] logs the two systems' FSMs merged into one
        ([MeBi Run Merge]), [b]'s states renamed apart where they conflict.
        [None]. Raises as {!extract_lts}, when run. *)
    val do_merge
      :  rocq_pair
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    (** [do_check_bisim {a; b} using] is the weak bisimilarity check of the
        two systems ([MeBi Run Bisim]), each saturated, on demand when too
        large; the FSMs and the result are logged.

        @raise Rocq_monad_utils.S.Errors.MEBI_exn
          when run, if they are not bisimilar and [FailIf NotBisimilar] is
          set (the default) (raised here). Also raises as {!extract_lts}
          and, when the two systems share states that move differently,
          Rocq's [UserError]. *)
    val do_check_bisim
      :  rocq_pair
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    (** [do_check_sim { a; b } using] decides whether [a]'s term is weakly
        simulated by [b]'s ([MeBi Run Sim]), reporting the verdict: bisimilar
        states are similar outright; otherwise {!similarity} decides.

        @raise CErrors.UserError
          when they are not similar and [FailIf NotBisimilar] is set (the
          default; a warning otherwise), and as {!similarity} (raised here).
          Also raises as {!do_check_bisim}'s checks, apart from the
          verdict. *)
    val do_check_sim
      :  rocq_pair
      -> Libnames.qualid list
      -> Model.Bisimilarity.t option M.mm

    (** [do_benchmark_graph ((ts, lts), (time, repeat)) using] times the LTS
        extraction of each term of the list [ts] (or of [ts] alone, if it is
        not a list) by [lts], each run for at least [time] seconds, [repeat]
        times ({!Benchmarking}), and logs the samples ([MeBi Benchmark]).
        [None].

        @raise NothingToBenchmark
          when run, for a list type that is not Rocq's [list] (raised here).
          Also raises as {!extract_lts}. *)
    val do_benchmark_graph
      :  rocq_args * (int * int)
      -> Libnames.qualid list
      -> Decode.bisimilarity option M.mm

    (** [run using x] runs the command [x], with the relations [using]
        available to constructors' premises, after loading the bounds and
        silent labels configured: a bisimilarity result for [CheckBisim] and
        [CheckSim], [None] otherwise.

        Raises as the command it runs, when run (propagated). *)
    val run : Libnames.qualid list -> t -> Model.Bisimilarity.t option M.mm
  end
end

module Make (Enc : Encoding.S) :
  S
  with type enc = Enc.t
   and type node = Enc.Tree.Node.t
   and type tree = Enc.Tree.t
   and type trees = Enc.Trees.t

(** [make ?enc ()] is a new wrapper over the encoding [enc ()] (by default
    {!Api.make_enc_int}). Raises nothing. *)
val make : ?enc:(unit -> (module Encoding.S)) -> unit -> (module S)

(** The shared instance the [MeBi ...] vernaculars run against, created on first
    use. Commands no longer build their own: per-command state lifetime is
    carried by the [~reset_encoding:true] they already pass, not by rebuilding
    the module tree. *)
val get : unit -> (module S)

(** Drops the shared instance so the next [get] builds a fresh one. *)
val reset : unit -> unit
