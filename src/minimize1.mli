exception UnexpectedBisimResult of Bisimilarity.result

type result = Fsm.fsm * (Fsm.state, Fsm.state) Hashtbl.t

val run : ?params:Utils.Logging.params -> Fsm.fsm -> result
