(** {!Bdd.S} over CUDD, through [mlcuddidl]. Dynamic reordering is off,
    so the variable order is the numbering. *)

let man : Cudd.Man.d Cudd.Man.t option ref = ref None
let get () = Option.get !man

type t = Cudd.Man.d Cudd.Bdd.t

let name = "cudd"

let init ~vars =
  let m = Cudd.Man.make_d ~numVars:vars () in
  Cudd.Man.disable_autodyn m;
  man := Some m
;;

let dtrue () = Cudd.Bdd.dtrue (get ())
let dfalse () = Cudd.Bdd.dfalse (get ())
let var i = Cudd.Bdd.ithvar (get ()) i
let dnot = Cudd.Bdd.dnot
let dand = Cudd.Bdd.dand
let dor = Cudd.Bdd.dor
let is_false = Cudd.Bdd.is_false
let equal = Cudd.Bdd.is_equal

let cube vs =
  List.fold_left (fun acc v -> Cudd.Bdd.dand acc (var v)) (dtrue ()) vs
;;

let exists = Cudd.Bdd.exist
let and_exists = Cudd.Bdd.existand
let permute f p = Cudd.Bdd.permute f p
let size = Cudd.Bdd.size
let satcount n f = Cudd.Bdd.nbminterms n f
let nodes () = Cudd.Man.get_node_count (get ())
let peak () = Cudd.Man.get_node_count_peak (get ())
