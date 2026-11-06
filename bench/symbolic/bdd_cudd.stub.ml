(** Stands in for {!Bdd_cudd} where opam's [mlcuddidl] is not installed
    (e.g. CI), so the prototype still builds: every operation fails, saying
    what to install. dune picks this file or [bdd_cudd.real.ml] by
    [select], in [dune]. *)

let missing () =
  failwith "bench/symbolic was built without CUDD: opam install mlcuddidl, then rebuild"
;;

type t = unit

let name = "cudd (not installed)"
let init ~vars:_ = missing ()
let dtrue () = missing ()
let dfalse () = missing ()
let var _ = missing ()
let dnot _ = missing ()
let dand _ _ = missing ()
let dor _ _ = missing ()
let is_false _ = missing ()
let equal _ _ = missing ()
let cube _ = missing ()
let exists _ _ = missing ()
let and_exists _ _ _ = missing ()
let permute _ _ = missing ()
let size _ = missing ()
let satcount _ _ = missing ()
let nodes () = missing ()
let peak () = missing ()
