(* https://github.com/Chris00/ocaml-benchmark/blob/master/src/benchmark.mli *)

module type S = sig
  module Timing : sig
    include Json.S with type k = Benchmark.t
  end

  include Json.S with type k = Benchmark.samples
end

module Make (Log : Logger.S) : S = struct
  open Benchmark

  module Timing = struct
    include
      Json.Thing.Make
        (Log)
        (struct
          type k = Benchmark.t

          let name = "Benchmark Timing Tests"
          let json ?(as_elt : bool = false) (x : k) : Yojson.t = `Null
        end)
  end

  module Json_ : Json.S with type k = Benchmark.samples =
    Json.List.Make
      (Log)
      (struct
        type k = string * t list

        let name = "Benchmark Samples"

        let json ?(as_elt : bool = false) ((x, ys) : k) : Yojson.t =
          `Assoc
            [ "name", `String x
            ; ( "tests"
              , `List
                  (List.fold_left
                     (fun (acc : Yojson.t list) (y : Benchmark.t) ->
                       Timing.json ~as_elt:true y :: acc)
                     []
                     (List.rev ys)) )
            ]
        ;;
      end)

  include Json_
end
