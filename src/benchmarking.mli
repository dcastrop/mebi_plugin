
module type S = sig
  module Timing : sig
    include Json.S with type k = Benchmark.t
  end

  include Json.S with type k = Benchmark.samples
end

module Make (Log : Logger.S) : S 