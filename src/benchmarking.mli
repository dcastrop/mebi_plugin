(** JSON printers for [MeBi Benchmark]'s results: a timing, and a set of
    timed samples (from the [benchmark] library). *)
module type S = sig
  (** One timing (user and system time) as JSON. *)
  module Timing : sig
    include Json.S with type k = Benchmark.t
  end

  include Json.S with type k = Benchmark.samples
end

module Make : S
