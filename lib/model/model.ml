module type S = Model_intf.S

module Make (Base : Base_term.S) (ConstructorBindings : Json.S) = struct
  module C = Components.Make (Base) (ConstructorBindings)
  include C

  (* Each algorithm is a functor over the components and the models it
     needs; the order is their dependency order. *)
  module LTS = LTS.Make (C)
  module Saturation = Saturation.Make (Base) (C)
  module FSM = FSM.Make (C) (LTS) (Saturation)
  module Minimization = Minimization.Make (C) (FSM)
  module Bisimilarity = Bisimilarity.Make (C) (FSM) (Minimization)
  module Product = Product.Make (Base) (C) (FSM) (Saturation)
  module SaturationEstimate = Saturation_estimate.Make (C) (FSM)
end
