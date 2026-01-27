module Wrapper = struct
  module type TYPE = sig
    module Monad : Monad.TYPE
    module Term : Term.TYPE
    module Model : Model.TYPE
  end

  module Make (Ctx : Context.TYPE) (Enc : Encoding.TYPE) : TYPE = struct
    module Monad = Monad.Make (Ctx) (Enc)

    (* module Monad = Monad.Make (Ctx) (Term) *)
    module Model = Model.Make (Term.MakeFromEnc (Enc))
    module Term = Model.T
    include Model

    (* TODO: *)
    (* let decode_term (x : Term.t) : EConstr.t = Monad.decode x *)
    (* let decode_label (x : Label.t) : EConstr.t = decode_term x.term *)
  end
end

module MEBI (Ctx : Context.TYPE) : Wrapper.TYPE =
  Wrapper.Make (Ctx) (Encoding.Int)

(***********************************************************************)

module type SProofSolver = sig end

module ProofSolver : SProofSolver = struct
  module type TYPE = sig
    val gl : unit -> Proofview.Goal.t ref

    module Ctx : Context.TYPE

    val update : Proofview.Goal.t ref -> unit
  end

  module type S = sig
    val gl : unit -> Proofview.Goal.t ref
  end

  module Make (X : S) : TYPE = struct
    let gl () : Proofview.Goal.t ref = X.gl ()

    module Ctx : Context.TYPE = Context.Make (struct
        let env () : Environ.env ref = ref (Proofview.Goal.env !(gl ()))
        let sigma () : Evd.evar_map ref = ref (Proofview.Goal.sigma !(gl ()))
      end)

    let update (gl : Proofview.Goal.t ref) : unit =
      Ctx.update (ref (Proofview.Goal.env !gl)) (ref (Proofview.Goal.sigma !gl))
    ;;
  end
end

let p () : (module SProofSolver) option ref = ref None

exception NoP of unit

let get_p () : (module SProofSolver) ref =
  match !(p ()) with None -> raise (NoP ()) | Some p -> ref p
;;

(* TODO:
   - in [loader.v] add command to initialise the plugin, creating references to the constructed modules.
   - thereafter, each command will "get" the module, which if [None] will throw a "reload the plugin" error *)

(***********************************************************************)

(* module _ = struct

   module type TYPE = sig

   end

   module type S = sig

   end

   module Make (X:S) : TYPE = struct

   end

   end *)

(***********************************************************************)
