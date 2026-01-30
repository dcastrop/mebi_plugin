let default_encoding () : (module Encoding.SEncoding) =
  (module Encoding.Int : Encoding.SEncoding)
;;

let default_context () : (module Rocq_context.SRocq_context) =
  (module Rocq_context.Default : Rocq_context.SRocq_context)
;;

(** [fresh_checker ...] makes a fresh [module Wrapper] to be used to run basic commands.
*)
let fresh_basic () =
  (* let module W =
     Wrapper.Make ((val default_context ())) ((val default_encoding ()))
     in *)
  ()
;;
