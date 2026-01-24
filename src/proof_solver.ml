(***********************************************************************)
module Log : Logger.LOGGER_TYPE = Logger.MkDefault ()

let () = Log.Config.enable_output ()
let () = Log.Config.configure_output Info true
let () = Log.Config.configure_output Debug true
let () = Log.Config.configure_output Trace true
(***********************************************************************)

module State = struct
  type t =
    | Main of main
    | Special of special
    | Util of util

  and main =
    | NewProof
    | WeakSim
    | NewCofix
    | GoalHyp
    | ApplyConstructors

  and special =
    | DoRefl
    | Solve of bool option

  and util =
    | DoUnfold of t
    | DoInversion of t

  let rec to_string : t -> string = function
    | Main NewProof -> "NewProof"
    | Main WeakSim -> "WeakSim"
    | Main NewCofix -> "NewCofix"
    | Main GoalHyp -> "GoalHyp"
    | Main ApplyConstructors -> "Main ApplyConstructors"
    | Special DoRefl -> "Special DoRefl"
    | Special (Solve x) ->
      Printf.sprintf
        "Special Solve (%s)"
        (Option.cata (Printf.sprintf "Some %b") "None" x)
    | Util (DoUnfold x) -> Printf.sprintf "Special DoUnfold (%s)" (to_string x)
    | Util (DoInversion x) ->
      Printf.sprintf "Special DoInversion (%s)" (to_string x)
  ;;

  (*
     exception NextState_SpecialStateSolve_None of unit

     let next : t -> t = function
     | Main NewProof -> Util (DoUnfold (Main WeakSim))
     | Main WeakSim ->  Util (DoUnfold (Main NewCofix))
     | Main NewCofix ->  Util (DoUnfold (Main WeakSim))
     | Main GoalHyp -> ()
     | Main ApplyConstructors -> ()
     | Special DoRefl -> ()
     | Special Solve None -> raise (NextState_SpecialStateSolve_None ())
     | Special Solve (Some true) -> Special (Solve None)
     | Special Solve (Some false) -> Util (DoUnfold (Main NewCofix))
     | Util DoUnfold x -> ()
     | Util DoInversion x -> () *)
end

(* module type CONSTR_SET_TYPE = Set.S with type t = Constr.t  *)
(* module type ECONSTR_SET_TYPE = Set.S with type t = EConstr.t  *)

module type SOLVER_TYPE = sig
  val gl : unit -> Proofview.Goal.t ref
  val state : State.t

  module ConstrSet : Set.S with type elt = Constr.t
end

module type S = sig
  val gl : Proofview.Goal.t
  val state : State.t
end

module Make (X : S) : SOLVER_TYPE = struct
  let gl () : Proofview.Goal.t ref = ref X.gl
  let state : State.t = X.state

  module ConstrSet : Set.S with type elt = Constr.t = Set.Make (struct
      type t = Constr.t

      let compare (a : t) (b : t) : int = Constr.compare a b
    end)
  (* module EConstrSet : Set.S with type elt = EConstr.t = Set.Make (struct
     type t = EConstr.t
     let compare (a:t) (b:t) : int = EConstr.c

     end) *)
end

(***********************************************************************)

let fresh () =
  Log.trace __FUNCTION__;
  Proofview.Goal.enter (fun (gl : Proofview.Goal.t) ->
    let module T : SOLVER_TYPE =
      Make (struct
        let gl = gl
        let state : State.t = Main NewProof
      end)
    in
    (* T. *)
    Proofview.tclUNIT ())
;;
