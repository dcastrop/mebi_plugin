type output_mode =
  | Coq
  | OCaml

type level = Feedback.level

let make_level_fun
      ?(debug : bool = false)
      ?(info : bool = false)
      ?(notice : bool = false)
      ?(warning : bool = false)
      ?(error : bool = false)
      ()
  : level -> bool
  =
  fun (x : level) ->
  match x with
  | Debug -> debug
  | Info -> info
  | Notice -> notice
  | Warning -> warning
  | Error -> error
;;

module Output = struct
  module type OUTPUT_TYPE = sig
    val output_type_as_string : string

    val do_feedback_output
      :  ?override:bool
      -> (level -> bool)
      -> level
      -> string
      -> unit
  end

  module type S = sig
    val output_type_as_string : string
    val get_output_fun : level -> string -> unit
  end

  module Make (X : S) : OUTPUT_TYPE = struct
    include X

    let do_feedback_output
          ?(override : bool = false)
          (f : level -> bool)
          (x : level)
          (y : string)
      : unit
      =
      if override || f x then get_output_fun x y
    ;;
  end

  module Rocq : OUTPUT_TYPE = Make (struct
      let output_type_as_string : string = "Rocq"

      let get_output_fun : level -> string -> unit =
        let f (x : ?loc:Loc.t -> Pp.t -> unit) : string -> unit =
          fun (y : string) -> Pp.str y |> x
        in
        function
        | Debug -> f Feedback.msg_debug
        | Info -> f Feedback.msg_info
        | Notice -> f Feedback.msg_notice
        | Warning -> f Feedback.msg_warning
        | Error -> f Feedback.msg_warning
      ;;
    end)

  module OCaml : OUTPUT_TYPE = Make (struct
      let output_type_as_string : string = "OCaml"

      let get_output_fun : level -> string -> unit =
        let f (x : string) : string -> unit = Printf.printf "[%s] %s\n" x in
        function
        | Debug -> f "Debug"
        | Info -> f "Info"
        | Notice -> f "Notice"
        | Warning -> f "Warning"
        | Error -> f "Error"
      ;;
    end)
end

module type LOGGER_TYPE = sig
  val enabled : bool ref

  (* Rocq's [Feedback.level] messages *)
  val debug : string -> unit
  val info : string -> unit
  val notice : string -> unit
  val warning : string -> unit
  val error : string -> unit

  (* custom printing messages *)
  val trace : string -> unit
  val result : string -> unit
  val override : level -> string -> unit

  (* user defined *)
  val is_level_enabled : level -> bool
end

module type S = sig
  val is_level_enabled : level -> bool
end

module Make (O : Output.OUTPUT_TYPE) (X : S) : LOGGER_TYPE = struct
  let enabled : bool ref = ref true

  let is_level_enabled : level -> bool =
    if !enabled then X.is_level_enabled else fun _ -> false
  ;;

  let do_feedback_output ?(override : bool = false) : level -> string -> unit =
    O.do_feedback_output is_level_enabled
  ;;

  let debug (x : string) : unit = do_feedback_output Debug x
  let info (x : string) : unit = do_feedback_output Info x
  let notice (x : string) : unit = do_feedback_output Notice x
  let warning (x : string) : unit = do_feedback_output Warning x
  let error (x : string) : unit = do_feedback_output Error x

  (** [trace x] is not supported by Rocq's [Feedback.level], and so we choose to show the trace only if both [Debug] and [Info] are enabled.
  *)
  let trace (x : string) : unit = if is_level_enabled Info then debug x

  (** [result x] is not supported by Rocq's [Feedback.level], and so we choose to show the trace only if both [Info] and [Notice] are enabled.
  *)
  let result (x : string) : unit = if is_level_enabled Info then notice x

  (** [override level x] will always print as [level] *)
  let override : level -> string -> unit = do_feedback_output ~override:true
end

module Test : LOGGER_TYPE =
  Make
    (Output.Rocq)
    (struct
      let is_level_enabled : level -> bool = make_level_fun ()
    end)
