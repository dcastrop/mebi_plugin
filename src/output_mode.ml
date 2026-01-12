open Output_kind

(** handles the different ways we may need to output messages from the plugin. Primarily through [Rocq], but this also supports the [OCaml]'s standard output. See [Make] for more details.
*)
module type OUTPUT_MODE = sig
  val output_mode_as_string : string

  val do_level_output
    :  ?__FUNCTION__:string
    -> ?prefix:string option
    -> ?override:bool
    -> (level -> bool)
    -> level
    -> string
    -> unit

  val do_special_output
    :  ?__FUNCTION__:string
    -> ?prefix:string option
    -> ?override:bool
    -> (special -> bool)
    -> special
    -> string
    -> unit
end

module type S = sig
  val output_mode_as_string : string

  val level_output_fun
    :  ?__FUNCTION__:string
    -> ?prefix:string option
    -> level
    -> string
    -> unit

  val special_output_fun
    :  ?__FUNCTION__:string
    -> ?prefix:string option
    -> special
    -> string
    -> unit
end

module Make (X : S) : OUTPUT_MODE = struct
  include X

  let do_level_output
        ?(__FUNCTION__ : string = "")
        ?(prefix : string option = None)
        ?(override : bool = false)
        (f : level -> bool)
        (x : level)
        (y : string)
    : unit
    =
    if override || f x then level_output_fun ~__FUNCTION__ ~prefix x y
  ;;

  let do_special_output
        ?(__FUNCTION__ : string = "")
        ?(prefix : string option = None)
        ?(override : bool = false)
        (f : special -> bool)
        (x : special)
        (y : string)
    : unit
    =
    if override || f x then special_output_fun ~__FUNCTION__ ~prefix x y
  ;;
end

module Rocq : OUTPUT_MODE = Make (struct
    let output_mode_as_string : string = "Rocq"

    let level_output_fun
          ?(__FUNCTION__ : string = "")
          ?(prefix : string option = None)
      : level -> string -> unit
      =
      let open Pp in
      let f ?(__FUNCTION__ : string = "") (g : ?loc:Loc.t -> Pp.t -> unit)
        : string -> unit
        =
        fun (y : string) ->
        let a : Pp.t =
          (match __FUNCTION__ with "" -> mt () | z -> str z ++ str ": ")
          |> v 0
        in
        let b : Pp.t =
          Option.cata (fun (prefix : string) -> str prefix) (mt ()) prefix
          |> v 0
        in
        let c : Pp.t = str y |> v 0 in
        g (seq [ v 0 (seq [ a; b ]); ws 0; c ] |> hv 0)
      in
      function
      | Debug -> f ~__FUNCTION__ Feedback.msg_debug
      | Info -> f Feedback.msg_info
      | Notice -> f Feedback.msg_notice
      | Warning -> f ~__FUNCTION__ (Feedback.msg_warning ~quickfix:[])
      | Error -> f ~__FUNCTION__ Feedback.msg_debug
    ;;

    let special_output_fun
          ?(__FUNCTION__ : string = "")
          ?(prefix : string option = None)
      : special -> string -> unit
      = function
      | Trace -> level_output_fun ~__FUNCTION__ ~prefix Debug
      | Result -> level_output_fun ~__FUNCTION__ ~prefix Info
      | Show -> level_output_fun ~__FUNCTION__ ~prefix Notice
    ;;
  end)

module OCaml : OUTPUT_MODE = Make (struct
    let output_mode_as_string : string = "OCaml"

    let f
          ?(__FUNCTION__ : string = "")
          ?(prefix : string option = None)
          (x : string)
      : string -> unit
      =
      Printf.printf
        "%s [%s] %s\n"
        (match __FUNCTION__ with "" -> "" | z -> Printf.sprintf "%s: " z)
        (Option.cata (fun y -> Printf.sprintf "%s: %s" y x) x prefix)
    ;;

    let level_output_fun
          ?(__FUNCTION__ : string = "")
          ?(prefix : string option = None)
      : level -> string -> unit
      =
      let f : string -> string -> unit = f ~__FUNCTION__ ~prefix in
      function
      | Debug -> f "Debug"
      | Info -> f "Info"
      | Notice -> f "Notice"
      | Warning -> f "Warning"
      | Error -> f "Error"
    ;;

    let special_output_fun
          ?(__FUNCTION__ : string = "")
          ?(prefix : string option = None)
      : special -> string -> unit
      =
      let f : string -> string -> unit = f ~__FUNCTION__ ~prefix in
      function Trace -> f "Trace" | Result -> f "Result" | Show -> f "Show"
    ;;
  end)

(***********************************************************************)

module Default = Rocq
