(***********************************************************************)

type level = Feedback.level

let make_level_fun
      ?(debug : bool = false)
      ?(info : bool = false)
      ?(notice : bool = true)
      ?(warning : bool = true)
      ?(error : bool = true)
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

let level_fun_preset_debug ?(trace : bool = false) () =
  make_level_fun ~debug:true ~info:trace ()
;;

let level_fun_preset_results () = make_level_fun ~info:true ()

(***********************************************************************)

(** [Output] handles the different ways we may need to output messages from the plugin. Primarily through [Rocq], but this also supports the [OCaml]'s standard output. See [Output.Make] for more details.
*)
module Output = struct
  module type OUTPUT_TYPE = sig
    val output_type_as_string : string

    val do_feedback_output
      :  ?__FUNCTION__:string
      -> ?prefix:string option
      -> ?override:bool
      -> (level -> bool)
      -> level
      -> string
      -> unit
  end

  module type S = sig
    val output_type_as_string : string

    val get_output_fun
      :  ?__FUNCTION__:string
      -> ?prefix:string option
      -> level
      -> string
      -> unit
  end

  module Make (X : S) : OUTPUT_TYPE = struct
    include X

    let do_feedback_output
          ?(__FUNCTION__ : string = "")
          ?(prefix : string option = None)
          ?(override : bool = false)
          (f : level -> bool)
          (x : level)
          (y : string)
      : unit
      =
      if override || f x then get_output_fun ~__FUNCTION__ ~prefix x y
    ;;
  end

  module Rocq : OUTPUT_TYPE = Make (struct
      let output_type_as_string : string = "Rocq"

      let get_output_fun
            ?(__FUNCTION__ : string = "")
            ?(prefix : string option = None)
        : level -> string -> unit
        =
        let open Pp in
        let f (g : ?loc:Loc.t -> Pp.t -> unit) : string -> unit =
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
          (* g (seq [ v 0 (seq [ a; b ]); c ] |> hv 0);
             g (seq [ hv 0 (seq [ a; b ]); c ] |> v 0);
             g (seq [ v 0 (seq [ a; ws 5; b ]); ws 5; str y |> v 0 ] |> v 0);
             g (seq [ v 0 (seq [ a; ws 5; b ]); ws 5; str y |> v 0 ] |> h);
             g (seq [ a; ws 5; b; ws 5; str y |> v 0 ] |> h);
             g (seq [ mt (); a; ws 5; b; ws 5; str y |> v 0; fnl () ] |> v 0);
             g
             (seq [ mt (); a; ws 5; b; seq [ ws 5; str y |> v 0 ]; fnl () ]
             |> v 0) *)
          g (seq [ v 0 (seq [ a; b ]); ws 0; c ] |> hv 0)
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

      let get_output_fun
            ?(__FUNCTION__ : string = "")
            ?(prefix : string option = None)
        : level -> string -> unit
        =
        let f (x : string) : string -> unit =
          Printf.printf
            "%s [%s] %s\n"
            (match __FUNCTION__ with "" -> "" | z -> Printf.sprintf "%s: " z)
            (Option.cata (fun y -> Printf.sprintf "%s: %s" y x) x prefix)
        in
        function
        | Debug -> f "Debug"
        | Info -> f "Info"
        | Notice -> f "Notice"
        | Warning -> f "Warning"
        | Error -> f "Error"
      ;;
    end)
end

(***********************************************************************)

type 'a to_string =
  | Args of (?args:Utils.Strfy.style_args -> 'a -> string)
  | Of of ('a -> string)

module type LOGGER_TYPE = sig
  val enabled : bool ref
  val prefix : string option

  (* Rocq's [Feedback.level] messages *)
  val debug : ?__FUNCTION__:string -> string -> unit
  val info : ?__FUNCTION__:string -> string -> unit
  val notice : ?__FUNCTION__:string -> string -> unit
  val warning : ?__FUNCTION__:string -> string -> unit
  val error : ?__FUNCTION__:string -> string -> unit

  (* custom printing messages *)
  val trace : ?__FUNCTION__:string -> string -> unit
  val result : ?__FUNCTION__:string -> string -> unit
  val override : ?__FUNCTION__:string -> level -> string -> unit

  (* utils for printing things *)
  val thing
    :  ?__FUNCTION__:string
    -> ?args:Utils.Strfy.style_args
    -> level
    -> string
    -> 'a
    -> 'a to_string
    -> unit

  val option
    :  ?__FUNCTION__:string
    -> ?args:Utils.Strfy.style_args
    -> level
    -> string
    -> 'a option
    -> 'a to_string
    -> unit

  (* user defined *)
  val is_level_enabled : level -> bool
end

module type S = sig
  val prefix : string option
  val is_level_enabled : level -> bool
end

module Make (O : Output.OUTPUT_TYPE) (X : S) : LOGGER_TYPE = struct
  let enabled : bool ref = ref true
  let prefix : string option = X.prefix

  let is_level_enabled : level -> bool =
    if !enabled then X.is_level_enabled else fun _ -> false
  ;;

  let do_feedback_output
        ?(override : bool = false)
        ?(prefix : string option = None)
        ?(__FUNCTION__ : string = "")
    : level -> string -> unit
    =
    O.do_feedback_output ~prefix ~override ~__FUNCTION__ is_level_enabled
  ;;

  let debug ?(__FUNCTION__ : string = "") (x : string) : unit =
    do_feedback_output ~__FUNCTION__ Debug x
  ;;

  let info ?(__FUNCTION__ : string = "") (x : string) : unit =
    do_feedback_output ~__FUNCTION__ Info x
  ;;

  let notice ?(__FUNCTION__ : string = "") (x : string) : unit =
    do_feedback_output ~__FUNCTION__ Notice x
  ;;

  let warning ?(__FUNCTION__ : string = "") (x : string) : unit =
    do_feedback_output ~__FUNCTION__ Warning x
  ;;

  let error ?(__FUNCTION__ : string = "") (x : string) : unit =
    do_feedback_output ~__FUNCTION__ Error x
  ;;

  (** [trace x] is not supported by Rocq's [Feedback.level], and so we choose to show the trace only if both [Debug] and [Info] are enabled.
  *)
  let trace ?(__FUNCTION__ : string = "") (x : string) : unit =
    if is_level_enabled Info then debug ~__FUNCTION__ x
  ;;

  (** [result x] is not supported by Rocq's [Feedback.level], and so we choose to show the trace only if both [Info] and [Notice] are enabled.
  *)
  let result ?(__FUNCTION__ : string = "") (x : string) : unit =
    if is_level_enabled Info then notice ~__FUNCTION__ x
  ;;

  (** [override level x] will always print as [level] *)
  let override ?(__FUNCTION__ : string = "") : level -> string -> unit =
    do_feedback_output ~__FUNCTION__ ~override:true
  ;;

  (** [thing level f x] uses outputs the result of [f x] to [level]. *)
  let thing
        ?(__FUNCTION__ : string = "")
        ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
        (level : level)
        (prefix : string)
        (x : 'a)
        (f : 'a to_string)
    : unit
    =
    let f : 'a -> string = match f with Args f -> f ~args | Of f -> f in
    do_feedback_output
      ~prefix:(Some (Printf.sprintf "%s: " prefix))
      ~__FUNCTION__
      level
      (f x)
  ;;

  let option
        ?(__FUNCTION__ : string = "")
        ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
        (level : level)
        (prefix : string)
        (x : 'a option)
        (f : 'a to_string)
    : unit
    =
    match x with
    | Some x -> thing ~__FUNCTION__ ~args level prefix x f
    | None ->
      thing ~__FUNCTION__ ~args level prefix "None" (Of Utils.Strfy.string)
  ;;
end

let make
      ?(prefix : string option = None)
      (is_level_enabled : level -> bool)
      (module O : Output.OUTPUT_TYPE)
  : (module LOGGER_TYPE)
  =
  let module X : LOGGER_TYPE =
    Make
      (O)
      (struct
        let prefix : string option = prefix
        let is_level_enabled : level -> bool = is_level_enabled
      end)
  in
  (module X)
;;

let debug
      ?(prefix : string option = None)
      ?(trace : bool = false)
      (module O : Output.OUTPUT_TYPE)
  : (module LOGGER_TYPE)
  =
  make ~prefix (level_fun_preset_debug ~trace ()) (module O)
;;

let results ?(prefix : string option = None) (module O : Output.OUTPUT_TYPE)
  : (module LOGGER_TYPE)
  =
  make ~prefix (level_fun_preset_results ()) (module O)
;;
