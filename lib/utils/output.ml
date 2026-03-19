let debug : bool = true
let trace : bool = true

module Kind = struct
  type t =
    | Debug
    | Info
    | Notice
    | Warning
    | Error
    | Trace
    | Result
    | Show

  (** the kind of message to be output. [Level] are Rocq standard, [Special] are extended by this plugin.
  *)
  type k =
    | Level of level
    | Special of special

  and level = Feedback.level

  and special =
    | Trace
    | Result
    | Show

  let kind : t -> k = function
    | Debug -> Level Debug
    | Info -> Level Info
    | Notice -> Level Notice
    | Warning -> Level Warning
    | Error -> Level Error
    | Trace -> Special Trace
    | Result -> Special Result
    | Show -> Special Show
  ;;

  (***********************************************************************)

  let default_level_fun
        ?(debug : bool = debug)
        ?(info : bool = true)
        ?(notice : bool = true)
        ?(warning : bool = true)
        ?(error : bool = true)
    : level -> bool
    = function
    | Debug -> debug
    | Info -> info
    | Notice -> notice
    | Warning -> warning
    | Error -> error
  ;;

  let default_level
    : (?debug:bool
       -> ?info:bool
       -> ?notice:bool
       -> ?warning:bool
       -> ?error:bool
       -> level
       -> bool)
        ref
    =
    ref default_level_fun
  ;;

  let default_special_fun
        ?(trace : bool = trace)
        ?(result : bool = true)
        ?(show : bool = true)
    : special -> bool
    = function
    | Trace -> trace
    | Result -> result
    | Show -> show
  ;;

  let default_special
    : (?trace:bool -> ?result:bool -> ?show:bool -> special -> bool) ref
    =
    ref default_special_fun
  ;;

  (***********************************************************************)

  module type S = sig
    type t

    val defaults : t -> bool
    val config : (t, bool) Hashtbl.t
    val is_enabled : t -> bool
    val configure : t -> bool -> unit
    val reset : t -> unit
  end

  module Make (O : sig
      type t

      val defaults : t -> bool
    end) : S with type t = O.t = struct
    include O

    let config : (t, bool) Hashtbl.t = Hashtbl.create 0

    let is_enabled (x : t) : bool =
      match Hashtbl.find_opt config x with None -> defaults x | Some x -> x
    ;;

    let configure (x : t) (y : bool) : unit = Hashtbl.replace config x y
    let reset (x : t) : unit = Hashtbl.replace config x (O.defaults x)
  end
end

module Mode = struct
  (** handles the different ways we may need to output messages from the plugin. Primarily through [Rocq], but this also supports the [OCaml]'s standard output. See [Make] for more details.
  *)
  module type S = sig
    val output_mode_as_string : string

    val do_level_output
      :  ?__FUNCTION__:string
      -> ?prefix:string option
      -> ?override:bool
      -> (Kind.level -> bool)
      -> Kind.level
      -> string
      -> unit

    val do_special_output
      :  ?__FUNCTION__:string
      -> ?prefix:string option
      -> ?override:bool
      -> (Kind.special -> bool)
      -> Kind.special
      -> string
      -> unit
  end

  module Make (X : sig
      val output_mode_as_string : string

      val level_output_fun
        :  ?__FUNCTION__:string
        -> ?prefix:string option
        -> Kind.level
        -> string
        -> unit

      val special_output_fun
        :  ?__FUNCTION__:string
        -> ?prefix:string option
        -> Kind.special
        -> string
        -> unit
    end) : S = struct
    include X

    let do_level_output
          ?(__FUNCTION__ : string = "")
          ?(prefix : string option = None)
          ?(override : bool = false)
          (f : Kind.level -> bool)
          (x : Kind.level)
          (y : string)
      : unit
      =
      if override || f x then level_output_fun ~__FUNCTION__ ~prefix x y
    ;;

    let do_special_output
          ?(__FUNCTION__ : string = "")
          ?(prefix : string option = None)
          ?(override : bool = false)
          (f : Kind.special -> bool)
          (x : Kind.special)
          (y : string)
      : unit
      =
      if override || f x then special_output_fun ~__FUNCTION__ ~prefix x y
    ;;
  end

  module Rocq : S = Make (struct
      let output_mode_as_string : string = "Rocq"

      let level_output_fun
            ?(__FUNCTION__ : string = "")
            ?(prefix : string option = None)
        : Kind.level -> string -> unit
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
        : Kind.special -> string -> unit
        = function
        | Trace -> level_output_fun ~__FUNCTION__ ~prefix Debug
        | Result -> level_output_fun ~__FUNCTION__ ~prefix Info
        | Show -> level_output_fun ~__FUNCTION__ ~prefix Notice
      ;;
    end)

  module OCaml : S = Make (struct
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
        : Kind.level -> string -> unit
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
        : Kind.special -> string -> unit
        =
        let f : string -> string -> unit = f ~__FUNCTION__ ~prefix in
        function Trace -> f "Trace" | Result -> f "Result" | Show -> f "Show"
      ;;
    end)

  (***********************************************************************)

  module Default = Rocq
end

module Config = struct
  type t =
    { mutable enabled : bool
    ; level_defaults :
        (?debug:bool
         -> ?info:bool
         -> ?notice:bool
         -> ?warning:bool
         -> ?error:bool
         -> Kind.level
         -> bool)
          ref
    ; special_defaults :
        (?trace:bool -> ?result:bool -> ?show:bool -> Kind.special -> bool) ref
    }

  let default () : t =
    { enabled = true
    ; level_defaults = Kind.default_level
    ; special_defaults = Kind.default_special
    }
  ;;

  module type S = sig
    module Mode : Mode.S

    val get : t ref
    val reset : unit -> unit
    val enable_output : unit -> unit
    val disable_output : unit -> unit
    val configure_output : Kind.t -> bool -> unit
    val is_enabled : Kind.t -> bool

    val do_output
      :  ?__FUNCTION__:string
      -> ?prefix:string option
      -> ?override:bool
      -> Kind.t
      -> string
      -> unit

    val show_mode : unit -> unit
    val show_enabled : unit -> unit
    val show_kind : Kind.t -> unit
  end

  type x = t

  module Make
      (M : Mode.S)
      (K : sig
         val level : Kind.level -> bool
         val special : Kind.special -> bool
       end) : S with module Mode = M = struct
    module Mode = M

    type t = x

    module Level : Kind.S with type t = Kind.level = Kind.Make (struct
        type t = Kind.level

        let defaults : t -> bool = K.level
      end)

    module Special : Kind.S with type t = Kind.special = Kind.Make (struct
        type t = Kind.special

        let defaults : t -> bool = K.special
      end)

    let get : t ref = ref (default ())
    let reset () : unit = get := default ()
    let enable_output () : unit = !get.enabled <- true
    let disable_output () : unit = !get.enabled <- false

    let configure_output (x : Kind.t) : bool -> unit =
      match Kind.kind x with
      | Level x -> Level.configure x
      | Special x -> Special.configure x
    ;;

    let is_enabled (x : Kind.t) : bool =
      match Kind.kind x with
      | Level x -> Level.is_enabled x
      | Special x -> Special.is_enabled x
    ;;

    (***********************************************************************)

    let is_level_enabled ?(override : bool = false) : Kind.level -> bool =
      fun (x : Kind.level) ->
      override
      || (!get.enabled && !((default ()).level_defaults) x && Level.is_enabled x)
    ;;

    let is_special_enabled ?(override : bool = false) : Kind.special -> bool =
      fun (x : Kind.special) ->
      override
      || (!get.enabled
          && !((default ()).special_defaults) x
          && Special.is_enabled x)
    ;;

    let do_output
          ?(__FUNCTION__ : string = "")
          ?(prefix : string option = None)
          ?(override : bool = false)
          (x : Kind.t)
      : string -> unit
      = function
      | "" -> ()
      | y ->
        (match Kind.kind x with
         | Level x ->
           Mode.do_level_output
             ~__FUNCTION__
             ~prefix
             (is_level_enabled ~override)
             x
             y
         | Special x ->
           Mode.do_special_output
             ~__FUNCTION__
             ~prefix
             (is_special_enabled ~override)
             x
             y)
    ;;

    (***********************************************************************)

    let f ?(__FUNCTION__ : string = "") : string -> unit =
      Mode.do_level_output ~__FUNCTION__ ~override:true Level.is_enabled Notice
    ;;

    let show_mode () : unit = f ~__FUNCTION__ Mode.output_mode_as_string

    let b ?(__FUNCTION__ : string = "") (x : bool) =
      f ~__FUNCTION__ (Printf.sprintf "%b" x)
    ;;

    let show_enabled () : unit = b ~__FUNCTION__ !get.enabled
    let show_kind (x : Kind.t) : unit = b ~__FUNCTION__ (is_enabled x)
  end
end
