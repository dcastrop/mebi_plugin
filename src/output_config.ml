type t = { mutable enabled : bool }

module type OUTPUT_CONFIG = sig
  val get : t ref
  val reset : unit -> unit
  val enable_output : unit -> unit
  val disable_output : unit -> unit
  val configure_output : Output_kind.t -> bool -> unit

  val do_output
    :  ?__FUNCTION__:string
    -> ?prefix:string option
    -> ?override:bool
    -> Output_kind.t
    -> string
    -> unit

  val show_mode : unit -> unit
  val show_enabled : unit -> unit
  val show_kind : Output_kind.t -> unit
end

module type S = sig
  val default : unit -> t
  val level : Output_kind.level -> bool
  val special : Output_kind.special -> bool
end

type x = t

module Make (Mode : Output_mode.OUTPUT_MODE) (D : S) : OUTPUT_CONFIG = struct
  open Output_kind

  type t = x

  module Level : Output_kind.OUTPUT_KIND with type t = Output_kind.level =
  Output_kind.Make (struct
      type t = Output_kind.level

      let defaults : t -> bool = D.level
    end)

  module Special : Output_kind.OUTPUT_KIND with type t = Output_kind.special =
  Output_kind.Make (struct
      type t = Output_kind.special

      let defaults : t -> bool = D.special
    end)

  let default () : t = D.default ()
  let get : t ref = ref (default ())
  let reset () : unit = get := default ()
  let enable_output () : unit = !get.enabled <- true
  let disable_output () : unit = !get.enabled <- false

  let configure_output (x : Output_kind.t) : bool -> unit =
    match kind x with
    | Level x -> Level.configure x
    | Special x -> Special.configure x
  ;;

  (***********************************************************************)

  let is_level_enabled ?(override : bool = false) : Output_kind.level -> bool =
    fun (x : Output_kind.level) ->
    override || (!get.enabled && Level.is_enabled x)
  ;;

  let is_special_enabled ?(override : bool = false)
    : Output_kind.special -> bool
    =
    fun (x : Output_kind.special) ->
    override || (!get.enabled && Special.is_enabled x)
  ;;

  let do_output
        ?(__FUNCTION__ : string = "")
        ?(prefix : string option = None)
        ?(override : bool = false)
        (x : Output_kind.t)
    : string -> unit
    =
    match kind x with
    | Level x ->
      Mode.do_level_output ~__FUNCTION__ ~prefix (is_level_enabled ~override) x
    | Special x ->
      Mode.do_special_output
        ~__FUNCTION__
        ~prefix
        (is_special_enabled ~override)
        x
  ;;

  (***********************************************************************)

  let f ?(__FUNCTION__ : string = "") : string -> unit =
    Mode.do_level_output ~__FUNCTION__ ~override:true Level.is_enabled Notice
  ;;

  let show_mode () : unit = f ~__FUNCTION__ Mode.output_mode_as_string

  let b ?(__FUNCTION__ : string = "") (x : bool) =
    f ~__FUNCTION__ (Utils.Strfy.bool x)
  ;;

  let show_enabled () : unit = b ~__FUNCTION__ !get.enabled

  let show_kind (x : Output_kind.t) : unit =
    match kind x with
    | Level x -> b ~__FUNCTION__ (Level.is_enabled x)
    | Special x -> b ~__FUNCTION__ (Special.is_enabled x)
  ;;
end
