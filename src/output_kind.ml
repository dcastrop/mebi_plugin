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

let default_level
      ?(debug : bool = false)
      ?(info : bool = false)
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

let default_special
      ?(trace : bool = true)
      ?(result : bool = true)
      ?(show : bool = true)
  : special -> bool
  = function
  | Trace -> trace
  | Result -> result
  | Show -> show
;;

(***********************************************************************)

module type OUTPUT_KIND = sig
  type t

  val defaults : t -> bool
  val config : (t, bool) Hashtbl.t
  val is_enabled : t -> bool
  val configure : t -> bool -> unit
  val reset : t -> unit
end

module type S = sig
  type t

  val defaults : t -> bool
end

module Make (O : S) : OUTPUT_KIND with type t = O.t = struct
  include O

  let config : (t, bool) Hashtbl.t = Hashtbl.create 0

  let is_enabled (x : t) : bool =
    match Hashtbl.find_opt config x with None -> defaults x | Some x -> x
  ;;

  let configure (x : t) (y : bool) : unit = Hashtbl.replace config x y
  let reset (x : t) : unit = Hashtbl.replace config x (O.defaults x)
end
