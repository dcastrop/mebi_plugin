module type OUTPUT_MODE = sig
  val output_mode_as_string : string

  val do_level_output
    :  ?__FUNCTION__:string
    -> ?prefix:string option
    -> ?override:bool
    -> (Output_kind.level -> bool)
    -> Output_kind.level
    -> string
    -> unit

  val do_special_output
    :  ?__FUNCTION__:string
    -> ?prefix:string option
    -> ?override:bool
    -> (Output_kind.special -> bool)
    -> Output_kind.special
    -> string
    -> unit
end

module type S = sig
  val output_mode_as_string : string

  val level_output_fun
    :  ?__FUNCTION__:string
    -> ?prefix:string option
    -> Output_kind.level
    -> string
    -> unit

  val special_output_fun
    :  ?__FUNCTION__:string
    -> ?prefix:string option
    -> Output_kind.special
    -> string
    -> unit
end

module Make : (_ : S) -> OUTPUT_MODE
module Rocq : OUTPUT_MODE
module OCaml : OUTPUT_MODE
module Default = Rocq
