module type S = sig
  module Config : Output.Config.S

  val enabled : bool ref
  val prefix : string option

  (* Rocq's [Feedback.level] messages *)
  val debug : ?__FUNCTION__:string -> string -> unit
  val info : ?__FUNCTION__:string -> string -> unit
  val notice : ?__FUNCTION__:string -> string -> unit
  val warning : ?__FUNCTION__:string -> string -> unit
  val error : ?__FUNCTION__:string -> string -> unit

  (* special printing messages *)
  val trace : ?__FUNCTION__:string -> string -> unit
  val result : ?__FUNCTION__:string -> string -> unit
  val show : ?__FUNCTION__:string -> string -> unit

  (* utils for printing things *)
  val thing
    :  ?__FUNCTION__:string
    -> ?args:Utils.Strfy.style_args
    -> Output.Kind.t
    -> string
    -> 'a
    -> 'a Utils.Strfy.to_string
    -> unit

  val things
    :  ?__FUNCTION__:string
    -> ?args:Utils.Strfy.style_args
    -> Output.Kind.t
    -> string
    -> 'a list
    -> 'a Utils.Strfy.to_string
    -> unit

  val option
    :  ?__FUNCTION__:string
    -> ?args:Utils.Strfy.style_args
    -> Output.Kind.t
    -> string
    -> 'a option
    -> 'a Utils.Strfy.to_string
    -> unit

  val options
    :  ?__FUNCTION__:string
    -> ?args:Utils.Strfy.style_args
    -> Output.Kind.t
    -> string
    -> 'a list option
    -> 'a Utils.Strfy.to_string
    -> unit
end

module Make
    (Mode : Output.Mode.S)
    (X : sig
       val prefix : string option
       val level : Feedback.level -> bool
       val special : Output.Kind.special -> bool
     end) : S = struct
  module Config : Output.Config.S =
    Output.Config.Make
      (Mode)
      (struct
        let level : Output.Kind.level -> bool = X.level
        let special : Output.Kind.special -> bool = X.special
      end)

  let enabled : bool ref = ref true
  let prefix : string option = X.prefix

  let do_output
        ?(__FUNCTION__ : string = "")
        ?(prefix : string option = None)
        ?(override : bool = false)
    : Output.Kind.t -> string -> unit
    =
    Config.do_output ~__FUNCTION__ ~prefix ~override
  ;;

  let debug ?(__FUNCTION__ : string = "") (x : string) : unit =
    do_output ~__FUNCTION__ Debug x
  ;;

  let info ?(__FUNCTION__ : string = "") (x : string) : unit =
    do_output ~__FUNCTION__ Info x
  ;;

  let notice ?(__FUNCTION__ : string = "") (x : string) : unit =
    do_output ~__FUNCTION__ Notice x
  ;;

  let warning ?(__FUNCTION__ : string = "") (x : string) : unit =
    do_output ~__FUNCTION__ Warning x
  ;;

  let error ?(__FUNCTION__ : string = "") (x : string) : unit =
    do_output ~__FUNCTION__ Error x
  ;;

  let trace ?(__FUNCTION__ : string = "") (x : string) : unit =
    do_output ~__FUNCTION__ Trace x
  ;;

  let result ?(__FUNCTION__ : string = "") (x : string) : unit =
    do_output ~__FUNCTION__ Result x
  ;;

  let show ?(__FUNCTION__ : string = "") (x : string) : unit =
    do_output ~__FUNCTION__ Show x
  ;;

  (** [thing level f x] uses outputs the result of [f x] to [level]. *)
  let thing
        ?(__FUNCTION__ : string = "")
        ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
        (k : Output.Kind.t)
        (prefix : string)
        (x : 'a)
        (f : 'a Utils.Strfy.to_string)
    : unit
    =
    do_output
      ~prefix:(Some (Printf.sprintf "%s: " prefix))
      ~__FUNCTION__
      k
      (Utils.Strfy.f_to_string f x)
  ;;

  let things
        ?(__FUNCTION__ : string = "")
        ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
        (k : Output.Kind.t)
        (prefix : string)
        (xs : 'a list)
        (f : 'a Utils.Strfy.to_string)
    : unit
    =
    (* NOTE: start and end *)
    let e : string -> unit =
      do_output ~prefix:(Some (Printf.sprintf "%s: " prefix)) ~__FUNCTION__ k
    in
    (* NOTE: indexed iterator *)
    let index : int ref = ref 0 in
    let fx (x : 'a) : unit =
      thing ~args k (Printf.sprintf "%i" !index) x f;
      index := !index + 1
    in
    e "start";
    List.iter fx xs;
    e "end"
  ;;

  let option
        ?(__FUNCTION__ : string = "")
        ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
        (k : Output.Kind.t)
        (prefix : string)
        (x : 'a option)
        (f : 'a Utils.Strfy.to_string)
    : unit
    =
    match x with
    | Some x -> thing ~__FUNCTION__ ~args k prefix x f
    | None -> thing ~__FUNCTION__ ~args k prefix "None" (Of Utils.Strfy.string)
  ;;

  let options
        ?(__FUNCTION__ : string = "")
        ?(args : Utils.Strfy.style_args = Utils.Strfy.style_args ())
        (k : Output.Kind.t)
        (prefix : string)
        (xs : 'a list option)
        (f : 'a Utils.Strfy.to_string)
    : unit
    =
    match xs with
    | Some xs -> things ~__FUNCTION__ ~args k prefix xs f
    | None -> thing ~__FUNCTION__ ~args k prefix "None" (Of Utils.Strfy.string)
  ;;
end

(***********************************************************************)

module MkDefault () : S =
  Make
    (Output.Mode.Default)
    (struct
      let prefix : string option = None
      let level : Output.Kind.level -> bool = !Output.Kind.default_level
      let special : Output.Kind.special -> bool = !Output.Kind.default_special
    end)

module Default : S = MkDefault ()
