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
    -> Output.Kind.t
    -> string
    -> 'a
    -> ('a -> string)
    -> unit

  val things
    :  ?__FUNCTION__:string
    -> Output.Kind.t
    -> string
    -> 'a list
    -> ('a -> string)
    -> unit

  val option
    :  ?__FUNCTION__:string
    -> Output.Kind.t
    -> string
    -> 'a option
    -> ('a -> string)
    -> unit

  val options
    :  ?__FUNCTION__:string
    -> Output.Kind.t
    -> string
    -> 'a list option
    -> ('a -> string)
    -> unit
end

module Make
    (Mode : Output.Mode.S)
    (X : sig
       val prefix : string option
       val level : Output.Kind.level -> bool
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
        (k : Output.Kind.t)
        (prefix : string)
        (x : 'a)
        (f : 'a -> string)
    : unit
    =
    do_output
      ~prefix:(Some (Printf.sprintf "%s: " prefix))
      ~__FUNCTION__
      k
      (f x)
  ;;

  let things
        ?(__FUNCTION__ : string = "")
        (k : Output.Kind.t)
        (prefix : string)
        (xs : 'a list)
        (f : 'a -> string)
    : unit
    =
    (* NOTE: start and end *)
    let e : string -> unit =
      do_output ~prefix:(Some (Printf.sprintf "%s: " prefix)) ~__FUNCTION__ k
    in
    (* NOTE: indexed iterator *)
    let index : int ref = ref 0 in
    let fx (x : 'a) : unit =
      thing k (Printf.sprintf "%i" !index) x f;
      index := !index + 1
    in
    e "start";
    List.iter fx xs;
    e "end"
  ;;

  let option
        ?(__FUNCTION__ : string = "")
        (k : Output.Kind.t)
        (prefix : string)
        (x : 'a option)
        (f : 'a -> string)
    : unit
    =
    match x with
    | Some x -> thing ~__FUNCTION__ k prefix x f
    | None -> thing ~__FUNCTION__ k prefix "None" (fun x -> x)
  ;;

  let options
        ?(__FUNCTION__ : string = "")
        (k : Output.Kind.t)
        (prefix : string)
        (xs : 'a list option)
        (f : 'a -> string)
    : unit
    =
    match xs with
    | Some xs -> things ~__FUNCTION__ k prefix xs f
    | None -> thing ~__FUNCTION__ k prefix "None" (fun x -> x)
  ;;
end

(***********************************************************************)

let default_level : Output.Kind.level -> bool = !Output.Kind.default_level
let default_special : Output.Kind.special -> bool = !Output.Kind.default_special

module MkDefault () : S =
  Make
    (Output.Mode.Default)
    (struct
      let prefix : string option = None
      let level : Output.Kind.level -> bool = default_level
      let special : Output.Kind.special -> bool = default_special
    end)

module Default : S = MkDefault ()

(***********************************************************************)

(** [module ReMake (Old) (New)] returns a new [Logger.S] with updated config.
    {b Example:}
    - [module Log = Logger.MkDefault ()]
    - module Log' = Logger.Remake (Log) (struct
      let level = Logger.default_level
      let special : Output.Kind.special -> bool = function
      | Trace -> false
      | Result -> true
      | Show -> true end) *)
module ReMake
    (Old : S)
    (New : sig
       val level : (Feedback.level -> bool) option
       val special : (Output.Kind.special -> bool) option
     end) : S with module Config.Mode = Old.Config.Mode =
  Make
    (Old.Config.Mode)
    (struct
      let prefix = Old.prefix

      let level : Output.Kind.level -> bool =
        match New.level with None -> Old.Config.Level.is_enabled | Some x -> x
      ;;

      let special : Output.Kind.special -> bool =
        match New.special with
        | None -> Old.Config.Special.is_enabled
        | Some x -> x
      ;;
    end)

(* NOTE: example of remake *)
(* module Log =
   Logger.ReMake
   (Log)
   (struct
   let level =
   Some (fun (x : Output.Kind.level) -> match x with _ -> true)
   ;;

   let special =
   Some (fun (x : Output.Kind.special) -> match x with _ -> true)
   ;;
   end) *)
