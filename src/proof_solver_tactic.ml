module type S = sig
  module Tac : sig
    type t =
      { get : unit Proofview.tactic
      ; msg : msg option
      }

    and msg = Output.Kind.t * string

    val to_string_opt : t -> string option
  end

  type t =
    { this : Tac.t
    ; next : t option
    }

  val create : ?kind:Output.Kind.t -> ?msg:string -> unit Proofview.tactic -> t
  val empty : unit -> t
  val do_nothing : unit -> t
  val seq : t -> t -> t

  exception EmptyTacticChain

  val chain : ?nonempty:bool -> t list -> t
  val unpack : t -> unit Proofview.tactic
end

module Make (Log : Logger.S) : S = struct
  module Tac = struct
    type t =
      { get : unit Proofview.tactic
      ; msg : msg option
      }

    and msg = Output.Kind.t * string

    let to_string_opt : t -> string option = function
      | { msg = None; _ } -> None
      | { msg = Some (k, s); _ } ->
        if Log.Config.is_enabled k then Some s else None
    ;;

    let create
          ?(kind : Output.Kind.t = Output.Kind.Info)
          (x : unit Proofview.tactic)
      : string option -> t
      = function
      | None -> { get = x; msg = None }
      | Some y -> { get = x; msg = Some (kind, y) }
    ;;
  end

  type t =
    { this : Tac.t
    ; next : t option
    }

  (** [create ?level ?msg tactic] ... *)
  let create
        ?(kind : Output.Kind.t = Info)
        ?(msg : string option)
        (x : unit Proofview.tactic)
    : t
    =
    { this = Tac.create ~kind x msg; next = None }
  ;;

  let empty () : t = create (Proofview.tclUNIT ())

  let do_nothing () : t =
    create ~kind:Debug ~msg:"(skip)" (Proofview.tclUNIT ())
  ;;

  (** [seq a b] appends [b] to the sequence of [a]. *)
  let rec seq : t -> t -> t = function
    | { this; next = None } -> fun (b : t) -> { this; next = Some b }
    | { this; next = Some next } ->
      fun (b : t) -> { this; next = Some (seq next b) }
  ;;

  exception EmptyTacticChain

  (** [chain ?nonempty (x::xs)] applies [seq x (chain xs)].
      @raise EmptyTacticChain if the list is empty and [?nonempty] s true. *)
  let rec chain ?(nonempty : bool = false) : t list -> t = function
    | [] -> if nonempty then raise EmptyTacticChain else empty ()
    | h :: [] -> h
    | h :: tl -> seq h (chain tl)
  ;;

  let to_string (x : t) : string =
    let rec f : t -> string option list = function
      | { this; next = None } -> [ Tac.to_string_opt this ]
      | { this; next = Some next } -> Tac.to_string_opt this :: f next
    in
    f x |> Utils.filter_opt |> Utils.str_sep ~sep:"; " ~last:"."
  ;;

  let unpack (x : t) : unit Proofview.tactic =
    Log.notice (to_string x);
    let rec f : t -> unit Proofview.tactic = function
      | { this; next = None } -> this.get
      | { this; next = Some next } -> Proofview.tclTHEN this.get (f next)
    in
    let y = f x in
    y
  ;;
end
