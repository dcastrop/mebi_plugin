module type S = sig
  type t =
    { this : tactic
    ; next : t option
    }

  and tactic =
    { get : unit Proofview.tactic
    ; msg : (Output.Kind.t * string) option
    }

  val create : ?level:Output.Kind.t -> ?msg:string -> unit Proofview.tactic -> t
  val empty : unit -> t
  val do_nothing : unit -> t
  val seq : t -> t -> t

  exception EmptyTacticChain

  val chain : ?nonempty:bool -> t list -> t
  val unpack : t -> unit Proofview.tactic
end

module Make (Log : Logger.S) : S = struct
  type t =
    { this : tactic
    ; next : t option
    }

  and tactic =
    { get : unit Proofview.tactic
    ; msg : (Output.Kind.t * string) option
    }

  (** [create ?level ?msg tactic] ... *)
  let create
        ?(level : Output.Kind.t = Info)
        ?(msg : string option)
        (x : unit Proofview.tactic)
    : t
    =
    { this = { msg = Option.cata (fun m -> Some (level, m)) None msg; get = x }
    ; next = None
    }
  ;;

  let empty () : t = create (Proofview.tclUNIT ())

  let do_nothing () : t =
    create ~level:Debug ~msg:"(skip)" (Proofview.tclUNIT ())
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

  let rec to_string : t -> string =
    let f : tactic -> string = function
      | { msg = None; _ } -> ""
      (* | { msg = Some (Debug, s); _ } -> "" *)
      | { msg = Some (_, s); _ } -> Printf.sprintf "%s; " s
    in
    function
    | { this; next = None } -> Printf.sprintf "%s." (f this)
    | { this; next = Some next } ->
      Printf.sprintf "%s%s" (f this) (to_string next)
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
