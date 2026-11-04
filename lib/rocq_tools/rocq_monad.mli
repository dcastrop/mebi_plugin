(** The plugin's monad over the Rocq context ([env], [sigma]) and the
    encoding tables ({!Bi_encoding}): a computation ['a mm] is a function
    from the current state to a value and the next state, so threading
    [sigma] through Rocq calls is implicit.

    {b Exceptions are deferred.} An ['a mm] does nothing until it is run
    ({!S.run}), so an exception raised inside it -- by a [state] function, or
    in the continuation of a [let*] -- is raised when it runs, not when it
    is built. A [try ... with] around the {e construction} of a monadic
    value therefore does not catch it; handle it around {!S.run}, or inside
    the function given to {!S.val-state}. [ASSISTED-CHANGES.md]
    (2026-10-09) lists the places that got this wrong, all since fixed. *)
module type S = Rocq_monad_intf.S

module Make (Enc : Encoding.S) : S with type enc = Enc.t
