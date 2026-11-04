(** The two-way table between Rocq terms and their encodings (the
    model's states and labels are encodings): [F] maps a term to its
    encoding, [B] an encoding back to its term. One table per instance,
    shared counter ({!Encoding.S}) across instances. *)
module type S = Bi_encoding_intf.S

module Make (Enc : Encoding.S) : S with type enc = Enc.t
