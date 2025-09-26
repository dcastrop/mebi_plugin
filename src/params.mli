(**********************)
(** Bounds ************)
(**********************)

val default_bound : int
val the_bounds : (int * int option) ref
val reset_bounds : unit -> unit
val fst_bound : unit -> int
val snd_bound : unit -> int
val printout_bounds : unit -> unit
val set_bounds : int * int option -> unit

(**********************)
(** Fail if incomplete*)
(**********************)

val the_fail_if_incomplete : bool ref
val reset_fail_if_incomplete : unit -> unit
val printout_fail_if_incomplete : unit -> unit
val set_fail_if_incomplete : bool -> unit

(**********************)
(** Fail if not bisim *)
(**********************)

val the_fail_if_not_bisim : bool ref
val reset_fail_if_not_bisim : unit -> unit
val printout_fail_if_not_bisim : unit -> unit
val set_fail_if_not_bisim : bool -> unit

(**********************)
(** File Dump *********)
(**********************)

val the_dump_to_file : bool ref
val reset_dump_to_file : unit -> unit
val printout_dump_to_file : unit -> unit
val set_dump_to_file : bool -> unit

(**********************)
(** Messages **********)
(**********************)

(* show any *)
val reset_show_any : unit -> unit
val printout_show_any : unit -> unit
val set_show_any : bool -> unit

(* notice *)
val reset_show_notice : unit -> unit
val printout_show_notice : unit -> unit
val set_show_notice : bool -> unit

(* debug *)
val reset_show_debug : unit -> unit
val printout_show_debug : unit -> unit
val set_show_debug : bool -> unit

(* details *)
val reset_show_details : unit -> unit
val printout_show_details : unit -> unit
val set_show_details : bool -> unit

(* results *)
val reset_show_result : unit -> unit
val printout_show_result : unit -> unit
val set_show_result : bool -> unit

(* warning *)
val reset_show_warning : unit -> unit
val printout_show_warning : unit -> unit
val set_show_warning : bool -> unit

(**********************)
(** Weak Mode *********)
(**********************)

val the_weak_mode : bool ref
val reset_weak_mode : unit -> unit
val printout_weak_mode : unit -> unit
val set_weak_mode : bool -> unit

(**********************)
(** Weak Type *********)
(**********************)

module WeakEnc : sig
  type t =
    | OptionConstr of Mebi_wrapper.E.t
    | CustomConstr of Mebi_wrapper.E.t * Mebi_wrapper.E.t

  (* val is_option : t -> bool *)
  val to_string : t -> string Mebi_wrapper.mm
  val eq : t -> t -> bool
end

val the_weak_types : (WeakEnc.t option * WeakEnc.t option) ref
val fst_weak_type : unit -> WeakEnc.t option
val snd_weak_type : unit -> WeakEnc.t option
val reset_weak_types : unit -> unit
val printout_weak_types : unit -> unit Mebi_wrapper.mm
val printout_fst_weak_type : unit -> unit Mebi_wrapper.mm
val printout_snd_weak_type : unit -> unit Mebi_wrapper.mm

module WeakArgs : sig
  type t =
    | OptionConstr of Constrexpr.constr_expr
    | CustomConstr of Constrexpr.constr_expr * Libnames.qualid
end

val set_fst_weak_type_arg : WeakArgs.t -> unit
val set_snd_weak_type_arg : WeakArgs.t -> unit
val set_weak_types_args : WeakArgs.t * WeakArgs.t option -> unit

(**********************)
(** Run ***************)
(**********************)

val obtain_weak_kinds_from_args : unit -> unit Mebi_wrapper.mm
val get_fst_params : unit -> int * WeakEnc.t option
val get_snd_params : unit -> int * WeakEnc.t option

(**********************)
(** All ***************)
(**********************)

val printout_all : unit -> unit Mebi_wrapper.mm
val reset_all : unit -> unit Mebi_wrapper.mm
