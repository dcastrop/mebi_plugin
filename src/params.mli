
(**********************)
(** Bounds ************)
(**********************)

val default_bound : int
val the_bounds : (int * (int option)) ref
val reset_bounds : unit -> unit
val fst_bound : unit -> int
val snd_bound : unit -> int
val printout_bounds : unit -> unit 
val set_bounds : (int * (int option)) -> unit 

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

val reset_show_debug : unit -> unit
val printout_show_debug : unit -> unit 
val set_show_debug : bool -> unit 

val reset_show_details : unit -> unit
val printout_show_details : unit -> unit 
val set_show_details : bool -> unit 

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

module WeakKind : sig
  type t =
    | OptionRef of (Mebi_wrapper.E.t * Names.GlobRef.t)
    | OptionConstr of Mebi_wrapper.E.t
    | CustomRef of
        (Mebi_wrapper.E.t * Names.GlobRef.t)
        * (Mebi_wrapper.E.t * Names.GlobRef.t)
    | CustomConstr of Mebi_wrapper.E.t * (Mebi_wrapper.E.t * Names.GlobRef.t)

  val to_string : t -> string 

  val eq : t -> t -> bool
end

val the_weak_types : (WeakKind.t option * WeakKind.t option) ref

val fst_weak_type : unit -> WeakKind.t option
val snd_weak_type : unit -> WeakKind.t option

val reset_weak_types : unit -> unit
val printout_weak_types : unit -> unit 
val printout_fst_weak_type : unit -> unit 
val printout_snd_weak_type : unit -> unit 

module WeakArgs : sig
  type t =
    | OptionRef of Libnames.qualid
    | OptionConstr of Constrexpr.constr_expr
    | CustomRef of Libnames.qualid * Libnames.qualid
    | CustomConstr of Constrexpr.constr_expr * Libnames.qualid
end

val set_fst_weak_type_arg : WeakArgs.t -> unit
val set_snd_weak_type_arg : WeakArgs.t -> unit
val set_weak_types_args : WeakArgs.t * WeakArgs.t option -> unit

(**********************)
(** Run ***************)
(**********************)

val obtain_weak_kinds_from_args : unit -> unit Mebi_wrapper.mm

val get_fst_params : unit -> int * WeakKind.t option
val get_snd_params : unit -> int * WeakKind.t option

(**********************)
(** All ***************)
(**********************)

val printout_all : unit -> unit
val reset_all : unit -> unit

