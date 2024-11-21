(** [split_at i l acc] is a tuple containing two lists [(l', acc)] split from list [l] at index [i]. *)
let rec split_at (i : int) (l : 'a list) (acc : 'a list) =
  if i <= 0
  then l, acc
  else (
    match l with
    | [] -> acc, []
    | h :: t -> split_at (i - 1) t (h :: acc))
;;

(** [strip_snd l] is the list of rhs elements in a list of tuples [l] (typically a [constr]). *)
let rec strip_snd (l : (Evd.econstr * Evd.econstr) list) : Evd.econstr list =
  match l with
  | [] -> []
  | h :: t -> snd h :: strip_snd t
;;

(** [econstr_mem m l] is [true] if [m] is in [l]. (checks first occurance.) *)
let rec econstr_mem
  (env : Environ.env)
  (sigma : Evd.evar_map)
  (m : Evd.econstr)
  (l : Evd.econstr list)
  : bool
  =
  match l with
  | [] -> false
  | h :: t ->
    (match EConstr.eq_constr sigma h m with
     | true -> true
     | _ -> econstr_mem env sigma m t)
;;

(** [econstr_tup_mem m l] is [true] if [m] is in [l]. (checks first occurance.) *)
let rec econstr_tup_mem
  (env : Environ.env)
  (sigma : Evd.evar_map)
  (m : Evd.econstr * Evd.econstr)
  (l : (Evd.econstr * Evd.econstr) list)
  : bool
  =
  match l with
  | [] -> false
  | h :: t ->
    (match EConstr.eq_constr sigma (snd h) (snd m) with
     | true -> true
     | _ -> econstr_tup_mem env sigma m t)
;;

(** [econstr_list_cap es to_check] is the list in of elements in [to_check] that do not appear in [es].*)
let rec econstr_list_cap
  env
  sigma
  (es : Evd.econstr list)
  (to_check : Evd.econstr list)
  : Evd.econstr list
  =
  match to_check with
  (* return nothing else *)
  | [] -> []
  (*  *)
  (* include [h] in return if not a member of [es], and continue checking [t]. *)
  | h :: t ->
    List.concat
      [ (match econstr_mem env sigma h es with
         | true -> []
         | _ -> [ h ])
      ; econstr_list_cap env sigma es t
      ]
;;

(** [econstr_tuplist_cap es to_check] is the list in of elements in [to_check] that do not appear in [es].*)
let rec econstr_tuplist_cap
  (env : Environ.env)
  (sigma : Evd.evar_map)
  (es : (Evd.econstr * Evd.econstr) list)
  (to_check : (Evd.econstr * Evd.econstr) list)
  : (Evd.econstr * Evd.econstr) list
  =
  match to_check with
  (* return nothing else *)
  | [] -> []
  (* include [h] in return if not a member of [es], and continue checking [t]. *)
  | h :: t ->
    List.concat
      [ (match econstr_tup_mem env sigma h es with
         | true -> []
         | _ -> [ h ])
      ; econstr_tuplist_cap env sigma es t
      ]
;;

(** [econstr_list_merge env sigma l1 l2] is the combination of [l1] and [l2] with any duplicates removed. *)
let rec econstr_list_merge
  (env : Environ.env)
  (sigma : Evd.evar_map)
  (l1 : Evd.econstr list)
  (l2 : Evd.econstr list)
  =
  match l1 with
  | [] -> l2
  | h :: t ->
    (match econstr_mem env sigma h l2 with
     | true -> econstr_list_merge env sigma t l2
     | _ -> h :: econstr_list_merge env sigma t l2)
;;

(** [econstr_tuplist_merge env sigma l1 l2] is the combination of [l1] and [l2] with any duplicates removed. *)
let rec econstr_tuplist_merge
  (env : Environ.env)
  (sigma : Evd.evar_map)
  (l1 : (Evd.econstr * Evd.econstr) list)
  (l2 : (Evd.econstr * Evd.econstr) list)
  =
  match l1 with
  | [] -> l2
  | h :: t ->
    (match econstr_tup_mem env sigma h l2 with
     | true -> econstr_tuplist_merge env sigma t l2
     | _ -> h :: econstr_tuplist_merge env sigma t l2)
;;

(** [econstr_list_unique env sigma l] is list [l] with any duplicates removed. *)
let rec econstr_list_unique
  (env : Environ.env)
  (sigma : Evd.evar_map)
  (l : Evd.econstr list)
  =
  match l with
  | [] -> []
  | h :: t ->
    (match econstr_mem env sigma h t with
     (* if dupe, skip this one add the next *)
     | true -> econstr_list_unique env sigma t
     (* else keep *)
     | _ -> h :: econstr_list_unique env sigma t)
;;
