(** [split_at i l acc] is a tuple containing two lists [(l', acc)] split from list [l] at index [i]. *)
let rec split_at i l acc =
  if i <= 0
  then l, acc
  else (
    match l with
    | [] -> acc, []
    | h :: t -> split_at (i - 1) t (h :: acc))
;;

(** [strip_snd l] is the list of rhs elements in a list of tuples [l] (typically a [constr]). *)
let rec strip_snd (l : (EConstr.t * EConstr.t) list) : EConstr.t list =
  match l with
  | [] -> []
  | h :: t -> snd h :: strip_snd t
;;
