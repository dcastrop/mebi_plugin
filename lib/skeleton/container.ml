(* See the [.mli]. doc/skeleton.md, "Terms as containers". *)

open Names

type path = int list

type shape =
  | Slot of int
  | Node of node

and node =
  { cons : constructor
  ; univ : UVars.Instance.t
  ; params : Constr.t list
  ; kids : shape list
  }

(* {1 Stage 1: the term language} *)

let nparams env ((ind, _) : constructor) : int =
  let mib, _ = Inductive.lookup_mind_specif env ind in
  mib.Declarations.mind_nparams
;;

let view env (t : Constr.t)
  : (constructor * UVars.Instance.t * Constr.t list * Constr.t list) option
  =
  let h, args = Constr.decompose_app_list t in
  match Constr.kind h with
  | Constr.Construct (c, u) ->
    let np = nparams env c in
    let _, mip = Inductive.lookup_mind_specif env (fst c) in
    let nf = mip.Declarations.mind_consnrealargs.(snd c - 1) in
    if List.length args = np + nf
    then
      Some
        ( c
        , u
        , List.filteri (fun i _ -> i < np) args
        , List.filteri (fun i _ -> i >= np) args )
    else None
  | _ -> None
;;

(* {1 Stage 2: refinement by the structural constructors} *)

let mem_c c l = List.exists (Construct.CanOrd.equal c) l

let split env (structural : constructor list) (t : Constr.t)
  : shape * Constr.t list
  =
  let values = ref [] in
  let next = ref 0 in
  let rec go (t : Constr.t) : shape =
    match view env t with
    | Some (c, u, params, fs) when mem_c c structural ->
      Node { cons = c; univ = u; params; kids = List.map go fs }
    | _ ->
      let p = !next in
      incr next;
      values := t :: !values;
      Slot p
  in
  let s = go t in
  s, List.rev !values
;;

let rebuild (s : shape) (values : Constr.t array) : Constr.t =
  let rec go = function
    | Slot p -> values.(p)
    | Node n ->
      Constr.mkApp
        ( Constr.mkConstructU (n.cons, n.univ)
        , Array.of_list (n.params @ List.map go n.kids) )
  in
  go s
;;

let rec slots = function
  | Slot p -> [ p ]
  | Node n -> List.concat_map slots n.kids
;;

let rec key = function
  | Slot _ -> "_"
  | Node n ->
    Printf.sprintf
      "%s/%d(%s)"
      (MutInd.to_string (fst (fst n.cons)))
      (snd n.cons)
      (String.concat "," (List.map key n.kids))
;;

let renumber (s : shape) : shape =
  let n = ref 0 in
  let rec go = function
    | Slot _ ->
      let p = !n in
      incr n;
      Slot p
    | Node x -> Node { x with kids = List.map go x.kids }
  in
  go s
;;

(* {1 Positions as paths} *)

let leaf_paths (s : shape) : path list =
  let rec go rev_path = function
    | Slot _ -> [ List.rev rev_path ]
    | Node n -> List.concat (List.mapi (fun i k -> go (i :: rev_path) k) n.kids)
  in
  go [] s
;;

let rec prefix (p : path) (q : path) : bool =
  match p, q with
  | [], _ -> true
  | i :: p, j :: q -> i = j && prefix p q
  | _ :: _, [] -> false
;;

let at (s : shape) (p : path) : shape option =
  let rec go s p =
    match p, s with
    | [], _ -> Some s
    | i :: p, Node n when i < List.length n.kids -> go (List.nth n.kids i) p
    | _ -> None
  in
  Stdlib.Option.map renumber (go s p)
;;

let agree_outside (s : shape) (p : path) (s' : shape) : bool =
  let rec go s p s' =
    match p, s, s' with
    | [], _, _ -> true
    | i :: p, Node n, Node n' ->
      Construct.CanOrd.equal n.cons n'.cons
      && List.length n.kids = List.length n'.kids
      && i < List.length n.kids
      && List.for_all
           Fun.id
           (List.mapi
              (fun j (k, k') -> if j = i then go k p k' else key k = key k')
              (List.combine n.kids n'.kids))
    | _ -> false
  in
  go s p s'
;;
