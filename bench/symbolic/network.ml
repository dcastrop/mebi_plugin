(** A system as a network of finite-state components: the rule IR of
    [SYMBOLIC_PLAN.md] in miniature. A global state is one local state per
    component. A global step is a [group]: some components each take one
    of their listed local moves, together, under the group's label; the
    rest stay put.

    The front ends below build networks from OCaml copies of the examples'
    semantics, already normalised: structural congruence (the [Proc]
    rules [do_comm], [do_assocl], [do_assocr]) is not represented, the
    parallel structure is a fixed vector. That is the normalisation the
    plan's state schema assumes; the hand-written [ProcCongruence.v] does
    the same in Rocq. *)

type label =
  | Tau
  | Vis of string

let label_to_string = function Tau -> "tau" | Vis s -> s

type component =
  { cname : string
  ; size : int (** local states [0 .. size-1] *)
  ; init : int
  ; names : string array (** each local state, printed *)
  }

(** One global step: each part is a component and the local moves
    [(src, dst)] it may take. The parts move together. *)
type group =
  { label : label
  ; parts : (int * (int * int) list) list
  }

type t =
  { name : string
  ; comps : component array
  ; groups : group list
  }

(** [explore steps root] is the local LTS of a sequential term: its
    states (in the order found, [root] first) and its moves. *)
let explore
      (type a)
      ~(show : a -> string)
      (steps : a -> ('l * a) list)
      (root : a)
  : component * ('l * int * int) list
  =
  let tbl : (a, int) Hashtbl.t = Hashtbl.create 16 in
  let order = ref [] in
  let moves = ref [] in
  let id t =
    match Hashtbl.find_opt tbl t with
    | Some i -> i, false
    | None ->
      let i = Hashtbl.length tbl in
      Hashtbl.add tbl t i;
      order := t :: !order;
      i, true
  in
  let rec go = function
    | [] -> ()
    | t :: rest ->
      let i, _ = id t in
      let next =
        List.filter_map
          (fun (l, t') ->
            let j, fresh = id t' in
            moves := (l, i, j) :: !moves;
            if fresh then Some t' else None)
          (steps t)
      in
      go (rest @ next)
  in
  ignore (id root);
  go [ root ];
  let names = Array.of_list (List.rev_map show !order) in
  ( { cname = show root; size = Array.length names; init = 0; names }
  , List.rev !moves )
;;

(** {1 Proc, layered semantics ([examples/Proc.v], [Layered])} *)

module Proc = struct
  type action =
    | Send of string
    | Recv of string

  type term =
    | Trec
    | Tend
    | Tfix of term
    | Tact of action * term
    | Tseq of term * term

  let rec subst t1 = function
    | Trec -> t1
    | Tend -> Tend
    | Tfix t -> Tfix t
    | Tact (a, t) -> Tact (a, subst t1 t)
    | Tseq (t, s) -> Tseq (t, subst t1 s)
  ;;

  (** [termLTS]: a send and a receive on [a] both carry the label [a]. *)
  let rec steps : term -> (label * term) list = function
    | Trec | Tend -> []
    | Tact ((Send a | Recv a), t) -> [ Vis a, t ]
    | Tseq (Tend, s) -> [ Tau, s ]
    | Tseq (t, s) -> List.map (fun (l, t') -> l, Tseq (t', s)) (steps t)
    | Tfix t -> [ Tau, subst (Tfix t) t ]
  ;;

  let rec show = function
    | Trec -> "rec"
    | Tend -> "end"
    | Tfix t -> "fix(" ^ show t ^ ")"
    | Tact (Send a, t) -> "!" ^ a ^ "." ^ show t
    | Tact (Recv a, t) -> "?" ^ a ^ "." ^ show t
    | Tseq (t, s) -> "(" ^ show t ^ ";" ^ show s ^ ")"
  ;;

  (** [cyclic act] is [tfix (tseq (tact act tend) trec)], as Test4's
      [a1], [a2], [b1], [b2]. *)
  let cyclic act = Tfix (Tseq (Tact (act, Tend), Trec))

  (** [network name terms]: the components [terms] in parallel. [compLTS]
      has no synchronisation: each component moves alone ([do_t] under
      [do_parl]/[do_parr]). ([do_par_end] merges two ended components; no
      component here ends, so it is left out.) *)
  let network (name : string) (terms : term list) : t =
    let comps_moves = List.map (explore ~show steps) terms in
    let comps = Array.of_list (List.map fst comps_moves) in
    let groups =
      List.concat
        (List.mapi
           (fun i (_, moves) ->
             let labels =
               List.sort_uniq compare (List.map (fun (l, _, _) -> l) moves)
             in
             List.map
               (fun l ->
                 { label = l
                 ; parts =
                     [ ( i
                       , List.filter_map
                           (fun (l', s, d) ->
                             if l = l' then Some (s, d) else None)
                           moves )
                     ]
                 })
               labels)
           comps_moves)
    in
    { name; comps; groups }
  ;;
end

(** {1 CCS ([examples/CCS.v])}, restricted to a top-level restriction
    over a parallel vector of sequential processes, which is the shape of
    every CCS system used here. *)

module Ccs = struct
  type act =
    | In of string
    | Out of string

  type proc =
    | Pnil
    | Pre of act option * proc
    | Sum of proc * proc
    | Var of string

  (** [steps defs p] is [step], with [var k] stepping as its definition. *)
  let rec steps (defs : string -> proc) : proc -> (act option * proc) list
    = function
    | Pnil -> []
    | Pre (l, p) -> [ l, p ]
    | Sum (p, q) -> steps defs p @ steps defs q
    | Var k -> steps defs (defs k)
  ;;

  let rec show = function
    | Pnil -> "0"
    | Pre (None, p) -> "tau." ^ show p
    | Pre (Some (In n), p) -> "?" ^ n ^ "." ^ show p
    | Pre (Some (Out n), p) -> "!" ^ n ^ "." ^ show p
    | Sum (p, q) -> "(" ^ show p ^ "+" ^ show q ^ ")"
    | Var k -> k
  ;;

  let act_label = function
    | None -> Tau
    | Some (In n) -> Vis ("?" ^ n)
    | Some (Out n) -> Vis ("!" ^ n)
  ;;

  (** [network name defs restricted procs] is [res restricted (par procs)]:
      each component's own steps not on a restricted name, and every
      handshake [!n] / [?n] between two components ([s_comm], [s_comm']),
      silent. *)
  let network
        (name : string)
        (defs : string -> proc)
        (restricted : string list)
        (procs : proc list)
    : t
    =
    let cm = List.map (explore ~show (steps defs)) procs in
    let comps = Array.of_list (List.map fst cm) in
    let moves = Array.of_list (List.map snd cm) in
    let n = Array.length comps in
    let pick i (f : act option -> bool) =
      List.filter_map
        (fun (l, s, d) -> if f l then Some (s, d) else None)
        moves.(i)
    in
    let allowed = function
      | None -> true
      | Some (In x | Out x) -> Bool.not (List.mem x restricted)
    in
    let own =
      List.concat
        (List.init n (fun i ->
           let ls =
             List.sort_uniq compare (List.map (fun (l, _, _) -> l) moves.(i))
           in
           List.filter_map
             (fun l ->
               if allowed l
               then
                 Some { label = act_label l; parts = [ i, pick i (( = ) l) ] }
               else None)
             ls))
    in
    let names =
      List.sort_uniq
        compare
        (List.concat_map
           (fun ms ->
             List.filter_map
               (fun (l, _, _) ->
                 match l with Some (Out x) -> Some x | _ -> None)
               ms)
           (Array.to_list moves))
    in
    let comm =
      List.concat_map
        (fun x ->
          List.concat
            (List.init n (fun i ->
               List.filter_map
                 (fun j ->
                   let outs = pick i (( = ) (Some (Out x)))
                   and ins = pick j (( = ) (Some (In x))) in
                   if i <> j && outs <> [] && ins <> []
                   then Some { label = Tau; parts = [ i, outs; j, ins ] }
                   else None)
                 (List.init n Fun.id))))
        names
    in
    { name; comps; groups = own @ comm }
  ;;
end

(** {1 Explicit expansion}, for the oracle and for counting. *)

(** [steps net s] is every global step out of [s]: label and target. *)
let steps (net : t) (s : int array) : (label * int array) list =
  List.concat_map
    (fun g ->
      (* every way the parts can move from [s] *)
      List.fold_left
        (fun acc (c, ms) ->
          List.concat_map
            (fun t ->
              List.filter_map
                (fun (src, dst) ->
                  if src = s.(c)
                  then (
                    let t' = Array.copy t in
                    t'.(c) <- dst;
                    Some t')
                  else None)
                ms)
            acc)
        [ s ]
        g.parts
      |> List.map (fun t -> g.label, t))
    net.groups
;;

let init (net : t) : int array = Array.map (fun c -> c.init) net.comps

let labels (net : t) : label list =
  List.sort_uniq compare (List.map (fun g -> g.label) net.groups)
;;
