(** Inputs generated in OCaml: the levers of [examples/Evaluation], with the
    same semantics, so they can be scaled past what the plugin extracts in
    reasonable time. Each family's state count is checked against its
    formula when built, and its pairs are bisimilar. *)

(** [explore ~silent steps roots] is the LTS reachable from [roots] under
    [steps], one side per root, encoded with one table (so a term both
    sides reach is one state, as in the plugin). [steps t] lists [t]'s
    moves as [(label, target)]. *)
let explore
      (type term)
      ~(name : string)
      ~(silent : int list)
      (steps : term -> (int * term) list)
      (x : term)
      (y : term)
  : Input.t
  =
  let enc : (term, int) Hashtbl.t = Hashtbl.create 1024 in
  let encode t =
    match Hashtbl.find_opt enc t with
    | Some i -> i
    | None ->
      let i = Hashtbl.length enc in
      Hashtbl.add enc t i;
      i
  in
  let side root : Input.side =
    let seen = Hashtbl.create 1024 in
    let edges = ref [] in
    let rec go = function
      | [] -> ()
      | t :: rest ->
        let i = encode t in
        let next =
          List.filter_map
            (fun (l, t') ->
              edges := (i, l, encode t') :: !edges;
              if Hashtbl.mem seen t'
              then None
              else (
                Hashtbl.add seen t' ();
                Some t'))
            (steps t)
        in
        go (List.rev_append next rest)
    in
    Hashtbl.add seen root ();
    go [ root ];
    { init = encode root
    ; states = Hashtbl.fold (fun t () acc -> encode t :: acc) seen []
    ; edges = !edges
    }
  in
  let a = side x in
  let b = side y in
  { name; silent; a; b = Some b; bisimilar = Some true }
;;

(** [check_states input n] fails unless each side of [input] has [n]
    states. *)
let check_states (i : Input.t) (n : int) : Input.t =
  let ok (s : Input.side) = Input.num_states s = n in
  if ok i.a && Option.fold ~none:true ~some:ok i.b
  then i
  else failwith (Printf.sprintf "%s: not %i states a side" i.name n)
;;

(** {1 Width ([examples/Evaluation/Base.v], [Width.v])} *)

module Width = struct
  type act =
    | A
    | B

  type proc =
    | Pnil
    | Pact of act * proc
    | Ppar of proc * proc

  let label = function A -> 1 | B -> 2

  let rec steps : proc -> (int * proc) list = function
    | Pnil -> []
    | Pact (a, p) -> [ label a, p ]
    | Ppar (p, q) ->
      List.map (fun (a, p') -> a, Ppar (p', q)) (steps p)
      @ List.map (fun (a, q') -> a, Ppar (p, q')) (steps q)
  ;;

  let p = Pact (A, Pact (B, Pnil))
  let q = Pact (B, Pnil)
  let rec spawn n p = if n = 0 then p else Ppar (p, spawn (n - 1) p)

  (** [make n]: [wl n] against [wr n], [2 * 3^(n+1)] states a side. *)
  let make (n : int) : Input.t =
    let rec pow b e = if e = 0 then 1 else b * pow b (e - 1) in
    explore
      ~name:(Printf.sprintf "width-%i" n)
      ~silent:[]
      steps
      (Ppar (spawn n p, q))
      (Ppar (q, spawn n p))
    |> fun i -> check_states i (2 * pow 3 (n + 1))
  ;;
end

(** {1 Depth ([examples/Evaluation/Depth.v])} *)

module Depth = struct
  type action =
    | T1
    | T2

  type term =
    | Trec
    | Tend
    | Tfix of term
    | Tact of action * term
    | Tpar of action * action * term

  (** [None], the collapse, is silent. *)
  let label = function None -> 0 | Some T1 -> 1 | Some T2 -> 2

  let rec subst t1 = function
    | Trec -> t1
    | Tend -> Tend
    | Tfix t -> Tfix t
    | Tact (a, t) -> Tact (a, subst t1 t)
    | Tpar (a, b, t) -> Tpar (a, b, subst t1 t)
  ;;

  let rec fix_depth = function Tfix u -> 1 + fix_depth u | _ -> 0

  (** [Guarded K]'s [termLTS], labels unencoded. *)
  let rec steps k : term -> (action option * term) list = function
    | Trec | Tend -> []
    | Tact (a, t) -> [ Some a, t ]
    | Tpar (a, b, t) -> [ Some a, Tact (b, t); Some b, Tact (a, t) ]
    | Tfix t ->
      (if fix_depth t < k
       then List.map (fun (a, t') -> a, Tfix t') (steps k (subst (Tfix t) t))
       else [])
      @ (match t with Tfix u -> [ None, Tfix u ] | _ -> [])
  ;;

  let x = Tact (T1, Tact (T2, Trec))

  (** [make k]: [tfix X] against [tfix (tfix X)]; [tfix X] has [2K + 1]
      states. The right side has one more, the [tfix (tfix X)] it starts
      from, so only the left is checked. *)
  let make (k : int) : Input.t =
    let i =
      explore
        ~name:(Printf.sprintf "depth-%i" k)
        ~silent:[ label None ]
        (fun t -> List.map (fun (a, t') -> label a, t') (steps k t))
        (Tfix x)
        (Tfix (Tfix x))
    in
    if Input.num_states i.a = (2 * k) + 1
    then i
    else failwith (Printf.sprintf "%s: not %i states" i.name ((2 * k) + 1))
  ;;
end
