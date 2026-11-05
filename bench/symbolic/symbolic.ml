(** Weak bisimilarity of two networks, on BDDs.

    Encoding. Each component's local state takes [ceil (log2 size)] bits;
    each bit is two variables, current and next, adjacent. Sets of states
    and of pairs are over the current variables only; a group's relation
    is over its own components' current and next variables.

    Only preimages are taken: [tau*] is never built as a relation. The
    weak answer [y =a=> y'] into a set [Z] is
    [pre_tau* (pre_a (pre_tau* Z))], and [pre_tau* Z] is a least fixpoint.

    Bisimilarity is the greatest fixpoint
    [R = reach1 x reach2 /\ not (exists a, x -a-> x' with no answer from y into R) /\ (the same with the sides swapped)], the condition of
    [theories/Bisimilarity.v]'s [weak_bisimilar]: a strong move answered
    by a weak one. *)

type order =
  | Blocks (** all of system 1's components, then all of system 2's *)
  | Zip (** component [i] of system 1 next to component [i] of system 2 *)

let order_to_string = function Blocks -> "blocks" | Zip -> "zip"

type stats =
  { bisimilar : bool
  ; states1 : float
  ; states2 : float
  ; pairs : float (** pairs in the largest bisimulation over reach1 x reach2 *)
  ; rel_nodes : int
  ; peak_nodes : int
  ; iterations : int
  ; seconds : float
  }

module Make (B : Bdd.S) = struct
  (* var.(s).(c).(k): the current variable of bit [k] of component [c] of
     system [s]; its next variable is one more *)
  type enc =
    { nets : Network.t array
    ; var : int array array array
    ; nvars : int
    }

  let bits n =
    let rec go k = if 1 lsl k >= n then k else go (k + 1) in
    max 1 (go 0)
  ;;

  let encode (order : order) (n1 : Network.t) (n2 : Network.t) : enc =
    let nets = [| n1; n2 |] in
    let var =
      Array.map (fun (n : Network.t) -> Array.map (fun _ -> [||]) n.comps) nets
    in
    let next = ref 0 in
    let place s c =
      let b = bits nets.(s).comps.(c).size in
      var.(s).(c) <- Array.init b (fun k -> !next + (2 * k));
      next := !next + (2 * b)
    in
    let n1c = Array.length n1.comps
    and n2c = Array.length n2.comps in
    (match order with
     | Blocks ->
       for c = 0 to n1c - 1 do
         place 0 c
       done;
       for c = 0 to n2c - 1 do
         place 1 c
       done
     | Zip ->
       for c = 0 to max n1c n2c - 1 do
         if c < n1c then place 0 c;
         if c < n2c then place 1 c
       done);
    { nets; var; nvars = !next }
  ;;

  (** [code e s c v ~next] is component [c] of system [s] in local state
      [v], over its current (or next) variables. *)
  let code (e : enc) s c v ~next =
    let off = if next then 1 else 0 in
    let r = ref (B.dtrue ()) in
    Array.iteri
      (fun k x ->
        let lit = B.var (x + off) in
        r := B.dand !r (if (v lsr k) land 1 = 1 then lit else B.dnot lit))
      e.var.(s).(c);
    !r
  ;;

  (** A group, compiled: its relation, the cube of its components' next
      variables (quantified in a preimage), of their current ones
      (quantified in an image), and the renaming between the two. *)
  type cgroup =
    { label : Network.label
    ; rel : B.t
    ; next_cube : B.t
    ; cur_cube : B.t
    ; swap : int array
    }

  let compile (e : enc) s (g : Network.group) : cgroup =
    let rel =
      List.fold_left
        (fun acc (c, moves) ->
          B.dand
            acc
            (List.fold_left
               (fun acc (src, dst) ->
                 B.dor
                   acc
                   (B.dand
                      (code e s c src ~next:false)
                      (code e s c dst ~next:true)))
               (B.dfalse ())
               moves))
        (B.dtrue ())
        g.parts
    in
    let cur =
      List.concat_map (fun (c, _) -> Array.to_list e.var.(s).(c)) g.parts
    in
    let swap = Array.init e.nvars Fun.id in
    List.iter
      (fun x ->
        swap.(x) <- x + 1;
        swap.(x + 1) <- x)
      cur;
    { label = g.label
    ; rel
    ; next_cube = B.cube (List.map succ cur)
    ; cur_cube = B.cube cur
    ; swap
    }
  ;;

  (** [pre g z]: the states with a [g]-step into [z]. *)
  let pre (g : cgroup) (z : B.t) : B.t =
    B.and_exists g.next_cube g.rel (B.permute z g.swap)
  ;;

  (** [post g z]: the [g]-successors of [z]. *)
  let post (g : cgroup) (z : B.t) : B.t =
    B.permute (B.and_exists g.cur_cube g.rel z) g.swap
  ;;

  let union f xs =
    List.fold_left (fun acc x -> B.dor acc (f x)) (B.dfalse ()) xs
  ;;

  let rec lfp (f : B.t -> B.t) (z : B.t) : B.t =
    let z' = B.dor z (f z) in
    if B.equal z z' then z else lfp f z'
  ;;

  let check ?(order = Zip) (n1 : Network.t) (n2 : Network.t) : stats =
    let t0 = Unix.gettimeofday () in
    let e = encode order n1 n2 in
    B.init ~vars:e.nvars;
    let groups =
      Array.mapi
        (fun s (n : Network.t) -> List.map (compile e s) n.groups)
        e.nets
    in
    let with_label s l =
      List.filter (fun (g : cgroup) -> g.label = l) groups.(s)
    in
    let pre_label s l z = union (fun g -> pre g z) (with_label s l) in
    let pre_tau_star s z = lfp (pre_label s Network.Tau) z in
    let pre_weak s (l : Network.label) z =
      match l with
      | Tau -> pre_tau_star s z
      | Vis _ -> pre_tau_star s (pre_label s l (pre_tau_star s z))
    in
    let init s =
      let r = ref (B.dtrue ()) in
      Array.iteri
        (fun c (comp : Network.component) ->
          r := B.dand !r (code e s c comp.init ~next:false))
        e.nets.(s).comps;
      !r
    in
    let reach s =
      lfp (fun z -> union (fun g -> post g z) groups.(s)) (init s)
    in
    let r1 = reach 0
    and r2 = reach 1 in
    let labels =
      List.sort_uniq compare (Network.labels n1 @ Network.labels n2)
    in
    let iterations = ref 0 in
    let rec gfp r =
      incr iterations;
      let bad =
        union
          (fun l ->
            B.dor
              (pre_label 0 l (B.dnot (pre_weak 1 l r)))
              (pre_label 1 l (B.dnot (pre_weak 0 l r))))
          labels
      in
      let r' = B.dand r (B.dnot bad) in
      if B.equal r r' then r else gfp r'
    in
    let r = gfp (B.dand r1 r2) in
    let nbits s =
      Array.fold_left (fun acc a -> acc + Array.length a) 0 e.var.(s)
    in
    (* a set over [k] current variables, counted over all [nvars] *)
    let count k f =
      B.satcount e.nvars f /. (2. ** float_of_int (e.nvars - k))
    in
    { bisimilar = Bool.not (B.is_false (B.dand r (B.dand (init 0) (init 1))))
    ; states1 = count (nbits 0) r1
    ; states2 = count (nbits 1) r2
    ; pairs = count (nbits 0 + nbits 1) r
    ; rel_nodes = B.size r
    ; peak_nodes = B.peak ()
    ; iterations = !iterations
    ; seconds = Unix.gettimeofday () -. t0
    }
  ;;
end
