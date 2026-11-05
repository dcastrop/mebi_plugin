(** The systems the prototype measures, each a pair with its expected
    verdict. *)

type pair =
  { name : string
  ; a : Network.t
  ; b : Network.t
  ; expect : bool
  }

(** {1 Test4, generalised ([examples/Bisimilarity/Proc/Test4/Terms.v])}

    [n] cyclic components, [tfix (tseq (tact act tend) trec)]: a send then
    a receive on [A], then the same on [B], and so on. [p] is that vector;
    [q] is it rotated right by one. At [n = 4] these are exactly Test4's
    [p] and [q] ([q = cpar (cpar b2 a1) (cpar a2 b1)]), under the
    normalised semantics: 3^n states. The bad variant puts [C] in [q]'s
    last component, so they are not bisimilar. *)
let proc ?(bad = false) (n : int) : pair =
  let comp i =
    let l = if i / 2 mod 2 = 0 then "A" else "B" in
    Network.Proc.cyclic (if i mod 2 = 0 then Send l else Recv l)
  in
  let p = List.init n comp in
  let q =
    match List.rev p with [] -> [] | last :: rest -> last :: List.rev rest
  in
  let q =
    if bad
    then
      List.mapi
        (fun i t -> if i = n - 1 then Network.Proc.cyclic (Send "C") else t)
        q
    else q
  in
  { name = Printf.sprintf "proc%s-%d" (if bad then "-bad" else "") n
  ; a = Network.Proc.network "p" p
  ; b = Network.Proc.network "q" q
  ; expect = Bool.not bad
  }
;;

(** {1 A chain of one-place buffers against a counter}

    [n] one-place buffers in a row, [i -> c1 -> ... -> o], the inner
    channels restricted (2^n states), against an [m]-place buffer of
    indistinguishable items, one component with [m + 1] states. Weakly
    bisimilar exactly when [m = n] (Milner's classic example). *)
let buffers ?m (n : int) : pair =
  let m = Option.value m ~default:n in
  let open Network.Ccs in
  let chan k =
    if k = 0 then "i" else if k = n then "o" else Printf.sprintf "c%d" k
  in
  let defs k =
    match String.split_on_char '_' k with
    | [ "B"; k ] ->
      let k = int_of_string k in
      Pre
        ( Some (In (chan k))
        , Pre (Some (Out (chan (k + 1))), Var (Printf.sprintf "B_%d" k)) )
    | [ "C"; k ] ->
      let k = int_of_string k in
      let c j = Var (Printf.sprintf "C_%d" j) in
      let inp = Pre (Some (In "i"), c (k + 1))
      and out = Pre (Some (Out "o"), c (k - 1)) in
      if k = 0 then inp else if k = m then out else Sum (inp, out)
    | _ -> Pnil
  in
  let impl =
    network
      "chain"
      defs
      (List.init (n - 1) (fun k -> chan (k + 1)))
      (List.init n (fun k -> Var (Printf.sprintf "B_%d" k)))
  in
  let spec = network "counter" defs [] [ Var "C_0" ] in
  { name = Printf.sprintf "buffers-%d-vs-%d" n m
  ; a = impl
  ; b = spec
  ; expect = m = n
  }
;;

(** {1 The Alternating Bit Protocol ([examples/CCS.v], [abp] and [def 20])}

    Copied definition by definition. [CCS/ABPBisimProofs.v] proves
    [weak_bisimilar abp spec]: 108 states against 2. *)
let abp () : pair =
  let open Network.Ccs in
  let v k = Var (string_of_int k) in
  let i n p = Pre (Some (In n), p)
  and o n p = Pre (Some (Out n), p)
  and tau p = Pre (None, p) in
  let defs k =
    match int_of_string k with
    | 10 -> i "acc" (v 12)
    | 11 -> i "acc" (v 13)
    | 12 -> o "s0" (v 14)
    | 13 -> o "s1" (v 15)
    | 14 -> Sum (i "g0" (v 11), Sum (i "g1" (v 12), tau (v 12)))
    | 15 -> Sum (i "g1" (v 10), Sum (i "g0" (v 13), tau (v 13)))
    | 16 -> Sum (i "r0" (o "del" (o "k0" (v 17))), i "r1" (o "k1" (v 16)))
    | 17 -> Sum (i "r1" (o "del" (o "k1" (v 16))), i "r0" (o "k0" (v 17)))
    | 18 ->
      Sum
        ( i "s0" (Sum (o "r0" (v 18), tau (v 18)))
        , i "s1" (Sum (o "r1" (v 18), tau (v 18))) )
    | 19 ->
      Sum
        ( i "k0" (Sum (o "g0" (v 19), tau (v 19)))
        , i "k1" (Sum (o "g1" (v 19), tau (v 19))) )
    | 20 -> i "acc" (o "del" (v 20))
    | _ -> Pnil
  in
  let impl =
    network
      "abp"
      defs
      [ "s0"; "s1"; "r0"; "r1"; "k0"; "k1"; "g0"; "g1" ]
      [ v 10; v 18; v 16; v 19 ]
  in
  { name = "abp"; a = impl; b = network "spec" defs [] [ v 20 ]; expect = true }
;;
