(* Value-free CCS (Milner, "Communication and Concurrency", 1989), as one
   inductive LTS for MeBi: names, input and output actions, tau as [None];
   prefix, choice, parallel composition with handshake, restriction of a
   list of names, and recursion through numbered definitions [def].

   Unlike [Proc] and [CADP], one relation, [step], serves every layer. The
   definitions are those used by [Bisimilarity/CCS/PluginProofs.v]: two
   vending machines, two chained one-place buffers and a two-place buffer,
   and the Alternating Bit Protocol with a one-place buffer as its spec. *)

Inductive name : Set :=
| a | b | c                         (* textbook examples *)
| coin | coffee | tea                (* vending machines *)
| i | o | m                         (* buffers *)
| acc | del                         (* ABP: accept, deliver *)
| s0 | s1 | r0 | r1                 (* ABP: data channel, in and out *)
| k0 | k1 | g0 | g1.                (* ABP: ack channel, in and out *)

Inductive act : Set := In (n : name) | Out (n : name).

Inductive proc : Set :=
| pnil
| pre (l : option act) (p : proc)
| sum (p q : proc)
| par (p q : proc)
| res (ns : list name) (p : proc)
| var (k : nat).

Definition name_eqb (x y : name) : bool :=
  match x, y with
  | a, a | b, b | c, c | coin, coin | coffee, coffee | tea, tea
  | i, i | o, o | m, m | acc, acc | del, del
  | s0, s0 | s1, s1 | r0, r0 | r1, r1 | k0, k0 | k1, k1 | g0, g0 | g1, g1 =>
    true
  | _, _ => false
  end.

Fixpoint mem (n : name) (ns : list name) : bool :=
  match ns with nil => false | cons x tl => name_eqb n x || mem n tl end.

(* [l] uses none of the restricted names [ns]. *)
Definition allowed (ns : list name) (l : option act) : bool :=
  match l with
  | None => true
  | Some (In x) | Some (Out x) => negb (mem x ns)
  end.

Notation "'!' n '.' p" :=
  (pre (Some (Out n)) p) (at level 40, n at level 0, p at level 40).
Notation "'?' n '.' p" :=
  (pre (Some (In n)) p) (at level 40, n at level 0, p at level 40).
Notation "'tau.' p" := (pre None p) (at level 40, p at level 40).

Definition def (k : nat) : proc :=
  match k with
  (* vending machines: VM1 = coin.(coffee.VM1 + tea.VM1),
     VM2 = coin.coffee.VM2 + coin.tea.VM2 *)
  | 0 => ? coin . sum (! coffee . var 0) (! tea . var 0)
  | 1 => sum (? coin . ! coffee . var 1) (? coin . ! tea . var 1)
  (* one-place buffers i -> m and m -> o *)
  | 2 => ? i . ! m . var 2
  | 3 => ? m . ! o . var 3
  (* two-place buffer, holding 0, 1 or 2 *)
  | 4 => ? i . var 5
  | 5 => sum (? i . var 6) (! o . var 4)
  | 6 => ! o . var 5
  (* Alternating Bit Protocol. Sender S_b accepts, sends b, then waits:
     the right ack moves on, a wrong one or a timeout (tau) resends. *)
  | 10 => ? acc . var 12                                         (* S_0 *)
  | 11 => ? acc . var 13                                         (* S_1 *)
  | 12 => ! s0 . var 14                                          (* S'_0 *)
  | 13 => ! s1 . var 15                                          (* S'_1 *)
  | 14 => sum (? g0 . var 11) (sum (? g1 . var 12) (tau. var 12)) (* W_0 *)
  | 15 => sum (? g1 . var 10) (sum (? g0 . var 13) (tau. var 13)) (* W_1 *)
  (* Receiver R_b: a frame with the expected bit is delivered and acked;
     a duplicate is acked again. *)
  | 16 => sum (? r0 . ! del . ! k0 . var 17) (? r1 . ! k1 . var 16)
  | 17 => sum (? r1 . ! del . ! k1 . var 16) (? r0 . ! k0 . var 17)
  (* Lossy one-place media: deliver, or lose (tau). *)
  | 18 => sum (? s0 . sum (! r0 . var 18) (tau. var 18))
              (? s1 . sum (! r1 . var 18) (tau. var 18))
  | 19 => sum (? k0 . sum (! g0 . var 19) (tau. var 19))
              (? k1 . sum (! g1 . var 19) (tau. var 19))
  (* Specification: a one-place buffer. *)
  | 20 => ? acc . ! del . var 20
  | _ => pnil
  end.

Inductive step : proc -> option act -> proc -> Prop :=
| s_pre l p : step (pre l p) l p
| s_suml p q l p' : step p l p' -> step (sum p q) l p'
| s_sumr p q l q' : step q l q' -> step (sum p q) l q'
| s_parl p q l p' : step p l p' -> step (par p q) l (par p' q)
| s_parr p q l q' : step q l q' -> step (par p q) l (par p q')
| s_comm p q n p' q' :
    step p (Some (Out n)) p' -> step q (Some (In n)) q' ->
    step (par p q) None (par p' q')
| s_comm' p q n p' q' :
    step p (Some (In n)) p' -> step q (Some (Out n)) q' ->
    step (par p q) None (par p' q')
| s_res ns p l p' :
    step p l p' -> allowed ns l = true -> step (res ns p) l (res ns p')
| s_var k l p' : step (def k) l p' -> step (var k) l p'.

Definition abp : proc :=
  res (cons s0 (cons s1 (cons r0 (cons r1 (cons k0 (cons k1 (cons g0 (cons g1 nil))))))))
    (par (par (var 10) (var 18)) (par (var 16) (var 19))).
