(** Deciding strong and weak bisimilarity and similarity on Sylvan's LDDs,
    from steps learned from the user's rules ([doc/learn.md]); the
    decision's last checks repeated with Sylvan's proof log on, for the
    certificate ([doc/certificate.md]).

    A state is a vector [shape; slot values] ({!Skeleton.split}), each
    value and shape an integer from a table.
    - {b Learning:} each summand ({!Skeleton.summands_of}) is learned on
      the slots it reads only, once per distinct projection, by an
      interpreter of its rule chain. A premise over a closed term is
      interpreted from its LTS's rules too, memoised.
    - {b Relations} hold only the positions they touch: a summand's static
      meta codes, widened where a result writes more (a changed shape).
      A projection whose result moves a slot it never read is learned on
      whole states instead (an escape).
    - {b Exploration} is chained: each summand in turn is applied to the
      round's frontier grown by the summands before it.
    - {b The verdict} is the weak or strong game over pairs of reachable
      states, by preimages within their product. *)

(** a system: an LTS (an inductive [M -> option A -> M -> Prop]) and a
    closed start term *)
type system =
  { lts : Names.inductive
  ; start : Constr.t (** closed *)
  }

(** a closed term whose steps the LTS's rules do not determine (a premise
    the interpreter cannot follow), with its LTS *)
exception Unfollowable of Names.inductive * Constr.t

type kind =
  | Bisim (** [weak_bisimilar], or [strong_bisimilar] *)
  | Sim (** [weak_sim] or [strong_sim]: the first answered by the second *)

type report =
  { verdict : bool
  ; states : float * float (** reachable states of each system *)
  ; pairs : float (** pairs in the greatest fixpoint *)
  ; labels : int
  ; interpreted : int (** closed terms' steps, from the rules *)
  ; projections : int * int (** projections learned, per system *)
  ; escapes : int * int (** projections learned on whole states instead *)
  ; summands : int * int
  ; width : int * int (** vector lengths *)
  ; rounds : int * int (** of exploration, per system (chained) *)
  ; iterations : int (** of the game *)
  ; nodes : int (** of the greatest fixpoint *)
  ; seconds : float
  }

(** [check env sigma ?width ?strong kind a b]: whether [a]'s start and
    [b]'s are weakly bisimilar ([Bisim]), or [a]'s weakly simulated by
    [b]'s ([Sim]). [width] is the first number of slots tried (default 8);
    it grows when a state needs more. With [strong] (default false), no
    label is silent: strong bisimilarity or simulation, each step answered
    by one step with the same label. With {!proof_log} set, the last checks
    are logged. Raises [Unfollowable]. *)
val check
  :  Environ.env
  -> Evd.evar_map
  -> ?width:int
  -> ?strong:bool
  -> kind
  -> system
  -> system
  -> report

(** [pr kind r]: the verdict and the costs. Raises nothing. *)
val pr : ?strong:bool -> kind -> report -> Pp.t

(** [proof_log]: where the next decision writes Sylvan's proof log (a
    file, or memory: then collection stays off after it, until
    {!Ldd.log_free}; [None]: no log). After the fixpoint, each side's last
    exploration round and the game's last check are repeated with the log
    open, each top-level preimage recorded with the way the game's proof
    uses it (doc/certificate.md, A). Set by the plugin, from
    [Set MeBi Log Mode]. *)
val proof_log : Proof_log.source option ref

(** {1 The plan of the logged check}

    Every set the logged check made, by Sylvan's node index
    ({!Ldd.index}), so that a proof can name the facts the log holds
    (doc/certificate.md, "The theorem from the certificate"). *)

(** a relation of the check: its label's number, its meta's node and its
    node (an answerer's relation backwards) *)
module Plan : sig
  type prel =
    { label : int
    ; meta : int
    ; rel : int
    }

  (** a union step [res = acc ∪ arg] *)
  type punion =
    { acc : int
    ; arg : int
    ; res : int
    }

  (** the images of [src] by [rels] (each in [imgs]), their union from the
      empty set, left to right ([unions]), and that union [res] *)
  type pimgs =
    { src : int
    ; rels : prel list
    ; imgs : int list
    ; unions : punion list
    ; res : int
    }

  (** a round of a silent closure backwards: the images of [fresh], the new
      pairs [fresh' = p \ tz] and [tz' = tz ∪ fresh'] *)
  type pround =
    { fresh : int
    ; p : pimgs
    ; tz : int
    ; fresh' : int
    ; tz' : int
    }

  (** a silent closure backwards from [cstart]: its rounds, until a round
      adds nothing ([final]) *)
  type pclosure =
    { cstart : int
    ; rounds : pround list
    ; final : int
    }

  (** a move of the mover: its relation, the image [img] of [z] by it,
      within [ok] (their difference empty) *)
  type pmove =
    { mrel : prel
    ; img : int
    ; ok : int
    }

  (** a label of the mover: the pairs [okset] that can answer it (the
      images of [tz] by the answerer's relations of that label, closed:
      [okfrom]; none for the silent label, whose [okset] is [tz]) *)
  type plabel =
    { lab : int
    ; okset : int
    ; okfrom : (pimgs * pclosure) option
    ; moves : pmove list
    }

  (** a summand of the mover, for completeness: its source shape, the
      positions it reads ([keep], position 0 the shape), its rule chain,
      its relations (label, codes, diagram, meta), the projections it learned
      ([seen]); and the coverage of [z] by them: [f] the image of [z] by
      the shape's filter [sh] (complete), [g] its image by [seen] read
      only (sound), their difference empty *)
  type psum =
    { sid : int
    ; keep : bool list
    ; chain : string list
    ; srels : (int * string * Ldd.t * int) list
    ; seen : Ldd.t
    ; escaped : bool
    ; sh : prel
    ; dsee : prel
    ; f : int
    ; g : int
    }

  (** a direction of the check: which side moves ([mover_a]: the first
      system), the silent closure [tzc] of [z], the labels, and the
      coverage of [z] by the mover's summands *)
  type pdir =
    { mover_a : bool
    ; tzc : pclosure
    ; plabels : plabel list
    ; cover : psum list
    }

  (** a normalisation tuple (doc/upto.md, route B): a vector, its
      representative, and the item that gives it: the item's shape, its
      slot map (slot [j] of the item's shape holds the vector's slot
      [nmap.(j)]), its path (each step's source shape and reordering
      summand, by index in [Skeleton.summands_of]) *)
  type pnorm =
    { nfrom : int array
    ; nto : int array
    ; nshape : Skeleton.shape
    ; nmap : int array
    ; npath : (Skeleton.shape * int) list
    }

  (** a system, for its encoding: its width (positions after the shape),
      its shapes (number, shape, each slot's position), its start *)
  type psys =
    { pwidth : int
    ; pshapes : (int * Skeleton.shape * int array) list
    ; pstart : Constr.t
    ; plts : Names.inductive
    ; pstart_v : int array
      (** the start's vector, not normalised (up to reordering) *)
    ; pnorm : pnorm list
      (** up to reordering: each vector met that is not its own
          representative *)
    }

  type t =
    { z : int
    ; start : int array (** the start pair's vector *)
    ; b_first : bool (** the second system's positions first *)
    ; widths : int * int (** the first and the second system's widths *)
    ; tau : int option (** the silent label's number, if weak *)
    ; label_terms : (int * Constr.t) list
    ; dirs : pdir list
    ; values : (int * Constr.t) list
    ; sys_a : psys
    ; sys_b : psys
    ; rinfo :
        ((int * int * int) * (int * string * Ldd.t * int * string list)) list
      (** each answerer's relation of the check, backwards, by (label,
          meta, node): its label, its codes (its system's, as [codes]
          letters: X read-write, O read, W write, C copy), its diagram, its
          summand's source shape and rule chain *)
    }
end

(** the plan of the last logged check ([None] when it was not logged) *)
val last_plan : Plan.t option ref

(** {1 Derivations} *)

(** a derivation of a step of a closed term: its rule, the values of the
    rule's variables (outermost first), its premises' derivations (in the
    rule's order), its label and target (normalised) *)
type deriv =
  { drule : Rules.rule
  ; dlts : Names.inductive
  ; dvals : Constr.t array
  ; dprems : dprem list
  ; dlbl : Constr.t
  ; dtgt : Constr.t
  }

and dprem =
  | DStep of deriv
  | DDecide of Constr.t

(** [derivations env sigma]: a function giving every step of a closed term
    by an LTS, each with its derivation (memoised) *)
val derivations
  :  Environ.env
  -> Evd.evar_map
  -> Names.inductive
  -> Constr.t
  -> deriv list

(** [derive_chain env sigma]: a function giving the steps of a partial term
    (placeholders [Meta i] for the slots a summand does not read) along a
    summand's rule chain, each with its derivation, whose variables may
    hold placeholders. Raises [Failure] where the interpreter is stuck. *)
val derive_chain
  :  Environ.env
  -> Evd.evar_map
  -> Names.inductive
  -> Constr.t
  -> string list
  -> deriv list

(** {1 Up to reordering (doc/upto.md, route B)} *)

(** [is_reordering cong sm]: the summand [sm]'s innermost rule is one of
    the congruence rules [cong], and its target only moves slots *)
val is_reordering : string list -> Skeleton.summand -> bool

(** [upto_off]: no decision up to reordering while it is set (the
    plugin's retry when a proof up to reordering cannot be built). Up to
    reordering is on by default, for weak goals of an LTS whose rules
    hold reorderings and whose summands are not dynamic; [MEBI_UPTO=off]
    turns it off. *)
val upto_off : bool ref
