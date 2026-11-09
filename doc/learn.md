# The engine: relations learned from the rules, on Sylvan

The engine (`lib/learn`) decides a goal without building an explicit graph
of the systems:
- **Rules** (`lib/rules`): an LTS's constructors read as rules (`Print
  MeBi Rules`): a source pattern, premises (steps of an LTS, decidable side
  conditions, opaque ones), a label and a target.
- **Shapes and slots** (`lib/skeleton`, [`skeleton.md`](skeleton.md)): a
  state is a container, a finite shape and a value per slot, serialised as
  a vector `[shape; v_0; ...]`; a *summand* is a rule chain that can fire
  from a shape.
- **Learning:** each summand is learned on the slots it reads only, once
  per distinct projection, by an interpreter of its rule chain; a premise
  over a closed term is interpreted from its LTS's rules, memoised; a step
  the interpreter cannot follow is an error (`Learn.Unfollowable`).
- **Relations** hold only the positions they touch (Sylvan's meta codes:
  copy, read, write, only-read, only-write; a meta stops at the last
  touched position, past which Sylvan carries the rest unchanged);
  exploration is chained, each
  summand in turn applied to the round's frontier.
- **The game:** the weak or strong simulation game over pairs of reachable
  states (one vector, the answering side's positions first), by
  preimages within their product, to its greatest fixpoint (`gfp`). With
  the log on, its check is made again as images with Sylvan's proof log
  open: the certificate ([`certificate.md`](certificate.md)).

The technical report's section "The symbolic engine" (`doc/report`) has
the details, with each part's code.
